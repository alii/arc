import { useEffect, useEffectEvent, useState } from 'react';

export type AtomVM = {
	call: (proc: string, msg: string) => Promise<string>;
	cast: (proc: string, msg: string) => void;
};

type EmscriptenModule = Partial<AtomVM> & {
	arguments?: string[];
	locateFile?: (path: string) => string;
	print?: (s: string) => void;
	printErr?: (s: string) => void;
	onRuntimeInitialized?: () => void;
	INITIAL_MEMORY?: number;
};

/**
 * `loading` — AtomVM.js/wasm still arriving; `warming` — the runtime is up but
 * AtomVM's own boot and the JS engine build are still running (calling in now
 * would hang the tab); `ready` — calls are safe; `warm` flips once the
 * background warm-up has loaded the interpreter's common modules.
 */
type Status =
	| { kind: 'loading' }
	| { kind: 'warming' }
	| { kind: 'ready'; vm: AtomVM; warm: boolean }
	| { kind: 'error'; message: string };

declare global {
	interface Window {
		Module?: EmscriptenModule;
	}
}

/**
 * Exercises the interpreter's common paths so the BEAM modules they live in
 * are loaded before the user's first run (AtomVM loads modules on first use;
 * a cold run of the fib example measured 4.0s vs 0.95s warm). Sent as
 * separate calls so a click that lands mid-warm-up waits for one chunk, not
 * all of them. Everything they print is swallowed by the loader.
 */
const WARM_UP_CHUNKS = [
	`class A { #x = 1; static s = 2; get x() { return this.#x; } m() { return super.toString(); } } new A().x;`,
	`function* g() { yield* [1, 2, 3]; } [...g(), ...new Set([4]), ...new Map([[5, 6]]).keys()].map((x) => x * 2).filter(Boolean).reduce((a, b) => a + b, 0);`,
	`async function f() { await Promise.resolve(1); for await (const x of (async function* () { yield 1; })()) x; } f();`,
	`Array.from({ length: 3 }, (_, i) => i).sort((a, b) => b - a).join(','); Object.entries({ a: 1, ...{ b: 2 } }); Object.assign({}, { c: 3 });`,
	`JSON.parse(JSON.stringify({ a: [1, { b: 'c' }], d: null }));`,
	`'Hello, World'.toUpperCase().split(', ').reverse().join('').padStart(20, '.').slice(1, -1) + \`\${1 + 1}\` + String(1.5e21) + (0.1 + 0.2).toFixed(2);`,
	// No regex here: RegExp is unsupported in the browser build (no PCRE in AtomVM).
	`Math.max(1, 2, Math.floor(Math.random() * 10)); new Date(0).toISOString(); 'x-y'.replace('-', '_').split('_').indexOf('y');`,
	`let e; try { null.x; } catch (err) { e = err instanceof TypeError && err.message; }`,
	`Symbol.iterator in [] && typeof BigInt(1) === 'bigint' && new Uint8Array([1, 2]).length;`,
	`console.log([1, 'two', { three: 3 }, [4], null, undefined, () => {}, new Map([[1, 2]])], 'x', 1.5); console.error('e');`,
];

/**
 * Loads AtomVM-WASM + the Arc bundle. Emscripten reads its config from a
 * global `Module` object that must exist before AtomVM.js runs, hence the
 * imperative script-tag dance rather than a clean ESM import.
 */
export function useAtomVM(onPrint: (line: string) => void) {
	const [status, setStatus] = useState<Status>({ kind: 'loading' });

	const onPrintStable = useEffectEvent(onPrint);

	useEffect(() => {
		if (window.Module) return;

		let rejectPending: ((reason: Error) => void) | null = null;
		let swallowPrints = false;

		const handleAbort = (s: string) => {
			if (rejectPending && (s.includes('Aborted') || s.includes('RuntimeError'))) {
				rejectPending(new Error(s));
				rejectPending = null;
			}
		};

		const mod: EmscriptenModule = {
			arguments: ['/atomvm/arc.avm'],
			locateFile: (p) => `/atomvm/${p}`,
			INITIAL_MEMORY: 256 * 1024 * 1024,
			print: (s) => {
				if (!swallowPrints) onPrintStable(s);
				handleAbort(s);
			},
			printErr: (s) => {
				if (!swallowPrints) onPrintStable(s);
				handleAbort(s);
			},
			onRuntimeInitialized: () => {
				const rawCall = mod.call!.bind(mod);
				const wrappedCall = (proc: string, msg: string): Promise<string> => {
					return new Promise<string>((resolve, reject) => {
						rejectPending = reject;
						rawCall(proc, msg).then(
							(result) => {
								rejectPending = null;
								resolve(result);
							},
							(err) => {
								rejectPending = null;
								reject(err);
							},
						);
					});
				};
				const vm: AtomVM = { call: wrappedCall, cast: mod.cast!.bind(mod) };
				setStatus({ kind: 'warming' });
				// The runtime is up but AtomVM's own main() is still booting in its
				// worker: `call` before it has finished spins on an uninitialised
				// mutex and hangs the tab. The Erlang listener fires `atomvm:ready`
				// once it is registered and the engine is built (arc_wasm_ffi).
				window.addEventListener(
					'atomvm:ready',
					async () => {
						setStatus({ kind: 'ready', vm, warm: false });
						const t0 = performance.now();
						// Nothing the warm-up prints belongs in the Output pane: not the
						// console chunk's lines, not AtomVM's module-loader chatter.
						swallowPrints = true;
						try {
							for (const chunk of WARM_UP_CHUNKS) {
								try {
									await vm.call('main', chunk);
								} catch (err) {
									console.warn('arc: warm-up chunk failed', err, chunk);
								}
							}
						} finally {
							swallowPrints = false;
						}
						console.debug(`arc: warm-up done in ${(performance.now() - t0).toFixed(0)}ms`);
						setStatus({ kind: 'ready', vm, warm: true });
					},
					{ once: true },
				);
			},
		};
		window.Module = mod;

		const script = document.createElement('script');
		script.src = '/atomvm/AtomVM.js';
		script.async = true;
		script.onerror = () => setStatus({ kind: 'error', message: 'failed to load AtomVM.js' });
		document.body.appendChild(script);
	}, [onPrintStable]);

	return status;
}
