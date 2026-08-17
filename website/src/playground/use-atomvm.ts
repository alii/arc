import { useEffect, useEffectEvent, useState } from 'react';

export type AtomVM = {
	call: (proc: string, msg: string) => Promise<string>;
	cast: (proc: string, msg: string) => void;
};

type EmscriptenModule = Partial<AtomVM> & {
	arguments?: string[];
	onAbort?: (what: unknown) => void;
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
	| { kind: 'error'; message: string; detail: string };

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
	`Math.max(1, 2, Math.floor(Math.random() * 10)); new Date(0).toISOString(); /a(?<n>b)+/g.exec('abb')?.groups.n; 'x-y'.replace(/-/g, '_');`,
	`let e; try { null.x; } catch (err) { e = err instanceof TypeError && err.message; }`,
	`Symbol.iterator in [] && typeof BigInt(1) === 'bigint' && new Uint8Array([1, 2]).length;`,
	`console.log([1, 'two', { three: 3 }, [4], null, undefined, () => {}, new Map([[1, 2]])], 'x', 1.5); console.error('e');`,
];

/**
 * The page half of `arc_js_bridge` (website/atomvm_shims/arc_js_bridge.erl):
 * the Erlang side runs `__arcJsBridge.run(id, thunk)` on the main thread via
 * `emscripten:run_script`, and the result goes back with `Module.cast` to the
 * registered `arc_js_bridge` process as `id \x1f status \x1f payload`.
 *
 * On top of it, RegExp for the browser build: AtomVM has no PCRE, but the
 * browser has the one regex engine with exactly JavaScript's semantics. The
 * Erlang shim (atomvm_shims/arc_regexp_ffi.erl) speaks UTF-8 byte offsets, so
 * the helpers convert to and from UTF-16 indices here.
 */
function installJsBridge(mod: EmscriptenModule) {
	const RS = '\x1e';
	const utf8Len = (s: string) => new TextEncoder().encode(s).length;

	/** Capturing-group count and (name → 1-based index) pairs of a pattern source. */
	const groupInfo = (source: string) => {
		let count = 0;
		let inClass = false;
		const names: [string, number][] = [];
		for (let i = 0; i < source.length; i++) {
			const c = source[i];
			if (c === '\\') {
				i++;
				continue;
			}
			if (inClass) {
				if (c === ']') inClass = false;
				continue;
			}
			if (c === '[') {
				inClass = true;
				continue;
			}
			if (c !== '(') continue;
			if (source[i + 1] !== '?') {
				count++;
			} else if (source[i + 2] === '<' && source[i + 3] !== '=' && source[i + 3] !== '!') {
				count++;
				const end = source.indexOf('>', i + 3);
				names.push([source.slice(i + 3, end), count]);
			}
		}
		return { count, names };
	};

	/** UTF-8 byte offset → UTF-16 index, or -1 if it does not fall on a code point boundary. */
	const utf16Index = (s: string, byteOffset: number) => {
		let bytes = 0;
		let idx = 0;
		for (const ch of s) {
			if (bytes >= byteOffset) break;
			const cp = ch.codePointAt(0)!;
			bytes += cp < 0x80 ? 1 : cp < 0x800 ? 2 : cp < 0x10000 ? 3 : 4;
			idx += ch.length;
		}
		return bytes === byteOffset ? idx : -1;
	};

	const bridge = {
		run(id: number, thunk: () => unknown) {
			let status = 'ok';
			let payload: string;
			try {
				payload = String(thunk());
			} catch (e) {
				status = 'error';
				payload = e instanceof Error ? e.message : String(e);
			}
			mod.cast!('arc_js_bridge', `${id}\x1f${status}\x1f${payload}`);
		},
		regexpCompile(pattern: string, flags: string) {
			new RegExp(pattern, flags); // throws SyntaxError → error reply
			const { count, names } = groupInfo(pattern);
			return `${count}${RS}${names.map(([n, i]) => `${n}=${i}`).join(',')}`;
		},
		regexpExec(pattern: string, flags: string, s: string, byteOffset: number, sticky: boolean) {
			const re = new RegExp(pattern, flags.replace(/[gyd]/g, '') + 'd' + (sticky ? 'y' : 'g'));
			const start = utf16Index(s, byteOffset);
			if (start < 0) return 'nomatch';
			re.lastIndex = start;
			const m = re.exec(s) as (RegExpExecArray & { indices?: [number, number][] }) | null;
			if (!m || !m.indices) return 'nomatch';
			const span = ([a, b]: [number, number]) => `${utf8Len(s.slice(0, a))},${utf8Len(s.slice(a, b))}`;
			const groups = m.indices.slice(1).map((g) => (g ? span(g) : '-1,0'));
			return `${span(m.indices[0])}${RS}${groups.join(';')}`;
		},
	};
	(globalThis as unknown as { __arcJsBridge: typeof bridge }).__arcJsBridge = bridge;
}

/** What stops the runtime from starting here, or null if all good. */
function preflight(): { message: string; detail: string } | null {
	// Dev only: ?simulate=nowasm | noisolation to see these states on a normal browser.
	const simulate = import.meta.env.DEV ? new URLSearchParams(location.search).get('simulate') : null;
	const noWasm =
		simulate === 'nowasm' || typeof WebAssembly === 'undefined' || typeof WebAssembly.instantiate !== 'function';
	const noIsolation = simulate === 'noisolation' || typeof SharedArrayBuffer === 'undefined' || !crossOriginIsolated;
	if (noWasm) {
		return {
			message: 'WebAssembly is off in this browser',
			detail: 'This is usually because of a managed device policy.',
		};
	}
	if (noIsolation) {
		return {
			message: 'Page isn’t cross-origin isolated',
			detail:
				'The headers that allow threads (COOP and COEP) were removed somewhere between the server and your browser.',
		};
	}
	return null;
}

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

		// Pre-flight: say clearly what is missing instead of loading forever.
		// AtomVM-WASM is a pthreads build, so besides WebAssembly itself it needs
		// SharedArrayBuffer, which browsers only expose to cross-origin-isolated
		// pages (COOP/COEP headers — see vercel.json / vite.config.js).
		const missing = preflight();
		if (missing) {
			setStatus({ kind: 'error', ...missing });
			return;
		}

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
			// Aborts before the runtime is up are almost always the environment
			// (CSP without 'wasm-unsafe-eval', memory limits) — surface them.
			onAbort: (what) => {
				setStatus((current) =>
					current.kind === 'ready'
						? current
						: {
								kind: 'error',
								message: 'The runtime couldn’t start',
								detail: `This is usually because of a strict Content Security Policy or a memory limit. (${String(what)})`,
							},
				);
			},
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
				installJsBridge(mod);
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
		script.onerror = () =>
			setStatus({
				kind: 'error',
				message: 'Couldn’t load the runtime',
				detail: 'The runtime script could not be downloaded.',
			});
		document.body.appendChild(script);
		// Runs once: `onPrintStable` is an Effect Event (not reactive, must not be
		// a dependency — its identity is not stable), and the loader is global.
		// eslint-disable-next-line react-hooks/exhaustive-deps
	}, []);

	return status;
}
