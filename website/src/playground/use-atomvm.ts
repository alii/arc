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
};

type Status = { kind: 'loading' } | { kind: 'ready'; vm: AtomVM } | { kind: 'error'; message: string };

declare global {
	interface Window {
		Module?: EmscriptenModule;
	}
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

		let rejectPending: ((reason: Error) => void) | null = null;

		const handleAbort = (s: string) => {
			if (rejectPending && (s.includes('Aborted') || s.includes('RuntimeError'))) {
				rejectPending(new Error(s));
				rejectPending = null;
			}
		};

		const mod: EmscriptenModule = {
			arguments: ['/atomvm/arc.avm'],
			locateFile: (p) => `/atomvm/${p}`,
			print: (s) => {
				onPrintStable(s);
				handleAbort(s);
			},
			printErr: (s) => {
				onPrintStable(s);
				handleAbort(s);
			},
			onRuntimeInitialized: () => {
				const rawCall = mod.call!.bind(mod);
				const wrappedCall = (proc: string, msg: string): Promise<string> => {
					return new Promise<string>((resolve, reject) => {
						rejectPending = reject;
						rawCall(proc, msg).then(
							(result) => { rejectPending = null; resolve(result); },
							(err) => { rejectPending = null; reject(err); },
						);
					});
				};
				setStatus({ kind: 'ready', vm: { call: wrappedCall, cast: mod.cast!.bind(mod) } });
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
