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

		const mod: EmscriptenModule = {
			arguments: ['/atomvm/arc.avm'],
			locateFile: (p) => `/atomvm/${p}`,
			print: onPrintStable,
			printErr: onPrintStable,
			onRuntimeInitialized: () => setStatus({ kind: 'ready', vm: mod as AtomVM }),
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
