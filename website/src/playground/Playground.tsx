import { useState, useRef } from 'react';
import { motion, AnimatePresence } from 'motion/react';
import { useAtomVM } from './use-atomvm';

const EXAMPLE = `const parent = Arc.self();
Arc.log("starting...");

for (let i = 0; i < 3; i++) {
  Arc.spawn(() => {
    Arc.sleep(300 * (i + 1));
    Arc.send(parent, "hello from process " + i);
  });
}

for (let i = 0; i < 3; i++) {
  Arc.log(Arc.receive());
}`;

const rows = EXAMPLE.split('\n').length;

export function Playground() {
	const [code, setCode] = useState(EXAMPLE);
	const [output, setOutput] = useState<{ id: number; text: string }[]>([]);
	const [running, setRunning] = useState(false);
	const [didRun, setDidRun] = useState(false);
	const nextId = useRef(0);

	if (running && !didRun) setDidRun(true);

	const push = (text: string) => setOutput((o) => [...o, { id: nextId.current++, text }]);

	const vm = useAtomVM(push);

	const run = async () => {
		if (vm.kind !== 'ready') return;
		setOutput([]);
		setRunning(true);
		try {
			const result = await vm.vm.call('main', code);
			push(`→ ${result}`);
		} catch (e) {
			push(`✗ ${e}`);
		} finally {
			setRunning(false);
		}
	};

	return (
		<div className="rounded-lg border border-neutral-200 dark:border-neutral-800 overflow-hidden">
			<div className="flex items-center justify-between px-4 py-2 bg-neutral-50 dark:bg-neutral-900/50 border-b border-neutral-200 dark:border-neutral-800">
				<span className="text-xs text-neutral-500">
					{vm.kind === 'loading' && 'Loading AtomVM…'}
					{vm.kind === 'error' && `error: ${vm.message}`}
					{vm.kind === 'ready' && (running ? 'Running' : didRun ? 'Idle' : 'Ready')}
				</span>
				<button
					onClick={run}
					disabled={vm.kind !== 'ready' || running}
					className="px-3 py-1 text-xs rounded bg-neutral-900 text-neutral-100 dark:bg-neutral-100 dark:text-neutral-900 disabled:opacity-40"
				>
					{running ? '…' : 'run'}
				</button>
			</div>

			<textarea
				value={code}
				onChange={(e) => setCode(e.target.value)}
				spellCheck={false}
				rows={rows}
				className="w-full p-4 bg-white text-neutral-700 dark:bg-neutral-950 dark:text-neutral-300 font-mono text-sm resize-none outline-none"
			/>

			<AnimatePresence>
				{output.length > 0 && (
					<motion.pre
						initial={{ opacity: 0 }}
						animate={{ opacity: 1 }}
						exit={{ height: 0, opacity: 0 }}
						transition={{ duration: 0.2, ease: [0.25, 0.1, 0.25, 1] }}
						className="m-0 bg-neutral-50 text-neutral-600 dark:bg-black dark:text-neutral-400 font-mono text-xs border-t border-neutral-200 dark:border-neutral-800 max-h-40 overflow-auto"
					>
						<div className="p-4">
							{output.map((line) => (
								<motion.div
									key={line.id}
									initial={{ opacity: 0, filter: 'blur(4px)', height: 0 }}
									animate={{ opacity: 1, filter: 'blur(0px)', height: 'auto' }}
									transition={{ duration: 0.25, ease: [0.23, 1, 0.32, 1] }}
									className="will-change-[filter] overflow-hidden"
								>
									{line.text}
								</motion.div>
							))}
						</div>
					</motion.pre>
				)}
			</AnimatePresence>
		</div>
	);
}
