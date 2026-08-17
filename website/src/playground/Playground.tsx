import { defaultKeymap, history, historyKeymap } from '@codemirror/commands';
import { javascript } from '@codemirror/lang-javascript';
import { Compartment, EditorState } from '@codemirror/state';
import { EditorView, keymap, placeholder } from '@codemirror/view';
import * as Select from '@radix-ui/react-select';
import { AnimatePresence, motion } from 'motion/react';
import { type ReactNode, useCallback, useEffect, useLayoutEffect, useRef, useState } from 'react';
import gitExamples from 'virtual:examples';
import { CodeView, type CodeLanguage } from './CodeView';
import { baseTheme, getIsDark, themeExtensions, watchColorScheme } from './theme';
import { AboutDialog } from '../components/about-dialog';
import { useAtomVM } from './use-atomvm';

const HELLO_EXAMPLE = {
	name: 'hello',
	code: `// Real JavaScript, running as WebAssembly via AtomVM.
const greet = (name) => \`Hello, \${name}!\`;

console.log(greet('world'));
console.log([1, 2, 3].map((n) => n * n));`,
};

const FIBONACCI_EXAMPLE = {
	name: 'fibonacci',
	code: `function* fib() {
  let [a, b] = [0, 1];
  while (true) {
    yield a;
    [a, b] = [b, a + b];
  }
}

const take = (it, k) => Array.from({ length: k }, () => it.next().value);

console.log(take(fib(), 10));`,
};

const examples = [HELLO_EXAMPLE, FIBONACCI_EXAMPLE, ...gitExamples];

type AotTab = 'erlang' | 'core' | 'ir';
type Tab = 'output' | AotTab;

const AOT_TABS: { id: AotTab; label: string; language: CodeLanguage; primary: boolean }[] = [
	{ id: 'erlang', label: 'Erlang', language: 'erlang', primary: true },
	{ id: 'core', label: 'Core Erlang', language: 'erlang', primary: false },
	{ id: 'ir', label: 'IR', language: 'plain', primary: false },
];

/**
 * What the AOT compiler said about a given source. `source` is the exact text
 * it was asked about, so a result can be shown as stale once the editor moves
 * on without throwing it away.
 */
type Aot =
	| { status: 'idle' }
	| { status: 'compiling'; source: string }
	| { status: 'done'; source: string; ms: number; ir: string; core: string; erlang: string }
	| { status: 'error'; source: string; ms: number; message: string };

/** The `aot` endpoint answers IR, Core Erlang and Erlang joined by U+001E. */
function parseEmitted(reply: string): { ir: string; core: string; erlang: string } {
	const [ir = '', core = '', erlang = ''] = reply.split('');
	return { ir, core, erlang };
}

function getIsMac() {
	return /Mac|iPhone|iPad|iPod/.test(navigator.userAgent);
}

function Spinner() {
	return (
		<svg
			className="h-3.5 w-3.5 inline-block animate-spinner-fade"
			viewBox="0 0 24 24"
			fill="none"
			stroke="currentColor"
			strokeWidth="2"
			strokeLinecap="round"
		>
			{[...Array(8)].map((_, i) => {
				const angle = i * 45;
				const rad = (angle * Math.PI) / 180;
				const x1 = 12 + 6 * Math.cos(rad);
				const y1 = 12 + 6 * Math.sin(rad);
				const x2 = 12 + 10 * Math.cos(rad);
				const y2 = 12 + 10 * Math.sin(rad);
				return (
					<line
						key={i}
						x1={x1}
						y1={y1}
						x2={x2}
						y2={y2}
						opacity={1 - i * 0.1}
						style={{ animationDelay: `${i * -0.125}s` }}
					/>
				);
			})}
		</svg>
	);
}

/** One line in the Output pane: what the program printed, what it evaluated to, or why it stopped. */
type OutputLine = { id: number; kind: 'stdout' | 'result' | 'error'; text: string };

const rainbowText =
	'animate-rainbow bg-[length:200%_auto] bg-clip-text text-transparent bg-[linear-gradient(90deg,#eb6f92,#f6c177,#9ccfd8,#c4a7e7,#ebbcba,#31748f,#eb6f92)]';

export function Playground() {
	const [code, setCode] = useState(examples[0]?.code ?? '');
	const [output, setOutput] = useState<OutputLine[]>([]);
	const [running, setRunning] = useState(false);
	const [didRun, setDidRun] = useState(false);
	const [elapsed, setElapsed] = useState(0);
	const [tab, setTab] = useState<Tab>('output');
	const [moreTabs, setMoreTabs] = useState(false);
	const [aot, setAot] = useState<Aot>({ status: 'idle' });
	const nextId = useRef(0);
	const editorRef = useRef<HTMLDivElement>(null);
	const outputRef = useRef<HTMLPreElement>(null);
	const viewRef = useRef<EditorView | null>(null);
	const codeRef = useRef(code);
	const runRef = useRef<() => void>(() => {});
	// The source being compiled right now, and the latest one asked for meanwhile.
	const inflightRef = useRef<string | null>(null);
	const pendingCompileRef = useRef<string | null>(null);

	if (running && !didRun) setDidRun(true);

	useLayoutEffect(() => {
		const el = outputRef.current;
		if (!el) return;
		const threshold = 32;
		const isAtBottom = el.scrollHeight - el.scrollTop - el.clientHeight < threshold;
		if (isAtBottom) el.scrollTop = el.scrollHeight;
	}, [output]);

	useEffect(() => {
		if (!running) return;
		setElapsed(0);
		const start = performance.now();
		const id = setInterval(() => setElapsed(performance.now() - start), 32);
		return () => clearInterval(id);
	}, [running]);

	const push = (kind: OutputLine['kind'], text: string) =>
		setOutput((o) => [...o, { id: nextId.current++, kind, text }]);

	const vm = useAtomVM((line) => push('stdout', line));

	const compile = useCallback(
		async (source: string) => {
			if (vm.kind !== 'ready') return;
			if (inflightRef.current !== null) {
				// One compile at a time; remember only the latest ask.
				pendingCompileRef.current = source;
				return;
			}
			inflightRef.current = source;
			setAot({ status: 'compiling', source });
			const t0 = performance.now();
			try {
				const reply = await vm.vm.call('aot', source);
				setAot({ status: 'done', source, ms: performance.now() - t0, ...parseEmitted(reply) });
			} catch (e) {
				setAot({ status: 'error', source, ms: performance.now() - t0, message: String(e) });
			}
			inflightRef.current = null;
			const next = pendingCompileRef.current;
			pendingCompileRef.current = null;
			if (next !== null && next !== source) void compile(next);
		},
		[vm],
	);

	const isAotTab = tab !== 'output';

	const runningRef = useRef(false);

	const run = useCallback(async () => {
		// One run at a time: ⌘⏎ bypasses the disabled button, and overlapping
		// calls into the VM are pointless (the listener serialises them anyway).
		if (vm.kind !== 'ready' || runningRef.current) return;
		runningRef.current = true;
		const source = codeRef.current;
		setOutput([]);
		setRunning(true);
		const t0 = performance.now();
		try {
			const result = await vm.vm.call('main', source);
			console.debug(`arc: run took ${(performance.now() - t0).toFixed(0)}ms`);
			// A script's completion value is almost always `undefined` — noise.
			if (result !== 'undefined') push('result', result);
		} catch (e) {
			push('error', String(e));
		} finally {
			setRunning(false);
			runningRef.current = false;
		}
		if (isAotTab) void compile(source);
	}, [vm, isAotTab, compile]);

	runRef.current = run;

	// Selecting a compiler tab compiles what's in the editor if it hasn't been
	// compiled yet (or has changed since).
	const selectTab = useCallback(
		(next: Tab) => {
			setTab(next);
			if (next === 'output') return;
			const source = codeRef.current;
			const fresh = aot.status !== 'idle' && aot.source === source;
			if (!fresh) void compile(source);
		},
		[aot, compile],
	);

	useEffect(() => {
		if (!editorRef.current) return;

		const themeCompartment = new Compartment();

		const updateListener = EditorView.updateListener.of((update) => {
			if (update.docChanged) {
				const newCode = update.state.doc.toString();
				codeRef.current = newCode;
				setCode(newCode);
			}
		});

		const state = EditorState.create({
			doc: code,
			extensions: [
				keymap.of([
					{
						key: 'Mod-Enter',
						run: () => {
							runRef.current();
							return true;
						},
					},
				]),
				history(),
				keymap.of([...defaultKeymap, ...historyKeymap]),
				javascript(),
				baseTheme,
				themeCompartment.of(themeExtensions(getIsDark())),
				EditorView.lineWrapping,
				placeholder('Write some JavaScript…'),
				updateListener,
			],
		});

		const view = new EditorView({
			state,
			parent: editorRef.current,
		});

		viewRef.current = view;

		const unwatch = watchColorScheme((dark) => {
			view.dispatch({ effects: themeCompartment.reconfigure(themeExtensions(dark)) });
		});

		return () => {
			unwatch();
			view.destroy();
		};
		// Mount once; the editor owns the document from here.
		// eslint-disable-next-line react-hooks/exhaustive-deps
	}, []);

	const loadExample = useCallback((code: string) => {
		const view = viewRef.current;
		if (!view) return;
		view.dispatch({
			changes: { from: 0, to: view.state.doc.length, insert: code },
		});
		setOutput([]);
		setDidRun(false);
	}, []);

	const stale = aot.status !== 'idle' && aot.status !== 'compiling' && aot.source !== code;
	const activeAotTab = AOT_TABS.find((t) => t.id === tab);
	const visibleAotTabs = AOT_TABS.filter((t) => t.primary || moreTabs || t.id === tab);
	const hiddenAotTabs = AOT_TABS.some((t) => !t.primary && !(moreTabs || t.id === tab));

	const aotBody = (() => {
		if (!activeAotTab) return null;
		if (aot.status === 'idle' || (aot.status === 'compiling' && aot.source === code && !stale)) {
			return (
				<Empty>
					{aot.status === 'compiling' ? (
						<>
							<Spinner /> compiling…
						</>
					) : (
						'run to compile'
					)}
				</Empty>
			);
		}
		if (aot.status === 'error') return <Empty tone="error">{aot.message}</Empty>;
		if (aot.status === 'compiling')
			return (
				<Empty>
					<Spinner /> compiling…
				</Empty>
			);
		return <CodeView code={aot[activeAotTab.id]} language={activeAotTab.language} className="h-full" />;
	})();

	return (
		<div className="flex flex-col h-full rounded-lg border border-rpd-text/15 dark:border-rp-overlay overflow-hidden bg-rpd-base dark:bg-rp-base">
			<div className="flex items-center shrink-0 px-3 py-1.5 bg-rpd-overlay/60 dark:bg-[#13111e] border-b border-rpd-text/10 dark:border-rp-overlay">
				<span className={`text-xs ${running ? rainbowText : 'text-rpd-muted dark:text-rp-subtle'}`}>
					{vm.kind === 'loading' && 'Loading AtomVM…'}
					{vm.kind === 'warming' && 'Warming up…'}
					{vm.kind === 'error' && 'Unavailable'}
					{vm.kind === 'ready' &&
						(running
							? `Running ${(elapsed / 1000).toFixed(1)}s`
							: didRun
								? `Done ${(elapsed / 1000).toFixed(1)}s`
								: vm.warm
									? 'Ready'
									: 'Ready · warming up caches…')}
				</span>
				<div className="flex items-center gap-1.5 ml-auto">
					<AboutDialog
						trigger={
							<button
								aria-label="How this page works"
								title="How this page works"
								className="flex items-center justify-center h-8 w-8 text-xs rounded-md text-rpd-muted dark:text-rp-subtle border border-rpd-text/10 dark:border-rp-overlay hover:border-rpd-text/25 dark:hover:border-rp-subtle/40 hover:text-rpd-subtle dark:hover:text-rp-text cursor-pointer transition-all duration-150"
							>
								?
							</button>
						}
					/>
					<Select.Root
						defaultValue="0"
						onValueChange={(value) => {
							const ex = examples[Number(value)];
							if (ex) loadExample(ex.code);
						}}
					>
						<Select.Trigger className="inline-flex items-center gap-1.5 px-2.5 h-8 text-xs rounded-md text-rpd-muted dark:text-rp-subtle bg-rpd-base/60 dark:bg-rp-base/40 border border-rpd-text/10 dark:border-rp-overlay hover:border-rpd-text/25 dark:hover:border-rp-subtle/40 hover:text-rpd-subtle dark:hover:text-rp-text cursor-pointer outline-none transition-all duration-150 data-[state=open]:border-rpd-text/25 dark:data-[state=open]:border-rp-subtle/40">
							<Select.Value />
							<Select.Icon>
								<svg
									width="12"
									height="12"
									viewBox="0 0 12 12"
									fill="none"
									stroke="currentColor"
									strokeWidth="1.5"
									strokeLinecap="round"
									strokeLinejoin="round"
									className="opacity-50"
								>
									<path d="M3 4.5l3 3 3-3" />
								</svg>
							</Select.Icon>
						</Select.Trigger>
						<Select.Portal>
							<Select.Content
								position="popper"
								sideOffset={6}
								className="z-50 min-w-[var(--radix-select-trigger-width)] max-h-64 overflow-hidden rounded-lg border border-rpd-text/15 dark:border-rp-overlay bg-rpd-surface dark:bg-rp-surface shadow-lg shadow-black/10 dark:shadow-black/30 data-[state=open]:animate-[select-in_150ms_ease-out] data-[state=closed]:animate-[select-out_100ms_ease-in]"
							>
								<Select.Viewport className="p-1">
									{examples.map((ex, i) => (
										<Select.Item
											key={i}
											value={String(i)}
											className="relative flex items-center px-2.5 py-1.5 text-xs rounded-md text-rpd-subtle dark:text-rp-subtle outline-none cursor-pointer select-none data-[highlighted]:bg-rpd-overlay/80 dark:data-[highlighted]:bg-rp-overlay data-[highlighted]:text-rpd-text dark:data-[highlighted]:text-rp-text data-[state=checked]:text-rpd-text dark:data-[state=checked]:text-rp-text transition-colors duration-100"
										>
											<Select.ItemText>{ex.name}</Select.ItemText>
										</Select.Item>
									))}
								</Select.Viewport>
							</Select.Content>
						</Select.Portal>
					</Select.Root>
					<button
						onClick={run}
						disabled={vm.kind !== 'ready' || running}
						aria-label={running ? 'Running code' : 'Run code'}
						className="flex items-center gap-1.5 px-3 py-1.5 h-8 text-sm rounded-md bg-rpd-text text-rpd-surface dark:bg-[#E0DEF4] dark:text-rp-base disabled:opacity-40 cursor-pointer font-medium"
					>
						{running && <Spinner />} run{' '}
						<kbd className="px-1 py-0.5 text-xs rounded bg-current/15 border border-current/20 font-mono leading-none">
							{getIsMac() ? <>⌘⏎</> : 'Ctrl⏎'}
						</kbd>
					</button>
				</div>
			</div>

			<div ref={editorRef} className="h-[300px] lg:h-auto lg:flex-[3] min-h-0" />

			<div className="flex flex-col shrink-0 h-[280px] lg:h-auto lg:flex-[2] min-h-0 border-t border-rpd-text/10 dark:border-rp-overlay bg-rpd-surface dark:bg-[#13111e]">
				<div className="flex items-center gap-0.5 shrink-0 px-2 border-b border-rpd-text/10 dark:border-rp-overlay text-xs">
					<TabButton active={tab === 'output'} onClick={() => selectTab('output')}>
						Output
					</TabButton>
					{visibleAotTabs.map((t) => (
						<TabButton key={t.id} active={tab === t.id} onClick={() => selectTab(t.id)}>
							{t.label}
						</TabButton>
					))}
					{hiddenAotTabs && (
						<button
							onClick={() => setMoreTabs(true)}
							aria-label="Show Core Erlang and IR tabs"
							title="Core Erlang · IR"
							className="px-2 py-2 text-rpd-muted dark:text-rp-muted hover:text-rpd-text dark:hover:text-rp-text cursor-pointer transition-colors"
						>
							⋯
						</button>
					)}
					<span className="ml-auto pl-2 text-rpd-muted dark:text-rp-muted whitespace-nowrap">
						{isAotTab && aot.status === 'compiling' && <span className={rainbowText}>compiling…</span>}
						{isAotTab && aot.status === 'done' && !stale && `compiled in ${(aot.ms / 1000).toFixed(1)}s`}
						{isAotTab && aot.status !== 'compiling' && stale && (
							<button
								onClick={() => compile(codeRef.current)}
								className="underline underline-offset-2 decoration-dotted hover:text-rpd-text dark:hover:text-rp-text cursor-pointer"
							>
								source changed — recompile
							</button>
						)}
					</span>
				</div>

				<div className="flex-1 min-h-0 relative">
					{tab === 'output' ? (
						<pre
							ref={outputRef}
							className="m-0 h-full overflow-auto text-rpd-subtle dark:text-rp-subtle font-mono text-xs"
						>
							{vm.kind === 'error' ? (
								<div className="h-full flex items-center justify-center p-6 whitespace-normal">
									<div className="flex items-start gap-3 max-w-[60ch] text-left">
										<svg
											className="shrink-0 mt-0.5 text-rpd-love dark:text-rp-love"
											width="14"
											height="14"
											viewBox="0 0 16 16"
											fill="none"
											stroke="currentColor"
											strokeWidth="1.5"
											strokeLinecap="round"
											strokeLinejoin="round"
											aria-hidden="true"
										>
											<path d="M8 1.5l6.5 11.5H1.5L8 1.5zM8 6v3.5M8 12h.01" />
										</svg>
										<div className="text-xs leading-relaxed">
											<p className="text-rpd-text dark:text-rp-text">{vm.message}.</p>
											<p className="mt-1 text-rpd-muted dark:text-rp-muted">{vm.detail}</p>
										</div>
									</div>
								</div>
							) : output.length === 0 ? (
								<Empty>{didRun || running ? '' : 'run to see output'}</Empty>
							) : (
								<div className="p-3 flex flex-col gap-0.5">
									<AnimatePresence initial={false}>
										{output.map((line, i) => (
											<motion.div
												key={line.id}
												initial={{ opacity: 0, filter: 'blur(4px)', height: 0 }}
												animate={{ opacity: 1, filter: 'blur(0px)', height: 'auto' }}
												transition={{ duration: 0.25, ease: [0.23, 1, 0.32, 1] }}
												className="will-change-[filter] overflow-hidden flex"
											>
												<span className="select-none text-rpd-muted/50 dark:text-rp-muted/50 mr-3 tabular-nums">
													{i + 1}
												</span>
												<LineMarker kind={line.kind} />
												<span
													className={`whitespace-pre-wrap break-words ${
														line.kind === 'error'
															? 'text-rpd-love dark:text-rp-love'
															: line.kind === 'result'
																? 'text-rpd-iris dark:text-rp-iris'
																: ''
													}`}
												>
													{line.text}
												</span>
											</motion.div>
										))}
									</AnimatePresence>
								</div>
							)}
						</pre>
					) : (
						aotBody
					)}
				</div>
			</div>
		</div>
	);
}

/** Glyph-free prefix so it renders the same in every font: a chevron for a value, a cross for an error. */
function LineMarker({ kind }: { kind: OutputLine['kind'] }) {
	if (kind === 'stdout') return null;
	const cls = `shrink-0 self-center mr-1.5 ${kind === 'error' ? 'text-rpd-love dark:text-rp-love' : 'text-rpd-iris dark:text-rp-iris'}`;
	return kind === 'error' ? (
		<svg
			className={cls}
			width="10"
			height="10"
			viewBox="0 0 10 10"
			fill="none"
			stroke="currentColor"
			strokeWidth="1.5"
			strokeLinecap="round"
			aria-label="error"
		>
			<path d="M2 2l6 6M8 2l-6 6" />
		</svg>
	) : (
		<svg
			className={cls}
			width="10"
			height="10"
			viewBox="0 0 10 10"
			fill="none"
			stroke="currentColor"
			strokeWidth="1.5"
			strokeLinecap="round"
			strokeLinejoin="round"
			aria-label="result"
		>
			<path d="M3.5 1.5L7 5l-3.5 3.5" />
		</svg>
	);
}

function TabButton({ active, onClick, children }: { active: boolean; onClick: () => void; children: ReactNode }) {
	return (
		<button
			onClick={onClick}
			className={`relative px-2.5 py-2 cursor-pointer transition-colors ${
				active
					? 'text-rpd-text dark:text-rp-text after:absolute after:left-2 after:right-2 after:-bottom-px after:h-px after:bg-rpd-text dark:after:bg-rp-text'
					: 'text-rpd-muted dark:text-rp-subtle hover:text-rpd-text dark:hover:text-rp-text'
			}`}
		>
			{children}
		</button>
	);
}

function Empty({ children, tone = 'muted' }: { children: ReactNode; tone?: 'muted' | 'error' }) {
	return (
		<div
			className={`h-full flex items-center justify-center gap-2 p-4 text-xs text-center whitespace-pre-wrap ${
				tone === 'error' ? 'text-rpd-love dark:text-rp-love' : 'text-rpd-muted dark:text-rp-muted'
			}`}
		>
			{children}
		</div>
	);
}
