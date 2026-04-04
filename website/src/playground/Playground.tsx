import { useState, useRef, useEffect, useCallback } from 'react';
import { motion, AnimatePresence } from 'motion/react';
import { useAtomVM } from './use-atomvm';
import { EditorView, keymap, placeholder } from '@codemirror/view';
import { EditorState, Compartment } from '@codemirror/state';
import { javascript } from '@codemirror/lang-javascript';
import { defaultKeymap, history, historyKeymap } from '@codemirror/commands';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import gitExamples from 'virtual:examples';

const DEFAULT_EXAMPLE = {
	name: 'playground',
	code: `const parent = Arc.self();
Arc.log("starting...");

for (let i = 0; i < 3; i++) {
  Arc.spawn(() => {
    Arc.sleep(300 * (i + 1));
    Arc.send(parent, "hello from process " + i);
  });
}

for (let i = 0; i < 3; i++) {
  Arc.log(Arc.receive());
}`,
};

const examples = [DEFAULT_EXAMPLE, ...gitExamples];

// Rose Pine
const rp = {
	base: '#191724',
	surface: '#1f1d2e',
	overlay: '#26233a',
	muted: '#6e6a86',
	subtle: '#908caa',
	text: '#e0def4',
	love: '#eb6f92',
	gold: '#f6c177',
	rose: '#ebbcba',
	pine: '#31748f',
	foam: '#9ccfd8',
	iris: '#c4a7e7',
};

// Rose Pine Dawn
const rpd = {
	base: '#faf4ed',
	surface: '#fffaf3',
	overlay: '#f2e9e1',
	muted: '#9893a5',
	subtle: '#797593',
	text: '#575279',
	love: '#b4637a',
	gold: '#ea9d34',
	rose: '#d7827e',
	pine: '#286983',
	foam: '#56949f',
	iris: '#907aa9',
};

const darkHighlight = HighlightStyle.define([
	{ tag: tags.keyword, color: rp.love },
	{ tag: tags.operator, color: rp.rose },
	{ tag: tags.variableName, color: rp.text },
	{ tag: tags.propertyName, color: rp.foam },
	{ tag: tags.function(tags.variableName), color: rp.rose },
	{ tag: tags.function(tags.propertyName), color: rp.foam },
	{ tag: tags.string, color: rp.gold },
	{ tag: tags.number, color: rp.iris },
	{ tag: tags.bool, color: rp.iris },
	{ tag: tags.null, color: rp.love },
	{ tag: tags.comment, color: rp.muted },
	{ tag: tags.paren, color: rp.subtle },
	{ tag: tags.brace, color: rp.subtle },
	{ tag: tags.bracket, color: rp.subtle },
	{ tag: tags.punctuation, color: rp.subtle },
	{ tag: tags.definition(tags.variableName), color: rp.iris },
]);

const lightHighlight = HighlightStyle.define([
	{ tag: tags.keyword, color: rpd.love },
	{ tag: tags.operator, color: rpd.rose },
	{ tag: tags.variableName, color: rpd.text },
	{ tag: tags.propertyName, color: rpd.foam },
	{ tag: tags.function(tags.variableName), color: rpd.rose },
	{ tag: tags.function(tags.propertyName), color: rpd.foam },
	{ tag: tags.string, color: rpd.gold },
	{ tag: tags.number, color: rpd.iris },
	{ tag: tags.bool, color: rpd.iris },
	{ tag: tags.null, color: rpd.love },
	{ tag: tags.comment, color: rpd.muted },
	{ tag: tags.paren, color: rpd.subtle },
	{ tag: tags.brace, color: rpd.subtle },
	{ tag: tags.bracket, color: rpd.subtle },
	{ tag: tags.punctuation, color: rpd.subtle },
	{ tag: tags.definition(tags.variableName), color: rpd.iris },
]);

const baseTheme = EditorView.theme({
	'&': {
		fontSize: '14px',
	},
	'&, .cm-content': {
		fontFamily: '"Iosevka Curly", ui-monospace, monospace',
	},
	'.cm-content': {
		padding: '16px 0',
	},
	'.cm-line': {
		padding: '0 16px',
	},
	'&.cm-focused': {
		outline: 'none',
	},
	'.cm-gutters': {
		display: 'none',
	},
	'.cm-activeLine': {
		backgroundColor: 'transparent',
	},
});

const darkTheme = EditorView.theme(
	{
		'&': {
			backgroundColor: rp.base,
			color: rp.text,
		},
		'.cm-content': {
			caretColor: rp.text,
		},
		'.cm-selectionBackground': {
			backgroundColor: `${rp.overlay} !important`,
		},
		'&.cm-focused .cm-selectionBackground': {
			backgroundColor: `${rp.overlay} !important`,
		},
		'.cm-cursor': {
			borderLeftColor: rp.text,
		},
	},
	{ dark: true },
);

const lightTheme = EditorView.theme(
	{
		'&': {
			backgroundColor: rpd.base,
			color: rpd.text,
		},
		'.cm-content': {
			caretColor: rpd.text,
		},
		'.cm-selectionBackground': {
			backgroundColor: `${rpd.overlay} !important`,
		},
		'&.cm-focused .cm-selectionBackground': {
			backgroundColor: `${rpd.overlay} !important`,
		},
		'.cm-cursor': {
			borderLeftColor: rpd.text,
		},
	},
	{ dark: false },
);

function getIsDark() {
	return window.matchMedia('(prefers-color-scheme: dark)').matches;
}

function getIsMac() {
	return /Mac|iPhone|iPad|iPod/.test(navigator.userAgent);
}

function Spinner() {
	return (
		<svg className="h-3.5 w-3.5 inline-block animate-spinner-fade" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round">
			{[...Array(8)].map((_, i) => {
				const angle = i * 45;
				const rad = (angle * Math.PI) / 180;
				const x1 = 12 + 6 * Math.cos(rad);
				const y1 = 12 + 6 * Math.sin(rad);
				const x2 = 12 + 10 * Math.cos(rad);
				const y2 = 12 + 10 * Math.sin(rad);
				return (
					<line key={i} x1={x1} y1={y1} x2={x2} y2={y2} opacity={1 - i * 0.1} style={{ animationDelay: `${i * -0.125}s` }} />
				);
			})}
		</svg>
	);
}

export function Playground() {
	const [code, setCode] = useState(examples[0]?.code ?? '');
	const [output, setOutput] = useState<{ id: number; text: string }[]>([]);
	const [running, setRunning] = useState(false);
	const [didRun, setDidRun] = useState(false);
	const [elapsed, setElapsed] = useState(0);
	const nextId = useRef(0);
	const editorRef = useRef<HTMLDivElement>(null);
	const viewRef = useRef<EditorView | null>(null);
	const codeRef = useRef(code);
	const runRef = useRef<() => void>(() => {});

	if (running && !didRun) setDidRun(true);

	useEffect(() => {
		if (!running) return;
		setElapsed(0);
		const start = performance.now();
		const id = setInterval(() => setElapsed(performance.now() - start), 32);
		return () => clearInterval(id);
	}, [running]);

	const push = (text: string) => setOutput((o) => [...o, { id: nextId.current++, text }]);

	const vm = useAtomVM(push);

	const run = useCallback(async () => {
		if (vm.kind !== 'ready') return;
		setOutput([]);
		setRunning(true);
		try {
			const result = await vm.vm.call('main', codeRef.current);
			push(`→ ${result}`);
		} catch (e) {
			push(`✗ ${e}`);
		} finally {
			setRunning(false);
		}
	}, [vm]);

	runRef.current = run;

	useEffect(() => {
		if (!editorRef.current) return;

		const isDark = getIsDark();
		const themeCompartment = new Compartment();

		const themeExts = (dark: boolean) => [
			dark ? darkTheme : lightTheme,
			syntaxHighlighting(dark ? darkHighlight : lightHighlight),
		];

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
				themeCompartment.of(themeExts(isDark)),
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

		const mq = window.matchMedia('(prefers-color-scheme: dark)');
		const handler = () => {
			view.dispatch({
				effects: themeCompartment.reconfigure(themeExts(getIsDark())),
			});
		};
		mq.addEventListener('change', handler);

		return () => {
			mq.removeEventListener('change', handler);
			view.destroy();
		};
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

	return (
		<div className="rounded-lg border border-rpd-overlay dark:border-rp-overlay overflow-hidden">
			<div className="flex items-center px-3 py-1.5 bg-rpd-overlay dark:bg-[#13111e] border-b border-rpd-overlay dark:border-rp-overlay">
				<span className={`text-xs ${running ? 'animate-rainbow bg-[length:200%_auto] bg-clip-text text-transparent bg-[linear-gradient(90deg,#eb6f92,#f6c177,#9ccfd8,#c4a7e7,#ebbcba,#31748f,#eb6f92)]' : 'text-rpd-muted dark:text-rp-subtle'}`}>
					{vm.kind === 'loading' && 'Loading AtomVM…'}
					{vm.kind === 'error' && `error: ${vm.message}`}
					{vm.kind === 'ready' && (running ? `Running ${(elapsed / 1000).toFixed(1)}s` : didRun ? `Done ${(elapsed / 1000).toFixed(1)}s` : 'Ready')}
				</span>
				<div className="flex items-center gap-1 ml-auto">
					<select
						onChange={(e) => {
							const ex = examples[Number(e.target.value)];
							if (ex) loadExample(ex.code);
						}}
						className="text-xs bg-transparent text-rpd-muted dark:text-rp-subtle border-none p-0 h-8 cursor-pointer outline-none"
					>
						{examples.map((ex, i) => (
							<option key={i} value={i}>{ex.name}</option>
						))}
					</select>
					<button
						onClick={run}
						disabled={vm.kind !== 'ready' || running}
						aria-label={running ? 'Running code' : 'Run code'}
						className="flex items-center gap-1.5 px-3 py-1.5 h-8 text-sm rounded-md bg-[#E0DEF4] text-rp-base disabled:opacity-40 cursor-pointer font-medium"
					>
						{running && <Spinner />} run <kbd className="px-1 py-0.5 text-xs rounded bg-rp-base/15 border border-rp-base/20 font-mono leading-none">{getIsMac() ? '⌘↵' : 'Ctrl↵'}</kbd>
					</button>
				</div>
			</div>

			<div ref={editorRef} />

			<AnimatePresence>
				{output.length > 0 && (
					<motion.pre
						initial={{ opacity: 0 }}
						animate={{ opacity: 1 }}
						exit={{ height: 0, opacity: 0 }}
						transition={{ duration: 0.2, ease: [0.25, 0.1, 0.25, 1] }}
						className="m-0 bg-rpd-surface text-rpd-subtle dark:bg-[#13111e] dark:text-rp-subtle font-mono text-xs border-t border-rpd-overlay dark:border-rp-overlay max-h-40 overflow-auto"
					>
						<div className="p-3 flex flex-col gap-0.5">
							{output.map((line, i) => (
								<motion.div
									key={line.id}
									initial={{ opacity: 0, filter: 'blur(4px)', height: 0 }}
									animate={{ opacity: 1, filter: 'blur(0px)', height: 'auto' }}
									transition={{ duration: 0.25, ease: [0.23, 1, 0.32, 1] }}
									className="will-change-[filter] overflow-hidden flex"
								>
									<span className="select-none text-rpd-muted/50 dark:text-rp-muted/50 mr-3 tabular-nums">{i + 1}</span>
									<span>{line.text}</span>
								</motion.div>
							))}
						</div>
					</motion.pre>
				)}
			</AnimatePresence>
		</div>
	);
}
