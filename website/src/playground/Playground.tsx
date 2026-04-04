import { useState, useRef, useEffect, useCallback } from 'react';
import { motion, AnimatePresence } from 'motion/react';
import { useAtomVM } from './use-atomvm';
import { EditorView, keymap, placeholder } from '@codemirror/view';
import { EditorState, Compartment } from '@codemirror/state';
import { javascript } from '@codemirror/lang-javascript';
import { defaultKeymap, history, historyKeymap } from '@codemirror/commands';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { tags } from '@lezer/highlight';

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

export function Playground() {
	const [code, setCode] = useState(EXAMPLE);
	const [output, setOutput] = useState<{ id: number; text: string }[]>([]);
	const [running, setRunning] = useState(false);
	const [didRun, setDidRun] = useState(false);
	const nextId = useRef(0);
	const editorRef = useRef<HTMLDivElement>(null);
	const viewRef = useRef<EditorView | null>(null);
	const codeRef = useRef(code);
	const runRef = useRef<() => void>(() => {});

	if (running && !didRun) setDidRun(true);

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

	return (
		<div className="rounded-lg border border-neutral-200 dark:border-[#26233a] overflow-hidden">
			<div className="flex items-center justify-between px-3 py-1.5 bg-neutral-50 dark:bg-[#13111e] border-b border-neutral-200 dark:border-[#26233a]">
				<span className="text-xs text-neutral-500 dark:text-[#908caa]">
					{vm.kind === 'loading' && 'Loading AtomVM…'}
					{vm.kind === 'error' && `error: ${vm.message}`}
					{vm.kind === 'ready' && (running ? 'Running' : didRun ? 'Idle' : 'Ready')}
				</span>
				<div className="flex items-center gap-2">
					<span className="text-xs text-neutral-400 dark:text-[#6e6a86] hidden sm:inline">⌘↵ to run</span>
					<button
						onClick={run}
						disabled={vm.kind !== 'ready' || running}
						className="px-2.5 py-0.5 text-xs rounded bg-neutral-900 text-neutral-100 dark:bg-[#e0def4] dark:text-[#191724] disabled:opacity-40 cursor-pointer"
					>
						{running ? '…' : 'run'}
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
						className="m-0 bg-neutral-50 text-neutral-600 dark:bg-[#13111e] dark:text-[#908caa] font-mono text-xs border-t border-neutral-200 dark:border-[#26233a] max-h-40 overflow-auto"
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
