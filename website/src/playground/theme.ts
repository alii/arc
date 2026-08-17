import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { EditorView } from '@codemirror/view';
import { tags } from '@lezer/highlight';

// Rose Pine
export const rp = {
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
export const rpd = {
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

type Palette = typeof rp;

function highlight(p: Palette) {
	return HighlightStyle.define([
		{ tag: tags.keyword, color: p.love },
		{ tag: tags.operator, color: p.rose },
		{ tag: tags.variableName, color: p.text },
		{ tag: tags.propertyName, color: p.foam },
		{ tag: tags.function(tags.variableName), color: p.rose },
		{ tag: tags.function(tags.propertyName), color: p.foam },
		{ tag: tags.string, color: p.gold },
		{ tag: tags.number, color: p.iris },
		{ tag: tags.bool, color: p.iris },
		{ tag: tags.null, color: p.love },
		{ tag: tags.atom, color: p.iris },
		{ tag: tags.comment, color: p.muted },
		{ tag: tags.paren, color: p.subtle },
		{ tag: tags.brace, color: p.subtle },
		{ tag: tags.bracket, color: p.subtle },
		{ tag: tags.punctuation, color: p.subtle },
		{ tag: tags.definition(tags.variableName), color: p.iris },
	]);
}

const darkHighlight = highlight(rp);
const lightHighlight = highlight(rpd);

/** Fills its container; the container decides the height and this scrolls inside it. */
export const baseTheme = EditorView.theme({
	'&': {
		fontSize: '13px',
		height: '100%',
	},
	'&, .cm-content': {
		fontFamily: '"Iosevka Curly", ui-monospace, monospace',
	},
	'.cm-scroller': {
		overflow: 'auto',
		lineHeight: '1.55',
	},
	'.cm-content': {
		padding: '14px 0',
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

function colours(p: Palette, dark: boolean) {
	return EditorView.theme(
		{
			'&': {
				backgroundColor: p.base,
				color: p.text,
			},
			'.cm-content': {
				caretColor: p.text,
			},
			'.cm-selectionBackground': {
				backgroundColor: `${p.overlay} !important`,
			},
			'&.cm-focused .cm-selectionBackground': {
				backgroundColor: `${p.overlay} !important`,
			},
			'.cm-cursor': {
				borderLeftColor: p.text,
			},
		},
		{ dark },
	);
}

const darkTheme = colours(rp, true);
const lightTheme = colours(rpd, false);

/** The theme + highlighting for one colour scheme; swap via a Compartment on scheme change. */
export function themeExtensions(dark: boolean) {
	return [dark ? darkTheme : lightTheme, syntaxHighlighting(dark ? darkHighlight : lightHighlight)];
}

export function getIsDark() {
	return window.matchMedia('(prefers-color-scheme: dark)').matches;
}

/** Calls `onChange(isDark)` whenever the OS colour scheme flips; returns the unsubscribe. */
export function watchColorScheme(onChange: (dark: boolean) => void) {
	const mq = window.matchMedia('(prefers-color-scheme: dark)');
	const handler = () => onChange(getIsDark());
	mq.addEventListener('change', handler);
	return () => mq.removeEventListener('change', handler);
}
