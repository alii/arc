/**
 * Render the README hero image — a styled terminal session — in both the
 * Rose Pine (dark) and Rose Pine Dawn (light) themes, matching the website's
 * <HeroCode> component (same colours, same Iosevka Curly font).
 *
 * Pipeline: build element tree -> satori (SVG) -> resvg (PNG) -> sharp (trim).
 *
 *   cd .github/scripts/hero && bun install && bun run generate
 */
import { Resvg } from '@resvg/resvg-js';
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import satori from 'satori';
import sharp from 'sharp';
import wawoff2 from 'wawoff2';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, '..', '..', '..');
const FONT_WOFF2 = resolve(ROOT, 'website/public/fonts/iosevka-curly.woff2');

// --- layout (logical px; ZOOM scales the final raster) --------------------
const FS = 17; // font size
const LH = 1.7; // line height (≈ tailwind leading-relaxed)
const PAD_X = 36;
const PAD_Y = 30;
const RADIUS = 14;
const ZOOM = 3.5;

// --- Rose Pine palettes (mirror website/src/index.css) --------------------
type Palette = {
	bg: string;
	text: string;
	muted: string;
	foam: string;
	love: string;
	iris: string;
	rose: string;
	gold: string;
};
const DARK: Palette = {
	bg: '#1f1d2e', // surface
	text: '#e0def4',
	muted: '#6e6a86',
	foam: '#9ccfd8',
	love: '#eb6f92',
	iris: '#c4a7e7',
	rose: '#ebbcba',
	gold: '#f6c177',
};
const LIGHT: Palette = {
	bg: '#fffaf3', // surface
	text: '#575279',
	muted: '#9893a5',
	foam: '#56949f',
	love: '#b4637a',
	iris: '#907aa9',
	rose: '#d7827e',
	gold: '#ea9d34',
};

// --- content --------------------------------------------------------------
// Each segment is [paletteKey | null, text]; null inherits the default text
// colour. `blank` is a spacer line.
type Key = keyof Palette;
type Seg = [Key | null, string];
type Line = { segs: Seg[] } | 'blank';

// Highlighting roles (Rose Pine): namespace (Arc) -> foam, method calls
// -> rose, keywords -> love, strings/numbers -> gold, commands -> foam,
// prompt -> muted. Variables/punctuation/operators stay default text.
const LINES: Line[] = [
	{
		segs: [
			['muted', '$ '],
			['foam', 'cat'],
			[null, ' ./example.js'],
		],
	},
	{
		segs: [
			['love', 'const'],
			[null, ' '],
			[null, 'inbox'],
			[null, ' = '],
			['foam', 'Arc'],
			[null, '.'],
			['rose', 'subject'],
			[null, '();'],
		],
	},
	{
		segs: [
			['foam', 'Arc'],
			[null, '.'],
			['rose', 'spawn'],
			[null, '(() => {'],
		],
	},
	{
		segs: [
			[null, '  '],
			[null, 'inbox'],
			[null, '.'],
			['rose', 'send'],
			[null, '('],
			['gold', '`'],
			[null, '${'],
			['foam', 'Arc'],
			[null, '.'],
			['rose', 'self'],
			[null, '()}'],
			['gold', ': Hello from child`'],
			[null, ');'],
		],
	},
	{ segs: [[null, '});']] },
	{
		segs: [
			['foam', 'Arc'],
			[null, '.'],
			['rose', 'log'],
			[null, '('],
			[null, 'inbox'],
			[null, '.'],
			['rose', 'receive'],
			[null, '());'],
		],
	},
	'blank',
	{
		segs: [
			['muted', '$ '],
			['foam', 'arc'],
			[null, ' ./example.js'],
		],
	},
	{
		segs: [
			[null, 'Pid<'],
			['gold', '0.83.0'],
			[null, '>: Hello from child'],
		],
	},
];

// --- element tree (no JSX; satori reads {type, props}) --------------------
function h(type: string, props: Record<string, unknown>, children?: unknown): unknown {
	return { type, props: { ...props, children } };
}

function lineEl(line: Line, pal: Palette): unknown {
	if (line === 'blank') {
		return h('div', { style: { display: 'flex', height: `${LH}em` } }, ' ');
	}
	const spans = line.segs.map(([key, text]) =>
		h('span', { style: { color: key ? pal[key] : pal.text, whiteSpace: 'pre' } }, text),
	);
	return h('div', { style: { display: 'flex', flexDirection: 'row', whiteSpace: 'pre' } }, spans);
}

function card(pal: Palette): unknown {
	return h(
		'div',
		{
			style: {
				display: 'flex',
				flexDirection: 'column',
				backgroundColor: pal.bg,
				borderRadius: RADIUS,
				padding: `${PAD_Y}px ${PAD_X}px`,
				fontFamily: 'Iosevka Curly',
				fontSize: FS,
				lineHeight: LH,
				color: pal.text,
			},
		},
		LINES.map((l) => lineEl(l, pal)),
	);
}

// root: large transparent canvas, card pinned top-left, trimmed afterwards.
function root(pal: Palette): unknown {
	return h(
		'div',
		{
			style: {
				display: 'flex',
				alignItems: 'flex-start',
				justifyContent: 'flex-start',
				width: 1600,
				height: 900,
			},
		},
		card(pal),
	);
}

async function main() {
	const ttf = await wawoff2.decompress(readFileSync(FONT_WOFF2));
	const fonts = [{ name: 'Iosevka Curly', data: ttf as Buffer, weight: 400 as const, style: 'normal' as const }];

	for (const [pal, out] of [
		[DARK, resolve(ROOT, '.github/js.png')],
		[LIGHT, resolve(ROOT, '.github/js-light.png')],
	] as const) {
		const svg = await satori(root(pal) as never, { width: 1600, height: 900, fonts });
		const png = new Resvg(svg, {
			fitTo: { mode: 'zoom', value: ZOOM },
			background: 'rgba(0,0,0,0)',
		})
			.render()
			.asPng();
		const trimmed = await sharp(png).trim().png().toBuffer();
		writeFileSync(out, trimmed);
		const meta = await sharp(trimmed).metadata();
		console.log(`Wrote ${out}  (${meta.width}x${meta.height})`);
	}
}

main().catch((e) => {
	console.error(e);
	process.exit(1);
});
