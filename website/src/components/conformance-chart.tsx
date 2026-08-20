import history from 'virtual:test262-history';
import { useEffect, useId, useMemo, useRef, useState } from 'react';

type Point = { x: number; y: number };

const pad = { top: 16, right: 16, bottom: 26, left: 16 };
const grid = [0, 25, 50, 75, 100];

const day = (iso: string) => Date.parse(iso + 'T00:00:00Z');
const fmtDate = (iso: string) =>
	new Date(iso + 'T00:00:00Z').toLocaleDateString('en', { month: 'short', day: 'numeric', timeZone: 'UTC' });
const fmtInt = (n: number) => n.toLocaleString('en');

/** Monotone cubic (Fritsch–Carlson) through `pts`, so the curve never overshoots a sample. */
function smoothPath(pts: Point[]): string {
	if (pts.length < 2) return '';
	const n = pts.length;
	const dx: number[] = [];
	const slope: number[] = [];
	for (let i = 0; i < n - 1; i++) {
		dx.push(pts[i + 1].x - pts[i].x);
		slope.push((pts[i + 1].y - pts[i].y) / dx[i]);
	}
	const tangent: number[] = [slope[0]];
	for (let i = 1; i < n - 1; i++) {
		if (slope[i - 1] * slope[i] <= 0) tangent.push(0);
		else {
			const w1 = 2 * dx[i] + dx[i - 1];
			const w2 = dx[i] + 2 * dx[i - 1];
			tangent.push((w1 + w2) / (w1 / slope[i - 1] + w2 / slope[i]));
		}
	}
	tangent.push(slope[n - 2]);
	let d = `M${pts[0].x},${pts[0].y}`;
	for (let i = 0; i < n - 1; i++) {
		const h = dx[i] / 3;
		d += ` C${pts[i].x + h},${pts[i].y + tangent[i] * h} ${pts[i + 1].x - h},${pts[i + 1].y - tangent[i + 1] * h} ${pts[i + 1].x},${pts[i + 1].y}`;
	}
	return d;
}

/** The element's content width in CSS pixels, so the SVG is drawn 1:1 and text never scales. */
function useWidth(ref: React.RefObject<HTMLElement | null>, initial: number): number {
	const [width, setWidth] = useState(initial);
	useEffect(() => {
		const el = ref.current;
		if (!el) return;
		const ro = new ResizeObserver(([entry]) => setWidth(Math.round(entry.contentRect.width)));
		ro.observe(el);
		return () => ro.disconnect();
	}, [ref]);
	return width;
}

// Fill and line are iris pulled toward the page so they sit in the theme:
// the fill more (darker), the line only slightly.
const fillStop =
	'[stop-color:color-mix(in_oklch,var(--color-rpd-iris)_65%,var(--color-rpd-text))] dark:[stop-color:color-mix(in_oklch,var(--color-rp-iris)_55%,var(--color-rp-base))]';
const lineStroke =
	'[stroke:color-mix(in_oklch,var(--color-rpd-iris)_85%,var(--color-rpd-text))] dark:[stroke:color-mix(in_oklch,var(--color-rp-iris)_82%,var(--color-rp-base))]';

export function ConformanceChart() {
	const id = useId().replace(/:/g, '');
	const wrapRef = useRef<HTMLElement>(null);
	const [hover, setHover] = useState<number | null>(null);

	const W = useWidth(wrapRef, 600);
	const H = W < 420 ? 170 : 220;
	const innerW = W - pad.left - pad.right;
	const innerH = H - pad.top - pad.bottom;

	const { pts, line, area } = useMemo(() => {
		const t0 = day(history[0].date);
		const t1 = day(history[history.length - 1].date);
		const span = Math.max(1, t1 - t0);
		const pts = history.map((h) => ({
			x: pad.left + ((day(h.date) - t0) / span) * innerW,
			y: pad.top + (1 - h.percent / 100) * innerH,
		}));
		const line = smoothPath(pts);
		const last = pts[pts.length - 1];
		const area = `${line} L${last.x},${pad.top + innerH} L${pts[0].x},${pad.top + innerH} Z`;
		return { pts, line, area };
	}, [innerW, innerH]);

	const latest = history[history.length - 1];
	const shown = history[hover ?? history.length - 1];
	const at = pts[hover ?? pts.length - 1];

	function onMove(e: React.PointerEvent<SVGSVGElement>) {
		const rect = e.currentTarget.getBoundingClientRect();
		const x = e.clientX - rect.left;
		let best = 0;
		for (let i = 1; i < pts.length; i++) {
			if (Math.abs(pts[i].x - x) < Math.abs(pts[best].x - x)) best = i;
		}
		setHover(best);
	}

	return (
		<figure ref={wrapRef} className="mt-3">
			<div className="flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1 text-sm">
				<p>
					<span className="text-2xl font-semibold tabular-nums text-rpd-text dark:text-rp-text">
						{shown.percent.toFixed(2)}%
					</span>{' '}
					<span className="text-rpd-muted dark:text-rp-muted tabular-nums">
						{fmtInt(shown.pass)} / {fmtInt(shown.tested)} passing
					</span>
				</p>
				<p className="text-rpd-muted dark:text-rp-muted tabular-nums">
					{hover === null ? `as of ${fmtDate(latest.date)}` : fmtDate(shown.date)}
				</p>
			</div>

			<svg
				width={W}
				height={H}
				viewBox={`0 0 ${W} ${H}`}
				className="mt-2 block max-w-full overflow-visible touch-pan-y select-none"
				role="img"
				aria-label={`test262 conformance over time, currently ${latest.percent}%`}
				onPointerMove={onMove}
				onPointerDown={onMove}
				onPointerLeave={() => setHover(null)}
			>
				<defs>
					<linearGradient id={`${id}-fill`} x1="0" y1="0" x2="0" y2="1">
						<stop offset="0%" className={fillStop} stopOpacity="0.55" />
						<stop offset="60%" className={fillStop} stopOpacity="0.14" />
						<stop offset="100%" className={fillStop} stopOpacity="0" />
					</linearGradient>
					<filter id={`${id}-glow`} x="-5%" y="-30%" width="110%" height="160%">
						<feGaussianBlur stdDeviation="3" />
					</filter>
					<clipPath id={`${id}-reveal`}>
						<rect x="0" y="0" width={W} height={H} className="origin-left animate-chart-reveal" />
					</clipPath>
				</defs>

				{grid.map((g) => {
					const y = pad.top + (1 - g / 100) * innerH;
					return (
						<g key={g}>
							<line
								x1={pad.left}
								x2={W - pad.right}
								y1={y}
								y2={y}
								strokeDasharray={g === 0 ? undefined : '2 4'}
								className="stroke-rpd-overlay dark:stroke-rp-overlay"
							/>
							{g !== 0 && (
								<text x={W - pad.right} y={y - 4} textAnchor="end" className="fill-rpd-muted dark:fill-rp-muted text-[10px]">
									{g}%
								</text>
							)}
						</g>
					);
				})}

				<g clipPath={`url(#${id}-reveal)`}>
					<path d={area} fill={`url(#${id}-fill)`} />
					<path
						d={line}
						fill="none"
						strokeWidth="4"
						strokeLinecap="round"
						opacity="0.25"
						filter={`url(#${id}-glow)`}
						className={lineStroke}
					/>
					<path
						d={line}
						fill="none"
						strokeWidth="2.25"
						strokeLinecap="round"
						strokeLinejoin="round"
						className={lineStroke}
					/>
				</g>

				<text x={pad.left} y={H - 8} className="fill-rpd-muted dark:fill-rp-muted text-[10px]">
					{fmtDate(history[0].date)}
				</text>
				<text x={W - pad.right} y={H - 8} textAnchor="end" className="fill-rpd-muted dark:fill-rp-muted text-[10px]">
					{fmtDate(latest.date)}
				</text>

				{hover !== null && (
					<line
						x1={at.x}
						x2={at.x}
						y1={pad.top}
						y2={pad.top + innerH}
						className="stroke-rpd-muted/50 dark:stroke-rp-muted/50"
					/>
				)}
				<g transform={`translate(${at.x} ${at.y})`}>
					<circle r="7" className="fill-rpd-iris/30 dark:fill-rp-iris/30">
						{hover === null && (
							<animate attributeName="r" values="4;9;4" dur="2.4s" repeatCount="indefinite" />
						)}
					</circle>
					<circle
						r="3"
						strokeWidth="2"
						className="fill-rpd-base stroke-rpd-text dark:fill-rp-base dark:stroke-rp-text"
					/>
				</g>
			</svg>
		</figure>
	);
}
