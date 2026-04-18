const kw = 'text-rpd-love dark:text-rp-love';
const id = 'text-rpd-iris dark:text-rp-iris';
const str = 'text-rpd-gold dark:text-rp-gold';
const interp = 'text-rpd-rose dark:text-rp-rose';
const dim = 'text-rpd-muted dark:text-rp-muted';
const cmd = 'text-rpd-foam dark:text-rp-foam';

function Line({ children, indent = false }: { children: React.ReactNode; indent?: boolean }) {
	return <div className={'whitespace-pre' + (indent ? ' pl-8' : '')}>{children}</div>;
}

function Tmpl({ children }: { children: React.ReactNode }) {
	return (
		<span className={str}>
			`<span className={interp}>${'{'}</span>
			{children}
			<span className={interp}>{'}'}</span>`
		</span>
	);
}

export function HeroCode() {
	return (
		<div className="rounded-xl bg-rpd-surface dark:bg-rp-surface p-6 font-mono text-sm leading-relaxed text-rpd-text dark:text-rp-text overflow-x-auto">
			<Line>
				<span className={dim}>$ </span>
				<span className={cmd}>cat</span> ./example.js
			</Line>
			<Line indent>
				<span className={kw}>const</span> <span className={id}>inbox</span> = Arc.subject();
			</Line>
			<Line indent>
				Arc.spawn(() =&gt; {'{'}
			</Line>
			<Line indent>
				{'    '}inbox.send(
				<Tmpl>
					Arc.self()<span className={str}>: Hello from child</span>
				</Tmpl>
				);
			</Line>
			<Line indent>{'}'});</Line>
			<Line indent>Arc.log(inbox.receive());</Line>
			<Line> </Line>
			<Line>
				<span className={dim}>$ </span>
				<span className={cmd}>arc</span> ./example.js
			</Line>
			<Line indent>Pid&lt;0.83.0&gt;: Hello from child</Line>
		</div>
	);
}
