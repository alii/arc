const kw = 'text-rpd-love dark:text-rp-love';
const id = 'text-rpd-iris dark:text-rp-iris';
const str = 'text-rpd-gold dark:text-rp-gold';
const interp = 'text-rpd-rose dark:text-rp-rose';
const dim = 'text-rpd-muted dark:text-rp-muted';
const prompt = 'text-rpd-rose dark:text-rp-rose';
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
				<span className={kw}>const</span> <span className={id}>pid</span> = Arc.spawn(() =&gt; {'{'}
			</Line>
			<Line indent>
				{'    '}
				<span className={kw}>const</span> <span className={id}>message</span> = Arc.receive();
			</Line>
			<Line indent>{'    '}Arc.log(message);</Line>
			<Line indent>
				{'    '}Arc.log(
				<Tmpl>
					Arc.self()<span className={str}>: Hello from child</span>
				</Tmpl>
				);
			</Line>
			<Line indent>{'}'});</Line>
			<Line indent>
				Arc.send(pid,{' '}
				<Tmpl>
					Arc.self()<span className={str}>: Hello from main</span>
				</Tmpl>
				);
			</Line>
			<Line> </Line>
			<Line>
				<span className={dim}>$ </span>
				<span className={cmd}>arc</span> ./example.js
			</Line>
			<Line indent>Pid&lt;0.82.0&gt;: Hello from main</Line>
			<Line indent>Pid&lt;0.83.0&gt;: Hello from child</Line>
		</div>
	);
}
