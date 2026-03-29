const kw = 'text-sky-600 dark:text-sky-300';
const id = 'text-rose-600 dark:text-rose-400';
const str = 'text-amber-600 dark:text-yellow-200';
const interp = 'text-rose-600 dark:text-rose-400';
const dim = 'text-neutral-400 dark:text-neutral-500';
const prompt = 'text-rose-600 dark:text-rose-400';
const cmd = 'text-sky-600 dark:text-sky-300';

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
		<div className="rounded-xl bg-neutral-50 dark:bg-neutral-900/50 p-6 font-mono text-sm leading-relaxed text-neutral-700 dark:text-neutral-200 overflow-x-auto">
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
