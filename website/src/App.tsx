import { Code } from './components/code';
import { ExternalLink } from './components/external-link';
import { HeroCode } from './components/hero-code';
import { Playground } from './playground/Playground';

export default function App() {
	return (
		<main className="flex flex-col gap-6 max-w-[600px] mx-auto px-5 py-16 lg:px-10 lg:py-16 leading-relaxed text-base">
			<div>
				<h1 className="text-lg font-semibold text-neutral-900 dark:text-neutral-100">
					arc <span className="align-top text-sm leading-none">⌒</span>
				</h1>
				<p className="mt-1">JavaScript on the BEAM</p>
			</div>

			<HeroCode />

			<p>
				Traditionally, JavaScript does concurrency with one event loop and a shared heap. The BEAM does it with isolated
				processes that share nothing. Arc is an experiment in running the former on the latter.
			</p>

			<p>
				Arc is an entire JavaScript engine written in <ExternalLink href="https://gleam.run">Gleam</ExternalLink>. Every{' '}
				<Code>Arc.spawn</Code> is a real Erlang process. You can have millions of them, each with its own heap — no
				stop-the-world garbage collection, and a crash in one leaves the others untouched. These are guarantees
				JavaScript has never had.
			</p>

			<div>
				<p>
					Tested against <ExternalLink href="https://github.com/tc39/test262">test262</ExternalLink> on every commit:
				</p>
				<picture className="block mt-3">
					<source media="(prefers-color-scheme: dark)" srcSet="/conformance-dark.png" />
					<img alt="test262 conformance chart" src="/conformance.png" className="w-full rounded-lg" />
				</picture>
			</div>

			<hr className="w-12 border-neutral-200 dark:border-neutral-800" />

			<div>
				<p className="mb-3">
					Try it — this is Arc running on <ExternalLink href="https://github.com/atomvm/AtomVM">AtomVM</ExternalLink>{' '}
					compiled to WebAssembly, so <Code>Arc.spawn</Code>, <Code>Arc.send</Code> and <Code>Arc.receive</Code> are
					real Erlang processes in your browser tab.
				</p>
				<Playground />
			</div>

			<hr className="w-12 border-neutral-200 dark:border-neutral-800" />

			{/* <div className="bg-neutral-100 dark:bg-neutral-900/50 rounded-lg p-4 font-mono text-sm text-neutral-700 dark:text-neutral-300 space-y-1">
        <p>
          <span className="text-neutral-400 dark:text-neutral-500 select-none">$ </span>gleam run --
          file.js
          <span className="text-neutral-400 dark:text-neutral-600 ml-4"># run a script</span>
        </p>
        <p>
          <span className="text-neutral-400 dark:text-neutral-500 select-none">$ </span>gleam test
          <span className="text-neutral-400 dark:text-neutral-600 ml-4"># unit tests</span>
        </p>
        <p>
          <span className="text-neutral-400 dark:text-neutral-500 select-none">$ </span>
          TEST262_EXEC=1 gleam test
          <span className="text-neutral-400 dark:text-neutral-600 ml-4"># full test262 suite</span>
        </p>
      </div> */}

			<div className="flex items-center gap-4">
				<ExternalLink href="https://github.com/alii/arc">GitHub</ExternalLink>
			</div>

			<p className="text-neutral-400 dark:text-neutral-500 text-sm">
				Arc is an extremely early research project, tread carefully.
			</p>
		</main>
	);
}
