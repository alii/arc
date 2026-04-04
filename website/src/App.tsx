import { Code } from './components/code';
import { ExternalLink } from './components/external-link';
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

			<Playground />

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
					<source media="(prefers-color-scheme: dark)" srcSet="https://raw.githubusercontent.com/alii/arc/master/.github/test262/conformance-dark.png" />
					<img alt="test262 conformance chart" src="https://raw.githubusercontent.com/alii/arc/master/.github/test262/conformance.png" className="w-full rounded-lg" />
				</picture>
			</div>

			<hr className="w-12 border-neutral-200 dark:border-neutral-800" />

			<div className="flex items-center gap-4">
				<ExternalLink href="https://github.com/alii/arc">GitHub</ExternalLink>
			</div>

			<p className="text-neutral-400 dark:text-neutral-500 text-sm">
				Arc is an extremely early research project, tread carefully.
			</p>
		</main>
	);
}
