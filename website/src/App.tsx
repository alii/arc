import { motion } from 'motion/react';
import { Code } from './components/code';
import { ExternalLink } from './components/external-link';
import { Playground } from './playground/Playground';

const item = { hidden: { opacity: 0 }, show: { opacity: 1 } };

export default function App() {
	return (
		<motion.main
			initial="hidden"
			animate="show"
			transition={{ staggerChildren: 0.05 }}
			className="flex flex-col gap-6 max-w-[600px] mx-auto px-5 py-16 lg:px-10 lg:py-16 leading-relaxed text-base font-mono"
		>
			<motion.div variants={item}>
				<h1 className="group text-lg font-semibold text-rpd-text dark:text-rp-text cursor-default">
					<span className="bg-[length:200%_auto] bg-clip-text bg-[linear-gradient(90deg,#eb6f92,#f6c177,#9ccfd8,#c4a7e7,#ebbcba,#31748f,#eb6f92)] transition-colors duration-500 group-hover:text-transparent group-hover:animate-rainbow">
						arc <span className="align-top text-sm leading-none">⌒</span>
					</span>
				</h1>
				<p className="mt-1">JavaScript on the BEAM</p>
			</motion.div>

			<motion.div variants={item}>
				<Playground />
			</motion.div>

			<motion.p variants={item}>
				Traditionally, JavaScript does concurrency with one event loop and a shared heap. The BEAM does it with isolated
				processes that share nothing. Arc is an experiment in running the former on the latter.
			</motion.p>

			<motion.p variants={item}>
				Arc is an entire JavaScript engine written in <ExternalLink href="https://gleam.run">Gleam</ExternalLink>. Every{' '}
				<Code>Arc.spawn</Code> is a real Erlang process. You can have millions of them, each with its own heap — no
				stop-the-world garbage collection, and a crash in one leaves the others untouched. These are guarantees
				JavaScript has never had.
			</motion.p>

			<motion.div variants={item}>
				<p>
					Tested against <ExternalLink href="https://github.com/tc39/test262">test262</ExternalLink> on every commit:
				</p>
				<picture className="block mt-3">
					<source media="(prefers-color-scheme: dark)" srcSet="https://raw.githubusercontent.com/alii/arc/master/.github/test262/conformance-dark.png" />
					<img alt="test262 conformance chart" src="https://raw.githubusercontent.com/alii/arc/master/.github/test262/conformance.png" className="w-full rounded-lg" />
				</picture>
			</motion.div>

			<motion.hr variants={item} className="w-12 border-rpd-overlay dark:border-rp-overlay" />

			<motion.div variants={item} className="flex items-center gap-4">
				<ExternalLink href="https://github.com/alii/arc">GitHub</ExternalLink>
			</motion.div>

			<motion.p variants={item} className="text-rpd-muted dark:text-rp-muted text-sm">
				Arc is an extremely early research project, tread carefully.
			</motion.p>
		</motion.main>
	);
}
