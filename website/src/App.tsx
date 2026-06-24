import { useEffect, useState } from 'react';
import { motion } from 'motion/react';
import { ExternalLink } from './components/external-link';
import { Playground } from './playground/Playground';

const item = { hidden: { opacity: 0 }, show: { opacity: 1 } };

export default function App() {
	const [shimmer, setShimmer] = useState(false);

	useEffect(() => {
		let interval: ReturnType<typeof setInterval>;
		const play = () => {
			setShimmer(true);
			setTimeout(() => setShimmer(false), 1600);
		};
		const timeout = setTimeout(() => {
			play();
			interval = setInterval(play, 15000);
		}, 2000);
		return () => {
			clearTimeout(timeout);
			clearInterval(interval);
		};
	}, []);

	return (
		<motion.main
			initial="hidden"
			animate="show"
			transition={{ staggerChildren: 0.05 }}
			className="flex flex-col gap-6 max-w-[600px] mx-auto px-5 py-16 lg:px-10 lg:py-16 leading-relaxed text-base font-mono"
		>
			<motion.div variants={item}>
				<h1 className="group text-lg font-semibold text-rpd-text dark:text-rp-text cursor-default">
					<span className={`bg-[length:200%_auto] bg-clip-text bg-[linear-gradient(90deg,#eb6f92,#f6c177,#9ccfd8,#c4a7e7,#ebbcba,#31748f,#eb6f92)] transition-colors duration-500 group-hover:text-transparent group-hover:animate-rainbow ${shimmer ? 'text-transparent animate-rainbow' : ''}`}>
						arc <span className="align-top text-sm leading-none">⌒</span>
					</span>
				</h1>
				<p className="mt-1">JavaScript on the BEAM</p>
			</motion.div>

			<motion.div variants={item}>
				<Playground />
			</motion.div>

			<motion.p variants={item}>
				Arc is a JavaScript engine written in <ExternalLink href="https://gleam.run">Gleam</ExternalLink> — the whole
				language, not a subset. It runs wherever the BEAM runs: on Erlang/OTP, and in the browser through{' '}
				<ExternalLink href="https://www.atomvm.net">AtomVM</ExternalLink> compiled to WebAssembly. The playground
				above is Arc running as WebAssembly.
			</motion.p>

			<motion.p variants={item}>
				It implements the language itself — closures, generators, async/await, classes, proxies, typed arrays, plus
				Intl and Temporal. The engine is small and host-agnostic: it knows nothing about the world outside
				ECMAScript. You embed it in a BEAM program and give it the globals and host functions you want — timers,
				I/O, a concurrency model — instead of inheriting a fixed runtime.
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
