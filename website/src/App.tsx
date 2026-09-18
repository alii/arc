import { motion } from 'motion/react';
import { useEffect, useState } from 'react';
import { AboutDialog } from './components/about-dialog';
import { ConformanceChart } from './components/conformance-chart';
import { ExternalLink } from './components/external-link';
import { Playground } from './playground/Playground';
import { useMediaQuery } from './use-media-query';

const item = { hidden: { opacity: 0 }, show: { opacity: 1 } };
// A variant-aware wrapper: motion children only stagger through motion parents.
const column = { hidden: {}, show: { transition: { staggerChildren: 0.05 } } };

export default function App() {
	const [shimmer, setShimmer] = useState(false);
	// The playground is desktop-only for now: it wants a full column of height,
	// and not mounting it on small screens also skips the WASM download there.
	const wide = useMediaQuery('(min-width: 1024px)');

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
			className="mx-auto max-w-360 px-5 py-16 lg:px-10 lg:py-12 leading-relaxed text-base font-mono lg:grid lg:grid-cols-[minmax(0,1fr)_minmax(0,2fr)] lg:gap-10 lg:items-start"
		>
			<motion.div variants={column} className="flex flex-col gap-6 max-w-150 lg:max-w-none lg:min-w-0">
				<motion.div variants={item}>
					<h1 className="group text-lg font-semibold text-rpd-text dark:text-rp-text cursor-default">
						<span
							className={`bg-size-[200%_auto] bg-clip-text bg-[linear-gradient(90deg,#eb6f92,#f6c177,#9ccfd8,#c4a7e7,#ebbcba,#31748f,#eb6f92)] transition-colors duration-500 group-hover:text-transparent group-hover:animate-rainbow ${shimmer ? 'text-transparent animate-rainbow' : ''}`}
						>
							arc <span className="align-top text-sm leading-none">⌒</span>
						</span>
					</h1>
					<p className="mt-1">JavaScript on the BEAM</p>
				</motion.div>

				<motion.p variants={item}>
					Arc is a JavaScript engine written in <ExternalLink href="https://gleam.run">Gleam</ExternalLink>. It
					implements ECMAScript and has no built-in timers, I/O or concurrency. The BEAM program that embeds it provides
					whatever globals and host functions the JavaScript needs.
				</motion.p>

				<motion.p variants={item}>
					By default Arc interprets JavaScript, and it can also compile it ahead of time to Erlang. It runs on
					Erlang/OTP, and in the browser on <ExternalLink href="https://www.atomvm.net">AtomVM</ExternalLink> compiled
					to WebAssembly.
					{wide ? ' The playground next to this text runs that way, and its Erlang tab shows the compiled output.' : ''}
					{wide && (
						<>
							{' '}
							<AboutDialog
								trigger={
									<button className="underline decoration-dotted underline-offset-4 hover:text-rpd-text dark:hover:text-rp-text cursor-pointer transition-colors">
										How this page works
									</button>
								}
							/>
						</>
					)}
				</motion.p>

				<motion.div variants={item}>
					<p>
						Tested against <ExternalLink href="https://github.com/tc39/test262">test262</ExternalLink> on every commit:
					</p>
					<ConformanceChart />
				</motion.div>

				<motion.hr variants={item} className="w-12 border-rpd-overlay dark:border-rp-overlay" />

				<motion.div variants={item} className="flex items-center gap-4">
					<ExternalLink href="https://github.com/alii/arc">GitHub</ExternalLink>
				</motion.div>

				<motion.p variants={item} className="text-rpd-muted dark:text-rp-muted text-sm">
					Arc is an early research project and is rough in places.
				</motion.p>
			</motion.div>

			{wide && (
				<motion.div variants={item} className="sticky top-12 h-[calc(100dvh-6rem)] min-h-140 min-w-0">
					<Playground />
				</motion.div>
			)}
		</motion.main>
	);
}
