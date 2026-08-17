import * as Dialog from '@radix-ui/react-dialog';
import { AnimatePresence, motion } from 'motion/react';
import { type ReactNode, useState } from 'react';
import { ExternalLink } from './external-link';

const ease = [0.23, 1, 0.32, 1] as const;

const section = { hidden: { opacity: 0, y: 6 }, show: { opacity: 1, y: 0 } };

/**
 * "How this page works" — the modal behind the `?` in the playground toolbar
 * and the link in the prose. Radix for a11y (focus trap, escape, labelling);
 * motion for the enter/exit animation, which is why the content is force-
 * mounted and gated by AnimatePresence instead of Radix's own mount logic.
 */
export function AboutDialog({ trigger }: { trigger: ReactNode }) {
	const [open, setOpen] = useState(false);

	return (
		<Dialog.Root open={open} onOpenChange={setOpen}>
			<Dialog.Trigger asChild>{trigger}</Dialog.Trigger>
			<AnimatePresence>
				{open && (
					<Dialog.Portal forceMount>
						<Dialog.Overlay asChild forceMount>
							<motion.div
								initial={{ opacity: 0 }}
								animate={{ opacity: 1 }}
								exit={{ opacity: 0 }}
								transition={{ duration: 0.2 }}
								className="fixed inset-0 z-50 bg-rpd-text/20 dark:bg-black/50 backdrop-blur-[2px]"
							/>
						</Dialog.Overlay>
						<Dialog.Content asChild forceMount aria-describedby={undefined}>
							<motion.div
								initial={{ opacity: 0, scale: 0.97, y: 8, filter: 'blur(6px)' }}
								animate={{ opacity: 1, scale: 1, y: 0, filter: 'blur(0px)' }}
								exit={{ opacity: 0, scale: 0.98, y: 4, filter: 'blur(4px)' }}
								transition={{ duration: 0.28, ease }}
								className="fixed z-50 left-1/2 top-1/2 -translate-x-1/2 -translate-y-1/2 w-[min(92vw,640px)] max-h-[85dvh] overflow-auto rounded-xl border border-rpd-text/15 dark:border-rp-overlay bg-rpd-surface dark:bg-rp-surface text-rpd-subtle dark:text-rp-subtle shadow-2xl shadow-black/20 dark:shadow-black/50 font-mono text-sm leading-relaxed outline-none"
							>
								<motion.div
									initial="hidden"
									animate="show"
									transition={{ staggerChildren: 0.05, delayChildren: 0.08 }}
									className="p-6 sm:p-8 flex flex-col gap-5"
								>
									<motion.div variants={section} className="flex items-start justify-between gap-4">
										<Dialog.Title className="text-base font-semibold text-rpd-text dark:text-rp-text">
											How this page works
										</Dialog.Title>
										<Dialog.Close
											aria-label="Close"
											className="shrink-0 -m-1 p-1 rounded-md text-rpd-muted dark:text-rp-muted hover:text-rpd-text dark:hover:text-rp-text cursor-pointer transition-colors"
										>
											<svg
												width="16"
												height="16"
												viewBox="0 0 16 16"
												fill="none"
												stroke="currentColor"
												strokeWidth="1.5"
												strokeLinecap="round"
											>
												<path d="M4 4l8 8M12 4l-8 8" />
											</svg>
										</Dialog.Close>
									</motion.div>

									<Section title="Everything runs in your tab">
										Arc — a JavaScript engine written in Gleam — is compiled to WebAssembly through{' '}
										<ExternalLink href="https://www.atomvm.net">AtomVM</ExternalLink>, a small BEAM implementation. The
										page loads it once (about 7 MB); after that nothing you type leaves the browser and there is no
										server involved.
									</Section>

									<Section title="Output">
										<Kbd>run</Kbd> hands your program to Arc's interpreter inside AtomVM. Whatever it prints shows up
										here, and its completion value too when it isn't <code>undefined</code>. Errors are shown as the
										engine reports them.
									</Section>

									<Section title="Erlang · Core Erlang · IR">
										Arc also compiles ahead of time: the same JavaScript is lowered to a small IR, then to Core Erlang,
										then to Erlang abstract forms that the Erlang compiler turns into BEAM bytecode. These tabs show
										each stage for the program in the editor — the Erlang tab is exactly what would be compiled. The
										final compile-to-BEAM step needs OTP's compiler, which doesn't exist in the browser, so here it's
										for reading rather than running.
									</Section>

									<Section title="Warming up">
										AtomVM loads code on first use and Arc builds JavaScript's global environment (hundreds of
										built-ins) when it starts. So right after the page loads, a little program runs behind the scenes to
										pull all of that in — otherwise your first click would pay for it. The status says{' '}
										<em>warming up caches</em> while that happens; you can run anyway, it just queues.
									</Section>

									<Section title="Speed, honestly">
										AtomVM in WebAssembly is an interpreter running an interpreter, so programs run slower here than on
										the real BEAM — roughly tens of times, not thousands. It got there because this page runs a lightly
										patched AtomVM (see <code>website/atomvm_patches</code> in the repo): the stock 0.7 release
										collected garbage every few instructions on code like Arc's. The timings are for feel, not for
										benchmarking.
									</Section>

									<Section title="Not (yet) in the browser build">
										Regular expressions: AtomVM has no PCRE, so <code>RegExp</code> throws a clear error rather than
										silently failing. Timers, filesystem and network are absent for the same reason — Arc is host-
										agnostic and this host provides none of them.
									</Section>

									<motion.p variants={section} className="text-xs text-rpd-muted dark:text-rp-muted">
										Arc is an early research project.{' '}
										<ExternalLink href="https://github.com/alii/arc">Source on GitHub</ExternalLink>.
									</motion.p>
								</motion.div>
							</motion.div>
						</Dialog.Content>
					</Dialog.Portal>
				)}
			</AnimatePresence>
		</Dialog.Root>
	);
}

function Section({ title, children }: { title: string; children: ReactNode }) {
	return (
		<motion.section variants={section} className="flex flex-col gap-1.5">
			<h3 className="text-xs uppercase tracking-wider text-rpd-muted dark:text-rp-muted">{title}</h3>
			<p>{children}</p>
		</motion.section>
	);
}

function Kbd({ children }: { children: ReactNode }) {
	return (
		<kbd className="px-1 py-0.5 text-xs rounded border border-current/25 bg-current/10 font-mono leading-none">
			{children}
		</kbd>
	);
}
