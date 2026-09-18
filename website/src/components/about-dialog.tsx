import * as Dialog from '@radix-ui/react-dialog';
import { AnimatePresence, motion } from 'motion/react';
import { type ReactNode, useState } from 'react';
import { ExternalLink } from './external-link';

const ease = [0.23, 1, 0.32, 1] as const;

/**
 * The "How this page works" modal, opened from the `?` in the playground toolbar
 * and from the link in the page text. Radix handles focus and escape. Motion
 * fades the modal in and out, so the content is force-mounted and shown by
 * AnimatePresence instead of Radix's own mount logic.
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
								<div className="p-6 sm:p-8 flex flex-col gap-5">
									<div className="flex items-start justify-between gap-4">
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
									</div>

									<Section title="What is running">
										This page runs Arc inside your browser. Arc is a JavaScript engine, and here it runs on{' '}
										<ExternalLink href="https://www.atomvm.net">AtomVM</ExternalLink>, a small BEAM implementation
										compiled to WebAssembly. It downloads once, at about 7 MB. After that nothing you type leaves your
										machine.
									</Section>

									<Section title="Tabs">
										<Kbd>run</Kbd> sends your program to Arc's interpreter, and the Output tab shows what it prints plus
										the final value, unless that is <code>undefined</code>. The other tabs show what Arc's ahead-of-time
										compiler does to the same program. IR comes first, then Core Erlang, then Erlang, which is what gets
										compiled to BEAM bytecode. Core Erlang is shown for reading only, because Arc never sends it through
										Erlang's Core Erlang compiler. The browser has no Erlang compiler, so the compiled output can't be
										run here.
									</Section>

									<Section title="Speed">
										Programs run roughly tens of times slower here than on the real BEAM, because AtomVM in WebAssembly
										is an interpreter running an interpreter. AtomVM also loads code on first use and Arc builds its
										built-ins at startup, so a small program runs in the background after the page loads to get that out
										of the way. The status reads <em>warming up caches</em> until it finishes, and if you press run
										before then, your program waits its turn.
									</Section>

									<Section title="Limits">
										AtomVM has no regex engine, so <code>RegExp</code> uses your browser's, at about a millisecond per
										match. There are no timers, filesystem or network.
									</Section>

									<p className="text-xs text-rpd-muted dark:text-rp-muted">
										Arc is an early research project.{' '}
										<ExternalLink href="https://github.com/alii/arc">Source on GitHub</ExternalLink>.
									</p>
								</div>
							</motion.div>
						</Dialog.Content>
					</Dialog.Portal>
				)}
			</AnimatePresence>
		</Dialog.Root>
	);
}

/** `title` names the paragraph in source and for screen readers. It is not shown. */
function Section({ title, children }: { title: string; children: ReactNode }) {
	return (
		<section aria-label={title}>
			<p>{children}</p>
		</section>
	);
}

function Kbd({ children }: { children: ReactNode }) {
	return (
		<kbd className="px-1 py-0.5 text-xs rounded border border-current/25 bg-current/10 font-mono leading-none">
			{children}
		</kbd>
	);
}
