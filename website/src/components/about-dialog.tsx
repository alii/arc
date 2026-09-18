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

									<Section title="Everything runs in your tab">
										Arc is a JavaScript engine written in Gleam. This page runs it on{' '}
										<ExternalLink href="https://www.atomvm.net">AtomVM</ExternalLink>, a small BEAM implementation
										compiled to WebAssembly. The download is about 7 MB and happens once. Nothing you type leaves your
										browser, and there is no server.
									</Section>

									<Section title="Output">
										<Kbd>run</Kbd> sends your program to Arc's interpreter inside AtomVM. The Output tab shows what the
										program prints, plus its final value unless that value is <code>undefined</code>. Errors appear as
										the engine reports them.
									</Section>

									<Section title="Erlang · Core Erlang · IR">
										Arc can also compile JavaScript ahead of time. It converts the program to a small intermediate
										representation (IR), then to a module shaped like Core Erlang, then to Erlang abstract forms, which
										the Erlang compiler turns into BEAM bytecode. The three tabs show these steps for the program in
										the editor. The Erlang tab is exactly what gets compiled. The Core Erlang tab is a readable copy of
										the middle step, because the real compile path skips the Core Erlang compiler. The last step needs
										OTP's compiler, which the browser does not have, so you can read the output here but not run it.
									</Section>

									<Section title="Warming up">
										AtomVM loads code the first time it is used, and Arc creates JavaScript's global environment
										(hundreds of built-ins) at startup. So after the page loads, a small program runs in the background
										to load all of that before your first click. The status reads <em>warming up caches</em> until it
										finishes. You can still press run, and it will wait its turn.
									</Section>

									<Section title="Speed">
										AtomVM in WebAssembly is an interpreter running an interpreter. Programs here run roughly tens of
										times slower than on the real BEAM.
									</Section>

									<Section title="Regular expressions">
										AtomVM has no regex engine, so <code>RegExp</code> uses your browser's own JavaScript engine. Arc
										asks the page to run the match and sends the result back to the BEAM side, converting offsets
										between UTF-8 and UTF-16. Each match takes about a millisecond. There are no timers, filesystem or
										network. Arc does not provide them and this page does not either.
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
