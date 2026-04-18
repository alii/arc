declare module 'arc:internal' {
	type Brands = 'Pid' | 'Timer' | 'Subject' | 'Selector';
}

declare module 'arc' {
	import type { Brand } from 'arc:internal';

	export interface Pid extends Brand<'Pid'> {}
	export interface Timer extends Brand<'Timer'> {}

	/**
	 * A typed channel bound to the process that created it.
	 *
	 * Subjects can be sent across processes — call {@link Subject.send} from
	 * anywhere and {@link Subject.receive} on the owning process. Each subject is
	 * tagged with a unique ref so selective receive is O(1) for the common case.
	 */
	export interface Subject<T> extends Brand<'Subject'> {
		/**
		 * Send a message to this subject's owner process.
		 *
		 * The message is structurally cloned. Returns {@link message}.
		 */
		send(message: T): T;

		/**
		 * Block until a message arrives on this subject.
		 *
		 * Only the owning process should call this.
		 */
		receive(): T;
		/**
		 * Block until a message arrives or {@link timeout} ms elapse.
		 * Returns `undefined` on timeout.
		 */
		receive(timeout: number): T | undefined;

		/**
		 * Returns a promise that resolves with the next message on this subject.
		 *
		 * Non-blocking — the event loop keeps running while you `await`.
		 */
		receiveAsync(): Promise<T>;
		/**
		 * Returns a promise that resolves with the next message, or `undefined`
		 * if {@link timeout} ms elapse first.
		 */
		receiveAsync(timeout: number): Promise<T | undefined>;

		toString(): string;
	}

	/**
	 * An immutable set of subjects to receive on. Chain {@link Selector.on} to
	 * register subjects, then call {@link Selector.receive} to block until a
	 * message arrives on any of them.
	 */
	export interface Selector<R = never> extends Brand<'Selector'> {
		/**
		 * Return a new selector that also listens on {@link subject}. Messages
		 * matched on it are returned as-is from {@link Selector.receive}.
		 */
		on<T>(subject: Subject<T>): Selector<R | T>;
		/**
		 * Return a new selector that also listens on {@link subject}. Messages
		 * matched on it are passed through {@link map} before being returned.
		 */
		on<T, R2>(subject: Subject<T>, map: (message: T) => R2): Selector<R | R2>;

		/**
		 * Block until a message arrives on any registered subject, then return
		 * it (after applying its mapper, if any).
		 */
		receive(): R;
		/**
		 * Block until a message arrives or {@link timeout} ms elapse.
		 * Returns `undefined` on timeout.
		 */
		receive(timeout: number): R | undefined;
	}

	/**
	 * Create a new subject bound to the current process.
	 *
	 * In TypeScript, you should always pass the type parameter
	 * to specify the values this subject can send/receive:
	 *
	 * ```ts
	 * const s = Arc.subject<string>() // Accepts strings
	 * ```
	 */
	export function subject(): Subject<never>;
	/**
	 * Create a new subject bound to the current process.
	 */
	export function subject<T>(): Subject<T>;

	/**
	 * Create an empty {@link Selector}. Chain {@link Selector.on} to register
	 * subjects, then call {@link Selector.receive} to block until a message
	 * arrives on any of them.
	 *
	 * @example
	 * const selector = Arc.select()
	 *   .on(replies)
	 *   .on(errors, err => { throw err });
	 * const result = selector.receive(1000);
	 */
	export function select(): Selector;

	/**
	 * Spawn a new process
	 *
	 * This function will copy the heap with copy-on-write semantics which means
	 * it's roughly still fast enough considering we're implementing a mutable
	 * lanugage in an immutable one.
	 *
	 * @param fn The closure to evaluate on the new process
	 */
	export function spawn<T>(fn: () => T): Pid;

	/**
	 * Schedule a callback to be executed after at-least {@link ms} milliseconds
	 * have passed.
	 *
	 * @param cb The callback
	 * @param ms Minimum amount of milliseconds before execution
	 * @returns A timer handle that can be passed to {@link clearTimeout}
	 */
	export function setTimeout(cb: () => void, ms: number): Timer;

	/**
	 * Cancel a timer created by {@link setTimeout}. If the timer hasn't fired
	 * yet, the callback will not be invoked. No-op if the timer already fired
	 * or {@link timer} isn't a timer.
	 *
	 * @param timer The timer handle from {@link setTimeout}
	 */
	export function clearTimeout(timer: Timer): void;

	/**
	 * Get the current process id
	 */
	export function self(): Pid;

	/**
	 * Inspect and print the passed arguments to stdout
	 *
	 * This is like `console.log`
	 *
	 * @param args Arguments, anything
	 */
	export function log(...args: unknown[]): void;

	/**
	 * Block the process until at-least {@link ms} has passed.
	 *
	 * BEAM will idle the process until the time is up, so this function uses
	 * zero cpu.
	 *
	 * @param ms Millseconds to sleep for
	 */
	export function sleep(ms: number): void;

	export function peek<T>(
		promise: Promise<T>,
	): { type: 'pending' } | { type: 'resolved'; value: T } | { type: 'rejected'; reason: unknown };
}
