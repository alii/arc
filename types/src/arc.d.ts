declare module 'arc:internal' {
	type Brands = 'Pid';
}

declare module 'arc' {
	import type { Brand } from 'arc:internal';

	export interface Pid extends Brand<'Pid'> {}

	/**
	 * Send a message to a process
	 *
	 * @param pid The pid to send the message to
	 * @param message The message
	 */
	export function send<T>(pid: Pid, message: T): void;

	/**
	 * Spawn a new process
	 *
	 * This function will copy the heap with copy-on-write semantics
	 * which means it's roughly still fast enough considering we're
	 * implementing a mutable lanugage in an immutable one.
	 *
	 * @param fn The closure to evaluate on the new process
	 */
	export function spawn<T>(fn: () => T): Pid;

	/**
	 * Receive a message from the process mailbox
	 *
	 * This API will probably change in the future to
	 * support patterns
	 *
	 * @param timeout The timeout to wait for
	 */
	export function receive<T>(timeout?: number): T;

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
	 * BEAM will idle the process until the time is up, so this
	 * function uses zero cpu.
	 *
	 * @param ms Millseconds to sleep for
	 */
	export function sleep(ms: number): void;

	export function peek<T>(
		promise: Promise<T>,
	): { type: 'pending' } | { type: 'resolved'; value: T } | { type: 'rejected'; reason: unknown };
}
