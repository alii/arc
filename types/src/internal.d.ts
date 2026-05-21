declare module 'arc:internal' {
	type Values<T> = T[keyof T];

	/**
	 * This variable does not exist at runtim
	 * @deprecated
	 */
	const brand: unique symbol;

	export interface Brand<T extends Brands> {
		[brand]: T;
	}
}
