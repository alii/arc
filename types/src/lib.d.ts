export {};

declare const opaque: unique symbol;
export type opaque<T> = { [opaque]: T };
