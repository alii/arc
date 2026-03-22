export function format_to_fixed(x, digits) {
	return x.toFixed(digits);
}

export function format_to_exponential(x, fraction_digits) {
	// -1 means "auto" (no argument in JS)
	return fraction_digits === -1 ? x.toExponential() : x.toExponential(fraction_digits);
}

export function format_to_precision(x, precision) {
	return x.toPrecision(precision);
}
