export function encode(str, preserve_uri_chars) {
	return preserve_uri_chars ? encodeURI(str) : encodeURIComponent(str);
}

export function decode(str) {
	// Erlang impl is tolerant of bad escapes; decodeURIComponent throws.
	// Match Erlang behavior by leaving invalid sequences as-is.
	try {
		return decodeURIComponent(str);
	} catch (_e) {
		return str;
	}
}
