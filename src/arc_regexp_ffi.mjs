import { Ok, Error, toList } from './gleam.mjs';

// JS RegExp flags that map directly; g/y are handled at the Gleam level
// via the offset-based exec loop, so we strip them here.
function normalize_flags(flags) {
	let out = '';
	for (const f of flags) {
		if ('imsu'.includes(f)) out += f;
	}
	return out;
}

export function regexp_test(pattern, flags, string) {
	try {
		return new RegExp(pattern, normalize_flags(flags)).test(string);
	} catch (_e) {
		return false;
	}
}

// Returns Result(List(#(Int, Int)), Nil) — byte offsets into the UTF-8 string.
// The Erlang re module returns byte indices; JS .exec returns UTF-16 code-unit
// indices. We convert to UTF-8 byte offsets to keep the Gleam side identical.
export function regexp_exec(pattern, flags, string, byte_offset) {
	let re;
	try {
		// Need 'd' flag for .indices, plus sticky at the translated offset.
		re = new RegExp(pattern, normalize_flags(flags) + 'dy');
	} catch (_e) {
		return new Error(undefined);
	}
	const char_offset = byte_to_char_index(string, byte_offset);
	re.lastIndex = char_offset;
	const m = re.exec(string);
	if (!m || !m.indices) return new Error(undefined);

	const tuples = [];
	for (const idx of m.indices) {
		if (idx === undefined) {
			tuples.push([-1, 0]);
		} else {
			const [cs, ce] = idx;
			const bs = char_to_byte_index(string, cs);
			const be = char_to_byte_index(string, ce);
			tuples.push([bs, be - bs]);
		}
	}
	return new Ok(toList(tuples));
}

const encoder = new TextEncoder();

function char_to_byte_index(s, ci) {
	return encoder.encode(s.slice(0, ci)).length;
}

function byte_to_char_index(s, bi) {
	// Walk codepoints, counting UTF-8 bytes until we hit bi.
	let bytes = 0;
	let chars = 0;
	for (const cp of s) {
		if (bytes >= bi) return chars;
		bytes += encoder.encode(cp).length;
		chars += cp.length; // surrogate pairs count as 2 UTF-16 units
	}
	return chars;
}
