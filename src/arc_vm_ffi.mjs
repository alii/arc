import { Ok, Error, List, toList } from './gleam.mjs';
import { Some, None } from '../gleam_stdlib/gleam/option.mjs';

// -- tuple_array: backed by plain JS Array on the JS target -----------------

export function array_from_list(items) {
	return items.toArray();
}

export function array_to_list(arr) {
	return toList(arr);
}

export function array_get(index, arr) {
	if (index >= 0 && index < arr.length) {
		return new Some(arr[index]);
	}
	return new None();
}

export function array_set(index, value, arr) {
	if (index >= 0 && index < arr.length) {
		const copy = arr.slice();
		copy[index] = value;
		return new Ok(copy);
	}
	return new Error(undefined);
}

export function array_size(arr) {
	return arr.length;
}

const MAX_DENSE_ELEMENTS = 10_000_000;

export function array_repeat(value, count) {
	if (count > MAX_DENSE_ELEMENTS) {
		throw new globalThis.Error('array_too_large');
	}
	return new Array(count).fill(value);
}

export function array_grow(arr, new_size, default_) {
	if (new_size > MAX_DENSE_ELEMENTS) {
		throw new globalThis.Error('array_too_large');
	}
	if (new_size <= arr.length) return arr;
	const grown = arr.slice();
	for (let i = arr.length; i < new_size; i++) grown.push(default_);
	return grown;
}

// -- Symbol identity: monotonic counter instead of make_ref() ----------------

let ref_counter = 0;
export function make_ref() {
	return ++ref_counter;
}

// -- CLI — stubbed on JS target. Browser embeds use arc/engine directly. ----

export function read_line(_prompt) {
	return new Error(undefined);
}

export function get_script_args() {
	if (typeof process === 'undefined') return toList([]);
	return toList(process.argv.slice(2));
}

export function read_file(_path) {
	return new Error('read_file: CLI not supported on JS target; use arc/engine');
}

// -- BEAM-only primitives: all panic -----------------------------------------

function beam_only(name) {
	throw new globalThis.Error(
		`Arc.${name} requires the BEAM target (Erlang processes/mailbox). ` + `Not available under --target=javascript.`,
	);
}

export function erlang_self() {
	beam_only('self');
}
export function send_message(_pid, _msg) {
	beam_only('send');
}
export function receive_user_message() {
	beam_only('receive');
}
export function receive_user_message_timeout(_ms) {
	beam_only('receive');
}
export function pid_to_string(_pid) {
	beam_only('pid');
}
export function sleep(_ms) {
	beam_only('sleep');
}
export function send_after(_ms, _pid, _msg) {
	beam_only('setTimeout');
}
export function cancel_timer(_tref) {
	beam_only('clearTimeout');
}
export function receive_any_event() {
	beam_only('event loop');
}
export function receive_settle_only() {
	beam_only('event loop');
}
export function spawn(_fun) {
	beam_only('spawn');
}
export function term_to_binary(_term) {
	throw new globalThis.Error(
		'term_to_binary not available on JS target. ' + 'Module/heap serialization requires BEAM.',
	);
}
export function binary_to_term(_bin) {
	throw new globalThis.Error(
		'binary_to_term not available on JS target. ' + 'Module/heap serialization requires BEAM.',
	);
}
