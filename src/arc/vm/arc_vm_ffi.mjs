import { Ok, Error, toList } from './gleam.mjs';
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

export function array_unsafe_get(index, arr) {
	return arr[index];
}

export function array_set_unchecked(index, value, arr) {
	const copy = arr.slice();
	copy[index] = value;
	return copy;
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

// -- tree_array: same JS Array backing, tracks default for unset slots -------

export function tree_array_new(default_) {
	return { d: default_, a: [] };
}
export function tree_array_from_list(items, default_) {
	return { d: default_, a: items.toArray() };
}
export function tree_array_to_list({ a }) {
	return toList(a);
}
export function tree_array_get(index, { d, a }) {
	if (index >= 0 && index < a.length) return a[index];
	return d;
}
export function tree_array_get_option(index, { d, a }) {
	if (index < 0 || index >= a.length) return new None();
	const v = a[index];
	// Default-valued slot = hole. Gleam zero-arity constructors are singletons
	// on the JS target, so === is correct for the JsUninitialized sentinel.
	return v === d ? new None() : new Some(v);
}
export function tree_array_set(index, value, { d, a }) {
	if (index < 0 || index >= MAX_DENSE_ELEMENTS) return { d, a };
	const copy = a.slice();
	while (copy.length < index) copy.push(d);
	copy[index] = value;
	return { d, a: copy };
}
export function tree_array_size({ a }) {
	return a.length;
}
export function tree_array_resize({ d, a }, new_size) {
	if (new_size < 0) return { d, a };
	return { d, a: a.slice(0, new_size) };
}
export function tree_array_reset(index, { d, a }) {
	if (index < 0 || index >= a.length) return { d, a };
	const copy = a.slice();
	copy[index] = d;
	return { d, a: copy };
}
export function tree_array_sparse_fold(f, acc, { d, a }) {
	for (let i = 0; i < a.length; i++) {
		if (a[i] !== d) acc = f(i, a[i], acc);
	}
	return acc;
}

// -- job_queue: plain JS Array, O(1) push / O(n) shift (acceptable on JS target)

export function job_queue_new() {
	return [];
}
export function job_queue_push(q, item) {
	return [...q, item];
}
export function job_queue_pop(q) {
	if (q.length === 0) return new None();
	const [head, ...rest] = q;
	return new Some([head, rest]);
}

// -- Fast string indexing (codepoint-based, no grapheme clustering) ---------

export function string_char_at(s, idx) {
	if (idx < 0) return new None();
	const it = s[Symbol.iterator]();
	let r;
	for (let i = 0; i <= idx; i++) {
		r = it.next();
		if (r.done) return new None();
	}
	return new Some(r.value);
}

export function string_codepoint_length(s) {
	let n = 0;
	for (const _ of s) n++;
	return n;
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
