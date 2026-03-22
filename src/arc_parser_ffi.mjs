import { Ok, Error } from "./gleam.mjs";

export function parse_float(s) {
  const f = parseFloat(s);
  if (Number.isFinite(f)) {
    return new Ok(f);
  }
  return new Error(undefined);
}

const encoder = new TextEncoder();
export function byte_size(s) {
  return encoder.encode(s).length;
}
