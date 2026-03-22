@external(erlang, "erlang", "term_to_binary")
@external(javascript, "../../arc_vm_ffi.mjs", "term_to_binary")
pub fn term_to_binary(term: anything) -> BitArray

@external(erlang, "erlang", "binary_to_term")
@external(javascript, "../../arc_vm_ffi.mjs", "binary_to_term")
pub fn binary_to_term(term: BitArray) -> anything
