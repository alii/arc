import arc/bytecode/key.{Named}
import arc/rt/builtins/console as b_console
import arc/rt/inspect
import arc/rt/obj as rt_obj
import arc/rt/types.{
  JFloat, JInt, StringKey, mk_bigint, mk_hole, mk_number, mk_string,
  mk_undefined,
}
import rt_helpers

pub fn renders_structures_test() {
  let st = rt_helpers.agent()
  let #(inner, st) = rt_obj.t_new_array(st, [mk_number(JInt(1)), mk_hole()])
  let #(o, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, o, StringKey(Named("a")), inner)
  let #(_, st) = rt_obj.t_set_prop(st, o, StringKey(Named("self")), o)
  assert inspect.inspect(st, o) == "{ a: [ 1, <empty> ], self: [Circular] }"
  let #(is_nan, st) = rt_helpers.global(st, "isNaN")
  let #(line, _) =
    b_console.format(st, [
      mk_string("s"),
      o,
      is_nan,
      mk_bigint(12),
      mk_number(JFloat(-0.0)),
      mk_undefined(),
      mk_string("t"),
    ])
  assert line
    == "s { a: [ 1, <empty> ], self: [Circular] } [Function: isNaN] 12n 0 undefined t"
}

pub fn format_specifiers_quote_objects_test() {
  let st = rt_helpers.agent()
  let #(line, _) =
    b_console.format(st, [mk_string("%o|%s|%d"), mk_string("q"), mk_string("q")])
  assert line == "'q'|q|%d"
}
