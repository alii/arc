//// binds a compiled template to a heap by numbering its keys

import arc/bytecode/key.{type Key, type SourceKey, SourceIndex, SourceName}
import arc/internal/tuple_array
import arc/rt/bytecode.{type FuncTemplate, FuncTemplate}
import arc/rt/store as rt_store
import arc/rt/types.{type Agent}
import gleam/list

pub fn template(
  st: Agent,
  t: FuncTemplate(SourceKey),
) -> #(FuncTemplate(Key), Agent) {
  let #(keys, st) =
    list.fold(tuple_array.to_list(t.keys), #([], st), fn(acc, k) {
      let #(done, st) = acc
      let #(k, st) = source_key(st, k)
      #([k, ..done], st)
    })
  let #(functions, st) =
    list.fold(tuple_array.to_list(t.functions), #([], st), fn(acc, child) {
      let #(done, st) = acc
      let #(child, st) = template(st, child)
      #([child, ..done], st)
    })
  let loaded =
    FuncTemplate(
      ..t,
      keys: tuple_array.from_list(list.reverse(keys)),
      functions: tuple_array.from_list(list.reverse(functions)),
    )
  #(loaded, st)
}

fn source_key(st: Agent, k: SourceKey) -> #(Key, Agent) {
  case k {
    SourceIndex(i) -> #(key.index(i), st)
    SourceName(name) -> rt_store.t_key(st, name)
  }
}
