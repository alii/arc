import arc/bytecode/key
import arc/compiler
import arc/engine.{ModuleReturned, Returned}
import arc/host.{State}
import arc/interp/entry
import arc/module/load_error
import arc/parser
import arc/rt/call.{NormalCompletion}
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/snapshot
import arc/rt/store as rt_store
import arc/rt/types.{type Agent, Agent, JsStore}
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import rt_helpers

fn agent() -> Agent {
  rt_helpers.agent() |> entry.link
}

fn run(st: Agent, source: String) -> #(String, Agent) {
  run_with(st, source, compiler.compile)
}

// repl code keeps top level lets on the realm
fn repl(st: Agent, source: String) -> #(String, Agent) {
  run_with(st, source, compiler.compile_repl)
}

fn run_with(st: Agent, source: String, compile) -> #(String, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compile(body, sb)
  let #(completion, st) = entry.run_script(st, template)
  let assert NormalCompletion(v) = completion
  #(rt_inspect.inspect(st, v), st)
}

fn names(st: Agent) -> Int {
  rt_gc.stats(st).names
}

fn sweep(st: Agent) -> Agent {
  rt_gc.t_collect_full(st, [])
}

pub fn dead_names_are_freed_test() {
  let st = sweep(agent())
  let base = names(st)
  let next0 = st.store.names.next
  let #(_, st) =
    run(
      st,
      "for (var i = 0; i < 20000; i++) { var o = {}; o['zk' + i] = i } o = null",
    )
  assert names(st) >= base + 20_000
  assert st.store.names.next >= next0 + 20_000
  let peak = st.store.names.next
  let st = sweep(st)
  assert names(st) <= base + 2
  assert st.store.names.next == peak
  // a freed text names a fresh number, never an old one
  let #(out, st) = run(st, "o = {}; o['zk' + 7] = 1; Object.keys(o)[0]")
  assert out == "'zk7'"
  assert st.store.names.next == peak + 1
}

pub fn growth_alone_triggers_a_sweep_test() {
  let st = sweep(agent())
  let base = names(st)
  let #(_, st) =
    run(
      st,
      "var o = {};
       function touch(i) { o['q' + i] = 1; delete o['q' + i] }
       for (var i = 0; i < 100000; i++) touch(i);
       'done'",
    )
  // swept at a return safepoint mid run, plain collections only
  assert names(st) < base + 50_000
}

pub fn live_holders_keep_their_names_test() {
  let st = sweep(agent())
  let #(_, st) =
    run(
      st,
      "var live = {}; live['zz' + 'prop'] = 1;
       var shaped = new (function () { this['zz' + 'slot'] = 2 })();
       var later = function (o) { o.zzlater = 3; return Object.keys(o)[0] };
       var it = (function* () { yield 0; var o = { zzgen: 4 }; yield Object.keys(o)[0] })();
       it.next();
       'ok'",
    )
  let #(_, st) = repl(st, "let zzlex = 5; const zzconst = 6; 'ok'")
  let st = sweep(st)
  assert rt_store.t_find_key(st, "zzprop") != None
  assert rt_store.t_find_key(st, "zzslot") != None
  assert rt_store.t_find_key(st, "zzlater") != None
  assert rt_store.t_find_key(st, "zzgen") != None
  assert rt_store.t_find_key(st, "zzlex") != None
  let #(out, st) =
    run(
      st,
      "[Object.keys(live)[0], live.zzprop, Object.keys(shaped)[0], shaped.zzslot,
        later({}), it.next().value].join()",
    )
  assert out == "'zzprop,1,zzslot,2,zzlater,zzgen'"
  let #(out, _) = repl(st, "zzlex + zzconst")
  assert out == "11"
}

pub fn dead_holders_release_their_names_test() {
  let st = sweep(agent())
  let #(_, st) =
    run(
      st,
      "var live = {}; live['yy' + 'prop'] = 1;
       var later = function (o) { return o.yylater };
       var it = (function* () { yield 0; return ({ yygen: 4 }).yygen })();
       it.next();
       live = later = it = null",
    )
  let st = sweep(st)
  assert rt_store.t_find_key(st, "yyprop") == None
  assert rt_store.t_find_key(st, "yylater") == None
  assert rt_store.t_find_key(st, "yygen") == None
}

pub fn private_names_are_freed_with_their_class_test() {
  let st = sweep(agent())
  let base = names(st)
  let #(_, st) =
    run(
      st,
      "for (var i = 0; i < 5000; i++) {
         var C = class { #zzpa = 1; #zzpb() {} static m(o) { return o.#zzpa } };
         new C();
       }
       C = null",
    )
  assert names(st) >= base + 10_000
  let st = sweep(st)
  assert names(st) <= base + 2
}

pub fn live_class_keeps_private_names_without_instances_test() {
  let st = sweep(agent())
  let #(_, st) =
    run(
      st,
      "var A = class {
         #zzpx = 7;
         static get(o) { return o.#zzpx }
         static has(o) { return #zzpx in o }
       };
       'ok'",
    )
  let st = sweep(st)
  let #(out, st) =
    run(
      st,
      "var msg; try { A.get({}) } catch (e) { msg = e.message }
       [A.has({}), /#zzpx/.test(msg), A.get(new A()), A.has(new A())].join()",
    )
  assert out == "'false,true,7,true'"
  let st = sweep(st)
  let #(out, _) = run(st, "A.get(new A())")
  assert out == "7"
}

pub fn json_parse_names_are_freed_test() {
  let st = sweep(agent())
  let base = names(st)
  let #(out, st) =
    run(
      st,
      "var s = '{' + Array.from({ length: 10000 }, function (_, i) { return '\"j' + i + '\":' + i }).join() + '}';
       var o = JSON.parse(s); var n = Object.keys(o).length; o = null; n",
    )
  assert out == "10000"
  assert names(st) >= base + 10_000
  let st = sweep(st)
  // the script's own globals stay named
  assert names(st) <= base + 5
}

pub fn snapshot_round_trips_after_a_sweep_test() {
  let st = sweep(agent())
  let #(_, st) =
    run(
      st,
      "var keep = {}; keep['zzsnap'] = 9; var t = {}; t['zzgone'] = 1; t = null",
    )
  let st = sweep(st)
  let pinned = dict.size(st.store.names.pinned)
  let swept = st.store.names.swept
  let assert Ok(bin) = snapshot.serialize(st)
  let assert Ok(st) = snapshot.deserialize(bin, rt_helpers.quiet_hooks())
  assert dict.size(st.store.names.pinned) == pinned
  assert st.store.names.swept == swept
  assert rt_store.t_find_key(st, "zzgone") == None
  let #(out, _) = run(st, "Object.keys(keep)[0] + keep.zzsnap")
  assert out == "'zzsnap9'"
}

pub fn unknown_key_text_is_a_placeholder_test() {
  let st = agent()
  assert rt_store.t_key_text(st, key.name(999_999)) == "<key 3999996>"
  // a holder the sweep missed still prints and enumerates
  let #(_, st) = run(st, "var lost = {}; lost['zzlost'] = 1; 'ok'")
  let assert Some(k) = rt_store.t_find_key(st, "zzlost")
  let js = st.store
  let names =
    types.NameTable(
      ..js.names,
      numbers: dict.delete(js.names.numbers, "zzlost"),
      texts: dict.delete(js.names.texts, k),
    )
  let st = Agent(..st, store: JsStore(..js, names:))
  let #(out, st) = run(st, "Object.keys(lost)[0] + ':' + lost['zzlost']")
  assert string.starts_with(out, "'<key ")
  assert string.ends_with(out, ">:undefined'")
  let #(v, st) = rt_helpers.global(st, "lost")
  assert string.contains(rt_inspect.inspect(st, v), "<key ")
}

pub fn module_exports_survive_a_sweep_test() {
  let eng = engine.new()
  let assert Ok(#(ModuleReturned(namespace: ns, ..), eng)) =
    engine.eval_module(
      eng,
      "test:names",
      "export const zzexported = 41; export function zzbump() { return zzexported + 1 }",
      fn(_, _) { Error(load_error.ResolveForbidden) },
      fn(_) { Error(load_error.LoadForbidden) },
    )
  let #(eng, Nil) =
    engine.with_state(eng, fn(s) {
      #(State(..s, agent: rt_gc.t_collect_full(s.agent, [])), Nil)
    })
  let assert Some(bump) = engine.read_export(eng, ns, "zzbump")
  let assert #(Returned(value:), eng) =
    engine.call(eng, bump, types.mk_undefined(), [])
  assert engine.inspect(eng, value) == "42"
  let assert Some(v) = engine.read_export(eng, ns, "zzexported")
  assert engine.inspect(eng, v) == "41"
}

pub fn running_frames_keep_their_names_test() {
  let st = sweep(agent())
  let base = names(st)
  // the script body's own keys live only in its running frame
  let #(out, st) =
    run(
      st,
      "function alloc(i) { var t = {}; t['g' + i] = i; return t }
       function loop() { for (var i = 0; i < 100000; i++) alloc(i) }
       loop();
       var o = {};
       o.zztop = 2;
       Object.keys(o)[0] + o.zztop",
    )
  assert out == "'zztop2'"
  // swept while the loop ran
  assert names(st) < base + 50_000
}
