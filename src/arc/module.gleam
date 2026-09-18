import arc/bytecode/error_kind.{SyntaxError, TypeError}
import arc/bytecode/key.{Named}
import arc/compiler.{type ExportSeed}
import arc/esm
import arc/internal/tuple_array.{type TupleArray}
import arc/internal/unsafe
import arc/interp/entry
import arc/interp/safepoint
import arc/interp/state.{type State, State}
import arc/link
import arc/module/graph
import arc/module/load_error.{type LoadError, type ResolveError}
import arc/module/registry
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins/reflect as b_reflect
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call as rt_call
import arc/rt/closure as rt_closure
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledCode, type Handle, type JsVal, type ReflectNative,
  FnFlags, KHandle, KStr, KTdz, ModuleNamespace, NoElements, PromiseFulfilled,
  PromisePending, PromiseRejected, ProxyObj, ReflectDefineProperty,
  ReflectDeleteProperty, ReflectGet, ReflectGetOwnPropertyDescriptor, ReflectHas,
  ReflectOwnKeys, SAsyncContext, SBox, SObject, SPromiseData, StepAwait,
  StepReturn, StepThrow, StepYield, StringKey, classify, mk_object, mk_string,
  mk_tdz, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type CompiledModule {
  CompiledModule(
    specifier: esm.Resolved,
    template: FuncTemplate,
    import_bindings: List(#(esm.Raw, List(esm.ImportBinding))),
    export_entries: List(esm.ExportEntry),
    export_names: Dict(String, Int),
    specifier_map: esm.SpecifierMap,
    requested_modules: List(#(esm.Resolved, esm.Phase)),
    export_seeds: Dict(String, ExportSeed),
    hoisted_funcs: List(#(String, Int)),
    has_tla: Bool,
  )
}

pub type HostModule {
  HostModule(specifier: String, exports: List(#(String, JsVal)))
}

pub type BundleModule {
  SourceModule(compiled: CompiledModule)
  SyntheticModule(host: HostModule)
}

pub type ModuleBundle {
  ModuleBundle(entry: String, modules: Dict(String, BundleModule))
}

fn with_source_module(
  bundle_module: BundleModule,
  default: a,
  k: fn(CompiledModule) -> a,
) -> a {
  case bundle_module {
    SourceModule(compiled) -> k(compiled)
    SyntheticModule(_) -> default
  }
}

pub fn source_specifiers(bundle: ModuleBundle) -> List(String) {
  use acc, spec, bundle_module <- dict.fold(bundle.modules, [])
  use _compiled <- with_source_module(bundle_module, acc)
  [spec, ..acc]
}

pub type CompileBundleError {
  GraphError(error: graph.GraphError)
  CompileError(specifier: String, error: compiler.CompileError)
}

pub type ModuleError {
  NotInBundle(specifier: String)
  EvaluationError(value: JsVal)
  EvaluationPending(promise: Handle)
}

const tla_never_settled_message = "module evaluation never completed: top-level await promise never settled"

pub fn compile_bundle_error_message(err: CompileBundleError) -> String {
  case err {
    GraphError(error: graph.ParseFailed(specifier, parse_error)) ->
      "SyntaxError in '"
      <> esm.resolved_text(specifier)
      <> "': "
      <> parser.parse_error_to_string(parse_error)
    GraphError(error: graph.ResolveFailed(raw, referrer, error)) ->
      load_error.resolve_failure_message(
        esm.raw_text(raw),
        esm.resolved_text(referrer),
        error,
      )
    GraphError(error: graph.LoadFailed(specifier, error)) ->
      load_error.load_failure_message(esm.resolved_text(specifier), error)
    GraphError(error: graph.SourcePhaseUnsupported(specifier)) ->
      "'"
      <> esm.resolved_text(specifier)
      <> "': source phase imports ('import source') are not supported"
    CompileError(specifier:, error:) ->
      compiler.error_message(error) <> " in '" <> specifier <> "'"
  }
}

pub fn format_compile_bundle_error(err: CompileBundleError) -> String {
  let phase = case err {
    GraphError(error: graph.ParseFailed(..)) -> ""
    GraphError(error: graph.ResolveFailed(..))
    | GraphError(error: graph.LoadFailed(..)) -> "ResolutionError: "
    GraphError(error: graph.SourcePhaseUnsupported(..)) -> "LinkError: "
    CompileError(..) -> "CompileError: "
  }
  phase <> compile_bundle_error_message(err)
}

pub fn module_error_phase(err: ModuleError) -> String {
  case err {
    NotInBundle(..) -> "ResolutionError: "
    EvaluationError(..) | EvaluationPending(..) -> ""
  }
}

pub fn error_message(err: ModuleError, st: Agent) -> String {
  case err {
    NotInBundle(specifier:) ->
      "Module '" <> specifier <> "' not found in bundle"
    EvaluationError(value:) -> "Uncaught " <> rt_inspect.format_error(st, value)
    EvaluationPending(promise: _) -> tla_never_settled_message
  }
}

pub type LinkInvariantBroken {
  UnresolvedDependency(specifier: esm.Raw)
  ModuleNotLinked(specifier: String)
  MissingExportCell(dep: String, name: String)
  MissingDeferredBox(dep: String)
  PreexistingNotANamespace(specifier: String, namespace: Handle)
  NamespaceBoxCorrupt(specifier: String)
}

pub fn link_invariant_message(broken: LinkInvariantBroken) -> String {
  "arc/module: linker invariant broken: " <> string.inspect(broken)
}

fn assert_link_invariant(result: Result(a, LinkInvariantBroken)) -> a {
  case result {
    Ok(v) -> v
    Error(broken) -> panic as link_invariant_message(broken)
  }
}

pub type EvaluatedBundle {
  EvaluatedBundle(value: JsVal, namespace: Handle)
}

pub fn compile_bundle(
  entry_specifier: String,
  entry_source: String,
  resolve: fn(String, String) -> Result(String, ResolveError),
  load: fn(String) -> Result(String, LoadError),
) -> Result(ModuleBundle, CompileBundleError) {
  compile_bundle_with_hosts(
    entry_specifier,
    entry_source,
    resolve,
    load,
    dict.new(),
  )
}

pub fn compile_bundle_with_hosts(
  entry_specifier: String,
  entry_source: String,
  resolve: fn(String, String) -> Result(String, ResolveError),
  load: fn(String) -> Result(String, LoadError),
  host_modules: Dict(String, HostModule),
) -> Result(ModuleBundle, CompileBundleError) {
  let resolve_request = fn(request: esm.ModuleRequest, referrer) {
    resolve(esm.raw_text(request.specifier), esm.resolved_text(referrer))
    |> result.map(esm.resolved_unchecked)
  }
  let load_source = fn(spec) { load(esm.resolved_text(spec)) }
  use source_graph <- result.try(
    graph.load(
      esm.resolved_unchecked(entry_specifier),
      entry_source,
      resolve_request,
      load_source,
      fn(spec) { dict.has_key(host_modules, esm.resolved_text(spec)) },
    )
    |> result.map_error(GraphError),
  )
  let with_hosts =
    dict.map_values(host_modules, fn(_spec, hm) { SyntheticModule(hm) })
  use modules <- result.map(
    dict.fold(source_graph.modules, Ok(with_hosts), fn(acc, specifier, node) {
      use modules <- result.try(acc)
      use compiled <- result.map(compile_source_module(node))
      dict.insert(modules, esm.resolved_text(specifier), SourceModule(compiled))
    }),
  )
  ModuleBundle(entry: entry_specifier, modules:)
}

fn compile_source_module(
  node: graph.SourceModule,
) -> Result(CompiledModule, CompileBundleError) {
  let graph.SourceModule(
    parsed: graph.ParsedModule(specifier:, source: _, items:, sb:, summary:),
    resolved:,
  ) = node
  use body <- result.map(
    compiler.compile_module(items, sb, summary)
    |> result.map_error(fn(error) {
      CompileError(specifier: esm.resolved_text(specifier), error:)
    }),
  )
  let requested_modules =
    list.map(resolved, fn(edge) {
      let #(request, resolved_specifier) = edge
      #(resolved_specifier, request.phase)
    })
  CompiledModule(
    specifier:,
    template: body.template,
    import_bindings: summary.imports,
    export_entries: summary.exports,
    export_names: body.export_names,
    specifier_map: graph.specifier_map(node),
    requested_modules:,
    export_seeds: body.export_seeds,
    hoisted_funcs: body.hoisted_funcs,
    has_tla: body.has_tla,
  )
}

fn seed_value(seed: ExportSeed) -> JsVal {
  case seed {
    compiler.SeedUndefined -> mk_undefined()
    compiler.SeedUninitialized -> mk_tdz()
  }
}

fn linkable_of_bundle(bundle: ModuleBundle) -> link.LinkableGraph {
  use acc, specifier, bundle_module <- dict.fold(bundle.modules, dict.new())
  let linkable = case bundle_module {
    SourceModule(m) ->
      link.project_module(m.import_bindings, m.export_entries, m.specifier_map)
      |> result.map_error(UnresolvedDependency)
      |> assert_link_invariant
    SyntheticModule(hm) ->
      link.LinkableModule(
        import_bindings: [],
        export_entries: list.map(hm.exports, fn(e) {
          link.LocalExport(export_name: e.0, local_name: e.0)
        }),
        star_exports: [],
      )
  }
  dict.insert(acc, esm.resolved_unchecked(specifier), linkable)
}

pub type LinkedModule {
  LinkedModule(
    local_boxes: Dict(String, Handle),
    exports: Dict(String, Handle),
    namespace_box: Handle,
    unit_id: Int,
  )
}

pub type Linked {
  Linked(
    modules: Dict(String, LinkedModule),
    deferred_boxes: Dict(String, Handle),
  )
}

type ModuleEvalStatus {
  Evaluating
  Evaluated
  Failed(value: JsVal)
}

pub type Finish =
  fn(Agent) -> Agent

fn no_drain(st: Agent) -> Agent {
  st
}

type GraphEvaluation {
  GraphEvaluation(agent: Agent, modules: Dict(String, ModuleEvalStatus))
}

fn evaluated_specifiers(evaluation: GraphEvaluation) -> Set(String) {
  use acc, spec, status <- dict.fold(evaluation.modules, set.new())
  case status {
    Evaluated -> set.insert(acc, spec)
    Evaluating | Failed(_) -> acc
  }
}

fn module_eval_status(
  evaluation: GraphEvaluation,
  specifier: String,
) -> Option(ModuleEvalStatus) {
  use <- option.lazy_or(
    dict.get(evaluation.modules, specifier) |> option.from_result,
  )
  use <- option.lazy_or(
    registry.read_module_error(evaluation.agent, specifier)
    |> option.map(Failed),
  )
  case registry.read_module_status(evaluation.agent, specifier) {
    Some(registry.Evaluated) -> Some(Evaluated)
    Some(registry.Evaluating) -> Some(Evaluating)
    None -> None
  }
}

fn set_eval_status(
  evaluation: GraphEvaluation,
  specifier: String,
  status: ModuleEvalStatus,
) -> GraphEvaluation {
  GraphEvaluation(
    ..evaluation,
    modules: dict.insert(evaluation.modules, specifier, status),
  )
}

fn with_agent(evaluation: GraphEvaluation, st: Agent) -> GraphEvaluation {
  GraphEvaluation(..evaluation, agent: st)
}

fn try_fold_state(
  items: List(i),
  s: s,
  initial: a,
  f: fn(s, a, i) -> #(s, Result(a, e)),
) -> #(s, Result(a, e)) {
  use acc, item <- list.fold(items, #(s, Ok(initial)))
  case acc {
    #(_, Error(_)) -> acc
    #(s, Ok(v)) -> f(s, v, item)
  }
}

pub type LinkedBundle {
  LinkedBundle(bundle: ModuleBundle, linked: Linked)
}

pub fn entry_namespace_of(linked_bundle: LinkedBundle, st: Agent) -> Handle {
  entry_namespace(linked_bundle.linked, linked_bundle.bundle.entry, st)
}

pub fn link_for_evaluation(
  bundle: ModuleBundle,
  st: Agent,
) -> #(Agent, Result(LinkedBundle, ModuleError)) {
  link_for_evaluation_reusing(bundle, st, dict.new(), dict.new())
}

pub fn link_for_evaluation_reusing(
  bundle: ModuleBundle,
  st: Agent,
  preexisting: Dict(String, Handle),
  preexisting_deferred: Dict(String, Handle),
) -> #(Agent, Result(LinkedBundle, ModuleError)) {
  let lg = linkable_of_bundle(bundle)
  case link.validate(lg) {
    Error(link_error) -> {
      let #(err, st) =
        rt_val.t_new_error(st, SyntaxError, link.error_message(link_error))
      #(st, Error(EvaluationError(err)))
    }
    Ok(Nil) -> {
      let pre =
        dict.fold(preexisting, dict.new(), fn(acc, spec, ns) {
          case rt_store.t_cell_get(st, ns) {
            SObject(kind: ModuleNamespace(exports:), ..) ->
              dict.insert(acc, spec, ReusedNamespace(ns, exports))
            _ ->
              assert_link_invariant(Error(PreexistingNotANamespace(spec, ns)))
          }
        })
      case stale_reused_export(bundle, lg, pre) {
        Some(#(spec, name)) -> {
          let #(err, st) =
            rt_val.t_new_error(
              st,
              SyntaxError,
              stale_reused_export_message(spec, name),
            )
          #(st, Error(EvaluationError(err)))
        }
        None -> {
          let #(st, linked, deferred_to_fill) =
            build_linked(bundle, st, pre, preexisting_deferred)
          let st =
            list.fold(deferred_to_fill, st, fn(st, pair) {
              let #(spec, proxy) = pair
              fill_deferred_namespace(st, bundle, linked, spec, proxy)
            })
          let st =
            instantiate_hoisted_functions(
              bundle,
              linked,
              st,
              set.from_list(dict.keys(pre)),
            )
          #(st, Ok(LinkedBundle(bundle:, linked:)))
        }
      }
    }
  }
}

pub fn evaluate_linked(
  linked_bundle: LinkedBundle,
  st: Agent,
  finish: Finish,
) -> #(Agent, Result(EvaluatedBundle, ModuleError)) {
  let #(st, _evaluated, res) =
    evaluate_linked_tracking(linked_bundle, st, finish, set.new())
  case res {
    Error(EvaluationPending(promise: _)) -> {
      let #(err, st) =
        rt_val.t_new_error(st, TypeError, tla_never_settled_message)
      #(st, Error(EvaluationError(value: err)))
    }
    other -> #(st, other)
  }
}

pub fn evaluate_linked_tracking(
  linked_bundle: LinkedBundle,
  st: Agent,
  finish: Finish,
  already_evaluated: Set(String),
) -> #(Agent, Set(String), Result(EvaluatedBundle, ModuleError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  let modules =
    set.fold(already_evaluated, dict.new(), fn(acc, spec) {
      dict.insert(acc, spec, Evaluated)
    })
  let evaluation = GraphEvaluation(agent: st, modules:)
  let #(evaluation, res) =
    eval_specifier(bundle, linked, evaluation, bundle.entry, finish)
  let res = {
    use value <- result.map(res)
    EvaluatedBundle(
      value:,
      namespace: entry_namespace(linked, bundle.entry, evaluation.agent),
    )
  }
  #(evaluation.agent, evaluated_specifiers(evaluation), res)
}

fn read_namespace_box(
  st: Agent,
  spec: String,
  box: Handle,
) -> Result(Handle, LinkInvariantBroken) {
  case rt_store.t_cell_get(st, box) {
    SBox(value:) ->
      case classify(value) {
        KHandle(ns) -> Ok(ns)
        _ -> Error(NamespaceBoxCorrupt(specifier: spec))
      }
    _ -> Error(NamespaceBoxCorrupt(specifier: spec))
  }
}

fn read_box_dict(
  boxes: Dict(String, Handle),
  st: Agent,
) -> List(#(String, Handle)) {
  use acc, spec, box <- dict.fold(boxes, [])
  let ns = read_namespace_box(st, spec, box) |> assert_link_invariant
  [#(spec, ns), ..acc]
}

pub fn linked_namespaces(
  linked_bundle: LinkedBundle,
  st: Agent,
) -> List(#(String, Handle)) {
  use acc, spec, lm <- dict.fold(linked_bundle.linked.modules, [])
  let ns =
    read_namespace_box(st, spec, lm.namespace_box) |> assert_link_invariant
  [#(spec, ns), ..acc]
}

pub fn linked_deferred_namespaces(
  linked_bundle: LinkedBundle,
  st: Agent,
) -> List(#(String, Handle)) {
  read_box_dict(linked_bundle.linked.deferred_boxes, st)
}

pub type DeferredNamespaceError {
  DeferredSpecifierNotInBundle(specifier: String)
}

pub fn get_or_create_deferred_namespace(
  st: Agent,
  linked_bundle: LinkedBundle,
  spec: String,
) -> #(Agent, Result(Handle, DeferredNamespaceError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  use <- bool.lazy_guard(!dict.has_key(bundle.modules, spec), fn() {
    #(st, Error(DeferredSpecifierNotInBundle(specifier: spec)))
  })
  case dict.get(linked.deferred_boxes, spec) {
    Ok(box) -> #(
      st,
      Ok(read_namespace_box(st, spec, box) |> assert_link_invariant),
    )
    Error(Nil) -> {
      let #(proxy, st) = reserve_cell(st)
      let st = fill_deferred_namespace(st, bundle, linked, spec, proxy)
      #(st, Ok(proxy))
    }
  }
}

pub fn evaluate_bundle(
  bundle: ModuleBundle,
  st: Agent,
  finish: Finish,
) -> #(Agent, Result(EvaluatedBundle, ModuleError)) {
  case link_for_evaluation(bundle, st) {
    #(st, Error(err)) -> #(st, Error(err))
    #(st, Ok(linked_bundle)) -> evaluate_linked(linked_bundle, st, finish)
  }
}

fn entry_namespace(linked: Linked, entry: String, st: Agent) -> Handle {
  let lm =
    dict.get(linked.modules, entry)
    |> result.replace_error(ModuleNotLinked(entry))
    |> assert_link_invariant
  read_namespace_box(st, entry, lm.namespace_box) |> assert_link_invariant
}

pub fn read_export(st: Agent, namespace: JsVal, name: String) -> Option(JsVal) {
  use ns <- option.then(case classify(namespace) {
    KHandle(h) -> Some(h)
    _ -> None
  })
  use exports <- option.then(case rt_store.t_cell_get(st, ns) {
    SObject(kind: ModuleNamespace(exports:), ..) -> Some(exports)
    _ -> None
  })
  use box <- option.then(dict.get(exports, name) |> option.from_result)
  case rt_store.t_cell_get(st, box) {
    SBox(value:) ->
      case classify(value) {
        KTdz -> None
        _ -> Some(value)
      }
    _ -> None
  }
}

fn eval_specifier(
  bundle: ModuleBundle,
  linked: Linked,
  evaluation: GraphEvaluation,
  specifier: String,
  finish: Finish,
) -> #(GraphEvaluation, Result(JsVal, ModuleError)) {
  case dict.get(bundle.modules, specifier) {
    Error(Nil) -> #(evaluation, Error(NotInBundle(specifier:)))
    Ok(SyntheticModule(_)) -> #(evaluation, Ok(mk_undefined()))
    Ok(SourceModule(compiled)) ->
      case module_eval_status(evaluation, specifier) {
        Some(Evaluated) -> #(evaluation, Ok(mk_undefined()))
        Some(Failed(err)) -> #(evaluation, Error(EvaluationError(err)))
        // circular dependency
        Some(Evaluating) -> #(evaluation, Ok(mk_undefined()))
        None ->
          eval_module_body(
            bundle,
            linked,
            evaluation,
            specifier,
            compiled,
            finish,
          )
      }
  }
}

fn eval_module_body(
  bundle: ModuleBundle,
  linked: Linked,
  evaluation: GraphEvaluation,
  specifier: String,
  compiled: CompiledModule,
  finish: Finish,
) -> #(GraphEvaluation, Result(JsVal, ModuleError)) {
  let evaluation = set_eval_status(evaluation, specifier, Evaluating)

  let #(evaluation, dep_result) = {
    use evaluation, Nil, #(resolved_dep, phase) <- try_fold_state(
      compiled.requested_modules,
      evaluation,
      Nil,
    )
    let dep_specifier = esm.resolved_text(resolved_dep)
    let to_evaluate = case phase {
      esm.Evaluation -> [dep_specifier]
      esm.Deferred ->
        gather_async_transitive_deps(
          bundle,
          evaluation,
          dep_specifier,
          set.new(),
        ).0
    }
    use evaluation, Nil, dep <- try_fold_state(to_evaluate, evaluation, Nil)
    let #(evaluation, r) =
      eval_specifier(bundle, linked, evaluation, dep, finish)
    #(evaluation, result.replace(r, Nil))
  }

  case dep_result {
    // pending tla is not a failure, don't cache
    Error(EvaluationPending(promise: _) as err) -> #(evaluation, Error(err))
    Error(err) -> {
      let #(error_val, st) = case err {
        EvaluationError(value: v) -> #(v, evaluation.agent)
        NotInBundle(..) | EvaluationPending(..) ->
          rt_val.t_new_error(
            evaluation.agent,
            TypeError,
            error_message(err, evaluation.agent),
          )
      }
      let st = registry.write_module_error(st, specifier, error_val)
      let evaluation =
        with_agent(evaluation, st)
        |> set_eval_status(specifier, Failed(error_val))
      #(evaluation, Error(err))
    }
    Ok(Nil) -> {
      let lm = linked_module(linked, compiled)
      let seeds =
        import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
        |> assert_link_invariant
        |> list.append(own_export_seeds(lm, compiled))
      let st =
        registry.write_module_status(
          evaluation.agent,
          specifier,
          registry.Evaluating,
        )
      let #(outcome, st) =
        run_module_body(st, specifier, compiled, lm.unit_id, seeds, finish)
      case outcome {
        BodyThrew(thrown) -> {
          let st =
            st
            |> registry.clear_module_status(specifier)
            |> registry.write_module_error(specifier, thrown)
          let evaluation =
            with_agent(evaluation, st)
            |> set_eval_status(specifier, Failed(thrown))
          #(evaluation, Error(EvaluationError(thrown)))
        }
        BodyReturned(v) -> {
          let st =
            registry.write_module_status(st, specifier, registry.Evaluated)
          let evaluation =
            with_agent(evaluation, st) |> set_eval_status(specifier, Evaluated)
          #(evaluation, Ok(v))
        }
        BodyPending(promise) -> #(
          with_agent(evaluation, st),
          Error(EvaluationPending(promise:)),
        )
      }
    }
  }
}

type BodyOutcome {
  BodyReturned(JsVal)
  BodyThrew(JsVal)
  BodyPending(Handle)
}

fn module_locals(
  template: FuncTemplate,
  seeds: List(#(Int, JsVal)),
) -> TupleArray(JsVal) {
  list.fold(
    seeds,
    tuple_array.repeat(mk_undefined(), template.local_count),
    fn(acc, seed) { tuple_array.set_unchecked(seed.0, seed.1, acc) },
  )
}

fn module_activation(
  st: Agent,
  template: FuncTemplate,
  unit_id: Int,
  seeds: List(#(Int, JsVal)),
) -> State {
  State(
    agent: st,
    pc: 0,
    stack: [],
    locals: module_locals(template, seeds),
    func: template,
    unit_id:,
    call_stack: [],
    outer_depth: st.call_depth,
    depth: st.call_depth,
    try_stack: [],
    this: mk_undefined(),
    new_target: mk_undefined(),
    home_object: mk_undefined(),
    call_args: [],
    eval_env: None,
  )
}

fn run_module_body(
  st: Agent,
  specifier: String,
  compiled: CompiledModule,
  unit_id: Int,
  seeds: List(#(Int, JsVal)),
  finish: Finish,
) -> #(BodyOutcome, Agent) {
  let outer_referrer = registry.read_active_referrer(st)
  let st = registry.write_active_referrer(st, Some(specifier))
  let #(outcome, st) = run_module_turns(st, compiled, unit_id, seeds, finish)
  #(outcome, registry.write_active_referrer(st, outer_referrer))
}

fn run_module_turns(
  st: Agent,
  compiled: CompiledModule,
  unit_id: Int,
  seeds: List(#(Int, JsVal)),
  finish: Finish,
) -> #(BodyOutcome, Agent) {
  let #(step, st) =
    entry.run_turn(module_activation(st, compiled.template, unit_id, seeds))
  case step {
    StepReturn(v) -> #(BodyReturned(v), safepoint.finish_turn(st, [v], finish))
    StepThrow(e) -> #(BodyThrew(e), safepoint.finish_turn(st, [e], finish))
    StepAwait(awaited, resume) ->
      drive_top_level_await(st, awaited, resume, finish)
    StepYield(..) -> {
      let #(err, st) =
        rt_val.t_new_error(st, TypeError, "InternalError: module body yielded")
      #(BodyThrew(err), safepoint.finish_turn(st, [err], finish))
    }
  }
}

// §16.2.1.5.3.4 execute async module
fn drive_top_level_await(
  st: Agent,
  awaited: JsVal,
  resume: types.Resume,
  finish: Finish,
) -> #(BodyOutcome, Agent) {
  let #(promise, st) = rt_async.t_new_promise(st)
  // held from gleam across drains
  let st = rt_store.t_pin_root(st, promise)
  let #(data, pstate, _) = rt_async.promise_data(st, promise)
  // mark handled, the host inspects it below
  let st = rt_store.t_cell_set(st, data, SPromiseData(pstate, is_handled: True))
  let #(ctx, st) = rt_store.t_cell_new(st, SAsyncContext(resume:, promise:))
  let st = rt_async.t_await(st, ctx, awaited)
  let st = safepoint.finish_turn(st, [], finish)
  case rt_async.promise_data(st, promise) {
    #(_, PromiseFulfilled(v), _) -> #(BodyReturned(v), st)
    #(_, PromiseRejected(reason), _) -> #(BodyThrew(reason), st)
    #(_, PromisePending(_), _) -> #(BodyPending(promise), st)
  }
}

// pinned: binding cells are held from gleam
fn alloc_box(st: Agent, val: JsVal) -> #(Handle, Agent) {
  let #(box, st) = rt_store.t_cell_new(st, SBox(val))
  #(box, rt_store.t_pin_root(st, box))
}

fn reserve_cell(st: Agent) -> #(Handle, Agent) {
  alloc_box(st, mk_undefined())
}

fn reserve_ns_boxes(
  st: Agent,
  specs: List(String),
  preexisting: fn(String) -> Result(Handle, Nil),
) -> #(Agent, Dict(String, Handle), List(#(String, Handle))) {
  use #(st, boxes, fresh), spec <- list.fold(specs, #(st, dict.new(), []))
  case preexisting(spec) {
    Ok(existing) -> {
      let #(box, st) = alloc_box(st, mk_object(existing))
      #(st, dict.insert(boxes, spec, box), fresh)
    }
    Error(Nil) -> {
      let #(obj, st) = reserve_cell(st)
      let #(box, st) = alloc_box(st, mk_object(obj))
      #(st, dict.insert(boxes, spec, box), [#(spec, obj), ..fresh])
    }
  }
}

// a namespace object kept from an earlier load of the same specifier
type ReusedNamespace {
  ReusedNamespace(namespace: Handle, exports: Dict(String, Handle))
}

// a proxy trap on a deferred namespace that forwards to reflect
type DeferredTrap {
  DeferredTrap(
    name: String,
    arity: Int,
    native: ReflectNative,
    always_triggers: Bool,
  )
}

fn build_linked(
  bundle: ModuleBundle,
  st: Agent,
  preexisting: Dict(String, ReusedNamespace),
  preexisting_deferred: Dict(String, Handle),
) -> #(Agent, Linked, List(#(String, Handle))) {
  let #(st, local_boxes) = preallocate_local_boxes(bundle, st, preexisting)
  let specs = dict.keys(bundle.modules)
  let #(st, namespace_boxes, ns_to_fill) =
    reserve_ns_boxes(st, specs, fn(spec) {
      dict.get(preexisting, spec) |> result.map(fn(p) { p.namespace })
    })
  let #(st, deferred_boxes, deferred_to_fill) =
    reserve_ns_boxes(st, needed_deferred_specs(bundle), dict.get(
      preexisting_deferred,
      _,
    ))
  let lg = linkable_of_bundle(bundle)
  let exports =
    list.fold(specs, dict.new(), fn(all, spec) {
      case dict.get(preexisting, spec) {
        Ok(ReusedNamespace(exports: existing_exports, ..)) ->
          dict.insert(all, spec, existing_exports)
        Error(Nil) -> {
          let key = esm.resolved_unchecked(spec)
          let map =
            link.exported_names(lg, key)
            |> list.fold(dict.new(), fn(map, name) {
              case link.resolve_export(lg, key, name) {
                // §16.2.1.6.3 ambiguous star names are not exported
                link.Unresolvable | link.Ambiguous -> map
                link.ResolvedTo(owner, binding) ->
                  dict.get(local_boxes, esm.resolved_text(owner))
                  |> result.try(dict.get(_, binding))
                  |> result.replace_error(MissingExportCell(
                    esm.resolved_text(owner),
                    binding,
                  ))
                  |> assert_link_invariant
                  |> dict.insert(map, name, _)
                link.ResolvedNamespace(target) -> {
                  let assert Ok(box) =
                    dict.get(namespace_boxes, esm.resolved_text(target))
                  dict.insert(map, name, box)
                }
                link.ResolvedDeferredNamespace(target) ->
                  dict.get(deferred_boxes, esm.resolved_text(target))
                  |> result.replace_error(
                    MissingDeferredBox(esm.resolved_text(target)),
                  )
                  |> assert_link_invariant
                  |> dict.insert(map, name, _)
              }
            })
          dict.insert(all, spec, map)
        }
      }
    })
  let st =
    list.fold(ns_to_fill, st, fn(st, pair) {
      let #(spec, obj) = pair
      let assert Ok(exp) = dict.get(exports, spec)
      rt_store.t_cell_set(st, obj, namespace_cell(exp, "Module"))
    })
  let #(st, modules) =
    list.fold(specs, #(st, dict.new()), fn(acc, spec) {
      let #(st, modules) = acc
      let assert Ok(lb) = dict.get(local_boxes, spec)
      let assert Ok(exp) = dict.get(exports, spec)
      let assert Ok(ns_box) = dict.get(namespace_boxes, spec)
      let #(unit_id, st) = rt_store.t_next_unit_id(st)
      let lm =
        LinkedModule(
          local_boxes: lb,
          exports: exp,
          namespace_box: ns_box,
          unit_id:,
        )
      #(st, dict.insert(modules, spec, lm))
    })
  #(st, Linked(modules:, deferred_boxes:), deferred_to_fill)
}

// GatherAsynchronousTransitiveDependencies
fn gather_async_transitive_deps(
  bundle: ModuleBundle,
  evaluation: GraphEvaluation,
  spec: String,
  seen: Set(String),
) -> #(List(String), Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #([], seen))
  let seen = set.insert(seen, spec)
  let already_started = case dict.get(evaluation.modules, spec) {
    Ok(Evaluated) | Ok(Evaluating) -> True
    Ok(Failed(_)) | Error(Nil) ->
      registry.read_module_status(evaluation.agent, spec) != None
  }
  use <- bool.guard(already_started, #([], seen))
  case dict.get(bundle.modules, spec) {
    Error(Nil) | Ok(SyntheticModule(_)) -> #([], seen)
    Ok(SourceModule(m)) ->
      case m.has_tla {
        True -> #([spec], seen)
        False ->
          list.fold(m.requested_modules, #([], seen), fn(acc, request) {
            let #(found, seen) = acc
            let #(more, seen) =
              gather_async_transitive_deps(
                bundle,
                evaluation,
                esm.resolved_text(request.0),
                seen,
              )
            #(list.append(found, more), seen)
          })
      }
  }
}

pub fn evaluate_async_transitive_deps(
  linked_bundle: LinkedBundle,
  st: Agent,
  finish: Finish,
) -> #(Agent, Result(List(#(String, Handle)), ModuleError)) {
  let LinkedBundle(bundle:, linked:) = linked_bundle
  let evaluation = GraphEvaluation(agent: st, modules: dict.new())
  let #(to_evaluate, _seen) =
    gather_async_transitive_deps(bundle, evaluation, bundle.entry, set.new())
  let #(evaluation, res) = {
    use evaluation, pendings, dep <- try_fold_state(to_evaluate, evaluation, [])
    case eval_specifier(bundle, linked, evaluation, dep, finish) {
      #(evaluation, Ok(_)) -> #(evaluation, Ok(pendings))
      #(evaluation, Error(EvaluationPending(promise:))) -> #(
        evaluation,
        Ok([#(dep, promise), ..pendings]),
      )
      #(evaluation, Error(err)) -> #(evaluation, Error(err))
    }
  }
  #(evaluation.agent, result.map(res, list.reverse))
}

fn needed_deferred_specs(bundle: ModuleBundle) -> List(String) {
  dict.fold(bundle.modules, [], fn(acc, _spec, bundle_module) {
    use m <- with_source_module(bundle_module, acc)
    list.fold(m.import_bindings, acc, fn(acc, entry) {
      let #(raw_dep, bindings) = entry
      let is_deferred =
        list.any(bindings, fn(binding) {
          case binding {
            esm.NamespaceImport(phase: esm.Deferred, ..) -> True
            _ -> False
          }
        })
      use <- bool.guard(!is_deferred, acc)
      let dep =
        esm.resolve(m.specifier_map, raw_dep)
        |> option.to_result(UnresolvedDependency(raw_dep))
        |> assert_link_invariant
      [esm.resolved_text(dep), ..acc]
    })
  })
  |> list.unique
}

// §16.2.1.6.4 step 9: exported functions callable before bodies run
fn instantiate_hoisted_functions(
  bundle: ModuleBundle,
  linked: Linked,
  st: Agent,
  already_evaluated: Set(String),
) -> Agent {
  dict.fold(bundle.modules, st, fn(st, spec, bundle_module) {
    use compiled <- with_source_module(bundle_module, st)
    use <- bool.guard(set.contains(already_evaluated, spec), st)
    let lm = linked_module(linked, compiled)
    let seeds =
      import_seeds(linked, compiled.specifier_map, compiled.import_bindings)
      |> assert_link_invariant
      |> list.append(own_export_seeds(lm, compiled))
    let locals = module_locals(compiled.template, seeds)
    list.fold(compiled.hoisted_funcs, st, fn(st, hf) {
      let #(name, func_idx) = hf
      case dict.get(lm.local_boxes, name) {
        Error(Nil) -> st
        Ok(box) -> {
          let child =
            tuple_array.get_unchecked(func_idx, compiled.template.functions)
          let captured =
            list.map(child.env_descriptors, fn(desc) {
              tuple_array.get_unchecked(desc.parent_index, locals)
            })
          let #(closure, st) =
            rt_closure.t_new_bytecode_function(
              st,
              child,
              bytecode.env_from_list(captured),
              lm.unit_id,
            )
          rt_store.t_cell_set(st, box, SBox(mk_object(closure)))
        }
      }
    })
  })
}

fn stale_reused_export(
  bundle: ModuleBundle,
  lg: link.LinkableGraph,
  preexisting: Dict(String, ReusedNamespace),
) -> Option(#(String, String)) {
  dict.to_list(bundle.modules)
  |> list.find_map(fn(entry) {
    let #(spec, bundle_module) = entry
    case bundle_module, dict.get(preexisting, spec) {
      SourceModule(_), Ok(ReusedNamespace(exports: existing_exports, ..)) -> {
        let key = esm.resolved_unchecked(spec)
        link.exported_names(lg, key)
        |> list.find_map(fn(name) {
          case link.resolve_export(lg, key, name) {
            link.Unresolvable | link.Ambiguous -> Error(Nil)
            link.ResolvedTo(..)
            | link.ResolvedNamespace(..)
            | link.ResolvedDeferredNamespace(..) ->
              case dict.has_key(existing_exports, name) {
                True -> Error(Nil)
                False -> Ok(#(spec, name))
              }
          }
        })
      }
      _, _ -> Error(Nil)
    }
  })
  |> option.from_result
}

fn stale_reused_export_message(specifier: String, name: String) -> String {
  "module '"
  <> specifier
  <> "' was re-loaded with an export '"
  <> name
  <> "' its already-instantiated namespace does not have: a loader must return "
  <> "the same source for a specifier it has already served"
}

fn preallocate_local_boxes(
  bundle: ModuleBundle,
  st: Agent,
  preexisting: Dict(String, ReusedNamespace),
) -> #(Agent, Dict(String, Dict(String, Handle))) {
  dict.fold(bundle.modules, #(st, dict.new()), fn(acc, spec, bundle_module) {
    let #(st, all) = acc
    let existing =
      dict.get(preexisting, spec)
      |> option.from_result
      |> option.map(fn(p) { p.exports })
    let #(st, boxes) = case bundle_module, existing {
      SourceModule(m), Some(existing_exports) -> #(
        st,
        list.fold(m.export_entries, dict.new(), fn(boxes, e) {
          case e {
            esm.LocalExport(export_name:, local_name:) ->
              dict.get(existing_exports, export_name)
              |> result.replace_error(MissingExportCell(spec, export_name))
              |> assert_link_invariant
              |> dict.insert(boxes, local_name, _)
            _ -> boxes
          }
        }),
      )
      SourceModule(m), None ->
        dict.fold(m.export_seeds, #(st, dict.new()), fn(a, local, seed) {
          let #(st, boxes) = a
          let #(box, st) = alloc_box(st, seed_value(seed))
          #(st, dict.insert(boxes, local, box))
        })
      SyntheticModule(hm), existing_exports ->
        list.fold(hm.exports, #(st, dict.new()), fn(a, export) {
          let #(st, boxes) = a
          let #(name, val) = export
          let reused =
            option.then(existing_exports, fn(ex) {
              dict.get(ex, name) |> option.from_result
            })
          case reused {
            Some(box) -> #(st, dict.insert(boxes, name, box))
            None -> {
              let #(box, st) = alloc_box(st, val)
              #(st, dict.insert(boxes, name, box))
            }
          }
        })
    }
    #(st, dict.insert(all, spec, boxes))
  })
}

// §10.4.6 module namespace exotic object
fn namespace_cell(exports: Dict(String, Handle), tag: String) -> types.Cell {
  SObject(
    kind: ModuleNamespace(exports:),
    proto: None,
    props: dict.new(),
    symbol_props: [
      #(types.symbol_to_string_tag, types.frozen_property(mk_string(tag), 0)),
    ],
    elements: NoElements,
    extensible: False,
  )
}

fn as_code(
  f: fn(Agent, rt_call.Frame, List(JsVal)) -> #(JsVal, Agent),
) -> CompiledCode {
  unsafe.coerce(f)
}

fn trap_flags() -> types.FnFlags {
  FnFlags(
    is_constructor: False,
    is_class_constructor: False,
    is_derived_constructor: False,
    is_arrow: True,
    is_method: False,
    is_generator: False,
    is_async: False,
    is_strict: True,
  )
}

pub fn alloc_host_fn(
  st: Agent,
  name: String,
  arity: Int,
  body: fn(Agent, List(JsVal)) -> #(JsVal, Agent),
) -> #(Handle, Agent) {
  let code = as_code(fn(st, _frame, args) { body(st, args) })
  rt_call.t_fn_new(st, code, trap_flags(), name, arity, None, None)
}

// proxy whose traps evaluate the module then forward
fn fill_deferred_namespace(
  st: Agent,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
  proxy: Handle,
) -> Agent {
  let lm =
    dict.get(linked.modules, spec)
    |> result.replace_error(ModuleNotLinked(spec))
    |> assert_link_invariant
  let #(target, st) =
    rt_store.t_cell_new(st, namespace_cell(lm.exports, "Deferred Module"))
  let #(handler, st) = rt_obj.t_new_object(st, Some(st.realm.object.prototype))
  let st =
    [
      DeferredTrap("get", 3, ReflectGet, always_triggers: False),
      DeferredTrap("has", 2, ReflectHas, always_triggers: False),
      DeferredTrap(
        "deleteProperty",
        2,
        ReflectDeleteProperty,
        always_triggers: False,
      ),
      DeferredTrap(
        "defineProperty",
        3,
        ReflectDefineProperty,
        always_triggers: False,
      ),
      DeferredTrap(
        "getOwnPropertyDescriptor",
        2,
        ReflectGetOwnPropertyDescriptor,
        always_triggers: False,
      ),
      DeferredTrap("ownKeys", 1, ReflectOwnKeys, always_triggers: True),
    ]
    |> list.fold(st, fn(st, t) {
      let #(fn_h, st) = alloc_deferred_trap(st, t, bundle, linked, spec)
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          handler,
          StringKey(Named(t.name)),
          mk_object(fn_h),
          writable: True,
          enumerable: True,
          configurable: True,
        )
      st
    })
  rt_store.t_cell_set(
    st,
    proxy,
    SObject(
      kind: ProxyObj(target:, handler:, revoked: False),
      proto: None,
      props: dict.new(),
      symbol_props: [],
      elements: NoElements,
      // private field checks read this directly
      extensible: False,
    ),
  )
}

fn alloc_deferred_trap(
  st: Agent,
  trap: DeferredTrap,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> #(Handle, Agent) {
  let DeferredTrap(name:, arity:, native:, always_triggers:) = trap
  use st, args <- alloc_host_fn(
    st,
    "%DeferredNamespace[" <> name <> "]%",
    arity,
  )
  let string_key = case args {
    [_, key, ..] ->
      case classify(key) {
        KStr(k) -> Some(k)
        _ -> None
      }
    _ -> None
  }
  let triggers =
    always_triggers
    || case string_key {
      Some(k) -> k != "then"
      None -> False
    }
  case triggers, native, string_key {
    // "then" is never observable via get
    False, ReflectGet, Some("then") -> #(mk_undefined(), st)
    False, _, _ -> b_reflect.dispatch(st, native, mk_undefined(), args)
    True, _, _ -> {
      let st = ensure_deferred_evaluated(st, bundle, linked, spec)
      b_reflect.dispatch(st, native, mk_undefined(), args)
    }
  }
}

// EnsureDeferredNamespaceEvaluation, raises
fn ensure_deferred_evaluated(
  st: Agent,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> Agent {
  use <- bool.guard(
    registry.read_module_status(st, spec) == Some(registry.Evaluated),
    st,
  )
  case registry.read_module_error(st, spec) {
    Some(err) -> rt_store.t_throw(st, err)
    None ->
      case ready_for_sync_execution(bundle, st, spec, set.new()).0 {
        False ->
          rt_val.t_throw_type_error(
            st,
            "Cannot synchronously evaluate deferred module: '"
              <> spec
              <> "' is still evaluating or has an unevaluated async dependency",
          )
        True -> evaluate_deferred_subgraph(st, bundle, linked, spec)
      }
  }
}

// ReadyForSyncExecution: evaluation-phase requests only
fn ready_for_sync_execution(
  bundle: ModuleBundle,
  st: Agent,
  spec: String,
  seen: Set(String),
) -> #(Bool, Set(String)) {
  use <- bool.guard(set.contains(seen, spec), #(True, seen))
  let seen = set.insert(seen, spec)
  case registry.read_module_status(st, spec) {
    Some(registry.Evaluated) -> #(True, seen)
    Some(registry.Evaluating) -> #(False, seen)
    None ->
      case dict.get(bundle.modules, spec) {
        Error(Nil) | Ok(SyntheticModule(_)) -> #(True, seen)
        Ok(SourceModule(m)) ->
          case m.has_tla {
            True -> #(False, seen)
            False ->
              list.fold(m.requested_modules, #(True, seen), fn(acc, request) {
                case acc.0, request.1 {
                  False, _ -> acc
                  True, esm.Deferred -> acc
                  True, esm.Evaluation ->
                    ready_for_sync_execution(
                      bundle,
                      st,
                      esm.resolved_text(request.0),
                      acc.1,
                    )
                }
              })
          }
      }
  }
}

// EvaluateSync, never drains re-entrantly
fn evaluate_deferred_subgraph(
  st: Agent,
  bundle: ModuleBundle,
  linked: Linked,
  spec: String,
) -> Agent {
  let evaluation = GraphEvaluation(agent: st, modules: dict.new())
  case eval_specifier(bundle, linked, evaluation, spec, no_drain) {
    #(evaluation, Ok(_)) -> evaluation.agent
    #(evaluation, Error(EvaluationError(value:))) ->
      rt_store.t_throw(evaluation.agent, value)
    #(evaluation, Error(NotInBundle(..) as other))
    | #(evaluation, Error(EvaluationPending(..) as other)) ->
      rt_val.t_throw_type_error(
        evaluation.agent,
        "Failed to evaluate deferred module '"
          <> spec
          <> "': "
          <> error_message(other, evaluation.agent),
      )
  }
}

fn import_seeds(
  linked: Linked,
  specifier_map: esm.SpecifierMap,
  import_bindings: List(#(esm.Raw, List(esm.ImportBinding))),
) -> Result(List(#(Int, JsVal)), LinkInvariantBroken) {
  use per_dep <- result.map(
    list.try_map(import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      use dep <- result.try(
        esm.resolve(specifier_map, raw_dep)
        |> option.to_result(UnresolvedDependency(raw_dep)),
      )
      let dep = esm.resolved_text(dep)
      use lm <- result.try(
        dict.get(linked.modules, dep)
        |> result.replace_error(ModuleNotLinked(dep)),
      )
      list.try_map(bindings, fn(binding) {
        case binding {
          esm.NamedImport(imported:, ..) ->
            forward_box(lm.exports, dep, imported)
          esm.DefaultImport(..) -> forward_box(lm.exports, dep, "default")
          esm.NamespaceImport(phase: esm.Deferred, ..) ->
            dict.get(linked.deferred_boxes, dep)
            |> result.replace_error(MissingDeferredBox(dep))
            |> result.map(mk_object)
          esm.NamespaceImport(phase: esm.Evaluation, ..) ->
            Ok(mk_object(lm.namespace_box))
        }
      })
    }),
  )
  per_dep
  |> list.flatten
  |> list.index_map(fn(box, idx) { #(idx, box) })
}

fn forward_box(
  dep_exports: Dict(String, Handle),
  dep: String,
  name: String,
) -> Result(JsVal, LinkInvariantBroken) {
  dict.get(dep_exports, name)
  |> result.replace_error(MissingExportCell(dep, name))
  |> result.map(mk_object)
}

fn linked_module(linked: Linked, compiled: CompiledModule) -> LinkedModule {
  let spec = esm.resolved_text(compiled.specifier)
  dict.get(linked.modules, spec)
  |> result.replace_error(ModuleNotLinked(spec))
  |> assert_link_invariant
}

fn own_export_seeds(
  lm: LinkedModule,
  compiled: CompiledModule,
) -> List(#(Int, JsVal)) {
  let import_locals =
    esm.binding_local_names(compiled.import_bindings) |> set.from_list
  lm.local_boxes
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    let #(local_name, box) = pair
    use <- bool.guard(set.contains(import_locals, local_name), Error(Nil))
    dict.get(compiled.export_names, local_name)
    |> result.map(fn(index) { #(index, mk_object(box)) })
  })
}
