---
name: nativefunction
description: Implement a new JavaScript native/builtin function (e.g. Array.from, String.prototype.trim, Math.abs).
---

# /nativefunction — Implement a JS native function

Implement a new JavaScript built-in/native function in the Lumen runtime.

The user will specify the function, e.g. `/nativefunction Array.from` or `/nativefunction String.prototype.trim`.

## Phase 1: Research prior art (MANDATORY)

Before writing any code, study how existing JS engines implement this function. This is critical — JS semantics are full of subtle edge cases.

1. **Read the ECMAScript spec** — WebSearch for the spec section (e.g. "Array.from ecma262 spec").
2. **Study QuickJS** — Best first reference. Search GitHub for the function name in `quickjs.c`:
   - WebFetch `https://raw.githubusercontent.com/nicedoc/quickjs/master/quickjs.c` or search GitHub
   - Look for the C implementation, note edge cases, type coercions, error conditions
3. **Cross-reference engine262** — JS engine in JS, maps directly to spec. Search in `engine262/src/`:
   - WebFetch from `https://github.com/nicedoc/engine262`
4. **Check test262** — Look at existing test cases to understand expected behavior:
   - Search `vendor/test262/test/built-ins/` for the relevant directory

Summarize the key semantics, edge cases, and spec requirements before proceeding.

## Phase 2: Implementation

Touch these files in order:

### 1. `src/lumen/vm/value.gleam` — Add NativeFn variant

Add a new variant to the `NativeFn` type. Naming convention:

- **Static methods** (on constructor): `NativeArrayFrom`, `NativeMathAbs`, `NativeObjectCreate`
- **Instance methods** (on prototype): `NativeArrayPrototypeMap`, `NativeStringPrototypeTrim`
- **Constructors**: `NativeMapConstructor`, `NativeSetConstructor`

```gleam
pub type NativeFn {
  // ... existing variants ...
  NativeArrayFrom           // Array.from — static
  NativeArrayPrototypeMap   // Array.prototype.map — instance
}
```

No other changes needed in value.gleam — these are plain tag variants with no GC implications.

### 2. `src/lumen/vm/builtins/<module>.gleam` — Implement the function

Find the correct builtin module (or create a new one if needed):

| JS Object                     | Gleam module                    |
| ----------------------------- | ------------------------------- |
| Array / Array.prototype       | `builtins/array.gleam`          |
| Object / Object.prototype     | `builtins/object.gleam`         |
| Function / Function.prototype | `builtins/function.gleam`       |
| Error (and subtypes)          | `builtins/error.gleam`          |
| Math                          | `builtins/math.gleam`           |
| String.prototype              | **NEW** `builtins/string.gleam` |
| Number.prototype              | **NEW** `builtins/number.gleam` |
| Other new globals             | **NEW** `builtins/<name>.gleam` |

#### Function signature patterns:

**Instance methods** (need `this`):

```gleam
pub fn array_map(
  this: JsValue,
  args: List(JsValue),
  heap: Heap,
) -> #(Heap, Result(JsValue, JsValue))
```

**Static methods** (no `this`):

```gleam
pub fn array_from(
  args: List(JsValue),
  heap: Heap,
  array_proto: Ref,    // if it needs to create arrays
) -> #(Heap, Result(JsValue, JsValue))
```

Return type is always `#(Heap, Result(JsValue, JsValue))` where `Error(JsValue)` is a thrown exception.

#### Init wiring:

Use `alloc_native_fn` + `add_method` helpers (already exist in object.gleam and array.gleam, copy the pattern if creating a new module):

```gleam
// Static method — add to constructor
let #(h, from_ref) =
  alloc_native_fn(h, function_proto, NativeArrayFrom, "from", 1)
let h = add_method(h, ctor_ref, "from", from_ref)

// Instance method — add to prototype
let #(h, map_ref) =
  alloc_native_fn(h, function_proto, NativeArrayPrototypeMap, "map", 1)
let h = add_method(h, array_proto, "map", map_ref)
```

The `alloc_native_fn` helper pattern (copy into new modules if needed):

```gleam
fn alloc_native_fn(h, function_proto, native, name, length) -> #(Heap, Ref)
fn add_method(h, obj_ref, name, fn_ref) -> Heap
```

### 3. `src/lumen/vm/builtins.gleam` — Wire new modules (if creating new builtin)

Only needed when adding a NEW global object (like Math, Map, Set). For methods on existing prototypes (Array.prototype.map), skip this step.

- Add `import lumen/vm/builtins/<new_module>`
- Add field to `Builtins` type (e.g. `math: Ref` for object globals, or `BuiltinType` for constructor+prototype pairs)
- Call init in `builtins.init()`

### 4. `src/lumen/vm/vm.gleam` — Add dispatch case

Add a case to `dispatch_native()`:

```gleam
// In dispatch_native():
value.NativeArrayFrom ->
  builtins_array.array_from(args, state.heap, state.builtins.array.prototype)
value.NativeArrayPrototypeMap ->
  builtins_array.array_map(this, args, state.heap)
```

If the new module is not yet imported, add the import at the top.

### 5. `test/compiler_test.gleam` — Add tests

Add tests in `compiler_test.gleam`. Use existing helpers:

```gleam
pub fn array_from_basic_test() {
  assert_normal("Array.from([1,2,3]).join(',')", JsString("1,2,3"))
}
```

If the function needs a global that isn't wired yet, add it to the `globals` dict in `run_js()`.

### 6. `test/test262_exec.gleam` — Wire globals (if new global object)

If you added a new global in `builtins.gleam`, also add it to `make_test262_globals()`:

```gleam
#("Math", value.JsObject(b.math)),
```

## Phase 3: Verify

1. `gleam check` — fast type check
2. `gleam test` — all existing + new tests must pass
3. Report test count to user

## Key patterns to know

- **JsValue types**: JsUndefined, JsNull, JsBool, JsNumber(JsNum), JsString, JsObject(Ref)
- **JsNum**: Finite(Float) | NaN | Infinity | NegInfinity (BEAM can't represent NaN/Inf as floats)
- **Heap objects**: Read with `heap.read(h, ref)`, write with `heap.write(h, ref, slot)`
- **Property flags**: `value.data_property(val)` for normal, `value.builtin_property(val)` for non-enumerable builtins
- **Prototype chain lookup**: `object.get_property(heap, ref, key)` walks the chain
- **Own property only**: Check `properties` dict directly or use `get_own_property` pattern from object.gleam
- **Creating arrays**: Allocate `ObjectSlot(kind: ArrayObject(length), elements: dict, prototype: Some(array_proto))`
- **Creating objects**: Allocate `ObjectSlot(kind: OrdinaryObject, properties: dict, elements: dict.new(), prototype: Some(object_proto))`
- **ToString**: `value.to_js_string(val)` — simple conversion, objects become "[object Object]"
- **ToNumber**: Defined in vm.gleam as `to_number()` — but if you need it in a builtin, reimplement the coercion locally (see math.gleam for example)
- **Throwing errors**: Use `object.make_type_error(heap, builtins, msg)` / `make_range_error` etc. Return as `Error(err_val)`
