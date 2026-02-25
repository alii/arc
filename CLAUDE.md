# Arc — JavaScript Runtime Prior Art

**CRITICAL**: When implementing any JavaScript runtime feature, you MUST study prior art from existing JS engines before writing code. There is an enormous amount of prior work to learn from:

- **QuickJS** (bellard/quickjs) — Best first reference. Small, complete, single-file C implementation. Easy to read and understand. Start here.
- **V8** (Google) — Production-grade, highly optimized. Good for understanding edge cases and spec compliance.
- **JavaScriptCore** (WebKit/Apple) — Another production engine. Good for cross-referencing.
- **engine262** — JS engine written in JS, directly maps to the ECMAScript spec. Excellent for understanding spec semantics.
- **test262** — The conformance test suite itself often reveals edge cases you wouldn't think of.

**How to research**: Use WebFetch/WebSearch to read the actual source code on GitHub. For QuickJS, the main file is `quickjs.c`. Search for the feature you're implementing (e.g. `JS_CallConstructor`, `js_call_c_function`, `this_val`). Read the ECMAScript spec sections referenced in the code. Cross-reference with engine262 for a more readable spec-aligned implementation.

**Do NOT just implement from memory or vibes.** JS semantics are full of subtle edge cases (e.g. `this` in arrow functions vs regular functions, `new.target`, constructor return value semantics). Get it right by reading how others do it.

---

# Writing Gleam Code

- Never assume functions exist in Gleam stdlib/library code. Gleam is a newer language and likely underrepresented in training data.
- Because of that, you need to research more than usual. Spend time reading documentation at hexdocs.pm, gleam.run, GitHub repos, and forums if you are even SLIGHTLY unsure.
- The goal is correct code, not concise or clever code. Idiomatic patterns are firmly defined in Gleam's design, but probably not in your training data. Look it up, research. NEVER assume.
- Use `gleam check` to check your code for errors. It is extremely fast, so feel free to run it very often.
- Consider spawning subtasks/subagents very often. They are super helpful because they enable the main thread to not use up tokens/context.

---

## Error Handling - NEVER silently discard errors

- **NEVER write `Error(_) ->`** — always bind the error and log it with `string.inspect(err)`. Silent error discards make debugging impossible.
  - WRONG: `Error(_) -> default_value`
  - RIGHT: `Error(err) -> { logging.log(logging.Warning, "context: " <> string.inspect(err)); default_value }`
- **NEVER write `fn(_err) {`** in error handlers — same rule, bind it and log it.
- **NEVER write `result.map_error(fn(_) { ... })`** — always include the original error in the new error message.
  - WRONG: `result.map_error(fn(_) { ServerError("parse failed") })`
  - RIGHT: `result.map_error(fn(err) { ServerError("parse failed: " <> string.inspect(err)) })`
- **Acceptable exceptions**: `Error(_)` is OK when the error type is `Nil` (e.g. `int.parse`, `dict.get`) since there's nothing to inspect. Pattern match wildcards like `Ok(_)` to discard success values are also fine.

---

## NEVER write nested case pyramids — use result/option combinators

**Nested `case` expressions where every failure branch returns the same default value are BANNED.** This is a pyramid of doom and is unacceptable in this codebase. Use `result.try`, `result.map`, `result.unwrap`, `option.map`, `option.unwrap`, and Gleam's `use` keyword to flatten them.

**WRONG — pyramid of doom:**
```gleam
let heap = case heap.read(state.heap, ctor_ref) {
  Ok(ObjectSlot(properties: ctor_props, ..)) ->
    case dict.get(ctor_props, "prototype") {
      Ok(DataProperty(value: JsObject(proto_ref), ..)) ->
        case heap.read(state.heap, proto_ref) {
          Ok(ObjectSlot(kind:, properties:, elements:, ..)) ->
            heap.write(state.heap, proto_ref, ObjectSlot(kind:, properties:, elements:, prototype: new_proto))
          _ -> state.heap
        }
      _ -> state.heap
    }
  _ -> state.heap
}
```

**RIGHT — extract a helper or use result combinators:**
```gleam
// Option A: helper functions (preferred when the pattern repeats)
let heap =
  get_field_ref(state.heap, ctor_ref, "prototype")
  |> result.map(set_slot_prototype(state.heap, _, new_proto))
  |> result.unwrap(state.heap)

// Option B: use with result.try (good for one-off chains)
let heap = {
  use ObjectSlot(properties: props, ..) <- result.try(heap.read(state.heap, ctor_ref))
  use DataProperty(value: JsObject(proto_ref), ..) <- result.try(
    dict.get(props, "prototype") |> result.nil_error
  )
  Ok(set_slot_prototype(state.heap, proto_ref, new_proto))
}
|> result.unwrap(state.heap)
```

**The rule**: If you find yourself writing 2+ levels of nested `case` where every `_`/error branch returns the same fallback, STOP and refactor. Either:
1. Extract a helper function that encapsulates the read-match-transform chain
2. Use `result.try`/`result.map`/`result.unwrap` pipeline
3. Use `use` with `result.try` for sequential dependent operations

This applies equally to `Option` chains — use `option.map`/`option.unwrap`/`option.then` instead of nested `case Some/None`.

---

## Gleam OTP Patterns - CRITICAL LESSONS

These apply only when targeting Erlang/BEAM. They do not apply to JavaScript target.

- **NEVER use manual process.register()** - Use `actor.named(builder, name)` in the actor builder chain instead. This is the proper Gleam OTP pattern.
  - WRONG: `process.register(pid, name)` in the initializer
  - RIGHT: `actor.new(...) |> actor.on_message(...) |> actor.named(name) |> actor.start()`

- **NEVER add retry logic for supervised process lookups** - `supervisor.start()` and `actor.start()` are BLOCKING operations. If a process is supervised, it's guaranteed to be registered before the supervisor returns.
  - WRONG: `lookup_with_retry(name, 50, 100)` with sleep delays
  - RIGHT: Direct `process.named(name)` that fails immediately if not found
  - Trust the supervision tree. Retries hide bugs and add complexity.

- **Create process.Name instances ONCE and reuse them** - `process.new_name()` creates unique atoms each time. You must create the Name once and pass the SAME instance to both registration and lookup. Atoms are never garbage collected, so creating them dynamically will eventually crash the VM.
  - WRONG: `process.new_name("foo")` in two places creates "foo_123" and "foo_456"
  - RIGHT: `let name = process.new_name("foo")` once at startup, then pass `name` everywhere

- **Research hexdocs.pm THOROUGHLY before implementing OTP patterns**:
  - Check `gleam/otp/actor` for actor functions like `actor.named()`
  - Check `gleam/erlang/process` for process functions like `process.named_subject()`
  - Look for examples on GitHub
  - Search forums for "gleam named actors" or "gleam supervision"

- **Trust blocking OTP operations** - Don't add defensive retries, timeouts, or polling:
  - `supervisor.start()` blocks until ALL children are started
  - `actor.start()` blocks until the actor is fully initialized AND registered (if using actor.named)
  - RestForOne/OneForOne supervisors start children SEQUENTIALLY in order
  - This means lookups after supervisor.start() are GUARANTEED to succeed if setup is correct

- **CRITICAL: Avoid try_call Pattern** - DO NOT implement or use a `try_call` function that wraps `process.call` to return `Result` instead of panicking. This pattern was removed from gleam_erlang because it causes **memory leaks**. If the callee responds after timeout, the message leaks in your mailbox forever.
  - Use `process.call` and let timeouts crash the process
  - Handle crashes through supervision (the supervisor restarts the process)
  - Reference: https://github.com/gleam-lang/erlang/pull/71

---

## General Rules - DO RESEARCH BEFORE ACTING

- ALWAYS look at what files actually exist before running commands. Use Glob/Read to check for test files, build scripts, config files, etc.
- NEVER blindly run commands like `gleam test`, `npm test`, `make`, etc. without first verifying those workflows exist in the project.
- Example: Before running `gleam test`, search for `**/*.gleam` files with "test" in the name, or look for a test directory. If none exist, DO NOT run test commands.
- NEVER assume standard project structures. Each project is different - research what's actually there first.
- NEVER assume build scripts, Makefiles, package.json scripts, CI/CD configs exist. Look first.
- If you need to verify code works, ASK the user how they normally verify/test their code instead of assuming a testing approach.
- The goal is to understand the ACTUAL project structure and workflows by researching files, not to assume standard conventions.

---

# Gleam Language Fundamentals

## No `if` Statement - Use `case` For Everything

Gleam has no `if` expression. All conditional logic uses `case`:

```gleam
// WRONG - doesn't compile
if x > 0 { "positive" } else { "non-positive" }

// RIGHT - use case
case x > 0 {
  True -> "positive"
  False -> "non-positive"
}
```

## Imports

Gleam requires explicit imports for types and constructors:

```gleam
import gleam/option.{type Option, None, Some}
import gleam/result
import myapp/messages.{type Event, Started, Stopped}
```

## Pattern Matching with `case`

Gleam uses `case` for pattern matching:

```gleam
case some_option {
  Some(value) -> use_value(value)
  None -> handle_none()
}
```

## Records

Create records with positional or labeled arguments:

```gleam
pub type User {
  User(name: String, age: Int, active: Bool)
}

let user = User("Alice", 30, True)           // positional
let user = User(name: "Alice", age: 30, active: True)  // labeled
```

Update records with spread syntax (creates a NEW record - Gleam is immutable):

```gleam
State(..state, field_name: new_value)
```

**New in v1.14.0**: Record update syntax works in constant definitions:

```gleam
pub const base_config = Config(host: "0.0.0.0", port: 8080)
pub const prod_config = Config(..base_config, port: 80)
```

## Result vs Option

- `Option(a)` — a value that may or may not be present. The field/slot is empty, not that an operation failed.
- `Result(a, ErrorType)` — an operation was attempted that can fail. Use a meaningful error type.
- `Result(a, Nil)` — appropriate when there is exactly ONE obvious failure mode with no extra info (like `dict.get` — key not found). Gleam stdlib uses this pattern and it's fine there.

**In our own code, do NOT write `Result(a, Nil)`.** Our APIs are in flux. If a function has one failure mode today, it might have two tomorrow. Using `Option` for single-failure-mode lookups means that if we later change to `Result(a, SomeError)`, every callsite breaks at compile time — the compiler catches the API change. With `Result(a, Nil)`, callers write `Error(_)` which silently swallows new error types.

**Rule of thumb**: If you're writing a new function and considering `Result(a, Nil)`, ask: is this an operation that failed, or is there just nothing here? If it's "nothing here" → `Option`. If it genuinely failed → define an error type, even a simple one.

**When matching Error, always bind the value**: Write `Error(Nil)` not `Error(_)`, so that if the error type changes, the compiler catches it.

## gleam check

Run `gleam check` frequently - it's fast and catches type errors early.

---

# Case Expression Patterns - Prefer Cleaner Alternatives

Gleam has several features that can replace verbose nested case expressions:

## 1. Nested Pattern Matching (instead of nested case)

```gleam
// BAD - nested case
case message {
  ConnectionDown(down) ->
    case down {
      process.ProcessDown(monitor:, ..) -> handle_down(monitor)
      process.PortDown(..) -> continue(state)
    }
}

// GOOD - nested pattern in single case
case message {
  ConnectionDown(process.ProcessDown(monitor:, ..)) -> handle_down(monitor)
  ConnectionDown(process.PortDown(..)) -> continue(state)
}
```

## 2. option.map for transforming values (NOT for side effects)

`option.map` is for transforming the value inside an Option, NOT for side effects.

```gleam
// GOOD - transforming a value
option.map(tag, json.string) |> option.unwrap(json.null())

// BAD - using map for side effects (the `let _ =` is a code smell!)
let _ = option.map(state.timer, process.cancel_timer)
```

**Rule of thumb**: If you're writing `let _ = option.map(...)` or `let _ = result.map(...)`, you're misusing map.

## 2b. Handle Option side effects with explicit case or a helper

For Option side effects, either use explicit case or create a helper:

```gleam
// Explicit case for Option side effects
case state.timer {
  Some(timer) -> process.cancel_timer(timer)
  None -> Nil
}
```

**For Results, do NOT use a generic helper** — handle errors explicitly so they get logged:

```gleam
// BAD - silently discards errors
let _ = result.map(channel.summon(registry, id, redis), channel.apply_patches(_, patches))

// GOOD - errors are logged with context
case channel.summon(registry, id, redis) {
  Ok(ch) -> channel.apply_patches(ch, patches)
  Error(err) -> logging.log(logging.Warning, "failed to summon channel: " <> string.inspect(err))
}
```

## 3. option.unwrap for default values

```gleam
// BAD
let value = case maybe_value {
  Some(v) -> v
  None -> default
}

// GOOD
let value = option.unwrap(maybe_value, default)
```

## 3b. option.map + option.unwrap for transform-or-default

When the `Some` branch transforms the value and `None` returns a default, combine `option.map` with `option.unwrap`:

```gleam
// BAD - unnecessary case expression
let parent_refs = case parent {
  Some(ref) -> [ref]
  None -> []
}

// GOOD - map the transform, unwrap with default
let parent_refs = parent |> option.map(list.wrap) |> option.unwrap([])
```

This applies whenever the pattern is `Some(x) -> f(x)` / `None -> default`. It's the combined form of sections 2 and 3.

## 4. option.flatten for Option(Option(a)) -> Option(a)

```gleam
// BAD
case nested_option {
  Some(inner) -> inner
  None -> None
}

// GOOD
option.flatten(nested_option)
```

## 5. option.lazy_or for "keep existing or create new"

```gleam
// BAD
let timer = case state.timer {
  Some(t) -> Some(t)
  None -> Some(create_new_timer())
}

// GOOD
let timer = option.lazy_or(state.timer, fn() { Some(create_new_timer()) })
```

## 6. ALWAYS use `use` with result.try/result.map — identity error propagation is BANNED

**Any time you write `Error(e) -> Error(e)` or `Error(err) -> Error(err)`, you MUST refactor to use `use` with `result.try` or `result.map`.** This is the most common anti-pattern in the codebase and it is never acceptable.

**`result.try`** — when the body returns a `Result`:
```gleam
// BAD — identity error propagation
case first_operation() {
  Ok(value) -> second_operation(value)  // returns Result
  Error(e) -> Error(e)
}

// GOOD
use value <- result.try(first_operation())
second_operation(value)
```

**`result.map`** — when the body returns a plain value (not Result):
```gleam
// BAD
case parse_expression(parser) {
  Ok(#(p, expr)) -> Ok(transform(expr))
  Error(e) -> Error(e)
}

// GOOD
use #(p, expr) <- result.map(parse_expression(parser))
transform(expr)
```

**When the Ok value is `Nil`, bind `Nil` — NEVER use `_`:**
```gleam
// BAD — discards the pattern, unclear what the Ok value is
use _ <- result.try(validate_something(input))

// GOOD — makes it explicit that Ok(Nil) is the success case
use Nil <- result.try(validate_something(input))
```

**Chained operations:**
```gleam
// BAD — nested case pyramid
case first_operation() {
  Ok(value1) ->
    case second_operation(value1) {
      Ok(value2) -> Ok(value2)
      Error(_) -> Error(Nil)
    }
  Error(_) -> Error(Nil)
}

// GOOD — flat chain with use
use value1 <- result.try(first_operation() |> result.map_error(fn(_) { Nil }))
second_operation(value1) |> result.map_error(fn(_) { Nil })
```

**BUT**: Only use `result.try` with `result.map_error` when the error transformation is **pure**. If you need side effects (like logging) on error, use explicit `case`:

```gleam
// BAD - side effect hidden inside result.map_error
use token_info <- result.try(
  summon_and_subscribe(state, token)
  |> result.map_error(fn(err) {
    logging.log(logging.Warning, "context: " <> string.inspect(err))  // side effect!
    errors.Unknown
  }),
)
complete_identify(state, token, token_info)

// GOOD - explicit case makes the logging visible
case summon_and_subscribe(state, token) {
  Ok(token_info) -> complete_identify(state, token, token_info)
  Error(err) -> {
    logging.log(logging.Warning, "context: " <> string.inspect(err))
    Error(errors.Unknown)
  }
}
```

The explicit `case` is better here because:

1. The side effect (logging) is clearly visible in the error branch
2. You're not abusing `result.map_error` for side effects
3. The code structure matches the intent

## 6b. result.map with result.unwrap for "transform or keep default"

```gleam
// BAD
case dict.get(map, key) {
  Ok(value) -> transform(value)
  Error(_) -> default
}

// GOOD
dict.get(map, key)
|> result.map(transform)
|> result.unwrap(default)
```

## 6c. Handle Result side effects explicitly (don't use result.map or result.map_error)

Don't use `result.map` or `result.map_error` for side effects — handle with a case expression:

```gleam
// BAD - using map for side effects
let _ =
  dict.get(state.channels, channel_id)
  |> result.map(channel.unsubscribe(_, events))

// GOOD for Error(Nil) - convert to Option if you want
case dict.get(state.channels, channel_id) {
  Ok(ch) -> channel.unsubscribe(ch, events)
  Error(_) -> Nil  // Error(Nil) has no info to log
}

// GOOD for real errors - explicit case with logging
case channel.summon(registry, id, redis) {
  Ok(ch) -> channel.apply_patches(ch, patches)
  Error(err) -> logging.log(logging.Warning, "context: " <> string.inspect(err))
}
```

## 7. Guards for boolean conditions in case

```gleam
// OK - separate case on boolean
case status {
  Ok(user) ->
    case is_admin {
      True -> show_admin_panel(user)
      False -> show_home(user)
    }
  Error(_) -> redirect_to_login()
}

// BETTER - guard clause
case status {
  Ok(user) if is_admin -> show_admin_panel(user)
  Ok(user) -> show_home(user)
  Error(_) -> redirect_to_login()
}
```

## 8. Multiple subjects for independent matches

```gleam
// When matching multiple values that don't depend on each other:
case username, password {
  "", "" -> Error("Both fields required")
  "", _ -> Error("Username required")
  _, "" -> Error("Password required")
  user, pass -> validate(user, pass)
}
```

## 9. bool.guard for early returns (on booleans, NOT values)

Use `bool.guard` with `use` when you already have a **boolean** and are checking `True`/`False`:

```gleam
import gleam/bool

// GOOD - you already have a boolean (is_admin)
use <- bool.guard(is_admin, show_admin_panel())
show_regular_panel()

// GOOD - set.contains returns Bool
use <- bool.guard(set.contains(allowed_ids, id), continue(state))
reject(state)
```

**BUT**: Use `case` when matching on actual values — don't convert to boolean just to use `bool.guard`:

```gleam
// GOOD - matching on a specific value (0)
case subscribers.size(connections) {
  0 -> stop_actor(state)
  _ -> continue(state)
}

// BAD - converting to boolean just to use bool.guard
use <- bool.guard(subscribers.size(connections) != 0, continue(state))
stop_actor(state)
```

```gleam
// GOOD - pattern match on the empty string literal
case token {
  "" -> decode.failure(Identify(""), "Token is empty")
  token -> decode.success(Identify(token))
}

// BAD - converting to boolean just to use bool.guard
use <- bool.guard(string.is_empty(token), decode.failure(Identify(""), "Token is empty"))
decode.success(Identify(token))
```

**Rule**: `bool.guard` is for when you _already have_ a boolean. Don't convert a value to a boolean just to use `bool.guard` — pattern match on the value directly instead.

## 10. The `use` keyword for callbacks — MANDATORY for callback-last functions

`use` flattens callback-heavy code. It works with any function that takes a callback as its last argument. Everything after the `use` becomes the callback body. **You MUST use `use` syntax whenever calling a function that takes a callback as its last argument.** Never pass an inline `fn(...) { ... }` when `use` is available.

```gleam
// BAD — inline callback
math_unary(args, state, fn(x) {
  Finite(float.absolute_value(x))
})

// GOOD — use syntax
use x <- math_unary(args, state)
Finite(float.absolute_value(x))
```

```gleam
// Common patterns:
use value <- result.try(some_result)      // Chain Results
use value <- option.map(some_option)      // Transform Options
use <- bool.guard(condition, early_return) // Early return
use field <- decode.field("name", decoder) // JSON decoding

// Custom callbacks work too:
fn with_connection(f: fn(Connection) -> a) -> a { ... }
use conn <- with_connection()
// use conn here
```

The `use` expression scopes to the current block—use braces to limit scope when needed.

**BUT: pass functions directly when they already have the right signature** — don't wrap in `use`:

```gleam
// BAD — wrapping a function that already has the right signature
use v <- conditional_jump(state, target)
value.is_truthy(v)

// GOOD — pass the function directly
conditional_jump(state, target, value.is_truthy)
```

**Rule**: If the callback body is just `some_function(x)` where `x` is the `use` binding, you should pass `some_function` directly instead of using `use`.

## 11. Record constructors as functions

Record constructors are functions in Gleam. Use them directly instead of wrapping in anonymous functions:

```gleam
// BAD - unnecessary anonymous function wrapper
|> result.map(fn(subject) { TokenHandle(subject:) })

// GOOD - use the constructor directly as a function
|> result.map(TokenHandle)
```

This works because `TokenHandle` as an expression is a function `fn(Subject) -> TokenHandle`. Any record constructor can be used this way wherever a function is expected:

```gleam
// These are equivalent:
list.map(items, fn(x) { Wrapper(x) })
list.map(items, Wrapper)

// Also works with multiple fields - partial application via capture:
list.map(names, User(name: _, role: "admin"))
```

## 12. Helper functions to reduce duplication

Extract repeated patterns into helpers:

```gleam
// BAD - repeated pattern
port: get_env("PORT", "8082") |> int.parse |> result.unwrap(8082),
redis_port: get_env("REDIS_PORT", "6379") |> int.parse |> result.unwrap(6379),

// GOOD - helper function
fn get_env_int(name: String, default: Int) -> Int {
  get_env(name, int.to_string(default))
  |> int.parse
  |> result.unwrap(default)
}

port: get_env_int("PORT", 8082),
redis_port: get_env_int("REDIS_PORT", 6379),
```

## 13. Function captures (partial application)

Use `_` as a placeholder to create partial application shorthand:

```gleam
// These are equivalent:
let add_one = fn(x) { add(1, x) }
let add_one = add(1, _)

// Works great with pipelines:
list.map(users, string.append("Hello, ", _))
```

Only a single `_` is allowed per capture expression.

## 14. Alternative patterns with `|`

Combine multiple patterns that produce the same result:

```gleam
case number {
  2 | 4 | 6 | 8 -> "Even"
  1 | 3 | 5 | 7 -> "Odd"
  _ -> "Other"
}

// Works with constructors too:
case event {
  ChannelDeleted(_) | ChannelRevoked(_) -> handle_removal()
  StateUpdated(state) -> handle_update(state)
}
```

All alternatives must bind the same variable names with the same types. Nested alternative patterns are not supported.

## 15. Pattern aliases with `as`

Bind a name to an entire matched subpattern:

```gleam
case lists {
  [[_, ..] as first, ..] -> first  // binds the whole inner list
  _ -> []
}

case message {
  ConnectionDown(process.ProcessDown(monitor:, ..) as down) -> {
    // can use both `monitor` and `down`
    handle_down(down)
  }
  _ -> continue(state)
}
```

## 16. String prefix matching with `<>`

The `<>` operator works for both concatenation AND pattern matching:

```gleam
case command {
  "subscribe:" <> channel_name -> subscribe(channel_name)
  "unsubscribe:" <> channel_name -> unsubscribe(channel_name)
  _ -> Error("Unknown command")
}
```

The left side must be a string literal (the prefix). The right side captures the remainder.

---

# Gleam Language Features

## Label Shorthand Syntax

When variable names match label names, omit the value:

```gleam
// Instead of:
User(name: name, age: age, role: role)

// Write:
User(name:, age:, role:)

// Works in function calls too:
calculate_total(quantity:, unit_price:, discount:)

// And in pattern matching:
case user {
  User(name:, age:, ..) -> io.println(name)
}
```

## `echo` for Debugging

`echo` prints ANY value (unlike `io.println` which requires String) and returns it, making it pipeline-friendly:

```gleam
echo 42              // prints 42, returns 42
echo [1, 2, 3]       // prints the list, returns it
echo user            // prints the record, returns it

// Works in pipelines:
tokens
|> list.filter(is_valid)
|> echo              // see intermediate value
|> list.map(activate)
```

Remove `echo` statements before committing — the compiler warns about them.

## `bool.lazy_guard` vs `bool.guard`

`bool.guard` eagerly evaluates the return value. Use `bool.lazy_guard` when the return value is expensive to compute:

```gleam
// bool.guard - return value is always evaluated
use <- bool.guard(is_cached, cached_value)

// bool.lazy_guard - return value only computed when needed
use <- bool.lazy_guard(is_cached, fn() { expensive_computation() })
```

Use `bool.lazy_guard` when the early-return value involves function calls, IO, or allocation. Use `bool.guard` for simple literal values.

## `let assert` with Custom Messages

`let assert` does a partial pattern match that panics on failure. Add `as` for a descriptive crash message:

```gleam
let assert [first, ..] = items as "Expected non-empty list"
let assert Ok(value) = result as "Database query should not fail here"
```

Generally use sparingly and prefer proper `Result` handling when failure is a realistic possibility. Most of the time `let assert` belongs in application code where you have strong guarantees about the data, not in library code. But there are legitimate uses — use your judgment. If the assertion fails, the process will crash with the provided message.

## `assert` for Test Assertions

Boolean assertions for test code:

```gleam
assert add(1, 2) == 3
assert list.length(items) > 0 as "Items should not be empty"
```

Primarily designed for test code. Rarely appropriate in application or library code, but edge cases exist.

## `todo` for Stubbing

`todo` marks unimplemented code — compiler warns, runtime crashes:

```gleam
pub fn new_feature() {
  todo as "Implement token refresh logic"
}
```

Useful for sketching out APIs before implementing them. The compiler still type-checks around `todo`.

## `panic` for Unreachable Code

`panic` crashes intentionally for "impossible" states:

```gleam
case impossible_state {
  _ -> panic as "This should never happen"
}
```

Prefer proper error handling in most cases. Use `panic` only when you're certain the code path is unreachable.

## `@deprecated` Attribute

Mark functions and type variants as deprecated:

```gleam
@deprecated("Use connect_v2 instead")
pub fn connect(url: String) -> Connection {
  // ...
}
```

The compiler emits warnings at call sites. Works on both functions and individual type variants.

## Opaque Types for Encapsulation

Hide constructors to enforce invariants:

```gleam
// In the module:
pub opaque type Token {
  Token(id: String, expires_at: Int)
}

pub fn new(id: String, ttl: Int) -> Token {
  Token(id:, expires_at: now() + ttl)
}

pub fn id(token: Token) -> String {
  token.id
}
```

Outside the module, code can't construct or destructure `Token` directly — only use the public API. This guarantees all tokens go through validation in `new`.

## Tail Call Optimization

Gleam optimizes tail calls. Use accumulator pattern for recursive functions:

```gleam
// BAD - not tail recursive (builds up stack)
pub fn sum(list: List(Int)) -> Int {
  case list {
    [] -> 0
    [first, ..rest] -> first + sum(rest)
  }
}

// GOOD - tail recursive with accumulator
pub fn sum(list: List(Int)) -> Int {
  sum_loop(list, 0)
}

fn sum_loop(list: List(Int), acc: Int) -> Int {
  case list {
    [] -> acc
    [first, ..rest] -> sum_loop(rest, acc + first)
  }
}
```

In practice, prefer stdlib functions (`list.fold`, `list.map`, etc.) over manual recursion.

## Guard Limitations

Guards in `case` expressions only support simple comparisons and boolean operators. They CANNOT contain:

- Function calls
- Case expressions
- Blocks

```gleam
// GOOD - simple comparison
case value {
  x if x > 0 && x < 100 -> "In range"
  _ -> "Out of range"
}

// BAD - function call in guard (won't compile)
case value {
  x if string.length(x) > 5 -> "Long"  // ERROR
  _ -> "Short"
}

// Instead, bind the result first:
let len = string.length(value)
case value {
  _ if len > 5 -> "Long"
  _ -> "Short"
}
```

## Pipeline and Data-First Convention

The pipe `|>` passes the left side as the **first argument** to the right side. Gleam's stdlib is designed data-first for this reason:

```gleam
// Natural pipeline flow because data is first arg:
tokens
|> list.filter(is_valid)
|> list.map(token.id)
|> string.join(", ")

// When you need a different argument position, use capture:
value
|> string.append("prefix: ", _)  // value goes to second arg
```

When designing your own functions, put the "data" argument first to enable pipelining.

## Constant Concatenation

The `<>` operator works in constant expressions:

```gleam
pub const greeting = "Hello"
pub const sentence = greeting <> " " <> "Joe" <> "!"
```

## Structural Equality

All Gleam types (including custom types) support structural equality with `==` out of the box. No need to implement comparison functions, derive traits, or use special libraries:

```gleam
type Point { Point(x: Int, y: Int) }

Point(1, 2) == Point(1, 2)  // True
Point(1, 2) == Point(3, 4)  // False

// Works as dict keys too, no special implementations needed:
let points = dict.from_list([#(Point(0, 0), "origin")])
```

---

## Useful stdlib Functions

These are easy to overlook but very handy:

```gleam
// string.to_option - converts empty string to None
string.to_option("")       // None
string.to_option("hello")  // Some("hello")

// string.inspect - converts ANY value to a string representation
string.inspect([1, 2, 3])  // "[1, 2, 3]"
string.inspect(Ok(42))     // "Ok(42)"

// function.identity - pass-through, useful as a no-op transformer
list.filter_map(items, function.identity)  // keeps only Ok values

// result.values - extract Ok values from a list of Results
result.values([Ok(1), Error("x"), Ok(3)])  // [1, 3]

// result.replace - replace the Ok value
result.replace(Ok(1), "done")  // Ok("done")
result.replace(Error(e), "done")  // Error(e)

// result.try_recover - try to recover from an error
result.try_recover(Error("not found"), fn(_) { Ok(default_value) })

// result.lazy_unwrap - unwrap with lazy default
result.lazy_unwrap(maybe_result, fn() { compute_default() })

// option.or / option.lazy_or - first Some wins
option.or(None, Some(1))  // Some(1)
option.or(Some(1), Some(2))  // Some(1)

// option.then - like result.try but for Option (map + flatten)
option.then(Some(1), fn(x) { Some(x + 1) })  // Some(2)
option.then(None, fn(x) { Some(x + 1) })     // None

// list.fold_until - fold with early termination
list.fold_until([1, 2, 3, 4], 0, fn(acc, i) {
  case i < 3 {
    True -> list.Continue(acc + i)
    False -> list.Stop(acc)
  }
})
// -> 3
```

---

## JSON Decoding Patterns

The `gleam/dynamic/decode` module (current API) has powerful combinators beyond simple field access:

```gleam
import gleam/dynamic/decode
import gleam/json

pub type Cat { Cat(name: String, lives: Int) }

pub fn cat_decoder() -> decode.Decoder(Cat) {
  use name <- decode.field("name", decode.string)
  use lives <- decode.field("lives", decode.int)
  decode.success(Cat(name:, lives:))
}

// Nested field access with at:
use name <- decode.at(["user", "profile", "name"], decode.string)

// Try multiple decoders (first success wins):
let flexible_int = decode.one_of(decode.int, [
  decode.string |> decode.then(fn(s) {
    case int.parse(s) {
      Ok(i) -> decode.success(i)
      Error(_) -> decode.failure(0, "numeric string")
    }
  }),
])

// Optional fields with defaults:
use color <- decode.optional_field("color", "blue", decode.string)
// If "color" key is missing or null, uses "blue"
```

---

## Gleam Tooling Notes

### No Separate Linter

Gleam has NO standalone linter. All lint-like functionality is built into the compiler. The compiler warns about:

- **Unused code**: variables, imports, private functions, modules, recursive function arguments
- **Dead code**: call graph analysis detects unreachable functions including mutual recursion loops
- **Unreachable code**: code after `panic`/`todo`, impossible patterns
- **Redundancy**: redundant `let assert` (total patterns), redundant function captures in pipelines, redundant comparisons (`x == x`)
- **Deprecation**: warns when referencing `@deprecated` items
- **Safety**: `todo`/`echo` reminders, integer overflow on JS target, detached doc comments
- **Shadowing**: warns when local definitions shadow unqualified imports

### Toolchain Commands

- **`gleam check`** — extremely fast type checking, use constantly
- **`gleam format`** — opinionated formatter, zero config, 2-space indent, enforced across ecosystem
- **`gleam fix`** — migration tool that rewrites deprecated syntax (NOT a linter autofix)
- **`gleam docs build`** — generates HTML docs from `///` and `////` comments
- **`gleam add <package>`** — adds hex dependencies
- **`gleam remove <package>`** — removes hex dependencies

### Language Server Code Actions

The language server provides automatic fixes in your editor:

- Remove unused imports
- Remove redundant tuple wrappers in case
- Auto-import modules
- Add omitted labels (fills with `todo`)
- Convert `let assert` to explicit `case` with `panic`
- Rewrite function calls to pipe syntax
- Convert to label shorthand syntax
- Generate function definitions for undefined functions
- Generate decoders for custom types

---

## FFI (Foreign Function Interface)

FFI should be used sparingly. Check for existing Gleam packages first. **FFI breaks type safety** — the compiler trusts your annotations without verification.

### When to Use FFI

- Accessing platform-specific APIs not available in Gleam packages
- Performance-critical code that benefits from native implementation
- Wrapping existing Erlang/Elixir/JavaScript libraries

### When NOT to Use FFI

- Standard operations covered by gleam_stdlib
- Things that "feel" easier in another language — often there's an idiomatic Gleam way
- As a first resort — search hexdocs.pm first

### Erlang FFI Syntax

```gleam
// In your Gleam file:
@external(erlang, "myapp_ffi", "do_thing")
pub fn do_thing(arg: String) -> Result(Int, Nil)

// In src/myapp_ffi.erl:
-module(myapp_ffi).
-export([do_thing/1]).

do_thing(Arg) ->
    case internal_logic(Arg) of
        {ok, Value} -> {ok, Value};
        error -> {error, nil}
    end.
```

### JavaScript FFI Syntax

```gleam
// In your Gleam file:
@external(javascript, "./myapp_ffi.mjs", "doThing")
pub fn do_thing(arg: String) -> Result(Int, Nil)

// In src/myapp_ffi.mjs:
import { Ok, Error } from "./gleam.mjs";

export function doThing(arg) {
  const result = internalLogic(arg);
  return result !== null ? new Ok(result) : new Error(undefined);
}
```

### FFI Best Practices

- **NEVER** name Erlang FFI modules the same as Gleam modules (causes infinite loops)
- Use `.mjs` extension for JavaScript FFI files
- Elixir modules need `Elixir.` prefix: `@external(erlang, "Elixir.Module", "func")`
- Write extensive tests for all FFI code
- Wrap FFI in validation layers when possible — check inputs and outputs
- Keep FFI modules small and focused
- Document the expected types thoroughly

---

## Concurrency Patterns (Erlang Target Only)

### Available Modules

**gleam_otp v1.2.0** provides:

- `gleam/otp/actor` — The primary abstraction for stateful processes
- `gleam/otp/static_supervisor` — Supervisors for fixed sets of children
- `gleam/otp/factory_supervisor` — Supervisors that dynamically create children
- `gleam/otp/supervision` — Child specification helpers
- `gleam/otp/system` — OTP system message handling
- `gleam/otp/port` — Port handling

**gleam_erlang v1.3.0** provides:

- `gleam/erlang/process` — Low-level process primitives (Subject, Selector, spawn, etc.)

**Note**: There is NO `gleam/otp/task` module. For parallel work, spawn processes directly with `process.spawn` or use actors.

### Actor Pattern

```gleam
import gleam/otp/actor
import gleam/erlang/process.{type Subject}

pub type Message {
  Push(value: String)
  Pop(reply_with: Subject(Result(String, Nil)))
  Shutdown
}

fn handle_message(state: List(String), msg: Message) -> actor.Next(List(String), Message) {
  case msg {
    Shutdown -> actor.stop()
    Push(value) -> actor.continue([value, ..state])
    Pop(client) -> {
      case state {
        [] -> {
          process.send(client, Error(Nil))
          actor.continue([])
        }
        [first, ..rest] -> {
          process.send(client, Ok(first))
          actor.continue(rest)
        }
      }
    }
  }
}

pub fn start() -> actor.StartResult(Subject(Message)) {
  actor.new([])
  |> actor.on_message(handle_message)
  |> actor.start
}
```

The `start` function returns `Result(Started(Subject(Message)), StartError)`. Access the subject via `started.data`.

### Parallel Work Without Tasks

Since there is no `task` module, for parallel work use `process.spawn` with subjects:

```gleam
import gleam/erlang/process

pub fn parallel_work() {
  // Create subjects to receive results
  let result1_subject = process.new_subject()
  let result2_subject = process.new_subject()

  // Spawn workers that send results back
  process.spawn(fn() {
    let result = expensive_query_1()
    process.send(result1_subject, result)
  })
  process.spawn(fn() {
    let result = expensive_query_2()
    process.send(result2_subject, result)
  })

  // Wait for results (with timeout)
  let assert Ok(result1) = process.receive(result1_subject, within: 5000)
  let assert Ok(result2) = process.receive(result2_subject, within: 5000)

  // Use results
  combine(result1, result2)
}
```

For more complex scenarios, use a Selector to wait on multiple subjects:

```gleam
let selector =
  process.new_selector()
  |> process.select(result1_subject)
  |> process.select(result2_subject)

// Receive from whichever responds first
let first_result = process.selector_receive(selector, within: 5000)
```

### Supervision

```gleam
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import gleam/erlang/process

pub fn start_app() -> actor.StartResult(supervisor.Supervisor) {
  supervisor.new(supervisor.OneForOne)
  |> supervisor.restart_tolerance(intensity: 5, period: 60)
  |> supervisor.add(supervision.worker(database.start))
  |> supervisor.add(supervision.worker(cache.start))
  |> supervisor.start
}

pub fn main() {
  let assert Ok(_) = start_app()
  process.sleep_forever()  // Keep main process alive!
}
```

**Restart strategies:**

- `OneForOne` — Only restart crashed child (default)
- `OneForAll` — Restart all children when one crashes
- `RestForOne` — Restart crashed child and all started after it

### Critical OTP Mistakes to Avoid

1. **Main function exiting immediately** — all spawned actors die. Use `process.sleep_forever()`.

2. **Self-call deadlock** — an actor calling itself synchronously:

```gleam
// WRONG: Actor can't receive while handling message
fn handle_message(state, msg) {
  let data = actor.call(self_subject, GetData, 1000)  // DEADLOCK!
  // ...
}
```

3. **Subject ownership violation** — only the process that created a Subject can receive on it:

```gleam
// WRONG: Subject created here, received in spawned process
let subject = process.new_subject()
process.spawn(fn() {
  process.receive(subject, 5000)  // FAILS! Wrong owner
})
```

---

## Common Mistakes to Avoid

1. **Using `===` instead of `==`** — JavaScript habit, Gleam uses `==`
2. **Using `if` instead of `case`** — Gleam has no if expression
3. **Discarding return values** — immutability means `set_header(req, ...)` returns a new request
4. **Using parentheses for grouping** instead of braces (`{ a + b } * c`)
5. **Creating process names dynamically** — causes atom table overflow and VM crash
6. **Using `actor.call()` without supervisors** — crashes propagate to caller
7. **Main function exiting immediately** — all spawned actors die
8. **Overusing catch-all `_` patterns** — hides unhandled cases when types change
9. **Nesting Results** instead of using `use` expressions
10. **Using `Option` for fallible operations** — use `Result(a, Nil)` instead
