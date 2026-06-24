/// REPL-runnable demos showcasing the Arc JavaScript engine.
/// Each example is a self-contained JS snippet — run one with `/examples <n>`.
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/string

pub type Example {
  Example(title: String, blurb: String, source: String)
}

pub fn all() -> List(Example) {
  [closures(), promises(), generators(), classes(), array_methods()]
}

pub fn get(n: Int) -> Option(Example) {
  all() |> list.drop(n - 1) |> list.first |> option.from_result
}

/// Print the `/examples` menu.
pub fn print_list() -> Nil {
  io.println("")
  io.println("  Arc examples — run one with `/examples <n>`")
  io.println("")
  all()
  |> list.index_fold(Nil, fn(_, ex, i) {
    let num = string.pad_start(int.to_string(i + 1), 2, " ")
    io.println("  " <> num <> ". " <> ex.title <> " — " <> ex.blurb)
  })
  io.println("")
}

/// Print the source of an example before it runs.
pub fn print_source(ex: Example) -> Nil {
  io.println("")
  io.println("── " <> ex.title <> " " <> string.repeat("─", 50))
  io.println(ex.blurb)
  io.println("")
  io.println(ex.source)
  io.println(string.repeat("─", 60))
  io.println("")
}

// -- 1. Closures -------------------------------------------------------------

fn closures() -> Example {
  Example(
    title: "Closures",
    blurb: "A counter factory — each closure keeps its own private state.",
    source: "function makeCounter() {
  let n = 0;
  return () => ++n;
}

const a = makeCounter();
const b = makeCounter();
console.log(a(), a(), a(), b());",
  )
}

// -- 2. Promises -------------------------------------------------------------

fn promises() -> Example {
  Example(
    title: "Promises & async/await",
    blurb: "Microtask ordering with async functions and Promise.all.",
    source: "async function double(x) {
  return x * 2;
}

async function main() {
  const xs = await Promise.all([1, 2, 3].map(double));
  console.log('doubled', xs);
}

main();
console.log('sync runs first');",
  )
}

// -- 3. Generators -----------------------------------------------------------

fn generators() -> Example {
  Example(
    title: "Generators",
    blurb: "A lazy infinite sequence, consumed with a spread + take.",
    source: "function* naturals() {
  let n = 1;
  while (true) yield n++;
}

function take(it, k) {
  const out = [];
  for (const x of it) {
    if (out.length === k) break;
    out.push(x);
  }
  return out;
}

console.log(take(naturals(), 5));",
  )
}

// -- 4. Classes --------------------------------------------------------------

fn classes() -> Example {
  Example(
    title: "Classes & inheritance",
    blurb: "Class fields, methods, and `super` in a small hierarchy.",
    source: "class Shape {
  constructor(name) { this.name = name; }
  describe() { return `${this.name} with area ${this.area()}`; }
}

class Circle extends Shape {
  constructor(r) { super('circle'); this.r = r; }
  area() { return Math.round(Math.PI * this.r * this.r); }
}

console.log(new Circle(3).describe());",
  )
}

// -- 5. Array methods --------------------------------------------------------

fn array_methods() -> Example {
  Example(
    title: "Array higher-order methods",
    blurb: "filter / map / reduce composed into a tiny pipeline.",
    source: "const sumOfEvenSquares = [1, 2, 3, 4, 5, 6]
  .filter((n) => n % 2 === 0)
  .map((n) => n * n)
  .reduce((a, b) => a + b, 0);

console.log(sumOfEvenSquares);",
  )
}
