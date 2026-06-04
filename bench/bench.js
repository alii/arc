// Microbenchmark harness for the arc VM.
// Run: gleam run -- bench/bench.js
// Prints "BENCH <name> <ms>" per section so runs can be diffed.

function bench(name, n, f) {
  // warmup
  f(Math.min(n, 1000));
  const t0 = Date.now();
  f(n);
  const ms = Date.now() - t0;
  console.log("BENCH " + name + " " + ms);
  return ms;
}

let total = 0;

total += bench("arith_loop", 300000, function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) s += i * 3 % 7;
  return s;
});

total += bench("fn_calls", 200000, function (n) {
  function add(a, b) { return a + b; }
  let s = 0;
  for (let i = 0; i < n; i++) s = add(s, 1);
  return s;
});

total += bench("closures", 100000, function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) {
    const f = (function (x) { return function (y) { return x + y; }; })(i);
    s += f(1);
  }
  return s;
});

total += bench("obj_props", 100000, function (n) {
  const o = { a: 1, b: 2, c: 3 };
  let s = 0;
  for (let i = 0; i < n; i++) {
    o.a = i;
    s += o.a + o.b + o.c;
  }
  return s;
});

total += bench("obj_create", 50000, function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) {
    const o = { x: i, y: i + 1 };
    s += o.x + o.y;
  }
  return s;
});

total += bench("array_push_index", 30000, function (n) {
  const a = [];
  for (let i = 0; i < n; i++) a.push(i);
  let s = 0;
  for (let i = 0; i < n; i++) s += a[i];
  return s;
});

total += bench("array_methods", 200, function (n) {
  let a = [];
  for (let i = 0; i < 2000; i++) a.push(i);
  let s = 0;
  for (let i = 0; i < n; i++) {
    // NOTE: params deliberately have unique names — the current tree has a
    // scope-resolution bug where sibling function exprs with same-named params
    // inside a for-loop body raise "Duplicate binding in lexical declaration".
    s += a.map(function (m) { return m + 1; })
      .filter(function (g) { return g % 2 === 0; })
      .reduce(function (p, q) { return p + q; }, 0);
  }
  return s;
});

total += bench("string_concat", 20000, function (n) {
  let s = "";
  for (let i = 0; i < n; i++) s += "x";
  return s.length;
});

total += bench("string_methods", 20000, function (n) {
  const base = "Hello, World! The quick brown fox jumps over the lazy dog.";
  let s = 0;
  for (let i = 0; i < n; i++) {
    s += base.indexOf("fox") + base.slice(10, 20).length + base.toUpperCase().length;
  }
  return s;
});

total += bench("try_catch", 50000, function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) {
    try {
      if (i % 100 === 0) throw new Error("e");
      s += 1;
    } catch (e) {
      s += 2;
    }
  }
  return s;
});

total += bench("generators", 2000, function (n) {
  function* gen(k) {
    for (let i = 0; i < k; i++) yield i;
  }
  let s = 0;
  for (let i = 0; i < n; i++) {
    for (const v of gen(20)) s += v;
  }
  return s;
});

total += bench("class_methods", 50000, function (n) {
  class Point {
    constructor(x, y) { this.x = x; this.y = y; }
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }
  }
  let s = 0;
  for (let i = 0; i < n; i++) {
    const p = new Point(i, i + 1);
    s += p.dist();
  }
  return s;
});

console.log("BENCH total " + total);
