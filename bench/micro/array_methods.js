// from bench/bench.js section array_methods
const run = function (n) {
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
};
run(200);
const t0 = Date.now();
run(200);
console.log("BENCH array_methods " + (Date.now() - t0));
