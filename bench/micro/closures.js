// from bench/bench.js section closures
const run = function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) {
    const f = (function (x) { return function (y) { return x + y; }; })(i);
    s += f(1);
  }
  return s;
};
run(1000);
const t0 = Date.now();
run(100000);
console.log("BENCH closures " + (Date.now() - t0));
