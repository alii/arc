// from bench/bench.js section obj_props
const run = function (n) {
  const o = { a: 1, b: 2, c: 3 };
  let s = 0;
  for (let i = 0; i < n; i++) {
    o.a = i;
    s += o.a + o.b + o.c;
  }
  return s;
};
run(1000);
const t0 = Date.now();
run(100000);
console.log("BENCH obj_props " + (Date.now() - t0));
