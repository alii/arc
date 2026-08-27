// from bench/bench.js section fn_calls
const run = function (n) {
  function add(a, b) { return a + b; }
  let s = 0;
  for (let i = 0; i < n; i++) s = add(s, 1);
  return s;
};
run(1000);
const t0 = Date.now();
run(200000);
console.log("BENCH fn_calls " + (Date.now() - t0));
