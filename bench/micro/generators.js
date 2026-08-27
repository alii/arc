// from bench/bench.js section generators
const run = function (n) {
  function* gen(k) {
    for (let i = 0; i < k; i++) yield i;
  }
  let s = 0;
  for (let i = 0; i < n; i++) {
    for (const v of gen(20)) s += v;
  }
  return s;
};
run(1000);
const t0 = Date.now();
run(2000);
console.log("BENCH generators " + (Date.now() - t0));
