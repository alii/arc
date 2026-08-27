// from bench/bench.js section arith_loop
const run = function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) s += i * 3 % 7;
  return s;
};
run(1000);
const t0 = Date.now();
run(300000);
console.log("BENCH arith_loop " + (Date.now() - t0));
