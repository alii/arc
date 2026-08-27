// from bench/bench.js section array_push_index
const run = function (n) {
  const a = [];
  for (let i = 0; i < n; i++) a.push(i);
  let s = 0;
  for (let i = 0; i < n; i++) s += a[i];
  return s;
};
run(1000);
const t0 = Date.now();
run(30000);
console.log("BENCH array_push_index " + (Date.now() - t0));
