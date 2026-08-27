// from bench/bench.js section string_concat
const run = function (n) {
  let s = "";
  for (let i = 0; i < n; i++) s += "x";
  return s.length;
};
run(1000);
const t0 = Date.now();
run(20000);
console.log("BENCH string_concat " + (Date.now() - t0));
