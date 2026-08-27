// from bench/bench.js section try_catch
const run = function (n) {
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
};
run(1000);
const t0 = Date.now();
run(50000);
console.log("BENCH try_catch " + (Date.now() - t0));
