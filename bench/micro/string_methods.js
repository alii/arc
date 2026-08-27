// from bench/bench.js section string_methods
const run = function (n) {
  const base = "Hello, World! The quick brown fox jumps over the lazy dog.";
  let s = 0;
  for (let i = 0; i < n; i++) {
    s += base.indexOf("fox") + base.slice(10, 20).length + base.toUpperCase().length;
  }
  return s;
};
run(1000);
const t0 = Date.now();
run(20000);
console.log("BENCH string_methods " + (Date.now() - t0));
