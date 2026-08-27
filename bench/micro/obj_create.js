// from bench/bench.js section obj_create
const run = function (n) {
  let s = 0;
  for (let i = 0; i < n; i++) {
    const o = { x: i, y: i + 1 };
    s += o.x + o.y;
  }
  return s;
};
run(1000);
const t0 = Date.now();
run(50000);
console.log("BENCH obj_create " + (Date.now() - t0));
