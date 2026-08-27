// from bench/bench.js section class_methods
const run = function (n) {
  class Point {
    constructor(x, y) { this.x = x; this.y = y; }
    dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }
  }
  let s = 0;
  for (let i = 0; i < n; i++) {
    const p = new Point(i, i + 1);
    s += p.dist();
  }
  return s;
};
run(1000);
const t0 = Date.now();
run(50000);
console.log("BENCH class_methods " + (Date.now() - t0));
