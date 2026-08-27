// forEach / some / find / findIndex / indexOf / includes / sort over a 2000-array
const run = function (n) {
  let a = [];
  for (let i = 0; i < 2000; i++) a.push((i * 7919) % 2003);
  let s = 0;
  for (let i = 0; i < n; i++) {
    a.forEach(function (v) { s += v; });
    if (a.some(function (v) { return v === -1; })) s++;
    s += a.find(function (v) { return v === 1000; });
    s += a.findIndex(function (v) { return v === 1500; });
    s += a.indexOf(1999) + (a.includes(2002) ? 1 : 0);
    s += a.slice().sort(function (x, y) { return x - y; })[0];
  }
  return s;
};
run(5);
const t0 = Date.now();
run(40);
console.log("BENCH array_iter " + (Date.now() - t0));
