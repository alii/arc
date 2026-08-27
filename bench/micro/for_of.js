// for-of over a plain array and over a generator
const run = function (n) {
  const a = [];
  for (let i = 0; i < 100; i++) a.push(i);
  function* gen(k) {
    for (let i = 0; i < k; i++) yield i;
  }
  let s = 0;
  for (let k = 0; k < n; k++) {
    for (const v of a) s += v;
    for (const v of gen(10)) s += v;
  }
  return s;
};
run(100);
const t0 = Date.now();
run(3000);
console.log("BENCH for_of " + (Date.now() - t0));
