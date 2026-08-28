const run = function (n) {
  let s = 0;
  let str = "";
  for (let i = 0; i < n; i++) {
    const f = i * 1.37 + 0.001;
    str = "n=" + i + " f=" + f;
    s += str.length;
    s += (i * 7).toString().length + f.toString().length;
    s += f.toFixed(2).length + (255 + i).toString(16).length;
    s += parseInt("12345" + (i % 10)) % 100;
    s += parseFloat("3.25e2") + parseInt("ff", 16);
    s += Number("42.5" + (i % 10)) > 42 ? 1 : 0;
    s += +("17") + Number("  12  ");
    s += String(i).length + (i + 0.5).toFixed(0).length;
    s += Math.floor(f) + Math.round(f * 10) % 3 + Math.abs(-i) % 5;
    s += Math.max(i, 3) + Math.min(i, 9) + (Math.sqrt(i) | 0);
  }
  return s;
};
run(500);
const t0 = Date.now();
run(20000);
console.log("BENCH number_fmt " + (Date.now() - t0));
