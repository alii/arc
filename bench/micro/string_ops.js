const run = function (n) {
  const base = "  Hello, World! The quick brown fox jumps over the lazy dog.  ";
  const csv = "alpha,beta,gamma,delta,epsilon";
  let s = 0;
  let acc = "";
  for (let i = 0; i < n; i++) {
    const t = base.trim();
    s += t.charCodeAt(i % 40) + t.charAt(3).length;
    s += t.indexOf("fox") + t.lastIndexOf("o");
    s += t.slice(4, 12).length + t.substring(20, 10).length;
    s += csv.split(",").length;
    s += t.replace("quick", "slow").length;
    s += t.toLowerCase().length + t.toUpperCase().length;
    if (t.startsWith("Hello")) s += 1;
    if (t.endsWith("dog.")) s += 1;
    if (t.includes("jumps")) s += 1;
    const name = "item" + i;
    acc = `${name}:${i % 7}/${t.length}`;
    s += acc.length + name.concat("-", "x").length;
    s += t.padStart(70, "*").length + csv.repeat(2).length;
  }
  return s;
};
run(500);
const t0 = Date.now();
run(12000);
console.log("BENCH string_ops " + (Date.now() - t0));
