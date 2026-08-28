const run = function (n) {
  const lines = [
    "2024-01-15 ERROR [db] connection refused to 10.0.0.12:5432",
    "2024-01-15 INFO [http] GET /api/users/42 200 12ms",
    "2024-01-16 WARN [cache] miss ratio 0.35 for key user:42:profile",
    "no match here at all",
  ];
  const dateRe = /^(\d{4})-(\d{2})-(\d{2})/;
  const levelRe = /\b(ERROR|WARN|INFO)\b/;
  const numG = /\d+/g;
  const word = /[a-z]+/gi;
  let s = 0;
  for (let i = 0; i < n; i++) {
    const line = lines[i & 3];
    if (dateRe.test(line)) s += 1;
    const m = levelRe.exec(line);
    if (m) s += m[1].length + m.index;
    const d = line.match(dateRe);
    if (d) s += d[1].length + d[2].length;
    const nums = line.match(numG);
    if (nums) s += nums.length;
    s += line.replace(numG, "#").length;
    s += line.replace(/refused|miss/, function (w) { return w.toUpperCase(); }).length;
    numG.lastIndex = 0;
    let k = 0;
    while (numG.exec(line) !== null && k < 20) k++;
    s += k;
    s += line.split(/\s+/).length;
    if (/users\/(\d+)/.test(line)) s += 2;
    s += line.search(word);
  }
  return s;
};
run(200);
const t0 = Date.now();
run(3000);
console.log("BENCH regexp " + (Date.now() - t0));
