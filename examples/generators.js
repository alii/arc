function* naturals() {
  let n = 1;
  while (true) yield n++;
}

function take(it, k) {
  const out = [];
  for (const x of it) {
    if (out.length === k) break;
    out.push(x);
  }
  return out;
}

console.log(take(naturals(), 8));
