function makeCounter() {
  let n = 0;
  return () => ++n;
}

const a = makeCounter();
const b = makeCounter();
console.log(a(), a(), a(), b());
