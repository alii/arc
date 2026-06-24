// async/await + Promise.all, with microtask ordering.
async function double(x) {
  return x * 2;
}

async function main() {
  const xs = await Promise.all([1, 2, 3].map(double));
  console.log("doubled", xs);
}

main();
console.log("sync runs first");
