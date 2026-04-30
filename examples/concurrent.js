// Two processes interleaving on the BEAM scheduler.
function run(name, delay) {
  for (let i = 1; ; i++) {
    console.log(`${name} tick ${i}`);
    Arc.sleep(delay);
  }
}

Arc.spawn(() => run("A", 50));
Arc.spawn(() => run("B", 200));

Arc.sleep(800);
console.log("done");
