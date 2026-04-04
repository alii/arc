// Each process gets its own copy of the heap.
let i = 1;
const main = Arc.self();

Arc.spawn(() => {
  i = 10;
  Arc.log("child set i to", i);
  Arc.send(main);
});

Arc.receive();
Arc.log("main still sees i as", i);
