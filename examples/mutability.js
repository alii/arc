// Each process gets its own copy of the heap.
let i = 1;
const done = Arc.subject();

Arc.spawn(() => {
	i = 10;
	Arc.log("child set i to", i);
	done.send(null);
});

done.receive();
Arc.log("main still sees i as", i);
