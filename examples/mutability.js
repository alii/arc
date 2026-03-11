// demonstrates that we copy the vm

let i = 1;
const main = Arc.self();

Arc.spawn(() => {
	i = 10;
	Arc.log(Arc.self(), 'I set `i` to', i);
	Arc.send(main); // with no arguemnts, sends undefined
});

Arc.receive();

Arc.log(main, 'but I see that `i` is', i);
