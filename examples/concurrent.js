// Two processes running concurrently on the BEAM scheduler.
// Each one loops forever, logging and sleeping — you can see
// the BEAM interleaving them in real time.

function run(name, delay) {
	Arc.log(Arc.self());
	let i = 0;
	while (true) {
		Arc.log(`${name} tick`, i);
		i = i + 1;
		Arc.sleep(delay);
	}
}

Arc.spawn(() => run('[Process A]', 200));
Arc.spawn(() => run('[Process B]', 1000));

Arc.log('Main process sleeping... watch A and B interleave!');
Arc.sleep(5000);
Arc.log('Done!');
