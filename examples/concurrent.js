// Two processes running concurrently on the BEAM scheduler.
// Each one loops forever, logging and sleeping — you can see
// the BEAM interleaving them in real time.

function run(name, delay) {
	Arc.log(Arc.self(), 'is starting');
	let i = 1;
	while (true) {
		Arc.log(`${name} tick`, i);
		i = i + 1;
		Arc.sleep(delay);
	}
}

Arc.spawn(() => run('[Process A]', 50));
Arc.spawn(() => run('    [Process B]', 200));

Arc.log('  [Main] sleeping... watch A and B interleave!');
Arc.sleep(800);
Arc.log('  [Main] Done!');
