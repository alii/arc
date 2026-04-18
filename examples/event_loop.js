// Async receive on a subject, with timers in the same process.
const inbox = Arc.subject();

async function waiter(s) {
	Arc.log('waiter: listening...');
	Arc.log('waiter: got', await s.receiveAsync());
}

async function ticker() {
	for (let i = 1; i <= 3; i++) {
		await new Promise((r) => Arc.setTimeout(r, 100));
		Arc.log('tick', i);
	}
}

waiter(inbox);
ticker();

Arc.spawn(() => {
	Arc.sleep(250);
	inbox.send('hello');
});
