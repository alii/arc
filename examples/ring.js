// Pass a token around a ring of 100 processes, 10 laps.
const N = 100, M = 10;
const setup = Arc.subject();
const main = Arc.subject();

for (let i = 0; i < N; i++) {
	Arc.spawn(() => {
		const inbox = Arc.subject();
		setup.send(inbox);
		const next = inbox.receive();
		while (true) {
			const msg = inbox.receive();
			if (msg === "stop") return;
			next.send({ lap: msg.lap, hops: msg.hops + 1 });
		}
	});
}

const inboxes = [];
for (let i = 0; i < N; i++) inboxes.push(setup.receive());

// Wire ring: each process forwards to the next, last forwards to main
for (let i = 0; i < N; i++) {
	inboxes[i].send(i === N - 1 ? main : inboxes[i + 1]);
}

console.log("ring of", N, "processes created");
inboxes[0].send({ lap: 0, hops: 0 });

for (let lap = 0; lap < M; ) {
	const msg = main.receive();
	lap = msg.lap + 1;
	console.log("lap", lap, "-", msg.hops, "hops");
	if (lap < M) inboxes[0].send({ lap, hops: 0 });
}
