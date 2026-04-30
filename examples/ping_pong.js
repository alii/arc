const ping = Arc.subject();

Arc.spawn(() => {
	const pong = Arc.subject();
	ping.send(pong);
	while (true) {
		const msg = pong.receive();
		console.log("pong:", msg.n);
		msg.reply.send({ n: msg.n + 1, reply: pong });
		if (msg.n >= 5) return;
	}
});

const pong = ping.receive();
pong.send({ n: 1, reply: ping });

while (true) {
	const msg = ping.receive();
	console.log("ping:", msg.n);
	if (msg.n >= 5) break;
	pong.send({ n: msg.n + 1, reply: ping });
}
