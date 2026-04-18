const reply = Arc.subject();

Arc.spawn(() => {
	const inbox = Arc.subject();
	reply.send(inbox);
	const msg = inbox.receive();
	msg.reply.send(msg.n * 2);
});

const worker = reply.receive();
worker.send({ n: 21, reply });
Arc.log(reply.receive()); // 42
