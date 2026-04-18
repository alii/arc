// Stateful counter actor — the classic Erlang pattern.
const ready = Arc.subject();

Arc.spawn(() => {
	const commands = Arc.subject();
	ready.send(commands);
	let count = 0;
	while (true) {
		const msg = commands.receive();
		if (msg.type === 'inc') count += msg.n;
		else if (msg.type === 'dec') count -= msg.n;
		else if (msg.type === 'get') msg.reply.send(count);
		else if (msg.type === 'stop') return msg.reply.send(count);
	}
});

const counter = ready.receive();
const reply = Arc.subject();

counter.send({ type: 'inc', n: 10 });
counter.send({ type: 'inc', n: 5 });
counter.send({ type: 'dec', n: 3 });
counter.send({ type: 'get', reply });
Arc.log('count:', reply.receive());

counter.send({ type: 'stop', reply });
Arc.log('stopped at:', reply.receive());
