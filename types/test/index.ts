/// <reference path="../src/index.d.ts" />

Arc.peek(Promise.resolve(''));

const inbox = Arc.subject<{ n: number; reply: Arc.Subject<number> }>();
const reply = Arc.subject<number>();

Arc.spawn(() => {
	const msg = inbox.receive();
	msg.reply.send(msg.n * 2);
});

inbox.send({ n: 21, reply });
const doubled: number = reply.receive();
const maybe: number | undefined = reply.receive(1000);
Arc.log(doubled, maybe);

const errors = Arc.subject<Error>();
const selector: Arc.Selector<number> = Arc.select()
	.on(reply)
	.on(errors, (err) => {
		throw err;
	});
const result: number = selector.receive();
const result2: number | undefined = selector.receive(1000);
Arc.log(result, result2);
