// Stateful counter actor — the classic Erlang pattern.
const me = Arc.self();

const counter = Arc.spawn(() => {
  let count = 0;
  while (true) {
    const msg = Arc.receive();
    if (msg.type === "inc") count += msg.n;
    else if (msg.type === "dec") count -= msg.n;
    else if (msg.type === "get") Arc.send(msg.from, count);
    else if (msg.type === "stop") return Arc.send(msg.from, count);
  }
});

Arc.send(counter, { type: "inc", n: 10 });
Arc.send(counter, { type: "inc", n: 5 });
Arc.send(counter, { type: "dec", n: 3 });
Arc.send(counter, { type: "get", from: me });
Arc.log("count:", Arc.receive());

Arc.send(counter, { type: "stop", from: me });
Arc.log("stopped at:", Arc.receive());
