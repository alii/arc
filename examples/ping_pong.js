const pong = Arc.spawn(() => {
  while (true) {
    const msg = Arc.receive();
    Arc.log("pong:", msg.n);
    Arc.send(msg.from, { n: msg.n + 1, from: Arc.self() });
    if (msg.n >= 5) return;
  }
});

Arc.send(pong, { n: 1, from: Arc.self() });

while (true) {
  const msg = Arc.receive();
  Arc.log("ping:", msg.n);
  if (msg.n >= 5) break;
  Arc.send(pong, { n: msg.n + 1, from: Arc.self() });
}
