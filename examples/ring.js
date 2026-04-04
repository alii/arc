// Pass a token around a ring of 100 processes, 10 laps.
const N = 100, M = 10;
const first = Arc.self();
let prev = first;

for (let i = 0; i < N; i++) {
  const next = prev;
  prev = Arc.spawn(() => {
    while (true) {
      const msg = Arc.receive();
      if (msg === "stop") return;
      Arc.send(next, { lap: msg.lap, hops: msg.hops + 1 });
    }
  });
}

Arc.log("ring of", N, "processes created");
Arc.send(prev, { lap: 0, hops: 0 });

for (let lap = 0; lap < M; ) {
  const msg = Arc.receive();
  lap = msg.lap + 1;
  Arc.log("lap", lap, "-", msg.hops, "hops");
  if (lap < M) Arc.send(prev, { lap, hops: 0 });
}
