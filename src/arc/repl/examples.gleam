/// REPL-runnable demos showcasing Arc's actor model on the BEAM.
/// Each example is a self-contained JS snippet using subjects for
/// message passing between processes.
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/string

pub type Example {
  Example(title: String, blurb: String, source: String)
}

pub fn all() -> List(Example) {
  [
    spawn_hello(),
    ping_pong(),
    counter_actor(),
    parallel_map(),
    pubsub(),
    request_reply(),
    worker_pool(),
    ring(),
  ]
}

pub fn get(n: Int) -> Option(Example) {
  all() |> list.drop(n - 1) |> list.first |> option.from_result
}

/// Print the `/examples` menu.
pub fn print_list() -> Nil {
  io.println("")
  io.println("  Arc examples — run one with `/examples <n>`")
  io.println("")
  all()
  |> list.index_fold(Nil, fn(_, ex, i) {
    let num = string.pad_start(int.to_string(i + 1), 2, " ")
    io.println("  " <> num <> ". " <> ex.title <> " — " <> ex.blurb)
  })
  io.println("")
}

/// Print the source of an example before it runs.
pub fn print_source(ex: Example) -> Nil {
  io.println("")
  io.println("── " <> ex.title <> " " <> string.repeat("─", 50))
  io.println(ex.blurb)
  io.println("")
  io.println(ex.source)
  io.println(string.repeat("─", 60))
  io.println("")
}

// -- 1. Spawn & Message ------------------------------------------------------

fn spawn_hello() -> Example {
  Example(
    title: "Spawn & Message",
    blurb: "The hello-world of actors: spawn a process, send it a message.",
    source: "const inbox = Arc.subject();

Arc.spawn(() => {
  inbox.send('hello from ' + Arc.self());
});

Arc.log(inbox.receive());",
  )
}

// -- 2. Ping Pong ------------------------------------------------------------

fn ping_pong() -> Example {
  Example(
    title: "Ping Pong",
    blurb: "Two processes volleying messages back and forth.",
    source: "const ping = Arc.subject();

Arc.spawn(() => {
  const pong = Arc.subject();
  ping.send(pong);
  while (true) {
    const m = pong.receive();
    if (m === 'stop') return;
    Arc.log('    <- pong', m.n);
    m.reply.send({ n: m.n + 1, reply: pong });
  }
});

const pong = ping.receive();
let n = 0;
pong.send({ n, reply: ping });
while (n < 5) {
  const m = ping.receive(1000);
  n = m.n;
  Arc.log('ping ->', n);
  pong.send({ n, reply: ping });
}
pong.send('stop');",
  )
}

// -- 3. Counter Actor --------------------------------------------------------

fn counter_actor() -> Example {
  Example(
    title: "Counter Actor",
    blurb: "A stateful server process — the GenServer pattern in JS.",
    source: "const ready = Arc.subject();

Arc.spawn(() => {
  const commands = Arc.subject();
  ready.send(commands);
  let n = 0;
  while (true) {
    const msg = commands.receive();
    if (msg.op === 'inc') n += msg.by;
    if (msg.op === 'get') msg.reply.send(n);
    if (msg.op === 'stop') return;
  }
});

const counter = ready.receive();
const reply = Arc.subject();
counter.send({ op: 'inc', by: 10 });
counter.send({ op: 'inc', by: 5 });
counter.send({ op: 'inc', by: 27 });
counter.send({ op: 'get', reply });
Arc.log('counter value:', reply.receive(1000));
counter.send({ op: 'stop' });",
  )
}

// -- 4. Parallel Map ---------------------------------------------------------

fn parallel_map() -> Example {
  Example(
    title: "Parallel Map",
    blurb: "Fan-out work to N processes, fan-in the results. True parallelism.",
    source: "const results = Arc.subject();
const inputs = [22, 23, 24, 25, 26, 27];

function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }

// Fan out: one process per input, each runs on its own BEAM scheduler.
for (const x of inputs) {
  Arc.spawn(() => results.send({ x, y: fib(x) }));
}

// Fan in: results arrive in completion order, not input order.
for (let i = 0; i < inputs.length; i++) {
  const r = results.receive(5000);
  Arc.log('fib(' + r.x + ') =', r.y);
}
Arc.log('done —', inputs.length, 'results computed in parallel');",
  )
}

// -- 5. PubSub ---------------------------------------------------------------

fn pubsub() -> Example {
  Example(
    title: "PubSub",
    blurb: "A broker fanning messages out to many subscribers.",
    source: "const ready = Arc.subject();

Arc.spawn(() => {
  const commands = Arc.subject();
  ready.send(commands);
  const subs = [];
  while (true) {
    const m = commands.receive();
    if (m.sub) { subs.push(m.sub); Arc.log('[broker] +sub, total:', subs.length); }
    if (m.pub) for (const s of subs) s.send(m.pub);
    if (m.stop) return;
  }
});

const broker = ready.receive();

// Spawn three subscribers.
for (let i = 1; i <= 3; i++) {
  const sub = Arc.subject();
  Arc.spawn(() => {
    while (true) {
      const m = sub.receive();
      if (m === 'stop') return;
      Arc.log('  [sub', i + ']', 'received:', m);
    }
  });
  broker.send({ sub });
}

Arc.sleep(20);
broker.send({ pub: 'breaking news' });
broker.send({ pub: 'more news' });
Arc.sleep(50);
broker.send({ stop: true });",
  )
}

// -- 6. Request/Reply --------------------------------------------------------

fn request_reply() -> Example {
  Example(
    title: "Request/Reply",
    blurb: "Synchronous calls over async messages — the `call` pattern.",
    source: "const ready = Arc.subject();

Arc.spawn(() => {
  const inbox = Arc.subject();
  ready.send(inbox);
  const data = { alice: 30, bob: 25, carol: 35 };
  while (true) {
    const m = inbox.receive();
    if (m === 'stop') return;
    m.reply.send(data[m.key]);
  }
});

const server = ready.receive();
const reply = Arc.subject();

function call(key) {
  server.send({ key, reply });
  return reply.receive(1000);
}

Arc.log('alice is', call('alice'));
Arc.log('bob is', call('bob'));
Arc.log('carol is', call('carol'));
server.send('stop');",
  )
}

// -- 7. Worker Pool ----------------------------------------------------------

fn worker_pool() -> Example {
  Example(
    title: "Worker Pool",
    blurb: "A pool of workers pulling jobs from a shared queue.",
    source: "const ready = Arc.subject();

Arc.spawn(() => {
  const inbox = Arc.subject();
  ready.send(inbox);
  const jobs = [];
  const waiting = [];
  while (true) {
    const m = inbox.receive();
    if (m.push) {
      if (waiting.length) waiting.shift().send(m.push);
      else jobs.push(m.push);
    }
    if (m.pull) {
      if (jobs.length) m.pull.send(jobs.shift());
      else waiting.push(m.pull);
    }
    if (m.stop) { for (const w of waiting) w.send(null); return; }
  }
});

const queue = ready.receive();

// Spawn 3 workers that pull jobs forever.
for (let w = 1; w <= 3; w++) {
  Arc.spawn(() => {
    const inbox = Arc.subject();
    while (true) {
      queue.send({ pull: inbox });
      const job = inbox.receive();
      if (job === null) return;
      Arc.sleep(30); // simulate work
      Arc.log('[worker', w + ']', 'finished job', job);
    }
  });
}

// Push 8 jobs — watch them get distributed across workers.
for (let j = 1; j <= 8; j++) queue.send({ push: j });
Arc.sleep(400);
queue.send({ stop: true });",
  )
}

// -- 8. Ring -----------------------------------------------------------------

fn ring() -> Example {
  Example(
    title: "Ring Benchmark",
    blurb: "Pass a token around a ring of 500 processes, 10 times.",
    source: "const N = 500, LAPS = 10;
const setup = Arc.subject();
const main = Arc.subject();

for (let i = 0; i < N; i++) {
  Arc.spawn(() => {
    const inbox = Arc.subject();
    setup.send(inbox);
    const next = inbox.receive();
    while (true) {
      const m = inbox.receive();
      if (m === 'stop') { next.send('stop'); return; }
      next.send(m + 1);
    }
  });
}

const inboxes = [];
for (let i = 0; i < N; i++) inboxes.push(setup.receive());
for (let i = 0; i < N; i++) inboxes[i].send(i === N - 1 ? main : inboxes[i + 1]);

Arc.log('ring of', N, 'processes built, sending token...');
inboxes[0].send(0);
for (let lap = 1; lap <= LAPS; lap++) {
  const hops = main.receive(5000);
  Arc.log('lap', lap, '->', hops, 'hops');
  if (lap < LAPS) inboxes[0].send(0);
}
inboxes[0].send('stop');
Arc.log('total:', N * LAPS, 'message passes');",
  )
}
