// Async functions in one process, with BEAM mailbox delivery.
const self = Arc.self();

async function waiter() {
  Arc.log("waiter: listening...");
  Arc.log("waiter: got", await Arc.receiveAsync());
}

async function ticker() {
  for (let i = 1; i <= 3; i++) {
    await new Promise((r) => Arc.setTimeout(r, 100));
    Arc.log("ticker:", i);
  }
}

waiter();
ticker();

Arc.spawn(() => {
  Arc.sleep(250);
  Arc.send(self, "hello");
});
