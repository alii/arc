const pid = Arc.spawn(() => {
  Arc.log(Arc.receive());
});

Arc.send(pid, "hello from " + Arc.self());
