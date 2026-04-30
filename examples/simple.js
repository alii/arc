const inbox = Arc.subject();

Arc.spawn(() => inbox.send('hello from ' + Arc.self()));

console.log(inbox.receive());
