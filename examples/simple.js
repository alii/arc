const inbox = Arc.subject();

Arc.spawn(() => inbox.send('hello from ' + Arc.self()));

Arc.log(inbox.receive());
