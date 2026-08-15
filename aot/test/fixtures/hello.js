function greet(name) { return "hello " + name; }
Promise.resolve("later").then(function (v) { console.log(v); });
console.log(greet("aot"));
