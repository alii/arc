const run = function (n) {
  const obj = {
    id: 12345,
    name: "Widget Deluxe",
    price: 19.99,
    tags: ["home", "garden", "tools", "sale"],
    active: true,
    owner: null,
    dims: { w: 10.5, h: 20, d: 3.25, unit: "cm" },
    reviews: [
      { user: "alice", stars: 5, text: "Great product, would buy again!" },
      { user: "bob", stars: 3, text: "It's fine. Shipping was slow.\nMeh." },
      { user: "carol", stars: 4, text: "Solid \"value\" for the money" },
    ],
    history: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
    meta: { created: "2024-01-15T10:00:00Z", updated: "2024-02-01T12:30:00Z", rev: 7 },
  };
  const items = [];
  for (let j = 0; j < 4; j++) items.push(obj);
  const doc = { count: 4, items: items };
  const text = JSON.stringify(doc);
  let s = 0;
  for (let i = 0; i < n; i++) {
    const parsed = JSON.parse(text);
    s += parsed.items[i & 3].reviews[1].stars + parsed.count;
    const out = JSON.stringify(parsed);
    s += out.length;
  }
  return s + text.length;
};
run(20);
const t0 = Date.now();
run(400);
console.log("BENCH json " + (Date.now() - t0));
