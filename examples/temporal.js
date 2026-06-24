// Temporal: modern date/time, built in.
const date = Temporal.PlainDate.from("2026-06-25");
const later = date.add({ days: 40 });

console.log(later.toString());
console.log("day of week:", later.dayOfWeek);
console.log("days until:", date.until(later).days);
