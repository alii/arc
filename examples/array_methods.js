// filter / map / reduce composed into a pipeline.
const sumOfEvenSquares = [1, 2, 3, 4, 5, 6]
  .filter((n) => n % 2 === 0)
  .map((n) => n * n)
  .reduce((a, b) => a + b, 0);

console.log(sumOfEvenSquares);
