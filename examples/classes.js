class Shape {
  constructor(name) { this.name = name; }
  describe() { return `${this.name} with area ${this.area()}`; }
}

class Circle extends Shape {
  constructor(r) { super("circle"); this.r = r; }
  area() { return Math.round(Math.PI * this.r * this.r); }
}

console.log(new Circle(3).describe());
