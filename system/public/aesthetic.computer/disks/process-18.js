// process-18 — homage to Casey Reas' Process 18.
// Translation from Reas' instructions: "A surface filled with N medium to
// small sized circles. Each circle has a different size and direction, but
// moves at the same slow rate. Display the instantaneous intersections
// between the circles and accumulate the aggregate trail."

const N = 100;
const SPEED = 0.6;
let elements = [];
let bg = 12;

function setup() {
  createCanvas(windowWidth, windowHeight);
  background(bg);
  for (let i = 0; i < N; i++) {
    const a = random(TWO_PI);
    elements.push({
      x: random(width),
      y: random(height),
      r: random(8, 36),
      vx: cos(a) * SPEED,
      vy: sin(a) * SPEED,
    });
  }
}

function draw() {
  // gentle decay leaves trails behind
  noStroke();
  fill(bg, 18);
  rect(0, 0, width, height);

  // move
  for (const e of elements) {
    e.x += e.vx;
    e.y += e.vy;
    if (e.x < -e.r) e.x += width + 2 * e.r;
    if (e.x > width + e.r) e.x -= width + 2 * e.r;
    if (e.y < -e.r) e.y += height + 2 * e.r;
    if (e.y > height + e.r) e.y -= height + 2 * e.r;
  }

  // draw intersections only — pairs whose distance is less than sum of radii
  stroke(255, 200);
  strokeWeight(0.6);
  noFill();
  for (let i = 0; i < N; i++) {
    const a = elements[i];
    for (let j = i + 1; j < N; j++) {
      const b = elements[j];
      const dx = a.x - b.x;
      const dy = a.y - b.y;
      const d = sqrt(dx * dx + dy * dy);
      if (d < a.r + b.r && d > abs(a.r - b.r)) {
        // approximate intersection by drawing both arcs lightly
        circle(a.x, a.y, a.r * 2);
        circle(b.x, b.y, b.r * 2);
      }
    }
  }
}

function mousePressed() {
  background(bg);
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
  background(bg);
}
