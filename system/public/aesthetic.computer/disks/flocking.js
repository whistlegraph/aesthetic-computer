// flocking — Reynolds-style boids, after Daniel Shiffman's classic port.
// Each boid steers by three forces: separation, alignment, cohesion.

const N = 90;
const MAX_SPEED = 2.4;
const MAX_FORCE = 0.06;
const PERCEPTION = 38;
const SEP_RADIUS = 18;

let boids = [];

function setup() {
  createCanvas(windowWidth, windowHeight);
  for (let i = 0; i < N; i++) {
    const a = random(TWO_PI);
    boids.push({
      x: random(width),
      y: random(height),
      vx: cos(a) * MAX_SPEED * 0.6,
      vy: sin(a) * MAX_SPEED * 0.6,
      ax: 0, ay: 0,
      h: random(360),
    });
  }
  colorMode(HSB, 360, 100, 100, 1);
}

function limit(vx, vy, max) {
  const m = sqrt(vx * vx + vy * vy);
  if (m > max) return [vx / m * max, vy / m * max];
  return [vx, vy];
}

function draw() {
  background(220, 25, 8, 0.18);

  for (const b of boids) {
    let sepX = 0, sepY = 0, sepN = 0;
    let aliX = 0, aliY = 0, aliN = 0;
    let cohX = 0, cohY = 0, cohN = 0;

    for (const o of boids) {
      if (o === b) continue;
      const dx = b.x - o.x;
      const dy = b.y - o.y;
      const d = sqrt(dx * dx + dy * dy);
      if (d > 0 && d < PERCEPTION) {
        aliX += o.vx; aliY += o.vy; aliN++;
        cohX += o.x; cohY += o.y; cohN++;
        if (d < SEP_RADIUS) {
          sepX += dx / d; sepY += dy / d; sepN++;
        }
      }
    }

    let fx = 0, fy = 0;
    if (sepN > 0) {
      [sepX, sepY] = limit(sepX / sepN, sepY / sepN, MAX_SPEED);
      fx += (sepX - b.vx) * 1.6;
      fy += (sepY - b.vy) * 1.6;
    }
    if (aliN > 0) {
      [aliX, aliY] = limit(aliX / aliN, aliY / aliN, MAX_SPEED);
      fx += (aliX - b.vx) * 1.0;
      fy += (aliY - b.vy) * 1.0;
    }
    if (cohN > 0) {
      const cx = cohX / cohN - b.x;
      const cy = cohY / cohN - b.y;
      const [tx, ty] = limit(cx, cy, MAX_SPEED);
      fx += (tx - b.vx) * 0.9;
      fy += (ty - b.vy) * 0.9;
    }
    [b.ax, b.ay] = limit(fx, fy, MAX_FORCE);

    b.vx += b.ax;
    b.vy += b.ay;
    [b.vx, b.vy] = limit(b.vx, b.vy, MAX_SPEED);
    b.x += b.vx;
    b.y += b.vy;

    if (b.x < 0) b.x += width;
    if (b.x > width) b.x -= width;
    if (b.y < 0) b.y += height;
    if (b.y > height) b.y -= height;

    noStroke();
    fill(b.h, 70, 90, 0.9);
    const ang = atan2(b.vy, b.vx);
    push();
    translate(b.x, b.y);
    rotate(ang);
    triangle(6, 0, -5, 3, -5, -3);
    pop();
  }
}

function mousePressed() {
  // scatter
  for (const b of boids) {
    const a = random(TWO_PI);
    b.vx = cos(a) * MAX_SPEED;
    b.vy = sin(a) * MAX_SPEED;
  }
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
}
