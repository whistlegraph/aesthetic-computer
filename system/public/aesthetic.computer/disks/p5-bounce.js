// p5-bounce — first .js piece on AC (Option D iframe host).
// Plain p5 global mode. AC wraps this in an iframe via dom.html.

let balls = [];
const N = 80;

function setup() {
  createCanvas(windowWidth, windowHeight);
  colorMode(HSB, 360, 100, 100, 1);
  noStroke();
  for (let i = 0; i < N; i++) {
    balls.push({
      x: random(width),
      y: random(height),
      vx: random(-3, 3),
      vy: random(-3, 3),
      r: random(8, 28),
      h: random(360),
    });
  }
}

function draw() {
  background(220, 30, 8, 0.25);
  for (const b of balls) {
    b.x += b.vx;
    b.y += b.vy;
    if (b.x < b.r || b.x > width - b.r) b.vx *= -1;
    if (b.y < b.r || b.y > height - b.r) b.vy *= -1;
    fill(b.h, 80, 95, 0.85);
    circle(b.x, b.y, b.r * 2);
  }
  fill(0, 0, 100);
  textFont("monospace", 14);
  text("p5.js running inside aesthetic.computer · click to reseed", 16, 28);
}

function mousePressed() {
  for (const b of balls) {
    b.vx = random(-6, 6);
    b.vy = random(-6, 6);
    b.h = random(360);
  }
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
}
