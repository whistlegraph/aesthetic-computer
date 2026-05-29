// pick — mouse picking + tossing.
// Move the mouse to bend a soft particle trail. Press on an orb to grab it,
// drag to fling, release to launch it with the drag velocity. Other orbs
// keep bouncing physics-style around the canvas.

const N = 14;
const TRAIL = 36;

let orbs = [];
let trail = []; // {x, y, age}
let grabbed = null;
let grabOffset = { x: 0, y: 0 };

function setup() {
  createCanvas(windowWidth, windowHeight);
  colorMode(HSB, 360, 100, 100, 1);
  for (let i = 0; i < N; i++) {
    orbs.push({
      x: random(width),
      y: random(height),
      vx: random(-2, 2),
      vy: random(-2, 2),
      r: random(18, 38),
      h: random(360),
    });
  }
}

function draw() {
  background(220, 25, 6, 0.22);

  // trail update — push current mouse pos, fade out old ones
  trail.push({ x: mouseX, y: mouseY, age: 0 });
  if (trail.length > TRAIL) trail.shift();
  noStroke();
  for (let i = 0; i < trail.length; i++) {
    const t = trail[i];
    const a = i / trail.length;
    fill((frameCount * 2 + i * 6) % 360, 60, 100, a * 0.5);
    circle(t.x, t.y, 6 + (1 - a) * 18);
    t.age++;
  }

  // orb physics
  for (const o of orbs) {
    if (o === grabbed) continue;
    o.x += o.vx;
    o.y += o.vy;
    if (o.x < o.r) { o.x = o.r; o.vx *= -0.9; }
    if (o.x > width - o.r) { o.x = width - o.r; o.vx *= -0.9; }
    if (o.y < o.r) { o.y = o.r; o.vy *= -0.9; }
    if (o.y > height - o.r) { o.y = height - o.r; o.vy *= -0.9; }
    // mild damping so a tossed orb eventually settles
    o.vx *= 0.995;
    o.vy *= 0.995;
  }

  // grabbed orb follows mouse exactly
  if (grabbed) {
    grabbed.x = mouseX - grabOffset.x;
    grabbed.y = mouseY - grabOffset.y;
    grabbed.vx = mouseX - pmouseX;
    grabbed.vy = mouseY - pmouseY;
  }

  // draw orbs
  for (const o of orbs) {
    const isGrabbed = o === grabbed;
    noStroke();
    // outer halo
    fill(o.h, 60, 100, isGrabbed ? 0.4 : 0.15);
    circle(o.x, o.y, o.r * 2.6);
    // body
    fill(o.h, 70, 95, 0.9);
    circle(o.x, o.y, o.r * 2);
    // highlight
    fill(0, 0, 100, 0.7);
    circle(o.x - o.r * 0.3, o.y - o.r * 0.3, o.r * 0.5);
    // ring around grabbed
    if (isGrabbed) {
      noFill();
      stroke(0, 0, 100, 0.9);
      strokeWeight(2);
      circle(o.x, o.y, o.r * 2 + 8);
    }
  }

  // HUD
  noStroke();
  fill(0, 0, 100, 0.8);
  textFont("monospace", 12);
  text(grabbed ? "tossing — release to fling" : "press an orb to pick it up", 12, 20);
}

function mousePressed() {
  // pick the topmost orb under the cursor (iterate in reverse draw order)
  for (let i = orbs.length - 1; i >= 0; i--) {
    const o = orbs[i];
    const dx = mouseX - o.x;
    const dy = mouseY - o.y;
    if (dx * dx + dy * dy < o.r * o.r) {
      grabbed = o;
      grabOffset.x = dx;
      grabOffset.y = dy;
      // bring to top of draw order
      orbs.splice(i, 1);
      orbs.push(o);
      return;
    }
  }
}

function mouseReleased() {
  grabbed = null;
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
}
