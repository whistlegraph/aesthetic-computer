// ucla-7-balls, 24.07.30.20.20
// Forces

/* 📝 Notes 
  - Today we will be working with forces.
  1. [x] Bouncing inside the screen.
   - [x] Add interactive inertia.
  2. [x] Bouncing balls.
    - [C] Make sure the drone and main ball bounce off one
         another.
    - [x] Make a drone ball and use the same boundary
         code to keep it in the screen.
*/

let bx = 64;
let by = 64;
let velx = 0;
let vely = 0;
let circleRadius = 16;

const drone = { x: 0, y: 0, velx: 0, vely: 0, radius: 8 };

let drag = 0.9;
let draglight = 0.99;

function boot({ screen }) {
  bx = screen.width / 2;
  by = screen.height / 2;
  drone.x = screen.width / 2;
  drone.y = screen.height / 4;
  drone.vely = 1;
}

function paint({ wipe, ink, screen, pen, num, sound }) {
  wipe("gray"); // Clearing the screen.

  if (pen && pen.drawing) {
    // console.log(pen.delta);
    velx += pen.delta.x;
    velx *= drag;
    vely += pen.delta.y;
    vely *= drag;
  }

  if (!pen || !pen.drawing) {
    // Simulation
    bx += velx; // add velocity to position
    by += vely;
    velx *= draglight;
    vely *= draglight;
    drone.x += drone.velx;
    drone.y += drone.vely;
  }

  // Collision
  // what is the x dist from bx to drone.x?
  // what is the y dist from by to drone.y?

  const xdist = Math.abs(bx - drone.x);
  const ydist = Math.abs(by - drone.y);
  let hitColor = "yellow";

  let topHitDist = 0;
  const droneBot = drone.y + drone.radius;
  const circleTop = by - circleRadius;
  topHitDist = Math.abs(droneBot - circleTop);
  ink("pink").write("tophitdist: " + topHitDist, 6, 58);

  // Ready for collision...
  if (
    xdist - drone.radius < circleRadius &&
    ydist - drone.radius < circleRadius
  ) {

    if (topHitDist < 1) {
      drone.vely *= -1;
      drone.y = by - circleRadius - drone.radius;
    }

    if (hitColor !== "red") {
      hitColor = "red";
      // drone.x = num.randInt(screen.width);
      // drone.y = num.randInt(screen.height);
      sound.synth({ type: "sine", tone: 200, duration: 1 });
    }

  }

  ink(hitColor).write("xdist: " + xdist, 6, 38);
  ink(hitColor).write("ydist: " + ydist, 6, 48);

  ink("cyan").line(bx, by, drone.x, drone.y);

  function bounce(axisVelocity) {
    const amount = Math.abs(axisVelocity);
    const tone = num.map(amount, 0, 68, 500, 1000);
    const volume = num.map(amount, 0, 68, 1, 0.25); // map vol 0.25->1
    const pan = num.map(bx, 0, screen.width, 1, -1);
    const duration = num.map(amount, 0, 68, 0.3, 0.05);
    // console.log("🎶 Now playing:", tone);
    sound.synth({ type: "sine", tone, volume, pan, duration, attack: 0.05 });
  }

  const bottomDist = screen.height - by;
  if (bottomDist < circleRadius) {
    by = screen.height - circleRadius;
    vely *= -1; // reverse the velocity.
    bounce(vely);
  }

  {
    // drone bottom
    const bottomDist = screen.height - drone.y;
    if (bottomDist < drone.radius) {
      drone.y = screen.height - drone.radius;
      drone.vely *= -1; // reverse the velocity.
      // bounce(drone.vely);
    }
  }

  const topDist = by;
  if (topDist < circleRadius) {
    by = circleRadius;
    vely *= -1;
    bounce(vely);
  }

  {
    // drone top
    const topDist = drone.y;
    if (topDist < drone.radius) {
      drone.y = drone.radius;
      drone.vely *= -1;
      // bounce(drone.vely);
    }
  }

  // Side to side bouncing.
  const rightDist = screen.width - bx;
  if (rightDist < circleRadius) {
    bx = screen.width - circleRadius;
    velx *= -1; // reverse the velocity.
    bounce(velx);
  }

  {
    // Drone right
    const rightDist = screen.width - drone.x;
    if (rightDist < drone.radius) {
      drone.x = screen.width - drone.radius;
      drone.velx *= -1; // reverse the velocity.
      // bounce(drone.velx);
    }
  }

  const leftDist = bx;
  if (leftDist < circleRadius) {
    bx = circleRadius;
    velx *= -1;
    bounce(velx);
  }

  {
    // Drone left
    const leftDist = drone.x;
    if (leftDist < drone.radius) {
      drone.x = drone.radius;
      drone.velx *= -1;
      // bounce(drone.velx);
    }
  }

  ink("white").write("velx: " + velx, 6, 18); // text, x, y
  ink("white").write("vely: " + vely, 6, 18 + 10); // text, x, y

  // Painting
  ink(hitColor).circle(bx, by, circleRadius); // x, y, radius
  ink("red").circle(bx, by, 2);
  ink("blue").line(bx, by, bx - velx, by);
  ink("cyan").line(bx, by, bx, by - vely);
  ink("orange").circle(drone.x, drone.y, drone.radius, "fill"); // 🟣 drone
  ink("purple").circle(drone.x, drone.y, drone.radius);
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function act({ event: e }) {
//  // Respond to user input here.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
