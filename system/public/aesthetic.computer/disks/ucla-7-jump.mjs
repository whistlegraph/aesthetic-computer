// ucla-7, 24.07.30.20.20
// Forces

/* 📝 Notes 
  - Today we will be working with forces.
  1. [x] A jumping character.
    - [x] Paint the floor and define the floory.
  2. [] Bouncing inside the screen.
  3. [] Bouncing balls.
*/

let floory = 300;
let warioy = floory - 50;
let wariovelocity = 0;
let wariosize = 15;
let drag = 0.85;
let gravity = 1.04;
let jumpVelocity = 35;

let hurdleX = -1;
let hurdleHeight;
let failedHurdle = false;
let hurdleSpeed = 2;

function paint({ ink, wipe, screen, sound, num }) {
  // Simulation
  if (hurdleX < 0) {
    hurdleX = screen.width + 1;
    hurdleHeight = num.randIntRange(10, 50);
    hurdleSpeed = num.randIntRange(2, 4);
    failedHurdle = false;
  }
  hurdleX -= hurdleSpeed;

  warioy += wariovelocity;
  wariovelocity *= drag;
  warioy *= gravity;

  // Hit the hurdle?
  let hitting = false;
  let wariox = screen.width / 2;
  let halfwario = wariosize / 2;
  let wariobottom = warioy - halfwario;
  let hurdleTop = floory - hurdleHeight;

  if (hurdleX > wariox - halfwario && hurdleX < wariox + halfwario && wariobottom > hurdleTop) {
    hitting = true;
    if (failedHurdle === false) {
      sound.synth({ tone: "sawtooth", tone: 200, duration: 0.2 });
      failedHurdle = true;
    }
  } else {
    hitting = false;
  }

  // Hit the floor!
  if (warioy > floory) {
     warioy = floory;
     jumping = false;
     doubleJumping = false;
  }

  if (warioy < 0) {
    warioy = 0;
    wariovelocity *= -1.25; // flip the velocity!
  }

  // Painting
  wipe("purple");

  ink("yellow").line(hurdleX, floory, hurdleX, floory - hurdleHeight) // x1, y1, x2, y2

  ink("white").write("vel: " + wariovelocity, 6, 18);
  ink("white").write("wy: " + warioy, 6, 28);
  ink("white").write("jumping: " + jumping, 6, 38);

  ink("green").box(0, floory, screen.width, 2) // x, y, w, h
  ink(hitting ? "red" : "yellow", 127).box(screen.width / 2, warioy, wariosize, "center");
  ink("red").point(screen.width / 2 - 1, warioy - 1);
}

let jumping = false;
let doubleJumping = false;

function act({ event: e, sound }) {
 // Respond to user input here.
 if (e.is("touch")) {
  if (jumping === false) {
    wariovelocity = -jumpVelocity;
    jumping = true;
    const pew = sound.synth({ type: "sine", tone: "3C", duration: 0.25, volume: 0.5 });
  } else if (jumping === true && doubleJumping === false) {
    doubleJumping = true;
    wariovelocity += -jumpVelocity/2;
    sound.synth({ type: "sine", tone: "3F", duration: 0.25, volume: 0.5 });
  }
 }
}

// 📚 Library

// function boot() {
// Runs once at the start.
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
