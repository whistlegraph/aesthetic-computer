// ucla-7, 24.07.30.20.20
// Forces

/* 📝 Notes 
  - Today we will be working with forces.
    15.  🎈 *BONUS* 🎶 Musical (Temporal) Systems
    - [x] Draw a rotating dial using turtle graphics.
      - [x] down, up, left, right, crawl, face, goto 
    - [x] Wire that dial up to a 'trigger'.
    - [] Replicate or make a blueprint for many dials.
      - []                   ^ Class

          What is a class?
            - contains state
            - contains functions or procedure
            - can have as many as you want
            - ^ it's a blue print for making

            - just like a program has variables and functions
            - a "class" has           properties and methods

            // Creating a class.
            class Dial {
              constructor() {
              }

              paint() {
              }
            }

            // Instantiating or making an 'instance of' the class.
            let myDial = new Dial();
            myDial.paint(); <- runs the paint method in the class
      - [] Make three of them.
      - [] One will be background color.
      - [] Two others will be sounds.
      - [] Directionality.

    📃 Up Next...
      Bouncing balls!
    16. - [] Using parameters in Aesthetic Computer (make sure development works)
*/

// Dial is a class with three properties, angle, wipeBlue, and dialSpeed
class Dial {
  angle = 0;
  speed;
  x;
  y;
  trigger;

  constructor(initialSpeed = 6, initialX, initialY) {
    this.speed = initialSpeed;
    this.x = initialX;
    this.y = initialY;
  }

  simulation(api) {
    this.angle = this.angle + this.speed;
    if (this.angle >= 360) {
      this.angle %= 360;
      this.trigger?.(api); // Run any trigger function if it's been set!
    }
  }

  painting({ goto, face, down, ink, crawl, right, up }) {
    goto(this.x, this.y); // Puts in the center by default.
    face(-90); // Will always face to the right --> 0 degrees
    down();
    ink("red");
    crawl(20);
    const center = goto(this.x, this.y); // <- get position
    right(this.angle);
    ink("yellow");
    crawl(20);
    up();
    ink("gray").circle(center.x, center.y, 20);
  }
}

let firstDial = new Dial(1, 64, 64);
let secondDial = new Dial(15, 64, 128);
let thirdDial = new Dial(29, 64, 184);

let wipeBlue = false;

firstDial.trigger = function ({ sound }) {
  wipeBlue = !wipeBlue;
  sound.synth({ tone: "C", duration: 0.5 });
};

secondDial.trigger = function ({ sound }) {
  sound.synth({ tone: "E", duration: 0.2 });
};

thirdDial.trigger = function ({ sound }) {
  sound.synth({ tone: "G", duration: 0.01 });
};

function paint({ api, wipe, ink, down, up, right, left, crawl, face, goto }) {
  wipe(wipeBlue ? 64 : "black");

  firstDial.simulation(api);
  firstDial.painting(api);
  secondDial.simulation(api);
  secondDial.painting(api);
  thirdDial.simulation(api);
  thirdDial.painting(api);
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
