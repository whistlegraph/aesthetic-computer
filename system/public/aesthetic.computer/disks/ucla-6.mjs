// ucla-6, 24.07.23.07.33
// Clocks, component design and relative coordinate systems.

/* 📝 Notes 
  - Today we will be designing clockfaces and working with time. 
    1. [] 🐢 Introduction to turtle graphics and relative coordinate systems.
    2.   - [] Making lines and basic operation.
    3.   - [] Making a circle.
    4.   - [] Patterns.
    4. [⏰] Keeping time and clock-making. 
    5. - [] Using LLMs to learn about the JavaScript `Date` Object. 
    6. - [] Writing the time.
    7. - [] Progress bars and review using `num.map`.
    8. - [] Learning `lineAngle(x1, y1, dist, degrees)`
    9. - [] Using parameters in Aesthetic Computer
*/

function boot({ wipe, fps }) {
  wipe("gray");
}

let angle2 = 0;

function paint({ wipe, ink, crawl, left, right, up, down, face, goto }) {
  wipe("gray")
  ink("yellow");

  goto();
  down();
  // const angle = right(1);
  crawl(32);

  // face(angle2);
  // angle2 = left(1);

  ink("blue");
  crawl(16);
  // face(angle);
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
