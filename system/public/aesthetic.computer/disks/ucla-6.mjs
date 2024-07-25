// ucla-6, 24.07.23.07.33
// Clocks, component design and relative coordinate systems.

/* 📝 Notes 
  - Today we will be designing clockfaces and working with time. 
    1. [x] 🐢 Introduction to turtle graphics and relative coordinate systems.
    2.   - [x] Making lines and basic operation.
    3.   - [x] Making a circle.
    4.   - [x] Patterns.
    5. [⏰] Keeping time and clock-making. 
    6. - [x] Using LLMs to learn about the JavaScript `Date` Object. 
    7. - [x] Writing the time.
    8. - [x] Progress bars and review using `num.map`.
    9. - [🟠] Building the clock face. 
    10.  - [] Clock hands.
    11.  - [] Numeric printed labels.
    12.  - [] Running two clocks simultaneously at different times.
    13.    - [] Component with options. 
    14.    - [] Asking an LLM to help us make offset times.
    15.  🎈 *BONUS* 🎶 Musical (Temporal) Systems

    📃 Up Next...
      Bouncing balls!
    16. - [] Using parameters in Aesthetic Computer (make sure development works)
*/

function boot({ wipe, fps }) {
  wipe("gray");
}

function paint({ flood, wipe, ink, num, crawl, left, right, up, down, face, goto, screen }) {
  wipe("gray");

  const now = new Date(); // Default to current time.
  const hour = now.getHours() - 12;
  const minutes = now.getMinutes();
  const seconds = now.getSeconds();
  const millis = now.getMilliseconds();

  function textClock(date, margin = 6, top = 20, color = "white") {
    const spacing = 12;

    ink(color).write("Hour: " + (date.getHours() - 12), margin, top);
    ink(color).write("Minutes: " + date.getMinutes(), margin, top + spacing);
    ink(color).write("Seconds: " + date.getSeconds(), margin, top + spacing * 2);

    // Making a digital clock display.
    const hour = date.getHours() - 12;
    const minutes = date.getMinutes();
    const seconds = date.getSeconds();
    const millis = date.getMilliseconds();
    ink(color).write(hour + ":" + minutes + ":" + seconds + ":" + millis, margin, top + 36);
  }

  textClock(now);
  textClock(now, 6, 180, "blue");
  textClock(now, 6, 182, "lime");

  function progressBar(x, y, unit, unitMax, color = "red", outline = false) {
    const max = 100;
    const height = 16;
    const progress = num.map(unit, 0, unitMax, 0, max); // mapping 'millis' from 0->1000 to 0->100
    if (outline) {
      ink("black").box(x, y, max, height, "outline");
    } else {
      ink("black").box(x + progress, y, max - progress, height);
    }
    ink(color).box(x, y, progress, height);
  }

  progressBar(6, 80, millis, 1000);
  progressBar(6, 100, seconds, 60, "green");
  progressBar(6, 120, minutes, 60, "yellow");

  progressBar(6, 160, millis, 1000, [255, 0, 0, 127], true);
  progressBar(6, 160, seconds, 60, [0, 255, 0, 127], true);
  progressBar(6, 160, minutes, 60, [255, 255, 0, 127], true);

  // Circular clock hand.
  ink("white");
  goto(screen.width / 2, screen.height - 100);
  down();
  face(0);
  crawl(30);
  up();
  crawl(-30);
  down();
  face(num.map(millis, 0, 1000, 0, 360));
  ink("red");
  crawl(30);
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
