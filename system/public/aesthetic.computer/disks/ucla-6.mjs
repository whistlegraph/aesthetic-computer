// ucla-6, 24.07.23.07.33
// Clocks, component design and relative coordinate systems.

/* 📝 Notes 
  - Today we will be designing clockfaces and working with time. 
    1. [] 🐢 Introduction to turtle graphics and relative coordinate systems.
    2.   - [] Making lines and basic operation.
    3.   - [] Making a circle.
    4.   - [] Patterns.
    4. [⏰] Keeping time and clock-making. 
    5. - [*] Using LLMs to learn about the JavaScript `Date` Object. 
    6. - [] Writing the time.
    7. - [] Progress bars and review using `num.map`.
    8. - [] Learning `lineAngle(x1, y1, dist, degrees)`
    9. - [] Using parameters in Aesthetic Computer
*/

// Built in JavaScript "Date" object.

function boot({ wipe, fps }) {
  wipe("gray");
}

function paint({ flood, wipe, ink, num, crawl, left, right, up, down, face, goto, screen }) {
  wipe("gray");

  // Blueprint...
  function textClock(margin = 6, top = 20, color = "white") {
    const spacing = 12;
    const now = new Date(); // Default to current time.
    ink(color).write("Hour: " + (now.getHours() - 12), margin, top);
    ink(color).write("Minutes: " + now.getMinutes(), margin, top + spacing);
    ink(color).write("Seconds: " + now.getSeconds(), margin, top + spacing * 2);
  }

  textClock(); // RUN THE CODE ABOVE with a 'margin' of 10 ^
  // textClock(6, 100, "blue");
  // textClock(6, 102, "lime");

  // Making a digital clock display.
  const date = new Date();
  const hour = date.getHours() - 12;
  const minutes = date.getMinutes();
  const seconds = date.getSeconds();
  const millis = date.getMilliseconds();
  ink("yellow").write(hour + ":" + minutes + ":" + seconds + ":" + millis, 6, 58);

  function progressBar(x, y, unit, unitMax, color = "red", outline = false) {
    const max = 100;
    const height = 16;
    const progress = num.map(unit, 0, unitMax, 0, max); // mapping 'millis' from 0->1000 to 0->100
    if (outline) {
      ink("black").box(x, y, max, height, "outline");
    } else {
      ink("black").box(x + progress, y, max - progress, height);
    }
    ink(color).box(x, y, progress, height); // x, y, w, h
  }

  progressBar(6, 80, millis, 1000); // x, y, unit, unitMax 
  progressBar(6, 100, seconds, 60, "green");
  progressBar(6, 120, minutes, 60, "yellow");

  progressBar(6, 160, millis, 1000, [255, 0, 0, 127], true); // x, y, unit, unitMax 
  progressBar(6, 160, seconds, 60, [0, 255, 0, 127], true); // x, y, unit, unitMax 
  progressBar(6, 160, minutes, 60, [255, 255, 0, 127], true); // x, y, unit, unitMax 
  //                                R, G, B

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
