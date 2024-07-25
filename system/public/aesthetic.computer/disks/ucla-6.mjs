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
    10.  - [-] Clock hands.
          - [x] millis
          - [] seconds
          - [] minutes
          - [] hour (subtract by 12)
    11.  - [] Numeric printed labels.
           - [] AM/PM
    12.  - [] Running two clocks simultaneously at different times.
    13.    - [] Component with options. 
    14.    - [] Asking an LLM to help us make offset times.
    15.  🎈 *BONUS* 🎶 Musical (Temporal) Systems

    📃 Up Next...
      Bouncing balls!
    16. - [] Using parameters in Aesthetic Computer (make sure development works)
*/

const theme = {
  millis: "red",
  seconds: "green",
  minutes: "yellow",
};

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
    ink(color).write(
      "Seconds: " + date.getSeconds(),
      margin,
      top + spacing * 2,
    );

    // Making a digital clock display.
    const hour = date.getHours() - 12;
    const minutes = date.getMinutes();
    const seconds = date.getSeconds();
    const millis = date.getMilliseconds();
    ink(color).write(
      hour + ":" + minutes + ":" + seconds + ":" + millis,
      margin,
      top + 36,
    );
  }

  textClock(now, 6, 20, "red");
  textClock(now, 6 + 1, 20 + 1, "blue");
  textClock(now, 6 + 2, 20 + 2, "white");

  function progressBar(
    x,
    y,
    width = 100,
    height = 16,
    unit,
    unitMax,
    color = "white",
    outline = false,
  ) {
    const progress = num.map(unit, 0, unitMax, 0, width); // mapping 'millis' from 0->1000 to 0->100
    if (outline) {
      ink("black").box(x, y, width, height, "outline");
    } else {
      ink("black").box(x + progress, y, width - progress, height);
    }
    ink(color).box(x, y, progress, height);
  }

  const progressTop = 70;
  progressBar(6, progressTop, 100, 8, millis, 1000, theme.millis);
  progressBar(6, progressTop + 8, 100, 12, seconds, 60, theme.seconds);
  progressBar(6, progressTop + 8 + 12, 100, 16, minutes, 60, theme.minutes);

  const outlinedTop = progressTop + 8 + 12 + 16 + 3;
  progressBar(6, outlinedTop, 100, 10, millis, 1000, [theme.millis, 127], true);
  progressBar(6, outlinedTop, 100, 10, seconds, 60, [theme.seconds, 127], true);
  progressBar(6, outlinedTop, 100, 10, minutes, 60, [theme.minutes, 127], true);

  // Circular clock hand.
  /*
      🐢-> (crawl) - move forward at the current angle
      🖍️ (down or up) - start or stop drawing
      ↩ (️left or right) - turn by a relative angle
      📐 (face) - face any angle
      🚀 (goto) - teleport to any x,y position
  */

  // Set scale and starting position. 
  const scale = 1; // We may decide on this number based on screen space.
  const center = [screen.width / 2, screen.height - screen.height / 3]; // Arrays
  goto(...center); // ... will 'spread' the array into the function parameters 

  // Registration point...
  face(0); // facing the angle of 0 -------->
  ink("white");
  down(); // start a drawing...
  crawl(30 * scale); // move forward by 30 and leave a white line
  up();
  crawl(-30 * scale);

  // Millis Hand
  face(num.map(millis, 0, 1000, 0, 360)); // a mapping of millis to 0->360
  ink(theme.millis);
  down();
  crawl(25 * scale);
  // up();

  goto(...center); // Seconds Hand
  face(num.map(seconds, 0, 60, 0, 360));
  ink(theme.seconds);
  down();
  crawl(28 * scale);
  up();

  goto(...center); // Minutes Hand
  face(num.map(minutes, 0, 60, 0, 360));
  ink(theme.minutes);
  down();
  crawl(30 * scale);
  up();
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
