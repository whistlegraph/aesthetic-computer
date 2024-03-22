// Rattle, 2023.8.14.21.03.16
// An instrument for shaking.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
 - [🍊] Add accelerometer information.
#endregion */

// 🥾 Boot
function boot({ wipe, ink, line, motion }) {
  // Runs once at the start.
  wipe();
  motion.start();
}

// 🎨 Paint
function paint({ wipe, ink, motion }) {
  // Executes every display frame.
  wipe(0, 100, 80);
  ink(255).write(JSON.stringify(motion.current), { x: 12, y: 20 });
}

// 🎪 Act
function act({ event }) {
  //  // Respond to user input here.
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
function leave({ motion }) {
  motion.stop();
}

// 📰 Meta
function meta() {
  return {
    title: "Rattle",
    desc: "An instrument for shaking.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, leave, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
