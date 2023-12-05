// iMessage, 2023.12.05.14.08.23.293
// A piece that loads for Apple's iMessage app extension.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Draw a purple line.
#endregion */

// 🥾 Boot
function boot({ wipe }) {
  // Runs once at the start.
  wipe("blue"); // Clear's the screen. Can use R, G, B or CSS colors.
}

// 🎨 Paint
function paint({ api, ink, line, pen, box }) {
  ink().write("iMessage");
  // ink("red").line(0, 0, 100, 100); Would draw a diagonal line.
  // return false; // Uncomment for proce55ing's "noLoop" functionality.
}

// 🎪 Act
// function act({ event: e }) {
//  // Respond to user input here.
// }

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Imessage",
    desc: "A piece that loads for Apple's iMessage app extension.",
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

export { boot, paint, meta };
export const nohud = true;

// 📚 Library
//   (Useful functions used throughout the piece)
