// List, 2024.1.23.22.58.33.239
// A directory of all pieces and commands.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Make list here in comments...



  - [] Add scrollability behavior from the `prutti` pages.
       (Maybe also generalize this scrollable behavior?)
#endregion */

// 🥾 Boot
function boot({ api, wipe }) {
  wipe("blue"); // Clear's the screen. Can use R, G, B or CSS colors.
}

// 🎨 Paint
function paint({ api, ink, line, pen, box }) {
  ink("red").line(0, 0, 100, 100);
  return false; // Uncomment for proce55ing's "noLoop" functionality.
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
    title: "List",
    desc: "A directory of all pieces and commands.",
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

// 📚 Library
//   (Useful functions used throughout the piece)
