// PTT, 2023.12.17.16.19.47.977
// Push to talk, with others.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Repair `microphone` somehow and than integrate it here.
  - [] Also re-read `baktok`.
  - [-] Record a message from the microphone, upload it and let everyone
        receive it via sockets... 
#endregion */

// 🥾 Boot
function boot({ api, wipe }) {
  // Runs once at the start.
  wipe("blue"); // Clear's the screen. Can use R, G, B or CSS colors.
}

// 🎨 Paint
function paint({ api, ink, line, pen, box }) {
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
    title: "PTT",
    desc: "Push to talk, with others.",
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
