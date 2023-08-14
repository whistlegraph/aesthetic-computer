// Deck, 2023.8.14.12.22.18
// A demo slide deck that explains aesthetic.computer.

/* #region 📚 README 
  See also: `/notes/funding-questions.txt`.
#endregion */

/* #region 🏁 TODO 
  - [] Make a "slide" system.
#endregion */

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
  wipe(0);
}

// 🎨 Paint
function paint({ ink, write }) {
  ink().wipe(0).write("What is aesthetic.computer?", { center: "xy" });
}

// 🎪 Act
// function act({ event }) {
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
    title: "Deck",
    desc: "A demo slide deck that explains aesthetic.computer.",
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
