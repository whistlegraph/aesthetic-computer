// Demo, 2023.10.15.14.55.00.575
// What is aesthetic.computer?

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ jump }) {
  jump(
    "https://www.dropbox.com/scl/fi/3cmnkp3oqoth9by99fieh/aesthetic-computer-demo.mov?rlkey=amzo78pi2qrrctiy434tle3nq&dl=0",
  );
}

// 🎨 Paint
function paint({ ink }) {
  return false; // Uncomment for a frame loop.
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
    title: "Demo",
    desc: "What is aesthetic.computer?",
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
