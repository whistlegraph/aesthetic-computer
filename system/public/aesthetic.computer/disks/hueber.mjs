// Hueber, 2024.3.02.23.53.39.809
// Hue-rotated / psychedelic uber riding.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🛑] This will never work with the security restrictions on Uber's current
       mobile web app.
#endregion */

// 🥾 Boot
// function boot({ api }) {
// Runs once at the start.
// }

function boot({ dom: { html } }) {
  html`
    <style>
      iframe { width: 100vw; height: 100vh; }
    </style>
    <iframe src="https://m.uber.com"></iframe>
  `
}

// 🎨 Paint
function paint({ wipe, ink }) {
  wipe("aqua"); // Would draw a diagonal line.
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
    title: "Hueber",
    desc: "Hue-rotated / psychedelic uber riding.",
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
