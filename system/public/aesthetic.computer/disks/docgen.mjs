// Docgen, 2024.2.01.20.00.56.109
// A tool to helpfully generate and map the piece API.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Generate docs for each top-level function. 
  - [] Some may need to be "fake" fired.
    - [] leave...
#endregion */

// 🥾 Boot
function boot($) {
  console.log("boot", $);
}

// 🎨 Paint
function paint($) {
}

// 🎪 Act
function act({ event: e }) {
  // Respond to user input here.
}

// 🧮 Sim
function sim() {}

// 🥁 Beat
function beat() {
  // Runs once per metronomic BPM.
}

// 👋 Leave
function leave() {
  // Runs once before the piece is unloaded.
}

// 📰 Meta
function meta() {
  return {
    title: "Docgen",
    desc: "Helpfully generates and maps the dynamic piece API.",
  };
}

// 🖼️ Preview
function preview({ ink, wipe }) {
  // Render a custom thumbnail image.
}

// 🪷 Icon
function icon() {
// Render an application icon, aka favicon.
}

export { boot, paint, act, sim, beat, leave, meta, preview, icon };

// 📚 Library
//   (Useful functions used throughout the piece)
