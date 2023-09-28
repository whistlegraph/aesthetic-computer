// Moods, 2023.9.28.01.40.14.735
// A live list of all our moods.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Render moods.
#endregion */

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
  wipe(0);

  fetch("/api/mood/all")
    .then((res) => res.json())
    .then((body) => {
      if (body.moods) {
        console.log("Moods:", body);
      } else {
        throw new Error(body.message);
      }
    })
    .catch((err) => console.warn("📶🙁 Mood error:", err));
}

// 🎨 Paint
function paint({ ink, wipe, pen }) {}

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
    title: "Moods",
    desc: "A live list of all our moods.",
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
