// login-wait, 2024.1.26.12.17.47.464
// A simple screen that shows when awaiting a hosted `Sign in` flow.

/* #region 🏁 TODO 
#endregion */

let ellipsisTicker;
export const nohud = true;

// 🥾 Boot
function boot({ gizmo }) {
  ellipsisTicker = new gizmo.EllipsisTicker();
}

// 🎨 Paint
function paint({ wipe, screen, help }) {
  wipe(70, 50, 100)
    .ink(230, 30, 100)
    .write(
      "Continue in browser" +
        ellipsisTicker.text(help.repeat, { pad: false }),
      { center: "xy", size: 2 },
      [0, 24],
      screen.width * 0.75,
    );
}

// 🧮 Sim
function sim() {
  ellipsisTicker?.sim();
}

// 🎪 Act
// function act({ event: e }) {
//  // Respond to user input here.
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
    title: "Login-wait",
    desc: "A simple screen that shows when awaiting Sign in.",
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

export { boot, paint, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
