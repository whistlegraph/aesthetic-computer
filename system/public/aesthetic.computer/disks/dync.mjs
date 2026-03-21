// Dync, 2024.1.17.20.54.39.695
// A percussive pad instrument.

/* #region 📚 README 
  - Welcome to your first aesthetic.computer piece.
  - You can `console.log(api);` to explore,
    and then destructure additional api commands
    to try them out!
  - Top-level commands make up each piece's application flow.
    Uncomment and export them to invoke their behavior and use their apis.
  - Enter `help` for the AC #help and ping @helper for additional explanation.
#endregion */

/* #region 🏁 TODO 
  - [] Draw a purple line.
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
    title: "Dync",
    desc: "A percussive pad instrument.",
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
