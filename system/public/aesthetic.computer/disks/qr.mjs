// QR, 2023.12.01.23.27.31.234
// Generate a QR code of an AC piece.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🍊] Draw a QR code given the parameters, respecting whether
       we are in debug mode or not. 
#endregion */

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

// 🥾 Boot
function boot({ api, wipe }) {
  // Runs once at the start.

  const qrcode = qr(location.href);
  const cells = qrcode.modules;
  console.log(qrcode, cells);

  wipe(0); // Clear's the screen. Can use R, G, B or CSS colors.
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
    title: "QR",
    desc: "Generate a QR code of an AC piece.",
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
