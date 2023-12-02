// QR, 2023.12.01.23.27.31.234
// Generate a QR code of an AC piece.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";
import * as starfield from "./starfield.mjs";

let cells;

// 🥾 Boot
function boot({ api, hud, params, net }) {
  let url = `${net.lan || net.host}`;
  const slug = params.join("~");
  if (slug) url += `/${slug}`;
  console.log(url);
  hud.label(`qr ${url}`);
  cells = qr(url).modules;

  starfield.boot(api, { stars: 512 });
  starfield.wipe(false);
}

const { floor, min, max } = Math;

// 🎨 Paint
function paint({ api, wipe, ink, screen }) {
  wipe(32, 0, 64); // Clear the screen

  starfield.paint(api, {
    alpha: 0.8,
    color: [255, 0, 200],
  });

  // 🔳 Paint the QR code.
  let margin = screen.width / screen.height > 0.8 ? 32 : 6;
  const width = screen.width - margin * 2;
  const height = screen.height - margin * 2;
  let scale = max(floor(min(width, height) / cells.length), 1); // At least 1.

  const size = cells.length * scale;
  const ox = (screen.width - size) / 2;
  const oy = (screen.height - size) / 2 + 8;

  for (let y = 0; y < cells.length; y += 1) {
    for (let x = 0; x < cells.length; x += 1) {
      ink(cells[y][x] ? "white" : "black").box(
        ox + x * scale,
        oy + y * scale,
        scale,
      );
    }
  }

  ink(undefined).box(ox - 2, oy - 2, size + 4, size + 4, "outline");
  ink("purple").box(ox - 4, oy - 4, size + 8, size + 8, "outline:4");
}

// 🎪 Act
// function act({ event: e }) {
//  // Respond to user input here.
// }

// 🧮 Sim
function sim($) {
  starfield.sim($);
}

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

export { boot, paint, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
