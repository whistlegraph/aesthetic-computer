// Sprite, 2023.11.14.21.29.43.964
// A reader for a 3D rotated AC sprite format in 16xN.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let sheet;

let viewing = true; // Rotating view frames.
let view = 0; // From 0 -> 15
let viewSpeed = 0.15;

let frames, fw, fh; // Animation frames.
let frameSpeed = 0.15;
let frame = 0;

const { floor } = Math;

// 🥾 Boot
function boot({ wipe, params, store }) {
  viewSpeed = parseFloat(params[0]) || viewSpeed;
  frame = parseFloat(params[1]) || frame;
  // frameSpeed = parseFloat(params[2]) || frameSpeed;
  if (store["sprite:sheet"]) ({ sheet, fw, fh } = store["sprite:sheet"]);
  wipe(64).ink(127).write("drop a 16xN square sheet png", { center: "xy" });
}

// 🎨 Paint
function paint({ wipe, paste, screen }) {
  if (sheet) {
    wipe(32);
    paste(
      {
        painting: sheet,
        crop: { x: floor(view) * fw, y: floor(frame) * fh, w: fw, h: fh },
      },
      screen.width / 2 - fw / 2,
      screen.height / 2 - fh / 2,
    );
  }
}

// 🎪 Act
// Respond to user input here.
function act({ event: e, store }) {
  if (e.is("touch")) viewing = false;

  if (e.is("draw")) {
    view += e.delta.x * 0.1;
    if (view < 0) view += 15;
    if (view > 15) view -= 15;
  }

  if (e.is("lift")) viewing = true;

  if (e.is("dropped:bitmap")) {
    sheet = e.painting;
    fw = sheet.width / 16;
    fh = fw;
    store["sprite:sheet"] = { sheet, fw, fh };
    frames = sheet.height / fh; // Frame support dropped for now.
  }
}

// 🧮 Sim
function sim() {
  if (viewing) view = (view + viewSpeed) % 15;
  // if (frame < frames) frame = (frame + frameSpeed) % frames;
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
    title: "Sprite",
    desc: "A reader for the AC sprite format.",
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

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
