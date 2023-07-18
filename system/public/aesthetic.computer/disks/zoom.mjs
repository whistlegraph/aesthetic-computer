// Zoom, 23.04.29.22.51
// Bounces in and rotates the current painting.
// 🖼️ Used for testing and implementing bitmap scaling and rotation.

/* #region 🏁 TODO
  - [🧡] Use this to test the new 2D GPU renderer for bitmaps,
        comparing it to the software rasterizer. 
#endregion */

let angle = 0;

// 🎨 Paint (Executes every display frame)
function paint({ wipe, screen, system, pen }) {
  // const osc = Math.sin(paintCount / 20);
  // const scale = 1.25 + osc; // Bounce in and out
  // const angle = (paintCount / 2); // Slowly rotate

  const scale = 1;

  function coords(w, h) {
    let x, y;
    if (pen) {
      x = pen.x - (w * scale) / 2;
      y = pen.y - (h * scale) / 2;
    } else {
      x = screen.width / 2 - (w * scale) / 2; // Center
      y = screen.height / 2 - (h * scale) / 2;
    }
    return { x, y };
  }

  const { x, y } = coords(system.painting.width, system.painting.height);

  let pw, ph;
  if (angle === 90 || angle === 270) {
    pw = system.painting.height;
    ph = system.painting.width;
  } else {
    pw = system.painting.width;
    ph = system.painting.height;
  }

  const boxPos = coords(pw, ph);

  wipe(32)
    .ink(255)
    .box(boxPos.x, boxPos.y, pw, ph)
    .ink(255, 0, 0)
    .box(boxPos.x, boxPos.y, pw, ph, "outline")
    .paste(system.painting, x, y, { scale, angle });
}

function act({ event: e }) {
  if (e.is("touch")) angle = (angle + 90) % 360;
}

export { paint, act };

// 📚 Library (Useful functions used throughout the piece)
