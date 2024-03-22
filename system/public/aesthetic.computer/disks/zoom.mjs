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

  const scale = { x: 1, y: 1 };

  const { abs } = Math;

  function coords(w, h) {
    let x, y;
    if (pen) {
      x = pen.x - (w * abs(scale.x)) / 2;
      y = pen.y - (h * abs(scale.y)) / 2;
    } else {
      x = screen.width / 2 - (w * abs(scale.x)) / 2; // Center
      y = screen.height / 2 - (h * abs(scale.y)) / 2;
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

  // console.log("Paint position:", x, y);
  // console.log("Painting:", system.painting.width, system.painting.height);

  let ax = 120;
  let ay = 120;
  // let tx = 0;
  // let ty = 0;

  // console.log(system.painting.width, system.painting.height);

  if (angle === 90) {
    ax += system.painting.height;
  } else if (angle === 180) {
    ay += system.painting.height;
    ax += system.painting.width;
  } else if (angle === 270) {
    ay += system.painting.width;
  }

  wipe(32)
    .ink(255, 255, 0)
    // .box(boxPos.x, boxPos.y, pw, ph)
    .box(120, 120, pw, ph)
    .ink(255, 0, 0)
    // .box(boxPos.x, boxPos.y, pw, ph, "outline")
    // .box(0, 0, pw, ph, "outline")
    //.paste(system.painting, x, y, { scale, angle });
    // TODO: Eventually floor these ^ 😃
    //.pan(tx, ty)
    .paste(system.painting, ax, ay, { scale, angle, anchor: { x: 0, y: 0 } })
    //.unpan();

  return false;
}

function act({ event: e, needsPaint }) {
  if (e.is("touch")) {
    angle = (angle + 90) % 360;
    needsPaint();
  }
}

export { paint, act };

// 📚 Library (Useful functions used throughout the piece)
