// 💅 Pline, 2022.01.24.02.41
// A *perfect* 1px line drawing algorithm.
// This piece also functions as a test implementation for `dirtyBox` rendering.
// (Where only partial screen updates are sent to the main thread)

// 🚒
// TODO: For some reason the first wipe is not always running...
//       Maybe it's a race condition? 22.09.17.13.34
// * This is a problem because any dirtyBox defined in the first paint
//   will also currently effect the first boot...
// * This needs to be restructured. 22.09.20.00.09

// Make hotkey & thumb button to clear the page?
// TODO: Better colors. Abstract everything so it can be used
//       in multiple instances. (See: `Painters` in `nail`)

let painting; // A bitmap to draw on.
let points = []; // This stored every point in a mark.
let allPoints = [];
let pointsToPaint = [];
let pointsToHighlight = [];
let usingMouse = true;
let lastPoint;
let priorPointsIndex = 0;
let tapped;
const tail = 2; // A red visual tail that follows the 1px line.
let db1;
let lastDirtyBox;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, paste, cursor, painting: p, screen, geo }) {
  cursor("none");
  // Make & display the canvas.
  painting = p(screen.width, screen.height, (gfx) => gfx.wipe(100, 10, 20));
  wipe(100, 100, 100);
  db1 = new geo.DirtyBox();

  // TODO: Glaze seems to be incompatible with dirty box on the first frame
  //       that gets rendered. 2022.04.11.05.57
}

let continuedBoxCopy;

// 🎨 Paint (Runs once per display refresh rate)
function paint({ pen, ink, page, screen, paste, geo, paintCount }) {
  // A. Replace any content painted last frame with the contents of `painting`.

  // TODO: How to automate this so I can write..
  // if (dirty) { paste({painting, crop: dirty}, dirty.x, dirty.y); }
  // else pase(painting);

  if (lastDirtyBox) {
    paste(
      { painting, crop: geo.Box.from(lastDirtyBox) },
      lastDirtyBox.x,
      lastDirtyBox.y
    );
    continuedBoxCopy = geo.Box.from(lastDirtyBox);
    lastDirtyBox = undefined;
  } else {
    paste(painting);
  }

  // B. Paint anything that needs to be permanent.
  if (pointsToPaint.length) {
    page(painting);
    pointsToPaint.forEach((p) => {
      ink(200, 255, 200, 100).plot(p.x, p.y);
      db1.soil(p);
    });
    pointsToPaint.length = 0;

    // Paste what was painted, cropped to the box.
    page(screen).paste(
      { painting, crop: geo.Box.from(db1.box) },
      db1.box.x,
      db1.box.y
    );
  }

  // C. Paint any preview pixels that are still being calculated if we are
  //    currently drawing.
  if (pointsToHighlight.length) {
    pointsToHighlight.forEach((p) => {
      ink(100, 100, 0).plot(p.x, p.y);
      db1.soil(p);
    });
    ink(200, 0, 0).plot(pen); // 🔴 Painting cursor.
    db1.soil(pen);
  }

  if (paintCount > 0n && pointsToHighlight.length === 0 && usingMouse) {
    // Or just paste the existing painting and paint a navigation cursor.
    ink(255, 255, 0, 100).plot({ x: pen.x, y: pen.y }); // 🟡 Navigation cursor.
    db1.soil(pen);
  }

  if (db1.soiled) lastDirtyBox = geo.Box.from(db1.box); // Store what pixels were updated this frame.

  if (continuedBoxCopy) {
    db1.soil(continuedBoxCopy);
    db1.soil({ x: continuedBoxCopy.right, y: continuedBoxCopy.bottom });
    continuedBoxCopy = undefined;
  }

  if (db1.soiled) {
    const db = db1;
    db1 = new geo.DirtyBox();
    return db;
  }

  return false;
}

// ✒ Act (Runs once per user interaction)
function act({
  event: e,
  num: { dist },
  abstract: { bresenham },
  geo,
  needsPaint,
}) {
  // TODO: Fix defocusing of window extending the dirtyBox.
  if (e.is("defocus")) {
    //console.log("defocus");
    // db1 = new geo.DirtyBox();
    // continuedBoxCopy = undefined;
    // lastDirtyBox = undefined;
  }

  if (e.penChanged === true) {
    if (e.is("touch") || e.is("draw") || e.is("move") || e.is("lift"))
      needsPaint();
  }

  if (e.is("touch")) {
    const p = point(e);
    allPoints.push(p); // Record points for playback.
    pointsToPaint.push(p);
    lastPoint = p;
    tapped = true;
  }

  if (e.is("draw")) {
    tapped = false;
    const p = point(e);
    const minDist = 0;

    if (dist(p.x, p.y, lastPoint.x, lastPoint.y) >= minDist) {
      // Make sure the points are not equal.
      if (lastPoint.x !== p.x || lastPoint.y !== p.y) {
        // console.log("☑️Points:", points.slice());

        // Add bresen points, filtering out repeats.
        bresenham(lastPoint.x, lastPoint.y, p.x, p.y).forEach((np, i) => {
          if (i > 0 || points.length < 2) points.push(np);
        });
        // console.log("☑️Bresen:", points.slice());

        lastPoint = p;
        // Filter out "L" shapes from interpolated points.
        const filteredPoints = pixelPerfectLine(points);
        // console.log("☑️Filtered:", filteredPoints.slice());

        filteredPoints.forEach((p, i) => {
          if (i >= priorPointsIndex && i < filteredPoints.length - 1) {
            pointsToHighlight.push(p); // Preview the filtered points.
            // Then immediately paint and record them.
            if (i > 0) pointsToPaint.push(p); // Queue points for painting.
            allPoints.push(p); // Record points for playback.
          }
        });

        points = filteredPoints.slice(-2); // Consume all but up to two points to leave for `pixelPerfect`.
        priorPointsIndex = 1; // Remember how many we have left over so we skip them on the next pass.
        // TODO: Make sure tail of 0 does not clone all of the highlight.
        pointsToHighlight = pointsToHighlight.slice(-tail); // Trim highlight points if we go over the tail.
      }
    }
  }

  if (e.is("lift")) {
    if (points.length && tapped === false)
      pointsToPaint.push(points[points.length - 1]); // Paint last point.
    points.length = 0;
    pointsToHighlight.length = 0;
    priorPointsIndex = 0;
    lastPoint = null;
    usingMouse = e.device === "mouse";
    console.log("➕ Pixels:", allPoints.length);
  }
}

// 💗 Beat (Runs once per bpm)
// function beat($api) {}

// 🧮 Simulate (Runs once per logic frame (120fps)).
// function sim($api) {}

// 📚 Library (Useful functions used throughout the program)

/**
 * Extract the necessary fields from the event object to produce a point sample.
 * @param e
 * @returns {{x, y, pressure}}
 */
function point(e) {
  return (({ x, y, pressure }) => ({ x, y, pressure }))(e);
}

// ⚠️ This has now been moved to `graph` 22.12.28.12.54
// Takes an array of pixel coordinates {x, y} and filters out L shapes.
// Note: It checks the previous, current, and next pixel and requires a minimum
//        set of 3 before it removes anything.
// Transcribed from: https://rickyhan.com/jekyll/update/2018/11/22/pixel-art-algorithm-pixel-perfect.html
function pixelPerfectLine(pixels) {
  if (pixels.length === 1 || pixels.length === 0) {
    return pixels; // Return the inputs if the length is 0 or 1.
  }

  let filtered = [];
  let c = 0;

  while (c < pixels.length) {
    if (
      c > 0 &&
      c + 1 < pixels.length &&
      (pixels[c - 1].x === pixels[c].x || pixels[c - 1].y === pixels[c].y) && // check left and up
      (pixels[c + 1].x === pixels[c].x || pixels[c + 1].y === pixels[c].y) && // check right and down
      pixels[c - 1].x !== pixels[c + 1].x && // check left and right of prev and next
      pixels[c - 1].y !== pixels[c + 1].y
    ) {
      // check top and bottom of prev and next
      c += 1;
    }
    filtered.push(pixels[c]);
    c += 1;
  }
  return filtered;
}

export { boot, paint, act };
