// Pull, 23.01.05.13.11 
// A brush for copying and moving pixels in various shapes. 

/* #region 🤝 TODO 
  - [🟡] Implement some kind of act override for brushes.
  - [] Implement pull for rectangles.
#endregion */

// 🎨 Paint (Executes every display frame)
function paint({ pen, box, geo }) {
  // Step 1. Click and drag to crop a box selection.
  // selection = new geo.Box(x, y, w, h).abs.crop(0, 0, width, height);
  // console.log(pen);
}

/*
// Start drag.
if (e.is("touch")) {
  if (state === "rest") {
    state = "selecting";
    cursor("none");
    select(geo, { x: e.x, y: e.y, w: 1, h: 1 }, sketch);
  }
  if (state === "selected") state = "placing";
}

// Continue drag.
if (e.is("draw")) {
  if (state === "selecting") {
    select(geo, e.drag, sketch);
  } else if (state === "placing") {
    selection.move(e.delta);
    cursor("tiny");
  }
}
*/

export const system = "nopaint:dont-paint-on-leave";

export { paint }

// 📚 Library (Useful functions used throughout the piece)
// ...
