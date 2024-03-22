// Crop, 2023.7.12.18.49.51
// Crop or extend your painting by drawing a rectangle.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟢] Draw an overlay rectangle to show the darkened crop on the painting, where
         returning to the prompt crops it.
#endregion */

const { max } = Math;
let crop, b;

// 🎨 Paint
function paint({
  pen,
  ink,
  painting,
  store,
  system: sys,
  screen,
  params,
  num,
}) {
  if (sys.nopaint.is("painting") && pen?.dragBox) {
    b = pen.dragBox.abs;
    const color = num.parseColor(params);
    crop = () => {
      const { x, y } = sys.nopaint.transform({ x: b.x, y: b.y });
      // Resize the original painting.
      sys.painting = painting(b.w, b.h, (p) => {
        if (color !== "erase") {
          p.wipe(color).paste(sys.painting, -x, -y);
        } else {
          p.paste(sys.painting, -x, -y);
        }
      });
      sys.nopaint.translation = { x: b.x, y: b.y };
      // Assume we changed the size of the painting so set the resolution lock.
      store["painting:resolution-lock"] = true;
      store.persist("painting:resolution-lock", "local:db");
      crop = null;
    };
  }
  if (b) {
    // Draw a preview on-screen.
    ink(0, 64).box(b, `outline:${max(screen.width, screen.height)}`);
    ink().box(b, "outline");
    sys.nopaint.needsPresent = true;
  }
}

// 🍪 Prints to the current painting.
function bake() {
  crop?.();
}

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
    title: "Crop",
    desc: "Crop or extend your painting by drawing a rectangle.",
  };
}

export const system = "nopaint:bake-on-leave";
export { paint, bake, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
