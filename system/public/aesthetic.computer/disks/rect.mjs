// Rect, 22.09.19.21.07
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region ✅ TODO 
 - [🟡] Write the ideal "rect" brush code.
  - [🟢] Re-read brush code, squashing down the api bit by bit.
 + Done
 - [x] Starting a pan while mid stroke
       cancels the stroke but still stamps
       a rectangle when the pan ends and the mouse lifts. 
 - [x] Abstract "needsBake" into nopaint. 
 - [x] I need an abstraction to know whether we are making a brush
       stroke or not, in order to manage panning and drawing logic
       across platforms.
#endregion */

let rect, color;

function boot({ params }) {
  color = params.map((str) => parseInt(str));
}

// 🎨
function paint({ pen, ink, system: { nopaint } }) {
  if (nopaint.is("painting") && pen?.dragBox) {
    ink(color).box(pen.dragBox); // Render an overlay box on the screen.
    rect = nopaint.brush.dragBox; // Store current brush's dragged box geometry.
  }
}

// Paints a stroke to the current painting (in the nopaint system).
function bake({ ink }) {
  if (rect) {
    ink(color).box(rect);
    rect = null;
  }
}

const system = "nopaint";

export { boot, paint, bake, system };
