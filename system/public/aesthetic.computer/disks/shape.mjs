// Shape, 23.02.09.20.23
// A brush for making filled freehand shapes in any color.
// (Requested by Artur)

/* #region 🏁 todo
  + Second Version
    + System
      - [] Add a 'n' shortcut to simply reload the same piece (without storing the painting)
      - [] Automate the inheritance call across the board. (see `act`). 
    - [] Add input smoothing / new Race abstraction... built in race for nopaint?
    - [] Support outline / border mode.
  + Done
    - [x] Add color support.
    - [x] Draw the filled shape and paint it to the backbuffer in `act:lift`. 
#endregion */

const screenGesture = [],
  brushGesture = [];
let shape, color;

function boot({ params, num }) {
  color = num.parseColor(params);
}

// 🎨 Paint
function paint({ pen, ink, system: { nopaint } }) {
  if (nopaint.is("painting")) {
    screenGesture.push([pen.x, pen.y]);
    ink(color).shape(screenGesture);

    brushGesture.push([nopaint.brush.x, nopaint.brush.y]);

    shape = () => {
      ink(color).shape(brushGesture.slice());
      brushGesture.length = 0;
      screenGesture.length = 0;
      shape = null; // TODO: Abstract this so writing it is unnecessary. 23.05.04.12.39
    };
  }
}

// 🍪 Prints to the current painting.
function bake() {
  shape?.();
}

function meta() {
  return {
    desc: "Make filled freehand shapes in any color.",
  };
}

// 📚 Library (Useful functions used throughout the piece)
export const system = "nopaint";

export { boot, paint, bake, meta };
