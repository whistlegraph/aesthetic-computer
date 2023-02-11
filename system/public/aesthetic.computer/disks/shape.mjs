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

const gesture = [];
let bake = false;

// 🎨 Paint
function paint({ system, screen, page, pen, paste, params }) {
  if (bake) {
    page(system.painting).paste(screen).page(screen); // Bake to painting.
    bake = false;
  }

  if (pen?.drawing) {
    const color = params.map((str) => parseInt(str)); // Read from params.
    gesture.push([pen.x, pen.y]); // Add the whole pen state as a gesture point.
    paste(system.painting).ink(color).shape(gesture);
  }
}

// ✒ Act
function act($) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality.

  if ($.event.is("lift")) {
    gesture.length = 0;
    bake = true;
  }
}

function meta() {
  return {
    desc: "Make filled freehand shapes in any color.",
  };
}

// 📚 Library (Useful functions used throughout the piece)
export const system = "nopaint";

export { paint, act, meta };
