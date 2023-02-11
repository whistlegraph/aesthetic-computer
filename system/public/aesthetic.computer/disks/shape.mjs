// Shape, 23.02.09.20.23 
// A brush for making outline shapes.

/* #region 🏁 todo
  + First Version
  - [] Draw the filled shape and paint it to the backbuffer in `act:lift`. 
  - [] 
  - System
#endregion */

const gesture = [];

// 🥾
function boot($) {
  $.wipe(0, 255, 0);
  $.ink(0).line(0, 0, $.screen.width, $.screen.height)
}

// 🎨
function paint({pen}) {
  if (pen.x)
}

// ✒ Act
function act({ event }) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality. // TODO: Deprecate the need for these "super"-like calls.
}

// 📚 Library (Useful functions used throughout the piece)
export const system = "nopaint";

export { boot, paint }