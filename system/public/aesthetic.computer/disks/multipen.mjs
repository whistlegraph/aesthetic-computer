// Multipen, 22.09.04.16.18
// A basic example of multi-touch / multiple tracked cursors from one client,
// using both the `pen` API for the primary pointer and `pens` for the others,
// in addition to filtering "touch:2" for pointer no. 2 in `act`.

/*
TODO
- [] Modify this test to encompass N pointers.
- [] Add numbers next to each pointer.
- [] Figure out what to do when a new pointer gets added after the first one
     was lifted, but a second or third one is already down.
- [x] Draw a point for one cursor, and a line for two.
*/

let rgb = [0];

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, pen, pens }) {
  wipe(128); // Gray background.
  ink(rgb).line(pen?.x, pen?.y, pens(2).x, pens(2).y); // Line from pen 1->2.
  ink(0, 0, 255).circle(pen?.x, pen?.y, 16); // Blue circle for 1st pen.
  ink(0, 255, 255).circle(pens(2).x, pens(2).y, 16); // Teal for 2nd pen.
}

// ✒ Act (Runs once per user interaction)
function act({ event: e }) {
  if (e.is("touch:2")) rgb = [255, 0, 0]; // Alter color when 2nd pen is down.
  if (e.is("lift:2")) rgb = [0]; // And again when it's lifted.
}

export { paint, act };
