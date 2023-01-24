// Whistlegraph, 2021.12.03.15.21
// Title: A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [] Add button: "Practice" -> `wgr`
  - [] Add button: "Record" -> `wgr:15 params` (15 seconds, eventually configurable)
  + Later
  - [] Button to a feed of latest posts / recordings.
  - [] Drawing tool / background configuration / theme.
  - [] Sound selection.
  + Done
  - [x] Add inkrn.
  - [x] Add text: *Whistlegraph Recorder*
    - [x] Import type...
#endregion */

let practice;

function boot({ wipe, ink, write, ui: { Button } }) {
  // TODO: Chained paint functions do not respect inkrn.
  wipe(0, 0, 200);
  ink(255, 255, 0, 255);
  write(0, 0, "Whistlegraph Recorder", [0, 0, 0, 255]);

  practice = new Button({x: 10, y: 10, w: 20, h: 20});

  // B. 🌟 Open Button
  ink(255, 0, 0).box(practice.box, "in"); // Border
}

function paint($) {
}

function act() {}

export { boot, paint, act };

// 📚 Library
// ...