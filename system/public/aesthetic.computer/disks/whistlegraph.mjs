// Whistlegraph, 2021.12.03.15.21
// Title: A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [🌜] Fix ink color / inkrn regression.
  - [] Move on to output page.
  + Later
  - [] Button to a feed of latest posts / recordings.
  - [] Drawing tool / background configuration / theme.
  - [] Sound selection.
  - [] Add duration to recording params.
  + Done
  - [x] Add button: "Practice" -> `wgr`
  - [x] Add button: "Record" -> `wgr:15 params` (15 seconds, eventually configurable)
  - [x] Add inkrn.
  - [x] Add text: *Whistlegraph Recorder*
    - [x] Import type...
#endregion */

let practice, record;

function boot({ wipe, ink, write, ui: { Button } }) {
  // TODO: Get the separate practice and record text colors working!

  wipe(0, 0, 200)
    .ink(255, 255, 0, 255)
    .write(0, 0, "Whistlegraph Recorder", [0, 0, 0, 255]);

  // Practice Button
  practice = new Button({ x: 10, y: 20, w: 80, h: 20 });
  ink(0, 200, 0)
    .box(practice.box, "fill")
    .ink(200, 0, 0)
    .write(practice.box.x, practice.box.y, "Practice", 127);

  // Record Button
  record = new Button({ x: 10, y: 50, w: 80, h: 20 });
  ink(255, 0, 0)
    .box(record.box, "fill")
    .ink(0, 0, 255)
    .write(record.box.x, record.box.y, "Record", 127);
}

function paint($) {}

function act({ event: e, jump }) {
  practice.act(e, () => jump("wgr practice"));
  record.act(e, () => jump("wgr record"));
}

export { boot, paint, act };

// 📚 Library
// ...
