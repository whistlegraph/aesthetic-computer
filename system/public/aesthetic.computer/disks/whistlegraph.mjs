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

function boot({ wipe }) {
  wipe(0, 0, 200).ink(255, 255, 0, 255).write(0, 0, "Whistlegraph Recorder", [0, 0, 0, 255]);
}

function paint($) {
}

function act() {}

export { boot, paint, act };

// 📚 Library
// ...