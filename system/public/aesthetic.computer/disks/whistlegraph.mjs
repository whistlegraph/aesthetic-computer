// Whistlegraph, 2021.12.03.15.21
// Title: A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [-] Add text: *Whistlegraph Recorder*
    - [🌓] Import type...
  - [] Add button: "Practice" -> `wgr`
  - [] Add button: "Record" -> `wgr:15 params` (15 seconds, eventually configurable)
  + Later
  - [] Button to a feed of latest posts / recordings.
  - [] Drawing tool / background configuration / theme.
  - [] Sound selection.
#endregion */

function boot($) {
  const { wipe, write } = $;
  wipe(0, 0, 200);
  // - [-] Remove the need for "$" in write by passing it inside the
  //       implementation within `disk` somehow.
  write($, 0, 0, "Whistlegraph Recorder", [200, 0, 0, 16]);
}

function paint($) {
  //$.write($, 0, 0, "Whistlegraph Recorder", [200, 0, 0, 16]);
}

function act() {}

export { boot, paint, act };

// 📚 Library
// ...