// Whistlegraph, 2021.12.03.15.21
// Title: A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [⚠️] Fix record flow errors - quitting during playback then re-recording is
       weird, especially on iOS.
       (Can hear old audio playing, etc.)
  - [] Why isn't "learn" clearing the top left.
    - [] It has to do with `boot` not clearing that top left part of the buffer.
  - [] Clear todos in `video`, `download`, and `wgr`.   
  + Later
  - [] Button to a feed of latest posts / recordings.
  - [] Drawing tool / background configuration / theme.
  - [] Sound selection.
  - [] Add duration to recording params.
  + Done
  - [x] Move on to output page for video option.
  - [x] Fix ink color / inkrn regression.
  - [x] Add button: "Practice" -> `wgr`
  - [x] Add button: "Record" -> `wgr:15 params` (15 seconds, eventually configurable)
  - [x] Add inkrn.
  - [x] Add text: *Whistlegraph Recorder*
    - [x] Import type...
#endregion */

let practice, record, learn;

function boot({ wipe, ink, write, num, ui: { Button } }) {
  const y = 35;
  const x = 4;

  // Title
  wipe(0, 0, 200)
    .ink(255, 255, 0, 255)
    .write("Whistlegraph Recorder", {x, y}, [0, 0, 0, 255]);

  // Practice Button
  practice = new Button({ x: x + 10, y: y + 30, w: 80, h: 20 });
  ink(0, 100, 0)
    .box(practice.box, "fill")
    .ink(0, 255, 0, 150)
    .box(practice.box, "outline")
    .ink(0, 200, 0)
    .write("Practice", num.p2.add(practice.box, {x: 4, y: 4}), [0, 50, 0]);

  // Record Button
  record = new Button({ x: x + 10, y: y + 60, w: 80, h: 20 });
  ink(255, 0, 0)
    .box(record.box, "fill")
    .ink(255, 0, 0)
    .write("Record", num.p2.add(record.box, {x: 4, y: 4}), [100, 0, 0]);

  // Learn Button
  learn = new Button({ x: x + 10, y: y + 90, w: 80, h: 20 });
  ink(0, 100, 255)
    .box(learn.box, "fill")
    .ink(0, 200, 200)
    .write("Learn", num.p2.add(learn.box, {x: 4, y: 4}), [0, 0, 50, 80]);
}

function paint() {
  // return true;
}

function act({ event: e, jump }) {
  practice.act(e, () => jump("wgr practice"));
  record.act(e, () => jump("wgr record"));
  learn.act(e, () => jump("wg"));
}

export { boot, paint, act };

// 📚 Library
// ...
