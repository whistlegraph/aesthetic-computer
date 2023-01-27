// Video, 23.1.26.12.01
// Play back and be able to export / upload a recorded video.

/* #region ✏️ todo
  + Now
  - [] Factor out / modify the old video overlay UI thing to only work
      in this piece.
  - [] Add a "loop" and a "home" (back to title) button.
  - [] Take advantage of the dead time / transcoding time.
  - [] Show a little game or helpful hint. (💡 @alex)
  - [] Only transcode upon tapping export.
  + Done
  - [x] How to know when we have started printing immediately?
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, ink, screen, rec }) {
  wipe(0, 255, 0);
  ink(0).line(0, 0, screen.width, screen.height);
  rec.print(); // Start printing a recording if it exists...
}

// 🎨 Paint (Executes every display frame)
function paint({
  wipe,
  rec: { printing, printProgress },
  screen: { width, height },
}) {
  if (printing) {
    // Draw progress bar for printing...
    const h = 16;
    wipe(80, 0, 0)
      .ink(255, 0, 0)
      .box(0, height / 2 - h / 2, printProgress * width, h);
  } else {
    // How to center the "No Video" text?
    wipe(40, 0, 0).ink(180, 0, 0).write("No Video", {center: "xy"});
  }
}

// ✒ Act (Runs once per user interaction)
// function act({ event: e }) { }

export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
// ...