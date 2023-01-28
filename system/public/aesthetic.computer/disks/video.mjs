// Video, 23.1.26.12.01
// Play back and be able to export / upload a recorded video.

/* #region ✏️ todo
  + Now
  - [😀] Fix recording visibility on iOS!
  - [] Add a "home" / button.
    - [] Transcode upon tapping export.
      - [] Then transcode & upload and reveal download options, based on
           device capability.

  - [-] Factor out / comment or modify the old video overlay UI code.
  - [] Would I be able hold onto / store the recording similar to a painting
       on the client? So that way refreshing the page can work...
  + Done
  - [x] Pressing "back" and then going "forward" should restart your recording,
  - [x] Show preview of video immediately when the piece loads if a video
        actually exists.
  - [x] It can be shown in a special DOM layer and this piece can have a clear
        backdrop with a few controls.
  - [x] How to know when we have started printing immediately?
  + Later
  - [] Make multi-scene recordings by not stopping a recording on back / 
       intentionally do multi-scene stuff, perhaps with different configurations
       of images even according to a rhythm, with preloaded piece source code.
  - [] Take advantage of the dead time / transcoding time.
    - [] Show a little game or helpful hint. (💡 @alex)
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, ink, screen, rec }) {
  wipe(0, 255, 0);
  ink(0).line(0, 0, screen.width, screen.height);

  // rec.print(); // Immediately start printing a recording if it exists.

  rec.present(); // Visually present a recording if one exists.
}

// 🎨 Paint (Executes every display frame)
function paint({
  wipe,
  rec: { printing, presenting, printProgress, presentProgress },
  screen: { width, height },
}) {

  if (presenting) {
    wipe(0, 0, 200, 40);
    // ink(255).box(0, height - 4, width * presentProgress, height - 4); // Present a progress bar.
  } else if (printing) {
    const h = 16; // Paint a printing / transcoding progress bar.
    wipe(80, 0, 0)
      .ink(255, 0, 0)
      .box(0, height / 2 - h / 2, printProgress * width, h);
  } else wipe(40, 0, 0).ink(180, 0, 0).write("No Video", { center: "xy" });

}

// ✒ Act (Runs once per user interaction)
// function act({ event: e }) { }

export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
// ...
