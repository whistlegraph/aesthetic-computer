// Video, 23.1.26.12.01
// Play back and be able to export / upload a recorded video.

/* #region ✏️ todo
  + Now
  - [x] Add "export" or "save" button.

    - [x] Transcode upon tapping export.
      - [] Reveal download options, based on
           device capability.

  - [😀] Factor out / comment or modify the old video overlay UI code.

  - [] Add "fuzzy" intro instead of "Get ready..." or a delay or something.
       (Test Pattern)
  - [-] Add stamp on export.

  - [] Would I be able hold onto / store the recording similar to a painting
       on the client? So that way refreshing the page can work...
  + Done
  - [x] Fix recording visibility on iOS!
    - [x] Unhandled Promise Rejection
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

let btn, label = "Download"; // "Export" | "Download" | "CODE" button.
let slug; // Stores a download code for prepending to locally downloaded videos.

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, rec }) {
  wipe(0);
  rec.present(); // Visually present a recording right away if one exists.
}

// 🎨 Paint (Executes every display frame)
function paint({
  wipe,
  ink,
  ui,
  rec: {
    printing,
    presenting,
    playing,
    printProgress,
    printed,
    presentProgress,
  },
  screen: { width, height },
}) {
  if (presenting) {
    if (!playing) {
      wipe(0, 100).ink(255, 200).write("TAP TO PLAY", { center: "xy " });
    } else {
      wipe(0, 0);
      // ink(0, 255, 255, 200).box( 0, height - 4, width * presentProgress, height - 4); // Present a progress bar.
    }

    // Draw the "Export" button.
    if (!btn)
      btn = new ui.Button({
        x: width - 80 - 6,
        y: height - 20 - 6,
        w: 80,
        h: 20,
      });

    if (printing) {
      const h = 16; // Paint a printing / transcoding progress bar.
      wipe(80, 0, 0, 180)
        .ink(0)
        .box(0, height / 2 - h / 2, width, h)
        .ink(255, 0, 0)
        .box(0, height / 2 - h / 2, printProgress * width, h)
        .ink(255, 200)
        .write("Transcoding...", { center: "xy" });
    } else {
      // Show "Export" (Print) button to transcode and save a video.
      ink(0, 200, 0)
        .box(btn.box, "fill")
        .ink(0, 80, 0)
        .write("Done", btn.box, [0, 40]);
    }
  } else {
    // TODO: Put a little delay on here?
    wipe(40, 0, 0).ink(180, 0, 0).write("NO VIDEO", { center: "xy" });
  }
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, rec, download, serverUpload: upload, num, jump }) {
  if (!rec.printing) {
    let noPrint = true;
    // Print (or download) a video.
    btn?.act(e, () => {
      if (rec.printed) {

        let filename = `whistlegraph-${num.timestamp()}.mp4`;
        download(filename); // Download the video locally.

      } else {
        // TODO: How do I keep track of network upload progress?
        //       Maybe with a callback?

        // Transcode and then upload.
        rec.print(async () => {
          try {
            const uploaded = await upload(".mp4");
            jump(`download ${uploaded.slug}`)

          } catch (e) {
            console.error("Upload failed:", e);
          }
        });
        noPrint = false;
      }
    });
    if (noPrint) {
      if (e.is("touch:1") && !rec.playing) rec.play();
      if (e.is("touch:1") && rec.playing) rec.pause();
    }
  }
}

export { boot, paint, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
