// Video, 23.1.26.12.01
// Play back and be able to export / upload a recorded video.

/* #region ✏️ todo
  + Now
  - [-] Add stamp on export.
  - [] Would I be able hold onto / store the recording similar to a painting
       on the client? So that way refreshing the page can work...
  + Done
  - [x] Add "fuzzy" intro instead of "Get ready..." or a delay or something.
       (Test Pattern)
  - [x] Add "export" or "save" button.
    - [x] Transcode upon tapping export.
      - [x] Reveal download options, based on
           device capability.
  - [x] Factor out / comment or modify the old video overlay UI code.
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

import { EllipsisTicker } from "../lib/ellipsis-ticker.mjs";

let btn;
let isPrinting = false;
let ellipsisTicker;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, rec, gizmo, jump, notice }) {
  if (rec.recording) {
    notice("TAPING", ["yellow", "red"]);
    jump("prompt");
    return;
  }
  wipe(0);
  rec.present(); // Visually present a recording right away if one exists.
  ellipsisTicker = new EllipsisTicker(gizmo.Hourglass);
}

// 🎨 Paint (Executes every display frame)
function paint({
  api,
  wipe,
  ink,
  ui,
  help,
  rec: { presenting, playing, printProgress, presentProgress },
  screen,
  paintCount,
}) {
  if (presenting) {
    if (!playing) {
      wipe(0, 100).ink(255, 200).write("||", { center: "xy " });
    }

    if (presentProgress) {
      if (playing) wipe(0, 0);
      ink(0).box(0, screen.height - 1, screen.width, screen.height - 1);
      ink(playing ? undefined : 64).box(
        0,
        screen.height - 1,
        screen.width * presentProgress,
        screen.height - 1,
      ); // Present a progress bar.
    }

    if (isPrinting) {
      const h = 16; // Paint a printing / transcoding progress bar.
      let text = "PROCESSING";
      text += ellipsisTicker.text(help.repeat);
      wipe(0, 0, 80, 180)
        .ink(0)
        .box(0, screen.height / 2 - h / 2, screen.width, h)
        .ink(255, 0, 0)
        .box(0, screen.height / 2 - h / 2, printProgress * screen.width, h)
        .ink(255, 200)
        .write(text, { center: "xy" });
    } else {
      // Show "Download" button which transcodes and saves a video.
      if (!btn)
        btn = new ui.TextButton("Download", { right: 6, bottom: 6, screen });
      btn.reposition({ right: 6, bottom: 6, screen });
      btn.paint(api);
    }
  } else if (paintCount > 16n) {
    wipe(40, 0, 0).ink(180, 0, 0).write("NO VIDEO", { center: "xy" });
  }
}

function sim() {
  ellipsisTicker?.sim();
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, rec, download, num, jump }) {
  if (!rec.printing) {
    // Download or print (render) a video.
    btn?.act(e, {
      push: () => {
        download(`tape-${num.timestamp()}.mp4`);
        // Transcode and then upload.
        // isPrinting = true;
        // rec.print(async () => {
        // });
        // let uploaded;
        // try {
        //   uploaded = await upload(".mp4");
        // } catch (e) {
        //   console.error("Upload failed:", e);
        //   uploaded = { slug: "local" };
        // }
      },
    });

    if (!btn?.down) {
      if (e.is("touch:1") && !rec.playing) rec.play();
      if (e.is("touch:1") && rec.playing) rec.pause();
    }
  }
}

export { boot, paint, sim, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
