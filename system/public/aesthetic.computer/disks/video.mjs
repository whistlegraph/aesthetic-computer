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
  ellipsisTicker = new gizmo.EllipsisTicker();
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
    if (playing) wipe(0, 0);

    if (presentProgress) {
      ink(0).box(0, screen.height - 1, screen.width, screen.height - 1);
      ink(playing ? "red" : 64).box(
        0,
        screen.height - 1,
        screen.width * presentProgress,
        screen.height - 1,
      ); // Present a progress bar.
    }

    if (!playing) {
      wipe(0, 100).ink(255, 200).write("||", { center: "xy " });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }

    if (isPrinting) {
      const h = 16; // Paint a printing / transcoding progress bar.
      let text = "PROCESSING";
      text += ellipsisTicker.text(help.repeat);

      let barWidth = printProgress * screen.width;

      if (printProgress > 0.98 && screen.width - barWidth >= 1) {
        barWidth = screen.width;
      }

      wipe(0, 0, 80, 180)
        .ink(0)
        .box(0, screen.height / 2 - h / 2, screen.width, h)
        .ink(0, 0, 255)
        .box(0, screen.height / 2 - h / 2, barWidth, h)
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

let printed = false;

// ✒ Act (Runs once per user interaction)
function act({ event: e, rec, download, num, jump, sound: { synth } }) {
  if (!rec.printing) {
    // Download or print (render) a video.
    btn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 600,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: () => {
        if (!printed) {
          isPrinting = true;
          btn.disabled = true;
          rec.print(() => {
            printed = true;
            btn.disabled = false;
            isPrinting = false;
          });
        } else {
          download(`tape-${num.timestamp()}.mp4`);
        }
        synth({
          type: "sine",
          tone: 800,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    if (!btn?.down && !btn?.disabled) {
      if (e.is("touch:1") && !rec.playing) rec.play();
      if (e.is("touch:1") && rec.playing) rec.pause();
    }
  }
}

export { boot, paint, sim, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
