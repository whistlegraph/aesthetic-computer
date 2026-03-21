// Selfie, 23.02.17.18.54
// Palette for making a decorated, photographic selfie. 
// Designed w/ @mollysoda.

/* #region 🚴 TODO 
- [] Add conditional button to camera in the corner that jupms to 
     `selfie` if we came from `mood`.
 - [] Reset buttton moves to bottom right corner as arrow to
      match the Decorate button.
 - [] Reset confirmation.

- [] Menu similar to `valbear`.
  - [] Pull / Move
  - [] Camera - color param
  - [x] Text - Have a bank of changing words
  - [x] "Save" / "Download" / "Done" 
  - [x] Icon 
  - [x] Sparkle 
#endregion */

// Buttons

// brect, wrect
// bline, wline
// smear

let shape, icon, status, glitter, pull, download, undo, reset, discord;

// 🥾 Boot (Runs once before first paint and sim)
function boot() {}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, system, ink, ui: { TextButton: TB }, screen }) {
  const y = 25;
  const x = 6;

  wipe(0).paste(system.painting); // Painting

  // Title
  ink(20, 200)
    .box(0, 0, screen.width, screen.height)
    .ink(0, 0, 200, 255)
    .write("Decorate a Selfie", { x, y }, [200, 150]);

  // Buttons
  let ys = 20;
  let xs = 6;
  const gap = 4;
  const g2 = gap * 2;

  shape = new TB("Shape", { x: x + xs, y: y + ys });
  shape.paint({ ink });

  icon = new TB("Icon", { x: shape.btn.box.x + shape.btn.box.w + g2, y: y + ys });
  icon.paint({ ink }, [[64], [127, 150], [200], [50]]);


  status = new TB("Status", { x: icon.btn.box.x + icon.btn.box.w + g2, y: y + ys });
  status.paint({ ink }, [[64], [127, 150], [200], [50]]);

  ys += 28;

  glitter = new TB("Glitter", { x: x + xs, y: y + ys });
  glitter.paint({ ink }, [[64], [127, 150], [200], [50]]);

  pull = new TB("Copy", { x: glitter.btn.box.x + glitter.btn.box.w + g2, y: y + ys });
  pull.paint({ ink }, [[64], [127, 150], [200], [50]]);

  ys += 54;

  ink(255, 255, 0, 100).write("(try each, in order)", {
    x: xs + 4,
    y: ys - 2,
  });

  let by = 15;
  ink(255, 255, 0, 100).line(0, ys + by, screen.width, ys + by);

  // Download
  download = new TB("Download", { x: x + xs, y: y + ys });
  download.paint({ ink }, [[40], [180], [200], [20]]);

  ys += 28;

  // Undo
  undo = new TB("Undo", { x: x + xs, y: y + ys });
  undo.paint({ ink }, [
    [20, 0, 0],
    [120, 60, 0],
    [200, 100, 0],
    [10, 0, 0],
  ]);

  // Reset
  reset = new TB("Reset", {
    x: undo.btn.box.x + undo.btn.box.w + g2,
    y: y + ys,
  });
  reset.paint({ ink }, [
    [40, 0, 0],
    [180, 0, 0],
    [200, 0, 0],
    [20, 0, 0],
  ]);

  ys += 28;

  // Help
  discord = new TB("Help", { x: x + xs, y: y + ys });
  discord.paint({ ink }, [
    [0, 0, 40],
    [0, 0, 180],
    [80, 80, 255],
    [0, 0, 20],
  ]);

  return false;
}

// ✒ Act (Runs once per user interaction)
function act(
  {
    system,
    store,
    needsPaint,
    event: e,
    jump,
    num: { randIntRange: rr, timestamp },
    download: dl,
    help,
  },
  num
) {
  shape.btn.act(e, () => jump(`shape`));
  icon.btn.act(e, () => jump(`icon`));

  status.btn.act(e, () => {
    const scale = rr(1, 3);
    jump(
      `word:${scale}:${scale} ${help.choose(
        "happy",
        "SMILE",
        "angel ;)",
        "sexxxy",
        "DEPRESSED",
        "horny?",
      )}`
    );
  });

  glitter.btn.act(e, () => jump(`sparkle`));

  download.btn.act(e, () => {
    if (store["painting"]) {
      dl(`selfie-${timestamp()}.png`, store["painting"], {
        scale: 2,
        cropToScreen: true,
      });
    }
  });

  const api = { system, store, needsPaint };
  undo.btn.act(e, () => system.nopaint.no(api));
  reset.btn.act(e, async () => jump(`mood`));
  discord.btn.act(e, () => jump("https://discord.gg/aesthetic-computer"));
}

function meta() {
  return { desc: 'Palette for decorating a mood selfie.' };
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint, act, meta };