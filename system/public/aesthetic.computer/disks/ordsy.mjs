// Ordsy, 23.02.17.18.54
// Palette for making a black + white "ordsy" picture.

/* #region 🚴 TODO 
  + Done
  - [] Make some test / sketch BW drawings tonight while out. 23.02.17.18.58
  - [-] Factor out `valbear` stuff / make a new menu wiht basic features.
  - [] Add good signature option.
  - [] Add option to print sat name.
  - [] Upload the image & associate it with a user account if one exists.
#endregion */

// Buttons

// brect, wrect
// bline, wline
// smear

let brect, wrect, smear, download, undo, reset, discord;

// 🥾 Boot (Runs once before first paint and sim)
function boot() {}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, system, ink, ui: { TextButton: TB }, screen }) {
  const y = 25;
  const x = 6;

  wipe(0).paste(system.painting); // Painting

  // Title
  ink(100, 20, 20, 150)
    .box(0, 0, screen.width, screen.height)
    .ink(255, 0, 0, 255)
    .write("Make an Ordsy", { x, y }, [0, 0, 0, 255]);

  // Buttons
  let ys = 20;
  let xs = 6;
  const gap = 4;
  const g2 = gap * 2;

  // Grass
  brect = new TB("Grass", { x: x + xs, y: y + ys });
  wrect.paint({ ink });

  // Smear
  smear = new TB("Smear", { x: sky.btn.box.x + sky.btn.box.w + g2, y: y + ys });
  smear.paint({ ink }, [[64], [127, 150], [200], [50]]);

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
  brect.btn.act(e, () => jump(`rect 0 ${rr(100, 200)} 0`));
  wrect.btn.act(e, () => jump(`rect 150 ${rr(100, 150)} ${rr(200, 255)}`));
  smear.btn.act(e, () => jump(`smear ${rr(4, 32)}`));

  download.btn.act(e, () => {
    if (store["painting"]) {
      dl(`ordsy-${timestamp()}.png`, store["painting"], {
        scale: 2,
        cropToScreen: true,
      });
    }
  });

  const api = { system, store, needsPaint };
  undo.btn.act(e, () => system.nopaint.no(api));
  reset.btn.act(e, async () => system.nopaint.noBang(api));
  discord.btn.act(e, () => jump("https://discord.gg/aesthetic-computer"));
}

function meta() {
  return { desc: 'Palette for making a black + white "ordsy" picture.' };
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint, act, meta };