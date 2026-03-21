// Valbear, 23.02.13.21.15
// Make your valentine a card with a cute bear!

/* #region 🚴 TODO 
  + Done
  - [x] Abstract 'no' and 'no!' so it's not redundant with the prompt.
  - [x] Abstract basic button code so it's not redundant.
  - [x] Add every button / wire up all the actions!
  - [x] Add meta function!
  - [x] Test on some different screen sizes / make it rly compact!
#endregion */

// Buttons
let grass,
  sky,
  smear,
  body,
  lines,
  heart,
  trail,
  words,
  download,
  undo,
  reset,
  discord;

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
    .write("Paint a Valentine Bear!", { x, y }, [0, 0, 0, 255]);

  // Buttons
  let ys = 20;
  let xs = 6;
  const gap = 4;
  const g2 = gap * 2;

  // Grass
  grass = new TB("Grass", { x: x + xs, y: y + ys });
  grass.paint({ ink });

  // Sky
  sky = new TB("Sky", { x: x + xs + grass.btn.box.w + g2, y: y + ys });
  sky.paint({ ink }, [
    [0, 0, 100],
    [0, 0, 255, 150],
    [100, 100, 255],
    [0, 0, 50],
  ]);

  // Smear
  smear = new TB("Smear", { x: sky.btn.box.x + sky.btn.box.w + g2, y: y + ys });
  smear.paint({ ink }, [[64], [127, 150], [200], [50]]);

  ys += 28;

  // Bear Body
  body = new TB("Bear Body", { x: x + xs, y: y + ys });
  body.paint({ ink }, [
    [100, 50, 0],
    [250, 150, 10],
    [100, 50, 0],
    [255, 100, 0],
  ]);

  // Lines
  lines = new TB("Detail Lines", {
    x: body.btn.box.x + body.btn.box.w + g2,
    y: y + ys,
  });
  lines.paint({ ink }, [[127], [200], [100], [200]]);

  ys += 28;

  // Heart
  heart = new TB("Freehand Heart", { x: x + xs, y: y + ys });
  heart.paint({ ink }, [
    [100, 0, 0],
    [250, 0, 0],
    [100, 0, 0],
    [255, 0, 0],
  ]);

  // Trail
  trail = new TB("<3 Trail", {
    x: heart.btn.box.x + heart.btn.box.w + g2,
    y: y + ys,
  });
  trail.paint({ ink }, [
    [100, 80, 80],
    [250, 200, 200],
    [100, 90, 90],
    [255, 200, 200],
  ]);

  ys += 28;

  // Words
  words = new TB("Sweet Nothings", { x: x + xs, y: y + ys });
  words.paint({ ink }, [[50], [100], [50], [127]]);

  ys += 54;

  ink(255, 255, 0, 100).write("(try each, in order)", {
    x: xs + 4,
    y: ys - 2,
  });

  let by = 15;
  ink(255, 255, 0, 100).line(0, ys + by, screen.width, ys + by);

  // Download
  download = new TB("Download Valentine", { x: x + xs, y: y + ys });
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
  grass.btn.act(e, () => jump(`rect 0 ${rr(100, 200)} 0`));
  sky.btn.act(e, () => jump(`rect 150 ${rr(100, 150)} ${rr(200, 255)}`));
  smear.btn.act(e, () => jump(`smear ${rr(4, 32)}`));
  body.btn.act(e, () => {
    help.flip() ? jump("oval 150-200 60-100 10-40 230-255") : jump("oval");
  });
  lines.btn.act(e, () => jump(`line:${rr(1, 4)} ? ? ?`));

  heart.btn.act(e, () =>
    jump(`shape ${rr(100, 255)} 0 ${rr(20, 255)}, ${(rr, 200, 255)}`)
  );

  trail.btn.act(e, () => jump(`sparkle`));

  words.btn.act(e, () => {
    const scale = rr(1, 3);
    jump(
      `word:${scale}:${scale} ${help.choose(
        "let's be marry?",
        "happy V day!",
        "ur so cute",
        "u B mine?",
        "i <3 u",
        "XOXO",
        "CUTIE",
        "awe",
        "Meditate w/ Me?",
        "WTF UR A GOD",
        "facetime ME!!!",
        "let's smash"
      )}`
    );
  });

  download.btn.act(e, () => {
    if (store["painting"]) {
      dl(`valbear-${timestamp()}.png`, store["painting"], {
        scale: 6,
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
  return { desc: "Make your valentine a card with a cute bear!" };
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint, act, meta };
