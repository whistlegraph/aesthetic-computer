// Valbear, 23.02.13.21.15
// Make your valentine a card with a cute bear!

/* #region 🚴 TODO 
  + Cleanup
  - [] Abstract basic button code so it's not redundant.
  - [] Abstract 'no' and 'no!' code so it's not redundant.
  + Done
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
function paint({ wipe, system, ink, num, ui: { Button }, screen }) {
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
  const offset = { x: gap, y: gap };
  const h = 19;
  const cw = 6; // Character width.

  // Grass
  const gTxt = "Grass";
  const gw = gTxt.length * cw + g2;
  grass = new Button({ x: x + xs, y: y + ys, w: gw, h });
  ink(0, 100, 0)
    .box(grass.box, "fill")
    .ink(0, 255, 0, 150)
    .box(grass.box, "outline")
    .ink(0, 200, 0)
    .write(gTxt, num.p2.add(grass.box, offset), [0, 50, 0]);

  // Sky
  const sTxt = "Sky";
  sky = new Button({
    x: x + xs + gw + g2,
    y: y + ys,
    w: sTxt.length * cw + g2,
    h,
  });
  ink(0, 0, 100)
    .box(sky.box, "fill")
    .ink(0, 0, 255, 150)
    .box(sky.box, "outline")
    .ink(100, 100, 255)
    .write(sTxt, num.p2.add(sky.box, offset), [0, 0, 50]);

  // Smear
  const smTxt = "Smear";
  smear = new Button({
    x: sky.box.x + sky.box.w + g2,
    y: y + ys,
    w: smTxt.length * cw + g2,
    h,
  });
  ink(64)
    .box(smear.box, "fill")
    .ink(127, 150)
    .box(smear.box, "outline")
    .ink(200)
    .write(smTxt, num.p2.add(smear.box, offset), [50]);

  ys += 28;

  // Bear Body
  const bTxt = "Bear Body";
  body = new Button({
    x: x + xs,
    y: y + ys,
    w: bTxt.length * cw + g2,
    h,
  });
  ink(100, 50, 0)
    .box(body.box, "fill")
    .ink(250, 150, 10)
    .box(body.box, "outline")
    .ink(100, 50, 0)
    .write(bTxt, num.p2.add(body.box, offset), [255, 100, 0]);

  // Lines
  const lTxt = "Detail Lines";
  lines = new Button({
    x: body.box.x + body.box.w + g2,
    y: y + ys,
    w: lTxt.length * cw + g2,
    h,
  });
  ink(127)
    .box(lines.box, "fill")
    .ink(200)
    .box(lines.box, "outline")
    .ink(100)
    .write(lTxt, num.p2.add(lines.box, offset), [200]);

  ys += 28;

  // Heart
  const hTxt = "Freehand Heart";
  heart = new Button({
    x: x + xs,
    y: y + ys,
    w: hTxt.length * cw + g2,
    h,
  });
  ink(100, 0, 0)
    .box(heart.box, "fill")
    .ink(250, 0, 0)
    .box(heart.box, "outline")
    .ink(100, 0, 0)
    .write(hTxt, num.p2.add(heart.box, offset), [255, 0, 0]);

  // Trail
  const htTxt = "<3 Trail";
  trail = new Button({
    x: heart.box.x + heart.box.w + g2,
    y: y + ys,
    w: htTxt.length * cw + g2,
    h,
  });
  ink(100, 80, 80)
    .box(trail.box, "fill")
    .ink(250, 200, 200)
    .box(trail.box, "outline")
    .ink(100, 90, 90)
    .write(htTxt, num.p2.add(trail.box, offset), [255, 200, 200]);

  ys += 28;

  // Words
  const wTxt = "Sweet Nothings";
  words = new Button({
    x: x + xs,
    y: y + ys,
    w: wTxt.length * cw + g2,
    h,
  });
  ink(50)
    .box(words.box, "fill")
    .ink(100)
    .box(words.box, "outline")
    .ink(50)
    .write(wTxt, num.p2.add(words.box, offset), [127]);

  ys += 54;
  ink(255, 255, 0, 100).write("(try each, in order)", {
    x: xs + 4,
    y: ys - 2,
  });

  let by = 15;
  ink(255, 255, 0, 100).line(0, ys + by, screen.width, ys + by);

  // Download
  const dTxt = "Download Valentine";
  download = new Button({
    x: x + xs,
    y: y + ys,
    w: dTxt.length * cw + g2,
    h,
  });
  ink(40)
    .box(download.box, "fill")
    .ink(180)
    .box(download.box, "outline")
    .ink(200)
    .write(dTxt, num.p2.add(download.box, offset), [20]);

  ys += 28;

  // Undo
  const uTxt = "Undo";
  undo = new Button({
    x: x + xs,
    y: y + ys,
    w: uTxt.length * cw + g2,
    h,
  });
  ink(20, 0, 0)
    .box(undo.box, "fill")
    .ink(120, 60, 0)
    .box(undo.box, "outline")
    .ink(200, 100, 0)
    .write(uTxt, num.p2.add(undo.box, offset), [10, 0, 0]);

  // Reset
  const rTxt = "Reset";
  reset = new Button({
    x: undo.box.x + undo.box.w + g2,
    y: y + ys,
    w: rTxt.length * cw + g2,
    h,
  });
  ink(40, 0, 0)
    .box(reset.box, "fill")
    .ink(180, 0, 0)
    .box(reset.box, "outline")
    .ink(200, 0, 0)
    .write(rTxt, num.p2.add(reset.box, offset), [20, 0, 0]);

  ys += 28;

  // Help
  const heTxt = "Help";
  discord = new Button({
    x: x + xs,
    y: y + ys,
    w: heTxt.length * cw + g2,
    h,
  });
  ink(0, 0, 40)
    .box(discord.box, "fill")
    .ink(0, 0, 180)
    .box(discord.box, "outline")
    .ink(80, 80, 255)
    .write(heTxt, num.p2.add(discord.box, offset), [0, 0, 20]);

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
  grass.act(e, () => jump(`rect 0 ${rr(100, 200)} 0`));
  sky.act(e, () => jump(`rect 150 ${rr(100, 150)} ${rr(200, 255)}`));
  smear.act(e, () => jump(`smear ${rr(4, 32)}`));
  body.act(e, () => {
    help.flip() ? jump("oval 150-200 60-100 10-40 230-255") : jump("oval");
  });
  lines.act(e, () => jump(`line:${rr(1, 4)} ? ? ?`));

  heart.act(e, () =>
    jump(`shape ${rr(100, 255)} 0 ${rr(20, 255)}, ${(rr, 200, 255)}`)
  );

  trail.act(e, () => jump(`sparkle`));

  words.act(e, () => {
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

  download.act(e, () => {
    if (store["painting"]) {
      dl(`valentine-${timestamp()}.png`, store["painting"], {
        scale: 6,
        cropToScreen: true,
      });
    }
  });

  undo.act(e, () => {
    // Ripped straight from prompt!
    const paintings = system.nopaint.undo.paintings;

    if (paintings.length > 1) {
      // Copy over the old picture here...
      const p = paintings[paintings.length - 2];
      const op = p.pixels;
      const pixels = new Uint8ClampedArray(op.length);
      pixels.set(op);

      store["painting"] = {
        width: p.width,
        height: p.height,
        pixels,
      };

      // Swap mode.
      // 'no' should swap...
      const temp = paintings[0];
      paintings[0] = paintings[1];
      paintings[1] = temp;

      // Rewind mode
      //paintings.length -= 1;

      store.persist("painting", "local:db");

      system.painting = store["painting"];
      needsPaint();
    }
  });

  reset.act(e, async () => {
    await store.delete("painting", "local:db");
    system.nopaint.undo.paintings.length = 0; // Reset undo stack.

    system.painting = null;

    needsPaint();
  });

  discord.act(e, () => jump("https://discord.gg/aesthetic-computer"));
}

function meta() {
  return { desc: "Make your valentine a card with a cute bear!" };
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint, act, meta };
