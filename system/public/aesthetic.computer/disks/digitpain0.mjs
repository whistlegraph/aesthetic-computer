// DIGITPAIN, 2022.04.08.22.52
// This piece currently hosts only DIGITPAIN 0 but will eventually be added to
// and host every DIGITPAIN picture.

// All ***DIGITPAIN TIMESIGS*** and titles are to be entered below, in order:
// DIGITPAIN 0: 2022.04.08.22.55

let img1OriginalPixels, img2OriginalPixels;
let img1, img2;

let thaumaTime = 0;
let thaumaMax = 3;

let swapTime = 0;
const swapMax = 0;
let swapCount = 0;
let needSwap = false;

let needsFlip = false;
let flip = true;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({
  wipe,
  net: { waitForPreload, preload, preloaded },
  cursor,
  fps,
  resolution,
  glaze,
}) {
  resolution(1000, 1250); // 3x5
  cursor("native");

  waitForPreload();

  preload("aesthetic.computer/disks/digitpain/0/0.webp").then((img) => {
    img1 = img;
    img1OriginalPixels = img.pixels.slice();
    if (img1 && img2) preloaded();
  });

  preload("aesthetic.computer/disks/digitpain/0/1.webp").then((img) => {
    img2 = img;
    img2OriginalPixels = img.pixels.slice();
    if (img1 && img2) preloaded();
  });

  // glaze({ on: true, type: "digitpain0" });
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim({ help: { choose } }) {
  if (img1 && img2) {
    thaumaTime += 1;
    if (thaumaTime > thaumaMax) {
      thaumaTime = 0;
      thaumaMax = choose(1, 2, 3, 4, 5);
      if (Math.random() > 0.995) {
        thaumaMax = choose(10, 20, 30, 60);
      }
      needsFlip = true;
    }
    swapTime += 1;
    if (swapTime >= swapMax) {
      swapTime = 0;
      needSwap = true;
    }
  }
}

// 🎨 Paint (Executes every display frame)
function paint({
  wipe,
  page,
  ink,
  num: { randIntRange: r },
  help: { choose },
  screen,
  paintCount,
  noise16DIGITPAIN,
}) {
  // Swap a pixel from both layers, every frame.
  if (needSwap) {
    for (let n = 0; n < choose(r(32, 64), r(128, 256)); n += 1) {
      const x = r(0, screen.width);
      const y = r(0, screen.height);
      const i = (x + y * screen.width) * 4;

      const img2p = [
        img2.pixels[i] * (1 + Math.random()),
        img2.pixels[i + 1],
        img2.pixels[i + 2],
        img2.pixels[i + 3],
      ];

      const img1p = [
        img1.pixels[i] * (1 + Math.random()),
        img1.pixels[i + 1],
        img1.pixels[i + 2],
        img1.pixels[i + 3],
      ];

      page(img1);
      ink(img2p).plot(x, y);

      page(img2);
      ink(img1p).plot(x, y);

      page(screen);
    }

    swapCount += 1;

    if (swapCount >= 2000) {
      swapCount = 0;
      if (choose(0, 1) === 0) {
        img1.pixels = img1OriginalPixels.slice();
      } else {
        img2.pixels = img2OriginalPixels.slice();
      }
    }
    needSwap = false;
  } else if (img1 === undefined || img2 === undefined) {
    wipe(choose(5, 15), 0, 0);
  }

  if (needsFlip) {
    if (img1 && img2) {
      if (flip) {
        wipe(
          r(0, choose(6, 6, 6, 12, 12, 12, 12, 48, 80)),
          r(0, 12),
          r(0, 12)
        ).paste(img1, choose(0, r(-2, 2)), choose(0, r(-2, 2)));
      } else {
        wipe(r(0, 12), r(0, 12), r(0, 12)).paste(
          img2,
          choose(0, r(-4, 4)),
          choose(0, r(-2, 2))
        );
      }
    }
    flip = !flip;
    needsFlip = false;
  }
}

// ✒ Act (Runs once per user interaction)
// function act({ event }) {}

// 💗 Beat (Runs once per bpm)
// function beat($api) {
// TODO: Play a sound here!
// }

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, sim, paint };
