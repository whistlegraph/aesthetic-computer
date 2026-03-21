// DIGITPAIN 2
// TODO: Preload images by name:

let frameProgress = 0;
let frameTime = 3;
let frame = 0;
const frames = { count: 7, loaded: false, images: [] };

// 🥾 Boot
async function boot({
  net: { waitForPreload, preload, preloaded },
  cursor,
  resize,
  help: { repeat },
}) {
  cursor("native");
  resize(600, 859);

  // Preload all images (by name).
  waitForPreload();
  ["bg1", "bg2", "bg3", "figure", "info", "shine", "signature"].forEach(
    (n, i) => {
      preload(`aesthetic.computer/disks/digitpain/2/${n}.webp`).then(({ img }) => {
        frames.images[i] = img;
        // Set the `loaded` flag if everything is finished.
        frames.loaded = frames.images.reduce((n) => n + 1, 0) === frames.count;
        if (frames.loaded) preloaded();
      });
    }
  );
}

// 🎨 Paint
function paint({ wipe, paste, num: { randIntRange: r }, help: { choose } }) {
  if (frames.loaded) {
    wipe(r(5, 10), 0, r(30, 40)).paste(frames.images[frame], 0, 0);
  }
}

// 🧮 Sim
function sim({ help: { choose } }) {
  if (frames.loaded) {
    frameProgress += 1;
    if (frameProgress >= frameTime) {
      frameProgress = 0;
      frame = (frame + 1) % frames.count;
    }
  }
}

export { boot, paint, sim };
