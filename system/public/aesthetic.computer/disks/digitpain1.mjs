// DIGITPAIN 1

let frameProgress = 0;
let frameTime = 3;
let frame = 0;
const frames = { count: 4, loaded: false, images: [] };

// 🥾 Boot
async function boot({
  net: { waitForPreload, preload, preloaded },
  cursor,
  resolution,
  help: { repeat },
}) {
  cursor("native");
  resolution(800, 1145);
  // Preload all images.
  waitForPreload();
  repeat(frames.count, (n) => {
    preload(`aesthetic.computer/disks/digitpain/1/${n}.webp`).then(({ img }) => {
      frames.images[n] = img;
      // Set the `loaded` flag if everything is finished.
      frames.loaded = frames.images.reduce((n) => n + 1, 0) === frames.count;
      if (frames.loaded) preloaded();
    });
  });
}

// 🎨 Paint
function paint({ wipe, paste, num: { randIntRange: r }, help: { choose } }) {
  if (frames.loaded) {
    wipe(r(0, 15), r(0, 15), r(0, 15)).paste(frames.images[frame], 0, 0);
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
