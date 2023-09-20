// BGM, 22.12.07.12.56
// Play indexed background music and run a visualizer.

/* #region 🏁 todo
 - [] Two tracks meant to play together as a musical system.
 - [] Add a clickable play button overlay if bgm is used as a landing page.
  - [] Draw a rasterized, filled triangle for the play button!
  - [] Draw thicker rasterized lines for the visualizer?
    - [] Use ChatGPT to generate the bresenham thickness code again.
 - [] Experiment with a nicer visualizer.
 - [] Radiate lines out from the center?
#endregion */

const trackCount = 17; // See `backgroundTrackURLs` in `bios.mjs`. 

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, bgm, params, num }) {
  params = params.map((str) => parseInt(str)) || 0;
  if (params.length === 0) params[0] = num.randInt(trackCount - 1);
  bgm.set(params[0]);
  wipe(0, 0, 100);
}

// 🎨 Paint (Executes every display frame)
function paint({ line, wipe, bgm, ink, screen }) {
  wipe(bgm.data.amplitude);

  let x = 0;
  bgm.data.sample.forEach((smp) => {
    ink(smp, 0, 0);
    line(x, screen.height, x, screen.height - smp);
    x += 1;
  });
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint };
