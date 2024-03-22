// Music 2 Whistlegraph 2, 22.12.16.03.18
// Plays and visualizes tracks from Charlie's Whistlegraph instrumental album.

/* #region 🏁 todo
  - [] Actually link the content of this music up to `bgm` and remove this file. 
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
