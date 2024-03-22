// Image IO, 22.09.30.10.05
//  🅰️ Test for loading images from URLs and pasting them.
//  🅱️️ Test for saving images from buffers.

let image;
let imgToExport;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resolution, net, needsPaint, painting, dowwnload }) {
  // 🅰️
  // net.waitForPreload();
  // net.preload(`aesthetic.computer/disks/digitpain/1/${0}.webp`).then((img) => {
  //   image = img;
  //   net.preloaded();
  //   needsPaint();
  // });

  // 🅱️
  resolution(256, 256);
  imgToExport = painting(256, 256, ({noise16DIGITPAIN}) => noise16DIGITPAIN());
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // TODO: Move a ball here!
  //console.log($api);
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, paste }) {
  // 🅰️
  //if (image) wipe(128).paste(image, 0, 0);

  // 🅱️
  //wipe(128); // Draw a gray background
  paste(imgToExport);
  return false; // Only once.
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, download }) {
  if (e.is("keyboard:down")) {
    download("noise.png", imgToExport);
  }
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
