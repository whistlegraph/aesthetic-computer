// Export, 2022.01.17.13.22
// A test disk for exporting to IPFS.

// Export TODO: Generate a statically runnable zip package of this disk that
//       successfully runs on IPFS (with boot and paint).
//       See also: https://www.fxhash.xyz/articles/guide-mint-generative-token
//       Then test sound and other functionality!

// 🥾 Boot (Runs once before first paint and sim)
function boot({ cursor, wipe, net: { socket } }) {
  cursor("none");
  wipe(0, 30, 80);
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ ink, num: { randInt: r, randIntArr: rA }, screen }) {
  ink(...rA(255, 4)).plot(r(screen.width), r(screen.height));
}

// ✒ Act (Runs once per user interaction)
// function act({ event }) { }

// 💗 Beat (Runs once per bpm)
// function beat($api) { }

// 🧮 Simulate (Runs once per logic frame (120fps)).
// function sim($api) { }

// 📚 Library (Useful functions used throughout the program)
// ...

export { boot, paint };
