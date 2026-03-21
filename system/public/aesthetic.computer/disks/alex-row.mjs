// Alex Row, 2021.12.09.18.52
// This disk should generate random pixels of a single row from left to right.

// TODO: Add fps feature. Also add bpm to boot. 2021.12.09.19.09

let x = 0;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize, wipe, cursor }) {
  cursor("none");
  resize(50, 2); // TODO: Make it so that "1" actually works. 2021.12.09.19.00
  wipe(255);
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ paintCount, ink, num: { randIntRange: r } }) {
  if (paintCount % 50 === 0) {
    // Every 100 frames.
    ink(r(100, 200), r(200, 255), r(50, 80)).plot(x, 0);
    x += 1;
  }
}

// 💗 Beat (Runs once per bpm)
//function beat({ sound: { bpm } }) {
//  bpm(80);
//}

// 📚 Library (Useful functions used throughout the program)
// ...

export { boot, paint };
