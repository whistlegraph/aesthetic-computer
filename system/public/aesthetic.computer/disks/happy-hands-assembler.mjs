// Happy Hands Assembler, 23.04.24.15.02
// Let's a some happy hands!

/* #region 🤝 Read Me 
#endregion */

const hands = 1024; // How many happy hands exist in total?
const key = "happy-hand-assembler:hand"; // Keep track of current hand index.

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ wipe, params, screen, store }) {
  let h = parseInt(params[0]);

  if (isNaN(h)) {
    const stored = await store.retrieve(key, "local:db");
    h = stored;
  }

  if (h === null || isNaN(h) || h === undefined || h < 0 || h > hands - 1) {
    console.warn("👎 Hand Not Found:", h);
    wipe(100, 0, 0)
      .ink(120, 0, 0)
      .line(0, 0, screen.width, screen.height)
      .line(0, screen.height, screen.width, 0);
  } else {
    // 🤚 We have a hand!
    wipe(0, 64, 0)
      .ink(0, 255, 0, 128)
      .write(h, { x: 4, y: screen.height - 13 });
    store[key] = h;
    store.persist(key, "local:db");
  }
}

// 🎨 Paint (Executes every display frame)
function paint($) {}

export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
// ...

/*
// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/
