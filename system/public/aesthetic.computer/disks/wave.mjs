// Happy Hands Assembler, 23.04.24.15.02
// Let's make a some happy hands!
// Coded by Jeffrey Alan Scudder & Tina Tarighian

/* #region 🏁 todo
  + Done
  - [x] We need a goal. (handtime, handprint, staka)
#endregion */

/* #region 🤝 Read Me 
#endregion */

import { HandInput } from "../lib/hand.mjs";

let h; // Current working hand.
const hands = 1024; // How many happy hands exist in total?
const key = "happy-hand-assembler:hand"; // Keep track of current hand index.
let handInput;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ wipe, params, screen, store }) {
  h = parseInt(params[0]);
  if (isNaN(h)) {
    // Try to receive last edited hand.
    const stored = await store.retrieve(key, "local:db");
    h = stored;
  }

  if (h === null || isNaN(h) || h < 0 || h > hands - 1) {
    console.warn("👎 `Happy Hand` Not Found:", h);
    wipe(100, 0, 0)
      .ink(120, 0, 0)
      .line(0, 0, screen.width, screen.height)
      .line(0, screen.height, screen.width, 0); // Draw a big red X.
  } else {
    // 🤚 We have a hand!
    wipe(0, 64, 0)
      .ink(0, 255, 0, 128)
      .write(h, { x: 4, y: screen.height - 13 }); // Drawing number of hand.
    store[key] = h; // Store and persist the current hand across page refreshes.
    store.persist(key, "local:db");
  }

  handInput = new HandInput();
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  const { wipe, ink, screen: { height } } = $;
  wipe(0);
  handInput.paint($);
  ink(0, 255, 0, 128).write(h, { x: 4, y: height - 13 }); // Print hand index.
}

// ✒ Act (Runs once per user interaction)
function act($) {
  handInput.act($);
}

function sim($) {
  handInput.sim($); // Calculate the hand points.
}

// Tab title and meta description of this piece.
function meta() {
  return {
    title: "Happy Hands Assembler",
    desc: "Get ready for some happy hands!",
  };
}

export { boot, paint, act, meta, sim};

// 📚 Library (Useful functions used throughout the piece)
