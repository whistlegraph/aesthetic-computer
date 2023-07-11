// Handtime, 2023.7.11.17.06.34
// Communicate manually.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Take the point from the hand class and send them.
  - [] Ignore signal from our id
#endregion */

import { HandInput } from "../lib/hand.mjs";

// 🥾 Boot
let handInput;
let server;
let remoteHandPoints;
async function boot({ net: { socket } }) {
  handInput = new HandInput();

  server = await socket((id, type, content) => {
    console.log("Our id:", server.id);
    console.log("Got new message:", id, type, content);
    // if (server.id !== id && type === "handtime:hand") {
    remoteHandPoints = content;
    // }
  });
}

// 🎨 Paint
function paint($) {
  $.wipe(0);
  handInput.paint($);

  // Draw remote points using boxes in a loop.
  if (remoteHandPoints) {
    for (let i = 0; i < remoteHandPoints.length; i += 1){
      $.ink("red").box(remoteHandPoints[i][0], remoteHandPoints[i][1], 5, "fill*center"); 
    }
  }

}

// 🎪 Act
function act($) {
  handInput.act($);
}

// 🧮 Sim
function sim($) {
  const points = handInput.sim($);
  server.send("handtime:hand", points);
}

// 📰 Meta
function meta() {
  return {
    title: "Handtime",
    desc: "Communicate manually.",
  };
}

export { boot, paint, sim, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
