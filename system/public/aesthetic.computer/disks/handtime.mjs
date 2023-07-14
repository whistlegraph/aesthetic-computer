// Handtime, 2023.7.11.17.06.34
// Communicate manually.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Ignore signal from our id
  + Done
  - [x] Take the point from the hand class and send them.
#endregion */

import { HandInput } from "../lib/hand.mjs";

let myHand, points;
let server;
const otherHands = {};

// 🥾 Boot
function boot({ net: { socket } }) {
  myHand = new HandInput();
  server = socket((id, type, content) => {
    if (type === "handtime:hand") otherHands[id] = content;
  });
}

// 🎨 Paint
function paint($) {
  $.wipe(64);
  myHand.paint($, { hidden: true }); // Instantiate but never draw the hand.

  // Draw all remote points.
  const keys = Object.keys(otherHands);
  for (let i = 0; i < keys.length; i += 1) {
    const id = keys[i];
    const points = otherHands[id];
    for (let j = 0; j < points.length; j += 1) {
      $.ink((server?.id) === id ? "brown" : "red").box(
        points[j][0],
        points[j][1],
        5,
        "fill*center"
      );
      delete otherHands[id]; // Consume the points.
      // ^ TODO: This will cause a flicker, therefore should go into
      //         a temporary cache that gets deleted after a healthy timeout
      //         duration if there are no updates.
    }
  }

  // Draw local points on top.
  for (let j = 0; j < points?.length; j += 1) {
    $.ink("lime").box(points[j][0], points[j][1], 3, "fill*center");
  }
}

// 🎪 Act
function act($) {
  myHand.act($);
}

// 🧮 Sim
function sim($) {
  points = myHand.sim($);
  if (points) server.send("handtime:hand", points);
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
