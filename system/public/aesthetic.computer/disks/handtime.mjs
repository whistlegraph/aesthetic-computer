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
let points;
let server;
let remoteHandPoints = {};
async function boot({ net: { socket } }) {
  handInput = new HandInput();

  server = await socket((id, type, content) => {
    console.log("Our id:", server.id);
    console.log("Got new message:", id, type, content);
    remoteHandPoints[id] = content;
  });
}

// 🎨 Paint
function paint($) {
  $.wipe(64);
  handInput.paint($, { hidden: true }); // Instantiate but never draw the hand.

  // Draw all remote points.
  const keys = Object.keys(remoteHandPoints);
  if (remoteHandPoints) {
    for (let i = 0; i < keys.length; i += 1){
      const points = remoteHandPoints[keys[i]];
      for (let j = 0; j < points.length; j += 1){
        $.ink("red").box(points[j][0], points[j][1], 5, "fill*center"); 
      }
    }
  }

  // Draw local points on top.
  for (let j = 0; j < points.length; j += 1){
    $.ink("lime").box(points[j][0], points[j][1], 3, "fill*center"); 
  }
}

// 🎪 Act
function act($) {
  handInput.act($);
}

// 🧮 Sim
function sim($) {
  points = handInput.sim($);
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
