// Handtime, 2023.7.11.17.06.34
// Communicate manually.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Now 
  - [] Keep a buffer of that line and fade it out so multiple lines can be painted.
  + Later
  - [-] Ignore signal from our id
  + Done
  - [x] Find midpoint between TI and paint it.
  - [x] Start drawing a line when TI are making contact.
  - [x] Stop drawing a line (collecting points) when TI stop making contact.
  - [x] Take the point from the hand class and send them.
  - [x] Keep fuzzy bug / make handInput dummy data the same as real data. 
#endregion */

import { HandInput } from "../lib/hand.mjs";

let myHand, points;
let server;
const otherHands = {};
let drawing = false;
const lines = [];
let currentMark;

// 🥾 Boot
function boot({ net: { socket }, num }) {
  myHand = new HandInput({ num });
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
    const otherPoints = otherHands[id];

    // Get T, I, and the midpoint between T and I, then graph the midpoint.
    const T = otherPoints[myHand.indices.t];
    const I = otherPoints[myHand.indices.i];
    const midpoint = $.num.midp(T, I);

    const dist = $.num.dist(T[0], T[1], I[0], I[1]);
    // console.log(dist, T, I);

    let ti;
    
    myHand.gesture(otherPoints).forEach((interaction) => {
      if (interaction.t && interaction.i) ti = interaction;
    });
    //console.log("1: ", myHand.gesture(otherPoints));

    $.ink(ti ? "pink" : "lime").box(...midpoint, 12, "fill*center");

    for (let j = 0; j < otherPoints.length; j += 1) {
      $.ink(server?.id === id ? "brown" : "red").box(
        otherPoints[j][0],
        otherPoints[j][1],
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
    let [x, y] = points[j];
    if (myHand.dummy) x = y = undefined;

    $.ink("lime").box(x, y, 3, "fill*center");
  }

  const T = points[myHand.indices.t];
  const I = points[myHand.indices.i];

  let ti;
  myHand.interactions.forEach((interaction) => {
    if (interaction.t && interaction.i) ti = interaction;
  });

  // console.log("2: ", myHand.interactions);
  // if (myHand.dummy) drawing = false;

  if (ti && !drawing) {
    drawing = true;
    currentMark = [];
    lines.push(currentMark);
  } else if (!ti && drawing) {
    drawing = false;
    currentMark = undefined;
    // console.log(lines);
  }

  const midpoint = $.num.midp(T, I);
  // TODO: Detect if TI is one of the gestures.
  $.ink(ti ? "blue" : "red").box(...midpoint, 9, "fill*center");

  if (drawing) {
    const lastPoint = currentMark[currentMark.length - 1];
    if (
      !lastPoint ||
      lastPoint[0] !== midpoint[0] ||
      lastPoint[1] !== midpoint[1]
    ) {
      // console.log(midpoint);
      currentMark.push(midpoint); // TODO: This will store duplicates!
    }
  }

  // Loop through every lines.
  lines.forEach((mark) => {
    $.ink("white").poly(mark);
  });
  
}

// 🎪 Act
function act($) {
  myHand.act($);

  // Know when TI makes or breaks contact.
}

// 🧮 Sim
function sim($) {
  points = myHand.sim($);
  if (points) {
    if (myHand.dummy) points = points.map((p) => [undefined, undefined]);
    server.send("handtime:hand", points);
  }
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
