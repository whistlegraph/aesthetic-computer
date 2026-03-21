// Handtime, 2023.7.11.17.06.34
// Communicate manually.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Now 
  - [] Either check for an existing connection or listen
       for "connected" message / (race condition on connected)/
  - [] Keep a buffer of that line and fade it out so multiple lines can be painted.
  - [] Add usernames to hands 
  - [] Make all the "others" different colors  
  - [] Choose colors somehow, maybe at beginning or during 
  - [] Support for line thickness 
  + Later
  - [-] Ignore signal from our id
  - [] Leave messages from timeout of data from useres 
  + Done
  - [x] Draw lines on remote side
  - [x] Leaving and joining IDs 
  - [x] Find midpoint between TI and paint it.
  - [x] Start drawing a line when TI are making contact.
  - [x] Stop drawing a line (collecting points) when TI stop making contact.
  - [x] Take the point from the hand class and send them.
  - [x] Keep fuzzy bug / make handInput dummy data the same as real data. 
#endregion */

import { HandInput } from "../lib/hand.mjs";

let myHand, points; // Our local hand data.
const gesture = emptyGesture(); // Our local gesture data.

let server;
const otherHands = {}; // Remote hand data, stored by client id.
const otherGestures = {}; // Remote drawn gestures for each client.
const FADE = 600;
const remoteMirroring = false; // Send your own points through the server, useful for debugging

// 🥾 Boot
function boot({ net: { socket }, num }) {
  myHand = new HandInput({ num });
  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id);
      delete otherHands[id];
      delete otherGestures[id]; // TODO: Fade out instead of delete.
    }
    if (type === "joined") {
      console.log("️👋 Hello:", id);
      otherGestures[id] = emptyGesture();
    }
    if (type.startsWith("connected")) {
      // Respond to: "connected" or "connected:already"
      console.log("Your ID is:", id);
      otherGestures[id] = emptyGesture();
    }
    if (type === "handtime:hand" && server.id === id && remoteMirroring) {
      otherHands[id] = content;
    }
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

    let g = otherGestures[id];
    if (!g) {
      otherGestures[id] = emptyGesture();
      g = otherGestures[id];
    }

    const { ti, midpoint } = processGesture(
      $,
      otherPoints,
      g,
      myHand.gesture(otherPoints),
    );
    $.ink(ti ? "pink" : "lime").box(...midpoint, 12, "fill*center");

    for (let j = 0; j < otherPoints.length; j += 1) {
      $.ink(server?.id === id ? "brown" : "red").box(
        otherPoints[j][0],
        otherPoints[j][1],
        5,
        "fill*center",
      );
    }
  }

  // Draw local points on top.
  for (let j = 0; j < points?.length; j += 1) {
    let [x, y] = points[j];
    if (myHand.dummy) x = y = undefined;
    $.ink("lime").box(x, y, 3, "fill*center");
  }

  if (points?.length > 0) {
    const { ti, midpoint } = processGesture(
      $,
      points,
      gesture,
      myHand.interactions,
    );
    $.ink(ti ? "blue" : "red").box(...midpoint, 9, "fill*center");
  }

  // Loop through every lines.
  Object.keys(otherGestures).forEach((g) => {
    otherGestures[g].lines.forEach((mark) =>
      $.ink(255, $.num.map(mark.fade, 0, FADE, 0, 255)).poly(mark.points),
    );
  });
  gesture.lines.forEach((mark) =>
    $.ink(255, $.num.map(mark.fade, 0, FADE, 0, 255)).poly(mark.points),
  );
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

  Object.keys(otherGestures).forEach((g) => {
    otherGestures[g].lines.forEach((mark, i) => {
      if (mark.active) return;
      mark.fade -= 1;
      if (mark.fade === 0) otherGestures[g].lines.splice(i, 1);
    });
  });
  gesture.lines.forEach((mark, i) => {
    if (mark.active) return;
    mark.fade -= 1;
    if (mark.fade === 0) gesture.lines.splice(i, 1);
  });
}

// 📰 Meta
function meta() {
  return { title: "Handtime", desc: "Communicate manually." };
}

export { boot, paint, sim, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

// Create a formatted, empty gesture.
function emptyGesture() {
  return { drawing: false, lines: [], currentMark: null };
}

// Decide if we are starting or stopping a mark, and add
// points.
function processGesture($, points, g, interactions) {
  const T = points[myHand.indices.t];
  const I = points[myHand.indices.i];
  const midpoint = $.num.midp(T, I);

  let ti;
  interactions.forEach((interaction) => {
    if (interaction.t && interaction.i) ti = interaction;
  });

  if (ti && !g.drawing) {
    g.drawing = true;
    g.currentMark = { points: [], fade: FADE, active: true }; //TODO: Move fade to sim
    g.lines.push(g.currentMark);
  } else if (!ti && g.drawing) {
    g.drawing = false;
    g.currentMark.active = false;
    g.currentMark = undefined;
  }

  if (g.drawing) {
    const lastPoint = g.currentMark.points[g.currentMark.length - 1];
    if (
      !lastPoint ||
      lastPoint[0] !== midpoint[0] ||
      lastPoint[1] !== midpoint[1]
    ) {
      g.currentMark.points.push(midpoint);
    }
  }

  return { ti, midpoint };
}
