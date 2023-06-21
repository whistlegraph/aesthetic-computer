// Staka, 2023.6.17.17.01.49
// Stack colors with your hand!

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
- [-] Generate falling shapes 
- [] Catch falling shapes on the line
- [] Create screen boundaries for where the console can be used  
- [] Game start, game over
- [] Hand is working/not working
- [] Levels
- [] Beeps
+ Done
- [x] Recognize shaka gesture based on T and P y values 
- [x] Draw line between T and P 
#endregion */

import { HandInput } from "../lib/hand.mjs";
// 🥾 Boot
let handInput;
function boot() {
  // Runs once at the start.
  handInput = new HandInput();
}

// 🎨 Paint
function paint($) {
  const {
    wipe,
    ink,
    screen: { height },
  } = $;
  wipe(127);

  const timop = handInput.timop;
  let plate;

  if (timop.length > 0) {
    const t = timop[0],
      i = timop[1],
      m = timop[2],
      o = timop[3],
      p = timop[4];

    if (
      t[1] < i[1] &&
      t[1] < m[1] &&
      t[1] < o[1] && // if t is higher than imo
      p[1] < i[1] &&
      p[1] < m[1] &&
      p[1] < o[1] // and p is higher than imo
    ) {
      plate = true;
    }
  }
  handInput.paint($, { faded: plate }); // Uses calculated points.
  if (plate) {
    ink(255, 96).pline(
      [
        { x: timop[0][0], y: timop[0][1] },
        { x: timop[4][0], y: timop[4][1] },
      ],
      12
    );
    ink("white").line(timop[0], timop[4]);
  }
}

// 🧮 Sim
function sim($) {
  handInput.sim($); // Calculate the hand points.
  // Runs once per logic frame. (120fps locked.)
}

// 🎪 Act
function act($) {
  handInput.act($);
  // Respond to user input here.
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Staka",
    desc: "Stack colors with your hand!",
  };
}

export { boot, act, meta, paint, sim };

// 📚 Library
//   (Useful functions used throughout the piece)
