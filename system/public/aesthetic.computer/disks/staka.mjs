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

let circle, plate, touching;

// 🥾 Boot
let handInput;
function boot({ num, geo, screen }) {
  // Runs once at the start.
  handInput = new HandInput();
  const radius = 16;
  // circle = new geo.Circle(num.randInt(screen.width), -radius, radius);
  circle = new geo.Circle(screen.width / 2, screen.height / 2, radius);
}

// 🎨 Paint
function paint($) {
  const {
    wipe,
    ink,
    screen: { height },
  } = $;
  wipe(127);
  
  handInput.paint($, { faded: plate !== undefined }); // Uses calculated points.
  if (plate) {
    ink(255, 96).pline(
      [
        { x: plate[0][0], y: plate[0][1] },
        { x: plate[1][0], y: plate[1][1] },
      ],
      12
    );
    ink(255).line(...plate);
  }
  let circleColor = touching ? [255, 0, 0] : "blue"; 
  ink(circleColor).circle(circle.x, circle.y, circle.radius, true);
  ink(255, 255, 0).circle(circle.x, circle.y, circle.radius);
}

// 🧮 Sim
function sim($) {
  handInput.sim($); // Calculate the hand points.
  // Runs once per logic frame. (120fps locked.)
//   circle.y += 0.1; 

  const timop = handInput.timop;

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
      plate = [timop[0], timop[4]];
      touching = circle.online(...plate[0], ...plate[1]);
    } else {
      plate = undefined;
    }
  }
  
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
