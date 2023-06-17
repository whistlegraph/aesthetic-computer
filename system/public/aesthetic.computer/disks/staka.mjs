// Staka, 2023.6.17.17.01.49
// Stack colors with your hand!

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
- [] Recognize shaka gesture based on T and P y values 
- [] Draw line between T and P 
- [] Generate falling shapes 
- [] Catch falling shapes on the line
- [] Create screen boundaries for where the console can be used  
- [] Game start, game over
- [] Hand is working/not working
- [] Levels
- [] Beeps

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
    const { wipe, ink, screen: { height } } = $;
    wipe(127);
    handInput.paint($);
  }

// 🎪 Act
function act($) {
  handInput.act($);
  // Respond to user input here.
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

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

export { boot, act, meta, paint };

// 📚 Library
//   (Useful functions used throughout the piece)
