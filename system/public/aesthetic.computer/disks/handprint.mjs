// Handprint, 2023.7.06.15.18.20
// Stamp an image of your hand.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - A. Decorate / draw colored shape based on hand data.
   - [] Allow different colors.
   - [] Patterns?
  - [] `handprint` uses video.
    - [] If no color is specified, then use video output.
  - [] `handprint yellow` just fills hand with yellow
  - [] `handprint red 0.1` low opacity red hand.
  - B. Explore ways of stamping the hands...
  - [1] Tap / whack spacebar to stamp.
  - [2] Use z values to create "dippable" hand printing. 
  - [3] Paint based on stillness or movement.
#endregion */

let handInput;
let handprint;
import { HandInput } from "../lib/hand.mjs";

// 🥾 Boot
function boot() {
  handInput = new HandInput();
}

// 🎨 Paint
function paint($) {
  handprint = () => {
    handInput.paint($);
  }
  handprint();
  $.system.nopaint.needsPresent = true;
}

// 🥞 Bake (to the painting)
function bake($) {
  handprint?.($);
}

// 🧮 Sim
function sim($) {
  handInput.sim($);
}

// 🧮 Act
function act($) {
  handInput.act($);
}

// 📰 Meta
function meta() {
  return {
    title: "Handprint",
    desc: "Stamp an image of your hand.",
  };
}

export const system = "nopaint:bake-on-leave";
export { boot, paint, bake, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)