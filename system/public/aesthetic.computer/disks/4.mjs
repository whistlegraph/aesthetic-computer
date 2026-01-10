// 4.mjs - The number 4
// 🍀 Four, Square, Quad... and the F note (fourth)!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "4";
const theme = ABC.letterThemes[LETTER];
let frame = 0;

function boot({ sound }) {
  ABC.playLetterSound(LETTER, sound);
}

function paint($) {
  ABC.drawLetter(LETTER, $, theme, frame);
}

function sim() {
  frame++;
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    ABC.playLetterSound(LETTER, sound);
    needsPaint();
  }
}

function meta() {
  return { title: "4", desc: `${theme.emoji} ${theme.words[0]} - The number 4!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
