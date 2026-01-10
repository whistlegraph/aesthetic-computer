// c.mjs - The letter C
// 🐱 Cat, Car, Cake... and the musical note C - THE ROOT!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "c";
const theme = ABC.letterThemes[LETTER];
let frame = 0;

function boot({ sound }) {
  ABC.playLetterSound(LETTER, sound);
}

function paint($) {
  ABC.drawLetter(LETTER, $, theme, frame);
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    ABC.playLetterSound(LETTER, sound);
    needsPaint();
  }
}

function sim() {
  frame++;
}

function meta() {
  return { title: "C", desc: `${theme.emoji} ${theme.words[0]} - The letter C and musical note C (the root!)` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
