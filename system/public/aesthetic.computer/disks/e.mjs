// e.mjs - The letter E
// 🐘 Elephant, Egg, Ear... and the musical note E!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "e";
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
  return { title: "E", desc: `${theme.emoji} ${theme.words[0]} - The letter E and musical note E!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
