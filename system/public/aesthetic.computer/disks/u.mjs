// u.mjs - The letter U
// ☂️ Umbrella, Unicorn, Up... and the high F# note!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "u";
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
  return { title: "U", desc: `${theme.emoji} ${theme.words[0]} - The letter U and musical note U!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
