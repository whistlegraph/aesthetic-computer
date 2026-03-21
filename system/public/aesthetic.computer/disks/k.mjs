// k.mjs - The letter K
// 🪁 Kite, King, Kangaroo... and the high F note!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "k";
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
  return { title: "K", desc: `${theme.emoji} ${theme.words[0]} - The letter K and musical note K!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
