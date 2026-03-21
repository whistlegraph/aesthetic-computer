// h.mjs - The letter H
// 🏠 House, Hat, Heart... and the high C note!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "h";
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
  return { title: "H", desc: `${theme.emoji} ${theme.words[0]} - The letter H and musical note H!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
