// g.mjs - The letter G
// 🍇 Grapes, Giraffe, Guitar... and the musical note G!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "g";
const theme = ABC.letterThemes[LETTER];

let frameCount = 0;
const FRAMES_PER_FONT = 90;

function boot({ sound }) {
  ABC.playLetterSound(LETTER, sound);
}

function paint($) {
  const fontIndex = Math.floor(frameCount / FRAMES_PER_FONT) % ABC.fonts.length;
  ABC.drawLetter(LETTER, $, theme, fontIndex);
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    ABC.playLetterSound(LETTER, sound);
    frameCount = (Math.floor(frameCount / FRAMES_PER_FONT) + 1) * FRAMES_PER_FONT;
    needsPaint();
  }
}

function sim() {
  frameCount++;
}

function meta() {
  return { title: "G", desc: `${theme.emoji} ${theme.words[0]} - The letter G and musical note G!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
