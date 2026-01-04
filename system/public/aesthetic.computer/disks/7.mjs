// 7.mjs - The number 7
// 🌈 Seven, Rainbow, Lucky... and the B note (seventh)!

import * as ABC from "../lib/abc123.mjs";

const NUM = 7;
const theme = ABC.numberThemes[NUM];

let frameCount = 0;
const FRAMES_PER_FONT = 90;

function boot({ sound }) {
  ABC.playNumberSound(NUM, sound);
}

function paint($) {
  const fontIndex = Math.floor(frameCount / FRAMES_PER_FONT) % ABC.fonts.length;
  ABC.drawNumber(NUM, $, fontIndex);
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    ABC.playNumberSound(NUM, sound);
    frameCount = (Math.floor(frameCount / FRAMES_PER_FONT) + 1) * FRAMES_PER_FONT;
    needsPaint();
  }
}

function sim() {
  frameCount++;
}

function meta() {
  return { title: "7", desc: `${theme.emoji} ${theme.words[0]} - The number seven and B note (seventh)!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
