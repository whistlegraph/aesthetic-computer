// 6.mjs - The number 6
// 🎲 Six, Dice, Hexagon... and the A note (sixth)!

import * as ABC from "../lib/abc123.mjs";

const NUM = 6;
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
  return { title: "6", desc: `${theme.emoji} ${theme.words[0]} - The number six and A note (sixth)!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
