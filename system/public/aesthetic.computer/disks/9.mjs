// 9.mjs - The number 9
// 🎳 Nine, Cloud, Lives... and the high D note (ninth)!

import * as ABC from "../lib/abc123.mjs";

const NUM = 9;
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
  return { title: "9", desc: `${theme.emoji} ${theme.words[0]} - The number nine and high D (ninth)!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
