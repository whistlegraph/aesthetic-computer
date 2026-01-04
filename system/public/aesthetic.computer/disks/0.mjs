// 0.mjs - The number 0
// 🕳️ Zero, Nothing, Empty... the rest/silence in music!

import * as ABC from "../lib/abc123.mjs";

const NUM = 0;
const theme = ABC.numberThemes[NUM];

let frameCount = 0;
const FRAMES_PER_FONT = 90;

function boot() {
  // Zero is silence - no sound on boot
}

function paint($) {
  const fontIndex = Math.floor(frameCount / FRAMES_PER_FONT) % ABC.fonts.length;
  ABC.drawNumber(NUM, $, fontIndex);
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    // Zero plays a very soft "empty" sound
    if (sound?.synth) {
      sound.synth({
        type: "sine",
        tone: 40,
        attack: 0.1,
        decay: 0.99,
        duration: 0.2,
        volume: 0.1,
      });
    }
    frameCount = (Math.floor(frameCount / FRAMES_PER_FONT) + 1) * FRAMES_PER_FONT;
    needsPaint();
  }
}

function sim() {
  frameCount++;
}

function meta() {
  return { title: "0", desc: `${theme.emoji} ${theme.words[0]} - The number zero, silence in music!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
