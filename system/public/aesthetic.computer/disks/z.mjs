// z.mjs - The letter Z
// 🦓 Zebra, Zoo, Zipper... the sleepy letter with no note (zzz)!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "z";
const theme = ABC.letterThemes[LETTER];

let frameCount = 0;
const FRAMES_PER_FONT = 90;

function boot({ sound }) {
  // Z has no musical note - play a soft snore/whoosh sound
  if (sound?.synth) {
    sound.synth({
      type: "noise-white",
      tone: 80,
      attack: 0.1,
      decay: 0.95,
      duration: 0.4,
      volume: 0.2,
    });
  }
}

function paint($) {
  const fontIndex = Math.floor(frameCount / FRAMES_PER_FONT) % ABC.fonts.length;
  ABC.drawLetter(LETTER, $, theme, fontIndex);
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    if (sound?.synth) {
      sound.synth({
        type: "noise-white",
        tone: 80,
        attack: 0.1,
        decay: 0.95,
        duration: 0.4,
        volume: 0.2,
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
  return { title: "Z", desc: `${theme.emoji} ${theme.words[0]} - The sleepy letter Z! Zzz...` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
