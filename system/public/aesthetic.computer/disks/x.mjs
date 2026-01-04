// x.mjs - The letter X
// 🩻 X-ray, Xylophone, Box... a mysterious letter with no note!

import * as ABC from "../lib/abc123.mjs";

const LETTER = "x";
const theme = ABC.letterThemes[LETTER];

let frameCount = 0;
const FRAMES_PER_FONT = 90;

function boot({ sound }) {
  // X has no musical note - play a percussive click instead
  if (sound?.synth) {
    sound.synth({
      type: "noise-white",
      tone: 200,
      attack: 0.001,
      decay: 0.8,
      duration: 0.1,
      volume: 0.3,
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
        tone: 200,
        attack: 0.001,
        decay: 0.8,
        duration: 0.1,
        volume: 0.3,
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
  return { title: "X", desc: `${theme.emoji} ${theme.words[0]} - The mysterious letter X!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
