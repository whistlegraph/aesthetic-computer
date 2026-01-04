// 1.mjs - The number 1
// ☝️ One, First, Single... and the C note (root)!

import * as ABC from "../lib/abc123.mjs";

const NUM = 1;
const theme = ABC.numberThemes[NUM];
let frame = 0;

function boot({ sound }) {
  ABC.playNumberSound(NUM, sound);
}

function paint($) {
  ABC.drawNumber(NUM, $, frame);
}

function sim() {
  frame++;
}

function act({ event: e, sound, jump, needsPaint }) {
  if (ABC.handleNavigation(e, jump)) return;
  if (e.is("touch")) {
    ABC.playNumberSound(NUM, sound);
    needsPaint();
  }
}

function meta() {
  return { title: "1", desc: `${theme.emoji} ${theme.words[0]} - The number one and C note (the root)!` };
}

export { boot, paint, sim, act, meta };
export const nohud = true;
