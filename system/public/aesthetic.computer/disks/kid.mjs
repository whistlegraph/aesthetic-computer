// Kid, 
// Asky software kid.

/* #region 🏁 TODO
#endregion */

import { choose } from "../lib/help.mjs";

const q1 = "Why do horses smell?";
const q2 = "Where is mommy?";

const prompt = choose(q1, q2);
const before = `
  You're playing a character who is a curious kid.

  - you are replying to:
  `;
const after = `
  - your responses are limited to 100 characters.
  `;

  export const scheme = {
    dark: {
      fg: [176, 196, 211],
      bg: [255, 248, 220, 100],
      block: [255, 255, 250],
      blockHi: [234, 213, 166],
      line: [255, 255, 250],
    },
    light: {
      fg: [0, 200],
      bg: [170, 150, 200],
      block: [30, 200, 200],
      blockHi: [200, 200, 30],
      line: [0, 0, 0, 128],
    },
  };

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  if (text === "halt") {
    console.log("Halted:", text);
    return true;
  }
}

// 💬 Receive each reply in full.
function reply(text) {
  console.log("😀 Replied with:", text);
}

function copied(text) {
  return text;
  // return `${text} ❤️‍🩹 https://aesthetic.computer/boyfriend`;
}

let painting;

// 🥾 Boot
function boot({ get, needsPaint }) {
  get
    .painting("2023.7.28.16.17.56")
    .by("@georgica")
    .then((p) => {
      painting = p;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(255, 248, 220);
  if (!painting) return;
  const xposition = screen.width/2 - painting.width/2;
  paste(painting, xposition, screen.height - painting.height);
}


export { prompt, before, after, halt, reply, boot, paint, copied };
export const system = "prompt:character"; // or "prompt:code"
