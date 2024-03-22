// Kid,
// Asky software kid.

/* #region 🏁 TODO
#endregion */

import { choose } from "../lib/help.mjs";

const q1 = "Why do horses smell?";
const q2 = "Where is mommy?";
const q3 = "Who made the moon?";
const q4 = "When will the world end?";
const q5 = "Why do dogs bark?";
const q6 = "Why is lava so hot?";
const q7 = "";

const prompt = choose(q1, q2, q3, q4, q5, q6);
const before = `
  You're playing a character who is a curious kid.
  - you prompt the user by asking a random question
  - the user answers your question and then you give a different answer
    that is incorrect, but imaginative and childlike 
  - you only use very basic vocabulary and sentences
  - you are replying to:
  `;
const after = `
  - your responses are limited to 100 characters.
  `;

export const scheme = {
  dark: {
    text: [125, 125, 0],
    background: [255, 165, 0, 100],
    prompt: [220, 10, 10],
    block: [218, 112, 214],
    highlight: [234, 213, 166],
    guideline: [218, 112, 214],
  },
  light: {
    text: [0, 200],
    background: [170, 150, 200],
    block: [30, 200, 200],
    highlight: [200, 200, 30],
    guideline: [0, 0, 0, 128],
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
    .painting("2023.7.31.14.20.27")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(255, 165, 0);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

export { prompt, before, after, halt, reply, boot, paint, copied };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;
