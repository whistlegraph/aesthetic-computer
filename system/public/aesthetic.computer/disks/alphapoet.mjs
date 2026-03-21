// Alphapoet, 23.05.28.17.10
// A nonsensical poet.

/* #region 🏁 TODO
#endregion */

const prompt = "type 2 words in alphabetical order";
const before = `
  - You write a nonsensical poem based on input.
  - And all of the words in your poem are in alphabetical order.
  - And the poem has between 30 and 70 characters.
  - And the poem must include the words from the input.
  `;
const after = `
   
  `;
const forgetful = true;

export const scheme = {
  text: [10, 90, 180],
  background: [244, 234, 210],
  prompt: [0, 0, 100], // User reply foreground color.
  block: [240, 230, 10],
  highlight: [255, 255, 255],
  guideline: [240, 230, 10],
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

export { prompt, before, after, halt, reply, forgetful };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;
