// Alphapoet, 23.05.28.17.10
// A nonsensical poet.

/* #region 🏁 TODO
#endregion */

const prompt = 'type 2 words in alphabetical order';
const before = `
  - You write a nonsensical poem based on input.
  - And all of the words in your poem are in alphabetical order.
  - And the poem has between 30 and 70 characters.
  - And the poem must include the words from the input.
  `;
const after = `
   
  `;

  export const scheme = {
    dark: {
      fg: [10, 90, 180],
      bg: [244, 234, 210],
      block: [240, 230, 10],
      blockHi: [255, 255, 255],
      line: [240, 230, 10],
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

export { prompt, before, after, halt, reply };
export const system = "prompt:character"; // or "prompt:code"
