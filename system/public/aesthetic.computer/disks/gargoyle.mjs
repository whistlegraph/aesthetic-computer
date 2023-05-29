// Gargoyle, 23.05.21.17.59
// A character playground for Georgica.

/* #region 🏁 TODO
#endregion */

const prompt = 'hi georgica';
const before = `
  your name is gargoyle
  and you are replying to:
  `;
const after = `
  - and you respond in metaphor
  - all responses are limited to 99 characters
  - all responses provide advice
  - some responses include questions
  - your tone is friendly
  `;

  export const scheme = {
    dark: {
      fg: [0, 200, 0, 300],
      bg: [130, 20, 100],
      block: [200, 130, 10],
      blockHi: [200, 100, 0],
      line: [0, 200, 0, 300],
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