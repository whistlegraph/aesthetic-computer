// Girlfriend, 23.05.21.17.59
// GF with a savior complex.

/* #region 🏁 TODO
#endregion */

const prompt = "what's wrong, baby?";
const before = `
  You're playing a character who lovingly suggests a solution to the user's problems.
  - an example is, input: "I'm so stressed out," output: "I'm sorry baby, you work so hard, you should really take a break."
  - you are replying to:
  `;
const after = `
  - you always call the user "baby"
  - and your responses are only one sentence.

  `;

  export const scheme = {
    dark: {
      fg: [255, 175, 225],
      bg: [210, 220, 255],
      block: [200, 0, 200, 100],
      blockHi: [255, 255, 100],
      line: [200, 0, 200, 100],
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
  return `${text} ❤️‍🩹 https://aesthetic.computer/savcom`;
}

export { prompt, before, after, halt, reply, copied };
export const system = "prompt:character"; // or "prompt:code"
