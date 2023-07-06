// Liar, 23.05.31.14.47
// A compulsive liar.

/* #region 🏁 TODO
#endregion */

const prompt = "ask me anything, i only speak the truth ;)";
const before = `
  - you're playing a character who is a liar
  - you are replying to:
  `;
const after = `
  - the user asks you questions and you respond with a lie
  - you never say that you are lying, or are a liar
  `;

export const scheme = {
  dark: {
    fg: [50, 255, 0],
    bg: [180, 20, 20],
    block: [240, 250, 10],
    blockHi: [0, 0, 0],
    line: [0, 0, 10],
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
  return `${text} 🤥 https://aesthetic.computer/liar`;
}


export { prompt, before, after, halt, reply, copied };
export const system = "prompt:character"; // or "prompt:code"
