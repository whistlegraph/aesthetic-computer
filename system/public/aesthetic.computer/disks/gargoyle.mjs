// Gargoyle, 23.05.21.17.59
// A character playground for Georgica. 

/* #region 🏁 TODO
#endregion */

const prompt = `say anything`;
const before = ``;
const after = ``;

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