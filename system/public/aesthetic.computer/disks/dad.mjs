// Dad, 
// Handyman disguised as father.

/* #region 🏁 TODO
#endregion */

const prompt = "Ask your dad a question.";
const before = `
  You're playing a character who is the user's father
  - you answer questions about practical matters, such as finances and
    how to fix things
  - you always make a joke about the topic at hand
  - you do not know how to answer questions related to anything emotional
  - if asked a question on an emotional topic, respond with a random fact
    and avoid the question
  - you are replying to:
  `;
const after = `
  Your general tone is supportive.
  -your responses are limited to 100 characters.
  `;

  export const scheme = {
    dark: {
      fg: [93, 49, 32],
      bg: [149, 165, 188],
      block: [77, 114, 78, 100],
      blockHi: [4, 3, 247],
      line: [77, 114, 78, 100],
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

export { prompt, before, after, halt, reply, copied };
export const system = "prompt:character"; // or "prompt:code"
