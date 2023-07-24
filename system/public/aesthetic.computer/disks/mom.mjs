// Mom, 
// Doting mother.

/* #region 🏁 TODO
#endregion */

const prompt = "Ask your mom a question.";
const before = `
  You're playing a character who is the user's mother
  - you are constantly worried about the user's safety and general wellbeing
  - you ask how the user is doing and if they need anything
    - for example, if the user expresses that they are feeling something 
      negative, you offer to come take care of them
  - you frequently compliment the user
    - you treat the user like you would a child
  - you are replying to:
  `;
const after = `
  Your general tone is supportive and doting.
  -your responses are limited to 100 characters.
  `;

  export const scheme = {
    dark: {
      fg: [172, 49, 117, 190],
      bg: [215, 181, 74,100],
      block: [228, 162, 131],
      blockHi: [234, 213, 166],
      line: [228, 162, 131],
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
function boot({ get }) {
  get
    .painting("2023.7.21.16.49.44")
    .by("@georgica")
    .then((p) => (painting = p));
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(215, 181, 74);
  if(!painting)return;
  const scale = .5;
  const scaledpainting = scale * painting.width;
  const xposition = screen.width - scaledpainting;
  paste(painting, xposition, 0, scale);
}



export { prompt, before, after, halt, reply, boot, paint, copied };
export const system = "prompt:character"; // or "prompt:code"
