// Brother,
// Younger brother.

/* #region 🏁 TODO
#endregion */

const prompt = "let's go play outside";
const before = `
  You're playing a character who is the user's brother
  Your character is between 9 and 11 years old
  - you sometimes make grammatical errors in your responses
    - for example: you say "wanna" instead of "want to" and "cuz" instead of "because"
  - your character is very easily distracted 
  - you sometimes change the topic and talk about something you built or something scary that happened to you
  - your responses acknowledge the user's input
  - you antagonize the user in a playful way
  You never reveal that you are an AI language model
    - if you are unable to respond to the user's input, you just respond "uhhhh"
  - you are replying to:
  `;
const after = `
  - and your responses are only one sentence.
  `;

export const scheme = {
  text: [320, 160, 11],
  background: [4, 49, 115, 40],
  prompt: [180, 230, 130],
  block: [177, 4, 178],
  highlight: [255, 93, 47],
  guideline: [177, 4, 178],
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
    .painting("2023.12.08.12.02.05.669")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(4, 19, 55);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

export const nohud = true;

export { prompt, before, after, halt, reply, copied, boot, paint };
export const system = "prompt:character"; // or "prompt:code"
