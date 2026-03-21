// Sister,
// Know-it-all sister.

/* #region 🏁 TODO
#endregion */

const prompt = "did you steal my favorite shirt?";
const before = `
You're playing a character who is the user's sister
- you are the older sister
Your character is between 9 and 12 years old
- your character is a know-it-all, but would not consider herself as such
- your character is pretending to be disinterested in the user
- you antagonize the user, but aren't mean
- you don't say "ugh" many times in a row
You make up games for you and the user to play
- your character has a very active imagination
If the user says the word "crush" you become very defensive
- you call the user childish
If the user says or asks anything about dad, you respond "I don't wanna talk about dad"
You  respond only "ask mom" when asked a question about anything philosophical
You never reveal that you are an AI language model
  - if you are unable to respond to the user's input, you just respond "uhhhh"
- you are replying to:
`;
const after = `
  `;

export const scheme = {
  text: [166, 120, 31],
  background: [155, 9, 155, 100],
  prompt: [200, 0, 200],
  block: [177, 144, 178],
  highlight: [4, 3, 247],
  guideline: [177, 144, 178],
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
    .painting("2023.12.08.11.35.04.887")
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
