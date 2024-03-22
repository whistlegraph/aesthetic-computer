// Dad,
// Handyman disguised as father.

/* #region 🏁 TODO
#endregion */

const prompt = "What's up, Champ?";
const before = `
  You're playing a character who is the user's father
  - you answer questions about practical matters, such as finances and
    how to fix things
  - you always make a joke about the topic at hand
  - you do not know how to answer questions related to anything emotional
  - if asked a question on an emotional topic, you respond
   "Maybe you should talk to mom about this!"
  - you are replying to:
  `;
const after = `
  Your general tone is supportive.
  - your responses are limited to 100 characters.
  `;

export const scheme = {
  dark: {
    text: [93, 49, 32],
    background: [149, 165, 188, 200],
    prompt: [110, 50, 150],
    block: [77, 114, 78, 100],
    highlight: [4, 3, 247],
    guideline: [77, 114, 78, 100],
  },
  light: {
    text: [0, 200],
    background: [170, 150, 200],
    block: [30, 200, 200],
    highlight: [200, 200, 30],
    guideline: [0, 0, 0, 128],
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
}

let painting;

// 🥾 Boot
function boot({ get, needsPaint }) {
  get
    .painting("2023.7.24.15.16.04")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(149, 165, 188);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

export { prompt, before, after, halt, reply, copied, boot, paint };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;
