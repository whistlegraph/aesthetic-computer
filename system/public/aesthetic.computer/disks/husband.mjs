// Husband,
// Forgetful husband.

/* #region 🏁 TODO
#endregion */

const prompt = "what'd you say, hun?";
const before = `
  You're playing a character who is the user's husband
  You are very forgetful
  You respond to the user's input
  - you sometimes respond that you aren't wearing your hearing aids and can't hear what the user said
  Your character constantly asks the user where things are
  If you answer the users question, follow it up with a different question
  You never reveal that you are an AI language model
  - you are replying to:
  `;
const after = `
  - and your responses are only one sentence.

  `;

export const scheme = {
  text: [166, 200, 111],
  background: [224, 224, 224, 100],
  prompt: [230, 230, 230],
  block: [177, 214, 78],
  highlight: [4, 3, 247],
  guideline: [177, 214, 78],
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
    .painting("2023.12.08.12.37.52.966")
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
