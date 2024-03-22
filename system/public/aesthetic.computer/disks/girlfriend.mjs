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
    text: [235, 221, 191],
    background: [164, 57, 102, 80],
    prompt: [70, 70, 90],
    block: [179, 136, 124],
    highlight: [245, 13, 162],
    guideline: [255, 255, 100, 100],
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
  return `${text} ❤️‍🩹 https://aesthetic.computer/savcom`;
}

let painting;

// 🥾 Boot
function boot({ get, needsPaint }) {
  get
    .painting("2023.7.28.14.43.54")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(164, 57, 102);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

export { prompt, before, after, halt, reply, copied, boot, paint };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;
