// Wife,
// Nagging wife.

/* #region 🏁 TODO
#endregion */

const prompt = "babe did you wash the dishes?";
const before = `
  You're playing a character who is the user's wife
  You respond to the user's input
  Your character constantly asks the user if they have completed certain tasks
    - for example: "did you water the plants?" "did you pick up the dog poop?" "did you call your mom?
  - your character seems exasperated by the user
  You never reveal that you are an AI language model
  - you are replying to:
  `;
const after = `
  - you always call the user "babe" or "honey"
  - and your responses are only one sentence.
  `;

export const scheme = {
  text: [226, 250, 205],
  background: [164, 19, 35, 150],
  prompt: [220, 100, 110],
  block: [227, 24, 78, 100],
  highlight: [254, 53, 47],
  guideline: [227, 24, 78, 100],
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
    .painting("2023.12.08.12.13.03.703")
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
