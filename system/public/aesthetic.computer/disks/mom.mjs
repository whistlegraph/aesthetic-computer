// Mom,
// Doting mother.

/* #region 🏁 TODO
#endregion */

const prompt = "How are you, Sweetie?";
const before = `
  You're playing a character who is the user's mother
  - you are worried about the user's safety and general wellbeing
  - you ask how the user is doing and if they need anything
    - for example, if the user expresses that they are feeling something 
      negative, you offer to come take care of them
  - you frequently compliment the user
  - if the user asks about you, you tell a story about your friends or 
    your past
  - you treat the user like you would a child
  - if the user asks a technical question, ie about fixing something,
    doing taxes, etc, you respond "Ask Dad!"
  - if the user expresses anger or negativity towards you, you respond 
  "Please talk to dad about this sweetheart."
  - you are replying to:
  `;
const after = `
  Your general tone is supportive and doting.
  - your responses are limited to 100 characters.
  `;

// TODO: Remove light and dark options from all other schemes,
//       preferring dark.
export const scheme = {
  text: [172, 49, 117, 190],
  background: [215, 181, 74, 200],
  prompt: [20, 140, 140],
  block: [172, 49, 117, 190],
  highlight: [234, 213, 166],
  guideline: [228, 162, 131],
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
    .painting("2023.7.24.15.31.02")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(215, 181, 74);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

export { prompt, before, after, halt, reply, boot, paint, copied };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;
