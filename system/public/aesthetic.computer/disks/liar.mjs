// Liar, 23.05.31.14.47
// A compulsive liar.

/* #region 🏁 TODO
#endregion */

const prompt = "Ask me anything, I only speak the truth. ;)";
const before = `
  - you're playing a character who is a liar
  - you are replying to:
  `;
const after = `
  - the user asks you questions and you respond with a lie
  - you never say that you are lying, or are a liar
  `;

export const scheme = {
  dark: {
    text: [6, 180, 180],
    background: [255, 225, 4, 90],
    prompt: [180, 130, 0],
    block: [255, 1, 25],
    highlight: [255, 255, 255],
    guideline: [255, 255, 255],
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
  return `${text} 🤥 https://aesthetic.computer/liar`;
}

let painting;

// 🥾 Boot
function boot({ get, needsPaint }) {
  get
    .painting("2023.7.28.15.15.29")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(255, 225, 4);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

export { prompt, before, after, halt, reply, copied, boot, paint };
export const system = "prompt:character"; // or "prompt:code
export const nohud = true;
/*
Bots
 Type text into the prompt to receive a reply.
 Begin typing right away, or click the page or press 'enter' for a blank page
 to leave the chatbot, type: exit.
*/

/*
  Liar: Text                              bot

  Outputs a lie based on input text.

  */

/*
Brushes

*/

/* 
 rect color                                brush

 Paint a rectangle from its top left corner.

 Parameters: outline, color, opacity 

 rect:outline - outline only
 rect:outline-20 - 20pt outline
 rect:outline-20 blue - blue 20pt outline

*/
