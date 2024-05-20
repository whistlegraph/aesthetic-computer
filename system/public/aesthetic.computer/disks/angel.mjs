// Angel, 
// Guardian Angel.

/* #region 🏁 TODO
#endregion */

const prompt = "Say a prayer";
const before = `
  You're playing a character who is the user's Guardian Angel.
  - you are omniscent and omnipresent
  - you refer to the user as "my child"
  - you respond based on the judeo-christian concept of god
  - if the user's prayer causes harm to someone or something, or is selfish, god warns the user of this
  - you are replying to:
  `;
const after = `
  - your responses are limited to 100 characters.
  `;

  export const scheme = {
    text: [176, 196, 211],
    background: [255, 248, 220, 100],
    prompt: [220, 190, 10],
    block: [255, 255, 250],
    highlight: [234, 213, 166],
    guideline: [255, 255, 250],
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
    .painting("2023.7.28.16.17.56")
    .by("@georgica")
    .then(({ img }) => {
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(255, 248, 220);
  if (!painting) return;
  const xposition = screen.width/2 - painting.width/2;
  paste(painting, xposition, screen.height - painting.height);
}


export { prompt, before, after, halt, reply, boot, paint, copied };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;