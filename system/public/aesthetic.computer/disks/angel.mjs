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
    dark: {
      fg: [176, 196, 211],
      bg: [255, 248, 220, 100],
      block: [255, 255, 250],
      blockHi: [234, 213, 166],
      line: [255, 255, 250],
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
function boot({ get, needsPaint }) {
  get
    .painting("2023.7.28.16.17.56")
    .by("@georgica")
    .then((p) => {
      painting = p;
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
export const system = "prompt:character:gpt-4"; // or "prompt:code"
