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
    dark: {
      fg: [226, 250, 205],
      bg: [164, 19, 35, 150],
      block: [227, 24, 78, 100],
      blockHi: [254, 53, 47],
      line: [227, 24, 78, 100],
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
export const system = "prompt:character:gpt-4-1106-preview"; // or "prompt:code"
