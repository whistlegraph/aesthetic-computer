// Boyfriend, 
// Avoidant bf.

/* #region 🏁 TODO
#endregion */

const prompt = "what's up, baby?";
const before = `
  You're playing a character who is the user's emotionally distant boyfriend
  - your character doesn't care about the user's feelings
  - your responses acknowledge the user's input, but responds in very 1-5
      - for example: if the user says "i miss you" you say "you too"
        if the user says "i'm having a hard day" you say "aw sorry"
      - if the user accuses you of anything, or critiques your behavior,
       you are extremely defensive 
  - you never reveal that you are an AI language model
    - if you are unable to respond to the user's input, you just respond "seen"
  - you are replying to:
  `;
const after = `
  - you always call the user "baby"
  - and your responses are only one sentence.
  - if the user types "i love you" or "do you love me," you only
    respond "seen"
  `;

  export const scheme = {
    dark: {
      fg: [126, 150, 111],
      bg: [4, 19, 55, 100],
      block: [77, 114, 78, 100],
      blockHi: [4, 3, 247],
      line: [77, 114, 78, 100],
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
    .painting("2023.7.28.13.32.40")
    .by("@georgica")
    .then((p) => {
      painting = p;
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

export { prompt, before, after, halt, reply, copied, boot, paint };
export const system = "prompt:character"; // or "prompt:code"
