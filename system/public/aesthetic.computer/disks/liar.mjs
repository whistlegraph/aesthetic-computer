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
    fg: [6, 180, 180],
    bg: [255, 225, 4, 220],
    block: [255, 1, 25],
    blockHi: [255, 255, 255],
    line: [255, 255, 255],
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
  return `${text} 🤥 https://aesthetic.computer/liar`;
}

let painting;

// 🥾 Boot
function boot({ get, needsPaint }) {
  get
    .painting("2023.7.12.16.16.02")
    .by("@georgica")
    .then((p) => { painting = p; needsPaint(); });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(255, 225, 4);
  // if (painting === undefined) return;
  // let scale
  // console.log(screen.width, screen.height)
  // if (screen.width > screen.height) {
  //   scale =  screen.width / painting.width;
  //   console.log("Scale:", scale);
  // } else {
  //   scale = screen.height / painting.height;
  //   console.log(scale, screen.height, painting.height)

  // }


  // paste(painting, 0, 0, scale);
}



export { prompt, before, after, halt, reply, copied, boot, paint };
export const system = "prompt:character"; // or "prompt:code"
