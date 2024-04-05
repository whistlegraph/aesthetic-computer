// Gargoyle, 23.05.21.17.59
// A character playground for @georgica.

/* #region 🏁 TODO
  - [] How to replace the "@" with the current user?
#endregion */

const prompt = "whats on your mind, @?";
const before = `
  your name is gargoyle
  and you are replying to:
  `;
const after = `
  - and you respond in metaphor
  - all responses are limited to 99 characters
  - all responses provide advice
  - you don't talk about gargoyles
  - some responses include questions
  - your tone is friendly
  `;

export const scheme = {
  text: [0, 200, 0, 300],
  background: [130, 20, 100, 200],
  prompt: [220, 210, 10],
  block: [200, 130, 10],
  highlight: [200, 100, 0],
  guideline: [0, 200, 0, 300],
};

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  if (text === "halt") {
    console.log("Halted:", text);
    return true;
  }
}

// 💬 Receive each response in full.
function reply(text) {
  console.log("😀 Replied with:", text);
}

function copied(text) {
  return `${text} 🧌 https://aesthetic.computer/gargoyle`;
}

let painting;

// 🥾 Boot
function boot({ get, net, needsPaint }) {
  net.waitForPreload();
  get
    .painting("2023.7.24.17.02.58")
    .by("@georgica")
    .then(({ img }) => {
      net.preloaded();
      painting = img;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, ink, paste }) {
  wipe(130, 20, 100);
  if (!painting) return;
  const xposition = screen.width / 2 - painting.width / 2;
  paste(painting, xposition, screen.height - painting.height);
}

function preview({ wipe, screen }) {
  wipe(scheme.dark.bg)
    .paste(
      painting,
      screen.width - painting?.width - 4,
      screen.height / 2 - painting?.height / 2,
    )
    .ink(0, 200, 0, 300)
    .write("gargoyle", { center: "xy", size: 3 });
}

export { prompt, before, after, halt, reply, copied, boot, paint, preview };
export const system = "prompt:character"; // or "prompt:code"
export const nohud = true;
