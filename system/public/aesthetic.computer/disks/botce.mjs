// Botce, 23.06.02.15.18
// Sotce tumblr bot.

/* #region 🏁 TODO
[🟢] Reset the conversation history on refresh.

[🟠] Add pink lotus image in the corner, semi-transparent
  - [] Rotate 90
  - [] Flip
  - [] Allow for multiple no (with countdown), with yes as reverse?
  - [?] Check on full transparency support.
  + Done
  - [x] Resize needs to be able to take 0-1 values with min and
       max settings.
  - [x] Crop (this needs to be an interactive thing.)
  - [x] Resize
  - [x] Load image into painting from pasted url.
  - [x] And drag and drop.
[] Respond well to "who wrote you" (Replace default GPT response).
[] Respond well to "what is amelia's art about".
[] Send to Amelia
#endregion */

const prompt = "botce, how do i.................";
const before = `
  Your name is botce and you are playing the role of spiritual advisor.

  And you speak in the first person.

  And you respond in esoteric spiritual aphorisms.
  
  And you frequently reference vajrayana buddhism.

  And, as a character, some of your interests include journaling, meditating, and eating fruit.

  If the user just says "I'm sad" or "I'm happy" you respond "Love you."

  And you finish every response with "x, B"

  If the user asks who you are, say that your name is botce, and you are a bot based on the artist Amelia Darling aka '@sotce' online. 

  Please advise the user's input here: 
  `;

const after = `
 Your responses are limited to 100 characters.
 All of your responses include lower case letters only. 
 `;

const forgetful = false;

function copied(text) {
  return `${text} 🪷⌨️ botce.ac`;
}

export const scheme = {
  dark: {
    fg: [234, 50, 35],
    bg: [252, 255, 237, 176],
    block: [255, 200, 220],
    blockHi: [255, 255, 255],
    line: [0, 0, 0],
  },
  light: {
    fg: [234, 50, 35],
    bg: [252, 255, 237],
    block: [130, 20, 0],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

// 📰 Meta
function meta() {
  return {
    title: "botce",
    desc: "botce, how do i.................",
  };
}

// 🛑 Intercept specific input text with a custom reply.
// function halt($, text) {
//   if (text === "halt") {
//     console.log("Halted:", text);
//     return true;
//   }
// }

// // 💬 Receive each reply in full.
// function reply(text) {
//   console.log("😀 Replied with:", text);
// }

let painting;

// 🥾 Boot
function boot({ get }) {
  get
    .painting("2023.5.03.13.38.15")
    .by("@jeffrey")
    .then((p) => (painting = p));
}

// 🎨 Paint
function paint({ wipe, ink, paste }) {
  wipe(252, 255, 237);
  paste(painting);
}

export { boot, prompt, before, after, forgetful, meta, paint, copied };
export const system = "prompt:character"; // or "prompt:code"
