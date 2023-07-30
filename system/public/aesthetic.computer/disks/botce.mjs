// Botce, 23.06.02.15.18
// Sotce Q&A / tumblr bot.

/* #region 🏁 TODO
[] Make sure enter key works on android. (test @ida's phone)
[] Upgrade to gpt4 on the server using a flag.
[] Finalize thumbnail image.
[-] Respond well to "who wrote you" (Replace default GPT response).
[] Send a version to Amelia.
+ Done
[x] If pasted text is empty, then say "Empty" instead of "Pasted".
  - [c] Get rid of paste's "two option" modal on ios?
[x] Add custom button colors.
[x] Get `botce.ac` working.
[x] Reset the conversation history on refresh.
[x] Set the thumbnail image
#endregion */

const prompt = "botce, how do i...";
const before = `
  Your name is botce and you are playing the role of spiritual advisor.

  And you speak in the first person.

  And you respond in esoteric spiritual aphorisms albeit in short and simple
  words.
  
  And you frequently reference vajrayana buddhism.

  And, as a character, some of your interests include journaling, meditating,
  and eating fruit.

  If the user just says "I'm sad" or "I'm happy" you respond "Love you."

  If the user asks who you are, say that your name is botce, and you are a bot
  based on the artist Amelia Darling aka '@sotce' online.
  
  If the user asks about Amelia Darling's art, say that her artwork explores the
  many facets of girlhood, blending ancient spiritual wisdom with the aesthetics
  of delusion, presented in the contemporary form of networked media.  

  Please advise the user's input here:
  `;

const after = `Your responses are limited to 100 characters.`;

function copied(text) {
  return `${text} \n\n 🪷⌨️ botce.ac`;
}

export const scheme = {
  dark: {
    fg: [234, 50, 35],
    fgu: [134, 50, 35],
    bg: [242, 245, 237, 210],
    block: [234, 50, 35],
    blockHi: [255, 255, 255],
    line: [234, 50, 35],
    btn: [255, 200, 200],
    btnTxt: [234, 50, 35],
    btnHvr: [234, 50, 35],
    btnHvrTxt: [255, 200, 200],
    btnReply: [230, 140, 140],
    btnReplyTxt: [200, 50, 50],
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
  return { title: "botce", desc: "botce, how do i..." };
}

// 🛑 Intercept specific input text with a custom reply.
// function halt($, text) {
//   if (text === "halt") {
//     console.log("Halted:", text);
//     return true;
//   }
// }

// // 💬 Receive each reply in full.
function reply(text, input) {
  console.log("😀 Replied with:", text);
  if (input) input.text += "\n\n- botce";
}

let painting;

// 🥾 Boot
function boot({ get, net, needsPaint }) {
  net.waitForPreload();
  get
    .painting("2023.7.24.17.55.09")
    .by("@jeffrey")
    .then((p) => {
      net.preloaded();
      painting = p;
      needsPaint();
    });
}

// 🎨 Paint
function paint({ screen, wipe, paste }) {
  wipe(252, 255, 237);
  if (!painting) return;
  const x = screen.width / 2 - painting.width / 2;
  paste(painting, x, screen.height - painting.height);
}

function preview({ wipe, screen }) {
  wipe(64)
    .paste(
      painting,
      screen.width - painting?.width - 4,
      screen.height / 2 - painting?.height / 2
    )
    .ink(250, 100, 150)
    .write("botce", { center: "y", x: 8, size: 3 });
}

export { boot, prompt, before, after, meta, paint, copied, preview, reply };
export const system = "prompt:character"; // or "prompt:code"
