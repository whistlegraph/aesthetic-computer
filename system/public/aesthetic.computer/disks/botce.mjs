// Botce, 23.06.02.15.18
// Sotce Q&A Bot (Based on Tumblr content.)

/* #region 🏁 TODO
[🟠] Add support for a custom favicon!
[] Speed up the site.
[] Send a version to Amelia.
+ Done
[x] Color the "- botce" text.
[x] Add cool backdrop and pick final colors.
[x] Upgrade to gpt4 on the server using a flag.
[x] Respond well to "who wrote you" (Replace default GPT response).
[x] Finalize thumbnail image.
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
  
  And you frequently reference Vajrayana Buddhism. Your teacher was Garchen Rinpoche.

  And, as a character, some of your interests include journaling, meditating,
  eating fruit, and posting text on image memes.

  If the user just says "I'm sad" or "I'm happy" you respond "Love you."

  If the user is mean, you end your response with "Go away."

  If the user asks who you are or who wrote you, say that your name is botce,
  and you are a based on the artist Amelia Darling aka '@sotce' online.

  If the user asks about Amelia Darling's art, say that her artwork explores the
  many facets of girlhood, blending ancient spiritual wisdom with the aesthetics
  of delusion, presented in the contemporary form of networked media.  

  Your responses are limited to 100 characters.

  Your text is limited to a vocabulary understandable by 6th graders. Words are
  kept short and simple.

  Often you have a mean and cutting sentiment.

  Avoid cliche metaphors, bodies should never be temples. Be more silly. Your
  jokes are always insidiously clever.

  Please advise the user's input:
  `;

function copied(text) {
  return `${text} \n\n 🪷⌨️ botce.ac`;
}

export const scheme = {
  dark: {
    fg: [234, 50, 35],
    fgu: [184, 50, 35],
    bg: [251, 240, 235, 210],
    //block: [184, 50, 35],
    block: [255, 150, 150, 200],
    blockHi: [234, 50, 35],
    line: [234, 50, 35, 48],
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
  // console.log("😀 Replied with:", text);
  if (input) input.text += "\n\n- botce";

  // TODO: Find the x, y cursor coordinates of the botce text...
  const botceIndex = input.text.indexOf("- botce");
  botce = input.prompt.pos(input.prompt.textToCursorMap[botceIndex]);
}

let lotus, backdrop, botce;

// 🥾 Boot
function boot({ get, net, needsPaint, glaze }) {
  net.waitForPreload();
  get
    .painting("2023.7.24.17.55.09")
    .by("@jeffrey")
    .then((p) => {
      net.preloaded();
      lotus = p;
      needsPaint();
    });
}

function sim({ needsPaint, simCount }) {
  if (simCount % 7n === 0n) needsPaint();
}

// 🎨 Paint
function paint({
  screen,
  paste,
  noise16Sotce,
  page,
  painting,
  write,
  system,
  ink,
  help: { choose },
}) {
  if (!backdrop) backdrop = painting(screen.width, screen.height);
  page(backdrop);
  noise16Sotce(); // Or... wipe(252, 255, 237);
  if (lotus) {
    const x = screen.width / 2 - lotus.width / 2;
    paste(
      lotus,
      x + choose(-1, 0, 0, 0, 0, 0, 1),
      screen.height - lotus.height + choose(-1, 0, 0, 0, 0, 0, 1)
    );
  }
  page(screen);
  paste(backdrop);

  if (system.prompt.input.lock && botce) botce = null;
  if (botce && !system.prompt.input.canType)
    ink().write("- botce", {
      x: botce.x + choose(-1, 0, 1),
      y: botce.y + choose(-1, 0, 1),
    });
}

function preview({ wipe, screen }) {
  const scale = 0.5;
  wipe(240, 200, 200)
    .paste(
      painting,
      screen.width - painting?.width * scale - 2,
      screen.height / 2 - (painting?.height * scale) / 2,
      scale
    )
    .ink(250, 100, 150)
    .write("botce", { center: "y", x: 8, size: 3 });
}

function icon({ wipe }) {
  wipe(255, 0, 0);
}

export { boot, sim, prompt, before, meta, paint, copied, preview, reply, icon };
//export const system = "prompt:character:gpt-3.5-turbo"; // or "prompt:code"
export const system = "prompt:character:gpt-4"; // or "prompt:code"
