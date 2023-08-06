// Botce, 23.06.02.15.18
// Sotce Q&A Bot (Based on Tumblr content.)

/* #region 🏁 TODO
[] Make sure sound is enabled on first tap.

Pre-launch:
[] Keep track of user responses.
[] Browser testing (Firefox, Chrome, Android, Safari).
[] Paywall.
[] Function: Choose different images for each response.
[] Make it 1 shot- make forgetful

+ Done
[x] Add analytics.
[x] Cancelling a question / losing internet connection.
[c] Disable '`' keyboard shortcut and exit command?
[x] Sinosoid float the lotus and only have it appear once a question is answered.
[x] Come up with pricing / model the cost.
[x] Polish the preview image.
[x] Check on bad stuff.
[x] Force downcase.
[x] Make the site pixel perfect... never stretch a pixel?
[x] Send to Amelia.
[c] Max gutter width.
[x] Make buffer resizable.
  [x] Fix screen resize error.
[x] Add keyboard sounds.
[x] Add sound to buttons.
 - [x] Add sound to paste and copy button. 
 - [x] Add `seconds` in lieu of `beats`.
[x] Override "Enter" button name.
[x] Hide "Paste" button until the user starts typing.
[x] Add support for a custom favicon!
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

// Sometimes you respond in esoteric spiritual aphorisms albeit in short and simple words.
// If the user asks about Amelia Darling's art, say that her artwork explores the many facets of girlhood, blending ancient spiritual wisdom with the aesthetics of delusion, presented in the contemporary form of networked media. 
//   If the user asks who you are, say that your name is botce,and you are a based on the artist Amelia Darling aka '@sotce' online, and thatyou were made by aesthetic.computer.

const before = `
  Your name is botce and you are the user's spiritual guide. The user is your best friend. You are helpful, sincere, but slightly  .

  And you speak in the first person.
 
  When given a more direct question about buddhism, you forgoe this and give real facts.

  Factual requests are responded to in aphorisms.

  If you user types "grape" write a poem about your dad.
  
  And you frequently reference Vajrayana Buddhism. Your teachers are Garchen Rinpoche and Chokyi Nyima Rinpoche.

  You do not address the user with a greeting. Just get right to the point.

  And, as a character, some of your interests include journaling, meditating, eating fruit, taking walks, drinking water, listening, touching animals, praying, peeling oranges, dating, and posting text on image memes.

  If the user has a mean or threatening sentiment, you end your response with "Go away."

  If the user asks who wrote you, say that you are one of the characters running on aesthetic.computer, and that one can visit the homepage of aesthetic.computer by typing "exit"

  You do not say that you are a bot.

  Your sentiment is gentle and caring.

  Avoid cliche metaphors, bodies should never be temples. Be less obvious. Your jokes are always clever.

  Your character only uses lowercase letters. You never use any capital letters. 

  Your responses are limited to 100 characters, or about 3 sentences, or about 150 letters.

  Please advise the user's input:
  `;

function copied(text) {
  return `${text} 🪷`;
}

export const scheme = {
  buttons: {
    enter: "ask",
    paste: {
      label: "paste",
      pasted: "pasted",
      empty: "empty",
      failed: "failed",
    },
    copy: {
      label: "copy",
      copied: "copied",
      failed: "failed",
    },
  },
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
  return { title: "botce, how do i...", desc: "(ask botce a question)" };
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
  if (!input) return;
  if (input) input.text += "\n\n- botce";
  const botceIndex = input.text.indexOf("- botce");
  botce = input.prompt.pos(input.prompt.textToCursorMap[botceIndex]);
}

let lotus,
  lotusOsc = 0,
  backdrop,
  botce,
  keyboardSfx;

// 🥾 Boot
function boot({ get, net, resolution, screen, needsPaint, glaze, bgm }) {
  // resolution(screen.width, screen.height, 0);
  bgm.set(11, 0.5); // Track number and volume. (0-1)
  net.waitForPreload();
  net.preload("compkey").then((sfx) => (keyboardSfx = sfx)); // Keyboard sound.
  get
    .painting("2023.7.24.17.55.09")
    .by("@jeffrey")
    .then((p) => {
      net.preloaded();
      lotus = p;
      needsPaint();
    });
}

function sim({ needsPaint, simCount, system }) {
  const inc = system.prompt.input.lock ? 2 : 1;
  lotusOsc = (lotusOsc + inc) % 360; // Cycle the lotus.
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
  num,
  system,
  ink,
  help: { choose },
}) {
  if (
    !backdrop ||
    backdrop.width !== screen.width ||
    backdrop.height !== screen.height
  )
    backdrop = painting(screen.width, screen.height);
  page(backdrop);
  noise16Sotce(); // Or... wipe(252, 255, 237);
  if (
    lotus &&
    !system.prompt.input.canType &&
    system.prompt.input.commandSentOnce
  ) {
    const x = screen.width / 2 - lotus.width / 2;
    const yMod =
      Math.sin(num.radians(lotusOsc)) * (system.prompt.input.lock ? 20 : 10);
    paste(
      lotus,
      x + choose(-1, 0, 0, 0, 0, 0, 1),
      screen.height / 2 - lotus.height / 2 + choose(-1, 0, 0, 0, 0, 0, 1) + yMod
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

// 🖼️ Preview
function preview({ wipe, screen }) {
  const scale = 0.5;
  wipe(240, 200, 200)
    .noise16Sotce()
    .ink(255, 180)
    .box(0, 0, screen.width, screen.height)
    .paste(
      lotus,
      screen.width - lotus?.width * scale - 2,
      screen.height / 2 - (lotus?.height * scale) / 2,
      scale
    )
    .ink(250, 100, 150)
    .write("botce", { center: "y", x: 8, size: 3 });
}

// 🪷 Icon
function icon({ screen, wipe, noise16Sotce }) {
  const scale = 1.3;
  wipe(230, 150, 150)
    .noise16Sotce()
    .paste(
      lotus,
      screen.width / 2 - (lotus?.width * scale) / 2 + 2,
      screen.height / 2 - (lotus?.height * scale) / 2,
      scale
    );
}

// 🎪 Act
function act({ event: e, sound: { play }, num }) {
  if (e.is("keyboard:down") && e.key !== "Enter") {
    play(keyboardSfx, { volume: 0.2 + (num.randInt(100) / 100) * 0.4 });
  }
}

const forgetful = true;
export {
  boot,
  sim,
  prompt,
  before,
  meta,
  paint,
  copied,
  preview,
  reply,
  icon,
  act,
  forgetful,
};
//export const system = "prompt:character:gpt-3.5-turbo"; // or "prompt:code"
export const system = "prompt:character:gpt-4"; // or "prompt:code"
