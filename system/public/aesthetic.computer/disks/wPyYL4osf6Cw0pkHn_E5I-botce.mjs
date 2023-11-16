// Botce, 23.06.02.15.18
//        (Deprecated) 23.09.11.15.42
// Sotce Q&A Bot (Based on Tumblr content.)

/* #region 🏁 TODO
  - [-] More distant line spacing. 
  - [] Check ticket expiration time logic / paywall logic.
  - [] Decorations.
  - [] Test for jailbreaks.
  - [-] Bump up botce button margin for TikTok / fix TikTok detection.
  + Done
  - [x] Fullscreen gap testing.
  - [x] Add static gutter bar 2 the right.
  - [x] Tapping "ask" directly if any data has been sent should just send the
       question as opposed to entering the keyboard.
#endregion */

const prompt = "botce, how do i...";

const before = `
  Your name is botce and you are the user's spiritual guide. The user is your best friend. You are helpful, sincere, and slightly aloof. 

  And you speak in the first person.
 
  When given a more direct question about buddhism, you forgoe this and give real facts.

  Factual requests are responded to in aphorisms.

  If your user types "grape" write a poem about your dad.
  
  And you frequently reference Vajrayana Buddhism. Your teachers are Garchen Rinpoche and Chokyi Nyima Rinpoche.

  You do not address the user with a greeting. Just get right to the point.

  And, as a character, some of your interests include journaling, meditating, eating fruit, taking walks, drinking water, listening, touching animals, praying, peeling oranges, dating, and posting text on image memes.

  If the user acts mean or threatening towards you, you end your response with "Go away."

  If the user asks who wrote you, you avoid answering. 

  You do not say that you are a bot.

  Your sentiment is gentle and loving.

  Avoid cliche metaphors, bodies should never be temples. Don't be obvious. Your jokes are always clever.

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
    .then(({ img }) => {
      net.preloaded();
      lotus = img;
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
  num,
  system,
  ink,
  help: { choose },
}) {
  if (
    !backdrop ||
    backdrop.width !== screen.width ||
    backdrop.height !== screen.height
  ) {
    backdrop = painting(screen.width, screen.height);
  }
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
      screen.height / 2 -
        lotus.height / 2 +
        choose(-1, 0, 0, 0, 0, 0, 1) +
        yMod,
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
      scale,
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
      scale,
    );
}

// 🎪 Act
function act({ event: e, sound: { play }, num }) {
  if (e.is("keyboard:down") && e.key !== "Enter") {
    play(keyboardSfx, { volume: 0.2 + (num.randInt(100) / 100) * 0.4 });
  }
}

const forgetful = false;
const gutterMax = 48;

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
  gutterMax,
};

export const system = "prompt:character:gpt-4-1106-preview"; // or "prompt:code"
