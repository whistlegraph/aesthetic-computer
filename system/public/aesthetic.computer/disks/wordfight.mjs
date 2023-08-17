// Wordfight, 2023.8.09.14.21.22
// A dynamic back and forth for two people and two words.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Choose a voice set from 0-22.
  - [] What happens on network failure? 
  - [] Is it possible to pan the speaker?
  - [x] Replace speech synthesis with a cloud API and/or use
    - https://jankapunkt.github.io/easy-speech? or https://www.masswerk.at/mespeak/#download
    - https://responsivevoice.org/text-to-speech-languages/us-english-text-to-speech/
       the call fails?
#endregion */

const lefts = [
  ["need", "won't", "can", "will", "can't", "shouldn't"],
  ["sit", "be", "not", "run", "keep", "stop", "cry"],
  ["i'm", "it's", "that's", "they're", "you're", "we're", "who's"],
  ["track", "see", "check", "smell", "charge", "become", "worship"],
];

const rights = [
  ["help", "come!", "scream?", "be", "run?", "talk...", "act"],
  ["here", "now", "down", "there", "slowly", "fully", "soon"],
  ["trying", "cold", "easy", "stupid", "beautiful", "lying", "dead"],
  [
    "down",
    "backwards",
    "bear",
    "cellphone",
    "100 dollars",
    "forward",
    "something",
  ],
];

let left, right;
let needs, helps;
let speaking = false;
let needsGen = false;
let newGen = false;
let voiceSet = 10;

let n = 0,
  h = 0;
const charWidth = 6;
let textColor = "white";

// 🥾 Boot
function boot($) {
  // $.resolution(160, 160);
  if ($.params[0]) voiceSet = parseInt($.params[0]);
  $.cursor("native");
  gen($);
  n = needs.pop();
  h = helps.pop();
}

// 🧮 Sim
function sim($) {
  if ($.simCount % 1000n === 0n) {
    needsGen = true;
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen }) {
  wipe(0);
  const cx = screen.width / 2;
  if (speaking) {
    ink(textColor).write(n, {
      center: "y",
      x: cx - charWidth * n.length - charWidth / 4,
    });
    ink(textColor).write(h, { center: "y", x: cx + h.length / 2 + 3 });
  } else {
    ink(255).write(n, {
      center: "y",
      x: cx - charWidth * n.length - charWidth / 4,
    });
    ink(255).write(h, { center: "y", x: cx + h.length / 2 + 3 });
  }
  ink(64).line(cx, 0, cx, screen.height);
}

// 🎪 Act
function act($) {
  const {
    event: e,
    help: { flip, shuffleInPlace },
    speak,
  } = $;
  if ((e.is("touch") && !speaking) || e.is("speech:completed")) {
    speaking = true;
    let voice;

    if (newGen) {
      newGen = false;
      n = needs.pop();
      h = helps.pop();
      textColor = "white";
      speak(n + " " + h, `female:${voiceSet}`, "cloud");
      speak(n + " " + h, `male:${voiceSet}`, "cloud", { skipCompleted: true });
    } else {
      if (flip()) {
        n = needs.pop();
        if (needs.length === 0) {
          needs = left.slice();
          shuffleInPlace(left);
        }
        voice = `female:${voiceSet}`;
        textColor = "white";
      } else {
        h = helps.pop();
        if (helps.length === 0) {
          helps = right.slice();
          shuffleInPlace(right);
        }
        voice = `male:${voiceSet}`;
        textColor = "white";
      }
      speak(n + " " + h, voice, "cloud");
    }

    if (needsGen) {
      gen($);
      newGen = true;
    }
  }
  // Respond to user input here.
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Wordfight",
    desc: "A dynamic back and forth for two people and two words.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, sim, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

function gen({ help: { shuffleInPlace }, num }) {
  console.log("⚔️ Switching word set...");
  // speaking = false;
  needsGen = false;
  const i = num.randInt(lefts.length - 1);
  left = lefts[i];
  right = rights[i];

  (needs = left.slice()), (helps = right.slice());

  shuffleInPlace(needs);
  shuffleInPlace(helps);
}
