// Need Help, 2023.8.09.14.21.22
// A dynamic statement of self-help.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// const left = ["need", "won't", "can", "will", "can't", "shouldn't"];
// const right = ["help", "come!", "scream?", "be", "run?", "talk...", "act"];
// const left = ["sit", "be", "not", "run", "keep", "stop", "cry"];
// const right = ["here", "now", "down", "there", "slowly", "fully", "soon"];

// const left = ["i'm", "it's", "that's", "they're", "you're", "we're", "who's"];
// const right = ["trying", "cold", "easy", "stupid", "beautiful", "lying", "dead"];

const left = ["track", "see", "check", "smell", "charge", "become", "worship"];
const right = ["down", "backwards", "bear", "cellphone", "100 dollars", "forward", "something"];

let needs = left.slice(),
  helps = right.slice();
let speaking = false;

let n = 0,
  h = 0;
const charWidth = 6;
let textColor = "white";

// 🥾 Boot
function boot({ wipe, ink, line, help: { shuffleInPlace } }) {
  // Runs once at the start.
  shuffleInPlace(needs);
  shuffleInPlace(helps);
  n = needs.pop();
  h = helps.pop();
}

// 🧮 Sim
function sim({ simCount, num, help: { flip, shuffleInPlace }, speak }) {
  if (simCount % 250n === 0n) {
    // let voice;
    // if (flip()) {
    //   n = needs.pop();
    //   if (needs.length === 0) {
    //     needs = need.slice();
    //     shuffleInPlace(need);
    //   }
    //   voice = "female";
    // } else {
    //   h = helps.pop();
    //   if (helps.length === 0) {
    //     helps = help.slice();
    //     shuffleInPlace(help);
    //   }
    //   voice = "male";
    // }
    // speak(n + " " + h, voice);
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen }) {
  wipe(0);
  const cx = screen.width / 2;
  ink(textColor).write(n, {
    center: "y",
    x: cx - charWidth * n.length - charWidth / 4,
  });
  ink(textColor).write(h, { center: "y", x: cx + h.length / 2 + charWidth / 4 });
  // ink(64).line(cx, 0, cx, screen.height);
}

// 🎪 Act
function act({ event: e, help: { flip, shuffleInPlace }, speak }) {
  if ((e.is("touch") && !speaking) || e.is("speech:completed")) {
    speaking = true;
    let voice;
    if (flip()) {
      n = needs.pop();
      if (needs.length === 0) {
        needs = left.slice();
        shuffleInPlace(left);
      }
      voice = "female";
      textColor = "white";
    } else {
      h = helps.pop();
      if (helps.length === 0) {
        helps = right.slice();
        shuffleInPlace(right);
      }
      voice = "male";
      textColor = "white";
    }
    speak(n + " " + h, voice);
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
    title: "Need Help",
    desc: "A dynamic statement of self-help.",
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
