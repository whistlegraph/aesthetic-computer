// Textfence, 2023.8.09.14.21.22
// A dynamic poem for two characters and two words.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Finalize words / great narrative.
  - [] Create global screenshot feature and take square screenshot for thumbnail.
  - [] Make a test mint on Zora:
       - [] Wait for parameters from Sam.
       - https://docs.zora.co/docs/smart-contracts/creator-tools/ZoraNFTCreator
       - Mint the piece on a testnet.
  + Launch Process
  - [] Deploy it / publish with Sam. (Tech)
  + Done
  - [x] Experiment with ssml.
  - [c] Generate all speech audio files to avoid hitting GCP on every request.
  - [x] Fully blank out the two words after each group change. 
    - [x] Optically center the text by getting the right-most drawn point
       of the left word and the left-most drawn point of the right word,
        spacing accordingly.
  - [x] Make the fence white?
  - [x] Rename to `textfence`.
  - [x] What voices to use.
  - [x] Figure out starting screen - press/tap/click here/me/now
  - [x] Word groups
    - [x] Some groups smaller than others? Just one group? Linear vs random?
  - [x] What happens on network failure? 
    - [x] It should pause and keep retrying...
  - [x] Implement a local cache in `speech` for already spoken phrases.
  - [x] Randomly choose a voice set from 0-22.
  - [x] Pan both voices left and right. 
  - [x] Replace speech synthesis with a cloud API and/or use
    - https://jankapunkt.github.io/easy-speech? or https://www.masswerk.at/mespeak/#download
    - https://responsivevoice.org/text-to-speech-languages/us-english-text-to-speech/
       the call fails?
#endregion */

const panSway = 0.9; // How much to pan each voice left or right.

const lefts = [
  // ["need", "won't", "can", "will", "can't", "shouldn't"],
  ["sit", "be", "not", "run", "keep", "stop", "cry", "click"],
  ["will", "won't", "can", "can't"],
  ["track", "see", "check", "save", "charge", "become", "worship"],
  ["i'm", "it's", "that's", "they're", "you're", "we're", "who's"],
];

const rights = [
  // ["help", "come!", "scream?", "be", "run?", "talk...", "act", "this"],
  ["here", "now", "down", "there", "slowly", "fully", "soon", "this"],
  ["be", "help", "come", "talk"],
  [
    "down",
    "backwards",
    "bear",
    "cellphone",
    "100 dollars",
    "forward",
    "something",
  ],
  [
    "trying",
    "cold",
    "easy",
    "stupid",
    "beautiful",
    "lying",
    "dead",
    "dying",
    "everything",
    "lost",
    "simple",
  ],
];

let left, right;
let leftDeck, rightDeck;
let wordsIndex = 0;

let groupTurnsMin, // 8-15
  groupTurns = 0;

let speaking = false;
let needsGen = false;
let newGen = false;
let voiceFemale, voiceMale;

let l = 0,
  r = 0;
const charWidth = 6;
let textColor = "white";
let textBlink = 0; // Store the shrinking blink delay.
const textBlinkTime = 120 * 1.5; // Delay for blink.
let textBlinkCallback; // A function that runs after the delay. (Speaks)

// 🥾 Boot
function boot($) {
  voiceFemale = $.params[0] ? parseInt($.params[0]) : 18; // $.num.randInt(22);
  voiceMale = $.params[1] ? parseInt($.params[1]) : 22; // $.num.randInt(22);
  console.log("🗣️ Voices chosen:", "Female:", voiceFemale, "Male:", voiceMale);
  $.cursor("native");
  gen($);

  const clickIndex = leftDeck.indexOf("click");
  const hereIndex = rightDeck.indexOf("here");
  l = leftDeck[clickIndex];
  r = rightDeck[hereIndex];
  leftDeck.splice(clickIndex, 1);
  rightDeck.splice(hereIndex, 1);
}

// 🧮 Sim
function sim($) {
  if (textBlink > 0) { // 1
    textBlink = textBlink - 1; // Subtract 1 from textBlink.
    if (textBlink === 0) {
      textColor = "white";
      textBlinkCallback();
    }
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, typeface }) {
  wipe(0);
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  const gap = 7;

  ink(255).line(cx, cy - 20, cx, cy + 2);

  // ⬅️ Left Word
  const leftWidthMinusOneCharacter = (l.length - 1) * charWidth;
  const lastLeftCharacter = l[l.length - 1];

  let leftMaxX = 0;
  {
    const commands = typeface.glyphs[lastLeftCharacter].commands;
    for (let i = 0; i < commands.length; i += 1) {
      const command = commands[i];
      if (command.args[0] > leftMaxX) leftMaxX = command.args[0];
      if (command.name === "line") {
        if (command.args[2] > leftMaxX) leftMaxX = command.args[2];
      }
    }
  }

  const leftX = cx - leftWidthMinusOneCharacter - leftMaxX - gap;

  // const leftRightSideX = leftX + leftWidthMinusOneCharacter + leftMaxX;
  // ink().line(leftRightSideX, 0, leftRightSideX, screen.height);

  ink(textColor).write(l, {
    center: "y",
    x: leftX,
  });

  // ➡️ Right Word
  const firstRightCharacter = r[0];

  let rightMinX = charWidth;
  {
    const commands = typeface.glyphs[firstRightCharacter].commands;
    for (let i = 0; i < commands.length; i += 1) {
      const command = commands[i];
      if (command.args[0] < rightMinX) rightMinX = command.args[0];
      if (command.name === "line") {
        if (command.args[2] < rightMinX) rightMinX = command.args[2];
      }
    }
  }

  const rightX = cx - rightMinX + gap;
  ink(textColor).write(r, { center: "y", x: rightX });
}

// 🎪 Act
function act($) {
  const {
    event: e,
    help: { flip, shuffleInPlace },
    speak,
    num,
  } = $;
  if ((e.is("touch") && !speaking) || (e.is("speech:completed") && speaking)) {
    speaking = true;
    let voice;

    groupTurns += 1;
    console.log(
      "🎴 Turns left:",
      `${groupTurnsMin - groupTurns + 1}/${groupTurnsMin}`
    );
    if (groupTurns === groupTurnsMin) needsGen = true;

    if (newGen) {
      console.log("⚔️ New word set...");
      newGen = false;
      l = leftDeck.pop();
      r = rightDeck.pop();
      textColor = "black";
      textBlink = textBlinkTime; // Setting textBlink to 30.
      textBlinkCallback = function () {
        const maleUtterance = utteranceFor("male", `${l} ${r}`, num);
        const femaleUtterance = utteranceFor("female", `${l} ${r}`, num);
        speak(femaleUtterance, `female:${voiceFemale}`, "cloud", {
           pan: -panSway
        });
        speak(maleUtterance, `male:${voiceMale}`, "cloud", {
          pan: panSway,
          skipCompleted: true,
        });
      };
    } else {
      let pan = 0;
      if (flip()) {
        l = leftDeck.pop();
        if (leftDeck.length === 0) {
          leftDeck = left.slice();
          shuffleInPlace(left);
        }
        voice = `female:${voiceFemale}`;
        pan = -panSway;
        textColor = "white";
      } else {
        r = rightDeck.pop();
        if (rightDeck.length === 0) {
          rightDeck = right.slice();
          shuffleInPlace(right);
        }
        voice = `male:${voiceMale}`;
        pan = 1;
        textColor = "white";
      }
      const utterance = utteranceFor(voice, `${l} ${r}`, num);
      speak(utterance, voice, "cloud", { pan });
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
    title: "click | here • textfence",
    desc: "A dynamic poem for two characters and two words.",
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
  // speaking = false;
  needsGen = false;
  groupTurns = 0;
  groupTurnsMin = num.randIntRange(8, 15);

  const i = wordsIndex; // num.randInt(lefts.length - 1);
  left = lefts[i];
  right = rights[i];

  wordsIndex = (wordsIndex + 1) % lefts.length;

  (leftDeck = left.slice()), (rightDeck = right.slice());

  shuffleInPlace(leftDeck);
  shuffleInPlace(rightDeck);
}

function utteranceFor(voice, text, num) {
  let rate, pitch;
  if (voice.startsWith("female")) {
    rate = `${num.randIntRange(90, 105)}%`;
    pitch = `+${num.randIntRange(10, 35)}%`;
  } else {
    rate = `${num.randIntRange(80, 105)}%`;
    pitch = `${num.randIntRange(-15, 0)}%`;
  }
  return `
  <speak>
    <prosody rate="${rate}" pitch="${pitch}">${text}</prosody>
  </speak>
  `;
}