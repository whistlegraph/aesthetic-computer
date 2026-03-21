// Textfence, 2023.8.09.14.21.22
// A dynamic poem for two characters and two words.
// Text by Georgica Pettus

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Launch Process
  + Next Version
  - [] The speakers / pan channels should be reversed for iOS?
       (This is probably an audio processor thing.)
  - [] Preload the audio for the next step before showing the words.
  + Done
  - [x] Mint on Zora using the production URL. 
  - [x] Finalize words / great narrative.
  - [x] Make sure there are no repeats.
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

const words = [
  // Scene 1 (Baby, no ego, just interaction)
  {
    left: ["feel", "be", "grow", "begin", "search", "click", "open"],
    right: ["here", "now", "there", "slowly", "fully", "soon", "this"],
  },
  // Scene 2 (Dos and Donts / Rules / Structure)
  {
    left: ["will", "don't", "can", "can't"],
    right: ["be", "help", "come", "talk", "run", "share"],
  },
  // Scene 3 (Dialogue)
  {
    left: ["i'm", "it's", "that's", "they're", "you're", "we're", "who's"],
    right: [
      "trying",
      "cold",
      "easy",
      "stupid",
      "beautiful",
      "lying",
      "everything",
      "lost",
      "simple",
      "bitter",
    ],
  },
  // Scene 4 (Death / existentialism)
  {
    left: ["that's", "it's", "i'm", "you're all", "everything is", "no one's"],
    right: ["right", "done", "gone", "missing", "sorry", "dead"],
  },
];

let left, right;
let leftDeck, rightDeck;
let wordsIndex = 0;

const turns = []; // Generate turn amount in advance to keep global fence
// progress.
let totalTurns, groupTurnsAmt;
let currentTurn = 0,
  groupTurns = 0;

let speaking = false;
let needsGen = false;
let voiceFemale, voiceMale;
let curtain = false;
let muted = false;

let l = 0,
  r = 0;
const charWidth = 6;
let textColor = "white";
let textBlink = 0; // Store the shrinking blink delay.
const textBlinkTime = 120 * 0.5; // Delay for blink.
let textBlinkCallback; // A function that runs after the delay. (Speaks)

// 🥾 Boot
function boot($) {
  voiceFemale = $.params[0] ? parseInt($.params[0]) : 18; // $.num.randInt(22);
  voiceMale = $.params[1] ? parseInt($.params[1]) : 22; // $.num.randInt(22);
  console.log("🗣️ Voices chosen:", "Female:", voiceFemale, "Male:", voiceMale);
  $.cursor("native");
  gen($);

  // console.log(leftDeck, rightDeck);
  const clickIndex = leftDeck.indexOf("click");
  const hereIndex = rightDeck.indexOf("here");
  l = leftDeck[clickIndex];
  r = rightDeck[hereIndex];
  leftDeck.splice(clickIndex, 1);
  rightDeck.splice(hereIndex, 1);
}

// 🧮 Sim
function sim($) {
  if (textBlink > 0) {
    // 1
    textBlink = textBlink - 1; // Subtract 1 from textBlink.
    if (textBlink === 0) {
      textColor = "white";
      textBlinkCallback();
    }
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, typeface, num }) {
  wipe(0);

  const cx = screen.width / 2;
  const cy = screen.height / 2;

  const progress = 1 - (totalTurns - currentTurn) / totalTurns;
  const gap = 7; // Max 1 space.
  ink(curtain ? 128 : 255).line(cx, cy - 20, cx, cy + 2);

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
  ink(curtain ? 128 : textColor).write(l, { center: "y", x: leftX });

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
  ink(curtain ? 128 : textColor).write(r, { center: "y", x: rightX });

  if (muted) {
    ink([255, 0, 0, 127]).write("MUTE", { x: 6, y: screen.height - 14 });
  }
}

// 🎪 Act
function act($) {
  const {
    event: e,
    help: { flip, shuffleInPlace },
    speak,
    num,
  } = $;

  if (e.is("lift") && speaking) {
    muted = !muted;
  }

  if (e.is("touch") && !speaking) {
    curtain = true;
  }

  if (e.is("lift") && !speaking) {
    curtain = false;
  }

  if ((e.is("lift") && !speaking) || (e.is("speech:completed") && speaking)) {
    speaking = true;
    let voice;

    groupTurns += 1;
    currentTurn += 1;

    console.log(
      "🎴 Group turns left:",
      `${groupTurnsAmt - groupTurns + 1}/${groupTurnsAmt}`,
      "⌛ Total turns left:",
      `${totalTurns - currentTurn + 1}/${totalTurns}`,
    );
    if (groupTurns === groupTurnsAmt) needsGen = true;

    if (needsGen) {
      gen($);
      console.log("⚔️ New word set...");
      l = leftDeck.pop();
      r = rightDeck.pop();
      textColor = "black";
      textBlink = textBlinkTime; // Setting textBlink to 30.
      textBlinkCallback = function () {
        const maleUtterance = utteranceFor("male", `${l} ${r}`, num);
        const femaleUtterance = utteranceFor("female", `${l} ${r}`, num);
        const completed = flip();
        speak(femaleUtterance, `female:${voiceFemale}`, "cloud", {
          pan: -panSway,
          volume: muted ? 0 : 1,
          skipCompleted: !completed,
        });
        speak(maleUtterance, `male:${voiceMale}`, "cloud", {
          pan: panSway,
          volume: muted ? 0 : 1,
          skipCompleted: completed,
        });
      };
    } else {
      let pan = 0;
      if (flip()) {
        l = leftDeck.pop();
        if (leftDeck.length === 0) {
          leftDeck = left.slice();
          const index = leftDeck.indexOf(l);
          if (index !== -1) leftDeck.splice(index, 1);
          shuffleInPlace(leftDeck);
        }
        console.log("left deck:", leftDeck);
        voice = `female:${voiceFemale}`;
        pan = -panSway;
        textColor = "white";
      } else {
        r = rightDeck.pop();
        if (rightDeck.length === 0) {
          rightDeck = right.slice();
          const index = rightDeck.indexOf(r);
          if (index !== -1) rightDeck.splice(index, 1);
          shuffleInPlace(rightDeck);
        }
        console.log("right deck:", rightDeck);
        voice = `male:${voiceMale}`;
        pan = 1;
        textColor = "white";
      }
      const utterance = utteranceFor(voice, `${l} ${r}`, num);
      speak(utterance, voice, "cloud", { pan, volume: muted ? 0 : 1 });
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
    title: "textfence",
    desc: "A dynamic line punctuated by a vertical fence.",
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

  const i = wordsIndex; // num.randInt(lefts.length - 1);
  left = words[i].left;
  right = words[i].right;

  if (wordsIndex === 0) {
    // Generate all random turns.
    for (let i = 0; i < words.length; i += 1) {
      turns[i] = num.randIntRange(8, 12);
    }
    totalTurns = num.add(turns);
    currentTurn = 0;
  }

  groupTurns = 0;
  groupTurnsAmt = turns[wordsIndex];

  wordsIndex = (wordsIndex + 1) % words.length;

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

export const nohud = true;