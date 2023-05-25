// Gargoyle, 23.05.21.17.59
// Georgica's prompt programming playground.

/* #region 🏁 TODO
  + Poetic
  - [] Write several characters.
  + Techical
  - [] Decide how to get back to main navigation page *especially on mobile
  - [] Add sound for `Music Box`
  - [] Mobile tap to open keyboard should function better...
    - []
  - [] Multiple prompts on a page.
    - [] Make sure history works among the different prompts.
    - [] Line breaks
  - [] Change `network failure` to `try again`
  - [] How to deal with longer responses?
  - [] Add some basic conversational support
  - [] Add a small illustration on bottom right corner.
    - [] Painting @import.
    - [] How would this function? `paste(by("@georgica").painting())`
    - [] Make the illustration:
      - [] Implement a zoomable canvas on `rect`.
        - [] Rename `rect` to `box`.
      - [] Implement a zoomable canvas on `line`.
  - [] Add some sound.
    - [] 1/3 New synth wav types!
    - [] Custom SFX / sampling.
  + Done
  - [x] Cancellable responses.
  - [x] Movable cursor support, with arrow keys and touch to move or drag. 
    - [x] Draw character once more on top of cursor.
          $.ink(255, 0, 0).draw
    - [x] Reset cursor position on return.
    - [x] Paste needs to work on movable cursor.
  - [x] Auto-wrap the text by word in TextInput objects.
  - [x] Better text typing / replacing experience / autotype clear characters
        and skip intro space. 
  - [x] Visual failure messages if offline or given a cancelled request.
  - [x] Progress spinner / prevent interaction.
#endregion */

// const hint = "char";
const hint = "code";

// // 🗨️ ??? Music Box
const prompt = "sing these notes";
const program = {
  before: `
  I have designed a musical program that takes in song titles and generates notes formatted using only letters a-g

  There is no other information in the output other than the notes a-g.
  There are no spaces between the letters.

  My musical program does not state what it is doing.

  Other constraints:
    - My program is case sensitive and all notes must be lowercase.
    -
   
what are the notes of: `,

after: 

  `
  Please remember that...

   - My musical program only accept a maximum of 32 notes with no spaces between letters.
  
   Now print a string of 32 notes and nothing else so I can input that into my program with no crashes. Output nothing else, just the code.

   therefore the notes for the input would be:
  `,
};

// // 🗨️ ??? Chain of Thought
// const prompt = "";
// const program = {
//   before: `
//  Jeffrey's Belly contents: Orange Creamsicles, Tapioca, Nuts

//   Q: Are oranges in Jeffrey's belly?
//   A: No there no oranges in Jeffrey's belly.

//   Q: Are there orange creamsicles in Jeffrey's Belly?
//   A: Yes there are 3 orange creamsicles in Jeffrey's Belly.

//   Jeffrey's Belly contents: Orange Creamsicles, Tapioca, Nuts

//   Q: What about
//   `,
//   after: ` in his belly?`,
// };

// // 🗨️ ??? SavCom
// const prompt = "what's wrong, baby?";
// const program = {
//   before:`
//   - you're playing a character who lovingly suggests a solution to the user's
//   problems
//   - an example is, input: "I'm so stressed out," output: "I'm sorry baby, you
//   work so hard, you should really take a break."
//   - you are replying to:`,
//   after: `
//   - you always call the user "baby"
//   - and your responses are only one sentence.

//   `,
// };

// // 🗨️ ??? Harold
// const prompt = "what are you thinking about?";
// const program = {
//   before:`
//   - you are a character that is a bird
//   - and you only write about other birds
//   - and the birds you describe are imaginary
//   - and you do not name them
//   - and you give physical descriptions
//   - and your descriptions are under 100 characters
//   - and your response does not state what it is going to do
//   - you input is:`,
//   after: `
//   - you describe an imaginary bird and mention its colors
//   - your responses always begin with "a bird flew by with..."

//   `,
// };

// // 🗨️ ??? Music Box
// const prompt = "write a lyric";
// const program = {
//   before:`
//   - you're playing a character who writes music
//   - you are replying to:`,
//   after: `
//   - you reply only in musical notation

//   `,
// };

// 🗨️ ??? Liar
// const prompt = "ask me anything but don't expect the truth";
// const program = {
//   before:`
//   - you're playing a character who is a liar
//   - you are replying to:`,
//   after: `
//   - you provide a false answer to questions
//   - you never say that you are lying, or are a liar

//   `,
// };

// 🗨️ ??? Liar
// const prompt = "ask me anything but don't expect the truth";
// const program = {
//   before:`
//   - you're playing a character who is a liar
//   - you are replying to:`,
//   after: `
//   - you provide a false answer to questions
//   - you never say that you are lying, or are a liar

//   `,
// };

// 🗨️ Alphabetical poet
// const prompt = "hi georgica";
// const program = {
//   before:`
//   - You respond to the input in nonsensical sentences
//   - And the words in your sentences are in alphabetical order.`,
//   after: `The words in your responses are in alphabetical order, beginning anywhere in the alphabet. You don't have to include every letter in the alphabet.`,
// };

// 🗨️ Primitive gargoyle adaptation
// const prompt = "hi georgica";
// const program = {
//   before: `
//   your name is gargoyle
//   and you are replying to:
//   `,
//   after: `
//   - and you sometimes respond in metaphor
//   - all responses are limited to 100 characters
//   - all responses provide advice
//   - some responses include questions
//   - you are apologetic if user suggests offense
//   - your tone is familiar
//   `,
// };

import { TextInput } from "../lib/type.mjs";
import { ask } from "../lib/ask.mjs";

let input,
  controller,
  messageComplete = false,
  processing = false;

let notes = [];

// 🥾 Boot (Runs once before first paint and sim)
async function boot($) {
  input = new TextInput(
    $,
    prompt,
    async (text) => {
      input.blank();
      processing = input.lock = true;
      controller = ask(
        { prompt: text, program, hint },
        function and(msg) {
          input.text += msg;
        },
        function done() {
          // TODO: Play a sound?

          notes = [...input.text];
          console.log("😀 About to perform:", notes);
          input.cursor = "stop";
          messageComplete = true;
          processing = input.lock = false;
        },
        function fail() {
          input.text = "NETWORK FAILURE";
          input.cursor = "stop";
          messageComplete = true;
          processing = input.lock = false;
        }
      );
    },
    { autolock: false, wrap: "word" }
  );
}

function beat({ sound: { square } }) {
  if (notes.length > 0) {
    const note = notes.shift();

    const tones = {
      a: 440,
      b: 493.88,
      c: 261.63,
      d: 293.66,
      e: 329.63,
      f: 349.23,
      g: 392,
    };

    const hz = tones[note];

    square({
      tone: hz,
      beats: 1,
      attack: 0.02,
      decay: 0.97,
      volume: 0.35,
    });
  }
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($) {
  input?.sim($);
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  $.wipe(0);
  return input?.paint($);
}

// ✒ Act (Runs once per user interaction)
function act($) {
  const { event: e } = $;

  if (!messageComplete && processing) {
    if (e.is("keyboard:down:escape")) {
      console.log(controller);
      controller?.abort();
    }
  }

  if (!messageComplete && !processing) input?.act($);
  if (messageComplete && (e.is("keyboard:down") || e.is("touch"))) {
    input.blank("blink"); // Clear input and switch back to blink cursor.
    input?.act($); // Capture any printable keystrokes.
    messageComplete = false;
  }
}

function leave() {
  controller?.abort();
}

export { boot, sim, paint, act, beat, leave };

// 📚 Library (Useful functions used throughout the piece)
// ...
