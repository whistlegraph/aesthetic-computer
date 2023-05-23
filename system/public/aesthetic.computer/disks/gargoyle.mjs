// Gargoyle, 23.05.21.17.59
// Georgica's prompt programming playground.

/* #region 🏁 todo
  - [] Movable cursor support, with arrow keys and touch to move or drag. 
    - [] Paste needs to work on movable cursor.
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
  - [x] Auto-wrap the text by word in TextInput objects.
  - [x] Better text typing / replacing experience / autotype clear characters
        and skip intro space. 
  - [x] Visual failure messages if offline or given a cancelled request.
  - [x] Progress spinner / prevent interaction.
#endregion */

const prompt = "hi georgica";
const program = {
  before:`You only respond in nonsense where the words are in alphabetical order.`,
  after: `The words in your responses are in alphabetical order, beginning anywhere in the alphabet. You don't have to include every letter in the alphabet.`,
};

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
  messageComplete = false,
  processing = false;

// 🥾 Boot (Runs once before first paint and sim)
async function boot($) {
  input = new TextInput(
    $,
    prompt,
    (text) => {
      input.blank();
      processing = input.lock = true;
      ask(
        { prompt: text, program, hint: "char" },
        function and(msg) {
          input.text += msg;
        },
        function done() {
          // TODO: Play a sound?
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
  if (!messageComplete && !processing) input?.act($);
  if (messageComplete && (e.is("keyboard:down") || e.is("touch"))) {
    input.blank("blink"); // Clear input and switch back to blink cursor.
    input?.act($); // Capture any printable keystrokes.
    messageComplete = false;
  }
}

export { boot, sim, paint, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
