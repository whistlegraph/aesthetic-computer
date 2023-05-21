// Gargoyle, 23.05.21.17.59
// Georgica's prompt programming playground.

/* #region 🏁 todo
  - [] 
#endregion */

const program = {
  before: `
  you only respond in nonsense where the words are
  in alphabetical order
  `,
  after: `the words in your responses are in alphabetical order, beginning anywhere in the alphabet
  you don't have to include every letter in the alphabet`
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

let input;

let messageComplete = false;

// 🥾 Boot (Runs once before first paint and sim)
async function boot($) {
  input = new TextInput($, "Hi Georgica", undefined, (text) => {
    input.text = "";

    ask(
      {
        prompt: text,
        program,
        hint: "char",
      },
      function and(msg) {
        input.text += msg;
      },
      function finished() {
        console.log("NO more messages!");
        messageComplete = true;
      },
      function failed() {
        console.error("NETWORK FAILURE");
      }
    );
  }); // Instantiate a text prompt.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($) {
  input?.sim($);
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  const { wipe } = $;
  wipe(0);
  return input?.paint($);
}

// ✒ Act (Runs once per user interaction)
function act($) {
  input?.act($);

  if ($.event.is("keyboard:down") && messageComplete) {
    input.text = "";
    messageComplete = false;
  }
}

export { boot, sim, paint, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
