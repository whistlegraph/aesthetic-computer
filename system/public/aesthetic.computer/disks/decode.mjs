// Decode, 23.06.14.11.29
// Converts token strings to poems.

/* #region 🏁 TODO
  - [] Add a custom rendered preview image / support on the server for
       for an AC rendered thumbnail.
#endregion */

import { GPT3BrowserTokenizer } from "../dep/gpt3-tokenizer/gpt3-tokenizer.js";

const tokenizer = new GPT3BrowserTokenizer({ type: "gpt3" });

const prompt = "Enter numbers to decode.";

// 🥾 Boot
async function boot({ store, system, params }) {
  if (params.length === 0) return;
  system.prompt.input.text = params.join(" ");
  await system.prompt.input.run(store);
}

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  const decoded = tokenizer.decode(text.split(" ")); // Devode tokens to text.
  $.system.prompt.input.text = decoded;
  $.system.prompt.input.scheme = scheme; // Change to "reply" color scheme.
  $.system.prompt.input.replied($); // Set the UI state back to normal.
  return { replied: true };
}

// 🎪 Act
function act({ system: { prompt }, event: e }) {
  if (e.is("text-input:editable")) prompt.input.scheme = altScheme;
  if (e.is("text-input:uneditable") && prompt.input.text.length > 0)
    prompt.input.scheme = scheme;
}

export const scheme = {
  dark: {
    fg: [0, 0, 0],
    bg: [210, 255, 40],
    block: [0, 0, 0],
    blockHi: [255, 255, 255],
    line: [0, 0, 10],
  },
  light: {
    fg: [0, 200],
    bg: [170, 150, 200],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

const altScheme = {
  dark: {
    fg: [50, 255, 0],
    bg: [10, 20, 20],
    block: [255, 255, 255],
    blockHi: [0, 0, 0],
    line: [0, 0, 10],
  },
  light: {
    fg: [0, 200],
    bg: [230, 255, 40],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

export { boot, prompt, halt, act };
export const system = "prompt"; // or "prompt:code"
export const wrap = "word";
