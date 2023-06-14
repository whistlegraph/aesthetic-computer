// Encode, 23.06.11.14.33
// Converts poems to token strings.

/* #region 🏁 TODO

#endregion */

import { GPT3BrowserTokenizer } from "../dep/gpt3-tokenizer/gpt3-tokenizer.js";

const tokenizer = new GPT3BrowserTokenizer({ type: "gpt3" });

const prompt = "write a poem";

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  const encoded = tokenizer.encode(text); // Encode text into tokens.
  $.system.prompt.input.text = encoded.bpe.join(" "); // Join ints into text.
  $.system.prompt.input.scheme = altScheme; // Change to "reply" color scheme.
  $.system.prompt.input.replied(); // Set the UI state back to normal.
  return true;
}

function editable(input) {
  input.scheme = scheme; // Flip the color scheme back to original.
}

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
    bg: [170, 150, 200],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

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
    bg: [230, 255, 40],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

export { prompt, halt, editable };
export const system = "prompt"; // or "prompt:code"
export const wrap = "word";