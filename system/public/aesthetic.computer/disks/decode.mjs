// Decode, 23.06.14.11.29
// Converts token strings to poems.

/* #region 🏁 TODO
  - [] Add a custom rendered preview image / support on the server for
       for an AC rendered thumbnail.
#endregion */

import { GPT3BrowserTokenizer } from "../dep/gpt3-tokenizer/gpt3-tokenizer.js";

const tokenizer = new GPT3BrowserTokenizer({ type: "gpt3" });

const prompt = "Enter numbers to decode.";
let needsResolution = false;

// 🥾 Boot
async function boot({ store, system, params, resolution, display }) {
  resolution(
    display.width / 2.25,
    display.height / 2.25
  );
  if (params.length === 0) return;
  system.prompt.input.text = params.join(" ");
  await system.prompt.input.run(store);
}

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  const decoded = tokenizer.decode(text.split(" ")); // Devode tokens to text.
  $.system.prompt.input.prompt.wrap = "word";
  $.system.prompt.input.text = decoded;
  $.system.prompt.input.scheme = scheme; // Change to "reply" color scheme.
  $.system.prompt.input.replied($); // Set the UI state back to normal.
  return { replied: true };
}

// 🎪 Act
function act({ system: { prompt }, event: e }) {
  if (e.is("text-input:editable")) {
    prompt.input.prompt.wrap = "char";
    prompt.input.text = prompt.input.text;
    prompt.input.scheme = altScheme;
  }
  if (e.is("text-input:uneditable") && prompt.input.text.length > 0) {
    prompt.input.prompt.wrap = "word";
    prompt.input.text = prompt.input.text;
    prompt.input.scheme = scheme;
  }
  if (e.is("reframed")) {
    needsResolution = true;
  }
}

export const scheme = {
  dark: {
    fg: [230, 274, 224],
    bg: [189, 164, 166, 100],
    block: [0, 0, 0],
    blockHi: [255, 255, 255],
    line: [0, 0, 10],
  },
  light: {
    fg: [0, 200],
    bg: [179, 164, 166],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

const altScheme = {
  dark: {
    fg: [123, 66, 102, 120],
    bg: [230, 234, 224, 200],
    block: [123, 66, 102, 120],
    blockHi: [0, 0, 0],
    line: [230, 234, 224, 120],
  },
  light: {
    fg: [0, 200],
    bg: [230, 255, 40],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

// 🎨 Paint
function paint({ noiseTinted, resolution, display }) {
  if (needsResolution) {
    console.log(display);
    resolution(
      display.width / 2.25,
      display.height / 2.25
    );
    needsResolution = false;
  }
  noiseTinted([189, 164, 166], 0.8, 0.6);
}

function sim({ needsPaint, simCount }) {
  if (simCount % 4n === 0n) needsPaint();
}

export { boot, prompt, halt, act, paint, sim };
export const system = "prompt"; // or "prompt:code"
export const wrap = "word";
