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
async function boot({ store, system, params, resolution, screen }) {
  resolution(screen.width / 1.2, screen.height / 1.2);
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
    text: [230, 274, 224],
    background: [189, 164, 166, 100],
    block: [0, 0, 0],
    highlight: [255, 255, 255],
    guideline: [0, 0, 10],
  },
  light: {
    text: [0, 200],
    background: [179, 164, 166],
    block: [30, 200, 200],
    highlight: [200, 200, 30],
    guideline: [0, 0, 0, 128],
  },
};

const altScheme = {
  dark: {
    text: [123, 66, 102, 120],
    background: [230, 234, 224, 200],
    block: [123, 66, 102, 120],
    highlight: [0, 0, 0],
    guideline: [230, 234, 224, 120],
  },
  light: {
    text: [0, 200],
    background: [230, 255, 40],
    block: [30, 200, 200],
    highlight: [200, 200, 30],
    guideline: [0, 0, 0, 128],
  },
};

// 🎨 Paint
function paint({ noiseTinted }) {
  noiseTinted([189, 164, 166], 0.8, 0.6);
}

function sim({ needsPaint, simCount }) {
  if (simCount % 4n === 0n) needsPaint();
}

export { boot, prompt, halt, act, paint, sim };
export const system = "prompt"; // or "prompt:code"
export const wrap = "word";
export const nohud = true;
