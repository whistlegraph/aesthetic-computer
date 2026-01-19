// English, 2026.01.19
// Translate any language to English.

const prompt = "enter text in any language to translate to english";
const before = `
  You are a language translator.
  - Translate the user's input text to English
  - If the text is already in English, simply clean it up grammatically if needed
  - Preserve the tone, style, and meaning of the original
  - Only output the translation, nothing else
  - No explanations, no quotation marks, no "Translation:" prefix
  - The user says:
`;
const after = `
  - Respond ONLY with the translated English text, nothing more
`;
const forgetful = true;

export const scheme = {
  text: [255, 255, 255],
  background: [20, 40, 80],
  prompt: [150, 200, 255],
  block: [100, 150, 200],
  highlight: [255, 255, 200],
  guideline: [100, 150, 200],
};

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  if (text === "halt") {
    return true;
  }
}

// 💬 Receive each reply in full.
function reply(text) {
  console.log("🇬🇧 Translated to English:", text);
}

function copied(text) {
  return text;
}

export { prompt, before, after, halt, reply, copied, forgetful };
export const system = "prompt:character";
export const nohud = true;

export function meta() {
  return {
    title: "English",
    desc: "🇬🇧 Translate any language to English",
  };
}
