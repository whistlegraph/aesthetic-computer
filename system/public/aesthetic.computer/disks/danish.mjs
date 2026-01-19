// Danish, 2026.01.19
// Translate any language to Danish.

const prompt = "indtast tekst på et hvilket som helst sprog for at oversætte til dansk";
const before = `
  You are a language translator.
  - Translate the user's input text to Danish
  - If the text is already in Danish, simply clean it up grammatically if needed
  - Preserve the tone, style, and meaning of the original
  - Only output the translation, nothing else
  - No explanations, no quotation marks, no "Oversættelse:" prefix
  - The user says:
`;
const after = `
  - Respond ONLY with the translated Danish text, nothing more
`;
const forgetful = true;

export const scheme = {
  text: [255, 255, 255],
  background: [198, 12, 48],
  prompt: [255, 200, 200],
  block: [150, 10, 40],
  highlight: [255, 255, 200],
  guideline: [150, 10, 40],
};

// 🛑 Intercept specific input text with a custom reply.
function halt($, text) {
  if (text === "halt") {
    return true;
  }
}

// 💬 Receive each reply in full.
function reply(text) {
  console.log("🇩🇰 Translated to Danish:", text);
}

function copied(text) {
  return text;
}

export { prompt, before, after, halt, reply, copied, forgetful };
export const system = "prompt:character";
export const nohud = true;

export function meta() {
  return {
    title: "Danish",
    desc: "🇩🇰 Translate any language to Danish",
  };
}
