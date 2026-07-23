const BOLD_UPPER = 0x1d400;
const BOLD_LOWER = 0x1d41a;
const BOLD_DIGIT = 0x1d7ce;
const ITALIC_UPPER = 0x1d434;
const ITALIC_LOWER = 0x1d44e;

function mapMath(text, style) {
  return Array.from(text, (character) => {
    const code = character.codePointAt(0);
    if (code >= 65 && code <= 90) {
      return String.fromCodePoint((style === "bold" ? BOLD_UPPER : ITALIC_UPPER) + code - 65);
    }
    if (code >= 97 && code <= 122) {
      if (style === "italic" && character === "h") return "ℎ";
      return String.fromCodePoint((style === "bold" ? BOLD_LOWER : ITALIC_LOWER) + code - 97);
    }
    if (style === "bold" && code >= 48 && code <= 57) {
      return String.fromCodePoint(BOLD_DIGIT + code - 48);
    }
    return character;
  }).join("");
}

function strike(text) {
  return Array.from(text, (character) => /\s/u.test(character) ? character : `${character}\u0336`).join("");
}

export function formatRichText(input) {
  let text = String(input || "").replace(/\r\n?/g, "\n").trim();
  text = text.replace(/^#{1,3}\s+(.+)$/gm, (_, title) => mapMath(title, "bold"));
  text = text.replace(/^\s*[-*]\s+/gm, "• ");
  text = text.replace(/\[([^\]]+)]\((https?:\/\/[^\s)]+)\)/g, "$1 — $2");
  text = text.replace(/\*\*([^*\n]+)\*\*/g, (_, value) => mapMath(value, "bold"));
  text = text.replace(/__([^_\n]+)__/g, (_, value) => mapMath(value, "bold"));
  text = text.replace(/~~([^~\n]+)~~/g, (_, value) => strike(value));
  text = text.replace(/(?<!\*)\*([^*\n]+)\*(?!\*)/g, (_, value) => mapMath(value, "italic"));
  text = text.replace(/(?<!_)_([^_\n]+)_(?!_)/g, (_, value) => mapMath(value, "italic"));
  return text;
}
