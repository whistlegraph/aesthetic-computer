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

// Prose: the whole body set in Unicode mathematical letters — type no
// keyboard makes, that survives iMessage, Signal and Instagram. Each style is
// a family; inside it `*x*` reads as the family's italic, `**x**` as its bold
// and `` `x` `` as mono. Links, @handles, #tags and emails stay plain so they
// still work. Only A–Z a–z 0–9 map; everything else passes through.
const PROSE = {
  sans: { upper: 0x1d5a0, lower: 0x1d5ba, digit: 0x1d7e2, italic: "sans-italic", bold: "sans-bold" },
  "sans-italic": { upper: 0x1d608, lower: 0x1d622, digit: 0x1d7e2, italic: "sans", bold: "sans-bold-italic" },
  "sans-bold": { upper: 0x1d5d4, lower: 0x1d5ee, digit: 0x1d7ec, italic: "sans-bold-italic", bold: "sans" },
  "sans-bold-italic": { upper: 0x1d63c, lower: 0x1d656, digit: 0x1d7ec, italic: "sans-bold", bold: "sans-italic" },
  mono: { upper: 0x1d670, lower: 0x1d68a, digit: 0x1d7f6, italic: "sans-italic", bold: "sans-bold" },
};
export const PROSE_STYLES = [...Object.keys(PROSE), "plain"];
const PROSE_KEEP = /(https?:\/\/\S+|[\w.+-]+@[\w-]+\.[\w.-]+|[@#][\w.]+)/g;
const PROSE_MARKS = /(\*\*[^*\n]+\*\*|\*[^*\n]+\*|`[^`\n]+`)/;

function proseLetters(text, style) {
  const family = PROSE[style];
  return text.replace(/[A-Za-z0-9]/g, (character) => {
    const code = character.charCodeAt(0);
    if (code <= 57) return String.fromCodePoint(family.digit + code - 48);
    if (code <= 90) return String.fromCodePoint(family.upper + code - 65);
    return String.fromCodePoint(family.lower + code - 97);
  });
}

export function formatProse(input, style = "sans") {
  const text = String(input || "");
  if (!style || style === "plain" || !text) return text;
  const family = PROSE[style];
  if (!family) throw new Error(`unknown prose style "${style}" (${PROSE_STYLES.join(", ")})`);
  return text
    .split(PROSE_MARKS)
    .map((part) => {
      let partStyle = style;
      let body = part;
      if (/^\*\*[^*]+\*\*$/.test(part)) [partStyle, body] = [family.bold, part.slice(2, -2)];
      else if (/^\*[^*]+\*$/.test(part)) [partStyle, body] = [family.italic, part.slice(1, -1)];
      else if (/^`[^`]+`$/.test(part)) [partStyle, body] = ["mono", part.slice(1, -1)];
      return body
        .split(PROSE_KEEP)
        .map((bit, i) => (i % 2 ? bit : proseLetters(bit, partStyle)))
        .join("");
    })
    .join("");
}
