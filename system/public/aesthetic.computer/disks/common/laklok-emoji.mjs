import { escapeColorCodes, splitColorCodes } from "../../lib/color-codes.mjs";

// A whole joined emoji, flag or keycap occupies one square in a bitmap line.
// Keep text-presentation symbols such as © and ♥ in the author's selected font.
const pictograph = /\p{Extended_Pictographic}\uFE0F?\p{Emoji_Modifier}?/u.source;
const clusters = new RegExp(
  "\\p{Regional_Indicator}{2}|[#*0-9]\\uFE0F?\\u20E3|" +
  pictograph + "(?:\\u200D" + pictograph + ")*(?:[\\u{E0020}-\\u{E007E}]+\\u{E007F})?\\uFE0E?",
  "gu",
);
function* emojiMatches(value) {
  for (const match of value.matchAll(clusters)) {
    const glyph = match[0];
    if (!glyph.endsWith("\uFE0E") && /[\uFE0F\u200D\u20E3]|\p{Emoji_Presentation}/u.test(glyph)) yield match;
  }
}
export const hasEmoji = value => !emojiMatches(value).next().done;

const sprites = new Map();
function emojiSprite(glyph, size) {
  const key = size + "/" + glyph;
  if (sprites.has(key)) return sprites.get(key);
  let sprite = null;
  try {
    const canvas = new OffscreenCanvas(size, size);
    const ctx = canvas.getContext("2d", { willReadFrequently: true });
    ctx.font = size + 'px "Apple Color Emoji","Segoe UI Emoji","Noto Color Emoji",sans-serif';
    const metrics = ctx.measureText(glyph);
    const w = metrics.actualBoundingBoxLeft + metrics.actualBoundingBoxRight;
    const h = metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent;
    if (w && h) {
      const scale = Math.min(1, size / w, size / h);
      ctx.scale(scale, scale);
      ctx.fillText(glyph, (size / scale - w) / 2 + metrics.actualBoundingBoxLeft,
        (size / scale - h) / 2 + metrics.actualBoundingBoxAscent);
      sprite = { width: size, height: size, pixels: ctx.getImageData(0, 0, size, size).data };
    }
  } catch { /* The piece can also run in a worker without a canvas font backend. */ }
  if (sprites.size >= 256) sprites.delete(sprites.keys().next().value);
  sprites.set(key, sprite);
  return sprite;
}

export function emojiMetrics(text, font, height) {
  const size = Math.max(8, height - 1);
  const advance = size + 1;
  const width = (value) => {
    let start = 0, pixels = 0;
    for (const m of emojiMatches(value)) {
      pixels += text.width(value.slice(start, m.index), font) + advance;
      start = m.index + m[0].length;
    }
    return pixels + text.width(value.slice(start), font);
  };
  return {
    width,
    box(value, pos, bounds) {
      // Give text.box one indivisible character per emoji. Restore its source
      // offsets immediately so links, speech, selection and copying see UTF-16.
      let marker = 0xe000;
      while (value.includes(String.fromCharCode(marker))) marker++;
      const placeholder = String.fromCharCode(marker);
      const starts = [];
      let compact = "", start = 0;
      function copy(until) {
        while (start < until) { starts.push(start); compact += value[start++]; }
      }
      for (const m of emojiMatches(value)) {
        copy(m.index);
        starts.push(start);
        compact += placeholder;
        start += m[0].length;
      }
      copy(value.length);
      starts.push(value.length);
      const metrics = { data: { proportional: true }, blockHeight: height - 1,
        getGlyph() {}, getAdvance: c => c === placeholder ? advance : text.width(c, font) };
      const box = text.box(compact, pos, bounds, 1, true, metrics);
      box.lines = box.lines.map((line, i) => {
        const map = [];
        let restored = "";
        for (let j = 0; j < line.length; j++) {
          const at = box.charMap[i][j];
          restored += line[j] === placeholder ? value.slice(starts[at], starts[at + 1]) : line[j];
          for (let k = starts[at]; k < starts[at + 1]; k++) map.push(k);
        }
        box.charMap[i] = map;
        return restored;
      });
      return box;
    },
    paint(api, coded, x, y, ink, shadow = false) {
      let color = "";
      const write = value => {
        if (!value) return;
        api.ink(ink).write(color + escapeColorCodes(value), { x, y }, false, undefined, false, font);
        x += text.width(value, font);
      };
      for (const [i, part] of splitColorCodes(coded).entries()) {
        if (i % 2) { color = "\\" + part + "\\"; continue; }
        let start = 0;
        for (const m of emojiMatches(part)) {
          write(part.slice(start, m.index));
          if (!shadow) {
            const sprite = emojiSprite(m[0], size);
            if (sprite) api.paste(sprite, x, y);
            else api.ink(ink).write("?", { x, y }, false, undefined, false, font);
          }
          x += advance;
          start = m.index + m[0].length;
        }
        write(part.slice(start));
      }
    },
  };
}
