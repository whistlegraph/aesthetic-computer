import { radioGardenGlyphs } from "./radio-garden-glyphs.mjs";
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
function* displayGlyphs(value) {
  let start = 0;
  for (const m of emojiMatches(value)) {
    for (const glyph of value.slice(start, m.index)) yield { glyph };
    yield { glyph: m[0], emoji: true };
    start = m.index + m[0].length;
  }
  for (const glyph of value.slice(start)) yield { glyph };
}

export const hasEmoji = value => !emojiMatches(value).next().done;


const MIN_EMOJI_HEIGHT = 16;
const turkeyRows = [
  "   @@ @@",
  "  @yy@yy@ @@",
  " @@oyooyo@yy@",
  "@yy@o@ooyooy@",
  "@oyo@o@oo@o@  @@@",
  "@yo@oo@oo@o@ @ccp@",
  " @oo@o@ooo@  @ccyou",
  "@yyooobb@@  @crr@",
  "@oooo@bbbb@@brr@",
  " @@o@bboobbbbbr@",
  " @oo@boooobbbr@",
  "  @@@bboobbbb@",
  "    @bbbbbbb@",
  "     @@@@@@@",
  "       y  y",
  "      yyy yyy"
];
const turkeyColors = {
  "@": [56, 32, 42], y: [255, 210, 98], o: [208, 121, 55], b: [132, 80, 75],
  c: [115, 219, 233], p: [20, 13, 22], r: [238, 66, 68], u: [231, 165, 80],
};
const radioRows = [
  "                ss",
  "              ss",
  "            ss",
  "          ss",
  "        ss",
  "  @@@@@@@@@@@@@@",
  " @cccccccccccccc@",
  " @c@sssssss@cwwc@",
  " @c@:::w:::@cwwc@",
  " @cccccccccccccc@",
  " @c@@@@@@@cc@@@c@",
  " @c@s@s@s@c@wsw@@",
  " @c@@s@s@@c@sss@@",
  " @c@s@s@s@cc@@@c@",
  " @cccccccccccccc@",
  "  @@@@@@@@@@@@@@",
];
const jellyRows = [
  "      pppp",
  "    ppbbbbpp",
  "   pbbhhbbbbp",
  "  pbhhhbbblbbp",
  " pbbbbbbblblbbp",
  " pbbblbblbblbbp",
  "pbbblblblbblbbbp",
  "pplllppllpplllpp",
  "  ppp  pp  ppp",
  "   t  l  l  t",
  "  t   l  l   t",
  "  t    l  l  t",
  "   t   l  l t",
  "    t l    lt",
  "   t  l     t",
  "     l",
];
const pixelGlyphs = new Map([
  ...Object.entries(radioGardenGlyphs),
  ["🦃", { rows: turkeyRows, colors: turkeyColors }],
  ["📻", { rows: radioRows, colors: {
    "@": [44, 33, 56], s: [138, 181, 191], c: [241, 106, 69],
    w: [255, 227, 157], ":": [60, 85, 99],
  } }],
  ["🪼", { rows: jellyRows, colors: {
    p: [124, 78, 180], b: [235, 141, 227], h: [255, 234, 242],
    l: [249, 193, 247], t: [109, 208, 224],
  } }],
]);
for (const grid of pixelGlyphs.values()) grid.width = Math.max(...grid.rows.map(row => row.length));

function pixelGlyph(glyph) {
  return pixelGlyphs.get(glyph.replace(/\uFE0F$/u, ""));
}
function glyphWidth(glyph, height) {
  const grid = pixelGlyph(glyph);
  return grid ? Math.round(grid.width * height / grid.rows.length) : height;
}

function gridSprite(grid, width, height) {
  const pixels = new Uint8ClampedArray(width * height * 4);
  for (let y = 0; y < height; y++) {
    const row = grid.rows[Math.floor(y * grid.rows.length / height)];
    for (let x = 0; x < width; x++) {
      const color = grid.colors[row[Math.floor(x * grid.width / width)]];
      if (!color) continue;
      const i = (y * width + x) * 4;
      pixels.set(color, i);
      pixels[i + 3] = 255;
    }
  }
  return { width, height, pixels };
}

const sprites = new Map();
function emojiSprite(glyph, size) {
  const key = size + "/" + glyph;
  if (sprites.has(key)) return sprites.get(key);
  let sprite = null;
  const grid = pixelGlyph(glyph);
  if (grid) sprite = gridSprite(grid, glyphWidth(glyph, size), size);
  else try {
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

// Reuse a fixed-resolution sprite when a drawing scales a standalone emoji.
export function singleEmojiSprite(value) {
  const match = emojiMatches(value).next().value;
  return match?.[0] === value ? emojiSprite(value, MIN_EMOJI_HEIGHT) : null;
}

export function emojiMetrics(text, font, height) {
  const size = Math.max(MIN_EMOJI_HEIGHT, height - 1);
  const advance = glyph => glyphWidth(glyph, size) + 1;
  const width = (value) => {
    let start = 0, pixels = 0;
    for (const m of emojiMatches(value)) {
      pixels += text.width(value.slice(start, m.index), font) + advance(m[0]);
      start = m.index + m[0].length;
    }
    return pixels + text.width(value.slice(start), font);
  };
  return {
    width, rowHeight: size + 1,
    fit(value, limit) {
      if (width(value) <= limit) return value;
      let prefix = "", used = text.width("...", font);
      for (const { glyph, emoji } of displayGlyphs(value)) {
        used += emoji ? advance(glyph) : text.width(glyph, font);
        if (used > limit) break;
        prefix += glyph;
      }
      return prefix + "...";
    },
    box(value, pos, bounds) {
      // Give text.box one indivisible character per emoji. Restore its source
      // offsets immediately so links, speech, selection and copying see UTF-16.
      let marker = 0xe000;
      while (value.includes(String.fromCharCode(marker))) marker++;
      const advances = new Map();
      const markers = new Map();
      const starts = [];
      let compact = "", start = 0;
      function copy(until) {
        while (start < until) { starts.push(start); compact += value[start++]; }
      }
      for (const m of emojiMatches(value)) {
        copy(m.index);
        starts.push(start);
        const w = advance(m[0]);
        if (!markers.has(w)) {
          while (value.includes(String.fromCharCode(marker))) marker++;
          const placeholder = String.fromCharCode(marker++);
          markers.set(w, placeholder);
          advances.set(placeholder, w);
        }
        compact += markers.get(w);
        start += m[0].length;
      }
      copy(value.length);
      starts.push(value.length);
      const metrics = { data: { proportional: true }, blockHeight: height - 1,
        getGlyph() {}, getAdvance: c => advances.get(c) ?? text.width(c, font) };
      const box = text.box(compact, pos, bounds, 1, true, metrics);
      box.lines = box.lines.map((line, i) => {
        const map = [];
        let restored = "";
        for (let j = 0; j < line.length; j++) {
          const at = box.charMap[i][j];
          restored += advances.has(line[j]) ? value.slice(starts[at], starts[at + 1]) : line[j];
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
        api.ink(ink).write(color + escapeColorCodes(value), { x, y: y + Math.floor((size + 1 - height) / 2) }, false, undefined, false, font);
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
          x += advance(m[0]);
          start = m.index + m[0].length;
        }
        write(part.slice(start));
      }
    },
  };
}

// TextInput stores the original UTF-16 string. Its optional cell layout maps
// every code unit back to one caret stop and one screen rectangle per glyph.
export function emojiDraftLayout(value, prompt) {
  if (!hasEmoji(value)) return null;
  const cells = [];
  const { typeface, letterHeight, gutter } = prompt;
  const size = Math.max(MIN_EMOJI_HEIGHT, letterHeight);
  const lineHeight = size + 1;
  let at = 0, px = 0, x = 0, y = 0;
  function nextLine() { px = 0; x = 0; y++; }
  function add(glyph, emoji = false) {
    const newline = glyph === "\n";
    const width = newline ? 0 : emoji ? glyphWidth(glyph, size) + 1 : typeface.getAdvance(glyph);
    if (px && px + width > gutter - prompt.letterWidth) nextLine();
    const cell = { start: at, end: at + glyph.length, x, y, left: px, width, top: y * lineHeight + (emoji ? 0 : Math.floor((lineHeight - letterHeight) / 2)),
      height: emoji ? lineHeight : letterHeight, glyph, bitmap: emoji ? emojiSprite(glyph, size) : null };
    cells.push(cell);
    at = cell.end;
    if (newline) nextLine(); else { px += width; x++; }
  }
  for (const { glyph, emoji } of displayGlyphs(value)) add(glyph, emoji);
  cells.push({ start: at, end: at, x, y, left: px, width: prompt.letterWidth, top: y * lineHeight + Math.floor((lineHeight - letterHeight) / 2), glyph: "" });
  return cells;
}
