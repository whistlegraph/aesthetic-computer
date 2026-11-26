import { atlasKeys, atlasSprite } from "./emoji-atlas/atlas.mjs";
import { radioGardenGlyphs } from "./radio-garden-glyphs.mjs";
import { escapeColorCodes, splitColorCodes } from "../../lib/color-codes.mjs";

// Source offsets are UTF-16 offsets, as required by the text editor. Longest
// matches keep flags, modifiers and joined sequences in a single caret cell.
let emojiTrie;
function buildEmojiTrie() {
  const root = new Map();
  function add(key, index) {
    let node = root;
    for (let i = 0; i < key.length; i++) {
      const unit = key.charCodeAt(i);
      if (!node.has(unit)) node.set(unit, new Map());
      node = node.get(unit);
    }
    node.index = index;
  }
  atlasKeys.forEach((key, index) => {
    add(key, index);
    // Qualifying selectors can be omitted inside keycaps and joined emoji.
    // Standalone text-presentation symbols retain their normal font.
    if (key.includes("\u200d") || key.endsWith("\u20e3")) {
      let forms = [""];
      for (const ch of key) {
        const extended = forms.map(prefix => prefix + ch);
        forms = ch === "\ufe0f" ? extended.concat(forms) : extended;
      }
      for (const form of forms) add(form, index);
    }
  });
  return root;
}

function* emojiMatches(value) {
  if (!/[^\x00-\x7f]/.test(value)) return;
  const root = emojiTrie ??= buildEmojiTrie();
  for (let start = 0; start < value.length; start++) {
    let node = root, end = start, atlasIndex;
    for (let i = start; i < value.length; i++) {
      const unit = value.charCodeAt(i);
      const next = node.get(unit);
      // Explicit emoji presentation is also valid on default-emoji bases.
      if (!next && !(unit === 0xfe0f && node.index !== undefined)) break;
      node = next ?? node;
      if (node.index !== undefined) {
        end = i + 1;
        atlasIndex = node.index;
      }
    }
    if (end === start) continue;
    if (value.charCodeAt(end) !== 0xfe0e) {
      yield { glyph: value.slice(start, end), index: start, atlasIndex };
    }
    start = end - 1;
  }
}
function* displayGlyphs(value) {
  let start = 0;
  for (const m of emojiMatches(value)) {
    for (const glyph of value.slice(start, m.index)) yield { glyph };
    yield { glyph: m.glyph, emoji: true, atlasIndex: m.atlasIndex };
    start = m.index + m.glyph.length;
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
function emojiSprite(glyph, size, atlasIndex) {
  const key = size + "/" + glyph;
  if (sprites.has(key)) return sprites.get(key);
  let sprite = null;
  const grid = pixelGlyph(glyph);
  if (grid) sprite = gridSprite(grid, glyphWidth(glyph, size), size);
  else {
    const base = atlasSprite(atlasIndex);
    if (base && base.height === size) sprite = base;
    else if (base) {
      const pixels = new Uint8ClampedArray(size * size * 4);
      for (let y = 0; y < size; y++) {
        for (let x = 0; x < size; x++) {
          const src = (Math.floor(y * base.height / size) * base.width + Math.floor(x * base.width / size)) * 4;
          pixels.set(base.pixels.subarray(src, src + 4), (y * size + x) * 4);
        }
      }
      sprite = { width: size, height: size, pixels };
    }
  }
  if (sprites.size >= 256) sprites.delete(sprites.keys().next().value);
  sprites.set(key, sprite);
  return sprite;
}

export function emojiMetrics(text, font, height) {
  const size = Math.max(MIN_EMOJI_HEIGHT, height - 1);
  const advance = glyph => glyphWidth(glyph, size) + 1;
  const width = (value) => {
    let start = 0, pixels = 0;
    for (const m of emojiMatches(value)) {
      pixels += text.width(value.slice(start, m.index), font) + advance(m.glyph);
      start = m.index + m.glyph.length;
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
        const w = advance(m.glyph);
        if (!markers.has(w)) {
          while (value.includes(String.fromCharCode(marker))) marker++;
          const placeholder = String.fromCharCode(marker++);
          markers.set(w, placeholder);
          advances.set(placeholder, w);
        }
        compact += markers.get(w);
        start += m.glyph.length;
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
    paint(api, coded, x, y, ink, shadow = false, scale = 1) {
      let color = "";
      const write = value => {
        if (!value) return;
        if (ink !== undefined) api.ink(ink);
        api.write(color + escapeColorCodes(value), { x, y: y + Math.floor((size + 1 - height) / 2) * scale, size: scale }, false, undefined, false, font);
        x += text.width(value, font) * scale;
      };
      for (const [i, part] of splitColorCodes(coded).entries()) {
        if (i % 2) { color = "\\" + part + "\\"; continue; }
        let start = 0;
        for (const m of emojiMatches(part)) {
          write(part.slice(start, m.index));
          if (!shadow) {
            const sprite = emojiSprite(m.glyph, size, m.atlasIndex);
            if (sprite) api.paste(sprite, x, y, scale);
            else {
              if (ink !== undefined) api.ink(ink);
              api.write("?", { x, y, size: scale }, false, undefined, false, font);
            }
          }
          x += advance(m.glyph) * scale;
          start = m.index + m.glyph.length;
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
  function add(glyph, emoji = false, atlasIndex) {
    const newline = glyph === "\n";
    const width = newline ? 0 : emoji ? glyphWidth(glyph, size) + 1 : typeface.getAdvance(glyph);
    if (px && px + width > gutter - prompt.letterWidth) nextLine();
    const cell = { start: at, end: at + glyph.length, x, y, left: px, width, top: y * lineHeight + (emoji ? 0 : Math.floor((lineHeight - letterHeight) / 2)),
      height: emoji ? lineHeight : letterHeight, glyph, bitmap: emoji ? emojiSprite(glyph, size, atlasIndex) : null };
    cells.push(cell);
    at = cell.end;
    if (newline) nextLine(); else { px += width; x++; }
  }
  for (const { glyph, emoji, atlasIndex } of displayGlyphs(value)) add(glyph, emoji, atlasIndex);
  cells.push({ start: at, end: at, x, y, left: px, width: prompt.letterWidth, top: y * lineHeight + Math.floor((lineHeight - letterHeight) / 2), glyph: "" });
  return cells;
}
