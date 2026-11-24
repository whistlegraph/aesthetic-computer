// bdf-supplement, 26.09.14 → 26.11.24
// Glyphs for characters a BDF font lacks. The `bdf-glyph` endpoint consults
// this after a miss, so chat and mail can show letters the X11 6x10 face
// never shipped (IPA, Latin Extended-B, Vietnamese, more Cyrillic) without a
// second font. Two sources, the hand first:
//   - SIX_BY_TEN below, drawn by hand;
//   - bdf-supplement-6x10.json, generated from 6x10's own glyphs by
//     toolchain/type/gen-6x10-supplement.py (accents composed from the
//     font's marks; turned, reversed, small-capital and stroked letters
//     made from their bases). Regenerate there; don't edit the JSON.
//
// Rows are top-to-bottom, `#` = lit pixel. Every 6x10 glyph is drawn on the
// same 6x10 cell the BDF uses: baseline at row 7, rows 8–9 for descenders.

import { readFileSync } from "node:fs";

const SIX_BY_TEN = {
  // Ȼ latin capital letter c with stroke — the slash slants, or it is just C
  0x023b: [
    "......",
    ".###..",
    "#...#.",
    "#..#..",
    "#..#..",
    "#.#...",
    "##..#.",
    ".###..",
    "......",
    "......",
  ],
  // Ɇ latin capital letter e with stroke
  0x0246: [
    "......",
    "#####.",
    "#..#..",
    "#..#..",
    "####..",
    "#.#...",
    "##....",
    "#####.",
    "......",
    "......",
  ],
  // ɑ latin small letter alpha — single-storey a
  0x0251: [
    "......",
    "......",
    "......",
    ".####.",
    "#...#.",
    "#...#.",
    "#...#.",
    ".####.",
    "......",
    "......",
  ],
  // ɓ latin small letter b with hook
  0x0253: [
    "......",
    ".##...",
    "#.....",
    "#.##..",
    "##..#.",
    "#...#.",
    "##..#.",
    "#.##..",
    "......",
    "......",
  ],
  // ɨ latin small letter i with stroke
  0x0268: [
    "......",
    "..#...",
    "......",
    ".##...",
    "..#...",
    "#####.",
    "..#...",
    ".###..",
    "......",
    "......",
  ],
  // ɱ latin small letter m with hook
  0x0271: [
    "......",
    "......",
    "......",
    "##.#..",
    "#.#.#.",
    "#.#.#.",
    "#.#.#.",
    "#...#.",
    "....#.",
    "...#..",
  ],
  // ʊ latin small letter upsilon
  0x028a: [
    "......",
    "......",
    "......",
    "##.##.",
    "#...#.",
    "#...#.",
    "#...#.",
    ".###..",
    "......",
    "......",
  ],
  // ⃠ combining enclosing circle backslash — drawn as its own cell
  0x20e0: [
    "......",
    ".###..",
    "#...#.",
    "##..#.",
    "#.#.#.",
    "#..##.",
    "#...#.",
    ".###..",
    "......",
    "......",
  ],
};

// { "029C": [10 rows of 6], … } — keyed by hex code point, like the endpoint.
function generated(file) {
  try {
    const table = JSON.parse(readFileSync(new URL(file, import.meta.url), "utf8"));
    return Object.fromEntries(Object.entries(table).map(([hex, rows]) => [parseInt(hex, 16), rows]));
  } catch {
    return {}; // A missing table only costs the generated glyphs, never the hand-drawn ones.
  }
}

const FONTS = {
  "6x10": { glyphs: { ...generated("./bdf-supplement-6x10.json"), ...SIX_BY_TEN }, ascent: 8, descent: 2, yOffset: -2 },
};

// Return glyph JSON in the exact shape `bdf-glyph` emits, or null.
export function supplementGlyph(fontName, charCode) {
  const font = FONTS[fontName];
  const rows = font?.glyphs[charCode];
  if (!rows) return null;

  const height = rows.length;
  const width = rows[0].length;
  const commands = [];
  rows.forEach((row, y) => {
    for (let x = 0; x < row.length; x++) {
      if (row[x] === "#") commands.push({ name: "point", args: [x, y] });
    }
  });

  return {
    resolution: [width, height],
    offset: [0, font.yOffset],
    baselineOffset: [0, font.ascent - height - font.yOffset],
    advance: width,
    bbx: { width, height, xOffset: 0, yOffset: font.yOffset },
    dwidth: { x: width, y: 0 },
    fontMetrics: { ascent: font.ascent, descent: font.descent },
    commands,
    supplement: true,
  };
}

export function hasSupplement(fontName, charCode) {
  return Boolean(FONTS[fontName]?.glyphs[charCode]);
}
