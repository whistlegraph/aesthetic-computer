// bdf-supplement, 26.09.14
// Hand-drawn glyphs for characters a BDF font lacks. The `bdf-glyph`
// endpoint consults this after a miss, so chat can show letters the X11
// 6x10 face never shipped (IPA, combining marks) without a second font.
//
// Rows are top-to-bottom, `#` = lit pixel. Every 6x10 glyph is drawn on the
// same 6x10 cell the BDF uses: baseline at row 7, rows 8–9 for descenders.

const SIX_BY_TEN = {
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

const FONTS = {
  "6x10": { glyphs: SIX_BY_TEN, ascent: 8, descent: 2, yOffset: -2 },
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
