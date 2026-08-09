// The canonical MacBook Neo (Mac17,5) keyboard — ANSI US, CountryCode 0.
//
// One source of truth. Anything that draws a MacBook keyboard imports from
// here rather than inlining its own rows, because hand-copied decks drift:
// a row that comes up one unit short silently fuses its last two caps (the
// classic "delete swallowed the = key") or drops a bracket off the end.
//
// Every row is exactly ROW_UNITS wide. That invariant is what
// validate-keyboard.mjs checks, and it is the thing that actually catches
// a missing or fused key — a wrong label is obvious by eye, a wrong width
// is not.

/// Apple's ANSI rows are 14.5 keycap units across, on every row including
/// the function row. Any row that does not sum to this is malformed.
export const ROW_UNITS = 14.5;

const letters = (s, style = "letter") => [...s].map((ch) => [ch, 1, style]);
const fkeys = () => Array.from({ length: 12 }, (_, i) => [`F${i + 1}`, 1, "f"]);

export const rows = [
  // Function row. 0.9 — MEASURED, not guessed. `measure-deck.mjs` finds the
  // six row bands in the product photo and reports their pitch: 77, 86, 85,
  // 86, 85 px. Every alpha row is ~85.5px; the function row is 77px, which
  // is 0.906 of a full row. It read 0.62 here originally (far too short) and
  // was then overcorrected to 1.0 by eye, because at a glance the row does
  // look full height. It is very slightly shorter, and only measurement
  // catches that. esc is 1.5u; the Touch ID / power cap closes the row at 1u.
  { name: "function", h: 0.9, keys: [
    ["esc", 1.5, "word"], ...fkeys(), ["power", 1, "power"]] },

  // Number row. `-` and `=` are their OWN caps; delete is 1.5u and must
  // never absorb them.
  { name: "number", h: 1, keys: [
    ["`", 1, "sym"], ...letters("1234567890", "sym"),
    ["-", 1, "sym"], ["=", 1, "sym"], ["delete", 1.5, "word"]] },

  // Upper row. BOTH brackets plus the backslash close it out.
  { name: "upper", h: 1, keys: [
    ["tab", 1.5, "word"], ...letters("qwertyuiop"),
    ["[", 1, "sym"], ["]", 1, "sym"], ["\\", 1, "sym"]] },

  { name: "home", h: 1, keys: [
    ["caps lock", 1.75, "word"], ...letters("asdfghjkl"),
    [";", 1, "sym"], ["'", 1, "sym"], ["return", 1.75, "word"]] },

  { name: "lower", h: 1, keys: [
    ["shift", 2.25, "word"], ...letters("zxcvbnm"),
    [",", 1, "sym"], [".", 1, "sym"], ["/", 1, "sym"], ["shift", 2.25, "word"]] },

  // Bottom row. The arrow cluster is one 3u block (inverted T, up/down
  // stacked at half height) rather than four separate caps.
  { name: "bottom", h: 1, keys: [
    ["fn", 1, "word"], ["control", 1, "word"], ["option", 1, "word"],
    ["command", 1.25, "word"], ["space", 5, "space"], ["command", 1.25, "word"],
    ["option", 1, "word"], ["arrows", 3, "arrows"]] },
];

/// Every legend that must be physically present exactly once. Split from
/// `rows` on purpose: if a row is edited into a shape that still sums to
/// 14.5 but has lost a bracket, only an independent inventory catches it.
export const expectedLegends = [
  "esc", ...Array.from({ length: 12 }, (_, i) => `F${i + 1}`), "power",
  "`", ..."1234567890", "-", "=", "delete",
  "tab", ..."qwertyuiop", "[", "]", "\\",
  "caps lock", ..."asdfghjkl", ";", "'", "return",
  ..."zxcvbnm", ",", ".", "/",
  "fn", "control", "space", "arrows",
];

/// Legends that legitimately appear twice (one per hand).
export const doubledLegends = ["shift", "command", "option"];

/// Lay the rows out in pixels. `unit` is the keycap pitch; `gap` is the
/// hairline between caps. Returns flat caps plus the deck's own size, so a
/// renderer never has to re-derive the geometry.
export function layout({ unit = 56, gap = 5, pad = 30 } = {}) {
  const wellH = rows.reduce((sum, row) => sum + row.h * unit, 0);
  const caps = [];
  let y = pad;
  for (const row of rows) {
    let x = pad;
    for (const [label, wUnits, style] of row.keys) {
      caps.push({
        label, style, row: row.name, units: wUnits,
        x: x + gap / 2, y: y + gap / 2,
        w: wUnits * unit - gap, h: row.h * unit - gap,
      });
      x += wUnits * unit;
    }
    y += row.h * unit;
  }
  return { caps, unit, gap, pad, width: ROW_UNITS * unit + pad * 2, height: wellH + pad * 2 };
}
