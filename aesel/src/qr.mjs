// qr.mjs — a scannable QR code in a terminal.
//
// Two QR modules share one character cell: the upper half block takes the
// foreground colour for the top module and the background colour for the
// bottom one, which keeps the modules square. Codes are always drawn as black
// on white with a quiet zone, regardless of the terminal's own theme, because
// a camera needs the contrast and the margin to find the code at all.
import { ErrorCorrectLevel, qrcode } from "./vendor/qr.mjs";

const UPPER = "\u2580"; // Upper half block.
const BLACK = 0;
const WHITE = 15;
const QUIET = 2; // Modules of margin on every side.

const cell = (top, bottom) =>
  `\x1b[38;5;${top ? BLACK : WHITE}m\x1b[48;5;${bottom ? BLACK : WHITE}m${UPPER}`;

// Render `text` as lines of ANSI. Each line already resets at its end, so the
// block can be pasted into any frame without leaking colour.
export function qrBlock(text, { quiet = QUIET } = {}) {
  const modules = qrcode(String(text), { errorCorrectLevel: ErrorCorrectLevel.L }).modules;
  const span = modules.length + quiet * 2;
  const dark = (x, y) => {
    const column = x - quiet;
    const row = y - quiet;
    if (column < 0 || row < 0 || column >= modules.length || row >= modules.length) return false;
    return Boolean(modules[row][column]);
  };

  const lines = [];
  for (let y = 0; y < span; y += 2) {
    let line = "";
    for (let x = 0; x < span; x += 1) {
      line += cell(dark(x, y), y + 1 < span ? dark(x, y + 1) : false);
    }
    lines.push(`${line}\x1b[0m`);
  }
  return { lines, width: span, height: lines.length, text: String(text) };
}
