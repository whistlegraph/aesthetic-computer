// core-video.mjs — a core's frame as terminal cells.
//
// One cell holds two pixels, one above the other: the upper half block takes
// the top pixel as its foreground and the bottom one as its background, the
// way qr.mjs draws a code. That works in every terminal with 24-bit colour,
// which is why it is the first sink. The block has the shape qrBlock returns
// — lines, width, height — so the frame renderer gives it the same column.
const UPPER = "▀";

/** Columns and rows that hold a width × height screen inside the room given, keeping its shape. */
export function screenSize(width, height, columns, rows) {
  const scale = Math.min(columns / width, (rows * 2) / height);
  return { columns: Math.max(1, Math.floor(width * scale)), rows: Math.max(1, Math.floor((height * scale) / 2)) };
}

/** Draw RGBA pixels into `columns` × `rows` cells. Each cell pixel is the average of the screen pixels under it. */
export function screenBlock(pixels, width, height, columns, rows) {
  const across = columns, down = rows * 2;
  const small = new Uint8Array(across * down * 3);
  for (let y = 0; y < down; y++) {
    const y0 = Math.floor((y * height) / down), y1 = Math.max(y0 + 1, Math.floor(((y + 1) * height) / down));
    for (let x = 0; x < across; x++) {
      const x0 = Math.floor((x * width) / across), x1 = Math.max(x0 + 1, Math.floor(((x + 1) * width) / across));
      let r = 0, g = 0, b = 0;
      for (let sy = y0; sy < y1; sy++) {
        for (let sx = x0, i = (sy * width + x0) * 4; sx < x1; sx++, i += 4) { r += pixels[i]; g += pixels[i + 1]; b += pixels[i + 2]; }
      }
      const count = (y1 - y0) * (x1 - x0), o = (y * across + x) * 3;
      small[o] = r / count; small[o + 1] = g / count; small[o + 2] = b / count;
    }
  }
  const lines = [];
  for (let row = 0; row < rows; row++) {
    let line = "", ink = -1, ground = -1;
    for (let x = 0; x < across; x++) {
      const t = (row * 2 * across + x) * 3, u = t + across * 3;
      const top = (small[t] << 16) | (small[t + 1] << 8) | small[t + 2];
      const bottom = (small[u] << 16) | (small[u + 1] << 8) | small[u + 2];
      // A colour is written only where it changes, which is most of the saving on flat backgrounds.
      if (top !== ink) { line += `\x1b[38;2;${small[t]};${small[t + 1]};${small[t + 2]}m`; ink = top; }
      if (bottom !== ground) { line += `\x1b[48;2;${small[u]};${small[u + 1]};${small[u + 2]}m`; ground = bottom; }
      line += UPPER;
    }
    lines.push(`${line}\x1b[0m`);
  }
  return { lines, width: across, height: rows };
}
