// art-to-braille.mjs — draw ANYTHING inline as Unicode braille (no deps, no files).
//
// The only route that actually renders arbitrary art inline on GitHub: pack a
// boolean dot-field into braille glyphs (U+2800, 2 wide x 4 tall dots per char)
// and drop the text in a ``` code fence. No MathJax (\rule is unsupported), no
// committed image.
//
//   node art-to-braille.mjs smiley
//   node art-to-braille.mjs flower
//   node art-to-braille.mjs image path/to/pic.png [cols] [--invert]
//
// image mode shells out to macOS `sips` to normalize+resize any input to an
// 8-bit PNG, then decodes it with Node's built-in zlib (no image library).

import { execFileSync } from "node:child_process";
import { inflateSync } from "node:zlib";
import { readFileSync } from "node:fs";

// (col 0..1, row 0..3) -> braille dot bit
const DOTBIT = { "0,0": 0x01, "0,1": 0x02, "0,2": 0x04, "1,0": 0x08, "1,1": 0x10, "1,2": 0x20, "0,3": 0x40, "1,3": 0x80 };

function render(cols, rows, on) {
  let out = "";
  for (let cy = 0; cy < rows; cy++) {
    for (let cx = 0; cx < cols; cx++) {
      let code = 0;
      for (let r = 0; r < 4; r++)
        for (let c = 0; c < 2; c++)
          if (on(cx * 2 + c, cy * 4 + r)) code |= DOTBIT[`${c},${r}`];
      out += String.fromCharCode(0x2800 + code);
    }
    out += "\n";
  }
  return out;
}

// ---- parametric shapes (normalized -1..1 field) ----
function shapeField(name, W, H) {
  return (px, py) => {
    const dx = (px / W - 0.5) * 2, dy = (py / H - 0.5) * 2;
    if (name === "smiley") {
      const ring = Math.abs(Math.hypot(dx, dy) - 0.85) < 0.06;
      const eyeL = Math.hypot(dx + 0.35, dy + 0.3) < 0.1;
      const eyeR = Math.hypot(dx - 0.35, dy + 0.3) < 0.1;
      const smile = dy > 0.1 && Math.abs(Math.hypot(dx, dy - 0.1) - 0.45) < 0.06;
      return ring || eyeL || eyeR || smile;
    }
    if (name === "flower") {
      const a = Math.atan2(dy, dx), r = Math.hypot(dx, dy);
      const petal = r < 0.78 * Math.abs(Math.cos(3 * a)) + 0.18; // rose curve
      const core = r < 0.16;
      return petal || core;
    }
    return Math.hypot(dx, dy) < 0.85; // circle
  };
}

// ---- minimal PNG decode (8-bit, color types 0/2/4/6, no interlace) ----
function decodePng(buf) {
  let p = 8; // skip signature
  let w, h, ct, idat = [];
  while (p < buf.length) {
    const len = buf.readUInt32BE(p); const type = buf.toString("ascii", p + 4, p + 8);
    const data = buf.subarray(p + 8, p + 8 + len);
    if (type === "IHDR") { w = data.readUInt32BE(0); h = data.readUInt32BE(4); ct = data[9]; }
    else if (type === "IDAT") idat.push(data);
    else if (type === "IEND") break;
    p += 12 + len;
  }
  const ch = { 0: 1, 2: 3, 4: 2, 6: 4 }[ct];
  if (!ch) throw new Error("unsupported PNG color type " + ct);
  const raw = inflateSync(Buffer.concat(idat));
  const stride = w * ch;
  const out = Buffer.alloc(h * stride);
  const pae = (a, b, c) => { const pp = a + b - c, da = Math.abs(pp - a), db = Math.abs(pp - b), dc = Math.abs(pp - c); return da <= db && da <= dc ? a : db <= dc ? b : c; };
  let q = 0;
  for (let y = 0; y < h; y++) {
    const f = raw[q++];
    for (let i = 0; i < stride; i++) {
      const x = raw[q++];
      const a = i >= ch ? out[y * stride + i - ch] : 0;
      const b = y > 0 ? out[(y - 1) * stride + i] : 0;
      const c = i >= ch && y > 0 ? out[(y - 1) * stride + i - ch] : 0;
      let v = x;
      if (f === 1) v = x + a; else if (f === 2) v = x + b; else if (f === 3) v = x + ((a + b) >> 1); else if (f === 4) v = x + pae(a, b, c);
      out[y * stride + i] = v & 0xff;
    }
  }
  return { w, h, ch, px: out };
}

function imageField(path, dotW, invert) {
  const tmp = "/tmp/braille-norm.png";
  execFileSync("sips", ["-s", "format", "png", "--resampleWidth", String(dotW), path, "-o", tmp], { stdio: "ignore" });
  const { w, h, ch, px } = decodePng(readFileSync(tmp));
  return {
    W: w, H: h,
    on: (x, y) => {
      if (x >= w || y >= h) return false;
      const o = (y * w + x) * ch;
      const lum = ch >= 3 ? 0.299 * px[o] + 0.587 * px[o + 1] + 0.114 * px[o + 2] : px[o];
      const dark = lum < 128;
      return invert ? !dark : dark;
    },
  };
}

// ---- main ----
const mode = process.argv[2] || "smiley";
let cols, rows, on;
if (mode === "image") {
  const path = process.argv[3];
  cols = parseInt(process.argv[4] || "50", 10);
  const invert = process.argv.includes("--invert");
  const f = imageField(path, cols * 2, invert);
  cols = Math.ceil(f.W / 2); rows = Math.ceil(f.H / 4); on = f.on;
} else {
  cols = 40; rows = 16;
  const W = cols * 2, H = rows * 4;
  on = shapeField(mode, W, H);
}
process.stdout.write(render(cols, rows, on));
