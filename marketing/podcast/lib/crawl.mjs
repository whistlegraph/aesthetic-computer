// crawl.mjs — the daily token's image: the episode as a Star Wars crawl.
//
// Drawn here in node rather than on the oven, which repaints a page this
// heavy only every few seconds while it captures. The glyphs are AC's own
// font_1 (6×10 vector drawings), rasterized once and scaled with area
// coverage, so a line sharpens as it nears and fades as it recedes. The
// frames loop seamlessly by construction: frame N would equal frame 0.
//
// The same geometry, driven by (clock), is what crawlPiece() writes as the
// token's live KidLisp $code.

import { readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const AC_ROOT = resolve(HERE, "..", "..", "..", "system", "public", "aesthetic.computer");
const { font_1: FONT } = await import(resolve(AC_ROOT, "disks", "common", "fonts.mjs"));
const GW = FONT.glyphWidth, GH = FONT.glyphHeight;

// ── glyphs ───────────────────────────────────────────────────────────────
const glyphs = new Map();
function glyph(ch) {
  if (glyphs.has(ch)) return glyphs.get(ch);
  const bits = new Uint8Array(GW * GH);
  const set = (x, y) => { if (x >= 0 && x < GW && y >= 0 && y < GH) bits[y * GW + x] = 1; };
  const file = FONT[ch];
  if (typeof file === "string" && ch !== " ") {
    const { commands } = JSON.parse(readFileSync(resolve(AC_ROOT, "disks", "drawings", "font_1", `${file}.json`), "utf8"));
    for (const { name, args } of commands) {
      if (name === "point") set(args[0], args[1]);
      else if (name === "line") {
        let [x0, y0, x1, y1] = args;
        const dx = Math.abs(x1 - x0), dy = -Math.abs(y1 - y0);
        const sx = x0 < x1 ? 1 : -1, sy = y0 < y1 ? 1 : -1;
        let err = dx + dy;
        for (;;) {
          set(x0, y0);
          if (x0 === x1 && y0 === y1) break;
          const e2 = 2 * err;
          if (e2 >= dy) { err += dy; x0 += sx; }
          if (e2 <= dx) { err += dx; y0 += sy; }
        }
      }
    }
  }
  glyphs.set(ch, bits);
  return bits;
}

// Text the font can draw: curly quotes and dashes folded, the rest dropped.
export const fontText = (s) => s
  .replace(/[\u2018\u2019]/g, "'").replace(/[\u201c\u201d]/g, '"').replace(/[\u2013\u2014]/g, "-")
  .replace(/\u2026/g, "...").split("").filter((c) => c === " " || typeof FONT[c] === "string").join("");

// ── layout ───────────────────────────────────────────────────────────────
function wrap(text, cols) {
  const out = []; let line = "";
  for (const word of text.split(/\s+/).filter(Boolean)) {
    if (`${line} ${word}`.trim().length > cols) { out.push(line.trim()); line = word; }
    else line += ` ${word}`;
  }
  if (line.trim()) out.push(line.trim());
  return out;
}

// Cool palettes, turned by the date: title, near lines, far lines.
const PALETTES = [
  [[255, 214, 90], [120, 230, 255], [150, 110, 255]],
  [[255, 150, 200], [110, 255, 220], [70, 130, 255]],
  [[180, 255, 120], [140, 200, 255], [200, 120, 255]],
  [[255, 190, 120], [120, 255, 255], [60, 90, 220]],
  [[240, 240, 255], [150, 255, 190], [60, 170, 255]],
  [[255, 120, 160], [180, 170, 255], [60, 210, 230]],
  [[120, 255, 230], [255, 200, 255], [110, 120, 255]],
];
const mix = (a, b, t) => a.map((v, i) => Math.round(v + (b[i] - v) * t));

export function crawlLayout({ title, body, date, size = 512, cols = 30 }) {
  const lines = [];
  for (const t of wrap(fontText(title).toUpperCase(), cols)) lines.push({ text: t, title: true });
  lines.push(null);
  for (const para of body.split(/\n\s*\n/)) {
    for (const t of wrap(fontText(para), cols)) lines.push({ text: t });
    lines.push(null);
  }
  while (lines.at(-1) === null) lines.pop();

  let h = 0;
  for (const ch of date) h = (h * 31 + ch.charCodeAt(0)) >>> 0;
  const [TITLE, NEAR, FAR] = PALETTES[h % PALETTES.length];
  lines.forEach((l, i) => { if (l) l.color = l.title ? TITLE : mix(NEAR, FAR, i / lines.length); });

  // The plane: k = D/(D+z) runs 1 at the bottom edge to KMIN at the horizon.
  // Text scales with k² and so does the spacing (dy/dz = A·k²/D), so a line's
  // gap always tracks its size.
  const u = size / 512;
  const W = size, H = size, HOR = 70 * u, D = 150, S0 = 2.4 * u, KMIN = 0.42;
  const A = (H - HOR) / (1 - KMIN);
  const L = 12 * S0 * D / A;
  const GONE = D * (1 / KMIN - 1);
  const TRAVEL = lines.length * L + GONE;

  let seed = h || 7;
  const rnd = () => ((seed = (seed * 1103515245 + 12345) >>> 0) / 2 ** 32);
  const stars = Array.from({ length: Math.round(70 * u * u) }, () => {
    const c = mix(NEAR, FAR, rnd()).map((v) => Math.round(v * (0.35 + 0.65 * rnd())));
    return { x: Math.floor(rnd() * W), y: Math.floor(rnd() * H), c };
  });

  return { lines, stars, W, H, HOR, D, S0, KMIN, A, L, GONE, TRAVEL, palette: h % PALETTES.length };
}

// Where line i sits when the crawl has advanced `zNow` along the plane.
function place(g, i, zNow) {
  const z = zNow - i * g.L;
  if (z < 0 || z > g.GONE) return null;
  const k = g.D / (g.D + z);
  return { y: g.H - g.A * (1 - k), s: g.S0 * k * k };
}

// ── raster ───────────────────────────────────────────────────────────────
function drawLine(buf, g, text, color, yBase, s) {
  const w = text.length * GW * s;
  const x0 = g.W / 2 - w / 2;
  // Glyphs hang from the baseline position like AC's write (y is the top).
  for (let ci = 0; ci < text.length; ci++) {
    const bits = glyph(text[ci]);
    const gx = x0 + ci * GW * s;
    for (let py = 0; py < GH; py++) for (let px = 0; px < GW; px++) {
      if (!bits[py * GW + px]) continue;
      // Area coverage of the scaled glyph pixel over the output grid.
      const ax = gx + px * s, ay = yBase + py * s, bx = ax + s, by = ay + s;
      for (let oy = Math.floor(ay); oy < by; oy++) {
        if (oy < 0 || oy >= g.H) continue;
        const cy = Math.min(by, oy + 1) - Math.max(ay, oy);
        for (let ox = Math.floor(ax); ox < bx; ox++) {
          if (ox < 0 || ox >= g.W) continue;
          const a = cy * (Math.min(bx, ox + 1) - Math.max(ax, ox));
          if (a <= 0) continue;
          const o = (oy * g.W + ox) * 3;
          for (let c = 0; c < 3; c++) buf[o + c] = Math.min(255, buf[o + c] + color[c] * a);
        }
      }
    }
  }
}

export function crawlFrame(g, t) {
  const buf = new Float32Array(g.W * g.H * 3);
  for (const { x, y, c } of g.stars) buf.set(c, (y * g.W + x) * 3);
  const zNow = t * g.TRAVEL;
  g.lines.forEach((l, i) => {
    if (!l) return;
    const p = place(g, i, zNow);
    if (p) drawLine(buf, g, l.text, l.color, p.y, p.s);
  });
  return Uint8Array.from(buf, (v) => Math.round(v));
}

// Render a seamless loop to a GIF through ffmpeg (palette per file).
export function renderCrawlGif(g, outPath, { seconds = 30, fps = 12 } = {}) {
  const frames = Math.round(seconds * fps);
  const ff = spawn("ffmpeg", [
    "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${g.W}x${g.H}`, "-r", String(fps), "-i", "-",
    "-filter_complex", "[0]split[a][b];[a]palettegen=max_colors=64:stats_mode=full[p];[b][p]paletteuse=dither=none",
    "-loop", "0", outPath,
  ], { stdio: ["pipe", "ignore", "pipe"] });
  let err = "";
  ff.stderr.on("data", (d) => (err += d));
  const done = new Promise((ok, fail) => ff.on("close", (code) => (code === 0 ? ok(frames) : fail(new Error(`ffmpeg: ${err.trim()}`)))));
  return (async () => {
    for (let n = 0; n < frames; n++) {
      if (!ff.stdin.write(Buffer.from(crawlFrame(g, n / frames).buffer))) await new Promise((r) => ff.stdin.once("drain", r));
    }
    ff.stdin.end();
    return done;
  })();
}

// The same crawl as a live KidLisp piece, moving with (clock). Strings keep
// their punctuation; only \ and " need escaping.
export function crawlPiece(g, { periodMs = 30000 } = {}) {
  const kl = (s) => s.replace(/\\/g, "").replace(/"/g, '\\"');
  const zNow = `(* (/ (mod (clock) ${periodMs}) ${periodMs}) ${g.TRAVEL.toFixed(1)})`;
  const src = ["(wipe black)"];
  for (const { x, y, c } of g.stars) src.push(`(ink ${c.join(" ")})(plot ${x} ${y})`);
  g.lines.forEach((l, i) => {
    if (!l) return;
    const text = kl(l.text);
    const z = `(- ${zNow} ${(i * g.L).toFixed(2)})`;
    const k = `(/ ${g.D} (+ ${g.D} (max ${z} 0)))`;
    const y = `(- (- ${g.H} (* ${g.A.toFixed(2)} (- 1 ${k}))) (* (max (- ${z} ${g.GONE.toFixed(1)}) 0) 100))`;
    const s = `(* ${g.S0} (* ${k} ${k}))`;
    const x = `(- ${g.W / 2} (* ${text.length * GW / 2} ${s}))`;
    src.push(`(ink ${l.color.join(" ")})`, `(write "${text}" ${x} ${y} nil ${s})`);
  });
  return src.join("\n");
}
