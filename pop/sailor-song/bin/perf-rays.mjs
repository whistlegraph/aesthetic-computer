#!/usr/bin/env node
// perf-rays.mjs — the tracks come off her strings (v81: "spatially, rather than a horizontal bottom overlay").
// Over the retimed performance footage (perf-video.mjs's base, no strip), every lane is a LINE running along the
// string direction, stacked above and below the real strings like a widened set of them (v97: parallel, not a
// fan); the stack follows her sway through src/guitar-track.json (bin/guitar-track.py). A lane's notes are glowing
// beads that slide along it toward the sound hole, arriving abreast of the hole as they sound. The lyric is ONE
// WORD at a time, bottom-middle, in the slab caption lettering — Comic Sans MS Bold bubble glyphs (fill + stroke +
// hard shadow, a hair of jitter and sway per glyph, after menuband's LyricCaption.swift) from bin/glyph-atlas.py's
// atlas — its characters filling in left to right across the word's sung span. Section · bar · chord small, top-left.
//
// Frames are read from the base video through an ffmpeg rawvideo pipe, drawn on in place, and written to another
// ffmpeg that encodes with the record's audio.
//
//   node pop/sailor-song/bin/perf-rays.mjs --audio out/sailor-song-v97.mp3 [--base out/sailor-song-v97-perf.mp4] [--window 6]
//     → out/<stem>-rays.mp4
import { readFileSync, existsSync } from "node:fs";
import { spawn, execFileSync } from "node:child_process";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url)), LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const arg = (k, d = null) => { const i = process.argv.indexOf(`--${k}`); return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith("--") ? process.argv[i + 1] : (process.argv.includes(`--${k}`) ? true : d); };
const AUDIO = resolve(arg("audio")); const stem = basename(AUDIO).replace(/\.(mp3|wav|flac)$/, "");
const BASE = resolve(arg("base") || resolve(OUT, `${stem}-perf.mp4`));
if (!existsSync(BASE)) { console.error(`✗ base video missing: ${BASE} (render perf-video.mjs without --strip first)`); process.exit(1); }
const R = JSON.parse(readFileSync(resolve(OUT, `${stem}.events.json`), "utf8"));
const orchPath = resolve(LANE, "src/orch/orch.events.json");
const ORCH = existsSync(orchPath) ? JSON.parse(readFileSync(orchPath, "utf8")) : {};
const T0 = R.startSec ?? 0, WIN = Number(arg("window", 6));
const probe = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height,r_frame_rate,nb_frames", "-of", "csv=p=0", BASE]).toString().trim().split(",");
const W = +probe[0], H = +probe[1], FPS = eval(probe[2]), NF = +probe[3] || Math.ceil(Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", BASE])) * FPS);
const outPath = resolve(arg("out") || resolve(OUT, `${stem}-rays.mp4`));

// ── the guitar in her frame, per frame: guitar-track.py's sound hole and a point on the string line (take clock, 960×540),
//    reached the way perf-video.mjs reaches the picture — record time → reg time (+startSec) → take time through the timemap ──
const sx = W / 960, sy = H / 540, SR = 48000;
const TRACK = JSON.parse(readFileSync(resolve(LANE, "src/guitar-track.json"), "utf8"));
const TFPS = TRACK.length > 1 ? (TRACK.length - 1) / TRACK.at(-1).t : 30;
const pairs = readFileSync(resolve(LANE, "src/vox/reg/timemap.txt"), "utf8").trim().split("\n").map((l) => l.trim().split(/\s+/).map(Number)).filter((p) => p.length === 2);
const takeOf = (reg) => { const x = reg * SR; let i = 1; while (i < pairs.length - 1 && pairs[i][1] < x) i++;
  const [s0, d0] = pairs[i - 1], [s1, d1] = pairs[i]; const f = d1 === d0 ? 0 : (x - d0) / (d1 - d0); return (s0 + (s1 - s0) * f) / SR; };
let HOLE = { x: 0, y: 0 }, HEAD = { x: 0, y: 0 }, U = { x: 1, y: 0 }, N = { x: 0, y: -1 }, RAY_LEN = 0;
const STR0 = 24, GAP = 12, FACE = { x: 420, y: 275 };                  // first lane 24 px off the string line, 12 px apart (960 wide); her chin
const guitarAt = (now) => { const k = TRACK[Math.min(TRACK.length - 1, Math.max(0, Math.round(takeOf(now) * TFPS)))];
  HOLE = { x: k.hole[0] * sx, y: k.hole[1] * sy }; HEAD = { x: k.head[0] * sx, y: k.head[1] * sy };
  const len = Math.hypot(HEAD.x - HOLE.x, HEAD.y - HOLE.y); RAY_LEN = len * 1.3;
  U = { x: (HEAD.x - HOLE.x) / len, y: (HEAD.y - HOLE.y) / len }; N = { x: U.y, y: -U.x };   // along the strings; up off them
  // the stack above the strings may not cross her face: the top lane, where it passes under her chin, stays below it
  const A = HOLE.y + U.y * (FACE.x * sx - HOLE.x) / U.x, B = N.y - U.y * N.x / U.x, top = (STR0 + (ABOVE - 1) * GAP) * sx;
  const offMax = (FACE.y * sy - A) / B, squeeze = offMax < top ? Math.max(0.4, (offMax - STR0 * sx) / (top - STR0 * sx)) : 1;
  for (const l of LANES) { const off = (STR0 + (Math.abs(l.slot) - 1) * GAP * (l.slot < 0 ? squeeze : 1)) * sx * -Math.sign(l.slot);
    l.ox = HOLE.x + N.x * off; l.oy = HOLE.y + N.y * off; } };

// ── lanes → extra strings; slot < 0 stacks above the real ones, > 0 below, nearest first ──
const LANES = [
  { name: "her",     voices: ["vox"],                                     rgb: [240, 90, 160],  slot: -1, lo: 54, hi: 72 },
  { name: "sister",  voices: ["sister", "chorale", "mirror"],             rgb: [255, 170, 200], slot: -2 },
  { name: "vowels",  voices: ["ooo", "aaa"],                              rgb: [255, 200, 230], slot: -3 },
  { name: "bed",     voices: ["pad", "pad-hit", "high", "arp", "hook", "vib"], rgb: [170, 120, 220], slot: -4 },
  { name: "strings", orchs: ["strings", "cello", "pizz"],                 rgb: [220, 120, 90],  slot: -5 },
  { name: "quartet", orchs: ["vln1", "vln2", "viola", "qcello"],          rgb: [250, 140, 120], slot: -6 },
  { name: "horns",   orch: "horns",                                       rgb: [240, 200, 80],  slot: -7 },
  { name: "harp",    orchs: ["harp", "glock"],                            rgb: [140, 210, 190], slot: -8 },
  { name: "choir",   orch: "aahs",                                        rgb: [230, 180, 230], slot: -9 },
  { name: "guitar",  voices: ["gtr", "gtr-climb", "gtr-pitchy"],          rgb: [230, 170, 70],  slot:1 },
  { name: "kick",    voices: ["kick", "rev-kick", "wub-kick"],            rgb: [90, 150, 240],  slot:2 },
  { name: "kit",     voices: ["clap", "snare", "hat", "tom", "rev-snare", "rev-tom", "rim"], rgb: [120, 180, 250], slot:3 },
  { name: "bass",    voices: ["bass", "sub", "808", "wub"],               rgb: [120, 110, 230], slot:4 },
  { name: "drums",   orchs: ["timpani", "taiko"],                         rgb: [200, 140, 60],  slot:5 },
  { name: "bells",   voices: ["bell", "gong", "rise"],                    rgb: [255, 240, 180], slot:6 },
  { name: "horse",   voices: ["gallop"],                                  rgb: [200, 170, 130], slot:7 },
];
const ABOVE = LANES.filter((l) => l.slot < 0).length;
for (const l of LANES) {
  const keys = l.orchs || (l.orch ? [l.orch] : null);
  l.ev = keys ? keys.flatMap((k) => (ORCH[k] || []).map((e) => ({ t: e.t, dur: e.dur, midi: e.midi, g: e.gain ?? 0.7 })))
    : (R.events || []).filter((e) => l.voices.includes((e.voice || "").toLowerCase())).map((e) => ({ t: e.t, dur: e.dur || 0.08, midi: e.midi, g: e.gain ?? 0.7 }));
  l.ev.sort((a, b) => a.t - b.t); l.cur = 0;
}
const bars = (R.bars || []).slice().sort((a, b) => a.t - b.t);
const sections = (R.sections || []).map((s) => ({ name: s.name.replace(/(\d)$/, " $1"), a: s.start, b: s.end }));
const barAt = (t) => { let b = null; for (const x of bars) { if (t >= x.t) b = x; else break; } return b; };
const secAt = (t) => sections.find((s) => t >= s.a && t < s.b);

// ── lyrics: words-record.json (master clock), one word at a time; lettered from the Comic Sans atlas ──
const WORDS = (() => { try { return JSON.parse(readFileSync(resolve(LANE, "src/words-record.json"), "utf8")); } catch { return []; } })().map((w) => ({ text: w.text, a: w.fromMs / 1000 + T0, b: w.toMs / 1000 + T0 }));
const ATLAS = resolve(LANE, "src/glyph-atlas/comic-72");
if (!existsSync(`${ATLAS}.json`)) execFileSync(resolve(LANE, "../.venv/bin/python"), [resolve(HERE, "glyph-atlas.py")], { stdio: "inherit" });
const GA = JSON.parse(readFileSync(`${ATLAS}.json`, "utf8")), GFILL = readFileSync(`${ATLAS}.fill.raw`), GOUTER = readFileSync(`${ATLAS}.outer.raw`);
const ACCENT = [240, 90, 160];                                            // her lane's pink: the stroke hugs the glyph in it, the shadow is it darkened
const INK = [250, 250, 250], STROKE = ACCENT.map((c) => Math.round(c * 0.85)), SHADOW = ACCENT.map((c) => Math.round(c * 0.4));
const fnv = (str) => { let h = 2166136261; for (const c of Buffer.from(str, "utf8")) h = Math.imul(h ^ c, 16777619) >>> 0; return h; };
const LYRIC_PX = 0.62 * sx;                                              // the 72 px atlas at ~45 px on a 960 frame
for (const w of WORDS) { w.gl = [...w.text].map((ch, i) => { const g = GA.glyphs[ch] || GA.glyphs["?"], h = fnv(`${w.a}:${i}:${ch}`);
  return { g, jy: (((h >>> 8) % 5) - 2) * GA.px / 60, rot: (((h >>> 16) % 5) - 2) * 0.5 * Math.PI / 180, per: 1.6 + ((h >>> 4) % 9) / 10, ph: (h % 100) / 50 }; });
  w.adv = w.gl.reduce((a, x) => a + x.g.adv, 0); }

// ── drawing (rgb24 frame buffer, additive glow) ──
let fb = null;
const px = (x, y, r, g, b, a = 1) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3;
  fb[o] = Math.min(255, fb[o] * (1 - a) + r * a); fb[o + 1] = Math.min(255, fb[o + 1] * (1 - a) + g * a); fb[o + 2] = Math.min(255, fb[o + 2] * (1 - a) + b * a); };
const add = (x, y, r, g, b, a) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3;
  fb[o] = Math.min(255, fb[o] + r * a); fb[o + 1] = Math.min(255, fb[o + 1] + g * a); fb[o + 2] = Math.min(255, fb[o + 2] + b * a); };
const dot = (x, y, rad, r, g, b, a) => { const R2 = rad * rad; for (let j = -rad; j <= rad; j++) for (let i = -rad; i <= rad; i++) { const d2 = i * i + j * j; if (d2 > R2) continue; add(x + i, y + j, r, g, b, a * (1 - Math.sqrt(d2) / (rad + 1)) * 0.9); } };
const line = (x0, y0, x1, y1, r, g, b, a) => { const n = Math.ceil(Math.hypot(x1 - x0, y1 - y0)); for (let k = 0; k <= n; k++) { const f = k / n; px(x0 + (x1 - x0) * f, y0 + (y1 - y0) * f, r, g, b, a); } };
const ray = (x0, y0, ux, uy, len, r, g, b, a) => { const n = Math.ceil(len); for (let k = 0; k <= n; k++) { const f = k / n, x = x0 + ux * k, y = y0 + uy * k, aa = a * (1 - 0.65 * f);
  add(x, y, r, g, b, aa); add(x - uy, y + ux, r, g, b, aa * 0.35); add(x + uy, y - ux, r, g, b, aa * 0.35); } };
const rect = (x, y, w, h, r, g, b, a = 1) => { for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) px(x + i, y + j, r, g, b, a); };
const FONT = {
  A:"01110100011000111111100011000110001",B:"11110100011000111110100011000111110",C:"01110100011000010000100001000101110",D:"11110100011000110001100011000111110",
  E:"11111100001000011110100001000011111",F:"11111100001000011110100001000010000",G:"01110100011000010011100011000101111",H:"10001100011000111111100011000110001",
  I:"11111001000010000100001000010011111",J:"00111000010000100001100011000101110",K:"10001100101010011000101001001010001",L:"10000100001000010000100001000011111",
  M:"10001110111011110101100011000110001",N:"10001110011010110011100011000110001",O:"01110100011000110001100011000101110",P:"11110100011000111110100001000010000",
  Q:"01110100011000110001101011001001101",R:"11110100011000111110101001001010001",S:"01111100001000001110000011000011110",T:"11111001000010000100001000010000100",
  U:"10001100011000110001100011000101110",V:"10001100011000110001100010101000100",W:"10001100011000110101101011101110001",X:"10001100010101000100010101000110001",
  Y:"10001100010101000100001000010000100",Z:"11111000010001000100010001000011111",0:"01110100111010110011100011000101110",1:"00100011000010000100001000010001110",
  2:"01110100010000100110010001000111111",3:"11111000100010000010000011000101110",4:"00010001100101010010111110001000010",5:"11111100001111000001000011000101110",
  6:"00110010001000011110100011000101110",7:"11111000010001000100001000010000100",8:"01110100011000101110100011000101110",9:"01110100011000101111000010001001100",
  ":":"00000001000010000000000010000100000"," ":"00000000000000000000000000000000000","-":"00000000000000001111100000000000000",".":"00000000000000000000000000110001100",
  "#":"01010010101111101010111110101001010","@":"01110100011011110101101111000001110","/":"00001000100010001000100010000000000","+":"00000001000010001111100010000100000",
  "'":"00100001000010000000000000000000000",",":"00000000000000000000000001100010010","?":"01110100010000100010001000000000100","!":"00100001000010000100001000000000100",
  "(":"00010001000100001000010000100000010",")":"01000001000001000010000100010001000","\"":"01010010100000000000000000000000000",";":"00000001000010000000000010000100100",
};
const text = (s, x, y, r, g, b, scale = 1, a = 1) => { let cx = x; for (const ch of String(s).toUpperCase()) { const gl = FONT[ch];
  if (gl) for (let j = 0; j < 7; j++) for (let i = 0; i < 5; i++) if (gl[j * 5 + i] === "1") rect(cx + i * scale, y + j * scale, scale, scale, r, g, b, a); cx += 6 * scale; } return cx - x; };
const textW = (s, scale = 1) => String(s).length * 6 * scale;
const shadowText = (s, x, y, r, g, b, scale, a) => { text(s, x + scale, y + scale, 0, 0, 0, scale, a * 0.7); text(s, x, y, r, g, b, scale, a); };

// a glyph from the atlas, blitted scaled and tilted (bilinear), one coverage layer in one colour
const glyph = (layer, g, cx, cy, sc, rot, r, gg, b, a) => { const w = g.w, h = GA.H, cs = Math.cos(rot), sn = Math.sin(rot), rad = Math.hypot(w, h) * sc / 2;
  for (let Y = Math.floor(cy - rad); Y <= cy + rad; Y++) for (let X = Math.floor(cx - rad); X <= cx + rad; X++) {
    const dx = X - cx, dy = Y - cy, u = (dx * cs + dy * sn) / sc + w / 2, v = (-dx * sn + dy * cs) / sc + h / 2;
    if (u < 0 || v < 0 || u >= w - 1 || v >= h - 1) continue; const u0 = u | 0, v0 = v | 0, fu = u - u0, fv = v - v0, o = v0 * GA.W + g.x + u0;
    const c = (layer[o] * (1 - fu) + layer[o + 1] * fu) * (1 - fv) + (layer[o + GA.W] * (1 - fu) + layer[o + GA.W + 1] * fu) * fv;
    if (c > 2) px(X, Y, r, gg, b, a * c / 255); } };

function drawFrame(fi) {
  const now = T0 + fi / FPS; guitarAt(now);
  // the lanes: a faint string each, from behind the hole out past the headstock, brighter where notes are coming
  for (const l of LANES) { const [r, g, b] = l.rgb;
    ray(l.ox, l.oy, U.x, U.y, RAY_LEN, r, g, b, 0.30); ray(l.ox, l.oy, -U.x, -U.y, RAY_LEN * 0.12, r, g, b, 0.22);
    // notes: a bead's distance along its lane is its time until it sounds; sounding notes sit abreast of the hole and glow
    const ev = l.ev; let k = l.cur; while (k < ev.length && ev[k].t + ev[k].dur < now - 0.4) k++; l.cur = k;
    for (let i = k; i < ev.length && ev[i].t < now + WIN; i++) { const e = ev[i];
      const until = e.t - now, on = until <= 0 && now < e.t + e.dur;
      const d = on ? 0 : Math.max(0, until) / WIN * RAY_LEN; const x = l.ox + U.x * d, y = l.oy + U.y * d;
      const pitchLift = l.lo && e.midi != null ? ((Math.min(l.hi, Math.max(l.lo, e.midi)) - l.lo) / (l.hi - l.lo) - 0.5) * 8 : 0;
      const near = 1 - Math.max(0, until) / WIN, rad = on ? 6 : 2.5 + 2.5 * near, a = (on ? 1 : 0.25 + 0.55 * near) * (0.5 + 0.5 * Math.min(1, e.g));
      const bx = x + N.x * pitchLift, by = y + N.y * pitchLift; dot(bx, by, rad * 2.2, r, g, b, a * 0.18); dot(bx, by, rad, r, g, b, a); if (on) dot(bx, by, 2, 255, 255, 255, 0.8);
      if (on && e.dur > 0.25) { const len = Math.min(RAY_LEN * 0.3, e.dur / WIN * RAY_LEN * 0.5); line(x, y, x + U.x * len, y + U.y * len, r, g, b, 0.5); } } }
  dot(HOLE.x, HOLE.y, 14, 255, 240, 220, 0.18); dot(HOLE.x, HOLE.y, 7, 255, 240, 220, 0.4);
  // the lyric: the word being sung, bottom-middle; its characters fill in left to right across its span, each with a pop
  let wi = -1; for (let i = 0; i < WORDS.length; i++) { if (WORDS[i].a - 0.12 <= now) wi = i; else break; }
  const Wd = wi >= 0 && now < Math.min(WORDS[wi + 1]?.a ?? Infinity, WORDS[wi].b + 1.2) ? WORDS[wi] : null;
  if (Wd) { const sc = Math.min(LYRIC_PX, (W - 60) / Wd.adv), n = Wd.gl.length; let pen = (W - Wd.adv * sc) / 2; const base = H - 44 * sy;
    for (let k = 0; k < n; k++) { const q = Wd.gl[k], g = q.g, lit = Wd.a + (Wd.b - Wd.a) * k / n, dt = now - lit, on = dt >= 0;
      const pop = on && dt < 0.08 ? 1 + 0.14 * (1 - dt / 0.08) : 1, s2 = sc * pop, sway = 0.6 * Math.sin(2 * Math.PI * now / q.per + q.ph);
      const cx = pen + (g.adv / 2) * sc, cy = base - (GA.ascent - GA.H / 2) * sc + (q.jy + sway) * sc; const gx = cx + (g.w / 2 + g.dx - g.adv / 2) * s2;
      glyph(GOUTER, g, gx + 3 * sc, cy + 3 * sc, s2, q.rot, ...SHADOW, 0.9); glyph(GOUTER, g, gx, cy, s2, q.rot, ...STROKE, 1); glyph(GFILL, g, gx, cy, s2, q.rot, ...INK, on ? 1 : 0.38);
      pen += g.adv * sc; } }
  // section · bar · chord, small, top-left
  const bar = barAt(now), sec = secAt(now); shadowText(`${sec ? sec.name : ""}   bar ${bar ? bar.n : "-"}   ${bar ? bar.chord : ""}`, 10, 10, 255, 255, 255, 2, 0.7);
}

// ── the pipes ──
const frameBytes = W * H * 3;
const dec = spawn("ffmpeg", ["-v", "error", "-i", BASE, "-f", "rawvideo", "-pix_fmt", "rgb24", "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawn("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", "-i", AUDIO, "-map", "0:v", "-map", "1:a",
  "-c:v", "libx264", "-crf", "18", "-preset", "medium", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "256k", "-movflags", "+faststart", "-shortest", outPath], { stdio: ["pipe", "inherit", "inherit"] });
let pending = Buffer.alloc(0), fi = 0;
dec.stdout.on("data", (chunk) => { pending = pending.length ? Buffer.concat([pending, chunk]) : chunk;
  while (pending.length >= frameBytes) { fb = Buffer.from(pending.subarray(0, frameBytes)); pending = pending.subarray(frameBytes); drawFrame(fi++);
    if (!enc.stdin.write(fb)) { dec.stdout.pause(); enc.stdin.once("drain", () => dec.stdout.resume()); }
    if (fi % 300 === 0) process.stdout.write(`\r  ${Math.round(100 * fi / NF)}%`); } });
dec.stdout.on("end", () => enc.stdin.end());
enc.on("close", (code) => { console.log(`\r${code ? "✗" : "✓"} ${outPath}  (${fi} frames, ${LANES.length} rays)`); process.exit(code || 0); });
