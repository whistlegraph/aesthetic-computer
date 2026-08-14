#!/usr/bin/env node
// score-video.mjs — bake a reference video for a cult cut: the score you can
// watch. Not a music video — an instrument-readable picture of the take.
//
// Three bands, top to bottom:
//   · the SECTION RIBBON — the whole arrangement at a glance, playhead sweeping
//   · the PIANO ROLL     — every scored hit, lane per voice, drawn at its time
//   · the WAVEFORM       — the rendered audio, filled up to the playhead
//
// It reads whatever the receipt actually has. With a per-event array it draws
// the real performance; with only sections it still draws the map. Voices that
// name a performer (who: camille/alex/jeffrey) are tinted per person, so the
// three-on-one-pitch stacking is visible, which is the point of the record.
//
// Dependency-free: raw RGB frames are piped straight into ffmpeg, and labels
// are drawn with a built-in 5x7 bitmap font so there is no font to find.
//
//   node pop/cult/bin/score-video.mjs                      # newest v* cut
//   node pop/cult/bin/score-video.mjs --audio out/x.mp3 --events out/x.events.json
//   node pop/cult/bin/score-video.mjs --fps 30 --width 1280 --height 720

import { spawn, spawnSync, execFileSync } from "node:child_process";
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const arg = (k, d = null) => {
  const i = process.argv.indexOf(`--${k}`);
  return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith("--")
    ? process.argv[i + 1] : (process.argv.includes(`--${k}`) ? true : d);
};

// ── pick the cut ──────────────────────────────────────────────────────
function newestCut() {
  const mp3s = readdirSync(OUT).filter((f) => /^cult-remix-v\d.*\.mp3$/.test(f));
  if (!mp3s.length) throw new Error(`no cult-remix mp3 in ${OUT}`);
  return mp3s.map((f) => resolve(OUT, f))
    .sort((a, b) => execFileSync("stat", ["-f%m", b]) - execFileSync("stat", ["-f%m", a]))[0];
}
const audio = resolve(arg("audio") || newestCut());
const stem = basename(audio).replace(/\.mp3$/, "");
// The receipt for a windowed cut lives under the full render's name.
const receiptGuesses = [
  arg("events"),
  resolve(OUT, `${stem}.events.json`),
  resolve(OUT, `${stem.replace(/-extended$/, "")}.events.json`),
  resolve(OUT, `${stem.replace(/-v(\d).*/, "-v$1")}.events.json`),
].filter(Boolean).map((p) => resolve(p));
const receiptPath = receiptGuesses.find(existsSync);
const R = receiptPath ? JSON.parse(readFileSync(receiptPath, "utf8")) : {};
if (!receiptPath) console.warn("! no events receipt found — drawing audio only");

const W = Number(arg("width", 1280)), H = Number(arg("height", 720));
const FPS = Number(arg("fps", 30));
const outPath = resolve(arg("out") || resolve(OUT, `${stem}-score.mp4`));

// ── audio: duration, offset into the full score, and a waveform ───────
const probe = (p, k) => execFileSync("ffprobe",
  ["-v", "error", "-show_entries", `format=${k}`, "-of", "csv=p=0", p], { encoding: "utf8" }).trim();
const DUR = Number(probe(audio, "duration"));
// A cut is a window onto the full render; events are in full-render time.
const OFFSET = Number(arg("offset", (() => {
  const m = /(\d+(?:\.\d+)?)s\+/.exec(R.cutWindow || "");
  if (m) return Number(m[1]);
  const full = Number(R.seconds ?? 0);
  // 50 s and 160 s cuts have both historically started at 48 s.
  return full > DUR + 1 ? Number(arg("start", 48)) : 0;
})()));

const PEAKS = W * 2;
const pcm = execFileSync("ffmpeg", ["-v", "error", "-i", audio, "-ac", "1",
  "-ar", "8000", "-f", "f32le", "-"], { maxBuffer: 1 << 30 });
const samples = new Float32Array(pcm.buffer, pcm.byteOffset, pcm.length / 4);
const wave = new Float32Array(PEAKS);
for (let i = 0; i < PEAKS; i++) {
  const a = Math.floor((i / PEAKS) * samples.length);
  const b = Math.floor(((i + 1) / PEAKS) * samples.length);
  let m = 0;
  for (let j = a; j < b; j++) { const v = Math.abs(samples[j]); if (v > m) m = v; }
  wave[i] = m;
}
let wmax = 0; for (const v of wave) if (v > wmax) wmax = v;
if (wmax > 0) for (let i = 0; i < PEAKS; i++) wave[i] /= wmax;

// ── the score ─────────────────────────────────────────────────────────
const BPM = R.tempoBPM || 120, BEAT = 60 / BPM, BAR = BEAT * 4;
const secOf = (v) => {
  if (Array.isArray(v)) return { a: v[0] * BAR, b: v[1] * BAR };   // bar pair
  if (Array.isArray(v.seconds)) return { a: v.seconds[0], b: v.seconds[1], note: v.narrative || v.note };
  if (Array.isArray(v.bars)) return { a: v.bars[0] * BAR, b: v.bars[1] * BAR, note: v.narrative || v.note };
  return { a: v.startSeconds ?? v.a ?? 0, b: v.endSeconds ?? v.b ?? 0, note: v.narrative || v.note };
};
// Sections arrive either as {name: range} or as an array of act objects.
const sections = (Array.isArray(R.sections)
  ? R.sections.map((v) => ({
      name: v.key || v.name || "",
      a: v.start ?? (v.bars ? v.bars[0] * BAR : 0),
      b: v.end ?? (v.bars ? v.bars[1] * BAR : 0),
      note: v.act || v.narrative || v.note,
    }))
  : Object.entries(R.sections || {}).map(([name, v]) => ({ name, ...secOf(v) }))
).filter((s) => s.b > s.a).sort((a, b) => a.a - b.a);
const events = (R.events || []).filter((e) => Number.isFinite(e.t));

// Lane order is fixed so the picture is comparable between versions.
const LANES = ["kick", "snare", "clap", "hat", "tap", "click", "beep", "skid",
  "bass", "sines", "pad", "sos", "material", "dot", "dash", "cult", "hook", "vox"];
const laneOf = (e) => {
  const v = (e.voice || e.name || "").toLowerCase();
  for (let i = 0; i < LANES.length; i++) if (v.includes(LANES[i])) return i;
  return LANES.length - 1;
};
// Camille / Alex / Jeffrey get their own colour wherever a hit names them.
const WHO = { camille: [255, 120, 190], alex: [120, 220, 255], jeffrey: [255, 200, 90] };
const laneColor = (i) => {
  const t = i / (LANES.length - 1);
  return i < 8 ? [90 + 120 * t, 150 + 60 * t, 255 - 40 * t] : [255 - 60 * t, 90 + 90 * t, 220];
};

// ── framebuffer ───────────────────────────────────────────────────────
const fb = Buffer.alloc(W * H * 3);
const px = (x, y, r, g, b, a = 1) => {
  x |= 0; y |= 0;
  if (x < 0 || y < 0 || x >= W || y >= H) return;
  const o = (y * W + x) * 3;
  fb[o] = fb[o] * (1 - a) + r * a;
  fb[o + 1] = fb[o + 1] * (1 - a) + g * a;
  fb[o + 2] = fb[o + 2] * (1 - a) + b * a;
};
const rect = (x, y, w, h, r, g, b, a = 1) => {
  for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) px(x + i, y + j, r, g, b, a);
};

// A 5x7 font, enough for the labels a score needs.
const FONT = {
  A:"01110100011000111111100011000110001",B:"11110100011000111110100011000111110",
  C:"01110100011000010000100001000101110",D:"11110100011000110001100011000111110",
  E:"11111100001000011110100001000011111",F:"11111100001000011110100001000010000",
  G:"01110100011000010011100011000101111",H:"10001100011000111111100011000110001",
  I:"11111001000010000100001000010011111",J:"00111000010000100001100011000101110",
  K:"10001100101010011000101001001010001",L:"10000100001000010000100001000011111",
  M:"10001110111011110101100011000110001",N:"10001110011010110011100011000110001",
  O:"01110100011000110001100011000101110",P:"11110100011000111110100001000010000",
  Q:"01110100011000110001101011001001101",R:"11110100011000111110101001001010001",
  S:"01111100001000001110000011000011110",T:"11111001000010000100001000010000100",
  U:"10001100011000110001100011000101110",V:"10001100011000110001100010101000100",
  W:"10001100011000110101101011101110001",X:"10001100010101000100010101000110001",
  Y:"10001100010101000100001000010000100",Z:"11111000010001000100010001000011111",
  0:"01110100111010110011100011000101110",1:"00100011000010000100001000010001110",
  2:"01110100010000100110010001000111111",3:"11111000100010000010000011000101110",
  4:"00010001100101010010111110001000010",5:"11111100001111000001000011000101110",
  6:"00110010001000011110100011000101110",7:"11111000010001000100001000010000100",
  8:"01110100011000101110100011000101110",9:"01110100011000101111000010001001100",
  ":":"00000001000010000000000010000100000"," ":"00000000000000000000000000000000000",
  "-":"00000000000000001111100000000000000",".":"00000000000000000000000000110001100",
  "'":"00100001000010000000000000000000000","/":"00001000100010001000100010000000000",
  "+":"00000001000010001111100010000100000","(":"00010001000100001000010000100000010",
  ")":"01000001000001000010000100010001000",",":"00000000000000000000000001100010010",
};
function text(s, x, y, r, g, b, scale = 2, a = 1) {
  let cx = x;
  for (const ch of String(s).toUpperCase()) {
    const gl = FONT[ch];
    if (gl) for (let j = 0; j < 7; j++) for (let i = 0; i < 5; i++)
      if (gl[j * 5 + i] === "1") rect(cx + i * scale, y + j * scale, scale, scale, r, g, b, a);
    cx += 6 * scale;
  }
  return cx - x;
}
const textW = (s, scale = 2) => String(s).length * 6 * scale;

// ── layout ────────────────────────────────────────────────────────────
const PAD = 28;
const RIB_Y = 96, RIB_H = 54;                       // section ribbon
const ROLL_Y = RIB_Y + RIB_H + 34;
const ROLL_H = H - ROLL_Y - 190;                    // piano roll
const WAVE_Y = H - 150, WAVE_H = 96;                // waveform
const PLOT_X = PAD, PLOT_W = W - PAD * 2;
// The roll scrolls: a window of WINDOW seconds centred a third in.
const WINDOW = Number(arg("window", 12));
const NOW_X = PLOT_X + PLOT_W * 0.34;

const T0 = OFFSET, T1 = OFFSET + DUR;
const xOfAbs = (t) => PLOT_X + ((t - T0) / DUR) * PLOT_W;      // whole-track axis
const trackName = (R.track || stem).replace(/[^\x20-\x7e]/g, "-");

function drawFrame(fi) {
  const tRel = fi / FPS, tAbs = OFFSET + tRel;
  fb.fill(0);
  // ground
  rect(0, 0, W, H, 8, 9, 16);
  for (let y = 0; y < H; y++) rect(0, y, W, 1, 8 + 10 * (y / H), 9 + 8 * (y / H), 16 + 22 * (y / H));

  // header
  text(trackName.slice(0, 46), PAD, 26, 235, 235, 245, 3);
  const clock = `${String(Math.floor(tRel / 60)).padStart(2, "0")}:${String(Math.floor(tRel % 60)).padStart(2, "0")}`;
  const total = `${String(Math.floor(DUR / 60)).padStart(2, "0")}:${String(Math.floor(DUR % 60)).padStart(2, "0")}`;
  text(`${clock} / ${total}`, W - PAD - textW(`${clock} / ${total}`, 3), 26, 150, 160, 190, 3);
  const bar = Math.floor((tAbs) / BAR) + 1;
  text(`BAR ${bar}  ${BPM} BPM  ${R.harmony?.key || "B MINOR"}`, PAD, 60, 120, 130, 165, 2);

  // ── section ribbon: the whole arrangement, always visible ──
  rect(PLOT_X, RIB_Y, PLOT_W, RIB_H, 18, 20, 34);
  let current = null;
  for (const s of sections) {
    const x0 = xOfAbs(Math.max(s.a, T0)), x1 = xOfAbs(Math.min(s.b, T1));
    if (x1 <= PLOT_X || x0 >= PLOT_X + PLOT_W) continue;
    const live = tAbs >= s.a && tAbs < s.b;
    if (live) current = s;
    const hue = sections.indexOf(s) / Math.max(1, sections.length - 1);
    const r = 60 + 150 * hue, g = 120 + 60 * (1 - hue), b = 220 - 60 * hue;
    rect(x0, RIB_Y, Math.max(1, x1 - x0 - 1), RIB_H, r, g, b, live ? 0.95 : 0.34);
    if (x1 - x0 > textW(s.name, 1) + 8)
      text(s.name.slice(0, 12), x0 + 5, RIB_Y + RIB_H / 2 - 4, 255, 255, 255, 1, live ? 1 : 0.7);
  }
  // played-so-far shading + playhead on the ribbon
  rect(PLOT_X, RIB_Y, Math.max(0, xOfAbs(tAbs) - PLOT_X), RIB_H, 255, 255, 255, 0.10);
  rect(xOfAbs(tAbs) - 1, RIB_Y - 6, 3, RIB_H + 12, 255, 255, 255, 0.95);
  if (current) {
    text(current.name, PAD, RIB_Y + RIB_H + 10, 240, 240, 250, 2);
    if (current.note)
      text(String(current.note).slice(0, 76), PAD + textW(current.name, 2) + 18,
        RIB_Y + RIB_H + 12, 190, 160, 235, 2);
  }

  // ── piano roll: the actual performance, scrolling ──
  const wA = tAbs - (NOW_X - PLOT_X) / PLOT_W * WINDOW;
  const wB = wA + WINDOW;
  const xOfWin = (t) => PLOT_X + ((t - wA) / WINDOW) * PLOT_W;
  const laneH = ROLL_H / LANES.length;
  for (let i = 0; i < LANES.length; i++) {
    const y = ROLL_Y + i * laneH;
    rect(PLOT_X, y, PLOT_W, Math.max(1, laneH - 1), 14, 16, 28, i % 2 ? 0.55 : 0.85);
    text(LANES[i].slice(0, 8), PLOT_X + 4, y + laneH / 2 - 3, 90, 100, 130, 1);
  }
  // bar lines through the roll
  for (let b = Math.floor(wA / BAR); b <= Math.ceil(wB / BAR); b++) {
    const x = xOfWin(b * BAR);
    if (x < PLOT_X || x > PLOT_X + PLOT_W) continue;
    const four = b % 4 === 0;
    rect(x, ROLL_Y, 1, ROLL_H, 60, 70, 100, four ? 0.55 : 0.22);
  }
  for (const e of events) {
    if (e.t > wB || (e.t + (e.dur || 0.12)) < wA) continue;
    const li = laneOf(e), y = ROLL_Y + li * laneH;
    const x0 = xOfWin(e.t), x1 = xOfWin(e.t + Math.max(e.dur || 0.10, 0.05));
    const who = WHO[(e.who || "").toLowerCase()];
    const [r, g, b] = who || laneColor(li);
    const hot = Math.max(0, 1 - Math.abs(tAbs - e.t) / 0.22);   // flash on strike
    const gain = Math.min(1, (e.gain ?? 0.6) * 1.4);
    const h = Math.max(2, (laneH - 4) * (0.4 + 0.6 * gain));
    rect(x0, y + (laneH - h) / 2, Math.max(2, x1 - x0), h, r, g, b, 0.45 + 0.5 * gain);
    if (hot > 0) rect(x0 - 1, y + 1, Math.max(3, x1 - x0 + 2), laneH - 2, 255, 255, 255, 0.55 * hot);
  }
  rect(NOW_X - 1, ROLL_Y - 8, 2, ROLL_H + 16, 255, 255, 255, 0.9);
  if (!events.length)
    text("no per-event data in receipt - showing sections + audio", PLOT_X + 4,
      ROLL_Y + ROLL_H / 2, 120, 130, 165, 2);

  // ── waveform ──
  const half = WAVE_H / 2, mid = WAVE_Y + half;
  for (let i = 0; i < PLOT_W; i++) {
    const v = wave[Math.floor((i / PLOT_W) * PEAKS)] || 0;
    const h = Math.max(1, v * half * 0.95);
    const played = PLOT_X + i <= xOfAbs(tAbs);
    rect(PLOT_X + i, mid - h, 1, h * 2,
      played ? 120 : 46, played ? 200 : 60, played ? 255 : 92, played ? 0.95 : 0.5);
  }
  rect(xOfAbs(tAbs) - 1, WAVE_Y - 4, 2, WAVE_H + 8, 255, 255, 255, 0.9);

  // performer key, only if the score names people
  if (events.some((e) => e.who)) {
    let x = PLOT_X;
    for (const [nm, c] of Object.entries(WHO)) {
      rect(x, H - 34, 14, 10, c[0], c[1], c[2]);
      x += 20 + text(nm, x + 20, H - 34, 170, 180, 205, 2);
      x += 14;
    }
  }
  return fb;
}

// ── encode ────────────────────────────────────────────────────────────
const frames = Math.ceil(DUR * FPS);
console.log(`▸ score video · ${W}x${H}@${FPS} · ${frames} frames · ${DUR.toFixed(1)}s`);
console.log(`  audio    ${audio}`);
console.log(`  receipt  ${receiptPath || "(none)"} · ${events.length} events · ${sections.length} sections`);
console.log(`  offset   ${OFFSET}s into the full score`);

const ff = spawn("ffmpeg", [
  "-v", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-i", audio,
  "-map", "0:v", "-map", "1:a",
  "-c:v", "libx264", "-preset", "medium", "-crf", "19", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "256k", "-shortest", outPath,
], { stdio: ["pipe", "inherit", "inherit"] });

let i = 0;
function pump() {
  while (i < frames) {
    const buf = drawFrame(i++);
    if (!ff.stdin.write(buf)) { ff.stdin.once("drain", pump); return; }
    if (i % (FPS * 10) === 0) process.stdout.write(`\r  ${((i / frames) * 100).toFixed(0)}%   `);
  }
  ff.stdin.end();
  process.stdout.write("\r  100%   \n");
}
pump();
ff.on("close", (code) => {
  if (code !== 0) { console.error("ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
});
