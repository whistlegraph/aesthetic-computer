#!/usr/bin/env node
// score-video.mjs — the /pop score video for sailor s, wannadash's shape
// (pop/cult/bin/score-video.mjs) with a fourth band for the singalong:
//
//   · SECTION RIBBON — the whole record at a glance, playhead sweeping
//   · PIANO ROLL     — every scored hit from the C engine's receipt, one lane
//                      per voice, scrolling, flashing on the strike
//   · LYRIC          — the line she is singing, the sung word lit, the next
//                      line waiting under it (src/words-record.json, local)
//   · WAVEFORM       — the master, filled up to the playhead
//
// The receipt (out/sailor-song-v7.events.json) is in full-take time and the
// record starts at its startSec, so OFFSET = startSec. Bars come from the
// receipt's real bar times (her clock), not from tempo arithmetic.
//
// Dependency-free: raw RGB frames piped into ffmpeg; labels in the 5x7 font.
//
//   node pop/sailor-song/bin/score-video.mjs                 # newest sailor-song-v*.mp3
//   node pop/sailor-song/bin/score-video.mjs --audio out/sailor-song-v7-master.wav
//   node pop/sailor-song/bin/score-video.mjs --fps 30 --width 1920 --height 1080

import { spawn, execFileSync } from "node:child_process";
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

function newestCut() {
  const mp3s = readdirSync(OUT).filter((f) => /^sailor-song-v\d+\.mp3$/.test(f));
  if (!mp3s.length) throw new Error(`no sailor-song mp3 in ${OUT}`);
  return mp3s.map((f) => resolve(OUT, f))
    .sort((a, b) => execFileSync("stat", ["-f%m", b]) - execFileSync("stat", ["-f%m", a]))[0];
}
const base = resolve(arg("audio") || newestCut());
const stem = basename(base).replace(/\.(mp3|wav|flac)$/, "").replace(/-master$/, "");
const receiptPath = [arg("events"), resolve(OUT, `${stem}.events.json`)].filter(Boolean).map((p) => resolve(p)).find(existsSync);
// v20: --click — the lyric check's clip lanes over her stem on a bare click + kick (pop/bin/lyricline.mjs
// builds the mix from the chart's real beats) instead of the full record: the timing proof, with clips
const CLICK = !!arg("click");
const clickWav = resolve(OUT, `.${stem}-click.wav`);
if (CLICK) execFileSync("node", [resolve(LANE, "../bin/lyricline.mjs"), "--vocal", resolve(LANE, "src/vox/cut/vocals-natural.wav"),
  "--words", resolve(LANE, "src/words-record.json"), "--bars", resolve(LANE, "measures.cut.json"), "--receipt", receiptPath,
  "--from", "0", "--audio-only", clickWav], { stdio: "inherit" });
const audio = CLICK ? clickWav : base;
const R = receiptPath ? JSON.parse(readFileSync(receiptPath, "utf8")) : {};
if (!receiptPath) console.warn("! no events receipt found — drawing audio only");
const wordsPath = resolve(LANE, "src/words-record.json");
const WORDS = existsSync(wordsPath) ? JSON.parse(readFileSync(wordsPath, "utf8")) : [];
const lyricLines = existsSync(resolve(LANE, "src/lyrics-sung.txt"))
  ? readFileSync(resolve(LANE, "src/lyrics-sung.txt"), "utf8").split("\n").map((l) => l.trim()).filter((l) => l && !l.startsWith("[")) : [];

const W = Number(arg("width", 1280)), H = Number(arg("height", 720));
const FPS = Number(arg("fps", 30));
const LYRICS = !!arg("lyrics");   // v19: the lyric check — only the words, over her own waveform
const outPath = resolve(arg("out") || resolve(OUT, `${stem}-${LYRICS ? "lyrics" : "score"}${CLICK ? "-click" : ""}.mp4`));

const probe = (p, k) => execFileSync("ffprobe", ["-v", "error", "-show_entries", `format=${k}`, "-of", "csv=p=0", p], { encoding: "utf8" }).trim();
const DUR = Number(probe(audio, "duration"));
const OFFSET = Number(arg("offset", R.startSec ?? 0));

const PEAKS = W * 2;
const pcm = execFileSync("ffmpeg", ["-v", "error", "-i", audio, "-ac", "1", "-ar", "8000", "-f", "f32le", "-"], { maxBuffer: 1 << 30 });
const samples = new Float32Array(pcm.buffer, pcm.byteOffset, pcm.length / 4);
const wave = new Float32Array(PEAKS);
for (let i = 0; i < PEAKS; i++) {
  const a = Math.floor((i / PEAKS) * samples.length), b = Math.floor(((i + 1) / PEAKS) * samples.length);
  let m = 0; for (let j = a; j < b; j++) { const v = Math.abs(samples[j]); if (v > m) m = v; }
  wave[i] = m;
}
let wmax = 0; for (const v of wave) if (v > wmax) wmax = v;
if (wmax > 0) for (let i = 0; i < PEAKS; i++) wave[i] /= wmax;

// her voice alone, in record time: the edited lead stem (cut time = record time + startSec), as a 200 Hz envelope
const ENV_HZ = 200;
let venv = new Float32Array(0);
if (LYRICS) {
  const vp = ["vocals-natural", "vocals-aesthetivox"].map((n) => resolve(LANE, `src/vox/cut/${n}.wav`)).find(existsSync);
  const raw = execFileSync("ffmpeg", ["-v", "error", "-i", vp, "-ac", "1", "-ar", "8000", "-f", "f32le", "-"], { maxBuffer: 1 << 30 });
  const v = new Float32Array(raw.buffer, raw.byteOffset, raw.length / 4), hop = 8000 / ENV_HZ;
  venv = new Float32Array(Math.floor(v.length / hop));
  for (let i = 0; i < venv.length; i++) { let m = 0; for (let j = i * hop; j < (i + 1) * hop; j++) m = Math.max(m, Math.abs(v[j])); venv[i] = m; }
  let vm = 0; for (const x of venv) vm = Math.max(vm, x); for (let i = 0; i < venv.length; i++) venv[i] = Math.sqrt(venv[i] / (vm || 1));   // sqrt: quiet singing still reads
  console.log(`  voice    ${vp}`);
}
const envAt = (tAbs) => venv[Math.floor(tAbs * ENV_HZ)] || 0;   // tAbs is cut time

// ── the score ─────────────────────────────────────────────────────────
const sections = (R.sections || []).map((v) => ({ name: v.name, a: v.start, b: v.end })).filter((s) => s.b > s.a).sort((a, b) => a.a - b.a);
// beats per bar come from the chart the engine played (the receipt's bars carry only t/dur)
const chartPath = ["measures.cut.json", "measures.reg.json", "measures.json"].map((f) => resolve(LANE, f)).find(existsSync);
const chartBars = chartPath ? JSON.parse(readFileSync(chartPath, "utf8")).bars : [];
const bars = (R.bars || []).slice().sort((a, b) => a.t - b.t).map((b) => {
  const cb = chartBars.find((c) => Math.abs(c.t - b.t) < 0.02);
  return { ...b, beats: cb ? cb.beats : [0, 1, 2, 3, 4].map((j) => b.t + (j * b.dur) / 4) };
});
const events = (R.events || []).filter((e) => Number.isFinite(e.t));
const barAt = (t) => { let n = 0; for (const b of bars) { if (t >= b.t) n = b.n; else break; } return n; };
const chordAt = (t) => { let c = ""; for (const b of bars) { if (t >= b.t) c = b.chord; else break; } return c; };
const bpmAt = (t) => { for (const b of bars) if (t >= b.t && t < b.t + b.dur) return Math.round(4 * 60 / b.dur); return 0; };

// lane order is fixed so versions compare
const LANES = ["kick", "snare", "clap", "hat", "block", "shaker", "tamb", "conga", "bass", "808", "sub", "pad", "high", "descant", "arp", "vib", "bell", "hook", "impact", "gtr", "vox"];
const laneOf = (e) => { const v = (e.voice || "").toLowerCase(); for (let i = 0; i < LANES.length; i++) if (v === LANES[i] || v.includes(LANES[i])) return i; return LANES.length - 1; };
const laneColor = (i) => {
  const t = i / (LANES.length - 1);
  if (i < 8) return [40 + 100 * t * 2, 110 + 50 * t, 230 - 40 * t];            // the kit: blues
  if (i < 14) return [190 - 60 * t, 80 + 60 * t, 190];                          // the bed: violets
  if (i < 19) return [230, 150 - 50 * t, 40];                                    // the AC voices: golds
  return [230, 60, 140];                                                          // her: pink
};

// ── framebuffer ───────────────────────────────────────────────────────
const fb = Buffer.alloc(W * H * 3);
const px = (x, y, r, g, b, a = 1) => {
  x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return;
  const o = (y * W + x) * 3;
  fb[o] = fb[o] * (1 - a) + r * a; fb[o + 1] = fb[o + 1] * (1 - a) + g * a; fb[o + 2] = fb[o + 2] * (1 - a) + b * a;
};
const rect = (x, y, w, h, r, g, b, a = 1) => { for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) px(x + i, y + j, r, g, b, a); };

const FONT = {
  A:"01110100011000111111100011000110001",B:"11110100011000111110100011000111110",C:"01110100011000010000100001000101110",
  D:"11110100011000110001100011000111110",E:"11111100001000011110100001000011111",F:"11111100001000011110100001000010000",
  G:"01110100011000010011100011000101111",H:"10001100011000111111100011000110001",I:"11111001000010000100001000010011111",
  J:"00111000010000100001100011000101110",K:"10001100101010011000101001001010001",L:"10000100001000010000100001000011111",
  M:"10001110111011110101100011000110001",N:"10001110011010110011100011000110001",O:"01110100011000110001100011000101110",
  P:"11110100011000111110100001000010000",Q:"01110100011000110001101011001001101",R:"11110100011000111110101001001010001",
  S:"01111100001000001110000011000011110",T:"11111001000010000100001000010000100",U:"10001100011000110001100011000101110",
  V:"10001100011000110001100010101000100",W:"10001100011000110101101011101110001",X:"10001100010101000100010101000110001",
  Y:"10001100010101000100001000010000100",Z:"11111000010001000100010001000011111",
  0:"01110100111010110011100011000101110",1:"00100011000010000100001000010001110",2:"01110100010000100110010001000111111",
  3:"11111000100010000010000011000101110",4:"00010001100101010010111110001000010",5:"11111100001111000001000011000101110",
  6:"00110010001000011110100011000101110",7:"11111000010001000100001000010000100",8:"01110100011000101110100011000101110",
  9:"01110100011000101111000010001001100",":":"00000001000010000000000010000100000"," ":"00000000000000000000000000000000000",
  "-":"00000000000000001111100000000000000",".":"00000000000000000000000000110001100","'":"00100001000010000000000000000000000",
  "/":"00001000100010001000100010000000000","+":"00000001000010001111100010000100000","(":"00010001000100001000010000100000010",
  ")":"01000001000001000010000100010001000",",":"00000000000000000000000001100010010","?":"01110100010000100010001000000000100",
  "!":"00100001000010000100001000000000100","#":"01010010101111101010111110101001010","@":"01110100011011110101101111000001110",
  "\"":"01010010100000000000000000000000000",";":"00000001000010000000000010000100100",
};
function text(s, x, y, r, g, b, scale = 2, a = 1) {
  let cx = x;
  for (const ch of String(s).toUpperCase()) {
    const gl = FONT[ch];
    if (gl) for (let j = 0; j < 7; j++) for (let i = 0; i < 5; i++) if (gl[j * 5 + i] === "1") rect(cx + i * scale, y + j * scale, scale, scale, r, g, b, a);
    cx += 6 * scale;
  }
  return cx - x;
}
const textW = (s, scale = 2) => String(s).length * 6 * scale;

// ── lyric lines from the record-time words ────────────────────────────
// words carry no line ids: rebuild the lines by walking lyrics-sung.txt's word counts
const lines = [];
{ let k = 0;
  for (const l of lyricLines) {
    const toks = l.split(/\s+/).filter(Boolean); const ws = WORDS.slice(k, k + toks.length); k += toks.length;
    if (ws.length) lines.push({ text: l, words: ws, a: ws[0].fromMs / 1000, b: ws[ws.length - 1].toMs / 1000 });
  }
  if (k !== WORDS.length) console.warn(`! lyric/word count drift: ${k} lyric words vs ${WORDS.length} timed`);
  lines.sort((a, b) => a.a - b.a);
}

// ── layout ────────────────────────────────────────────────────────────
const PAD = 28;
const RIB_Y = 96, RIB_H = 54;
const ROLL_Y = RIB_Y + RIB_H + 34;
const LYR_H = 96;                                    // the lyric band
const TRAY_H = 34;                                   // v18: the word tray — each sung word as a block, beat clicks under
const WAVE_H = 84, WAVE_Y = H - 30 - WAVE_H;
const LYR_Y = WAVE_Y - 18 - LYR_H;
const TRAY_Y = LYR_Y - 10 - TRAY_H;
const ROLL_H = TRAY_Y - 12 - ROLL_Y;
const PLOT_X = PAD, PLOT_W = W - PAD * 2;
const WINDOW = Number(arg("window", 12));
const NOW_X = PLOT_X + PLOT_W * 0.34;
const T0 = OFFSET, T1 = OFFSET + DUR;
const xOfAbs = (t) => PLOT_X + ((t - T0) / DUR) * PLOT_W;
const title = arg("title", "sailor s"), artist = arg("artist", "s@ge");

function drawFrame(fi) {
  const tRel = fi / FPS, tAbs = OFFSET + tRel;
  for (let y = 0; y < H; y++) rect(0, y, W, 1, 250 - 8 * (y / H), 247 - 8 * (y / H), 242 - 10 * (y / H));   // light mode: warm paper

  // header
  text(`${title}  -  ${artist}`, PAD, 26, 40, 30, 45, 3);
  const clock = `${String(Math.floor(tRel / 60)).padStart(2, "0")}:${String(Math.floor(tRel % 60)).padStart(2, "0")}`;
  const total = `${String(Math.floor(DUR / 60)).padStart(2, "0")}:${String(Math.floor(DUR % 60)).padStart(2, "0")}`;
  text(`${clock} / ${total}`, W - PAD - textW(`${clock} / ${total}`, 3), 26, 120, 110, 130, 3);
  text(`BAR ${barAt(tAbs)}  ${bpmAt(tAbs) || "-"} BPM  ${chordAt(tAbs) || ""}  G# MINOR +13C`, PAD, 60, 130, 120, 140, 2);

  // section ribbon
  rect(PLOT_X, RIB_Y, PLOT_W, RIB_H, 232, 226, 222);
  let current = null;
  sections.forEach((s, si) => {
    const x0 = xOfAbs(Math.max(s.a, T0)), x1 = xOfAbs(Math.min(s.b, T1));
    if (x1 <= PLOT_X || x0 >= PLOT_X + PLOT_W) return;
    const live = tAbs >= s.a && tAbs < s.b; if (live) current = s;
    const hue = si / Math.max(1, sections.length - 1);
    rect(x0, RIB_Y, Math.max(1, x1 - x0 - 1), RIB_H, 220 - 90 * hue, 90 + 50 * hue, 140 + 70 * hue, live ? 0.9 : 0.3);
    if (x1 - x0 > textW(s.name, 1) + 8) text(s.name.slice(0, 12), x0 + 5, RIB_Y + RIB_H / 2 - 4, 255, 255, 255, 1, live ? 1 : 0.7);
  });
  rect(PLOT_X, RIB_Y, Math.max(0, xOfAbs(tAbs) - PLOT_X), RIB_H, 0, 0, 0, 0.08);
  rect(xOfAbs(tAbs) - 1, RIB_Y - 6, 3, RIB_H + 12, 30, 20, 40, 0.95);
  if (current) text(current.name, PAD, RIB_Y + RIB_H + 10, 50, 40, 60, 2);

  // piano roll
  const wA = tAbs - (NOW_X - PLOT_X) / PLOT_W * WINDOW, wB = wA + WINDOW;
  const xOfWin = (t) => PLOT_X + ((t - wA) / WINDOW) * PLOT_W;
  const laneH = ROLL_H / LANES.length;
  for (let i = 0; i < LANES.length; i++) {
    const y = ROLL_Y + i * laneH;
    rect(PLOT_X, y, PLOT_W, Math.max(1, laneH - 1), 238, 233, 228, i % 2 ? 0.6 : 0.95);
    text(LANES[i].slice(0, 8), PLOT_X + 4, y + laneH / 2 - 3, 150, 140, 160, 1);
  }
  for (const b of bars) {
    if (b.t < wA - 1 || b.t > wB) continue;
    const x = xOfWin(b.t); if (x < PLOT_X || x > PLOT_X + PLOT_W) continue;
    rect(x, ROLL_Y, 1, ROLL_H, 120, 110, 140, b.n % 4 === 1 ? 0.5 : 0.2);
  }
  for (const e of events) {
    if (e.t > wB || (e.t + (e.dur || 0.12)) < wA) continue;
    const li = laneOf(e), y = ROLL_Y + li * laneH;
    const x0 = xOfWin(e.t), x1 = xOfWin(e.t + Math.max(e.dur || 0.10, 0.05));
    const [r, g, b] = laneColor(li);
    const hot = Math.max(0, 1 - Math.abs(tAbs - e.t) / 0.22);
    const gain = Math.min(1, (e.gain ?? 0.6) * 1.4);
    const h = Math.max(2, (laneH - 4) * (0.4 + 0.6 * gain));
    rect(x0, y + (laneH - h) / 2, Math.max(2, x1 - x0), h, r, g, b, 0.45 + 0.5 * gain);
    if (hot > 0) rect(x0 - 1, y + 1, Math.max(3, x1 - x0 + 2), laneH - 2, 40, 30, 50, 0.45 * hot);
  }
  rect(NOW_X - 1, ROLL_Y - 8, 2, ROLL_H + 16, 30, 20, 40, 0.9);

  // word tray: the scrolling window again — beat clicks as ticks, every word a block with its text
  rect(PLOT_X, TRAY_Y, PLOT_W, TRAY_H, 236, 231, 226, 0.95);
  for (const b of bars) for (let j = 0; j < b.beats.length - 1; j++) { const tb = b.beats[j]; if (tb < wA || tb > wB) continue;
    const x = xOfWin(tb); rect(x, TRAY_Y + TRAY_H - (j === 0 ? 12 : 7), 1, j === 0 ? 12 : 7, 120, 40, 90, j === 0 ? 0.9 : 0.5); }
  for (const w of WORDS) {
    if (w.muted) continue;                                   // held under another voice: not sung here
    const a = OFFSET + w.fromMs / 1000, b = OFFSET + w.toMs / 1000;
    if (b < wA || a > wB) continue;
    const x0 = xOfWin(a), x1 = xOfWin(b), now = tAbs >= a && tAbs < b, sung = tAbs >= b;
    rect(x0, TRAY_Y + 3, Math.max(2, x1 - x0 - 1), TRAY_H - 18, now ? 220 : sung ? 90 : 170, now ? 40 : sung ? 70 : 160, now ? 90 : sung ? 110 : 175, now ? 0.95 : 0.7);
    if (x1 - x0 > textW(w.text, 1) + 4) text(w.text.replace(/[^\w' ]/g, ""), x0 + 2, TRAY_Y + 7, now ? 255 : 30, now ? 245 : 20, now ? 240 : 40, 1, 0.95);
  }
  rect(NOW_X - 1, TRAY_Y - 2, 2, TRAY_H + 4, 30, 20, 40, 0.9);

  lyricBand(tRel, LYR_Y, LYR_H, 4);

  // waveform
  const half = WAVE_H / 2, mid = WAVE_Y + half;
  for (let i = 0; i < PLOT_W; i++) {
    const v = wave[Math.floor((i / PLOT_W) * PEAKS)] || 0, h = Math.max(1, v * half * 0.95);
    const played = PLOT_X + i <= xOfAbs(tAbs);
    rect(PLOT_X + i, mid - h, 1, h * 2, played ? 220 : 200, played ? 40 : 190, played ? 90 : 200, played ? 0.95 : 0.6);
  }
  rect(xOfAbs(tAbs) - 1, WAVE_Y - 4, 2, WAVE_H + 8, 30, 20, 40, 0.9);
  return fb;
}

// the line being sung (or the next one coming), the sung word lit
function lyricBand(tRel, LYR_Y, LYR_H, maxScale) {
  rect(PLOT_X, LYR_Y, PLOT_W, LYR_H, 244, 240, 236, 0.9);
  // v19: the line that started most recently (0.4 s early), else the first one coming — never flips back
  let li = -1; lines.forEach((l, k) => { if (tRel >= l.a - 0.4) li = k; });
  if (li < 0) li = 0;
  if (li >= 0) {
    const L = lines[li], next = lines[li + 1];
    let scale = maxScale; while (scale > 2 && textW(L.text, scale) > PLOT_W - 24) scale--;
    const fadeIn = Math.min(1, Math.max(0, (tRel - (L.a - 1.0)) / 0.5)), fadeOut = tRel > L.b + 1.0 ? Math.max(0, 1 - (tRel - L.b - 1.0) / 0.6) : 1;
    const alpha = Math.min(fadeIn, fadeOut);
    let cx = PLOT_X + (PLOT_W - textW(L.text, scale)) / 2;
    const toks = L.text.split(/\s+/);
    toks.forEach((tok, k) => {
      const w = L.words[k]; const a = w ? w.fromMs / 1000 : 0, b = w ? w.toMs / 1000 : 0;
      const muted = !!(w && w.muted);                        // in the line but not sung: stays grey
      const sung = !muted && tRel >= b, now = !muted && tRel >= a && tRel < b;
      const wpx = textW(tok, scale) - 1 * scale;
      // the unsung word in grey, the sung word in ink, the word being sung wiped
      // left to right in red — piecewise through its whisper tokens when it has them
      text(tok, cx, LYR_Y + 16, sung ? 40 : 150, sung ? 30 : 140, sung ? 45 : 150, scale, alpha * (sung ? 1 : 0.7));
      if (now) {
        let frac = (tRel - a) / Math.max(0.01, b - a);
        const T = w.tokens || [];
        if (T.length > 1) { const tot = tok.length; let done = 0; frac = 1;
          for (let q = 0; q < T.length; q++) { const ta = T[q].fromMs / 1000, tb = T[q].toMs / 1000, len = T[q].text.length / tot;
            if (tRel < ta) { frac = done; break; } if (tRel < tb) { frac = done + len * (tRel - ta) / Math.max(0.01, tb - ta); break; } done += len; } }
        const wipe = Math.max(0, Math.min(1, frac)) * wpx;
        // redraw the wiped part in red by clipping: draw the word again, then paper over the unsung remainder
        text(tok, cx, LYR_Y + 16, 220, 40, 90, scale, alpha);
        rect(cx + wipe, LYR_Y + 12, Math.max(0, wpx - wipe + 2), 7 * scale + 8, 244, 240, 236, 1);
        text(tok, cx, LYR_Y + 16, 150, 140, 150, scale, alpha * 0.7 * 0);   // (nothing) — keep alignment
        // the grey remainder back on top of the paper patch
        { let ccx = cx; for (const ch of tok.toUpperCase()) { const gl = FONT[ch]; if (gl) for (let jj = 0; jj < 7; jj++) for (let ii = 0; ii < 5; ii++) if (gl[jj * 5 + ii] === "1") { const gx = ccx + ii * scale; if (gx + scale > cx + wipe) rect(gx, LYR_Y + 16 + jj * scale, scale, scale, 150, 140, 150, alpha * 0.7); } ccx += 6 * scale; } }
      }
      cx += textW(tok, scale) + 6 * scale;
    });
    if (next) text(next.text, PLOT_X + (PLOT_W - textW(next.text, 2)) / 2, LYR_Y + LYR_H - 24, 120, 110, 130, 2, 0.6 * alpha);
  }
}

// ── the lyric check (loner's review-score lanes): every word is a BLOCK on its own track,
// rotating through LANES_N rows so neighbours never share one; each block carries the word and
// her own waveform inside it; the grey strip on top is the whole voice, so gaps and bleed show
const LWIN = Number(arg("window", 7)), LANES_N = Number(arg("lanes", 4));
function drawLyrics(fi) {
  const tRel = fi / FPS, tAbs = OFFSET + tRel;
  for (let y = 0; y < H; y++) rect(0, y, W, 1, 250 - 8 * (y / H), 247 - 8 * (y / H), 242 - 10 * (y / H));
  text(`${title}  -  lyric check`, PAD, 22, 40, 30, 45, 3);
  const clock = `${String(Math.floor(tRel / 60)).padStart(2, "0")}:${String((tRel % 60).toFixed(1)).padStart(4, "0")}`;
  text(clock, W - PAD - textW(clock, 3), 22, 120, 110, 130, 3);
  const recBar = (() => { let k = 0; bars.forEach((b, i) => { if (tAbs >= b.t) k = i + 1; }); return k; })();
  text(`BAR ${recBar}   HER VOICE ONLY - ONE BLOCK PER WORD (FORCED ALIGNMENT)`, PAD, 52, 130, 120, 140, 2);
  const wA = tRel - LWIN * 0.35, xOf = (t) => PLOT_X + ((t - wA) / LWIN) * PLOT_W, tOf = (x) => wA + ((x - PLOT_X) / PLOT_W) * LWIN;
  // the whole voice, thin, on top
  const VY = 84, VH = 60, vmid = VY + VH / 2;
  rect(PLOT_X, VY, PLOT_W, VH, 236, 231, 226, 0.95);
  for (let i = 0; i < PLOT_W; i++) { const h = Math.max(1, envAt(tOf(PLOT_X + i) + OFFSET) * (VH / 2 - 3)); rect(PLOT_X + i, vmid - h, 1, h * 2, 150, 140, 160, 0.8); }
  // the word lanes
  const LY = VY + VH + 12, LH = 64, LG = 6;
  for (let r = 0; r < LANES_N; r++) rect(PLOT_X, LY + r * (LH + LG), PLOT_W, LH, 238, 233, 228, 0.9);
  // v20: the bars, so words can be read against the measure — every bar and every beat carries
  // an ID you can say out loud: the bar's count in the RECORD, 1 upward in playing order (regular —
  // the chart's take-bar numbers jump at every seam, "27 to 45"); the chart's number sits small beside
  // it for the splice, which anchors in chart bars. A beat is bar.beat ("28.3").
  const gridBottom = LY + LANES_N * (LH + LG) - LG;
  bars.forEach((b, k) => {
    const tb = b.t - OFFSET; if (tb + b.dur < wA || tb > wA + LWIN) return;
    const id = k + 1, xb = xOf(tb), heavy = id % 4 === 1;
    if (xb >= PLOT_X && xb <= PLOT_X + PLOT_W) {
      rect(xb, VY - 18, heavy ? 3 : 2, gridBottom - VY + 18 + 14, 90, 40, 120, heavy ? 0.75 : 0.5);
      text(String(id), xb + 5, VY - 18, 90, 40, 120, 2, 0.95);
      if (b.n !== id) text(`chart ${b.n}`, xb + 5 + textW(String(id), 2) + 4, VY - 14, 150, 130, 160, 1, 0.7);
    }
    for (let j = 0; j < b.beats.length - 1; j++) { const xj = xOf(b.beats[j] - OFFSET);
      if (xj < PLOT_X || xj > PLOT_X + PLOT_W) continue;
      if (j > 0) rect(xj, VY - 8, 1, gridBottom - VY + 8 + 14, 90, 40, 120, 0.22);
      text(`${id}.${j + 1}`, xj + 3, gridBottom + 3, 110, 80, 130, 1, j === 0 ? 0.9 : 0.6);   // the beat's address, under the lanes
    }
  });
  const PAL = [[120, 70, 190], [60, 110, 200], [30, 150, 140], [200, 120, 40], [170, 60, 150]];
  WORDS.forEach((w, k) => {
    if (w.muted) return;
    const a = w.fromMs / 1000, b = w.toMs / 1000;
    if (b < wA || a > wA + LWIN) return;
    const r = k % LANES_N, y0 = LY + r * (LH + LG), mid = y0 + LH / 2;
    const x0 = Math.max(PLOT_X, xOf(a)), x1 = Math.min(PLOT_X + PLOT_W, xOf(b));
    const now = tRel >= a && tRel < b, sung = tRel >= b;
    const [cr, cg, cb] = now ? [220, 40, 90] : PAL[k % PAL.length];
    rect(x0, y0 + 2, Math.max(2, x1 - x0), LH - 4, cr, cg, cb, now ? 0.28 : 0.14);
    for (let x = Math.ceil(x0); x < x1; x++) { const h = Math.max(1, envAt(tOf(x) + OFFSET) * (LH / 2 - 8)); rect(x, mid - h + 6, 1, h * 2 - 6, cr, cg, cb, sung || now ? 0.9 : 0.55); }
    rect(x0, y0 + 2, 2, LH - 4, cr, cg, cb, 0.95); rect(x1 - 2, y0 + 2, 2, LH - 4, cr, cg, cb, 0.6);
    text(w.text.replace(/[^\w' ?,]/g, ""), x0 + 5, y0 + 6, now ? 200 : 40, now ? 30 : 30, now ? 80 : 50, 2, 0.95);
  });
  rect(xOf(tRel) - 1, VY - 6, 3, LY + LANES_N * (LH + LG) - VY + 6, 30, 20, 40, 0.95);
  lyricTicker(tRel, xOf(tRel), H - 24 - 110, 110);
  return fb;
}

// one stream of words scrolling right to left, the sung word crossing the playhead as she sings it
const TSC = 4, TGAP = 6 * TSC;
const REC = WORDS.filter((w) => !w.muted).sort((a, b) => a.fromMs - b.fromMs);   // record order: verse 2 plays before chorus 1 now
const tickX = []; { let x = 0; for (const w of REC) { tickX.push(x); x += textW(w.text, TSC) + TGAP; } }
function lyricTicker(tRel, headX, Y, Hh) {
  rect(PLOT_X, Y, PLOT_W, Hh, 244, 240, 236, 0.9);
  let k = REC.findIndex((w) => w.fromMs / 1000 > tRel) - 1; if (k < -1) k = REC.length - 1;
  let pos;                                                     // stream x under the playhead
  if (k < 0) pos = tickX[0] - (REC[0].fromMs / 1000 - tRel) * 120;
  else { const w = REC[k], a = w.fromMs / 1000, nb = k + 1 < REC.length ? REC[k + 1].fromMs / 1000 : a + 1;
    const x0 = tickX[k], x1 = k + 1 < REC.length ? tickX[k + 1] : x0 + textW(w.text, TSC);
    pos = x0 + (x1 - x0) * Math.min(1, (tRel - a) / Math.max(0.05, nb - a)); }
  const ty = Y + (Hh - 7 * TSC) / 2;
  REC.forEach((w, q) => {
    const x = headX + tickX[q] - pos, ww = textW(w.text, TSC);
    if (x > PLOT_X + PLOT_W || x + ww < PLOT_X) return;
    const a = w.fromMs / 1000, b = w.toMs / 1000, now = tRel >= a && tRel < b, sung = tRel >= b;
    const [r, g, bl] = now ? [220, 40, 90] : sung ? [40, 30, 45] : [160, 150, 160];
    text(w.text, x, ty, r, g, bl, TSC, now || sung ? 1 : 0.75);
    rect(x, ty + 7 * TSC + 6, Math.max(2, (b - a) * 60), 3, r, g, bl, 0.6);   // its sung length, as a bar
  });
  rect(PLOT_X, Y, 2, Hh, 244, 240, 236, 1); rect(headX - 1, Y + 6, 3, Hh - 12, 30, 20, 40, 0.9);
}

// ── encode ────────────────────────────────────────────────────────────
const frames = Math.ceil(DUR * FPS);
console.log(`▸ score video · ${W}x${H}@${FPS} · ${frames} frames · ${DUR.toFixed(1)}s`);
console.log(`  audio    ${audio}`);
console.log(`  receipt  ${receiptPath || "(none)"} · ${events.length} events · ${sections.length} sections · ${lines.length} lyric lines`);
console.log(`  offset   ${OFFSET}s into the take`);
const ff = spawn("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-i", audio, "-map", "0:v", "-map", "1:a", "-c:v", "libx264", "-preset", "medium", "-crf", "19", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "320k", "-movflags", "+faststart", "-shortest", outPath], { stdio: ["pipe", "inherit", "inherit"] });
let i = 0;
function pump() {
  while (i < frames) {
    const buf = LYRICS ? drawLyrics(i++) : drawFrame(i++);
    if (!ff.stdin.write(buf)) { ff.stdin.once("drain", pump); return; }
    if (i % (FPS * 10) === 0) process.stdout.write(`\r  ${((i / frames) * 100).toFixed(0)}%   `);
  }
  ff.stdin.end(); process.stdout.write("\r  100%   \n");
}
pump();
ff.on("close", (code) => { if (code !== 0) { console.error("ffmpeg failed"); process.exit(1); } console.log(`✓ ${outPath}`); });
