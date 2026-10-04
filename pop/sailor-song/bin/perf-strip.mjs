#!/usr/bin/env node
// perf-strip.mjs — the track's musical information as a strip for the bottom of the performance
// video (v23: "show the musical / track orchestral information in the video, on the bottom,
// scrolling by"). One lane per family — her voice, her guitar, the kit, the bass, the bed, and
// every orchestra part — with the notes scrolling right to left past a playhead, and above them
// the section, the bar, the chord and the tempo. Reads the engine's receipt (out/<stem>.events.json)
// and the orchestra's (src/orch/orch.events.json), both on the record's clock.
//
//   node pop/sailor-song/bin/perf-strip.mjs --audio out/sailor-song-v23.mp3 [--width 960] [--height 150] [--window 8]
//     → out/<stem>-strip.mp4   (perf-video.mjs --strip overlays it)
import { readFileSync, existsSync } from "node:fs";
import { spawn, execFileSync } from "node:child_process";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url)), LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const arg = (k, d = null) => { const i = process.argv.indexOf(`--${k}`); return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith("--") ? process.argv[i + 1] : (process.argv.includes(`--${k}`) ? true : d); };
const AUDIO = resolve(arg("audio")); const stem = basename(AUDIO).replace(/\.(mp3|wav|flac)$/, "");
const R = JSON.parse(readFileSync(resolve(OUT, `${stem}.events.json`), "utf8"));
const orchPath = resolve(LANE, "src/orch/orch.events.json");
const ORCH = existsSync(orchPath) ? JSON.parse(readFileSync(orchPath, "utf8")) : {};
const W = Number(arg("width", 960)), H = Number(arg("height", 150)), FPS = Number(arg("fps", 30)), WIN = Number(arg("window", 8));
const DUR = Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", AUDIO]).toString());
const T0 = R.startSec ?? 0;
const outPath = resolve(arg("out") || resolve(OUT, `${stem}-strip.mp4`));

// ── lanes: family → the receipt voices that land in it, a colour, a pitch range for note height ──
const LANES = [
  { name: "her",      voices: ["vox"],                               rgb: [240, 90, 160],  lo: 54, hi: 72 },
  { name: "guitar",   voices: ["gtr"],                               rgb: [230, 170, 70] },
  { name: "kit",      voices: ["kick", "clap", "hat", "rim", "conga", "bubble", "impact"], rgb: [90, 150, 240] },
  { name: "bass",     voices: ["bass", "sub", "808"],                rgb: [120, 110, 230], lo: 30, hi: 50 },
  { name: "bed",      voices: ["pad", "high", "descant", "arp", "hook", "vib"], rgb: [170, 120, 220], lo: 60, hi: 93 },
  { name: "mirror",   voices: ["mirror", "chorale", "bell"],            rgb: [255, 170, 200], lo: 50, hi: 90 },
  { name: "strings",  orchs: ["strings", "cello", "pizz"],           rgb: [220, 120, 90],  lo: 40, hi: 88 },
  { name: "horns",    orch: "horns",                                 rgb: [240, 200, 80],  lo: 44, hi: 70 },
  { name: "drums",    orchs: ["timpani", "taiko"],                   rgb: [200, 140, 60] },
  { name: "harp",     orch: "harp",                                  rgb: [140, 210, 190], lo: 56, hi: 84 },
  { name: "glock",    orch: "glock",                                 rgb: [200, 240, 250], lo: 66, hi: 82 },
  { name: "choir",    orch: "aahs",                                  rgb: [230, 180, 230], lo: 56, hi: 72 },
  { name: "quartet",  orchs: ["vln1", "vln2", "viola", "qcello"],      rgb: [250, 140, 120], lo: 44, hi: 95 },
  { name: "piano",    voices: ["piano"],                             rgb: [240, 240, 200], lo: 30, hi: 90 },
  { name: "wub",      voices: ["wub"],                               rgb: [100, 240, 180], lo: 28, hi: 40 },
];
for (const l of LANES) {
  const keys = l.orchs || (l.orch ? [l.orch] : null);
  l.ev = keys ? keys.flatMap((k) => (ORCH[k] || []).map((e) => ({ t: e.t, dur: e.dur, midi: e.midi, g: e.gain ?? 0.7 })))
    : (R.events || []).filter((e) => l.voices.some((v) => (e.voice || "").toLowerCase() === v)).map((e) => ({ t: e.t, dur: e.dur || 0.08, midi: e.midi, g: e.gain ?? 0.7 }));
  l.ev.sort((a, b) => a.t - b.t);
}
const bars = (R.bars || []).slice().sort((a, b) => a.t - b.t);
const sections = (R.sections || []).map((s) => ({ name: s.name.replace(/(\d)$/, " $1"), a: s.start, b: s.end }));
const barAt = (t) => { let b = null; for (const x of bars) { if (t >= x.t) b = x; else break; } return b; };
const secAt = (t) => sections.find((s) => t >= s.a && t < s.b);

// ── framebuffer + the 5×7 font (score-video.mjs's) ──
const fb = Buffer.alloc(W * H * 3);
const px = (x, y, r, g, b, a = 1) => { x |= 0; y |= 0; if (x < 0 || y < 0 || x >= W || y >= H) return; const o = (y * W + x) * 3;
  fb[o] = fb[o] * (1 - a) + r * a; fb[o + 1] = fb[o + 1] * (1 - a) + g * a; fb[o + 2] = fb[o + 2] * (1 - a) + b * a; };
const rect = (x, y, w, h, r, g, b, a = 1) => { x = Math.round(x); y = Math.round(y); w = Math.max(1, Math.round(w)); h = Math.max(1, Math.round(h)); for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) px(x + i, y + j, r, g, b, a); };
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

// ── lyrics (v26): the sung line, the word lit as she sings it ──
const WORDS = (() => { try { return JSON.parse(readFileSync(resolve(LANE, "src/words-record.json"), "utf8")); } catch { return []; } })()
  .map((w) => ({ text: w.text, a: w.fromMs / 1000 + T0, b: w.toMs / 1000 + T0, muted: !!w.muted }));
const norm = (t) => t.toLowerCase().replace(/[^a-z0-9']/g, "");
const LINES = []; { let k = 0; const lyr = (() => { try { return readFileSync(resolve(LANE, "src/lyrics-sung.txt"), "utf8"); } catch { return ""; } })()
    .split("\n").map((l) => l.trim()).filter((l) => l && !l.startsWith("["));
  for (const line of lyr) { const toks = line.split(/\s+/), got = []; let kk = k;
    for (const tok of toks) { const n = norm(tok); if (!n) continue; let j = kk; while (j < WORDS.length && j < kk + 6 && norm(WORDS[j].text) !== n) j++;
      if (j < WORDS.length && j < kk + 6) { got.push({ tok, w: WORDS[j] }); kk = j + 1; } else got.push({ tok, w: null }); }
    const timed = got.filter((g) => g.w); if (timed.length) { LINES.push({ words: got, a: timed[0].w.a, b: timed.at(-1).w.b }); k = kk; } } }
const LYRIC_H = 34;
// ── layout ──
const HEAD = 24 + LYRIC_H, LABEL_W = 56, NOW_X = LABEL_W + Math.round((W - LABEL_W) * 0.3);
const laneH = (H - HEAD) / LANES.length;
const xOf = (t, now) => NOW_X + ((t - now) / WIN) * (W - LABEL_W);
const nFrames = Math.ceil(DUR * FPS);
const cursor = {}; for (const l of LANES) cursor[l.name] = 0;

function drawFrame(fi) {
  const now = T0 + fi / FPS;
  rect(0, 0, W, H, 18, 16, 24);                                             // the strip (overlaid translucent)
  // bar lines and beats across the lanes
  for (const b of bars) { if (b.t + b.dur < now - WIN * 0.3 || b.t > now + WIN * 0.7) continue;
    const x = xOf(b.t, now); rect(x, HEAD, 1, H - HEAD, 255, 255, 255, 0.16);
    for (let j = 1; j < 4; j++) rect(xOf(b.t + b.dur * j / 4, now), HEAD, 1, H - HEAD, 255, 255, 255, 0.05);
    text(String(b.n), x + 3, HEAD + 2, 255, 255, 255, 1, 0.35); }
  // lanes
  LANES.forEach((l, li) => {
    const y0 = HEAD + li * laneH;
    if (li % 2) rect(LABEL_W, y0, W - LABEL_W, laneH, 255, 255, 255, 0.025);
    const [r, g, b] = l.rgb; let lit = false;
    const ev = l.ev; let k = cursor[l.name]; while (k < ev.length && ev[k].t + ev[k].dur + WIN < now) k++; cursor[l.name] = k;
    for (let i = k; i < ev.length && ev[i].t < now + WIN; i++) { const e = ev[i];
      const x0 = xOf(e.t, now), x1 = xOf(e.t + e.dur, now); if (x1 < LABEL_W) continue;
      const on = e.t <= now && now < e.t + e.dur; lit ||= on;
      const h = l.lo ? Math.max(2, laneH * 0.3) : laneH * 0.55;
      const y = l.lo && e.midi != null ? y0 + laneH - 2 - (Math.min(l.hi, Math.max(l.lo, e.midi)) - l.lo) / (l.hi - l.lo) * (laneH - 2 - h) - h : y0 + (laneH - h) / 2;
      rect(Math.max(LABEL_W, x0), y, Math.max(2, x1 - Math.max(LABEL_W, x0)), h, r, g, b, (on ? 0.95 : 0.5) * (0.5 + 0.5 * Math.min(1, e.g))); }
    text(l.name, 4, y0 + laneH / 2 - 3, r, g, b, 1, lit ? 1 : 0.45);
  });
  // playhead
  rect(NOW_X - 1, HEAD - 4, 2, H - HEAD + 4, 255, 255, 255, 0.85);
  // the lyric line (v26): the line in flight, or the next one arriving half a second early
  { const li = LINES.findIndex((l) => now >= l.a - 0.6 && now < l.b + 0.8); const L = li >= 0 ? LINES[li] : null;
    if (L) { const fits = (sc) => L.words.reduce((a, g) => a + textW(g.tok, sc) + 6 * sc, -6 * sc) <= W - 16;
      const sc = fits(3) ? 3 : 2, gap = 6 * sc, total = L.words.reduce((a, g) => a + textW(g.tok, sc) + gap, -gap); let x = Math.max(8, Math.round((W - total) / 2));
      for (const g of L.words) { const w = g.w, on = w && now >= w.a && now < w.b, done = w && now >= w.b;
        const al = on ? 1 : done ? 0.85 : 0.45; const [r, gg, b] = on ? [255, 220, 120] : [255, 255, 255];
        if (on) rect(x - 3, 4, textW(g.tok, sc) + 6, 7 * sc + 6, 255, 200, 90, 0.18);
        text(g.tok, x, sc === 3 ? 7 : 10, r, gg, b, sc, al); x += textW(g.tok, sc) + gap; } } }
  // header: section · bar · chord · tempo, and the record's section ribbon
  const bar = barAt(now), sec = secAt(now);
  const bpm = bar ? Math.round(240 / bar.dur) : 0;
  const line = `${sec ? sec.name : ""}   bar ${bar ? bar.n : "-"}   ${bar ? bar.chord : ""}   ${bpm || "-"} bpm   g# minor`;
  text(line, 6, LYRIC_H + 6, 255, 255, 255, 2, 0.9);
  const ribX = W - 260, ribW = 250;
  sections.forEach((s, si) => { const x0 = ribX + (s.a - T0) / DUR * ribW, x1 = ribX + (s.b - T0) / DUR * ribW; const live = sec === s;
    rect(x0, LYRIC_H + 8, Math.max(1, x1 - x0 - 1), 8, 220 - 120 * si / 7, 120 + 60 * si / 7, 200, live ? 0.95 : 0.35); });
  rect(ribX + (now - T0) / DUR * ribW - 1, LYRIC_H + 5, 2, 14, 255, 255, 255, 0.95);
}

const ff = spawn("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-c:v", "libx264", "-crf", "16", "-preset", "fast", "-pix_fmt", "yuv420p", outPath], { stdio: ["pipe", "inherit", "inherit"] });
let fi = 0;
const pump = () => { let ok = true; while (fi < nFrames && ok) { drawFrame(fi++); ok = ff.stdin.write(Buffer.from(fb)); if (fi % 300 === 0) process.stdout.write(`\r  ${Math.round(100 * fi / nFrames)}%`); }
  if (fi >= nFrames) ff.stdin.end(); else ff.stdin.once("drain", pump); };
pump();
ff.on("close", (code) => { console.log(`\r${code ? "✗" : "✓"} ${outPath}  (${nFrames} frames, ${LANES.length} lanes)`); process.exit(code || 0); });
