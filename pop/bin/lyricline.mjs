#!/usr/bin/env node
// lyricline.mjs — the timing proof you can watch, for any /pop vocal: the vocal stem on a
// bare click (every beat) + bass kick (every downbeat), each lyric word flashing up at the
// exact moment its audio lands, a bar.beat counter running. If a word's text and its sound
// arrive together, the boundary is right. Born as pop/imab/bin/lyricline.mjs (a fixed BPM);
// this one takes the chart's real beat times, so a human-timed take reads true.
//
//   node pop/bin/lyricline.mjs --vocal <stem.wav> --words <words.json> --bars <measures.json>
//        [--offset <s>] [--receipt <events.json>] [--from <s> --to <s>] [--out <mp4>] [--title <t>]
//
//   --vocal    the vocal stem on the CUT clock (e.g. src/vox/cut/vocals-natural.wav)
//   --words    [{text, fromMs, toMs, muted?}] on the RECORD clock (e.g. src/words-record.json)
//   --bars     {bars:[{n, t, beats:[…]}]} on the cut clock (e.g. measures.cut.json)
//   --offset   record start on the cut clock (or --receipt <events.json> to read startSec)
//   --from/to  render only this record window (seconds) — the seam you are judging
//   --sync-ms  display latency compensation (default 50, measured by loner's synccal)
//   --audio-only <wav>  write just the click + kick + vocal mix (record clock, from --from) and stop —
//              score-video.mjs --lyrics --click uses this to put the lyric check's clip lanes on a click

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { spawnSync } from "node:child_process";

const args = process.argv.slice(2);
const arg = (k, d) => { const i = args.indexOf(`--${k}`); return i >= 0 ? args[i + 1] : d; };
const VOCAL = resolve(arg("vocal")), WORDS = resolve(arg("words")), BARS = resolve(arg("bars"));
const receipt = arg("receipt") ? JSON.parse(readFileSync(resolve(arg("receipt")), "utf8")) : null;
const OFFSET = Number(arg("offset", receipt?.startSec ?? 0));
const FROM = Number(arg("from", 0)), TO = arg("to") ? Number(arg("to")) : null;
const SYNC = Number(arg("sync-ms", 50)) / 1000;
const TITLE = arg("title", basename(dirname(dirname(WORDS))));
const OUT = resolve(arg("out", `lyricline-${FROM.toFixed(0)}-${TO ? TO.toFixed(0) : "end"}.mp4`));
const WORK = `${process.env.HOME}/.cache/ac/lyricline`; mkdirSync(`${WORK}/labels`, { recursive: true });
const sh = (cmd, a) => spawnSync(cmd, a, { stdio: ["ignore", "ignore", "inherit"] });
const SR = 48_000;

const words = JSON.parse(readFileSync(WORDS, "utf8")).filter((w) => !w.muted);
const bars = JSON.parse(readFileSync(BARS, "utf8")).bars.slice().sort((a, b) => a.t - b.t);
const end = TO ?? Math.max(...words.map((w) => w.toMs / 1000)) + 2;
const PRE = FROM > 0 ? 1.0 : 0;                              // a second of grid before the window
const t0 = FROM - PRE, dur = end - t0;

// ── audio: the vocal slice (cut clock = record + OFFSET) over click + kick on the chart's beats ──
const raw = `${WORK}/.vox.f32`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", String(t0 + OFFSET), "-t", String(dur), "-i", VOCAL,
  "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
const vb = readFileSync(raw); const vox = new Float32Array(vb.buffer, vb.byteOffset, Math.floor(vb.length / 4));
const NT = Math.ceil(dur * SR), mix = new Float32Array(NT);
let vpk = 0; for (let i = 0; i < vox.length; i++) vpk = Math.max(vpk, Math.abs(vox[i]));
const vg = vpk > 0 ? 0.7 / vpk : 1;
for (let i = 0; i < vox.length && i < NT; i++) mix[i] = vox[i] * vg;
const tick = (t, freq, gain) => {
  const n = Math.floor(0.03 * SR), a = Math.floor(t * SR);
  for (let i = 0; i < n && a + i < NT; i++) { const tt = i / SR; if (a + i >= 0) mix[a + i] += Math.tanh(1.6 * Math.sin(2 * Math.PI * freq * tt) * Math.exp(-tt / 0.005)) * gain; }
};
const kn = Math.floor(0.45 * SR), K = new Float32Array(kn);
{ let ph = 0, acc = 0; const aa = 1 - Math.exp(-2 * Math.PI * 2200 / SR);
  for (let j = 0; j < kn; j++) { const t = j / SR; ph += 2 * Math.PI * (36 + 70 * Math.exp(-t / 0.036)) / SR;
    const r = Math.tanh(2.1 * (Math.sin(ph) * Math.exp(-t / 0.2) + Math.sin(2 * ph) * Math.exp(-t / 0.05) * 0.2)); acc += aa * (r - acc); K[j] = acc; } }
const beatsIn = [];                                          // {t (record), bar, beat}
bars.forEach((b, bi) => b.beats.slice(0, -1).forEach((bt, j) => { const t = bt - OFFSET; if (t >= t0 - 0.05 && t <= end) beatsIn.push({ t, bar: bi + 1, beat: j + 1 }); }));   // bar = its count in the record, 1 upward
for (const { t, beat } of beatsIn) {
  const tt = t - t0; tick(tt, beat === 1 ? 1700 : 1100, beat === 1 ? 0.45 : 0.28);
  if (beat === 1) { const a = Math.floor(tt * SR); for (let j = 0; j < kn && a + j < NT; j++) if (a + j >= 0) mix[a + j] += K[j] * 0.55; }
}
let pk = 0; for (let i = 0; i < NT; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < NT; i++) mix[i] *= 0.9 / pk;
const stb = new Float32Array(NT * 2); for (let i = 0; i < NT; i++) { stb[2 * i] = mix[i]; stb[2 * i + 1] = mix[i]; }
writeFileSync(`${WORK}/.line.f32`, Buffer.from(stb.buffer));
const lineWav = arg("audio-only") ? resolve(arg("audio-only")) : `${WORK}/.line.wav`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", `${WORK}/.line.f32`, "-c:a", "pcm_s16le", lineWav]);
if (arg("audio-only")) { console.log(`✓ ${lineWav}  (click + kick + vocal, record ${t0.toFixed(2)}–${end.toFixed(2)}s)`); process.exit(0); }

// ── video: word labels + bar.beat counter as PNGs (no drawtext in this ffmpeg), beat/bar flashes ──
const inWin = words.filter((w) => w.toMs / 1000 >= t0 && w.fromMs / 1000 <= end);
const counter = [...new Set(beatsIn.map((b) => `${b.bar}.${b.beat}`))];
const spec = { W: `${WORK}/labels`, words: inWin.map((w) => w.text), counter, title: `${TITLE}  ·  lyricline  ·  ${FROM.toFixed(1)}–${end.toFixed(1)}s` };
writeFileSync(`${WORK}/spec.json`, JSON.stringify(spec));
const gen = spawnSync(`${process.env.HOME}/aesthetic-computer/pop/.venv/bin/python`, ["-c", `
import json
from PIL import Image, ImageDraw, ImageFont
S = json.load(open("${WORK}/spec.json")); W = S["W"]
big = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 150)
mid = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 72)
small = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", 40)
for i, t in enumerate(S["words"]):
    img = Image.new("RGBA", (1920, 260), (0, 0, 0, 0)); d = ImageDraw.Draw(img)
    d.text(((1920 - d.textlength(t, font=big)) / 2, 40), t, font=big, fill=(255, 255, 255, 255)); img.save(f"{W}/w{i:03d}.png")
for i, c in enumerate(S["counter"]):
    img = Image.new("RGBA", (400, 100), (0, 0, 0, 0)); d = ImageDraw.Draw(img)
    d.text((10, 8), c, font=mid, fill=(119, 187, 255, 255)); img.save(f"{W}/c{i:03d}.png")
img = Image.new("RGBA", (1920, 60), (0, 0, 0, 0)); d = ImageDraw.Draw(img)
d.text((40, 8), S["title"], font=small, fill=(255, 255, 255, 110)); img.save(f"{W}/title.png")
`], { stdio: ["ignore", "inherit", "inherit"] });
if (gen.status !== 0) { console.error("✗ label gen failed"); process.exit(1); }

const inputs = ["-f", "lavfi", "-i", `color=c=0x101018:s=1920x1080:r=60:d=${dur.toFixed(2)}`, "-i", lineWav, "-i", `${WORK}/labels/title.png`];
inWin.forEach((_, i) => inputs.push("-i", `${WORK}/labels/w${String(i).padStart(3, "0")}.png`));
counter.forEach((_, i) => inputs.push("-i", `${WORK}/labels/c${String(i).padStart(3, "0")}.png`));
let fc = `[0:v][2:v]overlay=0:20[b0]`; let k = 0;
inWin.forEach((w, i) => {
  const at = w.fromMs / 1000 - t0 + SYNC, off = Math.max(w.toMs / 1000 - t0 + SYNC, at + 0.45);
  fc += `;[b${k}][${i + 3}:v]overlay=0:410:enable='between(t,${at.toFixed(3)},${off.toFixed(3)})'[b${k + 1}]`; k++;
});
// the bar.beat counter: each label shows from its beat to the next beat
beatsIn.forEach((b, j) => {
  const ci = counter.indexOf(`${b.bar}.${b.beat}`), at = b.t - t0 + SYNC, off = (beatsIn[j + 1]?.t ?? end) - t0 + SYNC;
  fc += `;[b${k}][${inWin.length + 3 + ci}:v]overlay=1500:60:enable='between(t,${at.toFixed(3)},${off.toFixed(3)})'[b${k + 1}]`; k++;
});
const beatEn = beatsIn.map((b) => `between(t,${(b.t - t0 + SYNC).toFixed(3)},${(b.t - t0 + SYNC + 0.09).toFixed(3)})`).join("+");
const barEn = beatsIn.filter((b) => b.beat === 1).map((b) => `between(t,${(b.t - t0 + SYNC).toFixed(3)},${(b.t - t0 + SYNC + 0.12).toFixed(3)})`).join("+") || "0";
fc += `;[b${k}]drawbox=x=1800:y=60:w=60:h=60:color=white@0.6:t=fill:enable='${beatEn}',drawbox=x=1700:y=60:w=60:h=60:color=0x77bbff@0.8:t=fill:enable='${barEn}'[v]`;
const r = sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...inputs, "-filter_complex", fc,
  "-map", "[v]", "-map", "1:a", "-c:v", "libx264", "-preset", "fast", "-crf", "20", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "192k", "-shortest", OUT]);
if (r.status !== 0 || !existsSync(OUT)) { console.error("✗ render failed"); process.exit(1); }
console.log(`✓ ${OUT}  (${inWin.length} words, ${beatsIn.length} beats, ${dur.toFixed(1)} s)`);
