#!/usr/bin/env node
// lyricline.mjs — the timing proof you can watch: the note-locked vocal
// on a bare click + bass kick, each lyric word flashing up at the exact
// grid moment its audio lands, with a bar counter running. If a word's
// text and its sound arrive together, the boundary is right.
//
//   node pop/imab/bin/lyricline.mjs
//   → out/imab-lyricline.mp4  (needs holyvox.mjs run first)

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const WORK = `${process.env.HOME}/.cache/ac/imab`;
const sh = (cmd, args) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"] });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;

const targets = JSON.parse(readFileSync(`${WORK}/holy-targets.json`, "utf8"));
const VOX = `${OUT}/imab-holyvox.wav`;
if (!existsSync(VOX)) { console.error("✗ run holyvox.mjs first"); process.exit(1); }

// ── audio: 2-bar count-in, then vocal over click + bass kick ──────────
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};
const vox = readF32(VOX);
const PLACE = 2 * BAR;
const total = PLACE + vox.length / SR + 2 * BAR;
const NT = Math.ceil(total * SR);
const mix = new Float32Array(NT);
const tick = (t, freq, gain) => {
  const n = Math.floor(0.03 * SR), a = Math.floor(t * SR);
  for (let i = 0; i < n && a + i < NT; i++) {
    const tt = i / SR;
    mix[a + i] += Math.tanh(1.6 * Math.sin(2 * Math.PI * freq * tt) * Math.exp(-tt / 0.005)) * gain;
  }
};
const kn = Math.floor(0.45 * SR), K = new Float32Array(kn);
{
  let ph = 0, acc = 0;
  const aa = 1 - Math.exp(-2 * Math.PI * 2200 / SR);
  for (let j = 0; j < kn; j++) {
    const t = j / SR;
    ph += 2 * Math.PI * (36 + 70 * Math.exp(-t / 0.036)) / SR;   // the bass kick: deeper
    const raw = Math.tanh(2.1 * (Math.sin(ph) * Math.exp(-t / 0.2) + Math.sin(2 * ph) * Math.exp(-t / 0.05) * 0.2));
    acc += aa * (raw - acc); K[j] = acc;
  }
}
const beats = Math.floor(total / BEAT);
for (let b = 0; b < beats; b++) {
  const t = b * BEAT;
  tick(t, b % 4 === 0 ? 1700 : 1100, b % 4 === 0 ? 0.5 : 0.3);
  if (t >= PLACE - 0.01) {
    const a = Math.floor(t * SR), g = 0.62;
    for (let j = 0; j < kn && a + j < NT; j++) mix[a + j] += K[j] * g;
  }
}
{
  const a = Math.floor(PLACE * SR);
  for (let j = 0; j < vox.length && a + j < NT; j++) mix[a + j] += vox[j] * 2.2;
}
let pk = 0; for (let i = 0; i < NT; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.9) for (let i = 0; i < NT; i++) mix[i] *= 0.9 / pk;
const stb = new Float32Array(NT * 2);
for (let i = 0; i < NT; i++) { stb[2 * i] = mix[i]; stb[2 * i + 1] = mix[i]; }
writeFileSync(`${WORK}/.line.f32`, Buffer.from(stb.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "2",
  "-i", `${WORK}/.line.f32`, "-c:a", "pcm_s16le", `${WORK}/.line.wav`]);

// ── video: PNG word labels (this ffmpeg has no drawtext) + beat flash ─
const LBL = `${WORK}/labels`;
const gen = spawnSync(`${process.env.HOME}/aesthetic-computer/pop/.venv/bin/python`, ["-c", `
import json, os
from PIL import Image, ImageDraw, ImageFont
W = os.path.expanduser("~/.cache/ac/imab")
os.makedirs(W + "/labels", exist_ok=True)
targets = json.load(open(W + "/holy-targets.json"))
font = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 150)
small = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", 44)
for i, t in enumerate(targets):
    label = t["label"].split("\u00b7")[0] + "   " + t["note"]
    img = Image.new("RGBA", (1920, 260), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    wpx = d.textlength(label, font=font)
    d.text(((1920 - wpx) / 2, 40), label, font=font, fill=(255, 255, 255, 255))
    img.save(f"{W}/labels/w{i:02d}.png")
lyr = "i\u2019m a butterfly, flapping for you guys, just a costume, i put on, in my room"
img = Image.new("RGBA", (1920, 80), (0, 0, 0, 0))
d = ImageDraw.Draw(img)
wpx = d.textlength(lyr, font=small)
d.text(((1920 - wpx) / 2, 10), lyr, font=small, fill=(255, 255, 255, 90))
img.save(f"{W}/labels/lyric.png")
print("labels done")
`], { stdio: ["ignore", "inherit", "inherit"] });
if (gen.status !== 0) { console.error("✗ label gen failed"); process.exit(1); }

const inputs = ["-f", "lavfi", "-i", `color=c=0x101018:s=1920x1080:r=30:d=${total.toFixed(2)}`,
  "-i", `${WORK}/.line.wav`, "-i", `${LBL}/lyric.png`];
targets.forEach((_, i) => inputs.push("-i", `${LBL}/w${String(i).padStart(2, "0")}.png`));
let fc = `[0:v][2:v]overlay=0:920[b0]`;
targets.forEach((t, i) => {
  const at = PLACE + t.t, off = at + Math.max(t.dur, 0.45);
  fc += `;[b${i}][${i + 3}:v]overlay=0:410:enable='between(t,${at.toFixed(3)},${off.toFixed(3)})'[b${i + 1}]`;
});
// beat + bar flashes (drawbox pulses — no text needed)
fc += `;[b${targets.length}]drawbox=x=1800:y=60:w=60:h=60:color=white@0.6:t=fill:enable='lt(mod(t,${BEAT.toFixed(4)}),0.09)',` +
  `drawbox=x=1700:y=60:w=60:h=60:color=0x77bbff@0.8:t=fill:enable='lt(mod(t,${BAR.toFixed(4)}),0.11)'[v]`;
const mp4 = `${OUT}/imab-lyricline.mp4`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...inputs,
  "-filter_complex", fc,
  "-map", "[v]", "-map", "1:a", "-c:v", "libx264", "-preset", "fast", "-crf", "20",
  "-c:a", "aac", "-b:a", "192k", "-shortest", mp4]);
console.log(`\u2713 ${mp4}`);
