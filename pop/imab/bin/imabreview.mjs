#!/usr/bin/env node
// imabreview.mjs — the track-based review mp4, loner-style: the full
// imabclub mix under a scrolling bar ruler (numbers every 4 bars, a
// fixed center playhead), section labels, beat/bar pulse boxes, and
// every vocal utterance flashing its word+note at the exact placed
// moment in all three passes. This is the alignment-perfecting tool:
// if a word flashes off its bar, the placement is wrong.
//
//   node pop/imab/bin/imabreview.mjs
//   → out/imabclub-review.mp4

import { readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const WORK = `${process.env.HOME}/.cache/ac/imab`;
const sh = (cmd, args) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"] });
const BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const BARS = 96, PASS = [16, 40, 64];
const PXB = 120;                                    // pixels per bar on the ruler

const MP3 = `${OUT}/imabclub-draft1.mp3`;
if (!existsSync(MP3)) { console.error("✗ render imabclub first"); process.exit(1); }
const targets = JSON.parse(readFileSync(`${WORK}/holy-targets.json`, "utf8"));
const dur = BARS * BAR + 4;

// ── PNG assets: ruler strip, section labels, word labels ──────────────
const gen = spawnSync(`${process.env.HOME}/aesthetic-computer/pop/.venv/bin/python`, ["-c", `
import json, os
from PIL import Image, ImageDraw, ImageFont
W = os.path.expanduser("~/.cache/ac/imab")
os.makedirs(W + "/review", exist_ok=True)
big = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 130)
med = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 64)
sml = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", 40)
PXB, BARS = ${PXB}, ${BARS}
ruler = Image.new("RGBA", (PXB * BARS + 1920, 150), (0, 0, 0, 0))
d = ImageDraw.Draw(ruler)
for b in range(BARS + 1):
    x = 960 + b * PXB
    major = b % 4 == 0
    d.rectangle([x, 70 if major else 95, x + 3, 148], fill=(255, 255, 255, 230 if major else 110))
    if major and b < BARS:
        d.text((x + 10, 8), str(b + 1), font=med, fill=(255, 255, 255, 220))
    for q in range(1, 4):
        xq = x + q * PXB // 4
        d.rectangle([xq, 118, xq + 2, 148], fill=(255, 255, 255, 60))
ruler.save(f"{W}/review/ruler.png")
for name, txt in [("s0", "INTRO"), ("s1", "PASS 1"), ("s2", "LIFT"), ("s3", "PASS 2"),
                  ("s4", "BREAK"), ("s5", "DROP · PASS 3"), ("s6", "OUTRO")]:
    img = Image.new("RGBA", (1920, 110), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    wpx = d.textlength(txt, font=med)
    d.text(((1920 - wpx) / 2, 20), txt, font=med, fill=(140, 190, 255, 235))
    img.save(f"{W}/review/{name}.png")
targets = json.load(open(W + "/holy-targets.json"))
for i, t in enumerate(targets):
    label = t["label"].split("\\u00b7")[0].replace("'", "\\u2019") + "   " + t["note"]
    img = Image.new("RGBA", (1920, 220), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    wpx = d.textlength(label, font=big)
    d.text(((1920 - wpx) / 2, 30), label, font=big, fill=(255, 255, 255, 255))
    img.save(f"{W}/review/w{i:02d}.png")
print("review assets done")
`], { stdio: ["ignore", "inherit", "inherit"] });
if (gen.status !== 0) process.exit(1);

// ── compose ───────────────────────────────────────────────────────────
const R = `${WORK}/review`;
const inputs = ["-f", "lavfi", "-i", `color=c=0x0d0d16:s=1920x1080:r=30:d=${dur.toFixed(2)}`, "-i", MP3, "-i", `${R}/ruler.png`];
const SECT = [[0, "s0"], [16, "s1"], [32, "s2"], [40, "s3"], [56, "s4"], [64, "s5"], [80, "s6"]];
SECT.forEach(([, n]) => inputs.push("-i", `${R}/${n}.png`));
targets.forEach((_, i) => inputs.push("-i", `${R}/w${String(i).padStart(2, "0")}.png`));
// the ruler scrolls right-to-left so the playhead (center) reads the bar
let fc = `[0:v][2:v]overlay=x='960-${PXB / BAR}*t-t*0':y=60[b0]`;
fc = `[0:v][2:v]overlay=x='960 - (t/${BAR.toFixed(5)})*${PXB} - 960':y=60[b0]`;
let bi = 0;
SECT.forEach(([bar, _], i) => {
  const t0 = bar * BAR, t1 = (SECT[i + 1]?.[0] ?? BARS) * BAR;
  fc += `;[b${bi}][${3 + i}:v]overlay=0:250:enable='between(t,${t0.toFixed(2)},${t1.toFixed(2)})'[b${bi + 1}]`; bi++;
});
targets.forEach((t, i) => {
  const wins = PASS.map((door) => [door * BAR + 0.05 + t.t, door * BAR + 0.05 + t.t + Math.max(t.dur, 0.45)]);
  const en = wins.map(([a, z]) => `between(t,${a.toFixed(3)},${z.toFixed(3)})`).join("+");
  fc += `;[b${bi}][${3 + SECT.length + i}:v]overlay=0:460:enable='${en}'[b${bi + 1}]`; bi++;
});
fc += `;[b${bi}]drawbox=x=956:y=40:w=8:h=190:color=0xffcc55@0.95:t=fill,` +
  `drawbox=x=40:y=40:w=50:h=50:color=white@0.55:t=fill:enable='lt(mod(t,${BEAT.toFixed(4)}),0.08)',` +
  `drawbox=x=110:y=40:w=50:h=50:color=0x77bbff@0.85:t=fill:enable='lt(mod(t,${BAR.toFixed(4)}),0.1)'[v]`;
const mp4 = `${OUT}/imabclub-review.mp4`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...inputs,
  "-filter_complex", fc, "-map", "[v]", "-map", "1:a",
  "-c:v", "libx264", "-preset", "fast", "-crf", "21", "-c:a", "aac", "-b:a", "192k", "-shortest", mp4]);
console.log(`✓ ${mp4}`);
