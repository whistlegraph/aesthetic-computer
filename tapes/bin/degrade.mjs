// send the same picture down every route that a real audio file travels and
// see what comes back. sync is left on — a route that breaks the chirp is a
// route that breaks the picture, and we want to know that.

import { createCanvas } from "canvas";
import { writeFileSync, mkdirSync } from "node:fs";
import { encode, decode } from "../lib/spectro.mjs";
import { testcard, psnr, load } from "../lib/image.mjs";
import * as C from "../lib/channel.mjs";

const args = process.argv.slice(2);
const src = args.find((a) => !a.startsWith("--"));
const img = src ? load(src) : testcard(256, 128);
const opts = { n: 2048, spacing: 4 };
const { samples, plan: p, start } = encode(img, opts);
const rate = p.rate;

const routes = [
  ["untouched", (x) => x],
  ["16-bit quantize", (x) => C.quantize(x, 16)],
  ["8-bit quantize", (x) => C.quantize(x, 8)],
  ["gain -20dB", (x) => C.gain(x, 0.1)],
  ["clipped +6dB", (x) => C.quantize(C.gain(x, 2), 16)],
  ["44.1→48→44.1", (x) => C.resample(C.resample(x, 44100 / 48000), 48000 / 44100)],
  ...Object.entries(C.codecs).map(([name, spec]) => [
    name,
    (x) => C.transcode(x, rate, spec),
  ]),
  ["cassette: good deck", (x) => C.cassette(x, rate, { snr: 55, wow: 0.0006 })],
  ["cassette: walkman", (x) => C.cassette(x, rate, { snr: 45, wow: 0.003 })],
  ["cassette: worn tape", (x) => C.cassette(x, rate, { snr: 36, wow: 0.007 })],
  [
    "cassette → mp3 192",
    (x) => C.transcode(C.cassette(x, rate, { snr: 45, wow: 0.003 }), rate, C.codecs["mp3 192"]),
  ],
];

mkdirSync("tapes/out", { recursive: true });
const results = [];

// every noisy route is a random variable, so ask it more than once. a single
// draw is how a route that fails half the time reads as fixed.
const TRIALS = 3;

for (const [name, fn] of routes) {
  const runs = C.trials(TRIALS, () => {
    const back = decode(fn(samples), {
      width: img.width,
      height: img.height,
      ...opts,
    });
    return { psnr: psnr(img, back), drift: back.start - start, img: back };
  });
  const lost = runs.filter((r) => Math.abs(r.drift) > 64).length;
  const ok = runs.filter((r) => Math.abs(r.drift) <= 64);
  const worst = ok.length ? Math.min(...ok.map((r) => r.psnr)) : NaN;
  const row = { name, psnr: worst, lost, img: (ok[0] ?? runs[0]).img };
  results.push(row);
  console.log(
    name.padEnd(22),
    (worst === Infinity ? "exact" : isFinite(worst) ? worst.toFixed(2) : "—").padStart(7),
    "dB worst of",
    TRIALS,
    lost ? `  ⚠️ sync lost ${lost}/${TRIALS}` : "",
  );
}

// contact sheet, so the numbers can be checked against what it looks like.
const cols = 4,
  rows = Math.ceil(results.length / cols);
const pad = 8,
  label = 18,
  cw = img.width + pad * 2,
  ch = img.height + label + pad * 2;
const cv = createCanvas(cols * cw, rows * ch);
const g = cv.getContext("2d");
g.fillStyle = "#111";
g.fillRect(0, 0, cv.width, cv.height);
g.font = "11px monospace";

results.forEach((r, i) => {
  const x = (i % cols) * cw + pad,
    y = Math.floor(i / cols) * ch + pad;
  if (r.img) {
    const tile = createCanvas(img.width, img.height);
    const id = tile.getContext("2d").createImageData(img.width, img.height);
    id.data.set(r.img.data);
    tile.getContext("2d").putImageData(id, 0, 0);
    g.drawImage(tile, x, y);
  }
  g.fillStyle = r.lost ? "#f99" : "#9f9";
  const q = r.psnr === Infinity ? "exact" : isFinite(r.psnr) ? r.psnr.toFixed(1) + " dB" : "fail";
  g.fillText(`${r.name}  ${q}${r.lost ? ` sync ${r.lost}/${TRIALS}` : ""}`, x, y + img.height + 13);
});

writeFileSync("tapes/out/degrade.png", cv.toBuffer("image/png"));
console.log("\n🖼  contact sheet → tapes/out/degrade.png");
