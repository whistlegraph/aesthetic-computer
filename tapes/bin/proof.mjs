// visual evidence. three claims, three pictures:
//   1. the picture really is in the audio (independent spectrogram)
//   2. a real photograph survives a simulated cassette
//   3. the sync bug and its fix, measured across draws rather than once

import sharp from "sharp";
import { createCanvas } from "canvas";
import { writeFileSync, mkdirSync } from "node:fs";
import { encode, decode } from "../lib/spectro.mjs";
import { render } from "../lib/spectrogram.mjs";
import { save, psnr, testcard } from "../lib/image.mjs";
import * as C from "../lib/channel.mjs";

const OUT = "tapes/out";
mkdirSync(OUT, { recursive: true });
const opts = { n: 2048, spacing: 4 };
const W = 240,
  H = 150;

async function grayscale(path) {
  const { data, info } = await sharp(path)
    .resize(W, H, { fit: "cover" })
    .grayscale()
    .normalise()
    .raw()
    .toBuffer({ resolveWithObject: true });
  const out = new Uint8Array(W * H * 4).fill(255);
  for (let i = 0; i < info.width * info.height; i += 1) {
    const v = data[i * info.channels];
    out[i * 4] = out[i * 4 + 1] = out[i * 4 + 2] = v;
  }
  return { width: W, height: H, data: out };
}

const photo = await grayscale("system/public/danzballet.studio/hero-pointe-shoes.jpg");
const card = testcard(W, H);

// ── 1. the picture is in the audio ────────────────────────────────────────
const { samples, plan: p } = encode(card, opts);
const spec = render(samples, { n: 2048, hop: 1024, rate: p.rate });
save(`${OUT}/proof-spectrogram.png`, spec);
save(`${OUT}/proof-card.png`, card);
save(`${OUT}/proof-card-decoded.png`, decode(samples, { width: W, height: H, ...opts }));
console.log(`🔊 spectrogram ${spec.width}×${spec.height}, 0–${(spec.nyquist / 1000).toFixed(1)}kHz`);

// ── 2. a photograph down a cassette ───────────────────────────────────────
const shots = [["source", (x) => x]];
for (const [name, fn] of [
  ["good deck", (x) => C.cassette(x, p.rate, { snr: 55, wow: 0.0006 })],
  ["walkman", (x) => C.cassette(x, p.rate, { snr: 45, wow: 0.003 })],
  ["worn tape", (x) => C.cassette(x, p.rate, { snr: 36, wow: 0.007 })],
  ["walkman → mp3 192", (x) =>
      C.transcode(C.cassette(x, p.rate, { snr: 45, wow: 0.003 }), p.rate, C.codecs["mp3 192"])],
])
  shots.push([name, fn]);

const enc = encode(photo, opts);
const photoRow = shots.map(([name, fn]) => {
  if (name === "source") return { name, img: photo, q: Infinity };
  C.seed(0x5eed);
  const back = decode(fn(enc.samples), { width: W, height: H, ...opts });
  return { name, img: back, q: psnr(photo, back) };
});
photoRow.forEach((r) => console.log(`📷 ${r.name.padEnd(20)} ${r.q === Infinity ? "source" : r.q.toFixed(2) + " dB"}`));

// ── 3. the sync bug, across draws ─────────────────────────────────────────
const route = (x) =>
  C.transcode(C.cassette(x, p.rate, { snr: 45, wow: 0.003 }), p.rate, C.codecs["mp3 192"]);
const TRIALS = 8;

function measure(label, decodeOpts, encodeOpts) {
  const e = encode(card, { ...opts, ...encodeOpts });
  const runs = C.trials(TRIALS, () => {
    const back = decode(route(e.samples), { width: W, height: H, ...opts, ...decodeOpts, ...encodeOpts });
    return { back, drift: Math.abs(back.start - e.start), q: psnr(card, back) };
  });
  const lost = runs.filter((r) => r.drift > 64).length;
  console.log(`🔍 ${label.padEnd(34)} sync lost ${lost}/${TRIALS}`);
  return { label, lost, runs, trials: TRIALS };
}

const before = measure("full-band PHAT, 0.15s chirp (bug)", { syncWide: true }, { chirpSecs: 0.15 });
const after = measure("banded PHAT, 0.4s chirp (fixed)", {}, {});

// ── sheets ────────────────────────────────────────────────────────────────
function sheet(path, rows, cellW, cellH) {
  const pad = 10,
    label = 18;
  const cols = Math.max(...rows.map((r) => r.cells.length));
  const cw = cellW + pad,
    ch = cellH + label + pad + 14;
  const cv = createCanvas(cols * cw + pad, rows.length * ch + pad);
  const g = cv.getContext("2d");
  g.fillStyle = "#0d0d10";
  g.fillRect(0, 0, cv.width, cv.height);
  g.font = "12px monospace";

  rows.forEach((row, ri) => {
    g.fillStyle = "#7bd";
    g.fillText(row.title, pad, ri * ch + pad + 10);
    row.cells.forEach((cell, ci) => {
      const x = ci * cw + pad,
        y = ri * ch + pad + 16;
      const t = createCanvas(cell.img.width, cell.img.height);
      const id = t.getContext("2d").createImageData(cell.img.width, cell.img.height);
      id.data.set(cell.img.data);
      t.getContext("2d").putImageData(id, 0, 0);
      g.drawImage(t, x, y, cellW, cellH);
      g.fillStyle = cell.bad ? "#f77" : "#9e9";
      g.fillText(cell.label, x, y + cellH + 13);
    });
  });
  writeFileSync(path, cv.toBuffer("image/png"));
}

sheet(
  `${OUT}/proof-photo.png`,
  [{ title: "a photograph, encoded to audio and decoded back", cells: photoRow.map((r) => ({ img: r.img, label: `${r.name}  ${r.q === Infinity ? "" : r.q.toFixed(1) + " dB"}` })) }],
  W,
  H,
);

sheet(
  `${OUT}/proof-sync.png`,
  [before, after].map((m) => ({
    title: `${m.label} — sync lost ${m.lost}/${m.trials}`,
    cells: m.runs.map((r, i) => ({
      img: r.back,
      bad: r.drift > 64,
      label: r.drift > 64 ? `#${i} LOST` : `#${i} ${r.q.toFixed(1)}dB`,
    })),
  })),
  W,
  H,
);

console.log(`\n🖼  ${OUT}/proof-spectrogram.png · proof-photo.png · proof-sync.png`);
