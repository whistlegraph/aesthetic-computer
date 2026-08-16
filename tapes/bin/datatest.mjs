// how many bytes make it through a cassette? encode a real kidlisp piece plus
// a plausible input timeline and push it down the same routes.

import { readFileSync } from "node:fs";
import { encode, decode, ber } from "../lib/data.mjs";
import * as C from "../lib/channel.mjs";

const piece = `(wipe 0)
(def t 0)
(each frame
  (set t (+ t 0.01))
  (ink (* 128 (+ 1 (sin t))) 40 200)
  (circle (/ width 2) (/ height 2) (* 60 (+ 1.2 (sin (* t 3))))))`;

// a tape is the piece plus what the hand did to it: 8 minutes of pointer at
// 30Hz, packed as 5 bytes an event.
const events = new Uint8Array(8 * 60 * 30 * 5);
for (let i = 0; i < events.length; i += 1) events[i] = (i * 37 + (i >> 5)) & 255;

const payloads = {
  "kidlisp piece": new TextEncoder().encode(piece),
  "piece + 8min of input": Uint8Array.from([
    ...new TextEncoder().encode(piece),
    ...events,
  ]),
};

const routes = [
  ["untouched", (x) => x],
  ["mp3 192", (x, r) => C.transcode(x, r, C.codecs["mp3 192"])],
  ["mp3 128", (x, r) => C.transcode(x, r, C.codecs["mp3 128"])],
  ["opus 64", (x, r) => C.transcode(x, r, C.codecs["opus 64"])],
  ["cassette: good deck", (x, r) => C.cassette(x, r, { snr: 55, wow: 0.0006 })],
  ["cassette: walkman", (x, r) => C.cassette(x, r, { snr: 45, wow: 0.003 })],
  ["cassette: worn tape", (x, r) => C.cassette(x, r, { snr: 36, wow: 0.007 })],
  [
    "cassette → mp3 192",
    (x, r) => C.transcode(C.cassette(x, r, { snr: 45, wow: 0.003 }), r, C.codecs["mp3 192"]),
  ],
];

// tone spacing is the whole dial: tight packs more bits, wide survives more
// wobble. rect leaks nothing when the clock is exact; blackman-harris keeps
// working once it isn't.
const grades = [
  ["dense  (file / mp3)", { spacing: 1, win: "rect" }],
  ["medium (good deck)", { spacing: 4, win: "bh" }],
  ["rugged (any walkman)", { spacing: 8, win: "bh" }],
];

let rugged = 0;

for (const [label, payload] of Object.entries(payloads)) {
  console.log(`\n📼 ${label} — ${payload.length} bytes`);
  for (const [grade, cfg] of grades) {
    const { samples, plan: p, start, bps } = encode(payload, cfg);
    if (cfg.spacing === 8) rugged = bps;
    const secs = samples.length / p.rate;
    console.log(
      `\n  ${grade}  ${p.rows} tones · ${Math.round(bps)} bps · ${Math.round(bps / 8)} B/s · ${secs.toFixed(1)}s of audio`,
    );
    for (const [name, fn] of routes) {
      const got = decode(fn(samples, p.rate), { at: start, ...cfg });
      if (got.bad) {
        console.log(`     ${name.padEnd(22)} framing lost`);
        continue;
      }
      const { ber: rate, wrong } = ber(payload, got.bytes);
      console.log(
        `     ${name.padEnd(22)} ${rate === 0 ? "perfect" : `BER ${rate.toExponential(2)} (${wrong} bits)`}`,
      );
    }
  }
}

const side = 45 * 60; // one side of a C90
const Bps = rugged / 8;
console.log(
  `\n📊 at the rugged rate (${Math.round(Bps)} B/s) one C90 side holds ` +
    `${((side * Bps) / 1e6).toFixed(2)} MB — about ` +
    `${((side * Bps) / (piece.length + events.length) * 8 / 60).toFixed(0)} hours of recorded piece.`,
);
