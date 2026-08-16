#!/usr/bin/env node
// stereo-sync-beat — QA track for the Juke's room-audio split play.
//
// 120 BPM, 60 s. Beats 1 & 3 are a LEFT-only 1 kHz tick (Neo's channel),
// beats 2 & 4 a RIGHT-only 1.8 kHz tock (Blueberry's), so machine-to-machine
// timing smear is instantly audible as a limp in the ping-pong. Every 8th
// bar plays all four beats in BOTH channels at 1.4 kHz — the simultaneity
// check: those must land as ONE hit in the room, not a flam.
//
//   node pop/synctest/bin/render-sync-beat.mjs
//   → pop/synctest/out/stereo-sync-beat.{wav,mp3}

import { execFileSync } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const OUT = join(dirname(fileURLToPath(import.meta.url)), "..", "out");
mkdirSync(OUT, { recursive: true });

const SR = 48000;
const BPM = 120;
const SECONDS = 60;
const beat = (60 / BPM) * SR; // samples per beat
const total = SECONDS * SR;
const L = new Float32Array(total);
const R = new Float32Array(total);

function hit(buf, start, hz, seconds = 0.045, amp = 0.85) {
  const n = Math.min(Math.floor(seconds * SR), total - start);
  for (let i = 0; i < n; i++) {
    const env = Math.exp((-5 * i) / n); // sharp pluck
    buf[start + i] += Math.sin((2 * Math.PI * hz * i) / SR) * env * amp;
  }
}

const beats = Math.floor(total / beat);
for (let b = 0; b < beats; b++) {
  const start = Math.round(b * beat);
  const bar = Math.floor(b / 4);
  const inBar = b % 4;
  if ((bar + 1) % 8 === 0) {
    hit(L, start, 1400);
    hit(R, start, 1400); // center: must be ONE hit in the room
  } else if (inBar === 0 || inBar === 2) {
    hit(L, start, 1000); // Neo speaks
  } else {
    hit(R, start, 1800); // Blueberry answers
  }
}

// 16-bit interleaved WAV
const pcm = Buffer.alloc(total * 4);
for (let i = 0; i < total; i++) {
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(L[i] * 32767))), i * 4);
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(R[i] * 32767))), i * 4 + 2);
}
const header = Buffer.alloc(44);
header.write("RIFF", 0);
header.writeUInt32LE(36 + pcm.length, 4);
header.write("WAVEfmt ", 8);
header.writeUInt32LE(16, 16);
header.writeUInt16LE(1, 20);
header.writeUInt16LE(2, 22);
header.writeUInt32LE(SR, 24);
header.writeUInt32LE(SR * 4, 28);
header.writeUInt16LE(4, 32);
header.writeUInt16LE(16, 34);
header.write("data", 36);
header.writeUInt32LE(pcm.length, 40);

const wav = join(OUT, "stereo-sync-beat.wav");
writeFileSync(wav, Buffer.concat([header, pcm]));
const mp3 = join(OUT, "stereo-sync-beat.mp3");
execFileSync("ffmpeg", ["-y", "-i", wav, "-codec:a", "libmp3lame", "-b:a", "320k", mp3],
             { stdio: "ignore" });
console.log(`wrote ${wav} and ${mp3}`);
