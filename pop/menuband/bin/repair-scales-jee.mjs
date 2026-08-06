#!/usr/bin/env node
// Fast iteration pass for the scales reel: replace both G syllables in the
// already-assembled vocal master with the verified standalone "jee" take.

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { decodeAudioMono } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
const SR = 48_000;
const vocalPath = `${OUT}/menuband-scales-vocal.wav`;
const jeePath = `${OUT}/sung/menuband-scales/words/jee-source-sung.wav`;

function writeWavF32(path, samples) {
  const buf = Buffer.alloc(44 + samples.length * 4);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + samples.length * 4, 4);
  buf.write("WAVE", 8); buf.write("fmt ", 12); buf.writeUInt32LE(16, 16);
  buf.writeUInt16LE(3, 20); buf.writeUInt16LE(1, 22);
  buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 4, 28);
  buf.writeUInt16LE(4, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(samples.length * 4, 40);
  for (let i = 0; i < samples.length; i++) buf.writeFloatLE(samples[i], 44 + i * 4);
  writeFileSync(path, buf);
}

const { audio: vocal } = decodeAudioMono(vocalPath, SR);
const { audio: jee } = decodeAudioMono(jeePath, SR);
const sourceAt = Math.floor(0.04 * SR);
const count = Math.floor(0.54 * SR);
const cleanG = jee.slice(sourceAt, sourceAt + count);
const fade = Math.floor(0.008 * SR);

for (const t of [6.1111, 16.6667]) {
  const targetAt = Math.floor(t * SR);
  for (let i = 0; i < cleanG.length && targetAt + i < vocal.length; i++) {
    const edge = Math.min(1, i / fade, (cleanG.length - 1 - i) / fade);
    const a = Math.sin(Math.max(0, edge) * Math.PI / 2) ** 2;
    vocal[targetAt + i] = vocal[targetAt + i] * (1 - a) + cleanG[i] * a;
  }
}

let peak = 0;
for (const v of vocal) peak = Math.max(peak, Math.abs(v));
if (peak > 0) for (let i = 0; i < vocal.length; i++) vocal[i] *= 0.85 / peak;
writeWavF32(vocalPath, vocal);
console.log(`✓ standalone jee installed at 6.1111s + 16.6667s`);
