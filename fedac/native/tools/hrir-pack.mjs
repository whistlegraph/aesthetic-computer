#!/usr/bin/env node
// hrir-pack — pack the MIT KEMAR compact HRIR set into one binary the
// spatializer can memory-map without a tar, a decoder, or a dependency.
//
// Source: https://sound.media.mit.edu/resources/KEMAR.html (compact.tar.Z)
// Gardner & Martin, MIT Media Lab, 1994. Free to use with citation.
//
// The compact set is 128-tap, 44.1 kHz, 16-bit big-endian, interleaved
// L/R, one file per (elevation, azimuth) with azimuth sweeping 0…180 to
// the RIGHT of the listener — the left hemisphere is the same data with
// the ears swapped. ITD and head shadow are baked into the measurements,
// which is the whole point: nothing here is modeled.
//
//   node hrir-pack.mjs <kemar/compact dir> <out-dir>

import { readFileSync, writeFileSync, readdirSync, mkdirSync } from "node:fs";
import { join } from "node:path";

const [, , srcDir, outDir] = process.argv;
if (!outDir) { console.error("usage: hrir-pack.mjs <kemar/compact> <out-dir>"); process.exit(1); }
mkdirSync(outDir, { recursive: true });

const TAPS = 128;
const elevs = readdirSync(srcDir).filter(d => d.startsWith("elev"))
  .map(d => ({ dir: d, el: parseInt(d.slice(4), 10) }))
  .sort((a, b) => a.el - b.el);

const index = { sr: 44100, taps: TAPS, source: "MIT KEMAR compact (Gardner & Martin 1994)", elevations: [] };
const chunks = [];
let offset = 0;

for (const { dir, el } of elevs) {
  const files = readdirSync(join(srcDir, dir)).filter(f => f.endsWith(".dat")).map(f => {
    const m = f.match(/H(-?\d+)e(\d+)a\.dat$/);
    return { file: f, az: parseInt(m[2], 10) };
  }).sort((a, b) => a.az - b.az);

  const azimuths = [];
  for (const { file, az } of files) {
    const b = readFileSync(join(srcDir, dir, file));
    const ir = Buffer.alloc(TAPS * 4); // Int16LE L then Int16LE R, deinterleaved
    for (let i = 0; i < TAPS; i++) {
      ir.writeInt16LE(b.readInt16BE(i * 4), i * 2);              // left ear
      ir.writeInt16LE(b.readInt16BE(i * 4 + 2), TAPS * 2 + i * 2); // right ear
    }
    chunks.push(ir);
    azimuths.push({ az, off: offset });
    offset += TAPS * 4;
  }
  index.elevations.push({ el, azimuths });
}

writeFileSync(join(outDir, "kemar-compact.bin"), Buffer.concat(chunks));
writeFileSync(join(outDir, "kemar-compact.json"), JSON.stringify(index));
const n = index.elevations.reduce((a, e) => a + e.azimuths.length, 0);
console.log(`packed ${n} measurements, ${index.elevations.length} elevations, ${(offset / 1024).toFixed(0)} KB`);
