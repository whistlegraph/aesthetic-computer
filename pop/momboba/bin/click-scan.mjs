#!/usr/bin/env node
// click-scan.mjs — objective pop/click detector for rendered masters.
// Decodes any audio file to mono f32 (ffmpeg), then flags samples where
// the instantaneous jump |x[i]-x[i-1]| is both absolutely large and far
// out of proportion to the local RMS — i.e. a discontinuity, not music.
//
// Run:  node pop/momboba/bin/click-scan.mjs <audio> [more...]

import { execFileSync } from "node:child_process";

const SRX = 44100;
// scan LEFT and RIGHT separately — a one-channel click can cancel in a
// mono downmix and hide from the detector
const CHANS = [["L", "pan=mono|c0=c0"], ["R", "pan=mono|c0=c1"]];
for (const f of process.argv.slice(2)) for (const [chName, chPan] of CHANS) {
  const raw = execFileSync("ffmpeg", ["-v", "quiet", "-i", f,
    "-af", chPan, "-f", "f32le", "-ac", "1", "-ar", String(SRX), "-"],
    { maxBuffer: 1 << 30 });
  const x = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2);
  const win = 2048;
  let rmsAcc = 0;
  const hits = [];
  for (let i = 1; i < x.length; i++) {
    rmsAcc += (x[i] * x[i] - rmsAcc) / win;       // running mean-square
    const rms = Math.sqrt(rmsAcc) + 1e-6;
    const d = Math.abs(x[i] - x[i - 1]);
    if (d > 0.06 && d > 9 * rms) {
      const t = i / SRX;
      if (!hits.length || t - hits[hits.length - 1].t > 0.05)
        hits.push({ t, d: +d.toFixed(3), rms: +rms.toFixed(4) });
    }
  }
  console.log(`${f} [${chName}]`);
  if (!hits.length) console.log("  ✓ clean — no discontinuities found");
  else {
    console.log(`  ✗ ${hits.length} suspect jump(s):`);
    for (const h of hits.slice(0, 20))
      console.log(`    ${Math.floor(h.t / 60)}:${(h.t % 60).toFixed(2).padStart(5, "0")} · Δ${h.d} vs rms ${h.rms}`);
  }
}
