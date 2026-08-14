#!/usr/bin/env node
// qc.mjs — the checks @jeffrey asks a /pop master to survive.
//
//   1. duration + format (ffprobe)
//   2. integrated LUFS + true peak (ffmpeg ebur128 + astats, on decode)
//   3. click scan — decode to f32 and hunt for sample-to-sample steps,
//      both raw and in a lowpassed band where a real discontinuity shows
//      up as an impossible jump for the band's slew rate.
//   4. mono fold-down — the Special Sign space is an ANTISYMMETRIC side
//      return, so it is exactly the thing that could vanish when a phone
//      speaker sums L+R. This measures, per 2 s window, how much level
//      the fold-down costs, and reports the worst window. Anything past a
//      couple of dB means the side send is doing work the dry mix isn't.
//
//   node pop/cult/bin/qc.mjs pop/cult/out/cult-remix.mp3

import { execFileSync } from "node:child_process";
import { readFileSync, unlinkSync } from "node:fs";
import { resolve } from "node:path";
import { tmpdir } from "node:os";

const target = resolve(process.cwd(), process.argv[2] || "pop/cult/out/cult-remix.mp3");
const run = (a) => execFileSync("ffmpeg", a, { encoding: "buffer", stdio: ["ignore", "pipe", "pipe"] });

const probe = execFileSync("ffprobe", ["-v", "error", "-show_entries",
  "format=duration,bit_rate:stream=codec_name,sample_rate,channels",
  "-of", "default=nw=1", target], { encoding: "utf8" });
console.log(`── ${target}`);
console.log(probe.trim().split("\n").map((l) => "   " + l).join("\n"));

// loudness
const log = execFileSync("ffmpeg", ["-hide_banner", "-nostats", "-i", target,
  "-af", "ebur128=peak=true:framelog=quiet,astats=measure_overall=Peak_level:measure_perchannel=none",
  "-f", "null", "-"], { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] } ).toString?.() ?? "";
const err = (() => {
  try {
    execFileSync("ffmpeg", ["-hide_banner", "-nostats", "-i", target, "-af",
      "ebur128=peak=true:framelog=quiet", "-f", "null", "-"], { stdio: ["ignore", "pipe", "pipe"] });
    return "";
  } catch { return ""; }
})();
const eb = execFileSync("bash", ["-c",
  `ffmpeg -hide_banner -nostats -i '${target}' -af ebur128=peak=true:framelog=quiet -f null - 2>&1 | tail -20`],
  { encoding: "utf8" });
console.log("── loudness");
for (const line of eb.split("\n"))
  if (/Integrated|Threshold|Range|True peak|Peak:|LRA|I: /.test(line)) console.log("   " + line.trim());

// click scan
const raw = resolve(tmpdir(), `qc-${process.pid}.raw`);
execFileSync("ffmpeg", ["-y", "-v", "error", "-i", target, "-f", "f32le", "-ac", "2", "-ar", "48000", raw]);
const buf = readFileSync(raw);
unlinkSync(raw);
const n = buf.length / 8;
const SR = 48000;
console.log(`── click scan · ${n} frames`);

for (const ch of [0, 1]) {
  let maxStep = 0, maxAt = 0, over = 0;
  // one-pole lowpass @ 2 kHz: in this band a legitimate signal cannot
  // move more than ~0.26 per sample at full scale, so anything near that
  // is a discontinuity, not music.
  const k = 1 - Math.exp((-2 * Math.PI * 2000) / SR);
  let lp = 0, prevLp = 0, maxLpStep = 0, maxLpAt = 0;
  let prev = buf.readFloatLE(ch * 4);
  for (let i = 1; i < n; i++) {
    const v = buf.readFloatLE(i * 8 + ch * 4);
    const d = Math.abs(v - prev);
    if (d > maxStep) { maxStep = d; maxAt = i; }
    if (d > 0.35) over++;
    lp += k * (v - lp);
    const dl = Math.abs(lp - prevLp);
    if (dl > maxLpStep) { maxLpStep = dl; maxLpAt = i; }
    prevLp = lp; prev = v;
  }
  console.log(`   ch${ch}  max raw step ${maxStep.toFixed(4)} @ ${(maxAt / SR).toFixed(3)}s` +
    `   ·  steps>0.35: ${over}` +
    `   ·  max 2k-band step ${maxLpStep.toFixed(4)} @ ${(maxLpAt / SR).toFixed(3)}s`);
}

// hard-clip run detection: consecutive samples at/over ±0.999
let runs = 0, longest = 0, cur = 0;
for (let i = 0; i < n; i++) {
  const l = Math.abs(buf.readFloatLE(i * 8)), r = Math.abs(buf.readFloatLE(i * 8 + 4));
  if (Math.max(l, r) >= 0.999) { cur++; longest = Math.max(longest, cur); }
  else { if (cur >= 3) runs++; cur = 0; }
}
console.log(`── clipping · runs of ≥3 samples at full scale: ${runs} (longest ${longest})`);

// mono fold-down: compare the stereo RMS against the summed-mono RMS in
// 2 s windows. Reported as the level the mix LOSES when folded.
{
  const win = SR * 2;
  let worst = 0, worstAt = 0, total = 0, mtotal = 0;
  for (let w = 0; w * win < n; w++) {
    let st = 0, mo = 0, cnt = 0;
    for (let i = w * win; i < Math.min(n, (w + 1) * win); i++, cnt++) {
      const l = buf.readFloatLE(i * 8), r = buf.readFloatLE(i * 8 + 4);
      st += (l * l + r * r) / 2;
      const m = 0.5 * (l + r);
      mo += m * m;
    }
    if (cnt < win / 4) continue;
    total += st; mtotal += mo;
    if (st < 1e-9) continue;
    const loss = 10 * Math.log10(st / Math.max(mo, 1e-12));
    if (loss > worst) { worst = loss; worstAt = (w * win) / SR; }
  }
  const overall = 10 * Math.log10(total / Math.max(mtotal, 1e-12));
  console.log(`── mono fold-down · worst window −${worst.toFixed(2)} dB @ ${worstAt.toFixed(1)}s` +
    `   ·  whole-track −${overall.toFixed(2)} dB`);
}
