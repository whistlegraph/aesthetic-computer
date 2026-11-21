// frothy-master.mjs — the frothyscreama master, one chain for both engines
// (bin/render-frothyscreama.mjs --js and c/run-c.mjs).
//
// The wattajetta cutie chain (pop/wattajetta/bin/cutie-master.mjs) re-cut for a
// screamo record, by the house gate (pop/MASTERING.md): −10 ±1 LUFS, ≤ −2 dBTP.
// The wall and the screams already fill 500 Hz–4 kHz, so nothing is lifted
// there; the tape rolls the top off, so the cymbals get a little air back.
//
//   lows   LR4 split at 120 Hz, the low band folded to mono and leaned on
//          through tanh at unity small-signal gain: the kick's peak comes down
//          and the bass under it does not move. The clip is NOT oversampled:
//          asoftclip's oversampler delays the band against the highs, and the
//          two then cancel around the split — it cost 3.4 dB at 60–120 Hz and
//          4 dB under 60 (measured 2026-11-20; the record read as thin)
//   teeth  that band's harmonics, 180–1100 Hz, mixed back under, quietly —
//          the bass is already gritty
//   tone   a little air over 9 kHz. (A 1.5 dB dip at 300 Hz and a harder lean
//          on the lows were tried first and read as thin, 2026-11-20.)
//   glue   4:1 compressor over −12 dB, 0.7 ms attack: it takes the snare,
//          crash and whisper heads so the limiter rests (idle 70 % of the
//          record, 0.7 dB at the 90th centile, 2.0 at the 99th, 3.1 at worst —
//          measured 2026-11-20), and it leaves the quiet rooms quiet (LRA 6.7).
//          A 3 ms attack let 6 dB through to the limiter.
//   level  one static gain into a 4× oversampled limiter at −2.3 dBFS. The
//          gain is measured, not ridden: the chain is run once without it,
//          and the gain is what carries that reading to the target.

import { spawnSync } from "node:child_process";

export const MASTER = {
  lowSplit: 120,
  lowDrive: 4,        // dB into the tanh and back out again
  teeth: 0.5,         // linear mix of the low band's harmonics
  mud: 0,             // dB at 300 Hz
  air: 2.5,           // dB shelf over 9 kHz
  glue: "threshold=-12dB:ratio=4:attack=0.7:release=70:makeup=1:knee=4",
  target: -10.4,      // LUFS integrated, after the limiter: with the ceiling below, PLR stays over 8
  trim: 0.15,         // dB over the measured gain: what the limiter takes back off the reading
  ceiling: -2.3,      // dBFS at the limiter; lands under −2 dBTP after decode
};

// ffmpeg -filter_complex for input 0 → [out]. fade is [start, seconds] or null.
// `upTo` stops the chain early for measurement: "gain" ends before the limiter.
export function masterGraph(sr, fade, M = MASTER, upTo = null) {
  const mono = "pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1";
  return [
    `[0:a]highpass=f=30,acrossover=split=${M.lowSplit}:order=4th[lo][hi]`,
    `[lo]${mono},volume=${M.lowDrive}dB,asoftclip=type=tanh,volume=${-M.lowDrive}dB,asplit=2[low][drv]`,
    `[drv]highpass=f=180,highpass=f=180,lowpass=f=1100,volume=${M.teeth}[teeth]`,
    `[low][hi][teeth]amix=inputs=3:normalize=0,` + [
      `equalizer=f=300:t=o:w=1.4:g=${M.mud}`,
      `highshelf=f=9000:g=${M.air}`,
      `acompressor=${M.glue}`,
      `volume=${M.gain}dB`,
      ...(upTo === "gain" ? [] : [
        `aresample=${sr * 4}`,
        `alimiter=limit=${M.ceiling}dB:attack=5:release=90:asc=1:level=disabled`,
        `aresample=${sr}`,
        ...(fade ? [`afade=t=out:st=${fade[0].toFixed(2)}:d=${fade[1]}`] : []),
      ]),
    ].join(",") + "[out]",
  ].join(";");
}

// Measure the chain without its gain and return the master's settings with
// the static gain filled in. `input` is the ffmpeg input arguments.
export function measured(ffmpeg, input, sr, M = MASTER) {
  const graph = masterGraph(sr, null, { ...M, gain: 0 }, "gain").replace("[out]", ",ebur128=framelog=quiet[out]");
  const r = spawnSync(ffmpeg, ["-hide_banner", "-nostats", ...input, "-filter_complex", graph, "-map", "[out]", "-f", "null", "-"], { encoding: "utf8", maxBuffer: 1 << 26 });
  const lufs = Number(r.stderr.slice(r.stderr.lastIndexOf("Integrated loudness")).match(/I:\s+(-?[\d.]+) LUFS/)?.[1]);
  if (!Number.isFinite(lufs)) throw new Error("the master could not measure the premaster");
  return { ...M, lufs, gain: Number((M.target - lufs + M.trim).toFixed(2)) };
}
