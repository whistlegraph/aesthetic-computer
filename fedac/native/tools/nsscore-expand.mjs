#!/usr/bin/env node
// nsscore-expand — the deterministic reference bake of the long form.
// Dwelling (NOTESPATIAL-COMPOSITION.md) turns Special Sign's 1:41 into
// 8–12 minutes by looping each movement N passes; this tool applies a
// pass plan to BOTH the score and the audio so they stay one timeline:
// events re-timed pass by pass, movement doors re-hung, the rotation
// ribbon resampled, and the master wav cut at the doors and re-joined
// with 50ms equal-power seams (the score cursor advances by segment
// minus crossfade, so the strip playhead never drifts from the sound).
//
//   node nsscore-expand.mjs <in.nsscore> <in.wav> [--passes 5,5,7,5,3,1]
//                           [--out-score x] [--out-audio x]

import { readFileSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf("--" + k); return i >= 0 ? args[i + 1] : d; };
const [inScore, inWav] = args;
if (!inWav) {
  console.error("usage: nsscore-expand.mjs <in.nsscore> <in.wav> [--passes 5,5,7,5,3,1] [--out-score x] [--out-audio x]");
  process.exit(1);
}
const PASSES = opt("passes", "5,5,7,5,3,1").split(",").map(Number);
const XF = 0.05; // seam crossfade — audible as breath, not as edit

const S = JSON.parse(readFileSync(inScore, "utf8"));
if (PASSES.length !== S.movements.length) throw new Error(`pass plan wants ${S.movements.length} entries`);

// segment list: source [t0,t1) per pass, and its landing time in the output
const segs = [];
let cursor = 0;
const outMovements = [];
S.movements.forEach((m, mi) => {
  const doorIn = cursor;
  for (let p = 0; p < PASSES[mi]; p++) {
    const segDur = m.t1 - m.t0;
    segs.push({ t0: m.t0, t1: m.t1, out: cursor });
    cursor += segDur - (segs.length > 1 ? XF : 0);
  }
  outMovements.push({ ...m, t0: +doorIn.toFixed(4), t1: +cursor.toFixed(4), passes: PASSES[mi] });
});
const outDur = cursor;

// events: every pass replays its movement's slice at the landing time
const outLanes = S.lanes.map(l => ({ ...l, events: [] }));
for (const seg of segs) {
  for (let i = 0; i < S.lanes.length; i++) {
    for (const e of S.lanes[i].events) {
      if (e.t < seg.t0 || e.t >= seg.t1) continue;
      outLanes[i].events.push({ ...e, t: +(seg.out + (e.t - seg.t0)).toFixed(4) });
    }
  }
}
for (const l of outLanes) l.events.sort((a, b) => a.t - b.t);

// rotation ribbon: resample source env through the segment map
let rotation;
if (S.rotation) {
  const N = 512;
  const srcAt = (τ) => {
    const seg = segs.findLast(s => τ >= s.out) ?? segs[0];
    const st = Math.min(seg.t1 - 0.001, seg.t0 + (τ - seg.out));
    const i = (st / S.dur) * (S.rotation.length - 1);
    return S.rotation[Math.max(0, Math.min(S.rotation.length - 1, Math.round(i)))];
  };
  rotation = Array.from({ length: N }, (_, i) => +srcAt((i / (N - 1)) * outDur).toFixed(4));
}

const outScore = {
  ...S, dur: +outDur.toFixed(3), movements: outMovements, lanes: outLanes,
  ...(rotation ? { rotation } : {}),
  expandedFrom: { dur: S.dur, passes: PASSES },
};
const scoreOut = opt("out-score", inScore.replace(/\.nsscore$/, "") + "-long.nsscore");
writeFileSync(scoreOut, JSON.stringify(outScore) + "\n");

// audio: trim every pass from the master, chain acrossfades
const audioOut = opt("out-audio", inWav.replace(/\.wav$/, "") + "-long.wav");
const parts = segs.map((s, i) => {
  const end = i === segs.length - 1 ? s.t1 + 2 : s.t1 + XF; // tail rings out
  return `[0:a]atrim=start=${s.t0}:end=${end},asetpts=PTS-STARTPTS[s${i}]`;
});
let chain = "", prev = "s0";
for (let i = 1; i < segs.length; i++) {
  const label = i === segs.length - 1 ? "out" : `x${i}`;
  chain += `;[${prev}][s${i}]acrossfade=d=${XF}:c1=tri:c2=tri[${label}]`;
  prev = label;
}
const fc = parts.join(";") + chain;
const r = spawnSync("ffmpeg", ["-y", "-v", "error", "-i", inWav,
  "-filter_complex", fc, "-map", "[out]", audioOut], { stdio: "inherit" });
if (r.status !== 0) throw new Error("ffmpeg splice failed");

const n = outLanes.reduce((a, l) => a + l.events.length, 0);
console.log(`${scoreOut} — ${Math.floor(outDur / 60)}m${Math.round(outDur % 60)}s, ${n} events, passes ${PASSES.join("/")}`);
console.log(audioOut);
