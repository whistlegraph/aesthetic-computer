#!/usr/bin/env node
// monosine-gen — a 60-second composition for a single sine emitter, built
// to test the spatial system: the emitter ORBITS the ring of machines
// (the rotation ribbon is its speed; audio pan and visuals integrate the
// same curve) while one just-intonation line over A=220 walks, circles,
// climbs, settles, vanishes. The virtual field sits 1:1 on the room —
// the tone passes seat to seat, which is the whole point.
//
//   node monosine-gen.mjs [out.nsscore]

import { writeFileSync } from "node:fs";

const A = 220;
const R = { u: 1, M2: 9 / 8, M3: 5 / 4, P4: 4 / 3, P5: 3 / 2, M6: 5 / 3, M7: 15 / 8 };
const hz = (r, oct = 0) => +(A * r * 2 ** oct).toFixed(3);

const events = [];
let t = 0;
const tone = (r, oct, dur, g, gap = 0.2) => {
  events.push({ t: +t.toFixed(3), dur: +dur.toFixed(3), hz: hz(r, oct), wave: "sine", g: +g.toFixed(3) });
  t += dur + gap;
};
const door = (name, sub) => ({ name, sub, t0: +t.toFixed(3) });
const movements = [];
let m;

m = door("I · Appear", "the tone breathes in");
tone(R.u, 0, 4.5, 0.24, -2);
tone(R.u, 0, 5, 0.36, 0.5);
movements.push({ ...m, t1: +t.toFixed(3), level: 0.3 });

m = door("II · Walk", "first lap, stepwise");
for (const r of [R.u, R.M2, R.M3, R.P4, R.M3, R.M2])
  tone(r, 0, 1.7, 0.42, 0.25);
movements.push({ ...m, t1: +t.toFixed(3), level: 0.5 });

m = door("III · Circle", "full spin — the pan test");
for (let pass = 0; pass < 2; pass++)
  for (const [r, o] of [[R.u, 0], [R.M3, 0], [R.P5, 0], [R.u, 1], [R.P5, 0], [R.M3, 0]])
    tone(r, o, 0.85, 0.5 + pass * 0.06, 0.12);
movements.push({ ...m, t1: +t.toFixed(3), level: 1 });

m = door("IV · Climb", "two octaves of light");
for (const [r, o] of [[R.u, 0], [R.P5, 0], [R.u, 1], [R.M3, 1], [R.P5, 1], [R.u, 2]])
  tone(r, o, 1.3, 0.44 + 0.03 * (o + 1), 0.2);
movements.push({ ...m, t1: +t.toFixed(3), level: 0.8 });

m = door("V · Settle", "the way home");
for (const [r, o] of [[R.P5, 1], [R.u, 1], [R.M6, 0], [R.P5, 0], [R.M3, 0]])
  tone(r, o, 1.8, 0.38, 0.35);
movements.push({ ...m, t1: +t.toFixed(3), level: 0.55 });

m = door("VI · Vanish", "one tone, thinning");
tone(R.u, 0, 6, 0.2, 0);
movements.push({ ...m, t1: +t.toFixed(3), level: 0.2 });

const dur = +t.toFixed(3);
// rotation = ORBIT SPEED (laps/sec-ish, integrated identically by the
// audio baker's pan and the diagram's spin): still → walk → whirl →
// steady → slowing → still.
const N = 256;
const speedAt = (τ) => {
  const inM = (k) => τ >= movements[k].t0 && τ < movements[k].t1;
  if (inM(0)) return 0;
  if (inM(1)) return 0.35;
  if (inM(2)) { const u = (τ - movements[2].t0) / (movements[2].t1 - movements[2].t0); return 0.6 + 0.5 * Math.sin(Math.PI * u); }
  if (inM(3)) return 0.5;
  if (inM(4)) { const u = (τ - movements[4].t0) / (movements[4].t1 - movements[4].t0); return 0.4 * (1 - u); }
  return 0;
};
const rotation = Array.from({ length: N }, (_, i) => +speedAt((i / (N - 1)) * dur).toFixed(4));

const score = {
  name: "monosine",
  dur,
  movements,
  rotation,
  lanes: [{ name: "sine", color: [179, 64, 46], events }], // graphic-score red
};

const dest = process.argv[2] || "monosine.nsscore";
writeFileSync(dest, JSON.stringify(score) + "\n");
console.log(`${dest} — ${events.length} tones, ${dur}s, 6 movements`);
