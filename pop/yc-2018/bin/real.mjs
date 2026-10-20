// real.mjs — real-instrument layers for the deep house YC bed (FluidSynth + GeneralUser GS).
//   node real.mjs   (reads grid2.json, deep/song.json; writes deep/{ep,bass,gtr,str}.wav)
// Kept low and warm: a tine Rhodes, a fingered electric bass doubling the sub,
// muted nylon-guitar plucks, and slow strings in the later grooves. Nothing
// voiced above D5, so the top stays the vocal's and the shaker's.
import { readFileSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const SR = 48000, PPQ = 480;
const { bpm } = JSON.parse(readFileSync("grid2.json", "utf8"));
const { bars: END } = JSON.parse(readFileSync("deep/song.json", "utf8"));
const SF2 = `${homedir()}/.cache/ac/soundfonts/GeneralUserGS.sf2`;
const sec = (b) => b < 8 ? "in" : b < 16 ? "A" : b < 20 ? "air" : b < 34 ? "B" : b < 38 ? "air2" : b < END ? "C" : "end";
const CH = [ // same harmony as deep.mjs: Bm9 · Gmaj9 · Dmaj9 · A9sus
  { root: 35, ep: [57, 61, 62, 66], gtr: [47, 54, 57, 62], str: [47, 54, 62, 66] },
  { root: 31, ep: [54, 57, 59, 62], gtr: [43, 50, 54, 59], str: [43, 50, 59, 62] },
  { root: 38, ep: [57, 61, 64, 66], gtr: [50, 57, 61, 66], str: [50, 57, 61, 66] },
  { root: 33, ep: [55, 59, 62, 64], gtr: [45, 52, 55, 62], str: [45, 52, 59, 64] },
];
const chord = (b) => CH[b % 4];
const vlq = (n) => { const o = [n & 0x7f]; while ((n >>= 7)) o.unshift((n & 0x7f) | 0x80); return o; };
const tk = (bar, beat = 0) => Math.round((bar * 4 + beat) * PPQ);
function smf(prog, ch, notes) {
  const us = Math.round(60e6 / bpm);
  const ev = [{ t: 0, o: 0, d: [0xff, 0x51, 3, (us >> 16) & 255, (us >> 8) & 255, us & 255] },
    { t: 0, o: 0, d: [0xc0 | ch, prog] }, { t: 0, o: 0, d: [0xb0 | ch, 7, 112] }, { t: 0, o: 0, d: [0xb0 | ch, 91, 18] }];
  for (const [t0, len, n, v] of notes) { ev.push({ t: t0, o: 1, d: [0x90 | ch, n, v] }, { t: t0 + len, o: 0, d: [0x80 | ch, n, 0] }); }
  ev.sort((a, b) => a.t - b.t || a.o - b.o);
  const out = []; let last = 0; for (const e of ev) { out.push(...vlq(e.t - last), ...e.d); last = e.t; }
  out.push(0, 0xff, 0x2f, 0);
  return Buffer.from([0x4d, 0x54, 0x68, 0x64, 0, 0, 0, 6, 0, 0, 0, 1, PPQ >> 8, PPQ & 255,
    0x4d, 0x54, 0x72, 0x6b, ...[24, 16, 8, 0].map((s) => (out.length >> s) & 255), ...out]);
}
const P = { ep: [], bass: [], gtr: [], str: [] };
const hum = (b, q) => Math.round(Math.sin(b * 7.1 + q * 3.3) * 10); // a few ticks of human
for (let b = 0; b < END; b++) {
  const s = sec(b), c = chord(b), groove = s === "A" || s === "B" || s === "C";
  // Rhodes: held voicing in the intro/breaks, syncopated comps in the grooves
  if (!groove) for (const n of c.ep) P.ep.push([tk(b), tk(0, 3.8), n, s === "in" ? 58 : 64]);
  else for (const q of s === "C" ? [0, 0.75, 1.5, 2.5, 3.25] : [0, 1.5, 2.5])
    for (const n of c.ep) P.ep.push([tk(b, q) + hum(b, q), tk(0, 0.55), n, q === 0 ? 74 : 62]);
  // bass: fingered electric, doubling the sub's off-beat walk an octave up
  if (groove || (s === "in" && b >= 4)) for (let q = 0; q < 4; q++) P.bass.push([tk(b, q + 0.5) + hum(b, q), tk(0, 0.4), c.root + 12 + (q === 3 && b % 2 ? 7 : 0), 92]);
  else P.bass.push([tk(b), tk(0, 3.6), c.root + 12, 70]);
  // nylon guitar: muted 16th plucks on the upbeats, arpeggiated
  if (groove || (s === "in" && b >= 6)) for (let i = 0; i < 8; i++) { const q = i * 0.5 + 0.25;
    P.gtr.push([tk(b, q) + hum(b, i), tk(0, 0.2), c.gtr[i % 4], i % 2 ? 54 : 66]); }
  // strings: slow, low, from the second groove on
  if (groove || s === "air2") for (const n of c.str) P.str.push([tk(b), tk(0, 4), n, s === "air2" ? 70 : 60]);
}
for (const n of [50, 57, 61, 64, 66]) P.ep.push([tk(END), tk(0, 6), n, 80]);
P.bass.push([tk(END), tk(0, 6), 38 + 12, 90]); for (const n of CH[2].str) P.str.push([tk(END), tk(0, 8), n, 70]);
// Held notes get a harp arpeggio over the chord, a new pattern each hold:
// up, down, up-down, then broken thirds, rising from 8ths to 16ths as it goes.
const { words: GW, e8: E8 } = JSON.parse(readFileSync("grid2.json", "utf8"));
P.arp = [];
const PATS = [[0, 1, 2, 3, 4], [4, 3, 2, 1, 0], [0, 2, 4, 2, 1, 3], [0, 2, 1, 3, 2, 4]];
let held = 0;
for (const w of GW) { if (w.slots < 4) continue;
  const a8 = w.slot + 1, b8 = w.slot + w.slots, pat = PATS[held % PATS.length], step = held % 3 === 2 ? 0.25 : 0.5; held++;
  for (let k = 0, q8 = a8; q8 < b8; k++, q8 += step * 2) {
    const bar = Math.floor(q8 / 8), c = chord(bar), tones = [...c.ep, c.ep[0] + 12], n = tones[pat[k % pat.length] % tones.length];
    P.arp.push([Math.round(q8 * PPQ / 2), Math.round(step * PPQ * 1.6), n, 58 + 16 * Math.min(1, (q8 - a8) / Math.max(1, b8 - a8))]); } }
console.log(`· ${held} held notes get arpeggios`);
const PROG = { ep: 4, bass: 33, gtr: 24, str: 49, arp: 46 };
for (const [k, notes] of Object.entries(P)) {
  writeFileSync(`deep/${k}.mid`, smf(PROG[k], 0, notes));
  const r = spawnSync("fluidsynth", ["-ni", "-o", "synth.reverb.room-size=0.25", "-o", "synth.reverb.level=0.5", "-g", "0.7", "-r", String(SR), "-F", `deep/${k}.wav`, SF2, `deep/${k}.mid`], { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`${k}: ${r.stderr}`);
  console.log(`· ${k} (${notes.length} notes, GM ${PROG[k]})`);
}
