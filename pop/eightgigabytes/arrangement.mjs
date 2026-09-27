// Shared instrumental score for the studio renderer and the live trio.
import { BPM, SECTIONS, AT, TOTAL_BEATS, CHORDS, ROOT, LINES } from "./score.mjs";
export const beat = 60 / BPM;
export const kind = (b) => SECTIONS.find(s => b >= s.start && b < s.start + s.bars * 4)?.id ?? "outro";
export const MIX_DB = { drums: -10.5, bass: -17.5, pad: -9, keys: -9.5, whistle: -11.5 };
export const OFFSET = (AT.r1 - 1) * beat - 0.35;
export const DURATION = TOTAL_BEATS * beat + 2.5 - OFFSET;
export function instrumentEvents() {
 const events = [], secs = b => b * beat, T = b => b * beat;
 const drums = "drums", bass = "bass", pad = "pad", keys = "keys", whistle = "whistle";
 const place = (bus, sound, t, gain = 1, pan = 0) => events.push({ bus, ...sound, t, gain, pan });
 const kick = (velocity = 1) => ({ type: "kick", velocity, duration: .42 });
 const tap = (velocity = 1) => ({ type: "tap", velocity, duration: .16 });
 const tick = (velocity = 1, frequency = 3150) => ({ type: "tick", velocity, frequency, duration: .03 });
 const bassTone = (midi, duration) => ({ type: "bass", midi, duration });
 const padTone = (midi, duration) => ({ type: "pad", midi, duration });
 const whistleTone = (midi, duration) => ({ type: "whistle", midi, duration });
 const fmKey = (midi, duration, velocity = 1) => ({ type: "keys", midi, duration, velocity });
 const toks = n => n.split(/\s+/).map(t => { const [k,d] = t.split(":"); return {k,d:Number(d)}; });
const SW = 0.06;                                                 // swing on the off eighths
const swung = (b) => (Math.abs((b % 1) - 0.5) < 1e-6 ? b + SW : b);
const chordAt = (b) => { const s = SECTIONS.find((s) => b >= s.start && b < s.start + s.bars * 4); return s.chords[Math.floor((b - s.start) / 4) % s.chords.length]; };
const isRefrain = (id) => id.startsWith("r");

for (let bar = 0; bar < TOTAL_BEATS / 4; bar++) {
  const b0 = bar * 4, id = kind(b0), ch = chordAt(b0), tri = CHORDS[ch], root = ROOT[ch];
  const inRef = isRefrain(id), bridge = id === "bridge", intro = id === "intro", outro = id === "outro";
  const opening = id === "r1" && bar - SECTIONS[1].start / 4 < 2;   // the record opens on a bare refrain
  if (intro) continue;                 // no intro at all (jeffrey): the record opens on the voice
  // clock: quarters always, eighths in the refrains — panned back and forth
  for (let e = 0; e < 8; e++) {
    const b = b0 + e / 2, off = e % 2 === 1;
    if (off && !inRef) continue;
    place(drums, tick(off ? 0.35 : intro || bridge ? 0.7 : 0.55), T(swung(b)), 0.32, (e % 4 < 2 ? -1 : 1) * 0.25);
  }
  if (!bridge && !intro && !opening && !(outro && bar === 1)) {
    place(drums, kick(1), T(b0), 0.95);
    place(drums, kick(0.9), T(b0 + 2.5), 0.85);
    if (inRef) place(drums, kick(0.75), T(b0 + 1.5), 0.7);
    place(drums, tap(1), T(b0 + 1), 0.72, 0.1);
    place(drums, tap(0.95), T(b0 + 3), 0.72, 0.1);
    place(drums, tap(0.35), T(swung(b0 + 1.5) + 0.25 * beat), 0.5, -0.15);          // ghost on the "a" of 2
    place(drums, tap(0.28), T(b0 + 3.75), 0.5, 0.2);                                  // ghost before the bar
    if (inRef) place(drums, tap(0.4), T(b0 + 2.5), 0.5, -0.1);
  }
  if (outro && bar === 1) { place(drums, kick(1), T(b0), 0.95); place(drums, tap(1), T(b0 + 1), 0.72); }
  // bass: root on 1, fifth on the "and of 2", root on 3.5 — long notes in the bridge
  if (!intro && !opening) {
    if (bridge) place(bass, bassTone(root, secs(3.9)), T(b0), 0.5);
    else {
      place(bass, bassTone(root, secs(1.35)), T(b0), 0.55);
      place(bass, bassTone(root + 7, secs(0.4)), T(swung(b0 + 1.5)), 0.4);
      place(bass, bassTone(root, secs(0.9)), T(b0 + 2.5), 0.5);
      place(bass, bassTone(root + (inRef ? 12 : 0), secs(0.4)), T(b0 + 3.5), 0.35);
    }
  }
  // pad: the triad, whole bar, quiet; up an octave in the bridge so it shines
  for (const m of tri) place(pad, padTone(m + (bridge ? 12 : 0), secs(4)), T(b0) - 0.05, bridge ? 0.11 : 0.075, (m % 3 - 1) * 0.4);
  // keys: comping, off the beat
  if (!bridge && !outro) {
    const hits = inRef ? [0, 1.5, 2.5, 3.5] : [0.5, 2.5, 3.5];
    for (const h of hits) for (const [k, m] of tri.entries()) place(keys, fmKey(m + 12, secs(0.6), h === 0 ? 0.9 : 0.7), T(swung(b0 + h)) + k * 0.012, 0.16, (k - 1) * 0.5);
  }
  if (outro) for (const m of tri) place(keys, fmKey(m + 12, secs(3), 0.9), T(b0) + (m % 3) * 0.02, 0.2, (m % 3 - 1) * 0.4);
}
// the whistle: the family voice. It doubles the refrain hook an octave up
// (the ballad's own doubleTranspose idea), and answers in the verse holds.
const neoRef = LINES.filter((l) => l.m === "neo" && /^I have eight|^I get warm and|^this is the one/.test(l.w));
for (const l of neoRef) { let b = l.at; for (const t of toks(l.n)) { if (t.k !== "r") place(whistle, whistleTone(Number(t.k) + 12, secs(t.d) * 0.95), T(b), 0.11, 0.2); b += t.d; } }
const answers = [
  [AT.r1 + 5.5, [[68, 0.5], [65, 0.5], [63, 1]]], [AT.r1 + 14.5, [[65, 0.25], [63, 0.25], [60, 1]]], [AT.r1 + 22, [[63, 0.5], [60, 0.5], [58, 1]]],   // refrain 1 has no frisbee yet: the whistle answers
  [AT.outro + 4, [[63, 0.5], [60, 0.5], [58, 0.5], [56, 2]]],
];
for (const [at, mot] of answers) { let b = at; for (const [m, d] of mot) { place(whistle, whistleTone(m, secs(d) * 0.9), T(b), 0.17, -0.3); b += d; } }

return events;
}
