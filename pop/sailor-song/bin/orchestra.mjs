#!/usr/bin/env node
// orchestra.mjs — the orchestral pop layer, played from HER chart (v22: "lets apply real
// orchestral pop to this now"). Every part reads c/sailor-chart.h — her bars, her beats,
// her chords, her tuned melody — and writes one MIDI per instrument on a linear clock
// (120 BPM × 480 ticks = 960 ticks a second, so a tick is a time, not a beat), which
// FluidSynth + GeneralUser GS renders to src/orch/<part>.wav, 48 k stereo, on the same
// timeline as the cut stems. The engine (c/sailorremix.c) mixes them per section.
//
//   node pop/sailor-song/bin/orchestra.mjs            # → src/orch/*.wav + orch.events.json
//
// Parts (GM program): strings 48 · cello 42 · pizz 45 · horns 60 · timpani 47 · harp 46 ·
// glock 9 · aahs 52. Where each one plays is decided HERE (a part is silent in a section it
// does not belong to); how loud, in the engine's ORCH table.
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url)), LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "src/orch"); mkdirSync(OUT, { recursive: true });
const SF2 = `${process.env.HOME}/.cache/ac/soundfonts/GeneralUserGS.sf2`;
if (!existsSync(SF2)) { console.error(`✗ soundfont missing: ${SF2}`); process.exit(1); }

// ── the chart ──
const H = readFileSync(resolve(LANE, "c/sailor-chart.h"), "utf8");
const TUNE = Number(/#define CHART_TUNE ([\d.]+)/.exec(H)[1]);   // semitones over A440
const bars = [...H.matchAll(/\{ (\d+), ([\d.]+), ([\d.]+), (\d), (\d), \{ ([^}]+) \} \}/g)]
  .map((m) => ({ n: +m[1], t: +m[2], dur: +m[3], chord: +m[4], nb: +m[5], beats: m[6].split(",").map(Number) }));
const notes = [...H.matchAll(/\{ ([\d.]+), ([\d.]+), (\d+) \}/g)].map((m) => ({ t: +m[1], dur: +m[2], midi: +m[3] }));
// v52: THE HESITATION (see c/sailorremix.c) — her voice from 60.08 to 64.29 plays 0.58 s earlier; her notes follow
const VOX_CUT = { a: 60.08, until: 64.29, d: 0 }; for (const v of notes) if (v.t >= VOX_CUT.a && v.t < VOX_CUT.until) v.t -= VOX_CUT.d;   // v53: off — the chart itself moved
if (bars.length < 80 || notes.length < 100) { console.error(`✗ chart parse: ${bars.length} bars, ${notes.length} notes`); process.exit(1); }

// the form (c/sailorremix.c SEC_FROM) and the harmony: Emaj7 bars sound G#m over her open G#
const SEC_FROM = { intro: 1, verse1: 11, chorus1: 28, verse2: 44, chorus2: 52, break: 68, bridge: 73, outro: 81 };
const section = (n) => { let s = "intro"; for (const [k, v] of Object.entries(SEC_FROM)) if (n >= v) s = k; return s; };
const ROOT = [44, 44, 47];                                  // G#2 G#2 B2
const TRIAD = [[8, 11, 3], [8, 11, 3], [11, 3, 6]];         // pitch classes
const PHRASE = (n) => (n - SEC_FROM[section(n)]) % 4;        // 0..3 inside a four-bar phrase
// chord tones in a register: every pc of the triad from lo to hi
const tones = (chord, lo, hi) => { const r = []; for (let m = lo; m <= hi; m++) if (TRIAD[chord].includes(m % 12)) r.push(m); return r; };
const voicing = (chord, lo, hi, count) => tones(chord, lo, hi).slice(0, count);
const ease = (x) => (x <= 0 ? 0 : x >= 1 ? 1 : 0.5 - 0.5 * Math.cos(Math.PI * x));
const grow = (n, a, b) => ease((n - a) / (b - a));           // 0 at bar a, 1 at bar b

// ── MIDI on a linear clock ──
const TPS = 960;                                            // ticks per second (120 BPM × 480)
const tick = (s) => Math.round(s * TPS);
const vlq = (n) => { const out = [n & 0x7f]; while ((n >>= 7)) out.unshift((n & 0x7f) | 0x80); return out; };
const u32 = (n) => [(n >> 24) & 255, (n >> 16) & 255, (n >> 8) & 255, n & 255];
const EAGER = 0.009;   // v101: the kit plays 9 ms ahead of the grid (c/sailorremix.c EAGER); the drums here must too
class Part {
  constructor(name, program) { this.name = name; this.program = program; this.ev = []; this.count = 0; this.receipt = []; this.eager = name === "timpani" || name === "taiko" || name === "pizz"; }
  note(t, midi, dur, vel) { if (this.eager) t -= EAGER;
    if (dur <= 0.02 || midi < 24 || midi > 108) return;
    const v = Math.max(1, Math.min(127, Math.round(vel)));
    this.ev.push({ t: tick(t), b: [0x90, midi, v] }, { t: tick(t + dur), b: [0x80, midi, 0] });
    this.count++; this.receipt.push({ t: +t.toFixed(3), dur: +dur.toFixed(3), midi, gain: +(v / 127).toFixed(2) });
  }
  write() {
    if (!this.count) return null;
    // note-offs before note-ons at the same tick, so a repeated pitch re-attacks
    this.ev.sort((x, y) => x.t - y.t || (x.b[0] === 0x80 ? -1 : 1));
    const bend = 8192 + Math.round((TUNE / 2) * 8192);     // her guitar's +13¢, bend range 2 st
    const trk = [0, 0xff, 0x51, 3, 0x07, 0xa1, 0x20,       // 120 BPM
                 0, 0xc0, this.program,
                 0, 0xe0, bend & 0x7f, (bend >> 7) & 0x7f,
                 0, 0xb0, 0x5b, 0,                          // no reverb — the engine's room
                 0, 0xb0, 0x5d, 0];                         // no chorus
    let last = 0;
    for (const e of this.ev) { trk.push(...vlq(e.t - last), ...e.b); last = e.t; }
    trk.push(...vlq(tick(1.5)), 0xff, 0x2f, 0);             // room for the last release
    const mid = Buffer.from([0x4d, 0x54, 0x68, 0x64, ...u32(6), 0, 0, 0, 1, 480 >> 8, 480 & 255,
      0x4d, 0x54, 0x72, 0x6b, ...u32(trk.length), ...trk]);
    const midPath = resolve(OUT, `${this.name}.mid`), wav = resolve(OUT, `${this.name}.wav`);
    writeFileSync(midPath, mid);
    const r = spawnSync("fluidsynth", ["-ni", "-g", "0.5", "-R", "0", "-C", "0", "-F", wav, "-r", "48000", SF2, midPath], { stdio: ["ignore", "ignore", "inherit"] });
    if (r.status !== 0) { console.error(`✗ fluidsynth failed on ${this.name}`); process.exit(1); }
    return wav;
  }
}
const P = {
  strings: new Part("strings", 48), cello: new Part("cello", 42), pizz: new Part("pizz", 45), horns: new Part("horns", 60),
  timpani: new Part("timpani", 47), harp: new Part("harp", 46), glock: new Part("glock", 9), aahs: new Part("aahs", 52),
  // v24: "string quartet vibes or big drums" — a quartet with lines of its own, and a taiko
  vln1: new Part("vln1", 40), vln2: new Part("vln2", 40), viola: new Part("viola", 41), qcello: new Part("qcello", 42), taiko: new Part("taiko", 116),
};
const barN = (n) => bars.find((b) => b.n === n);

// ── what she sings, bar by bar (the parts stay out of her way) ──
const sungIn = new Map();                                   // bar n → the pitch classes she sings in it
for (const v of notes) { const b = bars.find((x) => v.t >= x.t && v.t < x.t + x.dur); if (!b) continue; if (!sungIn.has(b.n)) sungIn.set(b.n, new Set()); sungIn.get(b.n).add(v.midi % 12); }
const sings = (n) => sungIn.has(n);
const herE = (n) => !!sungIn.get(n)?.has(4);               // E: the 4th over B, the b6 over G#m — her one non-triad note, 39 times
// v27: THE SUSPENSION — where she sings E, a part's D# (pc 3) holds E for the first half and resolves to D#: the 4–3 she is singing
const susNote = (part, t, m, dur, vel, n) => { if (herE(n) && m % 12 === 3) { part.note(t, m + 1, dur * 0.55, vel); part.note(t + dur * 0.55, m, dur * 0.47, vel * 0.9); } else part.note(t, m, dur, vel); };
const chordRun = (n) => { const i = bars.findIndex((b) => b.n === n); let a = i; while (a > 0 && bars[a - 1].chord === bars[i].chord) a--; let z = i; while (z + 1 < bars.length && bars[z + 1].chord === bars[i].chord) z++; return { from: a, to: z, pos: i - a }; };
// v27: HER KISS FIGURE — the first seven sung notes of bar 28 ("kiss me on the mouth and love me like a sailor"), as fractions of the bar
const b28 = barN(28), KISS = notes.filter((v) => v.t >= b28.t - 0.7 && v.t < b28.t + 2 * b28.dur).slice(0, 7).map((v) => ({ off: (v.t - b28.t) / b28.dur, dur: v.dur / b28.dur, midi: v.midi }));
const kissAt = (part, bar, lift, vel) => { for (const k of KISS) part.note(bar.t + k.off * bar.dur, k.midi + lift, Math.max(0.15, k.dur * bar.dur), vel); };

// ── the parts, bar by bar ──
for (const b of bars) {
  const s = section(b.n), c = b.chord, root = ROOT[c], bt = b.beats, nb = b.nb, beat = b.dur / nb, n = b.n;
  const mid = (j) => bt[j] + (bt[j + 1] - bt[j]) / 2;
  const chorus = s === "chorus1" || s === "chorus2", big = s === "chorus2";
  const ph = PHRASE(n), phN = Math.floor((n - SEC_FROM[s]) / 4);
  const early = s === "bridge" && n < 77;                       // v27: bars 73–76 — her quietest line, the band thins
  const finale = s === "outro" && n <= 81, release = s === "outro" && n >= 82;   // v41: the orchestra lets go from 82 — the last word dissolves   // v27: 81–82 full, 83–84 the band leaves her ring-out
  const lift = n === 27 || n === 51 || n === 72;                 // the bar before a lift
  // v27 register rule: while she sings, nothing sustained in 56–70 — a low pair under her and a high trio over her
  const lowPair = voicing(c, 44, 55, 2), hiTrio = voicing(c, 71, 83, 3), top = voicing(c, 83, 91, 2);
  // v67: the chorus's sustained parts swell in over its first four bars (the pickup stab and the timpani still mark the downbeat)
  const swellIn = s === "chorus1" ? 0.6 + 0.4 * grow(n, 28, 32) : s === "chorus2" ? 0.6 + 0.4 * grow(n, 52, 56) : 1;   // v101: from .6 — the downbeat was an 11 dB step down from the pickup
  const sus = (part, t, ms, dur, vel) => ms.forEach((m) => (m % 12 === 3 ? susNote(part, t, m, dur, vel, n) : part.note(t, m, dur, vel)));   // v101: the D# voice suspends (it was the last voice, usually not a D#)

  // STRINGS
  if (s === "verse1" && n >= 22) { const g = grow(n, 22, 28); sus(P.strings, b.t, lowPair, b.dur * 1.02, 30 + 36 * g); }
  else if (s === "chorus1") { sus(P.strings, b.t, hiTrio, b.dur * 1.02, 80 * swellIn); if (n >= 32) for (const m of lowPair) P.strings.note(b.t, m, b.dur * 1.02, 70); }   // the low pair enters at phrase 2
  else if (s === "chorus2") {
    if (phN >= 2) { for (let j = 0; j < nb; j++) { sus(P.strings, bt[j], hiTrio, beat * 1.05, 92); for (const m of lowPair) P.strings.note(bt[j], m, beat * 1.05, 84); } }
    else { sus(P.strings, b.t, hiTrio, b.dur * 1.02, 94 * swellIn); for (const m of lowPair) P.strings.note(b.t, m, b.dur * 1.02, 84 * swellIn); }
    if (n >= 60) for (const m of top) P.strings.note(b.t, m, b.dur * 1.02, 70);
  } else if (s === "break") { const g = grow(n, 68, 73), v = sings(n) ? [...lowPair, ...hiTrio] : voicing(c, 56, 75, 4); sus(P.strings, b.t, v, b.dur * 1.02, 52 + 36 * g); }
  else if (early) { for (const m of lowPair) P.strings.note(b.t, m, b.dur * 1.02, 56); }
  else if (s === "bridge") { const g = grow(n, 77, 81); for (let j = 0; j < nb; j++) { sus(P.strings, bt[j], hiTrio, beat * 1.05, 84 + 26 * g); for (const m of lowPair) P.strings.note(bt[j], m, beat * 1.05, 78 + 20 * g); } for (const m of top) P.strings.note(b.t, m, b.dur * 1.02, 84); }
  else if (finale) { for (let j = 0; j < nb; j++) for (const m of voicing(c, 51, 75, 5)) P.strings.note(bt[j], m, beat * 1.05, 110); for (const m of top) P.strings.note(b.t, m, b.dur * 1.02, 96); }
  else if (release) { for (const m of voicing(c, 51, 75, 4)) P.strings.note(b.t, m, b.dur * 1.04, n === 83 ? 72 : 52); }

  // CELLO — root and fifth through verse 1 and chorus 1; from chorus 2 a LAMENT BASS down each chord run
  // (G#m: G# F# E D# — her Emaj7 shape and the m7; B: D# C# B), one step per half bar, holding the last
  if ((s === "verse1" && n >= 18) || s === "chorus1" || s === "verse2") {
    const g = s === "verse1" ? 0.5 + 0.5 * grow(n, 18, 27) : 1;
    P.cello.note(b.t, root, b.dur * 1.04, (s === "chorus1" ? 88 : 68) * g); if (s !== "verse2") P.cello.note(b.t, root + 7, b.dur * 1.04, (s === "chorus1" ? 72 : 52) * g);
  } else if (s === "chorus2" || (s === "bridge" && !early) || finale) {
    const r = chordRun(n), seq = c === 2 ? [39, 37, 35] : [44, 42, 40, 39];
    for (let h = 0; h < 2; h++) { const m = seq[Math.min(seq.length - 1, r.pos * 2 + h)]; P.cello.note(b.t + h * b.dur / 2, m, b.dur / 2 * 1.04, finale ? 100 : 86); }
  } else if (early || s === "break" || release) { P.cello.note(b.t, root, b.dur * 1.04, release ? 60 : 72); }

  // PIZZICATO — her strum motif X..XX..X (1, &2, 3, &4) on root / fifth, in the verses
  if ((s === "verse1" && n >= 14) || s === "verse2") {
    const g = s === "verse1" ? 0.55 + 0.45 * grow(n, 14, 24) : 1;
    const hits = [[bt[0], root + 12, 84], [mid(1), root + 19, 62], [bt[2], root + 12, 78], [mid(3), root + 19, 62]];
    for (const [t, m, v] of hits) if (t) P.pizz.note(t, m, beat * 0.6, v * g);
  }

  // HORNS — tenor voicing (44–56), under her: sustains on odd phrases, stabs on even; the bridge climb; the finale
  if (((chorus || (s === "bridge" && !early)) && true) || finale) {
    const v = voicing(c, 44, 56, 3), swell = 64 + 24 * ph / 3 + (big ? 16 : 0) + (finale ? 24 : 0);
    if (finale || phN % 2 === 0) { sus(P.horns, b.t, v, b.dur * 1.02, swell * swellIn); if (ph === 0) for (const m of v) P.horns.note(b.t, m, beat * 0.9, 100); }
    else if (ph === 1 || ph === 3) { for (const m of v) { P.horns.note(bt[0], m, beat * 0.5, 96); if (nb > 2) P.horns.note(mid(1), m, beat * 0.4, 84); } }
  }

  // TIMPANI — chorus and bridge-climb downbeats on her root; a roll into every lift and into the button
  if (chorus || (s === "bridge" && !early) || finale) {
    if (ph % 2 === 0 || finale) P.timpani.note(bt[0], root, beat * 1.5, big || finale ? 112 : 92);
    if (ph === 3 && nb > 3 && !lift) P.timpani.note(bt[3], root, beat * 0.8, 70);
  }
  if ((n === 27 || n === 51 || n === 72 || n === 80 || n === 83) && nb >= 3) { const pk = n === 27 || n === 51; const t0 = pk ? bt[nb - 3] : bt[nb - 2], t1 = pk ? bt[nb - 2] : (bt[nb] ?? b.t + b.dur), k = pk ? 8 : 16;   // v51: the roll lands on the pickup beat
    for (let q = 0; q < k; q++) P.timpani.note(t0 + (t1 - t0) * q / k, root, (t1 - t0) / k * 1.2, 40 + 72 * q / (k - 1)); }

  // HARP — arpeggios above her (68+): verse 2's second half, the break, the bridge, the finale (16ths), a slow release
  if ((s === "verse2" && n >= 48) || s === "break" || s === "bridge" || s === "outro") {
    const seq = tones(c, 68, 95), up = seq.slice(0, 6), run = [...up, ...up.slice(1, -1).reverse()];
    const dens = finale ? 4 : release ? 1 : 2; let i = 0;
    for (let j = 0; j < nb; j++) for (let q = 0; q < dens; q++) { const t = bt[j] + (bt[j + 1] - bt[j]) * q / dens;
      P.harp.note(t, run[i++ % run.length], beat / dens * 2.5, (q === 0 ? 76 : 60) * (early ? 0.6 : release ? 0.7 : 1)); }
  }

  // QUARTET — lines, above and below her. Chorus 1: violin I runs the chord in 8ths (71+), violin II answers a
  // third under on the offbeats, viola double-stops 51–62 on 1 and 3, cello 8ths. Verse 2: the violins hold
  // high thirds. The break: a swell. Bars 73–76: violins tremolo alone, soft. 77–80 and the finale: full tremolo.
  if (chorus || s === "bridge" || s === "outro" || s === "verse2" || s === "break") {
    const hi = tones(c, 71, 88), lo = tones(c, 64, 80);
    const vel = s === "chorus1" ? 78 * swellIn : s === "chorus2" ? 90 * swellIn : early ? 62 + 10 * grow(n, 73, 77) : s === "bridge" ? 84 + 24 * grow(n, 77, 81) : finale ? 104 : release ? 56 : 60;
    if (chorus || finale) {
      const run = [...hi.slice(0, 4), ...hi.slice(1, 3).reverse()];
      for (let j = 0; j < nb; j++) for (let q = 0; q < 2; q++) { const i = (j * 2 + q) % run.length, t = bt[j] + (bt[j + 1] - bt[j]) * q / 2;
        P.vln1.note(t, run[i] + (finale ? 12 : 0), beat / 2 * 1.1, vel * (q ? 0.8 : 1));
        if (q) P.vln2.note(t, lo[Math.min(lo.length - 1, i + 1)], beat / 2 * 1.1, vel * 0.75); }
      for (const j of [0, 2]) if (bt[j]) sus(P.viola, bt[j], voicing(c, 49, 55, 2), beat * 2 * 0.98, vel * 0.85);   // v101: below her
      if (!finale) for (let j = 0; j < nb; j++) for (let q = 0; q < 2; q++) P.qcello.note(bt[j] + (bt[j + 1] - bt[j]) * q / 2, root + (q ? 7 : 0), beat / 2 * 0.9, vel * (q ? 0.7 : 0.9));
    } else if (s === "verse2") {
      for (const [part, m] of [[P.vln1, hi[2]], [P.vln2, hi[0]]]) part.note(b.t, m, b.dur * 1.03, 56);
    } else if (s === "break") {
      const g = 0.6 + 0.4 * grow(n, 68, 73);
      P.vln1.note(b.t, hi[3], b.dur * 1.03, 70 * g); P.vln2.note(b.t, hi[1], b.dur * 1.03, 64 * g);
      sus(P.viola, b.t, voicing(c, 51, 62, 2), b.dur * 1.03, 62 * g); P.qcello.note(b.t, root, b.dur * 1.03, 72 * g);
    } else if (release) {
      P.vln1.note(b.t, hi[2], b.dur * 1.04, vel); P.vln2.note(b.t, hi[0], b.dur * 1.04, vel * 0.9);
    } else {   // bridge: tremolo 16ths, up the chord bar by bar
      const k = Math.min(hi.length - 1, (n - 73) % 4 + 1);
      for (let j = 0; j < nb; j++) for (let q = 0; q < 4; q++) { const t = bt[j] + (bt[j + 1] - bt[j]) * q / 4;
        P.vln1.note(t, hi[k], beat / 4 * 1.2, vel * (q ? 0.7 : 0.95)); P.vln2.note(t, hi[Math.max(0, k - 2)], beat / 4 * 1.2, vel * 0.65); }
      // (v101: viola/qcello sit out the climb — they arrive with the finale)
    }
  }

  // TAIKO — on her roots (44/47, not the sample's G/A/C): 1 and 3 in chorus 2, from bar 70 in the break, every beat
  // with a fill into each phrase in the bridge climb and the finale
  if (s === "chorus2" || (s === "break" && n >= 70) || (s === "bridge" && !early) || finale) {
    const g = s === "break" ? 0.5 + 0.5 * grow(n, 70, 73) : s === "chorus2" ? 0.85 : 1;
    const hits = s === "chorus2" || s === "break" ? [0, 2] : [0, 1, 2, 3];
    for (const j of hits) if (bt[j] != null) P.taiko.note(bt[j], j % 2 ? root + 3 : root, beat * 0.8, (j % 2 ? 84 : 112) * g);
    if (s !== "chorus2" && ph === 3 && nb > 3 && !lift) for (let q = 0; q < 4; q++) P.taiko.note(bt[3] + beat * q / 4, root + 1, beat / 4, (70 + 12 * q) * g);
  }

  // AAHS — a block choir ABOVE her (68–83): the second half of chorus 1, chorus 2, the bridge climb, the finale, a soft release
  if ((s === "chorus1" && n >= 36) || s === "chorus2" || s === "outro") {   // v101: no aahs in the bridge — 81 is the arrival
    const v = voicing(c, 68, 83, 4), g = s === "chorus1" ? grow(n, 36, 40) : s === "chorus2" ? 0.2 + 0.8 * grow(n, 52, 58) : release ? 0.55 : 1;   // v67: the aahs come in late
    sus(P.aahs, b.t, v, b.dur * 1.05, (big || finale ? 92 : 76) * g);
  }

  // THE PICKUP (v35) — the chorus begins on "Oh, won't you": from beat 3 of bars 27 and 51 the strings, horns and timpani
  // are already in, at the chorus's own voicing and weight
  if (n === 27 || n === 51) { const jP = nb > 2 ? nb - 2 : 0, tP = bt[jP], dP = b.t + b.dur - tP, cv = n === 51; const nc = c;   // v51: two beats before "kiss" (27.4 / 51.3)
    sus(P.strings, tP, voicing(nc, 71, 83, 3), dP * 1.02, cv ? 64 : 56); for (const m of voicing(nc, 44, 55, 2)) P.strings.note(tP, m, dP * 1.02, cv ? 56 : 46);   // v101: ÷1.6 — the run-up under the landing
    for (const m of voicing(nc, 44, 56, 3)) P.horns.note(tP, m, dP * 1.02, 60);
    P.timpani.note(tP, root, beat * 1.5, cv ? 112 : 100); if (cv) P.taiko.note(tP, root, beat * 0.8, 112);
    for (let j = jP; j < nb; j++) for (let q = 0; q < 2; q++) { const t = bt[j] + (bt[j + 1] - bt[j]) * q / 2; P.vln1.note(t, tones(nc, 71, 88)[(j * 2 + q) % 4], beat / 2 * 1.1, cv ? 90 : 78); } }
  // THE ANSWER (v27) — her kiss figure comes back in her own silences: pizz + harp after chorus 1 (bar 42), glock +
  // violin I an octave up after chorus 2 (bar 66), and the horns open the finale with it (bar 81 — the one new thing there)
  if (n === 42) { kissAt(P.pizz, b, 0, 92); kissAt(P.harp, b, 12, 84); }
  if (n === 66) { kissAt(P.glock, b, 12, 80); kissAt(P.vln1, b, 12, 92); }
  if (n === 81) { kissAt(P.horns, b, -12, 112); kissAt(P.strings, b, 0, 100); }
}

// THE BUTTON (v27) — on her LAST STRUM, bar 84's downbeat: the roll came in bar 83; the hit, then the chord
// let go in three steps over three seconds so her guitar is the last thing heard
{ const b84 = barN(84); if (b84) { const tB = b84.t, c = b84.chord, root = ROOT[c];
  P.timpani.note(tB - 0.008, root, 2.5, 120); P.taiko.note(tB, root, 1.2, 120);
  for (const [dt, vel] of [[0, 104], [1.0, 74], [2.0, 46]]) {
    for (const m of voicing(c, 51, 87, 8)) P.strings.note(tB + dt, m, 1.15, vel);
    for (const m of voicing(c, 44, 56, 3)) P.horns.note(tB + dt, m, 1.15, vel);
    for (const m of voicing(c, 68, 83, 4)) P.aahs.note(tB + dt, m, 1.15, vel * 0.9);
    P.cello.note(tB + dt, root, 1.15, vel); P.vln1.note(tB + dt, tones(c, 79, 91)[1], 1.15, vel); P.vln2.note(tB + dt, tones(c, 71, 83)[1], 1.15, vel * 0.9); }
  for (const m of tones(c, 56, 95)) P.harp.note(tB + 0.02 * (m - 56) / 4, m, 2.5, 80);   // one upward sweep
} }

// GLOCKENSPIEL — her melody an octave up: chorus 2's second half and the bridge climb (where she sings)
for (const v of notes) {
  if (v.dur < 0.18) continue;
  const b = bars.find((x) => v.t >= x.t && v.t < x.t + x.dur); if (!b) continue;
  const s = section(b.n);
  if (s === "chorus2" && b.n >= 60) P.glock.note(v.t, v.midi + 12, Math.max(0.25, v.dur), 68);   // v101: the climb keeps its glock for the finale's kiss answer
}

// ── render ──
const receipt = {};
for (const p of Object.values(P)) {
  const wav = p.write();
  receipt[p.name] = p.receipt;
  console.log(wav ? `  ${p.name.padEnd(8)} ${String(p.count).padStart(4)} notes → ${wav.replace(LANE + "/", "")}` : `  ${p.name.padEnd(8)} silent`);
}
writeFileSync(resolve(OUT, "orch.events.json"), JSON.stringify(receipt));
console.log(`✓ ${OUT}`);
