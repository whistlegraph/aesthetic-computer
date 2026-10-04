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
class Part {
  constructor(name, program) { this.name = name; this.program = program; this.ev = []; this.count = 0; this.receipt = []; }
  note(t, midi, dur, vel) {
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

// ── the parts, bar by bar ──
for (const b of bars) {
  const s = section(b.n), c = b.chord, root = ROOT[c], bt = b.beats, nb = b.nb, beat = b.dur / nb;
  const mid = (j) => bt[j] + (bt[j + 1] - bt[j]) / 2;
  const chorus = s === "chorus1" || s === "chorus2", big = s === "chorus2" || s === "bridge";
  const last = b.n === 27 || b.n === 51 || b.n === 72;        // the bar before a lift

  // STRINGS — creep in under verse 1 (bar 22 → 27), full chords in the choruses, high and thin
  // in verse 2, swelling through the break and the bridge, long in the outro
  if (s === "verse1" && b.n >= 22) {
    const g = grow(b.n, 22, 28);
    for (const m of voicing(c, 51, 63, 3)) P.strings.note(b.t, m, b.dur * 1.02, 30 + 40 * g);
  } else if (chorus) {   // v26: variation — chorus 1 legato; chorus 2 legato for two phrases, re-bowed on every beat for the next two
    const v = voicing(c, 51, 75, 6), swell = 1 + 0.15 * Math.sin(Math.PI * (PHRASE(b.n) + 0.5) / 4), phN = Math.floor((b.n - SEC_FROM[s]) / 4);
    if (big && phN % 4 >= 2) { for (let j = 0; j < nb; j++) for (const m of v) P.strings.note(bt[j], m, beat * 1.05, 92 * swell); }
    else for (const m of v) P.strings.note(b.t, m, b.dur * 1.02, (big ? 96 : 84) * swell);
    if (big && phN >= 2) for (const m of voicing(c, 75, 83, 2)) P.strings.note(b.t, m, b.dur * 1.02, 72);
  } else if (s === "verse2") {
    for (const m of voicing(c, 68, 80, 2)) P.strings.note(b.t, m, b.dur * 1.02, 48);
  } else if (s === "break") {
    const g = grow(b.n, 68, 73);
    for (const m of voicing(c, 56, 75, 4)) P.strings.note(b.t, m, b.dur * 1.02, 56 + 36 * g);
  } else if (s === "bridge") {
    // the climb: re-bowed every beat, louder bar by bar, an octave added from 77
    const g = grow(b.n, 73, 81);
    for (let j = 0; j < nb; j++) for (const m of voicing(c, 51, 75, 5)) P.strings.note(bt[j], m, beat * 1.05, 70 + 40 * g);
    if (b.n >= 77) for (const m of voicing(c, 75, 87, 3)) P.strings.note(b.t, m, b.dur * 1.02, 84);
  } else if (s === "outro") {   // v24: the finale — "if anything we should still be ramping up"
    for (let j = 0; j < nb; j++) for (const m of voicing(c, 51, 75, 5)) P.strings.note(bt[j], m, beat * 1.05, 110);
    for (const m of voicing(c, 75, 87, 3)) P.strings.note(b.t, m, b.dur * 1.02, 96);
  }

  // CELLO — long root and fifth from bar 18, under everything but the break
  if ((s === "verse1" && b.n >= 18) || s === "verse2" || chorus || s === "bridge" || s === "outro") {
    const g = s === "verse1" ? 0.5 + 0.5 * grow(b.n, 18, 27) : 1;
    P.cello.note(b.t, root, b.dur * 1.04, (chorus || s === "bridge" ? 92 : 72) * g);
    P.cello.note(b.t, root + 7, b.dur * 1.04, (chorus || s === "bridge" ? 76 : 56) * g);
  }

  // PIZZICATO — her own strum motif X..XX..X (1, &2, 3, &4) on root / fifth, in the verses only
  if ((s === "verse1" && b.n >= 14) || s === "verse2") {
    const g = s === "verse1" ? 0.55 + 0.45 * grow(b.n, 14, 24) : 1;
    const hits = [[bt[0], root + 12, 84], [mid(1), root + 19, 62], [bt[2], root + 12, 78], [mid(3), root + 19, 62]];
    for (const [t, m, v] of hits) if (t) P.pizz.note(t, m, beat * 0.6, v * g);
  }

  // HORNS — sustained chord tones in the choruses and the bridge, swelling across each phrase;
  // a stab on every phrase downbeat
  if (chorus || s === "bridge" || s === "outro") {
    const ph = PHRASE(b.n), v = voicing(c, 56, 68, 3), swell = 64 + 24 * ph / 3 + (big ? 16 : 0) + (s === "outro" ? 24 : 0), phN = Math.floor((b.n - SEC_FROM[s]) / 4);
    if (phN % 2 === 0) { for (const m of v) P.horns.note(b.t, m, b.dur * 1.02, swell); if (ph === 0) for (const m of v) P.horns.note(b.t, m - 12, beat * 0.9, 100); }   // v26: odd phrases sustain…
    else if (ph === 1 || ph === 3) { for (const m of v) { P.horns.note(bt[0], m, beat * 0.5, 96); if (nb > 2) P.horns.note(mid(1), m, beat * 0.4, 84); } }   // …even phrases stab (bars 2 and 4)
  }

  // TIMPANI — the chorus and bridge downbeats on her root; a roll into every lift
  if (chorus || s === "bridge" || s === "outro") {
    if (PHRASE(b.n) % 2 === 0 || s === "outro") P.timpani.note(bt[0], root, beat * 1.5, big || s === "outro" ? 112 : 92);
    if (PHRASE(b.n) === 3 && nb > 3) P.timpani.note(bt[3], root, beat * 0.8, 70);
  }
  if (last && nb >= 3) {                                        // a crescendo roll over the last two beats
    const t0 = bt[nb - 2], t1 = bt[nb] ?? b.t + b.dur, k = 16;
    for (let q = 0; q < k; q++) P.timpani.note(t0 + (t1 - t0) * q / k, root, (t1 - t0) / k * 1.2, 40 + 72 * q / (k - 1));
  }

  // HARP — 8th-note arpeggios across the chord: verse 2, the break, the bridge and the outro
  if ((s === "verse2" && b.n >= 48) || s === "break" || s === "bridge" || s === "outro") {   // v26: the harp waits for verse 2's second half
    const seq = tones(c, 56, 83); const up = seq.slice(0, 6), run = [...up, ...up.slice(1, -1).reverse()];
    const dens = s === "bridge" ? 4 : 2; let i = 0;
    for (let j = 0; j < nb; j++) for (let q = 0; q < dens; q++) {
      const t = bt[j] + (bt[j + 1] - bt[j]) * q / dens;
      P.harp.note(t, run[i++ % run.length], beat / dens * 2.5, (q === 0 ? 76 : 60) * (s === "outro" ? 0.8 : 1));
    }
  }

  // QUARTET (v24) — not a pad: lines. Chorus 1: violin I runs the chord in 8ths, violin II answers on
  // the offbeats a third under, viola double-stops re-bowed on 1 and 3, cello root/fifth in 8ths.
  // Verse 2: the violins hold high thirds. The break: the quartet alone swells. The bridge: 16th
  // tremolo climbing. The outro is the finale: everything, an octave up.
  if (chorus || s === "bridge" || s === "outro" || s === "verse2" || s === "break") {
    const hi = tones(c, 71, 88), lo = tones(c, 64, 80), fin = s === "outro";
    const vel = s === "chorus1" ? 78 : s === "chorus2" ? 90 : s === "bridge" ? 84 + 24 * grow(b.n, 73, 81) : fin ? 104 : 60;
    if (chorus || fin) {
      const run = [...hi.slice(0, 4), ...hi.slice(1, 3).reverse()];
      for (let j = 0; j < nb; j++) for (let q = 0; q < 2; q++) { const i = (j * 2 + q) % run.length, t = bt[j] + (bt[j + 1] - bt[j]) * q / 2;
        P.vln1.note(t, run[i] + (fin ? 12 : 0), beat / 2 * 1.1, vel * (q ? 0.8 : 1));
        if (q) P.vln2.note(t, lo[Math.min(lo.length - 1, i + 1)], beat / 2 * 1.1, vel * 0.75); }
      for (const j of [0, 2]) if (bt[j]) for (const m of voicing(c, 60, 72, 2)) P.viola.note(bt[j], m, beat * 2 * 0.98, vel * 0.85);
      for (let j = 0; j < nb; j++) for (let q = 0; q < 2; q++) P.qcello.note(bt[j] + (bt[j + 1] - bt[j]) * q / 2, root + (q ? 7 : 0), beat / 2 * 0.9, vel * (q ? 0.7 : 0.9));
    } else if (s === "verse2") {
      for (const [part, m] of [[P.vln1, hi[2]], [P.vln2, hi[0]]]) part.note(b.t, m, b.dur * 1.03, 56);
    } else if (s === "break") {
      const g = 0.6 + 0.4 * grow(b.n, 68, 73);
      P.vln1.note(b.t, hi[3], b.dur * 1.03, 70 * g); P.vln2.note(b.t, hi[1], b.dur * 1.03, 64 * g);
      for (const m of voicing(c, 60, 72, 2)) P.viola.note(b.t, m, b.dur * 1.03, 62 * g); P.qcello.note(b.t, root, b.dur * 1.03, 72 * g);
    } else {   // bridge: tremolo 16ths, up the chord bar by bar
      const k = Math.min(hi.length - 1, (b.n - 73) % 4 + 1);
      for (let j = 0; j < nb; j++) for (let q = 0; q < 4; q++) { const t = bt[j] + (bt[j + 1] - bt[j]) * q / 4;
        P.vln1.note(t, hi[k], beat / 4 * 1.2, vel * (q ? 0.7 : 0.95)); P.vln2.note(t, hi[Math.max(0, k - 2)], beat / 4 * 1.2, vel * 0.65); }
      for (const m of voicing(c, 60, 72, 2)) P.viola.note(b.t, m, b.dur * 1.03, vel * 0.8); P.qcello.note(b.t, root, b.dur * 1.03, vel * 0.9);
    }
  }

  // TAIKO (v24) — big drums: 1 and 3 in chorus 2, rising through the break, every beat and a
  // 16th fill into each phrase in the bridge and the finale
  if (s === "chorus2" || s === "break" || s === "bridge" || s === "outro") {
    const g = s === "break" ? 0.4 + 0.6 * grow(b.n, 68, 73) : s === "chorus2" ? 0.85 : 1;
    const hits = s === "chorus2" || s === "break" ? [0, 2] : [0, 1, 2, 3];
    for (const j of hits) if (bt[j] != null) P.taiko.note(bt[j], j % 2 ? 48 : 43, beat * 0.8, (j % 2 ? 84 : 112) * g);
    if (s !== "chorus2" && PHRASE(b.n) === 3 && nb > 3) for (let q = 0; q < 4; q++) P.taiko.note(bt[3] + beat * q / 4, 45, beat / 4, (70 + 12 * q) * g);
    if ((s === "bridge" || s === "outro") && nb > 3) P.taiko.note(mid(3), 48, beat * 0.4, 72 * g);
  }

  // AAHS — a block choir on the chord: the second half of chorus 1, all of chorus 2, the bridge, the outro
  if ((s === "chorus1" && b.n >= 36) || s === "chorus2" || s === "bridge" || s === "outro") {
    const v = voicing(c, 56, 71, 4), g = s === "chorus1" ? grow(b.n, 36, 40) : 1;
    for (const m of v) P.aahs.note(b.t, m, b.dur * 1.05, (big ? 92 : 76) * g);
  }
}

// THE BUTTON (v25) — the end of bar 84 is the record's last downbeat: a timpani roll into it, then the
// whole orchestra holds the chord for five seconds under her ring-out, taiko and timpani on the hit
{ const b84 = barN(84); if (b84) { const tE = b84.t + b84.dur, c = b84.chord, root = ROOT[c], beat = b84.dur / b84.nb;
  for (let q = 0; q < 12; q++) P.timpani.note(b84.beats[2] + (tE - b84.beats[2]) * q / 12, root, beat / 6 * 1.2, 50 + 70 * q / 11);
  P.timpani.note(tE, root, 2.5, 120); P.taiko.note(tE, 43, 1.2, 120); P.taiko.note(tE + 0.02, 48, 1.0, 100);
  for (const m of voicing(c, 51, 87, 8)) P.strings.note(tE, m, 5.0, 110);
  for (const m of voicing(c, 56, 68, 3)) { P.horns.note(tE, m, 4.5, 104); P.horns.note(tE, m - 12, 4.5, 96); }
  for (const [part, m] of [[P.vln1, tones(c, 79, 91)[1]], [P.vln2, tones(c, 71, 83)[1]], [P.viola, tones(c, 60, 72)[0]], [P.qcello, root]]) part.note(tE, m, 5.0, 108);
  P.cello.note(tE, root, 5.0, 100); P.cello.note(tE, root - 12, 5.0, 90);
  for (const m of voicing(c, 56, 71, 4)) P.aahs.note(tE, m, 5.0, 100);
  for (const m of tones(c, 56, 95)) P.harp.note(tE + 0.02 * (m - 56) / 4, m, 3.0, 80);   // one upward harp sweep
} }

// GLOCKENSPIEL — her melody an octave up, chorus 2, the bridge and the outro
for (const v of notes) {
  if (v.dur < 0.18) continue;
  const b = bars.find((x) => v.t >= x.t && v.t < x.t + x.dur); if (!b) continue;
  const s = section(b.n);
  if ((s === "chorus2" && b.n >= 60) || s === "bridge" || s === "outro") P.glock.note(v.t, v.midi + 12, Math.max(0.25, v.dur), s === "outro" ? 56 : 68);   // v26: glock from chorus 2's second half
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
