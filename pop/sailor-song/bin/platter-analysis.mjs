#!/usr/bin/env node
// platter-analysis.mjs — read her take against the papers platter.
//
// measures.json, vox-notes.json and take.analysis.json say WHAT she played.
// This pass asks what the platter's rules say about it: the rhythm platter
// (necklace geometry, syncopation vector, chronotonic distance, entrainment,
// beat bins), the chamber platter (form proportions, climax and stillness
// positions, phrase lengths, register per phrase), the bass platter (sub
// register, tempo band, delay in beats) and the pop strategies digest
// (track length, the cut, the build curve). Every number cites the digest
// rule it comes from. Nothing here touches audio except the vocal and guitar
// stems, read once for a loudness envelope per bar.
//
//   node pop/sailor-song/bin/platter-analysis.mjs          → platter-analysis.json + stdout
//
// Rhythm math is pop/lib/necklace.mjs (papers/rhythm-platter is its spec).

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import * as N from "../../lib/necklace.mjs";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const M = JSON.parse(readFileSync(resolve(LANE, "measures.json"), "utf8"));
const V = JSON.parse(readFileSync(resolve(LANE, "vox-notes.json"), "utf8"));
const A = JSON.parse(readFileSync(resolve(LANE, "take.analysis.json"), "utf8"));
const DUR = A.duration;
const bars = M.bars;

// Sections as render.mjs has them (bar numbers, 1-based inclusive).
const SECTIONS = [
  ["intro", 1, 10], ["verse1", 11, 27], ["chorus1", 28, 43], ["verse2", 44, 51],
  ["chorus2", 52, 67], ["break", 68, 71], ["bridge", 72, 80], ["outro", 81, 84],
];
const barOf = (t) => bars.find((b) => t >= b.t && t < b.t + b.dur);
const sectionOfBar = (n) => SECTIONS.find(([, a, b]) => n >= a && n <= b)?.[0];
const r3 = (x) => Math.round(x * 1000) / 1000;
const r1 = (x) => Math.round(x * 10) / 10;
const median = (a) => { const s = [...a].sort((x, y) => x - y); return s.length ? s[Math.floor(s.length / 2)] : NaN; };
const pct = (a, p) => { const s = [...a].sort((x, y) => x - y); return s.length ? s[Math.min(s.length - 1, Math.floor(p * s.length))] : NaN; };
const mean = (a) => (a.length ? a.reduce((x, y) => x + y, 0) / a.length : NaN);

const out = { source: "measures.json + vox-notes.json + take.analysis.json", date: "2026-09-29" };

// ── 1. RHYTHM · her strum motif as a necklace (digest 01–06) ─────────────────
// The motif is X..XX..X on 8 pulses (8ths): onsets {0,3,4,7}, IOI (3,1,3,1).
const motif = "x..xx..x";
const motifBars = bars.filter((b) => b.strum.length === 8).map((b) => ({ ...b, strum: b.strum.toLowerCase() }));
const patternCounts = {};
for (const b of motifBars) patternCounts[b.strum] = (patternCounts[b.strum] || 0) + 1;
const an = N.analyze(motif);
const E48 = N.toBox(N.bjorklund(4, 8));
const named = {
  "E(4,8) straight 8ths": E48,
  "tresillo E(3,8)": N.toBox(N.bjorklund(3, 8)),
  "cinquillo": "x.xx.xx.",
  "her complement (hocket partner)": N.toBox(N.complement(motif)),
};
const distances = Object.fromEntries(Object.entries(named).map(([k, v]) => {
  const d = N.dist(motif, v, { measure: "chronotonic", cyclic: true });
  let swap = null;
  try { swap = N.dist(motif, v, { measure: "swap", cyclic: true }).distance; } catch { /* unequal k */ }
  return [k, { box: v, chronotonic: d.distance, rotation: d.rotation, swap }];
}));
// per-bar drift from the motif: chronotonic (the platter's default), fixed rotation
// because the bar's downbeat is anchored — a rotation would be a different bar.
const perBarDrift = motifBars.map((b) => ({
  n: b.n, strum: b.strum, chord: b.chord,
  chronotonic: N.dist(motif, b.strum, { cyclic: false }).distance,
  extraOnsets: N.rhythm(b.strum).k - 4,
}));
const driftBySection = Object.fromEntries(SECTIONS.map(([s]) => {
  const rows = perBarDrift.filter((d) => sectionOfBar(d.n) === s);
  return [s, { bars: rows.length, meanChronotonic: r3(mean(rows.map((d) => d.chronotonic))), meanExtraOnsets: r3(mean(rows.map((d) => d.extraOnsets))), verbatim: rows.filter((d) => d.chronotonic === 0).length }];
}));
out.rhythm = {
  motif: { box: motif, ...an, syncopation: N.syncopationProfile(motif), metricWeights: N.metricWeights(8), periodic: N.isPeriodic(motif) },
  kitPattern: { box: "x..xx..x", note: "render.mjs kit: kick 1 and &2, snare 3, rim &4 — the SAME necklace as her hand; the kit doubles her, it does not interlock" },
  distances,
  patternCounts,
  perBarDrift,
  driftBySection,
  morphToStraight: N.morphPath(motif, E48).map(N.toBox),
};

// ── 2. RHYTHM · beat bins: where her off-beat strums sit inside the beat (07) ─
// The necklace decides which pulse; an offset table decides where in the bin.
const beats = M.beats.snapped;
const offbeatPhase = [], onbeatOff = [];
for (const s of M.strumList) {
  if (s.amp < 0.3) continue;
  let i = beats.findIndex((b) => b > s.t) - 1;
  if (i < 0 || i >= beats.length - 1) continue;
  const ph = (s.t - beats[i]) / (beats[i + 1] - beats[i]);
  if (ph > 0.3 && ph < 0.75) offbeatPhase.push(ph);
  else if (ph < 0.15) onbeatOff.push((s.t - beats[i]) * 1000);
  else if (ph > 0.85) onbeatOff.push((s.t - beats[i + 1]) * 1000);
}
const swingPhase = median(offbeatPhase);
// her VOICE against her own 8th grid (vox-notes onsets are pre-lock, i.e. as sung)
const beatList = bars.flatMap((b) => b.beats.slice(0, -1));
const grid8 = [];
for (let i = 0; i < beatList.length - 1; i++) grid8.push(beatList[i], (beatList[i] + beatList[i + 1]) / 2);
const voiceRows = V.notes.map((n) => { let best = grid8[0]; for (const t of grid8) if (Math.abs(t - n.t) < Math.abs(best - n.t)) best = t; const b = barOf(n.t); return { off: (n.t - best) * 1000, sec: b ? sectionOfBar(b.n) : null, dur: n.dur }; });
const voiceBySection = Object.fromEntries(SECTIONS.map(([sName]) => { const r = voiceRows.filter((x) => x.sec === sName); return r.length > 3 ? [sName, { notes: r.length, signedMedianMs: Math.round(median(r.map((x) => x.off))), absMedianMs: Math.round(median(r.map((x) => Math.abs(x.off)))), lateShare: r3(r.filter((x) => x.off > 15).length / r.length) }] : null; }).filter(Boolean));
out.beatBins = {
  offbeatStrums: offbeatPhase.length,
  offbeatPhaseMedian: r3(swingPhase), offbeatPhaseP10: r3(pct(offbeatPhase, 0.1)), offbeatPhaseP90: r3(pct(offbeatPhase, 0.9)),
  swingRatio: r3(swingPhase / (1 - swingPhase)),
  offbeatLateMsAt120: r1((swingPhase - 0.5) * 500),
  voiceVsGrid: {
    all: { signedMedianMs: Math.round(median(voiceRows.map((x) => x.off))), absMedianMs: Math.round(median(voiceRows.map((x) => Math.abs(x.off)))) },
    bySection: voiceBySection,
    longNotesSignedMs: Math.round(median(voiceRows.filter((x) => x.dur > 0.4).map((x) => x.off))),
    shortNotesSignedMs: Math.round(median(voiceRows.filter((x) => x.dur <= 0.2).map((x) => x.off))),
    afterLockVox: "lock-vox.mjs pulls these onto the grid: abs median 41 → 8 ms (its own readout)",
  },
  note: "digest 07 (Danielsen): a beat is a bin, not a point. Her & sits late of the mathematical 8th; that offset is the groove and must live in a separate microtiming layer, never folded into onsets",
};

// ── 3. RHYTHM · entrainment window (07, London) ───────────────────────────────
const bpmBySection = Object.fromEntries(SECTIONS.map(([s, a, b]) => {
  const rows = bars.filter((x) => x.n >= a && x.n <= b);
  return [s, { median: r1(median(rows.map((x) => x.bpm))), min: r1(Math.min(...rows.map((x) => x.bpm))), max: r1(Math.max(...rows.map((x) => x.bpm))), start: r1(rows[0].t), end: r1(rows[rows.length - 1].t + rows[rows.length - 1].dur) }];
}));
const bpmMed = median(bars.map((b) => b.bpm));
const beatMs = 60000 / bpmMed;
out.entrainment = {
  medianBpm: r1(bpmMed),
  periodsMs: { eighth: r1(beatMs / 2), quarter: r1(beatMs), half: r1(beatMs * 2), bar: r1(beatMs * 4), chordCycle2bars: r1(beatMs * 8) },
  window: { floorMs: 100, ceilingMs: 2000, sweetSpotMs: [500, 700] },
  felt: {
    quarterInSweetSpot: beatMs >= 500 && beatMs <= 700, quarterMsFromSweetSpot: r1(beatMs < 500 ? beatMs - 500 : beatMs > 700 ? beatMs - 700 : 0),
    halfInWindow: beatMs * 2 <= 2000,
    barAtCeiling: Math.abs(beatMs * 4 - 2000) < 200,
    chordCycleFelt: beatMs * 8 <= 2000,
  },
  bpmBySection,
  tempoArch: { verse1Sag: r1(bpmBySection.verse1.median - bpmMed), chorus1Lift: r1(bpmBySection.chorus1.median - bpmMed), bridge: r1(bpmBySection.bridge.median - bpmMed) },
};

// ── 4. HARMONY · harmonic rhythm and where the IV actually is ─────────────────
const chordRuns = [];
for (const b of bars) {
  const last = chordRuns[chordRuns.length - 1];
  if (last && last.chord === b.chord) { last.bars++; last.to = b.n; } else chordRuns.push({ chord: b.chord, from: b.n, to: b.n, bars: 1 });
}
const runLen = {};
for (const r of chordRuns) { runLen[r.chord] = runLen[r.chord] || []; runLen[r.chord].push(r.bars); }
out.harmony = {
  key: A.key[0], tuningCents: A.tuningCents,
  vocabulary: { "G#m": "vi (the Cmaj7 shape through the open low string: Emaj7/G#)", "B": "I", "Emaj7": "IV" },
  barCounts: bars.reduce((o, b) => ((o[b.chord] = (o[b.chord] || 0) + 1), o), {}),
  runs: chordRuns,
  runLengths: Object.fromEntries(Object.entries(runLen).map(([c, l]) => [c, { runs: l.length, median: median(l), max: Math.max(...l) }])),
  harmonicRhythmBeats: r1(mean(chordRuns.map((r) => r.bars)) * 4),
  ivBars: bars.filter((b) => b.chord === "Emaj7").map((b) => `${b.n}:${sectionOfBar(b.n)}`),
};

// ── 5. MELODY · her tuned line (vox-notes.json) ───────────────────────────────
// her voice enters at ~24 s; tuned "notes" before that are Demucs bleed of the guitar
const notes = V.notes.filter((n) => n.t >= 23);

const TONIC = 56; // G#3 in the guitar frame
const degName = ["1", "b2", "2", "b3", "3", "4", "b5", "5", "b6", "6", "b7", "7"];
// the hook: most repeated 4-note degree sequence (by count, ignoring octave)
const degSeq = notes.map((n) => degName[((n.midi - 56) % 12 + 12) % 12]);
const grams = {};
for (let i = 0; i + 4 <= degSeq.length; i++) { const g = degSeq.slice(i, i + 4).join(" "); grams[g] = (grams[g] || 0) + 1; }
const topGrams = Object.entries(grams).sort((x, y) => y[1] - x[1]).slice(0, 6);
const degHist = {};
for (const n of notes) { const d = degName[((n.midi - TONIC) % 12 + 12) % 12]; degHist[d] = (degHist[d] || 0) + n.dur; }
const totalDur = notes.reduce((s, n) => s + n.dur, 0);
const degShare = Object.fromEntries(Object.entries(degHist).sort((a, b) => b[1] - a[1]).map(([d, v]) => [d, r3(v / totalDur)]));
const intervals = notes.slice(1).map((n, i) => (n.t - (notes[i].t + notes[i].dur) < 0.6 ? n.midi - notes[i].midi : null)).filter((x) => x !== null);
const intHist = {};
for (const i of intervals) intHist[i] = (intHist[i] || 0) + 1;
const steps = intervals.filter((i) => Math.abs(i) <= 2 && i !== 0).length, leaps = intervals.filter((i) => Math.abs(i) > 2).length, repeats = intervals.filter((i) => i === 0).length;
// phrases: the tuned notes run contiguous through vowels, so breaths are read
// off the dry vocal stem: 10 ms RMS, a phrase ends at a gap of ≥ 0.35 s under
// the gate, and must hold ≥ 0.3 s.
function phrasesFromStem(path, { gapS = 0.35, minS = 0.3 } = {}) {
  if (!existsSync(path)) return [];
  const { samples: x, sampleRate: sr } = readWavMono(path);
  const hop = Math.floor(sr / 100), nf = Math.floor(x.length / hop);
  const db = new Float32Array(nf);
  for (let f = 0; f < nf; f++) { let s = 0; for (let i = f * hop; i < (f + 1) * hop; i++) s += x[i] * x[i]; db[f] = 10 * Math.log10(s / hop + 1e-12); }
  const gate = pct(Array.from(db).filter((v) => v > -70), 0.85) - 10; // 10 dB under the loud frames: swept 6–22, this is the knee where breaths open and words do not
  const on = Array.from(db, (v) => v > gate);
  const res = []; let start = null, lastOn = null;
  for (let f = 0; f <= nf; f++) {
    const v = f < nf && on[f];
    if (v) { if (start === null) start = f; lastOn = f; }
    else if (start !== null && (f - lastOn) / 100 >= gapS) { if ((lastOn - start) / 100 >= minS) res.push({ start: start / 100, end: (lastOn + 1) / 100 }); start = null; }
  }
  if (start !== null && (lastOn - start) / 100 >= minS) res.push({ start: start / 100, end: (lastOn + 1) / 100 });
  return res.filter((p) => p.start > 20); // her voice enters at ~24 s; before that is bleed
}
const phrases = phrasesFromStem(resolve(LANE, "src/vox/vocals-dry-48k.wav")).map((p) => ({ ...p, notes: notes.filter((n) => n.t + n.dur / 2 >= p.start && n.t + n.dur / 2 <= p.end) })).filter((p) => p.notes.length);
const phraseRows = phrases.map((p) => {
  const b = barOf(p.start);
  const m = p.notes.map((n) => n.midi);
  const bt = b ? b.beats.slice(0, -1) : [];
  const beatIn = bt.length ? bt.reduce((best, t, i) => (Math.abs(t - p.start) < Math.abs(bt[best] - p.start) ? i : best), 0) : null;
  return { start: r1(p.start), bar: b?.n, section: b ? sectionOfBar(b.n) : null, beatIn: beatIn === null ? null : beatIn + 1, offsetMs: beatIn === null ? null : Math.round((p.start - bt[beatIn]) * 1000), beats: r1((p.end - p.start) / (beatMs / 1000)), notes: m.length, lo: Math.min(...m), hi: Math.max(...m), first: m[0], last: m[m.length - 1], degreeLast: degName[((m[m.length - 1] - TONIC) % 12 + 12) % 12], contour: m.length > 1 ? (m[m.length - 1] > m[0] ? "rise" : m[m.length - 1] < m[0] ? "fall" : "level") : "one" };
});
// register per section (R8 chamber-01): median midi of sung notes
const registerBySection = Object.fromEntries(SECTIONS.map(([s, a, b]) => {
  const rows = notes.filter((n) => { const bb = barOf(n.t); return bb && bb.n >= a && bb.n <= b; });
  if (!rows.length) return [s, null];
  const m = rows.map((n) => n.midi);
  return [s, { notes: rows.length, medianMidi: median(m), lo: Math.min(...m), hi: Math.max(...m), meanDurMs: r1(mean(rows.map((n) => n.dur)) * 1000) }];
}));
// chord-tone share: over each bar's chord, is the sung pitch class a chord tone?
const CHORD_PCS = { "G#m": [8, 11, 3], "B": [11, 3, 6], "Emaj7": [4, 8, 11, 3] };
let ct = 0, nct = 0; const nctByChord = {};
for (const n of notes) {
  const b = barOf(n.t); if (!b) continue;
  const pcs = CHORD_PCS[b.chord]; const pc = n.midi % 12;
  const isCt = pcs.includes(pc);
  if (isCt) ct += n.dur; else { nct += n.dur; nctByChord[b.chord] = (nctByChord[b.chord] || 0) + n.dur; }
}
// harmony stems: which diatonic shift keeps the most chord tones over her chords
const SCALE = [8, 10, 11, 1, 3, 4, 6]; // G# natural minor pcs
const shiftDeg = (midi, k) => { const pc = midi % 12; let i = SCALE.indexOf(pc); if (i < 0) return null; let j = i + k, oct = 0; while (j < 0) { j += 7; oct--; } while (j >= 7) { j -= 7; oct++; } return midi - pc + SCALE[j] + 12 * oct; };
const harmonyFit = Object.fromEntries([2, -2, 4, -5, -7].map((k) => {
  let hit = 0, tot = 0;
  for (const n of notes) { const b = barOf(n.t); if (!b) continue; const m = shiftDeg(n.midi, k); if (m == null) continue; tot += n.dur; if (CHORD_PCS[b.chord].includes(((m % 12) + 12) % 12)) hit += n.dur; }
  return [`shift${k > 0 ? "+" : ""}${k}`, r3(hit / tot)];
}));
out.melody = {
  notes: notes.length, tonic: "G#3 = midi 56 (guitar frame)",
  range: { lo: Math.min(...notes.map((n) => n.midi)), hi: Math.max(...notes.map((n) => n.midi)) },
  degreeShareByDuration: degShare,
  motion: { steps, leaps, repeats, stepShare: r3(steps / (steps + leaps + repeats)), intervalHist: intHist },
  phrases: phraseRows,
  phraseBeats: { median: median(phraseRows.map((p) => p.beats)), p90: pct(phraseRows.map((p) => p.beats), 0.9) },
  cadenceDegrees: phraseRows.reduce((o, p) => ((o[p.degreeLast] = (o[p.degreeLast] || 0) + 1), o), {}),
  registerBySection,
  topDegree4grams: topGrams,
  phrasesBySection: phraseRows.reduce((o, p) => ((o[p.section] = (o[p.section] || 0) + 1), o), {}),
  chordToneShare: r3(ct / (ct + nct)), nonChordToneByChord: Object.fromEntries(Object.entries(nctByChord).map(([c, v]) => [c, r1(v)])),
  harmonyStemChordToneShare: harmonyFit,
};

// ── 6. FORM · proportions against chamber-01 R1/R5/R6/R12 and pop-strategies ─
const secRows = SECTIONS.map(([s, a, b]) => {
  const rows = bars.filter((x) => x.n >= a && x.n <= b);
  const start = rows[0].t, end = rows[rows.length - 1].t + rows[rows.length - 1].dur;
  return { section: s, bars: rows.length, from: a, to: b, start: r1(start), end: r1(end), frac: r3(start / DUR) };
});
// loudness envelope per bar from the stems (dB RMS), for where HER climax is
function rmsPerBar(path) {
  if (!existsSync(path)) return null;
  const { samples: x, sampleRate: sr } = readWavMono(path);
  return bars.map((b) => { const a = Math.floor(b.t * sr), z = Math.min(x.length, Math.floor((b.t + b.dur) * sr)); let s = 0; for (let i = a; i < z; i++) s += x[i] * x[i]; return r1(10 * Math.log10(s / Math.max(1, z - a) + 1e-12)); });
}
const voxRms = rmsPerBar(resolve(LANE, "src/vox/vocals-dry-48k.wav"));
const gtrRms = rmsPerBar(resolve(LANE, "src/vox/guitar-48k.wav"));
const loudest = voxRms ? voxRms.map((v, i) => [v, i + 1]).sort((p, q) => q[0] - p[0]).slice(0, 8).map(([v, n]) => ({ bar: n, section: sectionOfBar(n), t: r1(bars[n - 1].t), dB: v })) : null;
const voxBySection = voxRms ? Object.fromEntries(SECTIONS.map(([s, a, b]) => [s, r1(mean(voxRms.slice(a - 1, b)))])) : null;
const gtrBySection = gtrRms ? Object.fromEntries(SECTIONS.map(([s, a, b]) => [s, r1(mean(gtrRms.slice(a - 1, b)))])) : null;
const climaxBar = loudest ? loudest[0].bar : null;
const climaxSpan = loudest ? (() => { // longest run of bars within 3 dB of the peak
  const peak = loudest[0].dB; let best = [0, 0], cur = null;
  voxRms.forEach((v, i) => { if (v >= peak - 3) { if (!cur) cur = [i + 1, i + 1]; else cur[1] = i + 1; if (cur[1] - cur[0] >= best[1] - best[0]) best = [...cur]; } else cur = null; });
  return best;
})() : null;
out.form = {
  durationS: DUR, bars: bars.length,
  sections: secRows,
  phraseLengthCheck: secRows.map((s) => ({ section: s.section, bars: s.bars, inR1Set: [8, 12, 16, 24].includes(s.bars) })),
  rules: {
    R5climaxAt: { frac: 0.71, t: r1(0.71 * DUR), bar: barOf(0.71 * DUR)?.n, section: sectionOfBar(barOf(0.71 * DUR)?.n) },
    R6stillnessAt: { frac: 0.36, t: r1(0.36 * DUR), bar: barOf(0.36 * DUR)?.n, section: sectionOfBar(barOf(0.36 * DUR)?.n) },
    popBuildLayerEntries: [0.10, 0.28, 0.40, 0.62, 0.78].map((f) => ({ frac: f, bar: Math.round(1 + f * (bars.length - 1)), section: sectionOfBar(Math.round(1 + f * (bars.length - 1))) })),
  },
  measured: {
    voxDbBySection: voxBySection, gtrDbBySection: gtrBySection,
    loudestVoxBars: loudest,
    herClimax: climaxBar ? { bar: climaxBar, t: r1(bars[climaxBar - 1].t), frac: r3(bars[climaxBar - 1].t / DUR), section: sectionOfBar(climaxBar), plateauBars: climaxSpan } : null,
    voxRmsPerBar: voxRms, gtrRmsPerBar: gtrRms,
  },
};

// ── 7. THE CUT · pop-strategies: tracks run ~1:30; remove repeated statements ─
// Candidate edit: seams on downbeats (10 ms fades). Bars dropped: intro 1–6
// (keep 4), verse2 44–51 (a restatement), chorus2 52–67 (a restatement).
const keep = [[7, 43], [68, 84]];
const cutLen = keep.reduce((s, [a, b]) => s + bars.slice(a - 1, b).reduce((t, x) => t + x.dur, 0), 0);
out.cut = {
  rule: "chamber-04 form facts: tracks run about 1:30; wannadash went 3:28 → 1:54 by removing repeated statements, seams on downbeats with 10 ms fades",
  keepBars: keep, dropBars: [[1, 6], [44, 67]],
  lengthS: r1(cutLen), lengthMMSS: `${Math.floor(cutLen / 60)}:${String(Math.round(cutLen % 60)).padStart(2, "0")}`,
  seams: keep.slice(1).map(([a]) => ({ bar: a, t: bars[a - 1].t, chord: bars[a - 1].chord, prevChord: bars[keep[0][1] - 1].chord })),
  note: "verse2 + chorus2 restate verse1 + chorus1 on the same chords; the break (68–71) then lands right after chorus1 — check the lyric carries",
};

// ── 8. BASS · register, tempo band, delay in beats (bass-01 rules) ────────────
const hz = (m) => 440 * 2 ** ((m - 69) / 12) * 2 ** (A.tuningCents / 1200);
out.bass = {
  tempoBand: bpmMed >= 110 && bpmMed <= 125 ? "dub techno 110–125 (R12)" : "outside the platter's bands",
  subRoots: { "G#1 (midi 32)": r1(hz(32)), "B1 (midi 35)": r1(hz(35)), "E1 (midi 28)": r1(hz(28)), "E2 (midi 40)": r1(hz(40)) },
  R1: "sub 30–60 Hz octave (C1–C2), mono, sine plus 2nd/3rd harmonic; B1 at 62 Hz sits at the top of the band — E1 and G#1 are the sub's home",
  R3: `kit is kick 1 / snare 3 at ${r1(bpmMed)}: the dubstep half-time layout; a one-drop variant leaves beat 1 empty and puts kick + cross-stick on 3`,
  R4: "first bass entry late: at least one 8-bar phrase of drums and space before it (her intro is 10 bars — the sub can wait until bar 11)",
  R5: { delayMs: r1(0.93 * beatMs), repeats: "4–5", note: "0.93 of a beat, so it revolves around her beat rather than locking; in follow mode compute it per bar from measures.json" },
  R8: "duck the sub 2–3 dB per kick, fastest attack; her guitar's low G#2 (104 Hz) and B2 (124 Hz) share the kick body band 100–200 Hz — high-pass the guitar or thin the kick body",
  R9: "mono below 100 Hz; stereo only on returns and on the sines above 100 Hz",
  R10: "a harmonic copy of the sub high-passed at 150 Hz carrying 250–700 Hz so the note survives a phone",
  phoneBand: "MASTERING.md: loudest 8 s may lose ≤ 5 dB through the mono 180 Hz–8 kHz proxy",
};

writeFileSync(resolve(LANE, "platter-analysis.json"), JSON.stringify(out, null, 1));

// ── stdout digest ─────────────────────────────────────────────────────────────
const P = (s) => process.stdout.write(s + "\n");
P(`RHYTHM  motif ${motif} IOI (${an.ioi}) k=${an.k} n=${an.n}  euclidean=${an.euclidean}  periodic=${N.isPeriodic(motif)}  evenness=${r3(an.evenness)}  balanced=${an.balance.balanced}`);
P(`        syncopation ${JSON.stringify(N.syncopationProfile(motif))}`);
for (const [k, v] of Object.entries(distances)) P(`        → ${k.padEnd(32)} ${v.box}  chronotonic ${v.chronotonic}${v.swap != null ? `  swap ${v.swap}` : ""}`);
P(`        morph to straight 8ths: ${out.rhythm.morphToStraight.join(" → ")}`);
P(`        bars verbatim on the motif: ${patternCounts[motif]}/${motifBars.length}; drift by section: ${Object.entries(driftBySection).map(([s, v]) => `${s} ${v.meanChronotonic}`).join(", ")}`);
P(`BINS    off-beat strum phase median ${out.beatBins.offbeatPhaseMedian} (p10 ${out.beatBins.offbeatPhaseP10}, p90 ${out.beatBins.offbeatPhaseP90})  swing ${out.beatBins.swingRatio}:1  & is ${out.beatBins.offbeatLateMsAt120} ms late at 120`);
P(`        voice vs her 8th grid: signed median ${out.beatBins.voiceVsGrid.all.signedMedianMs} ms (abs ${out.beatBins.voiceVsGrid.all.absMedianMs}); by section ${Object.entries(voiceBySection).map(([k, v]) => `${k} ${v.signedMedianMs > 0 ? '+' : ''}${v.signedMedianMs}`).join(', ')}; long notes ${out.beatBins.voiceVsGrid.longNotesSignedMs}, short notes ${out.beatBins.voiceVsGrid.shortNotesSignedMs}`);
P(`ENTRAIN quarter ${out.entrainment.periodsMs.quarter} ms (sweet spot 500–700: ${out.entrainment.felt.quarterMsFromSweetSpot} ms off), bar ${out.entrainment.periodsMs.bar} ms (at ceiling ${out.entrainment.felt.barAtCeiling}), 2-bar chord cycle ${out.entrainment.periodsMs.chordCycle2bars} ms felt as rhythm: ${out.entrainment.felt.chordCycleFelt}`);
P(`        bpm by section: ${Object.entries(bpmBySection).map(([s, v]) => `${s} ${v.median}`).join(", ")}`);
P(`HARMONY bars ${JSON.stringify(out.harmony.barCounts)}  harmonic rhythm ${out.harmony.harmonicRhythmBeats} beats  run lengths ${JSON.stringify(out.harmony.runLengths)}`);
P(`        IV (Emaj7) bars: ${out.harmony.ivBars.join(" ")}`);
P(`MELODY  ${notes.length} notes ${out.melody.range.lo}–${out.melody.range.hi}  degrees by duration ${JSON.stringify(degShare)}`);
P(`        motion steps ${steps} leaps ${leaps} repeats ${repeats} (step share ${out.melody.motion.stepShare})  phrases ${phraseRows.length}, median ${out.melody.phraseBeats.median} beats  cadences ${JSON.stringify(out.melody.cadenceDegrees)}`);
P(`        phrase starts by beat-in-bar ${JSON.stringify(phraseRows.reduce((o, p) => ((o[p.beatIn] = (o[p.beatIn] || 0) + 1), o), {}))}  start offset ms median ${median(phraseRows.map((p) => p.offsetMs))}  contours ${JSON.stringify(phraseRows.reduce((o, p) => ((o[p.contour] = (o[p.contour] || 0) + 1), o), {}))}`);
for (const p of phraseRows) P(`          ${String(p.start).padStart(6)}s bar ${String(p.bar).padStart(2)} beat ${p.beatIn} ${String(p.offsetMs).padStart(4)}ms ${p.section.padEnd(7)} ${String(p.beats).padStart(4)} beats ${String(p.notes).padStart(2)} notes ${p.lo}–${p.hi} ${p.contour.padEnd(5)} ends on ${p.degreeLast}`);
P(`        register by section: ${Object.entries(registerBySection).filter(([, v]) => v).map(([s, v]) => `${s} ${v.medianMidi} (${v.lo}–${v.hi})`).join(", ")}`);
P(`        hook 4-grams ${JSON.stringify(topGrams)}  phrases by section ${JSON.stringify(out.melody.phrasesBySection)}`);
P(`        chord-tone share ${out.melody.chordToneShare}  non-chord-tone seconds by chord ${JSON.stringify(out.melody.nonChordToneByChord)}  harmony stems chord-tone share ${JSON.stringify(harmonyFit)}`);
P(`FORM    ${secRows.map((s) => `${s.section} ${s.bars}b @${s.start}s (${s.frac})`).join(" · ")}`);
P(`        R5 climax at 0.71 → ${out.form.rules.R5climaxAt.t}s bar ${out.form.rules.R5climaxAt.bar} (${out.form.rules.R5climaxAt.section});  R6 stillness at 0.36 → ${out.form.rules.R6stillnessAt.t}s bar ${out.form.rules.R6stillnessAt.bar} (${out.form.rules.R6stillnessAt.section})`);
if (out.form.measured.herClimax) P(`        her loudest bar ${out.form.measured.herClimax.bar} (${out.form.measured.herClimax.section}) at ${out.form.measured.herClimax.t}s = ${out.form.measured.herClimax.frac}; plateau bars ${out.form.measured.herClimax.plateauBars}`);
if (voxBySection) P(`        vox dB by section ${JSON.stringify(voxBySection)}`);
if (gtrBySection) P(`        gtr dB by section ${JSON.stringify(gtrBySection)}`);
P(`CUT     keep ${JSON.stringify(keep)} → ${out.cut.lengthMMSS}; seam at bar ${out.cut.seams[0].bar} (${out.cut.seams[0].prevChord} → ${out.cut.seams[0].chord})`);
P(`BASS    ${out.bass.tempoBand}; sub roots ${JSON.stringify(out.bass.subRoots)}; delay ${out.bass.R5.delayMs} ms`);
P(`✓ ${resolve(LANE, "platter-analysis.json")}`);
