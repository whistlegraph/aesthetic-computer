#!/usr/bin/env node
// bake.mjs — prepare everything momabobasheep.c needs (the house pattern:
// JS composes + bakes, C renders). The COMPOSITION stays in JS — this
// script never re-walks the score. It:
//
//   1. runs bin/score-extract.mjs (the verbatim composition twin of the
//      renderer) so out/momabobasheep.events.json is fresh, then reads it;
//   2. replicates ONLY the render-side jeffrey-choir fan-out (the staged
//      deb()/wind gates, the ei%3 breathing skip, the ei%5/%7 octave
//      stretches, the melody shadow that follows the whistle) and emits a
//      FLAT list of voice render instructions — so the C engine is a dumb,
//      fast DSP executor with zero composition logic. The jeffrey entry
//      plan itself is re-derived on its own seeded rng stream (independent
//      of the main walk, bit-identical by construction) because
//      events.json drops the pan draw; we ASSERT against events.json so a
//      drift in either copy fails the bake instead of shipping a wrong mix;
//   3. pre-processes the locked vocal takes the exact way the renderer
//      does — voiced-core trim (pitchTrack) + 16 s loop-crossfade sustain —
//      and writes them as raw float32 mono files into c/vocals/, so the C
//      engine just freads floats (like americomputadora's c/vocals/);
//   4. writes score.h: every event table, the chord-bar drone walk, the
//      bass onsets for the sidechain duck, and the smooth level-arc spans.
//
// usage: node pop/momboba/c/bake.mjs

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { readWavMono } from "../../lib/wav.mjs";
import { pitchTrack } from "../../lib/analysis.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));   // pop/momboba/c
const ROOT = dirname(HERE);                             // pop/momboba
const REPO = resolve(ROOT, "../..");
const SR = 44_100;                                      // engine render rate

// ── 1) the composed walk — always re-extracted, never re-implemented ────
{
  const r = spawnSync("node", [join(ROOT, "bin", "score-extract.mjs")], { stdio: "inherit" });
  if (r.status !== 0) { console.error("✗ score-extract failed"); process.exit(1); }
}
const ev = JSON.parse(readFileSync(join(ROOT, "out", "momabobasheep.events.json"), "utf8"));
const BAR = ev.bar;

// rebuild the chord timeline (ci/tr/mi/k) from the extract's chord list —
// `base` is the untransposed chord name, `k` the bar index inside its
// movement (the choir's fade-in ramp needs it).
const CHORD_NAMES = ["F", "Dm", "Bb", "C", "Gm", "Eb"];
const TRIAD = [[5, 9, 0], [2, 5, 9], [10, 2, 5], [0, 4, 7], [7, 10, 2], [3, 7, 10]];
// drone/bass per base chord (mirrors the renderer's CHORDS table — data, not logic)
const CHORD_DRONE = [[29, 41, 48, 53, 57], [26, 38, 45, 50, 53], [22, 34, 41, 46, 50], [24, 36, 43, 48, 52], [31, 43, 50, 55, 58], [27, 39, 46, 51, 55]];
const CHORD_BASS = [41, 38, 34, 36, 43, 39];
const chordTimeline = [];
{
  let prevMi = -1, k = 0;
  for (const c of ev.chords) {
    if (c.mi !== prevMi) { k = 0; prevMi = c.mi; }
    chordTimeline.push({ t: c.t, ci: CHORD_NAMES.indexOf(c.base), tr: c.tr, mi: c.mi, k });
    k++;
  }
}
const chordAt = (t) => {
  let c = chordTimeline[0];
  for (const e of chordTimeline) { if (e.t <= t) c = e; else break; }
  return c;
};

// smooth chapter-level curve — same interpolation the renderer bakes in
const lvSpan = [];
{
  let accBar = 0;
  for (const m of ev.movements) { lvSpan.push({ c: (accBar + m.bars / 2) * BAR, lv: m.level }); accBar += m.bars; }
}
function levelSmoothAt(t) {
  if (t <= lvSpan[0].c) return lvSpan[0].lv;
  if (t >= lvSpan[lvSpan.length - 1].c) return lvSpan[lvSpan.length - 1].lv;
  for (let i = 0; i < lvSpan.length - 1; i++) {
    if (t >= lvSpan[i].c && t <= lvSpan[i + 1].c) {
      const f = (t - lvSpan[i].c) / (lvSpan[i + 1].c - lvSpan[i].c);
      return lvSpan[i].lv + (lvSpan[i + 1].lv - lvSpan[i].lv) * f;
    }
  }
  return 1;
}

// ── 2a) jeffrey entry plan — its OWN rng stream, replicated rng-exactly ──
// (events.json mirrors this but drops the pan draw; we need pan, and we
// assert against the extract below so the two copies can never drift.)
function seedFrom(str) {
  let h = 0x811c9dc5 >>> 0;
  for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 0x01000193) >>> 0; }
  return h >>> 0;
}
const jeffreyEvents = [];
{
  let _rj = seedFrom("jeffrey-harmonies");
  const rndJ = () => { _rj = (Math.imul(_rj, 1664525) + 1013904223) >>> 0; return _rj / 0xffffffff; };
  const CENTER = 47.2;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    const mv = ev.movements[mi];
    if (rndJ() >= 0.35 + mv.level * 0.45) continue;
    const tj = t + rndJ() * 0.8;
    const cands = TRIAD[ci].flatMap((pc) => [pc + 36 + tr, pc + 48 + tr])
      .filter((m) => m >= 41 && m <= 54);
    let midi = cands[0];
    for (const m of cands) if (Math.abs(m - CENTER) < Math.abs(midi - CENTER)) midi = m;
    const dur = BAR * (1.6 + rndJ() * 1.1);
    const pan = -0.35 + rndJ() * 0.7;
    const lvl = levelSmoothAt(tj);
    jeffreyEvents.push({ t: tj, dur, midi, g: 0.08 + lvl * 0.065, p: pan });
    if (mv.alto && rndJ() < 0.5) {
      const ups = cands.filter((m) => m > midi + 2);
      if (ups.length) jeffreyEvents.push({ t: tj + 0.4, dur: dur * 0.9, midi: ups[0], g: 0.06 + lvl * 0.05, p: -pan });
    }
  }
}
// parity gate: the replicated stream must equal the extract's dump
{
  if (jeffreyEvents.length !== ev.jeffrey.length) {
    console.error(`✗ jeffrey plan drifted: ${jeffreyEvents.length} vs extract ${ev.jeffrey.length}`);
    process.exit(1);
  }
  for (let i = 0; i < jeffreyEvents.length; i++) {
    const a = jeffreyEvents[i], b = ev.jeffrey[i];
    if (Math.abs(a.t - b.t) > 0.002 || a.midi !== b.midi || Math.abs(a.g - b.g) > 0.001) {
      console.error(`✗ jeffrey plan drifted at entry ${i}: ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);
      process.exit(1);
    }
  }
  console.log(`✓ jeffrey plan matches score-extract (${jeffreyEvents.length} entries)`);
}

// ── 3) vocal takes → loopable sustains → raw float32 files ──────────────
// Verbatim math from the renderer: trim to the longest contiguous VOICED
// run (pitchTrack; key clicks are unvoiced → cut), loop-crossfade the core
// into a 16 s sustain, soft-fade the head. Written raw so the C engine
// just freads floats — no WAV parsing for the choir.
const SUS_SEC = 16;
const loadSustain = (path) => {
  const { samples, sampleRate } = readWavMono(resolve(REPO, path));
  const frames = pitchTrack(samples, { sampleRate, fmin: 60, fmax: 500 });
  let best = [0, 0], cur = null;
  frames.forEach((r, i) => {
    if (r.hz != null && r.clarity > 0.55) {
      if (!cur) cur = [i, i]; else cur[1] = i;
      if (cur[1] - cur[0] > best[1] - best[0]) best = [...cur];
    } else cur = null;
  });
  const hop = Math.floor(0.010 * sampleRate);
  const s0 = Math.max(0, best[0] * hop - Math.floor(0.06 * sampleRate));
  const s1 = Math.min(samples.length, best[1] * hop + Math.floor(0.10 * sampleRate));
  const seg = Float32Array.from(samples.subarray(s0, s1));
  const xf = Math.floor(0.40 * sampleRate);                 // loop crossfade
  const ls = Math.floor(seg.length * 0.25), le = Math.floor(seg.length * 0.82);
  const total = Math.floor(SUS_SEC * sampleRate);
  const sus = new Float32Array(total);
  sus.set(seg.subarray(0, Math.min(le, total)), 0);
  let w = Math.min(le, total);
  while (w < total && le - ls > xf * 2) {
    for (let i = 0; i < xf; i++) {                           // equal-power seam
      const idx = w - xf + i; if (idx < 0 || idx >= total) continue;
      const fa = Math.cos((i / xf) * Math.PI / 2), fb = Math.sin((i / xf) * Math.PI / 2);
      sus[idx] = sus[idx] * fa + seg[ls + i] * fb;
    }
    const chunk = Math.min(le - ls - xf, total - w);
    sus.set(seg.subarray(ls + xf, ls + xf + chunk), w);
    w += chunk;
  }
  const fade = Math.floor(0.10 * sampleRate);                // soft head edge
  for (let i = 0; i < fade; i++) sus[i] *= 0.5 - 0.5 * Math.cos(Math.PI * i / fade);
  return { sus, sr: sampleRate };
};

const manifest = JSON.parse(readFileSync(join(ROOT, "voice", "manifest.json"), "utf8"));
const vocDir = join(HERE, "vocals");
mkdirSync(vocDir, { recursive: true });
const takes = [];
console.log("# baking vocal sustains → c/vocals/*.f32");
for (const m of manifest) {
  const vib = loadSustain(m.file);
  const flat = m.flatFile ? loadSustain(m.flatFile) : vib;
  const vName = `${m.vowel}.f32`, fName = `${m.vowel}-flat.f32`;
  writeFileSync(join(vocDir, vName), Buffer.from(vib.sus.buffer));
  writeFileSync(join(vocDir, fName), Buffer.from(flat.sus.buffer));
  takes.push({ vowel: m.vowel, midi: m.midi, sr: vib.sr, vibN: vib.sus.length, flatN: flat.sus.length, vName, fName });
  console.log(`  ✓ ${m.vowel} (${vib.sus.length} + ${flat.sus.length} samples @ ${vib.sr})`);
}

// ── 2b) the choir fan-out — VOICE-level render instructions ─────────────
// The renderer's staged-introduction logic, run here once; the C engine
// just executes the flat list. Pitch is pre-baked as playback RATES
// (target/glide vs the take's locked midi, × the 48k→44.1k resample).
const nearestTakeIdx = (target) => {
  let ti = 0, bdd = 1e9;
  takes.forEach((tk, i) => { const d = Math.abs(tk.midi - target); if (d < bdd) { bdd = d; ti = i; } });
  return ti;
};
const voiceEvs = [];
const pushVoice = (ti, target, t0, dur, gain, pan, fc, stagger, opts = {}) => {
  const tk = takes[ti];
  const rate = Math.pow(2, (target - tk.midi) / 12) * (tk.sr / SR);
  const rate0 = opts.glideFrom != null
    ? Math.pow(2, (opts.glideFrom - tk.midi) / 12) * (tk.sr / SR) : rate;
  voiceEvs.push({ take: ti, flat: opts.flat ? 1 : 0, rate, rate0, t: t0, dur, gain, pan, fc, stagger });
};
let placed = 0;
jeffreyEvents.forEach((e, ei) => {
  const { ci, tr, mi, k } = chordAt(e.t);
  const deb = (m0) => mi < m0 ? 0 : mi === m0 ? Math.min(1, (k + 1) / 5) : 1;
  const wind = mi === 8 ? 0 : 1;              // resolve sheds the extras
  if (deb(1) === 0) return;                   // dusk: no choir at all
  if (ei % 3 === 2) return;                   // breathe — skip a third
  const dur = e.dur * 1.1;
  const pcs = TRIAD[ci];
  let tenor = e.midi;
  if (tenor - takes[nearestTakeIdx(tenor)].midi > 3) tenor -= 12;
  pushVoice(nearestTakeIdx(tenor), tenor, e.t, dur, e.g * 0.55 * deb(1), e.p, 2400, 0);
  if (deb(2) > 0) {
    pushVoice(nearestTakeIdx(tenor), tenor + 0.13, e.t, dur * 0.96, e.g * 0.32 * deb(2),
      Math.max(-0.9, Math.min(0.9, e.p + 0.5)), 2000, 0.22);
  }
  if (deb(3) > 0) {
    let bRoot = pcs[0] + 36 + tr - 12;
    if (bRoot < 34) bRoot += 12;
    pushVoice(nearestTakeIdx(bRoot + 12), bRoot, e.t, dur, e.g * 0.38 * deb(3), -e.p * 0.5, 750, 0.35 + (ei % 3) * 0.1, { flat: true });
  }
  if (ei % 2 === 0 && deb(4) > 0 && wind) {   // upper voice, close position
    const ups = pcs.flatMap((pc) => [pc + 48 + tr, pc + 60 + tr])
      .filter((m) => m > tenor + 2 && m <= Math.min(tenor + 9, 53)).sort((x, y) => x - y);
    if (ups.length) pushVoice(nearestTakeIdx(ups[0]), ups[0], e.t, dur * 0.9, e.g * 0.30 * deb(4), -e.p, 1700, 0.5);
  }
  if (ei % 5 === 1 && deb(6) > 0 && wind) {   // 2-oct-down taffy ghost
    const deep = tenor - 24;
    if (deep >= 29) pushVoice(nearestTakeIdx(tenor), deep, e.t, dur * 1.3, e.g * 0.40 * deb(6), e.p * 0.3, 500, 0.6, { flat: true });
  }
  if (ei % 7 === 3 && deb(6) > 0 && wind) {   // up-octave shimmer
    const high = tenor + 12;
    if (high <= 60) pushVoice(nearestTakeIdx(tenor), high, e.t, dur * 0.8, e.g * 0.20 * deb(6), -e.p * 0.8, 2800, 0.8);
  }
  placed++;
});
// melody shadow — vocals glide along the whistle line through the dreams
let shadows = 0;
{
  let prevMidi = null;
  ev.whistle.forEach((e, wi) => {
    if (wi % 4 !== 0) return;
    const smi = chordAt(e.t).mi;
    if (smi < 4 || smi > 7) return;
    const lvl = levelSmoothAt(e.t);
    if (lvl < 0.40) return;
    let m = e.midi;
    while (m > 53) m -= 12;
    while (m < 41) m += 12;
    pushVoice(nearestTakeIdx(m), m, e.t, Math.max(e.dur * 2.2, 3.0), 0.05 + lvl * 0.05,
      0.22, 2200, 0.05, { glideFrom: prevMidi ?? m });
    prevMidi = m;
    shadows++;
  });
}
console.log(`✓ choir fan-out: ${placed} entries → ${voiceEvs.length - shadows} voices + ${shadows} melody shadows`);

// ── whistle pan — the renderer pans the harmony-third (gain 0.16 exactly)
// at 0.26 and the main line at 0.14; the extract drops pan, so re-derive.
const whistleEvs = ev.whistle.map((e) => ({ ...e, pan: e.g === 0.16 ? 0.26 : 0.14 }));

// ── drone walk per bar (lift + transpose pre-applied, like the renderer) ─
const lift = (m) => (m < 26 ? m + 12 : m);
const bars = chordTimeline.map((c) => ({
  t: c.t,
  subMidi: CHORD_BASS[c.ci] + c.tr,
  droneMidi: CHORD_DRONE[c.ci].map((m) => lift(m + c.tr)),
}));

// ── bass onsets for the choir sidechain duck ─────────────────────────────
const bassOnsets = ev.bass.map((e) => e.t).sort((a, b) => a - b);

// ── score.h ──────────────────────────────────────────────────────────────
const num = (x) => (Number.isInteger(x) ? `${x}.0` : String(x));
const mev = (list) => list.map((e) => `  {${num(e.t)}, ${num(e.dur)}, ${num(e.midi)}, ${num(e.g)}},`).join("\n");
const h = `// score.h — generated by c/bake.mjs from out/momabobasheep.events.json
// (the verbatim composition twin of bin/render-momabobasheep.mjs).
// Do not hand-edit — the composition lives in JS.
#define RENDER_SEC ${ev.renderSec}
#define MASTER_SEC ${ev.masterSec}
#define BAR_SEC ${num(BAR)}

typedef struct { double t, dur, midi, gain; } MEv;          // marimba note
typedef struct { double t, dur, midi, gain, pan; } WEv;     // whistle note
typedef struct { double t, dur, gain, pan; double midi[3]; } BellEv;
typedef struct { double t, subMidi; double droneMidi[5]; } Bar;
typedef struct { int take, flat; double rate, rate0, t, dur, gain, pan, fc, stagger; } VoiceEv;

#define N_BASS ${ev.bass.length}
static const MEv EV_BASS[N_BASS] = {
${mev(ev.bass)}
};
#define N_PAD ${ev.pad.length}
static const MEv EV_PAD[N_PAD] = {
${mev(ev.pad)}
};
#define N_LEAD ${ev.arp.length}
static const MEv EV_LEAD[N_LEAD] = {
${mev(ev.arp)}
};
#define N_ALTO ${ev.alto.length}
static const MEv EV_ALTO[N_ALTO] = {
${mev(ev.alto)}
};
#define N_SPARK ${ev.sparkle.length}
static const MEv EV_SPARK[N_SPARK] = {
${mev(ev.sparkle)}
};

#define N_WHISTLE ${whistleEvs.length}
static const WEv EV_WHISTLE[N_WHISTLE] = {
${whistleEvs.map((e) => `  {${num(e.t)}, ${num(e.dur)}, ${num(e.midi)}, ${num(e.g)}, ${num(e.pan)}},`).join("\n")}
};

#define N_BELL ${ev.bells.length}
static const BellEv EV_BELL[N_BELL] = {
${ev.bells.map((e) => `  {${num(e.t)}, ${num(e.dur)}, ${num(e.gain)}, ${num(e.p)}, {${e.midis.map(num).join(", ")}}},`).join("\n")}
};

#define N_BARS ${bars.length}
static const Bar BARS[N_BARS] = {
${bars.map((b) => `  {${num(b.t)}, ${num(b.subMidi)}, {${b.droneMidi.map(num).join(", ")}}},`).join("\n")}
};

#define N_ONSETS ${bassOnsets.length}
static const double BASS_ONSETS[N_ONSETS] = {
  ${bassOnsets.map(num).join(", ")}
};

// smooth chapter-level curve (movement centres) — the dynamic arc
#define N_LV ${lvSpan.length}
static const double LV_C[N_LV] = {${lvSpan.map((s) => num(s.c)).join(", ")}};
static const double LV_V[N_LV] = {${lvSpan.map((s) => num(s.lv)).join(", ")}};

// jeffrey choir — flat voice render instructions (fan-out baked in JS).
// rate/rate0 carry both the pitch shift and the ${takes[0].sr}→${SR} resample.
#define N_VOICE ${voiceEvs.length}
static const VoiceEv EV_VOICE[N_VOICE] = {
${voiceEvs.map((v) => `  {${v.take}, ${v.flat}, ${num(v.rate)}, ${num(v.rate0)}, ${num(v.t)}, ${num(v.dur)}, ${num(v.gain)}, ${num(v.pan)}, ${num(v.fc)}, ${num(v.stagger)}},`).join("\n")}
};

// baked sustains (raw float32 mono @ ${takes[0].sr} Hz) in c/vocals/
#define N_TAKES ${takes.length}
static const char *TAKE_FILE[N_TAKES][2] = {  // [take][0]=vibrato [1]=flat
${takes.map((t) => `  {"${t.vName}", "${t.fName}"},`).join("\n")}
};
static const long TAKE_LEN[N_TAKES][2] = {
${takes.map((t) => `  {${t.vibN}, ${t.flatN}},`).join("\n")}
};
`;
writeFileSync(join(HERE, "score.h"), h);
console.log(`✓ c/score.h (${ev.arp.length} arp / ${ev.bass.length} bass / ${ev.pad.length} pad / ` +
  `${ev.alto.length} alto / ${ev.sparkle.length} sparkle / ${whistleEvs.length} whistle / ` +
  `${ev.bells.length} bells / ${voiceEvs.length} choir voices / ${bars.length} bars)`);
