#!/usr/bin/env node
// render-bed.mjs — pop/blackboard's instrument bed, synthesized bottom-up.
//
// Everything here is AC-grown: sine-bump bass and detuned-sine pads in the
// pop/cult mixing school (no master tanh, raised-cosine tails, sidechain as a
// breath not a pump), a patient two-stage kick, chalk TAPS (band-passed noise
// ticks — chalk dotting a board), and chalk SCRAPES — a port of Menu Band's
// TrackDrum "Continuous membrane friction" voice, taken via pop/cult/bin/
// render5.mjs (see pop/cult/reference/trackdrum-friction-reference.swift):
// two one-pole low-passes over the SAME white sample whose difference is the
// friction band, a sine carrier FM'd by the band itself (tanh(band·8)·0.055),
// tanh grip, and separate attack/release one-poles so a gesture swells like a
// real drag instead of gating on. Chalk on slate is friction; this voice IS
// friction physics, so it is the right instrument for "you scrape a line".
//
// Out: out/blackboard-bed.wav (stereo f32 48k) — the instruments stem.
// Run:  node pop/blackboard/bin/render-bed.mjs

import { writeFileSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { BPM, SPB, BAR, BARS, DURATION_S, SR, bt, CHORDS, SECTIONS, HOOK_MELODY }
  from "./score.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
mkdirSync(OUT, { recursive: true });

const N = Math.ceil(DURATION_S * SR);
const TAU = Math.PI * 2;
const hz = (m) => 440 * Math.pow(2, (m - 69) / 12);
const clamp = (x, a, b) => Math.min(b, Math.max(a, x));
const smooth = (x) => x * x * (3 - 2 * x);

// deterministic noise (LCG) — the render is reproducible
let seed = 20260814;
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const nrnd = () => rnd() * 2 - 1;

// buses — music ducks under the kick; drums never duck
const musL = new Float64Array(N), musR = new Float64Array(N);
const drmL = new Float64Array(N), drmR = new Float64Array(N);
const kickTimes = [];   // for the sidechain envelope

// equal-power pan into a bus
function emit(busL, busR, i, v, pan = 0) {
  if (i < 0 || i >= N) return;
  const a = (pan + 1) * Math.PI / 4;
  busL[i] += v * Math.cos(a);
  busR[i] += v * Math.sin(a);
}

const sectionAt = (bar) => SECTIONS.find((s) => bar >= s.bar0 && bar < s.bar1)?.name;
const isHook = (bar) => /^hook/.test(sectionAt(bar) ?? "");

// ── section gains (the arrangement moves by texture, not volume walls) ─────
const PAD_G  = { intro: 0.34, verse: 0.42, pre: 0.5, hook1: 0.62, bridge: 0.36,
                 hook2: 0.62, outcome: 0.5, hook3: 0.66, outro: 0.34 };
const BASS_G = { intro: 0, verse: 0.5, pre: 0.55, hook1: 0.72, bridge: 0.4,
                 hook2: 0.72, outcome: 0.55, hook3: 0.72, outro: 0.3 };
const KICK_G = { intro: 0, verse: 0.55, pre: 0.6, hook1: 0.85, bridge: 0.4,
                 hook2: 0.9, outcome: 0.6, hook3: 0.9, outro: 0.35 };
const DRONE_G = { intro: 1.0, verse: 0.22, pre: 0.22, hook1: 0.3, bridge: 0.75,
                  hook2: 0.3, outcome: 0.4, hook3: 0.3, outro: 1.0 };

// ── pads: detuned sine pairs on the chord tones, retriggered per chord ─────
function pad(t0, dur, tones, gain) {
  const n = Math.round(dur * SR), i0 = Math.round(t0 * SR);
  const atk = 0.55, rel = 0.9;
  for (const [ti, midi] of tones.entries()) {
    const f = hz(midi);
    const det = [1.0015, 0.9985];              // ±~2.6 cents, one per side
    const pans = [-0.35 + ti * 0.35, 0.35 - ti * 0.35];
    for (let v = 0; v < 2; v++) {
      let ph = rnd();
      const fd = f * det[v];
      for (let i = 0; i < n; i++) {
        const u = i / SR;
        let env = 1;
        if (u < atk) env = smooth(u / atk);
        const left = dur - u;
        if (left < rel) env *= smooth(clamp(left / rel, 0, 1));
        ph += fd / SR;
        // fundamental + a whisper of the 2nd — sine bumps, not twang
        const s = Math.sin(TAU * ph) + 0.13 * Math.sin(TAU * 2 * ph + 1.1);
        emit(musL, musR, i0 + i, s * env * gain * 0.052 / tones.length, pans[v]);
      }
    }
  }
  // soft root an octave under the tones (E2-region anchor inside the pad)
  const rf = hz(tones[0] - 12);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    let env = 1;
    if (u < atk) env = smooth(u / atk);
    const left = dur - u;
    if (left < rel) env *= smooth(clamp(left / rel, 0, 1));
    ph += rf / SR;
    emit(musL, musR, i0 + i, Math.sin(TAU * ph) * env * gain * 0.03, 0);
  }
}

// ── bass: fundamental + sub + a whisper of 2nd through a one-pole ──────────
function bassNote(t, midi, dur, gain) {
  const f = hz(midi);
  const n = Math.round((dur + 0.08) * SR), i0 = Math.round(t * SR);
  const lpA = 1 - Math.exp((-TAU * 300) / SR);
  let lp = 0, ph = 0, phS = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    let env = u < 0.02 ? u / 0.02 : Math.exp(-Math.max(0, u - 0.02) * 1.1);
    const left = dur + 0.08 - u;
    if (left < 0.06) env *= clamp(left / 0.06, 0, 1);
    ph += f / SR; phS += (f / 2) / SR;
    const s = Math.sin(TAU * ph) + 0.5 * Math.sin(TAU * phS) + 0.12 * Math.sin(TAU * 2 * ph);
    lp += lpA * (s - lp);
    emit(musL, musR, i0 + i, lp * env * gain * 0.20, 0);
  }
}

// ── kick: sweep + two-stage envelope + its own tanh + sub layer ────────────
function kick(t, gain) {
  kickTimes.push(t);
  const n = Math.round(0.42 * SR), i0 = Math.round(t * SR);
  let ph = 0, phSub = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 46 + 124 * Math.exp(-u * 52);         // 170 → 46 Hz inside ~25 ms
    ph += f / SR; phSub += 44 / SR;
    const env = 0.6 * Math.exp(-u * 30) + Math.exp(-u * 7);
    const body = Math.tanh(1.8 * Math.sin(TAU * ph)) * env;
    const sub = Math.sin(TAU * phSub) * Math.exp(-u * 9) * 0.35;
    const click = (Math.sin(TAU * 1650 * u) + 0.6 * Math.sin(TAU * 3900 * u))
      * Math.exp(-u * 420) * 0.10;
    const fade = Math.min(1, (n - i) / (0.01 * SR));
    emit(drmL, drmR, i0 + i, (body + sub + click) * gain * 0.5 * fade, 0);
  }
}

// ── chalk tap: band-passed noise tick (two-pole resonator) ─────────────────
function tap(t, { gain = 1, pan = 0, f = 3200 } = {}) {
  const n = Math.round(0.035 * SR), i0 = Math.round(t * SR);
  const r = 0.982, w = TAU * f / SR;
  const a1 = 2 * r * Math.cos(w), a2 = -r * r;
  let y1 = 0, y2 = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const x = nrnd() * Math.exp(-u / 0.004);
    const y = x * (1 - r) + a1 * y1 + a2 * y2;
    y2 = y1; y1 = y;
    const fade = Math.min(1, (n - i) / (0.004 * SR));
    emit(drmL, drmR, i0 + i, y * gain * 0.5 * fade, pan);
  }
}

// ── chalk scrape: the TrackDrum friction voice (via pop/cult render5) ──────
function friction(t, dur, {
  shape = "drag", gain = 1, pan = 0,
  cut0 = 1300, cut1 = null, res0 = 190, res1 = null,
  rough = 0.55, rel = 0.10,
} = {}) {
  const c1 = cut1 ?? cut0, r1 = res1 ?? res0;
  const n = Math.round((dur + rel + 0.06) * SR), i0 = Math.round(t * SR);
  const atkA = 1 - Math.exp(-1 / (SR * 0.0025));    // physical attack
  const relA = 1 - Math.exp(-1 / (SR * rel));
  let lvl = 0, nf = 0, ns = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR, x = u / dur;
    let target = 0;
    if (x < 1) {
      target = shape === "skid" ? Math.exp(-x * 4.2)
        : shape === "slide" ? Math.sin(Math.PI * x)
          : Math.pow(smooth(clamp(x / 0.92, 0, 1)), 1.6);      // drag
    }
    if (shape === "drag") target *= 0.62;   // it lands on a downbeat; stay under it
    lvl += (target > lvl ? atkA : relA) * (target - lvl);
    const k = clamp(x, 0, 1);
    const cut = cut0 + (c1 - cut0) * k;
    const res = res0 + (r1 - res0) * k;
    const fa = 1 - Math.exp((-TAU * cut) / SR);
    const sa = 1 - Math.exp((-TAU * Math.max(35, cut * 0.18)) / SR);
    const white = nrnd();
    nf += fa * (white - nf);
    ns += sa * (white - ns);
    const band = nf - ns;                              // the band IS the skid
    ph += (res * (1 + Math.tanh(band * 8) * 0.055)) / SR;   // self-FM = the drag
    if (ph >= 1) ph -= Math.floor(ph);
    const carrier = Math.sin(TAU * ph);
    const gnarl = Math.tanh(band * (5 + rough * 5));
    const texture = gnarl * 0.44 + carrier * (0.08 + Math.abs(gnarl) * (0.42 + rough * 0.30));
    const fade = Math.min(1, (n - i) / (0.012 * SR));
    emit(drmL, drmR, i0 + i, texture * lvl * 0.15 * gain * fade, pan);
  }
}

// ── bell: quiet sine bell doubling the hook melody (hooks 2 and 3) ─────────
function bell(t, midi, dur, gain) {
  const f = hz(midi + 12);
  const parts = [[1, 1], [2.01, 0.32], [2.76, 0.16]];
  const n = Math.round(Math.min(dur * 1.4, 1.8) * SR), i0 = Math.round(t * SR);
  for (const [ratio, amp] of parts) {
    let ph = 0;
    for (let i = 0; i < n; i++) {
      const u = i / SR;
      const env = (u < 0.004 ? u / 0.004 : 1) * Math.exp(-u * 2.4 / Math.max(0.4, dur));
      ph += (f * ratio) / SR;
      const fade = Math.min(1, (n - i) / (0.01 * SR));
      emit(musL, musR, i0 + i, Math.sin(TAU * ph) * env * amp * gain * 0.055 * fade,
        ratio > 2 ? 0.25 : -0.15);
    }
  }
}

// ── board drone: low-passed noise into a feedback comb tuned to E2 ─────────
function drone() {
  const delay = Math.round(SR / hz(40));   // E2
  const buf = new Float64Array(delay);
  let w = 0, lpIn = 0, lpLoop = 0;
  const inA = 1 - Math.exp((-TAU * 250) / SR);
  const loopA = 1 - Math.exp((-TAU * 900) / SR);
  for (let i = 0; i < N; i++) {
    const bar = Math.floor(i / SR / BAR);
    const g = DRONE_G[sectionAt(Math.min(bar, BARS - 1))] ?? 0;
    lpIn += inA * (nrnd() * 0.5 - lpIn);
    const rd = buf[w];
    lpLoop += loopA * (rd - lpLoop);
    const y = lpIn * 0.08 + lpLoop * 0.93;
    buf[w] = y;
    w = (w + 1) % delay;
    // ease the section gain so the drone breathes instead of stepping
    const u = (i / SR) % BAR / BAR;
    const barNow = Math.min(bar, BARS - 1);
    const gPrev = DRONE_G[sectionAt(Math.max(0, barNow - 1))] ?? g;
    const eased = gPrev + (g - gPrev) * smooth(clamp(u * 2, 0, 1));
    let fadeOut = 1;
    const left = DURATION_S - i / SR;
    if (left < 3) fadeOut = clamp(left / 3, 0, 1);
    emit(musL, musR, i, y * eased * 0.030 * fadeOut, 0);
  }
}

// ═══ THE SCORE ═════════════════════════════════════════════════════════════

// pads — one voice per contiguous chord span
{
  let span0 = 0;
  for (let bar = 1; bar <= BARS; bar++) {
    if (bar === BARS || CHORDS[bar].name !== CHORDS[span0].name
      || sectionAt(bar) !== sectionAt(span0)) {
      const g = PAD_G[sectionAt(span0)] ?? 0.4;
      pad(bt(span0), (bar - span0) * BAR + 0.4, CHORDS[span0].tones, g);
      span0 = bar;
    }
  }
}

// bass + kick patterns per bar
for (let bar = 0; bar < BARS; bar++) {
  const sec = sectionAt(bar);
  const ch = CHORDS[bar];
  const bg = BASS_G[sec] ?? 0, kg = KICK_G[sec] ?? 0;
  if (bg > 0) {
    if (sec === "bridge") {
      bassNote(bt(bar, 0), ch.root, 2.6 * SPB, bg);
    } else if (isHook(bar)) {
      bassNote(bt(bar, 0), ch.root, 1.4 * SPB, bg);
      bassNote(bt(bar, 2), ch.root, 1.1 * SPB, bg * 0.9);
      bassNote(bt(bar, 2.75), ch.root + 12, 0.35 * SPB, bg * 0.4);  // offbeat bump
    } else if (sec === "outro") {
      if (bar % 2 === 0) bassNote(bt(bar, 0), ch.root, 2.4 * SPB, bg);
    } else {
      bassNote(bt(bar, 0), ch.root, 1.3 * SPB, bg);
      bassNote(bt(bar, 2), ch.root, 1.1 * SPB, bg * 0.85);
    }
  }
  if (kg > 0) {
    if (sec === "verse" && bar < 6) {
      // voice enters over pad alone; drums wait for bar 6
    } else if (isHook(bar)) {
      kick(bt(bar, 0), kg); kick(bt(bar, 1), kg * 0.72);
      kick(bt(bar, 2), kg * 0.92); kick(bt(bar, 3), kg * 0.72);
    } else if (sec === "bridge") {
      kick(bt(bar, 0), kg);
      if (bar >= 30) kick(bt(bar, 2), kg * 0.8);   // "performance" rises
    } else if (sec === "outro") {
      if (bar === 44) kick(bt(bar, 0), kg);        // the last kick
    } else {
      kick(bt(bar, 0), kg); kick(bt(bar, 2), kg * 0.85);
    }
  }
}

// chalk taps — offbeat ticks; sparser than a hat line, brighter in hooks
for (let bar = 0; bar < BARS; bar++) {
  const sec = sectionAt(bar);
  if (sec === "intro") {
    if (bar === 2) tap(bt(bar, 1.5), { gain: 0.5, f: 2900, pan: -0.3 });
    if (bar === 3) {
      tap(bt(bar, 1.5), { gain: 0.55, f: 3400, pan: 0.3 });
      tap(bt(bar, 3.5), { gain: 0.6, f: 3800, pan: -0.2 });
      tap(bt(bar, 3.75), { gain: 0.5, f: 4300, pan: 0.2 });   // pickup into the verse
    }
    continue;
  }
  if (sec === "outro") {
    if (bar === 45) tap(bt(bar, 1.5), { gain: 0.4, f: 3000, pan: 0.2 });
    if (bar === 47) tap(bt(bar, 3.5), { gain: 0.55, f: 2600, pan: 0 });  // chalk set down
    continue;
  }
  if (sec === "bridge") {
    tap(bt(bar, 3.5), { gain: 0.3, f: 2700 + 500 * rnd(), pan: bar % 2 ? 0.25 : -0.25 });
    continue;
  }
  const g = isHook(bar) ? 0.55 : 0.42;
  tap(bt(bar, 1.5), { gain: g, f: 2800 + 700 * rnd(), pan: -0.28 });
  tap(bt(bar, 3.5), { gain: g * 0.9, f: 3300 + 900 * rnd(), pan: 0.28 });
  if (isHook(bar)) tap(bt(bar, 2.75), { gain: 0.4, f: 4100 + 500 * rnd(), pan: 0.1 });
}

// chalk scrapes — the friction gestures, placed narratively
friction(bt(3, 1), 2.0, { shape: "slide", gain: 0.9, cut0: 900, cut1: 2400,
  res0: 120, res1: 320, rough: 0.6, pan: -0.15 });          // the first stroke, into the verse
friction(bt(7, 2), 0.7, { shape: "skid", gain: 0.5, cut0: 1600, cut1: 2200,
  res0: 220, res1: 300, rough: 0.5, pan: 0.3 });
friction(bt(11, 2), 0.8, { shape: "skid", gain: 0.5, cut0: 1400, cut1: 2000,
  res0: 200, res1: 260, rough: 0.55, pan: -0.3 });
friction(bt(19, 2), 1.6, { shape: "drag", gain: 0.9, cut0: 800, cut1: 2600,
  res0: 110, res1: 380, rough: 0.65, pan: 0 });             // drag INTO hook 1
// the bridge showcase — the scrape sings with the words
friction(bt(24, 0), 2.6, { shape: "slide", gain: 0.55, cut0: 700, cut1: 1500,
  res0: 90, res1: 180, rough: 0.45, pan: -0.2 });
friction(bt(25, 3.5), 1.5, { shape: "drag", gain: 0.95, cut0: 900, cut1: 2600,
  res0: 130, res1: 420, rough: 0.7, pan: 0.15 });           // under "scrape a line"
friction(bt(28, 2.5), 1.1, { shape: "drag", gain: 0.7, cut0: 1000, cut1: 2200,
  res0: 150, res1: 300, rough: 0.6, pan: -0.2 });           // under "the trace"
friction(bt(30, 0), 3.2, { shape: "slide", gain: 0.6, cut0: 800, cut1: 2000,
  res0: 100, res1: 260, rough: 0.5, pan: 0.2 });            // under the "performance" rise
for (const h of [20, 32, 40]) {
  friction(bt(h + 1, 2), 0.7, { shape: "skid", gain: 0.45, cut0: 1500, cut1: 2100,
    res0: 210, res1: 280, rough: 0.5, pan: 0.3 });          // off the back of "boards"
  friction(bt(h + 3, 2), 1.2, { shape: "drag", gain: 0.7, cut0: 800, cut1: 2400,
    res0: 120, res1: 340, rough: 0.6, pan: -0.15 });        // drag into the next section
}
friction(bt(46, 0), 3.0, { shape: "slide", gain: 0.8, cut0: 700, cut1: 1600,
  res0: 90, res1: 200, rough: 0.45, pan: 0 });              // the last slow stroke

// bell doubles — hooks 2 and 3 only (the hook has earned being rung by then)
for (const h of [32, 40]) {
  for (const s of HOOK_MELODY) bell(s.t + bt(h), s.midi, s.dur, h === 40 ? 1.15 : 1.0);
}

// the drone runs the whole track at section-shaped gain
drone();

// ── sidechain: music breathes under the kick (0.35 depth, 9 ms in, 0.28 s out)
{
  const duck = new Float64Array(N).fill(1);
  const depth = 0.35, atkN = Math.round(0.009 * SR), relN = Math.round(0.28 * SR);
  for (const t of kickTimes) {
    const i0 = Math.round(t * SR);
    for (let i = 0; i < atkN; i++) {
      const v = 1 - depth * (i / atkN);
      const k = i0 + i; if (k < N) duck[k] = Math.min(duck[k], v);
    }
    for (let i = 0; i < relN; i++) {
      const v = 1 - depth * (1 - smooth(i / relN));
      const k = i0 + atkN + i; if (k < N) duck[k] = Math.min(duck[k], v);
    }
  }
  for (let i = 0; i < N; i++) { musL[i] *= duck[i]; musR[i] *= duck[i]; }
}

// ── sum, trim, write ───────────────────────────────────────────────────────
const L = new Float64Array(N), R = new Float64Array(N);
for (let i = 0; i < N; i++) { L[i] = musL[i] + drmL[i]; R[i] = musR[i] + drmR[i]; }
let peak = 0;
for (let i = 0; i < N; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
const trim = peak > 0 ? 0.85 / peak : 1;
for (let i = 0; i < N; i++) { L[i] *= trim; R[i] *= trim; }

function writeWavStereo(path, l, r) {
  const n = l.length;
  const buf = Buffer.alloc(44 + n * 8);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 8, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 8, 28);
  buf.writeUInt16LE(8, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 8, 40);
  for (let i = 0; i < n; i++) {
    buf.writeFloatLE(l[i], 44 + i * 8);
    buf.writeFloatLE(r[i], 48 + i * 8);
  }
  writeFileSync(path, buf);
}

const bedPath = `${OUT}/blackboard-bed.wav`;
writeWavStereo(bedPath, L, R);

// section RMS report — the shape should live in the arrangement
for (const s of SECTIONS) {
  const a = Math.round(bt(s.bar0) * SR), b = Math.min(N, Math.round(bt(s.bar1) * SR));
  let e = 0;
  for (let i = a; i < b; i++) e += L[i] * L[i] + R[i] * R[i];
  const rms = 10 * Math.log10(e / ((b - a) * 2) + 1e-12);
  console.log(`  ${s.name.padEnd(8)} bars ${String(s.bar0).padStart(2)}–${String(s.bar1 - 1).padEnd(2)}  rms ${rms.toFixed(1)} dB`);
}
console.log(`✓ ${bedPath} (peak trim ×${trim.toFixed(3)}, ${(N / SR).toFixed(1)} s, ${BPM} BPM)`);
