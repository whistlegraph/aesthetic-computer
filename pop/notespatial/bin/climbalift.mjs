#!/usr/bin/env node
// climbalift.mjs — the Climb and the Lift, cut out of Note(s)pat(ial) Native
// and rebuilt as one big-room record with a through line.
//
// The one idea: a chord broken into a line, running the ring, getting faster
// and higher until it becomes the theme. Every section carries the arpeggio
// at one rate in one place — half notes on a single seat, the sung 3+3+2 of
// the verse, quarters and eighths hopping the ring in the first climb,
// sixteenths and thirty-seconds in the glock climbs — and each climb arrives
// at a lift: the theme, one key up, hopping the ring. Climb = the arp
// accelerating and rising. Lift = the theme. That is the title.
//
// Form (100 bars, two arches, one peak; 128 BPM, the peak at 136):
//   intro 8 · verse 8 · climb 8 · lift 8 · still 8 · verse 8 · climb 8 ·
//   LIFT 16 (+3) · after 8 · climb 8 · LIFT 8 (+6, the peak) · outro 4 (home)
// Keys: a minor-third ladder that never lands — Bm, Dm, Fm — each lift
// prepared in the climb before it by alternating tonic and target at
// halving durations (digest 01 R11), then a tritone fall home in the outro.
// Harmony stays dark and mostly still: the climbs pinned on one chord, the
// lifts on i i VI VI iv iv V V with the raised leading tone under the theme.
//
// Space (digests 02, 05): one moving voice outside the lifts — the arp —
// hop rate stepping with note rate; kick, sub and hats pinned; every section
// keeps a silence and a solo seat; the blast turns the field once at the
// second lift, the eight-turn spin once at the peak.
//
// Instruments: the mill's physically modelled voices where they earn it —
// FEM stone bells for the held voice, the modal marimba for the verse line,
// the novelizer's friction voice for the sigh, its two-mass creature for the
// still drone, its implosive kick and crackle snare for the glock climbs, the
// waveguide guitar for power chords, the wobble engine split clean/dirty for
// the wub — and supersaws, sines and noise for the rest.
//
//   node pop/notespatial/bin/climbalift.mjs [--out print.wav] [--no-guitar] [--fast]
//   (--fast skips the C renders: FEM bells, novelizer voices, guitar)
//
// Deterministic. Output: 32-bit float stereo binaural print, 44.1 kHz, peak 0.95,
// for pop/notespatial/bin/master-v2.sh with ARC=0.

import { writeFileSync, readFileSync, mkdtempSync, mkdirSync, existsSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { mixEventSupersaw } from '../../dance/synths/supersaw.mjs';
import { renderWobble } from '../../dance/synths/wobble.mjs';
import { renderSkrill } from '../../dance/synths/skrill.mjs';
import { mixEventMarimba } from '../../marimba/synths/marimba.mjs';
import { applyBitcrush, softClip } from '../../dance/synths/fx.mjs';
import { readWavMono } from '../../lib/wav.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, '../../..');
const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf('--' + k); return i >= 0 ? args[i + 1] : d; };
const NO_GUITAR = args.includes('--no-guitar'), FAST = args.includes('--fast');
const OUT = resolve(opt('out', join(HERE, '../out/climbalift-print.wav')));
const EVENTS = opt('events', null); // --events score.json: every sounding event, for the score video
const CACHE = join(HERE, '../out/cache'); mkdirSync(CACHE, { recursive: true });
const WORK = mkdtempSync(join(tmpdir(), 'climbalift-'));
const SDK = { ...process.env, SDKROOT: process.env.SDKROOT || '/Library/Developer/CommandLineTools/SDKs/MacOSX26.sdk' };

// ── clock: a tempo per section ────────────────────────────────────────
const SR = 44100;
const FORM = [['intro', 8, 128], ['verse1', 8, 128], ['climb1', 8, 128], ['lift1', 8, 128], ['still', 8, 128], ['verse2', 8, 128], ['climb2', 8, 128], ['lift2', 16, 128], ['after', 8, 128], ['climb3', 8, 128], ['lift3', 8, 136], ['outro', 4, 136]];
const SEC = {}; { let t = 0, bar = 0; for (const [n, bars, bpm] of FORM) { const beat = 60 / bpm, barLen = 4 * beat; SEC[n] = { name: n, bar, t, bars, bpm, beat, barLen, end: t + bars * barLen }; t += bars * barLen; bar += bars; } }
const LAST = SEC.outro, DUR = LAST.end + 4, N = Math.ceil(DUR * SR);
const TR = { lift2: 3, after: 3, climb3: 3, lift3: 6 }; // semitones above Bm; the outro is home
const trOf = n => TR[n] || 0;
const hz = m => 440 * 2 ** ((m - 69) / 12);
const NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const noteName = m => NAMES[m % 12] + (Math.floor(m / 12) - 1);
let seed = 20260925; const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296 * 2 - 1;

// ── the room: 5 ring seats (0 front, clockwise), 5 = held center, 6 = sub ──
const RING = 5, CENTER = 5, SUB = 6, OUTPUTS = 7;
const seatAz = k => k === SUB || k === CENTER ? 0 : k / RING * 360;
const layer = () => Array.from({ length: OUTPUTS }, () => new Float32Array(N));
const L = { drums: layer(), sub: layer(), wubsub: layer(), bass: layer(), wub: layer(), growl: layer(), walk: layer(), pad: layer(), lead: layer(), voice: layer(), ghost: layer(), guitar: layer(), fx: layer() };
const REAR = [2, 3], FRONT_PAIR = [1, 4];
// the event log: which layer and seat each buffer is, so every note can be drawn
const LAYER_OF = new Map(); for (const [name, lay] of Object.entries(L)) lay.forEach((buf, k) => LAYER_OF.set(buf, [name, k]));
const EV = [];
const logEv = (buf, t, dur, f, g, kind) => { if (!EVENTS) return; const w = LAYER_OF.get(buf); if (w) EV.push([w[0], w[1], +t.toFixed(4), +dur.toFixed(4), f ? +f.toFixed(2) : 0, +g.toFixed(3), kind]); };

// ── tiny additive synth ───────────────────────────────────────────────
// waves: sine, triangle, saw, noise, hnoise (noise high-passed ~5 kHz), mnoise (~900 Hz)
function tone(buf, t, dur, f, g, { wave = 'sine', attack = .005, decay = dur * .6 } = {}) {
  logEv(buf, t, dur, wave === 'sine' || wave === 'triangle' || wave === 'saw' ? f : 0, g, wave);
  const s0 = Math.round(t * SR), n = Math.round(dur * SR), a = Math.max(1, attack * SR), d = Math.max(1, decay * SR), inc = f / SR;
  let ph = rnd() * .5 + .5, lp = 0; const c = wave === 'hnoise' ? .51 : .12;
  for (let i = 0; i < n && s0 + i < N; i++) {
    const env = i < a ? i / a : i > n - d ? Math.exp(-4 * (i - (n - d)) / d) : 1;
    ph += inc; if (ph >= 1) ph -= 1;
    let v;
    if (wave === 'sine') v = Math.sin(ph * 2 * Math.PI);
    else if (wave === 'triangle') v = 4 * Math.abs(ph - .5) - 1;
    else if (wave === 'saw') v = 2 * ph - 1;
    else if (wave === 'noise') v = rnd();
    else { const w = rnd(); lp += c * (w - lp); v = w - lp; }
    buf[s0 + i] += v * env * g;
  }
}
const pluck = (buf, t, dur, m, g) => { tone(buf, t, dur, hz(m), g, { attack: .003, decay: dur * .78 }); tone(buf, t, dur * .5, hz(m) * 2, g * .22, { attack: .002, decay: dur * .42 }); };
const glock = (buf, t, dur, m, g) => { const f = hz(m); tone(buf, t, Math.max(dur, .22), f, g, { attack: .001, decay: .2 }); tone(buf, t, .12, f * 2.76, g * .5, { attack: .001, decay: .1 }); tone(buf, t, .07, f * 5.4, g * .25, { attack: .001, decay: .06 }); tone(buf, t, .006, 6000, g * .3, { wave: 'noise', attack: .0005, decay: .005 }); };
const saw = (buf, t, dur, m, g, preset = 'lead', detuneCents) => (logEv(buf, t, dur, hz(m), g, 'saw:' + preset), mixEventSupersaw({ startSec: t, midi: m, gain: g, durSec: dur }, buf, { sampleRate: SR, preset, ...(detuneCents ? { detuneCents } : {}) }));
// the lead: shrill and sour — a supersaw, an octave copy 45 cents flat, a bright sine 35 cents flat
const lead = (seat, t, dur, m, g) => { saw(L.lead[seat], t, dur, m, g); saw(L.lead[seat], t, dur, m + 11.55, g * .26); tone(L.lead[seat], t, dur, hz(m) * 2 * 2 ** (-35 / 1200), g * .1, { attack: .01, decay: dur * .6 }); };

// ── C voices, rendered once and cached ────────────────────────────────
function resampleTo44(samples, sr) { if (sr === SR) return samples; const n = Math.floor(samples.length * SR / sr), out = new Float32Array(n); for (let i = 0; i < n; i++) { const x = i * sr / SR, j = Math.floor(x), f = x - j; out[i] = (samples[j] || 0) * (1 - f) + (samples[j + 1] || 0) * f; } return out; }
function cached(key, make) { const p = join(CACHE, key + '.f32'); if (existsSync(p)) { const b = readFileSync(p); return new Float32Array(b.buffer, b.byteOffset, b.length / 4); } const a = make(); if (a) writeFileSync(p, Buffer.from(a.buffer, a.byteOffset, a.byteLength)); return a; }
const run = (bin, argv) => { const r = spawnSync(bin, argv, { encoding: 'utf8', env: SDK }); if (r.status !== 0) { console.warn(`${bin} failed:`, (r.stderr || '').split('\n').slice(-2).join(' ')); return false; } return true; };
// FEM stone bell (pop/bell): one strike per pitch, peak-normalized by the engine, so gain is level
const BELL = join(ROOT, 'pop/bell/c/bell');
function femBell(geometry, midi) {
  if (FAST || !existsSync(BELL)) return null;
  return cached(`bell-${geometry}-${midi}`, () => { const wav = join(WORK, `bell-${geometry}-${midi}.wav`); if (!run(BELL, ['--note', noteName(midi), '--material', 'bronze', '--geometry', geometry, '--dur', geometry === 'church' ? '5' : '3', '--vel', '.9', '--sr', String(SR), '--out', wav])) return null; return readWavMono(wav).samples; });
}
// novelizer voices (pop/novelizer): a written line → one wav, normalized to −1 dBFS
const NV = name => join(ROOT, 'pop/novelizer/build', name);
function novelizer(name, notes, key) { // notes: [[hz, start, dur, vel]]
  if (FAST || !existsSync(NV(name)) || !notes.length) return null;
  return cached(`nv-${name}-${key}`, () => { const txt = join(WORK, `${name}-${key}.txt`); writeFileSync(txt, notes.map(n => n.join(' ')).join('\n') + '\n'); if (!run(NV(name), ['--notes', txt, '--out', WORK])) return null; const { samples, sampleRate } = readWavMono(join(WORK, `${name}-notes.wav`)); return resampleTo44(samples, sampleRate); });
}
function place(buf, samples, t, g, { fadeAfter = Infinity, fade = .3, f = 0, kind = 'sample' } = {}) { if (!samples) return false; logEv(buf, t, Math.min(samples.length / SR, fadeAfter + fade), f, g, kind); const s0 = Math.round(t * SR), fa = Math.round(fadeAfter * SR), fl = Math.max(1, Math.round(fade * SR)); for (let i = 0; i < samples.length && s0 + i < N; i++) { const env = i < fa ? 1 : Math.exp(-4 * (i - fa) / fl); if (env < 1e-3) break; buf[s0 + i] += samples[i] * g * env; } return true; }

// ── the voices ────────────────────────────────────────────────────────
let GHOST = true;
// the held voice: a bronze handbell struck at the center (church bell in the still passages); a sine stack if the engine is missing
function voice(seat, t, dur, m, g, geometry = 'handbell') {
  const b = femBell(geometry, m);
  if (!place(L.voice[CENTER], b, t, g * .9, { fadeAfter: Math.max(dur * 1.3, .4), fade: geometry === 'church' ? 2.5 : .6, f: hz(m), kind: 'bell:' + geometry })) {
    tone(L.voice[CENTER], t, dur, hz(m), g, { attack: .004, decay: dur * .8 }); tone(L.voice[CENTER], t, dur * .55, hz(m) * 2, g * .28, { attack: .002, decay: dur * .45 });
    tone(L.voice[CENTER], t, dur * 1.5, hz(m) * .56, g * .18, { attack: .01, decay: dur }); tone(L.voice[CENTER], t, dur * 1.5, hz(m) * .92, g * .14, { attack: .01, decay: dur }); // Risset's sour partials
  }
  if (GHOST) ghost(t, dur, m, g);
}
// the screwed voice: an octave down, 45 cents flat, an eighth late, slow to arrive, behind you; bitcrushed at the mix
function ghost(t, dur, m, g, beat = .46875) {
  const f = hz(m - 12) * 2 ** (-45 / 1200), d = Math.max(dur, 1.2 * beat);
  for (const k of REAR) { tone(L.ghost[k], t + beat / 2, d * 1.3, f * (k === 2 ? 1 : 2 ** (9 / 1200)), g * .5, { wave: 'triangle', attack: .35, decay: d * .7 }); tone(L.ghost[k], t + beat / 2, d * 1.3, f * 2, g * .14, { attack: .4, decay: d * .6 }); }
  tone(L.ghost[4], t + beat / 2 + .05, d * 1.6, hz(m) * 2, g * .12, { attack: .5, decay: d * .9 }); // the shimmer, an octave up, front-left
}
// the verse line: the marimba (rosewood), sung low, at the center
const marimba = (seat, t, dur, m, g) => { logEv(L.voice[CENTER], t, dur, hz(m), g, 'marimba'); mixEventMarimba({ startSec: t, midi: m, gain: g, durSec: dur }, L.voice[CENTER], { preset: 'rosewood', sampleRate: SR }); if (GHOST) ghost(t, dur, m, g * .8); };

// ── low end ───────────────────────────────────────────────────────────
// The bass, three layers (papers/bass-platter digest 02, rules R1–R7):
//   L1 sub   — one sine at the real root (E1–B1, 41–62 Hz), mono, on the SUB feed, ducked 6 dB by the kick.
//   L2 mid   — the wobble engine an octave up (E2–B2), resonance tamed to Q 1.3, sweep under two octaves,
//              LFO phase-synced to the grid, band-limited 75 Hz–5 kHz, at the front seat; the bounce is the sidechain.
//   L3 growl — the skrill two octaves up (E3–B3), vowels stepping a quarter note, wide on the rear pair, held sections only.
// The engines' own subs are never used (the wobble's is an octave under the note; the skrill's is a DC hump).
const MID = { bomp: { subGain: 0, cutLo: 90, cutHi: 700, q: 1.3, lfoDepth: .7, drive: 1.6, edge: .25, crush: 0 }, row: { subGain: 0, cutLo: 90, cutHi: 900, q: 1.3, lfoDepth: .7, drive: 1.8, edge: .25, crush: 0, lfo: '1/16', lfoShape: 'sine' }, reese: { subGain: 0, cutLo: 150, cutHi: 450, q: 1.5, lfoDepth: .5, drive: 1.8, edge: .2, crush: 0 } };
function placeSeg(buf, seg, t, g, filters = []) { const s0 = Math.round(t * SR); for (let i = 0; i < seg.length && s0 + i < N; i++) { let x = seg[i]; for (const f of filters) x = f(x); buf[s0 + i] += x * g; } }
function wub(t, dur, m, g, preset = 'bomp', bpm = 128, { growl = false } = {}) {
  tone(L.wubsub[SUB], t, dur, hz(m), .34 * g / .5, { attack: .004, decay: .15 });
  const seg = renderWobble({ startSec: t, midi: m + 12, durSec: dur, gain: g * 1.1 }, { preset, bpm, sampleRate: SR, phaseSync: true, params: MID[preset] || MID.bomp });
  logEv(L.wub[0], t, dur, hz(m + 12), g, 'wub:' + preset);
  placeSeg(L.wub[0], seg, t, 1, [biquad('hp', 75, .7), biquad('lp', 5000, .7)]);
  // the phone layer (bass-platter R8): a parallel distorted copy of the mid, band-limited 250–700 Hz, 12 dB under it, so a phone hears the bass move
  placeSeg(L.wub[0], seg, t, .7, [biquad('hp', 250, .8), x => Math.tanh(x * 4) / Math.tanh(4), biquad('hp', 250, .8), biquad('lp', 700, .8)]);
  if (growl) { const gr = renderSkrill({ startSec: t, midi: m + 24, durSec: dur, gain: g * .5, preset: 'growl' }, { bpm, sampleRate: SR, phaseSync: true, params: { subGain: 0, lfo: '1/4', q: 5, drive: 2.2, edge: .2 } }); for (const k of REAR) { logEv(L.growl[k], t, dur, hz(m + 24), g * .5, 'growl'); placeSeg(L.growl[k], gr, t + (k === 3 ? .011 : 0), .5, [biquad('hp', 150, .7), biquad('lp', 6000, .7), x => Math.tanh(x * 1.3) / Math.tanh(1.3)]); } }
}
const wubs = (s, chordFn, { preset = 'bomp', from = 0, to = s.bars, g = .5, len = 2, growl = false } = {}) => { for (let bar = from; bar < to; bar++) for (const b of len === 2 ? [0, 2] : [0]) wub(s.t + bar * s.barLen + b * s.beat, len * s.beat * .96, chordFn(bar, b)[0] - 24, g, preset, s.bpm, { growl }); };
// the verse bass: sub eighths with a tanh'd saw octave on the back seat (the only place the bounce lives)
function bassEighths(s, chordFn, { from = 0, to = s.bars, g = 1 } = {}) {
  for (let bar = from; bar < to; bar++) { const root = chordFn(bar, 0)[0] - 24, t0 = s.t + bar * s.barLen; for (let b = 0; b < 4; b += .5) { tone(L.sub[SUB], t0 + b * s.beat, .45 * s.beat, hz(root), .3 * g, { attack: .004, decay: .3 * s.beat }); tone(L.bass[3], t0 + b * s.beat, .45 * s.beat, hz(root + 12), .3 * g, { wave: 'saw', attack: .004, decay: .3 * s.beat }); } }
}

// ── drums (pinned: kick front, claps and hats on the rear pair) ───────
const kicks = [];
// the kick: a club electro kick — a 190→48 Hz body that drops in 35 ms and is gone in 280, saturated
// inside; a chirp-and-noise click; a 150 Hz knock a phone can hear; the whole drums seat gets a 300 Hz
// dip, a 2.5 kHz presence lift and a transient push at the mix. The sub cabinet takes only the body's low part.
function kick(t, g = 1) {
  kicks.push(t); logEv(L.drums[0], t, .3, 0, g, 'kick');
  const s0 = Math.round(t * SR); let ph = 0, ph2 = 0, hp = 0;
  for (let i = 0; i < SR * .5 && s0 + i < N; i++) {
    const x = i / SR, f = 48 + 142 * Math.exp(-x / .035); ph += f / SR;
    const env = Math.exp(-x / .28) * Math.min(1, i / 6), v = Math.sin(ph * 2 * Math.PI) * env;
    L.drums[0][s0 + i] += Math.tanh(v * 1.8 * g) / Math.tanh(1.8) * .9;
    L.sub[SUB][s0 + i] += v * .26 * g * (f < 90 ? 1 : .3);
    if (x < .03) { const cenv = Math.exp(-x / .004), w = rnd(); hp += .4 * (w - hp); const fc = 300 + 1500 * Math.exp(-x / .004); ph2 += fc / SR; L.drums[0][s0 + i] += ((w - hp) + Math.sin(ph2 * 2 * Math.PI) * .6) * cenv * .55 * g; } // the click: noise and a chirp
  }
  tone(L.drums[0], t, .06, 150, .4 * g, { wave: 'triangle', attack: .001, decay: .045 }); // the knock
}
function clap(t, g = 1) { for (const [dt, gg] of [[0, .6], [.011, .7], [.023, .8], [.034, 1]]) for (const k of REAR) tone(L.drums[k], t + dt, .06 + (gg === 1 ? .16 : 0), 0, .5 * gg * g, { wave: 'mnoise', attack: .001, decay: gg === 1 ? .13 : .04 }); tone(L.drums[2], t, .09, 190, .18 * g, { wave: 'triangle', attack: .001, decay: .07 }); }
const hat = (t, open = false, g = 1, seat = 3) => tone(L.drums[seat], t, open ? .16 : .035, 0, (open ? .28 : .18) * g, { wave: 'hnoise', attack: .0005, decay: open ? .13 : .025 });
const crash = (t, g = 1) => { for (let k = 0; k < RING; k++) tone(L.fx[k], t + k * .012, 1.4, 0, .22 * g, { wave: 'hnoise', attack: .002, decay: 1.1 }); };
function roll(t0, beats, beat) { const t1 = t0 + beats * beat; let t = t0; while (t < t1) { const u = (t - t0) / (beats * beat), step = u < .5 ? beat / 2 : u < .8 ? beat / 4 : beat / 8; clap(t, .25 + .6 * u); t += step; } }
function riser(t0, seconds, g = 1) { const s0 = Math.round(t0 * SR), n = Math.round(seconds * SR); let lp = 0; for (let i = 0; i < n && s0 + i < N; i++) { const u = i / n, w = rnd(); lp += (.02 + .9 * u * u) * (w - lp); L.fx[3][s0 + i] += (w - lp) * u * u * .4 * g; } }
function kit(s, { from = 0, to = s.bars, four = true, claps = true, hats = true, open = false, half = false, g = 1 } = {}) {
  for (let bar = from; bar < to; bar++) for (let b = 0; b < 4; b++) {
    const t = s.t + bar * s.barLen + b * s.beat, seat = bar % 2 ? 2 : 3;
    if (four && (!half || b % 2 === 0)) kick(t, (half ? .8 : 1) * g);
    if (claps && (b === 1 || b === 3)) clap(t, (half ? .6 : .9) * g);
    if (hats) { hat(t, false, .45 * g, seat); if (open) hat(t + s.beat / 2, true, .7 * g, seat); }
  }
}
// the double-time kit of the glock climbs: the novelizer's implosive kick and crackle snare, if built
function doubleKit(s, { from = 0, to = s.bars } = {}) {
  const kp = [1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0], kn = [], sn = [];
  for (let bar = from; bar < to; bar++) for (let e = 0; e < 16; e++) { const t = bar * s.barLen + e * s.beat / 4; if (kp[e]) kn.push([55, +t.toFixed(4), .12, .9]); if (e % 4 === 2) sn.push([200, +t.toFixed(4), .1, .8]); hat(s.t + t, e % 4 === 2, .4, e % 8 < 4 ? 3 : 2); }
  const ik = novelizer('implokick', kn, `${s.name}-${from}-${to}`), cs = novelizer('cracklesnare', sn, `${s.name}-${from}-${to}`);
  if (!place(L.drums[0], ik, s.t + from * s.barLen, .9, { kind: 'implokick' })) for (const [, t] of kn) kick(s.t + t, .95); else for (const [, t] of kn) { kicks.push(s.t + t); logEv(L.drums[0], s.t + t, .3, 0, .9, 'implokick'); }
  if (!place(L.drums[2], cs, s.t + from * s.barLen, .7, { kind: 'cracklesnare' })) for (const [, t] of sn) clap(s.t + t, .8); else { for (const [, t] of sn) logEv(L.drums[2], s.t + t, .2, 0, .7, 'cracklesnare'); place(L.drums[3], cs, s.t + from * s.barLen + .004, .5, { kind: 'cracklesnare' }); }
}
// the explosion: a boom and a blast at one seat, debris around the ring, a scream that falls. Twice on the record.
function explode(t, seat, size = 1) {
  const s0 = Math.round(t * SR); let ph = 0, lp = 0;
  for (let i = 0; i < SR * 1.1 && s0 + i < N; i++) {
    const x = i / SR, f = 32 + 90 * Math.exp(-x / .05); ph += f / SR;
    const boom = Math.sin(ph * 2 * Math.PI) * Math.exp(-x / .35), w = rnd(); lp += .12 * (w - lp);
    const blast = (w * Math.exp(-x / .06) + lp * Math.exp(-x / .5)) * .9;
    L.fx[seat][s0 + i] += (boom * .45 + blast * .35) * size; L.sub[SUB][s0 + i] += boom * .2 * size;
  }
  for (let d = 0; d < 9; d++) { const k = (seat + 1 + d) % RING, td = t + .09 + d * (.075 + .02 * d); tone(L.fx[k], td, .09, 0, .2 * size * (1 - d / 10), { wave: 'mnoise', attack: .001, decay: .06 }); tone(L.fx[k], td, .12, 2400 * 2 ** (-d / 3), .08 * size * (1 - d / 10), { wave: 'saw', attack: .002, decay: .09 }); }
  for (const [dt, m, g] of [[0, 98, .3], [.06, 93, .25], [.13, 86, .2]]) saw(L.fx[(seat + 2) % RING], t + dt, .5, m, g * size, 'stab');
}

// ── air and dread ─────────────────────────────────────────────────────
function drone(t0, seconds, m, g = .14, beatHz = 4) { // two low triangles beating at beatHz, behind you
  const f = hz(m), cents = 1200 * Math.log2((f + beatHz) / f);
  tone(L.pad[2], t0, seconds, f, g, { wave: 'triangle', attack: 3, decay: seconds * .3 }); tone(L.pad[3], t0, seconds, f * 2 ** (cents / 1200), g, { wave: 'triangle', attack: 3.5, decay: seconds * .3 });
}
function creature(t0, seconds, m, g = .35) { const c = novelizer('twomass', [[hz(m), 0, seconds, .6]], `creature-${m}-${seconds | 0}`); if (!place(L.pad[3], c, t0, g, { fadeAfter: seconds, fade: 2, f: hz(m), kind: 'twomass' })) drone(t0, seconds, m - 12, g * .5, 2); }
function wind(t0, seconds, g = .5) { // filtered noise breathing in slow swells, at the front pair
  const s0 = Math.round(t0 * SR), n = Math.round(seconds * SR); let lp = 0, hp = 0, c = .02;
  for (let i = 0; i < n && s0 + i < N; i++) { const x = i / SR, w = rnd(); if ((i & 1023) === 0) c = .01 + .08 * (.5 + .5 * Math.sin(x * .37 + 1.3)) * (.6 + .4 * Math.sin(x * .11)); lp += c * (w - lp); hp += .002 * (lp - hp); const swell = .5 + .5 * Math.sin(x / 7.5 * Math.PI * 2 - Math.PI / 2), seat = Math.floor(x / 7.5) % 2 ? 1 : 4; L.fx[seat][s0 + i] += (lp - hp) * swell * g * Math.min(1, x / 2, (seconds - x) / 2); }
}

// ── harmony ───────────────────────────────────────────────────────────
const CH = { Bm: [59, 62, 66], G: [55, 59, 62], Em: [52, 55, 59], 'F#': [54, 58, 61], C: [60, 64, 67], D: [62, 66, 69], A: [57, 61, 64], 'F#m': [54, 57, 61] };
const PROG = { verse: ['Bm', 'Bm', 'G', 'G', 'Bm', 'Bm', 'C', 'F#'], climb: ['Bm', 'Bm', 'Bm', 'Bm', 'Bm', 'Bm', 'Bm', 'Bm'], lift1: ['Bm', 'Bm', 'Bm', 'Bm', 'Em', 'Em', 'F#', 'F#'], lift: ['Bm', 'Bm', 'G', 'G', 'Em', 'Em', 'F#', 'F#'] };
const chordOf = (prog, bar, tr = 0) => CH[PROG[prog][bar % 8]].map(m => m + tr);
const SHAPE = { Bm: [47, 54, 59, 62, 66], G: [43, 47, 50, 55, 59, 67], Em: [40, 47, 52, 55, 59, 64], 'F#': [42, 49, 54, 58, 61, 66], C: [48, 52, 55, 60, 64], D: [50, 57, 62, 66], A: [45, 52, 57, 61, 64], 'F#m': [42, 49, 54, 57, 61, 66] };
const gtrChords = (prog, tr = 0) => PROG[prog].map(c => SHAPE[c].map(m => m + tr).join(',')).join('|');
const POWER = { Bm: [47, 54, 59], G: [43, 50, 55], Em: [40, 47, 52], 'F#': [42, 49, 54], C: [48, 55, 60], D: [50, 57, 62], A: [45, 52, 57], 'F#m': [42, 49, 54] };
const powerChords = (prog, tr = 0) => PROG[prog].map(c => POWER[c].map(m => m + tr).join(',')).join('|');

// ── the amp and the cabinet ───────────────────────────────────────────
// A distorted guitar is a chain, not a clip: a tight high-pass and a mid push
// into the first stage (the pedal), two asymmetric tube-ish stages with a DC
// block between them, a tone stack, then a closed 4×12 cabinet — the 110 Hz
// thump, the 450 Hz scoop, the presence peak, and the cone's steep roll-off
// above 5 kHz, which is what turns fizz into grind. Take two is a little darker.
function biquad(type, f, q, gainDb = 0) {
  const A = 10 ** (gainDb / 40), w = 2 * Math.PI * f / SR, cs = Math.cos(w), sn = Math.sin(w), al = sn / (2 * q);
  let b0, b1, b2, a0, a1, a2;
  if (type === 'hp') { b0 = (1 + cs) / 2; b1 = -(1 + cs); b2 = (1 + cs) / 2; a0 = 1 + al; a1 = -2 * cs; a2 = 1 - al; }
  else if (type === 'lp') { b0 = (1 - cs) / 2; b1 = 1 - cs; b2 = (1 - cs) / 2; a0 = 1 + al; a1 = -2 * cs; a2 = 1 - al; }
  else if (type === 'peak') { b0 = 1 + al * A; b1 = -2 * cs; b2 = 1 - al * A; a0 = 1 + al / A; a1 = -2 * cs; a2 = 1 - al / A; }
  else if (type === 'ls') { const sq = 2 * Math.sqrt(A) * al; b0 = A * ((A + 1) - (A - 1) * cs + sq); b1 = 2 * A * ((A - 1) - (A + 1) * cs); b2 = A * ((A + 1) - (A - 1) * cs - sq); a0 = (A + 1) + (A - 1) * cs + sq; a1 = -2 * ((A - 1) + (A + 1) * cs); a2 = (A + 1) + (A - 1) * cs - sq; }
  else { const sq = 2 * Math.sqrt(A) * al; b0 = A * ((A + 1) + (A - 1) * cs + sq); b1 = -2 * A * ((A - 1) + (A + 1) * cs); b2 = A * ((A + 1) + (A - 1) * cs - sq); a0 = (A + 1) - (A - 1) * cs + sq; a1 = 2 * ((A - 1) - (A + 1) * cs); a2 = (A + 1) - (A - 1) * cs - sq; }
  const c = [b0 / a0, b1 / a0, b2 / a0, a1 / a0, a2 / a0]; let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
  return x => { const y = c[0] * x + c[1] * x1 + c[2] * x2 - c[3] * y1 - c[4] * y2; x2 = x1; x1 = x; y2 = y1; y1 = y; return y; };
}
const stage = (gain, bias) => { const off = Math.tanh(bias); return x => (Math.tanh(x * gain + bias) - off) / Math.tanh(gain); }; // asymmetric: even harmonics
function ampCab(buf, s0, n, { drive = 1, dark = 0, clean = false } = {}) {
  const pre = [biquad('hp', 110, .7), biquad('peak', 750, .8, clean ? 1.5 : 5)], st1 = stage(clean ? 1.2 : 4 * drive, .18), dc1 = biquad('hp', 30, .7), st2 = stage(clean ? 1 : 1.6 * drive, -.12), dc2 = biquad('hp', 30, .7);
  const tone = [biquad('ls', 200, .7, -2), biquad('peak', 2800 - 400 * dark, 1.2, 4 - dark)];
  const cab = [biquad('peak', 110, 1.4, 4), biquad('peak', 450, 1, -3), biquad('peak', 1600, 2.5, -2), biquad('lp', 4800 - 600 * dark, 1.1), biquad('lp', 5200 - 600 * dark, .9)];
  let sag = 0; const sagC = 1 - Math.exp(-1 / (.03 * SR)), sagR = 1 - Math.exp(-1 / (.25 * SR));
  for (let i = s0; i < s0 + n && i < N; i++) {
    let x = buf[i]; for (const f of pre) x = f(x);
    const e = Math.abs(x); sag += (e > sag ? sagC : sagR) * (e - sag); // the power amp gives a little under a big chord
    x = dc1(st1(x * (1 - .35 * Math.min(1, sag * 2)))); x = dc2(st2(x));
    for (const f of tone) x = f(x); for (const f of cab) x = f(x);
    buf[i] = x * (clean ? 1.6 : .9);
  }
}

// ── lines: [beat within the block, beats, midi] ───────────────────────
const THEME = [ // the suite's theme, up a whole step: four two-bar phrases, each opening on a broken chord
  [[0, 1, 74], [1, .5, 78], [1.5, .5, 81], [2, 1.5, 78], [3.5, .5, 76], [4, 1, 78], [5, 1, 76], [6, 2, 74]],
  [[0, 1, 78], [1, .5, 81], [1.5, .5, 83], [2, 1, 81], [3, 1, 78], [4, 1.5, 76], [5.5, .5, 74], [6, 2, 71]],
  [[0, 1, 74], [1, 1, 79], [2, .5, 81], [2.5, .5, 83], [3, 1, 81], [4, 1, 79], [5, .5, 78], [5.5, .5, 76], [6, 2, 74]],
  [[0, 1, 76], [1, .5, 78], [1.5, .5, 81], [2, 1.5, 85], [3.5, .5, 81], [4, 1, 78], [5, 1, 76], [6, 2, 74]],
].flatMap((p, i) => p.map(([a, d, m]) => [i * 8 + a, d, m]));
const ghastly = line => line.map(([a, d, m]) => [a, d, a >= 24 && a < 32 && m % 12 === 9 ? m + 1 : m]); // over the F#: A becomes A#, the raised leading tone
// the verse: the 3+3+2 arp, sung an octave under the hook, landing C natural over the Neapolitan and A# over the F#
const VERSE = [[.5, 1.5, 66], [2, 1.5, 62], [3.5, .5, 59], [4.5, 1.5, 62], [6, 1.5, 66], [7.5, .5, 69], [8.5, 1.5, 67], [10, 1.5, 62], [11.5, .5, 59], [12.5, 1.5, 62], [14, 2, 66], [16.5, 1.5, 66], [18, 1.5, 62], [19.5, .5, 59], [20.5, 1.5, 62], [22, 1.5, 66], [23.5, .5, 69], [24.5, 1.5, 67], [26, 1.5, 64], [27.5, .5, 60], [28.5, 1.5, 61], [30, 2, 58]];
// the sigh: the only new line kept from the bridge, sung over the drone alone
const SIGH = [[0, 2, 73], [2, 1, 71], [3, 1, 69], [4, 1, 69], [5, 1, 66], [6, 2, 69], [8, 1, 74], [9, 1, 78], [10, 2, 81], [12, 1.5, 79], [13.5, .5, 78], [14, 2, 76]];
const play = (s, line, t0, seatOf, inst, g, durScale = .92, tr = 0) => { for (const [a, d, m] of line) inst(seatOf(t0 + a * s.beat), t0 + a * s.beat, d * s.beat * durScale, m + tr, g); };

// ── the arpeggio: the spine ───────────────────────────────────────────
// rate = notes per beat. Hop rate follows note rate, dwelling 200 ms or more per seat (digest 05 R5);
// the last two beats may spin (a seat per note, under 0.5 s a lap) as the cadence (digest 02 R7).
let arpHop = 0;
function arp(s, { from = 0, to = s.bars, rate = 1, octaves = 1, base = 12, inst, g = .3, chordFn, seatFn, registerStep = 0, spinLast = 0, upDown = true }) {
  const dwell = s.beat / rate, per = Math.max(1, Math.ceil(.2 / dwell));
  for (let bar = from; bar < to; bar++) {
    const c = chordFn(bar), reg = base + registerStep * Math.floor((bar - from) / 2) * 12;
    const up = Array.from({ length: octaves }, (_, o) => c.map(m => m + reg + 12 * o)).flat(), seq = upDown ? [...up, ...up.slice(1, -1).reverse()] : up;
    const perBar = Math.round(4 * rate);
    for (let n = 0; n < perBar; n++) {
      const t = s.t + bar * s.barLen + n * dwell, lastTwo = bar === to - 1 && n >= perBar - 2 * rate;
      if (lastTwo && spinLast === 0) continue; // silence before the chord
      if (lastTwo) arpHop++; else if (n % per === 0) arpHop++;
      const seat = seatFn ? seatFn(t, arpHop) : arpHop % RING;
      inst(L.walk[seat], t, dwell * .9, seq[n % seq.length] + (lastTwo ? 12 : 0), g);
    }
  }
}
// key preparation: the last four bars of a climb alternate tonic and target at 2, 1, ½, ½ bars (digest 01 R11)
const prep = (prog, trNow, trNext) => (bar, b = 0) => { const rel = bar - 4; if (rel < 2) return chordOf(prog, bar, trNow); if (rel < 3) return chordOf(prog, bar, trNext); return chordOf(prog, bar, b < 2 ? trNow : trNext); };

// ── the guitar (waveguide, pop/guitar) ────────────────────────────────
function guitar(s, chords, { pattern = 'D...D.D.', drive = .9, mute = 'open', bars = s.bars, g = .45, seeds = [1, 2], seats = FRONT_PAIR, damp = .15, force = .8, clean = false } = {}) {
  if (NO_GUITAR || FAST) return;
  const bin = join(ROOT, 'pop/guitar/c/strum');
  if (!existsSync(bin)) return console.warn('no strum binary; skipping guitar');
  seeds.forEach((sd, i) => {
    const wav = join(WORK, `gtr-${s.name}-${i}.wav`);
    if (!run(bin, ['--chord', chords, '--pattern', pattern, '--bpm', String(s.bpm), '--bars', String(bars), '--electric', '--drive', String(clean ? .15 : Math.min(.55, drive)), '--mute', mute, '--damp', String(damp), '--force', String(force), '--sr', String(SR), '--seed', String(sd), '--tail', '1.5', '--out', wav])) return;
    const { samples } = readWavMono(wav); let pk = 0; for (const v of samples) pk = Math.max(pk, Math.abs(v));
    const norm = pk > 0 ? .8 / pk : 0, s0 = Math.round(s.t * SR), dst = L.guitar[seats[i]], tmp = new Float32Array(N);
    for (let n = 0; n < samples.length && s0 + n < N; n++) tmp[s0 + n] = samples[n] * norm;
    ampCab(tmp, s0, samples.length, { drive, dark: i, clean });
    let pk2 = 0; for (let n = s0; n < s0 + samples.length && n < N; n++) pk2 = Math.max(pk2, Math.abs(tmp[n]));
    const norm2 = pk2 > 0 ? g / pk2 : 0; for (let n = s0; n < s0 + samples.length && n < N; n++) dst[n] += tmp[n] * norm2;
    for (let bar = 0; bar < bars; bar++) chords.split('|')[bar % chords.split('|').length].split(',').forEach(m => logEv(dst, s.t + bar * s.barLen, s.barLen, hz(+m), g, clean ? 'guitar:clean' : 'guitar:amp'));
  });
}

// ── placements and joins ──────────────────────────────────────────────
const hookSeat = (s, t) => Math.floor((t - s.t) / s.barLen) % RING; // the theme: one seat per bar, clockwise
const answerSeat = (s, t) => (RING - Math.floor((t - s.t) / (16 * s.barLen) * RING) % RING) % RING; // counter-clockwise, one orbit per 16 bars
const mutes = [], stops = [], turns = [];
const mute = (t0, t1, fadeIn = .01) => mutes.push([t0, t1, fadeIn]);
const tapeStop = () => {};                    // no spin-downs: the record is continuous
const blast = t => turns.push({ type: 'blast', t });
const spin = (t0, t1, n) => turns.push({ type: 'spin', t0, t1, turns: n });
function stutter(s, t, m, g = .4) { for (let k = 0; k < 8; k++) pluck(L.walk[[0, 1, 4, 2, 3][k % RING]], t + k * s.beat / 4, s.beat / 4, m, g * (1 - k / 9)); } // the 2-beat pickup

// ── the bed: never silent ─────────────────────────────────────────────
// Cabin air: brown noise through a slow low-pass, breathing over half a minute, at the
// front pair with a little behind. Space hats: a sixteenth-note tick the whole record long,
// dark and small, each tick echoing a dotted eighth later on the next seats, the pattern
// orbiting the ring once every 12.5 s (digest 02 R2: motion, not spin).
const BED = layer(); BED.forEach((buf, k) => LAYER_OF.set(buf, ['bed', k]));
{ let br = 0, lp = 0, lp2 = 0; const c = 1 - Math.exp(-2 * Math.PI * 320 / SR), c2 = 1 - Math.exp(-2 * Math.PI * 2400 / SR);
  for (let i = 0; i < N; i++) { const x = i / SR, w = rnd(); br = .995 * br + .02 * w; lp += c * (br - lp); lp2 += c2 * (w - lp2); const breath = .75 + .25 * Math.sin(x / 31 * Math.PI * 2) + .1 * Math.sin(x / 7.3 * Math.PI * 2 + 1); const air = lp * 1.4 * breath + (w - lp2) * .004 * breath; BED[1][i] += air * .55; BED[4][i] += air * .55; BED[2][i] += air * .3; BED[3][i] += air * .3; } }
{ for (const [n] of FORM) { const s = SEC[n]; for (let bar = 0; bar < s.bars; bar++) for (let e = 0; e < 16; e++) {
  const t = s.t + bar * s.barLen + e * s.beat / 4, seat = Math.floor(t / 2.5) % RING, acc = e % 4 === 2 ? 1 : e % 2 ? .45 : .7;
  tone(BED[seat], t, .028, 0, .11 * acc, { wave: 'noise', attack: .0005, decay: .02 });
  tone(BED[(seat + 1) % RING], t + s.beat * .75, .05, 0, .05 * acc, { wave: 'noise', attack: .001, decay: .045 });
  tone(BED[(seat + 2) % RING], t + s.beat * 1.5, .08, 0, .022 * acc, { wave: 'noise', attack: .002, decay: .07 }); } } }
{ let lp = 0; const c = 1 - Math.exp(-2 * Math.PI * 3800 / SR); for (let k = 0; k < RING; k++) { lp = 0; const b = BED[k]; for (let i = 0; i < N; i++) { lp += c * (b[i] - lp); b[i] = lp; } } } // deep: nothing above 4 kHz

// ═══ the sections ═════════════════════════════════════════════════════
// intro: the voice alone at the center; then the arp at half notes on seat 1; the drone arrives
{ const s = SEC.intro;
  GHOST = false; play(s, THEME.filter(([a]) => a < 8), s.t, () => CENTER, voice, .55); GHOST = true;
  play(s, THEME.filter(([a]) => a >= 8 && a < 16), s.t, () => CENTER, voice, .5);
  arp(s, { from: 4, to: 8, rate: .5, octaves: 1, base: 12, inst: pluck, g: .3, chordFn: () => CH.Bm, seatFn: () => 0, upDown: false });
  drone(s.t + 4 * s.barLen, 4 * s.barLen + 2, 54, .1); }
// verse 1: kick only; the 3+3+2 arp sung on the marimba; the sub in eighths; the pickup
function verse(s, { second = false } = {}) {
  kit(s, { from: 0, to: 7, claps: false, hats: second, half: second });
  if (!second) bassEighths(s, bar => chordOf('verse', bar), { to: 7, g: .9 });
  play(s, VERSE, s.t, () => CENTER, marimba, .8);
  if (second) { guitar(s, gtrChords('verse'), { pattern: 'D.d.U.d.', drive: .25, g: .34, damp: .05, clean: true, bars: 7 }); for (let bar = 0; bar < 7; bar++) for (const [k, [b, deg]] of [[.5, 2], [2, 1], [3.5, 0]].entries()) tone(L.walk[[2, 3, 2][k]], s.t + bar * s.barLen + b * s.beat, .38 * s.beat, hz(chordOf('verse', bar)[deg] + 12), .16, { wave: 'triangle', attack: .006, decay: .25 * s.beat }); }
  mute(s.end - s.barLen, s.end - 2 * s.beat); stutter(s, s.end - 2 * s.beat, 73);
}
verse(SEC.verse1);
// the climb: the arp accelerating and rising over a pinned chord, one voice moving; the kit gathers; silence, then the lift
function climb(s, { rate1 = 1, rate2 = 2, inst1 = pluck, inst2 = pluck, octaves1 = 1, octaves2 = 1, g = .32, trNow = 0, trNext = 0, double = false, wubPreset = null, hook = false, rise = false, rollIn = false, base = 12 }) {
  const chordFn = trNext !== trNow ? prep('climb', trNow, trNext) : bar => chordOf('climb', bar, trNow);
  const seatFn = (t, hop) => hop % RING;
  arp(s, { from: 0, to: 4, rate: rate1, octaves: octaves1, base, inst: inst1, g, chordFn, seatFn, registerStep: 1 });
  arp(s, { from: 4, to: 8, rate: rate2, octaves: octaves2, base: base + 12, inst: inst2, g: g * 1.05, chordFn, seatFn, registerStep: 1, spinLast: 1 });
  if (double) { doubleKit(s, { to: 8 }); wubs(s, chordFn, { preset: wubPreset, g: .55, to: 8 }); }
  else { kit(s, { from: 0, to: 2, claps: false, hats: false }); kit(s, { from: 2, to: 4, claps: false }); kit(s, { from: 4, to: 6 }); kit(s, { from: 6, to: 8, four: false, claps: false }); bassEighths(s, chordFn, { to: 6, g: .8 }); }
  if (hook) { GHOST = false; play(s, THEME.filter(([a]) => a < 16).map(([a, d, m]) => [a * .5, d * .5, m]), s.t + 4 * s.barLen, () => CENTER, voice, .45, .92, trNext); GHOST = true; }
  if (rollIn) roll(s.end - 2 * s.barLen, 7, s.beat);
  if (rise) riser(s.t + 4 * s.barLen, 3.5 * s.barLen, 1);
  mute(s.end - s.beat, s.end - .02); // one beat of nothing, then the chord
}
climb(SEC.climb1, { rate1: 1, rate2: 2, inst1: pluck, inst2: (buf, t, d, m, g) => { pluck(buf, t, d, m, g); saw(buf, t, d, m, g * .5, 'stab'); }, rollIn: true });
// the lift: the theme hopping the ring in shrill saws over the bell at the center; the walk on the seats across; the wub; the kit opens
function lift(s, { tr = 0, passes = 1, answer = false, liftOctave = false, guitarOn = false, padsOn = false, impact = 'crash', tutti = false, spinLast = false, prog = 'lift' }) {
  if (impact === 'explode') { explode(s.t, 0, .8); blast(s.t); } else crash(s.t, .8);
  kit(s, { open: true });
  wubs(s, bar => chordOf(prog, bar, tr), { preset: 'bomp', g: .5, to: passes > 1 ? 8 : s.bars, growl: tutti });
  if (passes > 1) wubs(s, bar => chordOf(prog, bar, tr), { preset: 'bomp', g: .5, from: 8, growl: true });
  if (padsOn) for (let bar = 0; bar < s.bars; bar += 2) chordOf(prog, bar, tr).forEach((m, k) => saw(L.pad[[1, 4, 2][k]], s.t + bar * s.barLen, 2 * s.barLen * .98, m, .1, 'pad', 42));
  for (let p = 0; p < passes; p++) {
    const t0 = s.t + p * 8 * s.barLen, line = ghastly(THEME.map(([a, d, m]) => [a, d, (liftOctave && p === passes - 1 && a % 8 >= 4 ? m + 12 : m) + tr]));
    play(s, line, t0, t => hookSeat(s, t), lead, .48 + .02 * p);
    play(s, line, t0, () => CENTER, voice, .36);
    if (answer && (p === passes - 1)) play(s, line.filter(([a]) => a % 8 >= 4).map(([a, d, m]) => [a + .5, d * .6, m - 12]), t0, t => answerSeat(s, t), (k, t, d, m, g) => saw(L.lead[k], t, d, m, g, 'stab'), .28);
    for (let bar = 0; bar < 8; bar++) { const c = chordOf(prog, bar, tr); for (let e = 0; e < 8; e++) { const t = t0 + bar * s.barLen + e * s.beat / 2, seat = (hookSeat(s, t) + 2 + e % 2) % RING; pluck(L.walk[seat], t, .55 * s.beat, c[[0, 1, 2, 1][e % 4]] + 12 + (e >= 4 ? 12 : 0), .2); } }
  }
  if (guitarOn) guitar(s, powerChords(prog, tr), { pattern: 'D...D.D.', drive: 1, force: 1, g: .45, mute: 'open', damp: .1 });
  if (tutti) { const chord = [59, 62, 66, 71, 74].map(m => m + tr), order = [0, 2, 4, 1, 3]; order.forEach((seat, k) => { const t = s.t + (3 + k) * s.barLen; saw(L.lead[seat], t, s.end - t, chord[k], .38); }); voice(CENTER, s.t + 6 * s.barLen, 2 * s.barLen, 83 + tr, .5, 'church'); wub(s.t + 6 * s.barLen, 2 * s.barLen, 35 + tr, .6, 'reese', s.bpm, { growl: true }); }
  if (spinLast) spin(s.t + 4 * s.barLen, s.end, 8);
}
lift(SEC.lift1, { prog: 'lift1', impact: 'crash' });
// still: two bars of nothing; then the creature and the beating drone behind you, and the ghost alone singing the second phrase
{ const s = SEC.still;
  tapeStop(s.t); mute(s.t, s.t + 2 * s.barLen, .5);
  creature(s.t + 2 * s.barLen, 6 * s.barLen, 47, .35); drone(s.t + 2 * s.barLen, 6 * s.barLen + 2, 54, .12, 4); wind(s.t + 2 * s.barLen, 6 * s.barLen, .7);
  for (const [a, d, m] of THEME.filter(([a]) => a >= 8 && a < 16)) ghost(s.t + 3 * s.barLen + a * s.beat, d * s.beat * 1.4, m, .55, s.beat);
  voice(CENTER, s.t + 7 * s.barLen, s.barLen, 71, .3, 'church'); }
verse(SEC.verse2, { second: true });
// climb 2: the glock — sixteenths then thirty-seconds, three octaves, double-time kit, the row wub, the key prepared toward Dm
climb(SEC.climb2, { rate1: 4, rate2: 8, inst1: glock, inst2: glock, octaves1: 2, octaves2: 3, g: .26, base: 12, trNow: 0, trNext: 3, double: true, wubPreset: 'row' });
// LIFT 2 (+3): the blast turns the field; the theme twice, the answer counter-rotating on the second pass; power chords
lift(SEC.lift2, { tr: 3, passes: 2, answer: true, liftOctave: true, guitarOn: true, padsOn: true, impact: 'explode' });
// after: a bar of nothing, then the sigh on the friction voice over the drone, the half-note arp on seat 1 closing the intro
{ const s = SEC.after, tr = trOf('after');
  tapeStop(s.t); mute(s.t, s.t + s.barLen, .5);
  drone(s.t + s.barLen, 7 * s.barLen + 2, 54 + tr, .12, 4); wind(s.t + s.barLen, 7 * s.barLen, .6);
  const fr = novelizer('frictus', SIGH.map(([a, d, m]) => [+hz(m + tr - 12).toFixed(2), +(a * s.beat).toFixed(4), +(d * s.beat * .95).toFixed(4), .7]), `sigh-${tr}`);
  if (!place(L.voice[CENTER], fr, s.t + s.barLen, .55, { kind: 'frictus', f: hz(73 + tr - 12) })) play(s, SIGH, s.t + s.barLen, () => CENTER, (k, t, d, m, g) => voice(k, t, d, m, g, 'church'), .45, .92, tr);
  for (const [a, d, m] of SIGH) ghost(s.t + s.barLen + a * s.beat, d * s.beat, m + tr, .4, s.beat);
  arp(s, { from: 5, to: 8, rate: .5, octaves: 1, base: 12, inst: pluck, g: .28, chordFn: () => CH.Bm.map(m => m + tr), seatFn: () => 0, upDown: false }); }
// climb 3: the extreme break — thirty-seconds throughout, highest register, the hook at double speed in the new key, the one riser, prepared toward Fm
climb(SEC.climb3, { rate1: 8, rate2: 8, inst1: glock, inst2: glock, octaves1: 3, octaves2: 3, g: .26, base: 24, trNow: 3, trNext: 6, double: true, wubPreset: 'bomp', hook: true, rise: true });
// LIFT 3 (+6, 136 BPM): the peak. The theme once, lifted, answered; the tutti chord enters in pentagram order; the eight-turn spin
lift(SEC.lift3, { tr: 6, passes: 1, answer: true, liftOctave: true, guitarOn: true, padsOn: true, impact: 'explode', tutti: true, spinLast: true });
// outro (home): a bar of nothing; the opening phrase at half density in Bm; the ring hands back the last five notes; one knock from behind
{ const s = SEC.outro;
  mute(s.t, s.t + s.barLen, .6);
  GHOST = false; play(s, THEME.filter(([a]) => a < 8), s.t + s.barLen, () => CENTER, voice, .3, .92, 0); GHOST = true;
  THEME.slice(-5).reverse().forEach(([, , m], k) => pluck(L.walk[[0, 4, 3, 2, 1][k]], s.t + 2 * s.barLen + k * s.beat * 1.2, s.beat * 1.1, m, .3));
  tone(L.drums[3], s.end - 1.5 * s.beat, .27, 130, .35, { attack: .012, decay: .22 }); }

// ═══ the mix ══════════════════════════════════════════════════════════
// the ghost is crushed; the ring drums are clipped; nothing touches the sub, the glock or the bell
for (const k of [...REAR, 4]) applyBitcrush(L.ghost[k], { bits: 6, downsample: 6, mix: .65, sampleRate: SR });
{ const d = L.drums[0], fs = [biquad('peak', 300, 1.2, -3), biquad('peak', 2500, 1.5, 2)]; for (let i = 0; i < N; i++) { let x = d[i]; for (const f of fs) x = f(x); d[i] = x; } for (const t of kicks) { const s0 = Math.round(t * SR), n = Math.round(.015 * SR); for (let i = 0; i < n && s0 + i < N; i++) d[s0 + i] *= 1 + .4 * (1 - i / n); } }
for (let k = 0; k < RING; k++) softClip(L.drums[k], 1.7);
// sidechain: every kick ducks pads, bass, guitar and (lightly) the lead and walk; never the wub or the ghost
const duck = new Float32Array(N).fill(1);
for (const t of kicks) { const s0 = Math.round(t * SR); for (let i = 0; i < SR * .45 && s0 + i < N; i++) { const x = i / SR, g = x < .005 ? 1 - .68 * (x / .005) : .32 + .68 * (1 - Math.exp(-(x - .005) / .16)); duck[s0 + i] = Math.min(duck[s0 + i], Math.min(1, g)); } }
const apply = (lay, depth) => { for (const buf of lay) for (let i = 0; i < N; i++) buf[i] *= 1 - depth * (1 - duck[i]); };
// electro: everything but the drums pumps on the kick
apply(L.pad, 1); apply(L.bass, 1); apply(L.guitar, .9); apply(L.lead, .75); apply(L.walk, .7); apply(L.wubsub, .6); apply(L.wub, 1); apply(L.growl, .8); apply(L.ghost, .6); apply(L.voice, .45); apply(L.fx, .5);
// the level arc: two arches, one peak; the climbs ramp across their whole length
const ARC = { intro: [.45, .45], verse1: [.55, .55], climb1: [.6, .8], lift1: [.85, .85], still: [.35, .35], verse2: [.55, .55], climb2: [.8, .95], lift2: [1, 1], after: [.4, .4], climb3: [.9, 1.05], lift3: [1.15, 1.15], outro: [.35, .35] };
const arcAt = t => { let prev = ARC.intro[1]; for (const [n] of FORM) { const s = SEC[n], [a, b] = ARC[n]; if (t < s.t - 2 * s.beat) return prev; if (t < s.t) return prev + (a - prev) * (1 - (s.t - t) / (2 * s.beat)); if (t < s.end) return a + (b - a) * (t - s.t) / (s.end - s.t); prev = b; } return prev; };
const arc = new Float32Array(N); for (let i = 0; i < N; i += 64) { const g = arcAt(i / SR); for (let j = i; j < Math.min(N, i + 64); j++) arc[j] = g; }
// the field turns: the blast displaces every ring voice 0.4 of a seat and springs back; the spin is eight turns, quintic
const ease5 = u => u * u * u * (u * (u * 6 - 15) + 10);
const shiftAt = t => { let f = 0; for (const p of turns) { if (p.type === 'blast' && t >= p.t && t < p.t + 4.8) { const x = t - p.t, w = 2 * Math.PI * .92; f += .4 * Math.exp(-.58 * w * x) * Math.cos(w * x); } if (p.type === 'spin') { if (t >= p.t1) f += p.turns * RING; else if (t >= p.t0) f += p.turns * RING * ease5((t - p.t0) / (p.t1 - p.t0)); } } return f; };
function turnLayer(lay) { const out = Array.from({ length: RING }, () => new Float32Array(N)); let sh = 0; for (let i = 0; i < N; i++) { if ((i & 255) === 0) sh = shiftAt(i / SR); const f = Math.floor(sh), u = sh - f; for (let k = 0; k < RING; k++) { const v = lay[k][i]; if (v === 0) continue; const a = (((k + f) % RING) + RING) % RING, b = (a + 1) % RING; out[a][i] += v * (1 - u); out[b][i] += v * u; } } for (let k = 0; k < RING; k++) lay[k] = out[k]; }
for (const name of ['lead', 'walk', 'pad', 'ghost', 'guitar', 'fx', 'growl']) turnLayer(L[name]);
// ── depth: distance, a hall around the ring, and dub throws that walk the seats ──
// Far layers are darker and quieter (air), and send more to the hall; near layers stay dry.
// The hall is convolved per seat before the head, so the reverb has a direction. The throws are a
// dotted-eighth tape delay whose repeats step one seat clockwise each time (bass-platter R9:
// returns high-passed at 100 Hz, low-passed at 4.5 kHz; the sub never goes to a send).
const FAR = { pad: [3500, .8], ghost: [3200, .85], growl: [5000, .9], fx: [4500, .9] };
for (const [name, [cut, g]] of Object.entries(FAR)) for (let k = 0; k < RING; k++) { const f = biquad('lp', cut, .7), b = L[name][k]; for (let i = 0; i < N; i++) b[i] = f(b[i]) * g; }
const SEND = { pad: .55, ghost: .6, growl: .35, fx: .35, lead: .22, voice: .3, walk: .18, guitar: .18 };
const WET = layer();
for (const [name, g] of Object.entries(SEND)) for (let k = 0; k < RING + 1; k++) { const src = L[name][k], dst = WET[k]; for (let i = 0; i < N; i++) dst[i] += src[i] * g; }
{ // the hall: a long room from bin/room-ir.mjs (tail scale 3 ≈ 5 s), convolved per seat by ffmpeg
  const irPath = join(HERE, '../out/hall-ir-rt3.wav'); if (!existsSync(irPath)) spawnSync('node', [join(HERE, 'room-ir.mjs'), irPath, '3'], { stdio: 'inherit' });
  const { samples: irMono } = readWavMono(irPath); const irF = join(WORK, 'hall.f32'); writeFileSync(irF, Buffer.from(irMono.buffer, irMono.byteOffset, irMono.byteLength));
  for (let k = 0; k <= CENTER; k++) {
    const inF = join(WORK, `wet${k}.f32`), outF = join(WORK, `hall${k}.f32`); writeFileSync(inF, Buffer.from(WET[k].buffer));
    const r = spawnSync('ffmpeg', ['-y', '-v', 'error', '-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', inF, '-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', irF, '-filter_complex', '[0:a][1:a]afir=gtype=none:dry=0:wet=1,highpass=f=100,lowpass=f=4500[out]', '-map', '[out]', '-f', 'f32le', '-c:a', 'pcm_f32le', outF]);
    if (r.status !== 0) { console.warn('hall failed for seat', k); continue; }
    const h = new Float32Array(readFileSync(outF).buffer); const dst = L.fx[k]; for (let i = 0; i < N && i < h.length; i++) dst[i] += h[i] * .8;
  }
}
{ // the throws: dotted eighth at 128, five repeats, each one seat further round, darker each time
  const D = Math.round(.75 * (60 / 128) * SR), THROW = { voice: .28, lead: .22 };
  for (const [name, g] of Object.entries(THROW)) for (let k = 0; k <= CENTER; k++) {
    const src = L[name][k]; if (!src.some(v => v !== 0)) continue;
    const hp = biquad('hp', 100, .7); let tap = new Float32Array(N); for (let i = 0; i < N; i++) tap[i] = hp(src[i]) * g;
    for (let n = 1; n <= 5; n++) { const lp = biquad('lp', 4500 * .8 ** n, .7), gain = .55 ** n, seat = k === CENTER ? n % RING : (k + n) % RING, dst = L.fx[seat]; for (let i = 0; i + n * D < N; i++) dst[i + n * D] += lp(tap[i]) * gain; }
  }
}
// mix the layers into seat feeds
const MIX = { drums: 1, sub: .7, wubsub: 1, bass: .9, wub: 1, growl: .8, walk: .85, pad: .9, lead: .85, voice: .95, ghost: .9, guitar: .8, fx: .9 };
const feeds = layer();
for (const [name, lay] of Object.entries(L)) for (let k = 0; k < OUTPUTS; k++) { const src = lay[k], dst = feeds[k], g = MIX[name]; for (let i = 0; i < N; i++) dst[i] += src[i] * g * arc[i]; }
// the tape stops: the mix itself slows to nothing over the 1.5 s before each stop
for (const T of stops) { const n = Math.round(1.5 * SR), s1 = Math.round(T * SR), s0 = s1 - n; for (const f of feeds) { const src = f.slice(s0, s1); let pos = 0; for (let i = 0; i < n; i++) { const u = i / n, rate = (1 - u) ** 2; pos += rate; const j = Math.floor(pos), fr = pos - j; f[s0 + i] = ((src[j] || 0) * (1 - fr) + (src[j + 1] || 0) * fr) * (1 - u * .3); } } }
// the silences: hard, with a 10 ms edge
for (const [t0, t1, fadeIn] of mutes) { const a = Math.round(t0 * SR), b = Math.round(t1 * SR), e = Math.round(fadeIn * SR), e2 = Math.round(.01 * SR); for (const f of feeds) for (let i = a; i < b && i < N; i++) f[i] *= i < a + e ? (1 - (i - a) / e) ** 2 : i > b - e2 ? (b - i) / e2 : 0; }
for (let k = 0; k < RING; k++) { const f = feeds[k], b = BED[k]; for (let i = 0; i < N; i++) f[i] += b[i] * .9 * (.7 + .3 * arc[i]) * (1 - .4 * (1 - duck[i])); }
const rms = feeds.map(f => { let e = 0; for (const v of f) e += v * v; return (10 * Math.log10(e / f.length + 1e-12)).toFixed(1); });
console.log('feed RMS dBFS  ring', rms.slice(0, RING).join(' '), ' C', rms[CENTER], ' SUB', rms[SUB]);

// ── the head: measured KEMAR, seat energies equalized (as notespatial-native-render.mjs) ──
const binPath = join(ROOT, 'fedac/native/tools/hrir/kemar-compact.bin'), idxPath = join(ROOT, 'fedac/native/tools/hrir/kemar-compact.json');
const idx = JSON.parse(readFileSync(idxPath, 'utf8')), raw = readFileSync(binPath), taps = idx.taps, el0 = idx.elevations.find(e => e.el === 0);
const ir = azDeg => { let az = ((azDeg % 360) + 360) % 360, swap = false; if (az > 180) { az = 360 - az; swap = true; } const a = el0.azimuths.reduce((b, c) => Math.abs(c.az - az) < Math.abs(b.az - az) ? c : b); const Lr = new Float32Array(taps), Rr = new Float32Array(taps); for (let i = 0; i < taps; i++) { Lr[i] = raw.readInt16LE(a.off + i * 2) / 32768; Rr[i] = raw.readInt16LE(a.off + taps * 2 + i * 2) / 32768; } let e = 0; for (let i = 0; i < taps; i++) e += Lr[i] * Lr[i] + Rr[i] * Rr[i]; const gEq = Math.sqrt(1.84 / e); for (let i = 0; i < taps; i++) { Lr[i] *= gEq; Rr[i] *= gEq; } return swap ? [Rr, Lr] : [Lr, Rr]; };
const inputs = [], graph = [];
for (let k = 0; k < OUTPUTS; k++) { writeFileSync(join(WORK, `seat${k}.f32`), Buffer.from(feeds[k].buffer)); const [Lr, Rr] = ir(seatAz(k)); writeFileSync(join(WORK, `ir${k}L.f32`), Buffer.from(Lr.buffer)); writeFileSync(join(WORK, `ir${k}R.f32`), Buffer.from(Rr.buffer)); inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(WORK, `seat${k}.f32`)); }
for (let k = 0; k < OUTPUTS; k++) for (const ear of 'LR') inputs.push('-f', 'f32le', '-ar', String(SR), '-ac', '1', '-i', join(WORK, `ir${k}${ear}.f32`));
for (let k = 0; k < OUTPUTS; k++) { const w = k === CENTER ? 1.1 : 1; graph.push(`[${k}:a]asplit[s${k}a][s${k}b]`, `[s${k}a][${OUTPUTS + k * 2}:a]afir=gtype=none:dry=1:wet=${w}[l${k}]`, `[s${k}b][${OUTPUTS + k * 2 + 1}:a]afir=gtype=none:dry=1:wet=${w}[r${k}]`); }
graph.push(Array.from({ length: OUTPUTS }, (_, k) => `[l${k}]`).join('') + `amix=inputs=${OUTPUTS}:normalize=0[L]`, Array.from({ length: OUTPUTS }, (_, k) => `[r${k}]`).join('') + `amix=inputs=${OUTPUTS}:normalize=0[R]`, '[L][R]join=inputs=2:channel_layout=stereo[out]');
const mixed = join(WORK, 'binaural.f32');
const r = spawnSync('ffmpeg', ['-y', '-v', 'error', ...inputs, '-filter_complex', graph.join(';'), '-map', '[out]', '-f', 'f32le', '-c:a', 'pcm_f32le', mixed], { stdio: 'inherit' });
if (r.status !== 0) throw Error('ffmpeg binaural mix failed');
const st = new Float32Array(readFileSync(mixed).buffer), n = st.length >> 1;
let pk = 0; for (const v of st) pk = Math.max(pk, Math.abs(v));
const g = .95 / pk, pcm = Buffer.alloc(44 + n * 8);
pcm.write('RIFF', 0); pcm.writeUInt32LE(36 + n * 8, 4); pcm.write('WAVE', 8); pcm.write('fmt ', 12); pcm.writeUInt32LE(16, 16); pcm.writeUInt16LE(3, 20); pcm.writeUInt16LE(2, 22); pcm.writeUInt32LE(SR, 24); pcm.writeUInt32LE(SR * 8, 28); pcm.writeUInt16LE(8, 32); pcm.writeUInt16LE(32, 34); pcm.write('data', 36); pcm.writeUInt32LE(n * 8, 40);
for (let i = 0; i < n; i++) { pcm.writeFloatLE(st[i * 2] * g, 44 + i * 8); pcm.writeFloatLE(st[i * 2 + 1] * g, 48 + i * 8); }
writeFileSync(OUT, pcm);
if (EVENTS) writeFileSync(EVENTS, JSON.stringify({ sr: SR, dur: DUR, form: FORM, sec: SEC, tr: TR, arc: ARC, mutes, kicks: kicks.length, layers: Object.keys(L), events: EV }));
const total = FORM.reduce((a, [, b]) => a + b, 0);
console.log(`${total} bars · ${Math.floor(LAST.end / 60)}:${String(Math.round(LAST.end % 60)).padStart(2, '0')} · ${kicks.length} kicks · ${OUT}`);
console.log(FORM.map(([n]) => `${n} ${Math.floor(SEC[n].t / 60)}:${String(Math.floor(SEC[n].t % 60)).padStart(2, '0')}`).join('  '));
