#!/usr/bin/env node
// wub-batch.mjs — hear the wub alone, and three ways of rethinking it, over the
// same eight bars (Bm Bm G G Em Em F# F#, 128 BPM, half-bar notes, a plain kick
// for context). Every candidate shares one clean sine sub in front; only the
// mid layer changes. Peak-normalized, stereo, 44.1 kHz.
//   node pop/notespatial/bin/wub-batch.mjs   → out/wub/0-current-bomp.wav … 3-accordion-reed.wav
import { writeFileSync, mkdirSync, existsSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { mixEventWobble } from '../../dance/synths/wobble.mjs';
import { renderSkrill } from '../../dance/synths/skrill.mjs';
import { softClip } from '../../dance/synths/fx.mjs';
import { readWavMono } from '../../lib/wav.mjs';
const HERE = dirname(fileURLToPath(import.meta.url)), ROOT = resolve(HERE, '../../..'), OUT = join(HERE, '../out/wub'); mkdirSync(OUT, { recursive: true });
const SR = 44100, BPM = 128, BEAT = 60 / BPM, BAR = 4 * BEAT, BARS = 8, N = Math.ceil((BARS * BAR + 2) * SR);
const hz = m => 440 * 2 ** ((m - 69) / 12);
const ROOTS = [35, 35, 31, 31, 28, 28, 30, 30]; // B1 B1 G1 G1 E1 E1 F#1 F#1 — the track's wub pitch (root − 24)
const NOTES = []; for (let bar = 0; bar < BARS; bar++) for (const b of [0, 2]) NOTES.push({ t: bar * BAR + b * BEAT, dur: 2 * BEAT * .96, m: ROOTS[bar] });
const SDK = { ...process.env, SDKROOT: process.env.SDKROOT || '/Library/Developer/CommandLineTools/SDKs/MacOSX26.sdk' };
const lp1 = (buf, f) => { const c = 1 - Math.exp(-2 * Math.PI * f / SR); let y = 0; for (let i = 0; i < buf.length; i++) { y += c * (buf[i] - y); buf[i] = y; } };
const hp1 = (buf, f) => { const c = 1 - Math.exp(-2 * Math.PI * f / SR); let y = 0; for (let i = 0; i < buf.length; i++) { y += c * (buf[i] - y); buf[i] -= y; } };
function tone(buf, t, dur, f, g, attack = .005, decay = dur * .5) { const s0 = Math.round(t * SR), n = Math.round(dur * SR), a = attack * SR, d = decay * SR; let ph = 0; for (let i = 0; i < n && s0 + i < buf.length; i++) { const env = i < a ? i / a : i > n - d ? Math.exp(-4 * (i - (n - d)) / d) : 1; ph += f / SR; buf[s0 + i] += Math.sin(ph * 2 * Math.PI) * env * g; } }
function context() { // the kick on every beat, and the clean sub every candidate shares
  const kick = new Float32Array(N), sub = new Float32Array(N);
  for (let bar = 0; bar < BARS; bar++) for (let b = 0; b < 4; b++) { const s0 = Math.round((bar * BAR + b * BEAT) * SR); let ph = 0; for (let i = 0; i < SR * .3; i++) { const x = i / SR, f = 42 + 118 * Math.exp(-x / .028); ph += f / SR; kick[s0 + i] += Math.sin(ph * 2 * Math.PI) * Math.exp(-x / .14) * .8; } }
  for (const { t, dur, m } of NOTES) tone(sub, t, dur, hz(m), .5, .004, .15);
  return { kick, sub };
}
function duck(buf) { for (let bar = 0; bar < BARS; bar++) for (let b = 0; b < 4; b++) { const s0 = Math.round((bar * BAR + b * BEAT) * SR); for (let i = 0; i < SR * .25; i++) { const x = i / SR; buf[s0 + i] *= x < .01 ? 1 - .8 * x / .01 : .2 + .8 * (1 - Math.exp(-(x - .01) / .08)); } } }
function write(name, mid, midGain, note) {
  const { kick, sub } = context(); duck(mid);
  const L = new Float32Array(N), R = new Float32Array(N);
  for (let i = 0; i < N; i++) { const c = kick[i] + sub[i], m = mid[i] * midGain; L[i] = c + m * 1.0; R[i] = c + m * .9; }
  let pk = 0; for (let i = 0; i < N; i++) pk = Math.max(pk, Math.abs(L[i]), Math.abs(R[i])); const g = .9 / pk;
  const b = Buffer.alloc(44 + N * 8); b.write('RIFF', 0); b.writeUInt32LE(36 + N * 8, 4); b.write('WAVE', 8); b.write('fmt ', 12); b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20); b.writeUInt16LE(2, 22); b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 8, 28); b.writeUInt16LE(8, 32); b.writeUInt16LE(32, 34); b.write('data', 36); b.writeUInt32LE(N * 8, 40);
  for (let i = 0; i < N; i++) { b.writeFloatLE(L[i] * g, 44 + i * 8); b.writeFloatLE(R[i] * g, 48 + i * 8); }
  const wav = join(OUT, name + '.wav'); writeFileSync(wav, b);
  spawnSync('ffmpeg', ['-y', '-v', 'error', '-i', wav, '-c:a', 'libmp3lame', '-b:a', '256k', '-metadata', `title=${name} — ${note}`, join(OUT, name + '.mp3')]);
  console.log(name.padEnd(20), note);
}
// 0 · what the track has now: the wobble engine's bomp, split — the sub side here is the shared sub, the dirty side is the mid
{ const mid = new Float32Array(N); for (const { t, dur, m } of NOTES) mixEventWobble({ startSec: t, midi: m, durSec: dur, gain: .5 }, mid, { preset: 'bomp', bpm: BPM, sampleRate: SR, params: { subGain: 0, cutLo: 70, cutHi: 3500, q: 4, drive: 4.5, edge: .45 } }); softClip(mid, 3); hp1(mid, 130);
  write('0-current-bomp', mid, .6, 'wobble engine: detuned saws through one resonant low-pass, cutoff gated open/shut every eighth (Q 4), tanh 3'); }
// 1 · the skrill: FM growl with a swept formant pair, talking in vowels on a sample-and-hold eighth
{ const mid = new Float32Array(N); for (const { t, dur, m } of NOTES) { const seg = renderSkrill({ midi: m, durSec: dur, gain: .5, preset: 'talk' }, { bpm: BPM, sampleRate: SR, params: { subGain: 0, lfo: '1/2', q: 6, drive: 2.4 } }); const s0 = Math.round(t * SR); for (let i = 0; i < seg.length && s0 + i < N; i++) mid[s0 + i] += seg[i]; } hp1(mid, 110); lp1(mid, 5000);
  write('1-skrill-talk', mid, .7, 'skrill engine: FM growl into two formants morphing ee→ah→ow, stepping every half bar; no low-pass sweep'); }
// 2 · the two-mass creature: physically modelled vocal folds at the bass root, tanh and a cabinet
{ const bin = join(ROOT, 'pop/novelizer/build/twomass'), txt = join(OUT, 'twomass.txt'); writeFileSync(txt, NOTES.map(({ t, dur, m }) => `${hz(m + 12).toFixed(2)} ${t.toFixed(4)} ${dur.toFixed(4)} 0.85`).join('\n') + '\n');
  const r = spawnSync(bin, ['--notes', txt, '--out', OUT], { encoding: 'utf8', env: SDK });
  if (r.status === 0) { const { samples, sampleRate } = readWavMono(join(OUT, 'twomass-notes.wav')); const mid = new Float32Array(N); for (let i = 0; i < N; i++) { const x = i * sampleRate / SR, j = Math.floor(x); mid[i] = (samples[j] || 0) * (1 - (x - j)) + (samples[j + 1] || 0) * (x - j); } softClip(mid, 2.5); hp1(mid, 90); lp1(mid, 3200);
    write('2-twomass-growl', mid, .8, 'novelizer two-mass: vocal folds driven an octave above the sub, tanh 2.5, 3.2 kHz cabinet — a growl with real formants'); } else console.warn('twomass failed', r.stderr); }
// 3 · the accordion: three musette reeds a semitone of detune apart at the bass root, one bellows per note
{ const bin = join(ROOT, 'pop/accordion/c/accordion'), mid = new Float32Array(N);
  NOTES.forEach(({ t, dur, m }, k) => { const wav = join(OUT, `reed-${k}.wav`); const r = spawnSync(bin, ['--hz', hz(m + 12).toFixed(2), '--dur', dur.toFixed(3), '--vel', '.95', '--voices', '3', '--detune', '22', '--bellows', k % 2 ? 'pull' : 'push', '--tremor', '.5', '--sr', String(SR), '--seed', String(k + 1), '--out', wav], { encoding: 'utf8', env: SDK }); if (r.status !== 0) return console.warn('accordion failed', r.stderr); const { samples } = readWavMono(wav); const s0 = Math.round(t * SR); for (let i = 0; i < samples.length && s0 + i < N; i++) mid[s0 + i] += samples[i]; });
  softClip(mid, 2); hp1(mid, 90); lp1(mid, 4000);
  write('3-accordion-reed', mid, .8, 'accordion reeds: three detuned reeds on one bellows per note, push/pull alternating, tanh 2, 4 kHz cabinet'); }
