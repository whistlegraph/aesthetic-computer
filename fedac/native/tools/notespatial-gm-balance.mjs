#!/usr/bin/env node
// Measure the actual score's GM notes at unit gain. One trim per program
// preserves written accents, envelopes and chapter dynamics.
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import { renderGmBank } from './notespatial-gm-audio.mjs';
import { GM_NAMES } from './notespatial-orchestra.mjs';

export function noteLevel(samples, sampleRate) {
  // Strongest 100 ms RMS window: compare a pluck's audible body with a
  // sustained tone without boosting the pluck for its silent release tail.
  const window = Math.min(samples.length, Math.round(sampleRate * .1));
  let energy = 0, strongest = 0, peak = 0;
  for (let i = 0; i < samples.length; i++) {
    const v = samples[i];
    if (!Number.isFinite(v)) throw Error('Nonfinite calibration audio');
    peak = Math.max(peak, Math.abs(v));
    energy += v * v;
    if (i >= window) energy -= samples[i - window] ** 2;
    if (i >= window - 1) strongest = Math.max(strongest, energy / window);
  }
  return { rms: Math.sqrt(strongest), peak };
}

export const median = a => { const s = [...a].sort((a, b) => a - b), i = Math.floor(s.length / 2); return s.length % 2 ? s[i] : (s[i - 1] + s[i]) / 2; };
export function balanceMeasurements(rows, target) {
  return rows.map(row => {
    // Limit boosts to +6 dB; a very quiet/aperiodic patch must not turn
    // into an amplified noise bed. Peaks also limit the allowed boost.
    const gain = Math.max(10 ** (-24 / 20), Math.min(2, target / Math.max(row.rms, 1e-9), 1.8 / Math.max(row.peak, 1e-9)));
    return { ...row, gain: +gain.toFixed(6), db: +(20 * Math.log10(gain)).toFixed(2) };
  });
}

if (process.argv[1] && new URL(import.meta.url).pathname === process.argv[1]) {
  const score = JSON.parse(readFileSync(process.argv[2] || new URL('../scores/notespatial-native-gm128-fx-kick.nsscore', import.meta.url)));
  const work = mkdtempSync(join(tmpdir(), 'notespatial-balance-')), sampleRate = 44100;
  const events = score.lanes.flatMap(l => l.events).filter(e => Number.isInteger(e.gm));
  let bank;
  try {
    bank = await renderGmBank(events, { work, sampleRate });
    const measurements = Array.from({ length: 128 }, () => []);
    for (const e of events) measurements[e.gm].push(noteLevel(bank.read(e), sampleRate));
    const rows = measurements.map((notes, program) => {
      if (!notes.length) throw Error(`GM ${program} missing from calibration score`);
      return { program, name: GM_NAMES[program], notes: notes.length, rms: median(notes.map(n => n.rms)), peak: Math.max(...notes.map(n => n.peak)) };
    });
    const target = .18; // leave space for the existing percussion and mix effects
    const programs = balanceMeasurements(rows, target);
    const coreHash = createHash('sha256').update(readFileSync(new URL('../src/gm_synth.c', import.meta.url))).digest('hex');
    const result = { method: 'Median strongest 100 ms RMS of each program’s notes in the suite; target .18; -24 to +6 dB trim; unit-voice peak ceiling 1.8', sampleRate, coreHash, target, programs };
    const out = process.argv[3] || new URL('./notespatial-gm-levels.json', import.meta.url);
    writeFileSync(out, JSON.stringify(result, null, 2) + '\n');
    console.log(`Target RMS ${target.toFixed(4)}; raw range ${(20 * Math.log10(Math.max(...rows.map(r => r.rms)) / Math.min(...rows.map(r => r.rms)))).toFixed(1)} dB`);
    for (const r of programs.sort((a, b) => a.db - b.db)) console.log(`${String(r.program + 1).padStart(3)} ${r.name.padEnd(20)} ${r.rms.toFixed(4)} ${r.db.toFixed(1)} dB`);
  } finally { bank?.close(); rmSync(work, { recursive: true, force: true }); }
}
