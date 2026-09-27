#!/usr/bin/env node
// Frame-cost check for the spatial-rehearsal piece: drive boot/sim/paint
// through a whole score at a fixed frame rate with counting stubs for the
// runtime, and report, per chapter, the JS time per frame on this machine
// and the peak number of primitives a frame asks the raster for. The
// laptops run QuickJS on a software raster, so the absolute numbers here
// are not theirs; the shape is. Two things transfer: the count of draw
// calls per frame (each is a JS→C crossing and a fill on the laptop) and
// any per-frame work that grows with the score rather than with what is
// on screen.
//
//   node notespatial-native-perf.mjs [score.nsscore] [--seat 3] [--fps 30] [--size 1366x768]
//   node notespatial-native-perf.mjs --all          # every seat, summary only

import { readFileSync } from 'node:fs';
import { performance } from 'node:perf_hooks';

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf('--' + k); return i >= 0 ? args[i + 1] : d; };
const file = args.find(a => a.endsWith('.nsscore')) || new URL('../scores/notespatial-native.nsscore', import.meta.url);
const score = JSON.parse(readFileSync(file, 'utf8'));
const FPS = +opt('fps', 30), [W, H] = opt('size', '1366x768').split('x').map(Number);
const seats = score.seats || 6;
const seatList = args.includes('--all') ? Array.from({ length: seats }, (_, i) => i) : [+opt('seat', 3) - 1];
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;

async function run(seat) {
  const piece = await import('../pieces/spatial-rehearsal.mjs?seat=' + seat + '&' + Math.random());
  let cmd = null;
  const system = {
    startSSH() {},
    readFile(p) {
      if (p.endsWith('config.json')) return JSON.stringify({ seat, seats, maxSeconds: score.dur, machineName: 'perf' });
      if (p.endsWith('presence.json')) return JSON.stringify({ seats: Array.from({ length: seats }, (_, i) => ({ seat: i, state: 'online' })) });
      return JSON.stringify(cmd);
    },
    readFileBytes(p) { return p.endsWith('.nsscore') ? new TextEncoder().encode(JSON.stringify(score)).buffer : null; },
    writeFile() { counts.status++; },
    battery: { percent: 80, charging: true },
  };
  const counts = { status: 0, synth: 0, update: 0, box: 0, line: 0, write: 0, circle: 0, ink: 0 };
  const sound = {
    time: 0, microphone: { hot: false, recording: false, close() {} },
    speaker: { amplitudes: { left: .05, right: .05 } },
    room: { setMix() {} }, drive: { setMix() {} }, wobble: { setMix() {} }, glitch: { setMix() {} },
    synth() { counts.synth++; return { update() { counts.update++; }, kill() {} }; },
  };
  const screen = { width: W, height: H };
  const api = { sound, system, screen, wifi: { ip: '0.0.0.0' } };
  const paintApi = { ...api, wipe() {}, ink() { counts.ink++; }, box() { counts.box++; }, line() { counts.line++; }, circle() { counts.circle++; }, write() { counts.write++; } };
  piece.boot(api);
  cmd = { id: 'prepare', action: 'prepare', startAt: 3, mode: 'score' }; sound.time = .1; piece.sim(api);
  cmd = { id: 'play', action: 'play' }; sound.time = .2; piece.sim(api);

  const chapters = (score.movements || [{ name: 'all', t0: 0, t1: score.dur }]).map(m => ({ ...m, frames: 0, sim: 0, paint: 0, worst: 0, worstAt: 0, peak: { box: 0, line: 0, write: 0, total: 0, at: 0 } }));
  const dt = 1 / FPS;
  let worstFrame = { ms: 0, t: 0, box: 0, line: 0, write: 0 };
  for (let t = 0; t < score.dur; t += dt) {
    sound.time = 3 + t;
    const before = { ...counts };
    const a = performance.now(); piece.sim(api); const b = performance.now(); piece.paint(paintApi); const c = performance.now();
    const ch = chapters.find(m => t >= m.t0 && t < m.t1); if (!ch) continue;
    const box = counts.box - before.box, line = counts.line - before.line, write = counts.write - before.write, total = box + line + write;
    const ms = c - a;
    ch.frames++; ch.sim += b - a; ch.paint += c - b;
    if (ms > ch.worst) { ch.worst = ms; ch.worstAt = t; }
    if (total > ch.peak.total) ch.peak = { box, line, write, total, at: t };
    if (ms > worstFrame.ms) worstFrame = { ms, t, box, line, write };
  }
  return { chapters, counts, worstFrame };
}

for (const seat of seatList) {
  const { chapters, counts, worstFrame } = await run(seat);
  const label = seat === score.center ? 'C' : String(seat + 1);
  console.log(`\n${score.name} — seat ${label} at ${FPS} fps, ${W}x${H} — JS cost on this machine (QuickJS on the laptop is slower; compare shapes, not numbers)`);
  console.log('chapter            frames  sim ms/f  paint ms/f  worst ms  @      peak draw calls/frame (box+line+write)  @');
  for (const ch of chapters) {
    if (!ch.frames) continue;
    console.log(`${ch.name.padEnd(18)} ${String(ch.frames).padStart(6)}  ${(ch.sim / ch.frames).toFixed(3).padStart(8)}  ${(ch.paint / ch.frames).toFixed(3).padStart(10)}  ${ch.worst.toFixed(2).padStart(8)}  ${mmss(ch.worstAt).padStart(5)}  ${String(ch.peak.total).padStart(5)} (${ch.peak.box}+${ch.peak.line}+${ch.peak.write})`.padEnd(112) + mmss(ch.peak.at));
  }
  const frames = chapters.reduce((a, c) => a + c.frames, 0);
  console.log(`totals: ${counts.synth} synths, ${counts.update} voice updates (${(counts.update / frames).toFixed(1)}/frame), ${counts.status} status writes, ${counts.line} lines, ${counts.box} boxes, ${counts.write} labels; worst frame ${worstFrame.ms.toFixed(2)} ms at ${mmss(worstFrame.t)} drawing ${worstFrame.line} lines, ${worstFrame.box} boxes, ${worstFrame.write} labels`);
}
