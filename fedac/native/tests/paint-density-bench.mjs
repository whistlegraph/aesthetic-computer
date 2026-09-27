#!/usr/bin/env node
// paint-density-bench.mjs — how many draw calls a piece's paint makes per
// frame at a score's densest second, with a counting fake of the native API.
//
//   node fedac/native/tests/paint-density-bench.mjs <piece.mjs> <score.nsscore> [seat=0] [seconds=t1,t2,…]
//
// Drives boot → prepare → play → sim at the given score seconds and calls
// paint once per second sampled, printing write/box/line/ink/circle counts
// and paint wall time. No audio, no display: sound.synth returns a stub.
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
const [pieceArg, scoreArg, seatArg = '0', secondsArg] = process.argv.slice(2);
if (!pieceArg || !scoreArg) { console.error('usage: paint-density-bench.mjs <piece.mjs> <score.nsscore> [seat] [t1,t2,…]'); process.exit(2); }
const piece = await import(resolve(pieceArg));
const scoreBytes = readFileSync(resolve(scoreArg));
const score = JSON.parse(scoreBytes);
const seat = Number(seatArg), seats = score.seats || 6;
let densest = 0, best = 0; const per = new Map();
for (const lane of score.lanes || []) for (const e of lane.events || []) { const s = Math.floor(e.t); per.set(s, (per.get(s) || 0) + 1); }
for (const [s, n] of per) if (n > best) { best = n; densest = s; }
const seconds = secondsArg ? secondsArg.split(',').map(Number) : [densest + .3, densest + .5, densest + .8];
const files = {
  '/pieces/spatial-rehearsal-config.json': JSON.stringify({ seat, seats, machineName: 'bench', maxSeconds: score.dur }),
  '/pieces/spatial-rehearsal.nsscore': scoreBytes,
  '/pieces/trio-fleet-config.json': readFileSync(resolve(scoreArg), 'utf8'),
};
let now = 100, cmd = null, status = null;
const counts = { write: 0, box: 0, line: 0, ink: 0, circle: 0 };
const api = {
  system: { readFile: (p) => (p.endsWith('command.json') ? JSON.stringify(cmd || {}) : files[p] ?? ''), readFileBytes: (p) => { const v = files[p]; return v instanceof Buffer ? v.buffer.slice(v.byteOffset, v.byteOffset + v.byteLength) : Buffer.from(String(v)).buffer; }, writeFile: (p, v) => { if (String(p).includes('status')) { try { status = JSON.parse(v); } catch {} } }, battery: { percent: 80, charging: false, minutesLeft: 200 }, startSSH: () => {}, brightness: 100, brightnessAdjust: () => {}, dmxSend: () => true, pty: { spawn: () => {} } },
  sound: { get time() { return now; }, microphone: { close() {}, hot: false, recording: false }, speaker: { amplitudes: { left: .1, right: .1 } }, synth: () => ({ kill() {}, update() {} }), volume: { setMono() {}, setMonoOutput() {}, setMix() {}, mono: true, monoOutput: 'left', mix: .25 }, deck: { load: () => true, pause() {}, play() {}, seek() {}, setSpeed() {}, setCrossfader() {}, setVolume() {}, setMasterVolume() {}, decks: [{ loaded: true, duration: score.dur, position: 0, playing: false, error: '' }] }, room: { setMix() {} }, drive: { setMix() {} }, wobble: { setMix() {} }, glitch: { setMix() {} } },
  screen: { width: 1366, height: 768 }, wifi: { ip: '192.168.1.0' },
  wipe: () => {}, ink: () => counts.ink++, box: () => counts.box++, line: () => counts.line++, circle: () => counts.circle++, write: () => counts.write++,
};
piece.boot?.(api);
cmd = { id: 'p1', action: 'prepare', startAt: now + 3, arrangementHash: score.arrangementHash, runId: 'bench' }; now += .02; piece.sim?.(api);
cmd = { id: 'p2', action: 'play', runId: 'bench' }; now += .02; piece.sim?.(api);
const origin = now + 3 - .04;
// through the downbeat on time (the Trio piece calls >100 ms late a missed downbeat), then on
cmd = { id: 'ka-arm', action: 'keepalive', runId: 'bench' }; now = origin - 1.5; piece.sim?.(api);
cmd = { id: 'ka-down', action: 'keepalive', runId: 'bench' }; now = origin - .05; piece.sim?.(api); now = origin + .02; piece.sim?.(api);
let walked = .02, ka = 0;
for (const t of seconds) {
  // walk sim to t in 50 ms steps so cursors and voices are where they would be; keep the heartbeat alive
  for (let s = walked; s <= t; s += .05) { now = origin + s; if (s - ka > 1) { ka = s; cmd = { id: 'ka' + s.toFixed(2), action: 'keepalive', runId: 'bench' }; } piece.sim?.(api); }
  walked = t; now = origin + t; piece.sim?.(api);
  for (const k in counts) counts[k] = 0;
  const t0 = performance.now(); piece.paint?.(api); const ms = performance.now() - t0;
  console.log(`t=${t.toFixed(1)}s  phase ${status?.phase ?? '?'}${status?.error ? ' ERROR ' + status.error : ''}  write ${counts.write}  box ${counts.box}  line ${counts.line}  ink ${counts.ink}  circle ${counts.circle}  paint ${ms.toFixed(2)} ms (node, no raster)`);
}
