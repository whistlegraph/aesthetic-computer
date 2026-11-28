// 🔬 Watching someone over a real link, measured. One body walks and turns at
// a steady rate; its commands go up a link, the server runs them, snapshots
// come down another link to a watcher, and the watcher's picture is sampled
// frame by frame. A steady walk shown perfectly moves the same distance and
// turns the same angle every frame: `ripple` is how far the picture is from
// that (the spread of the per-frame step over its mean; 0 is perfect), and
// `stops` counts frames where the body hardly moved at all.
//   node rax/test/netcode-watch.mjs        prints the table
import { HZ, spawn } from '../world.mjs';
import { Match } from '../match.mjs';
import { SnapshotClock, SeatClocks } from '../presentation.mjs';
import { interpolate } from '../net.mjs';

// The links, as [name, up, down]: each { base, jitter, stall: [every, for] } in ms.
export const LINKS = [
  ['wired', { base: 15, jitter: 2 }, { base: 15, jitter: 2 }],
  ['wifi', { base: 30, jitter: 18 }, { base: 30, jitter: 18 }],
  ['wifi, clumping uplink', { base: 30, jitter: 18, stall: [700, 70] }, { base: 30, jitter: 12 }],
  ['phone', { base: 60, jitter: 40, stall: [900, 120] }, { base: 60, jitter: 40, stall: [1100, 120] }],
];

export function watch({ up, down, own = true, seconds = 12, fps = 60, seed = 11 }) {
  const random = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296);
  // A WebSocket keeps its order: nothing lands before what was sent before it.
  const link = (l) => { let last = 0; return (ms) => (last = Math.max(last, ms + l.base + random() * l.jitter + (l.stall && ms % l.stall[0] < l.stall[1] ? l.stall[1] - (ms % l.stall[0]) : 0))); };
  const upAt = link(up), downAt = link(down);
  const match = new Match(), seat = match.join();
  const playback = new SnapshotClock(), bodies = new SeatClocks();
  playback.reset();
  let history = [], packets = [], seq = 0, sent = [], nextClient = 0, nextServer = 5, nextFrame = 3, heard = 0;
  const shown = [];
  for (let ms = 0; ms < seconds * 1000; ms++) {
    for (let i = 0; i < packets.length; i++) {
      const packet = packets[i];
      if (packet.at > ms) continue;
      packets.splice(i--, 1);
      if (packet.commands) match.input(seat, packet.commands);
      else if (packet.snap.tick > heard) {
        heard = packet.snap.tick;
        history.push(packet.snap);
        history = history.slice(-64);
        playback.receive(packet.snap.tick, ms);
        bodies.receive(packet.snap.moves, packet.snap.players, ms);
      }
    }
    if (ms >= nextClient) {
      // Forward, turning ninety degrees a second: a steady walk and a steady aim.
      const c = { seq: ++seq, fwd: 1, right: 0, yaw: (seq * 90 / HZ) % 360, pitch: 0, buttons: 0 };
      sent = [...sent, c].slice(-3);
      packets.push({ at: upAt(ms), commands: sent });
      nextClient += 1000 / HZ;
    }
    if (ms >= nextServer) {
      match.step();
      packets.push({ at: downAt(ms), snap: JSON.parse(JSON.stringify(match.snapshot())) });
      nextServer += 1000 / HZ;
    }
    if (ms >= nextFrame) {
      const tick = playback.sample(ms), body = own ? bodies.sample(seat, ms) : null;
      const p = body ? interpolate(history, body.tick, seat, body.ahead, true) : interpolate(history, tick, seat, playback.ahead || 0);
      if (p && ms > 2000) shown.push({ x: p.x, z: p.z, yaw: p.yaw, behind: (seq - (body ? body.tick : (history.at(-1)?.moves?.[seat] ?? 0) - (playback.latest - tick))) * 1000 / HZ });
      nextFrame += 1000 / fps;
    }
  }
  const steps = shown.slice(1).map((p, i) => Math.hypot(p.x - shown[i].x, p.z - shown[i].z));
  const turns = shown.slice(1).map((p, i) => Math.abs(((p.yaw - shown[i].yaw + 540) % 360) - 180));
  const mean = (v) => v.reduce((a, b) => a + b, 0) / v.length;
  const ripple = (v) => { const m = mean(v); return Math.sqrt(mean(v.map((d) => (d - m) ** 2))) / m; };
  return {
    walk: ripple(steps), aim: ripple(turns),
    stops: steps.filter((d) => d < mean(steps) * 0.2).length / steps.length,
    behind: mean(shown.map((p) => p.behind)),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const pct = (n) => `${(n * 100).toFixed(1)}%`.padStart(7);
  console.log('link                     playback    walk ripple  aim ripple  stopped frames  behind');
  for (const [name, up, down] of LINKS)
    for (const own of [false, true]) {
      const r = watch({ up, down, own });
      console.log(`${name.padEnd(24)} ${(own ? 'own steps' : 'server tick').padEnd(11)} ${pct(r.walk)}      ${pct(r.aim)}     ${pct(r.stops)}         ${r.behind.toFixed(0)} ms`);
    }
}
