import { clamp, move } from './world.mjs';

// Only input crosses the authority boundary: never client positions, damage or dt.
export function command(raw) {
  if (!raw || !Number.isSafeInteger(raw.seq) || raw.seq < 1 ||
      !['fwd', 'right', 'yaw', 'pitch', 'viewTick'].every(k => Number.isFinite(raw[k]))) return null;
  return { seq: raw.seq, fwd: clamp(raw.fwd, -1, 1), right: clamp(raw.right, -1, 1),
    yaw: ((raw.yaw % 360) + 360) % 360, pitch: clamp(raw.pitch, -85, 85),
    buttons: raw.buttons === 1 ? 1 : 0, fire: raw.fire === true,
    viewTick: Math.floor(raw.viewTick) };
}

export function reconcile(authority, pending, ack) {
  const remaining = pending.filter(c => c.seq > ack);
  return { player: remaining.reduce((p, c) => move(p, c), { ...authority }), pending: remaining };
}

export function interpolate(history, tick, seat) {
  if (!history.length) return null;
  let a = history[0], b = history.at(-1);
  for (const snap of history) {
    if (snap.tick <= tick) a = snap;
    if (snap.tick >= tick) { b = snap; break; }
  }
  const p = a.players[seat], q = b.players[seat];
  if (!p || !q) return q || p;
  if (p.life !== q.life || p.alive !== q.alive) return tick < b.tick ? p : q;
  const t = a.tick === b.tick ? 0 : clamp((tick - a.tick) / (b.tick - a.tick), 0, 1);
  const out = { ...q };
  for (const key of ['x', 'y', 'z', 'pitch']) out[key] = p[key] + (q[key] - p[key]) * t;
  const turn = ((q.yaw - p.yaw + 540) % 360) - 180;
  out.yaw = p.yaw + turn * t;
  return out;
}
