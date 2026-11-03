import { clamp, move, HZ } from './world.mjs';

// Only input crosses the authority boundary: never client positions, damage or dt.
export function command(raw) {
  if (
    !raw ||
    !Number.isSafeInteger(raw.seq) ||
    raw.seq < 1 ||
    !['fwd', 'right', 'yaw', 'pitch'].every((k) => Number.isFinite(raw[k]))
  )
    return null;
  return {
    seq: raw.seq,
    fwd: clamp(raw.fwd, -1, 1),
    right: clamp(raw.right, -1, 1),
    yaw: ((raw.yaw % 360) + 360) % 360,
    pitch: clamp(raw.pitch, -85, 85),
    buttons: raw.buttons === 1 ? 1 : 0,
    fire: raw.fire === true,
    use: raw.use === true,
    drop: raw.drop === true,
    chalk:
      raw.chalk && ['x', 'y', 'z'].every((k) => Number.isFinite(raw.chalk[k]))
        ? { x: clamp(raw.chalk.x, -18, 18), y: clamp(raw.chalk.y, 0, 12), z: clamp(raw.chalk.z, -18, 18) }
        : null,
  };
}

export function reconcile(authority, pending, ack, objects = [], tick = 0) {
  const remaining = pending.filter((c) => c.seq > ack);
  return {
    player: remaining.reduce((p, c, i) => move(p, c, objects, (tick + i + 1) / HZ), {
      ...authority,
    }),
    pending: remaining,
  };
}

export function interpolate(history, tick, seat) {
  if (!history.length) return null;
  let a = history[0],
    b = history.at(-1);
  for (const snap of history) {
    if (snap.tick <= tick) a = snap;
    if (snap.tick >= tick) {
      b = snap;
      break;
    }
  }
  const p = a.players[seat],
    q = b.players[seat];
  if (!p || !q) return q || p;
  if (p.life !== q.life || p.alive !== q.alive) return tick < b.tick ? p : q;
  const t = a.tick === b.tick ? 0 : clamp((tick - a.tick) / (b.tick - a.tick), 0, 1);
  const out = { ...q };
  for (const key of ['x', 'y', 'z', 'pitch']) out[key] = p[key] + (q[key] - p[key]) * t;
  const turn = ((q.yaw - p.yaw + 540) % 360) - 180;
  out.yaw = p.yaw + turn * t;
  return out;
}
