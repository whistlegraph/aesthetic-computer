import { clamp, move, HZ, WORLD, MAX_HALF } from './world.mjs';

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
    duck: raw.duck === true,
    bike: raw.bike === true, // 🛵 get on or off the cabin bike (bike.mjs)
    chalk:
      raw.chalk && ['x', 'y', 'z'].every((k) => Number.isFinite(raw.chalk[k]))
        ? { x: clamp(raw.chalk.x, -MAX_HALF, MAX_HALF), y: clamp(raw.chalk.y, 0, 30), z: clamp(raw.chalk.z, -MAX_HALF, MAX_HALF) }
        : null,
  };
}

export function reconcile(authority, pending, ack, objects = [], tick = 0, fixed = [], flat = false, open = null, half = WORLD) {
  const remaining = pending.filter((c) => c.seq > ack);
  return {
    player: remaining.reduce((p, c, i) => move(p, c, objects, (tick + i + 1) / HZ, fixed, flat, open, half), {
      ...authority,
    }),
    pending: remaining,
  };
}

// `ahead`: ticks the picture has run past the newest snapshot (the wire went
// quiet: SnapshotClock.ahead). The walk is carried on along its last heading
// for that long, across the ground only, so a late packet is a body still
// walking, not one stopped and then thrown forward.
const GUESS_SPEED = 12 / 60; // metres a tick: nobody is guessed faster than a sprint
// `own`: `tick` counts the body's own steps (snapshot `moves`), not the
// server's ticks: the timeline a watched body is even on (presentation.mjs SeatClocks).
export function interpolate(history, tick, seat, ahead = 0, own = false) {
  if (!history.length) return null;
  const key = own ? (s) => s.moves?.[seat] ?? 0 : (s) => s.tick;
  if (ahead > 0 && history.length > 1 && tick >= key(history.at(-1))) {
    // (In its own steps the newest snapshots may all be the same step: the one before is the last that differs.)
    const b = history.at(-1);
    let j = history.length - 2;
    while (own && j > 0 && key(history[j]) >= key(b)) j--;
    const a = history[j];
    const p = a.players[seat], q = b.players[seat];
    const ticks = key(b) - key(a);
    if (p && q && ticks > 0 && ticks <= 6 && p.life === q.life && q.alive && p.alive) {
      let vx = (q.x - p.x) / ticks, vz = (q.z - p.z) / ticks;
      const speed = Math.hypot(vx, vz);
      if (speed > GUESS_SPEED) { vx *= GUESS_SPEED / speed; vz *= GUESS_SPEED / speed; }
      return { ...q, x: q.x + vx * ahead, z: q.z + vz * ahead };
    }
    return q || p;
  }
  // The pair around `tick`, found from the newest back (a seat's steps start
  // again with a new body in it: only the run that leads to now is read).
  let i = history.length - 1;
  while (i > 0 && key(history[i - 1]) >= tick && key(history[i - 1]) <= key(history[i])) i--;
  const b = history[i], a = i > 0 && key(history[i - 1]) <= key(b) ? history[i - 1] : b;
  const p = a.players[seat],
    q = b.players[seat];
  if (!p || !q) return q || p;
  if (p.life !== q.life || p.alive !== q.alive) return tick < key(b) ? p : q;
  const t = key(a) === key(b) ? 0 : clamp((tick - key(a)) / (key(b) - key(a)), 0, 1);
  const out = { ...q };
  for (const key of ['x', 'y', 'z', 'pitch']) out[key] = p[key] + (q[key] - p[key]) * t;
  const turn = ((q.yaw - p.yaw + 540) % 360) - 180;
  out.yaw = p.yaw + turn * t;
  return out;
}
