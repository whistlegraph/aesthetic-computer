// Shared deterministic world math. No device or network dependencies.
export function ribbon(score, key, t, fallback = 0) {
  const a = score[key];
  if (!a?.length) return fallback;
  const u = Math.max(0, Math.min(1, t / score.dur)) * (a.length - 1);
  const i = Math.floor(u), j = Math.min(i + 1, a.length - 1);
  return a[i] + (a[j] - a[i]) * (u - i);
}

// Analytic integral of the piecewise-linear ribbon; independent of frame
// rate, missed frames, and the order in which a seat renders the world.
export function rotationAt(score, t) {
  t = Math.max(0, Math.min(score.dur, t));
  const a = score.rotation;
  let area = 0;
  if (a?.length === 1) area = a[0] * t;
  else if (a?.length > 1) {
    const dt = score.dur / (a.length - 1);
    const n = Math.min(a.length - 1, Math.floor(t / dt));
    for (let i = 0; i < n; i++) area += (a[i] + a[i + 1]) * dt / 2;
    if (n < a.length - 1) {
      const x = t - n * dt;
      area += a[n] * x + (a[n + 1] - a[n]) * x * x / (2 * dt);
    }
  }
  return (score.lanes.length > 1 ? 0.15 * t : 0) + Math.PI * area;
}

export function voicePosition(score, i, t) {
  if (score.geometry === 'line') {
    const lanePath = score.lanes[i].linePosition;
    let u = ribbon(lanePath ? { dur: score.dur, linePosition: lanePath } : score, 'linePosition', t);
    if (score.linePasses?.length) {
      let pass = score.linePasses[0];
      for (const next of score.linePasses) { if (next.at > t) break; pass = next; }
      const x = Math.max(0, (t - pass.at) / pass.step), n = score.seatOrder.length;
      u = x <= n - 1 ? x / (n - 1) : x < n ? 1 : Math.max(0, 1 - (x - n) / .8);
    }
    u = Math.max(0, Math.min(1, u));
    return { x: (u - .5) * 3.2, y: 0, z: 0, angle: 0, line: u };
  }
  const lane = score.lanes[i], pinned = Number.isFinite(lane.az);
  const orbit = lane.orbitSeconds > 0
    ? t / lane.orbitSeconds * Math.PI * 2 * (lane.orbitDirection === -1 ? -1 : 1)
    : rotationAt(score, t);
  const angle = (pinned ? lane.az : i / score.lanes.length * Math.PI * 2 +
    (lane.azOffset || 0) + orbit) + ribbon(score, 'fieldShift', t) * Math.PI;
  const el = Math.max(-1, Math.min(1, (pinned ? lane.el || 0 :
    ribbon(score, 'elevation', t)) + ribbon(score, 'fieldTilt', t)));
  const distance = Math.max(0.25, Math.min(3, (lane.dist || 1.2) *
    ribbon(score, 'fieldScale', t, 1)));
  return { angle, x: Math.cos(angle) * distance, y: el, z: Math.sin(angle) * distance };
}

export function sourceGain(score, position, seat, seats) {
  if (score.geometry !== 'line') return seatGain(position.angle, seat, seats);
  const order = score.seatOrder || [], index = order.indexOf(seat);
  if (index < 0 || order.length < 2) return 0;
  const u = Math.max(0, Math.min(1, position.line)) * (order.length - 1);
  const a = Math.floor(u), f = u - a;
  if (index === a) return Math.cos(f * Math.PI / 2);
  if (index === a + 1) return Math.sin(f * Math.PI / 2);
  return 0;
}

// Equal-power handoff between adjacent full-range seats on a horizontal
// ring. Elevation is shown, but three coplanar outputs cannot reproduce it.
export function seatGain(angle, seat, seats) {
  const u = ((angle / (2 * Math.PI) * seats) % seats + seats) % seats;
  const a = Math.floor(u), f = u - a;
  if (seat === a) return Math.cos(f * Math.PI / 2);
  if (seat === (a + 1) % seats) return Math.sin(f * Math.PI / 2);
  return 0;
}

// Light only the seat holding at least 90% of a source's routed power.
export function hasFocus(score, seat, seats, t) {
  return score.lanes.some((lane, i) => lane.events?.some(e => t >= e.t && t < e.t + e.dur) &&
    sourceGain(score, voicePosition(score, i, t), seat, seats) ** 2 >= .9);
}
