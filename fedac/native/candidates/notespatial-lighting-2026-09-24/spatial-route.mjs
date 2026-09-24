// Routing snapshot from ../notespatial-2026-09-24/notespatial-performance-optimized.mjs.
// Reuses the exact audio geometry while compiling; no runtime event scanning.
// Spatial rehearsal, 26.09.18
// Local synthesis on every seat, coordinated over Wi-Fi. Microphones stay closed.
// Shared deterministic world math. No device or network dependencies.
function eventGain(score, event) {
  const trim = Number.isInteger(event.gm) ? score.gmGains?.[event.gm] ?? 1 : 1;
  return Math.min(.65, Math.max(0, event.g * (score.gain ?? .35) * (Number.isFinite(trim) ? trim : 1)));
}

function ribbon(score, key, t, fallback = 0) {
  const a = score[key];
  if (!a?.length) return fallback;
  const u = Math.max(0, Math.min(1, t / score.dur)) * (a.length - 1);
  const i = Math.floor(u), j = Math.min(i + 1, a.length - 1);
  return a[i] + (a[j] - a[i]) * (u - i);
}

// Analytic integral of the piecewise-linear ribbon; independent of frame
// rate, missed frames, and the order in which a seat renders the world.
function rotationAt(score, t) {
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

// A ring may keep one seat in the middle of the room (score.center = its
// index; score.ring = how many seats stand on the circle). A lane marked
// {center: true} sounds only there; every other lane routes around the ring.
const ringSeats = (score, seats) => score.ring ?? (Number.isInteger(score.center) ? seats - 1 : seats);

function voicePosition(score, i, t) {
  if (score.lanes[i]?.center) return { angle: 0, x: 0, y: 0, z: 0, center: true };
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

function sourceGain(score, position, seat, seats) {
  if (Number.isInteger(score.center)) {
    if (position.center) return seat === score.center ? 1 : 0;
    if (seat === score.center) return 0;
    const ringIndex = seat > score.center ? seat - 1 : seat;
    if (score.geometry !== 'line') return seatGain(position.angle, ringIndex, ringSeats(score, seats));
  }
  if (position.center) return 0;
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
function seatGain(angle, seat, seats) {
  const u = ((angle / (2 * Math.PI) * seats) % seats + seats) % seats;
  const a = Math.floor(u), f = u - a;
  if (seat === a) return Math.cos(f * Math.PI / 2);
  if (seat === (a + 1) % seats) return Math.sin(f * Math.PI / 2);
  return 0;
}


export {eventGain, voicePosition, sourceGain};
