// 🌞 rax's sky: one clock for the whole world, so everyone at the tower
// sees the same sun. A day runs faster than a real one (DAY_MS), counted
// from the epoch, with nothing to store or send: any two clients agree.
//
// The phase runs 0 … 1 over a day: 0 is midnight, 0.25 sunrise, 0.5 noon,
// 0.75 sunset. The sun rises in the east (+x), crosses the south (-z), where
// the tower's door faces, and sets in the west.
// 🌇 The day is Los Angeles's own (2026-11-19): noon in rax is noon there,
// for everyone, whatever their clock says. (It was a day in twenty-four
// minutes.) The zone's distance from UTC is asked once a minute, so the
// clocks changing is followed.
export const DAY_MS = 24 * 60 * 60 * 1000;
export const STEPS = 480; // sky pictures in a day: one every three minutes
const LA = new Intl.DateTimeFormat('en-US', { timeZone: 'America/Los_Angeles', hourCycle: 'h23', hour: 'numeric', minute: 'numeric', second: 'numeric' });
let shiftAt = -Infinity, shift = 0;
function zoneShift(now) {
  if (Math.abs(now - shiftAt) > 60000) {
    const part = Object.fromEntries(LA.formatToParts(new Date(now)).map((p) => [p.type, Number(p.value)]));
    const there = ((part.hour * 60 + part.minute) * 60 + part.second) * 1000;
    const utc = Math.floor(((((now % DAY_MS) + DAY_MS) % DAY_MS)) / 1000) * 1000;
    shift = there - utc;
    shiftAt = now;
  }
  return shift;
}
export const phase = (now = Date.now()) => ((((now + zoneShift(now)) % DAY_MS) + DAY_MS) % DAY_MS) / DAY_MS;
export const stepOf = (t) => Math.floor(t * STEPS) % STEPS;

const mix = (a, b, k) => {
  const ch = (shift) => Math.round(((a >> shift) & 255) + (((b >> shift) & 255) - ((a >> shift) & 255)) * k);
  return (ch(16) << 16) | (ch(8) << 8) | ch(0);
};
export const blend = mix;

// The sky's three colours through the day: overhead, part-way down, and at
// the horizon. Between two keys the colours cross evenly.
const NIGHT = { zenith: 0x0b0d1a, mid: 0x11152a, horizon: 0x1a1f3a };
const DAWN = { zenith: 0x3b4a7a, mid: 0xd98a8a, horizon: 0xffc48a };
const DAY = { zenith: 0x6fa8e6, mid: 0x9ec4e9, horizon: 0xcfe3f5 };
const DUSK = { zenith: 0x2f3566, mid: 0xc86a8a, horizon: 0xff9e6a };
const KEYS = [[0, NIGHT], [0.21, NIGHT], [0.25, DAWN], [0.31, DAY], [0.69, DAY], [0.75, DUSK], [0.79, NIGHT], [1, NIGHT]];

export function skyAt(t) {
  let k = 0;
  while (k < KEYS.length - 2 && t >= KEYS[k + 1][0]) k++;
  const [t0, a] = KEYS[k], [t1, b] = KEYS[k + 1], f = Math.min(1, Math.max(0, (t - t0) / (t1 - t0)));
  const angle = 2 * Math.PI * (t - 0.25); // 0 at sunrise
  const raw = { x: Math.cos(angle), y: Math.sin(angle) * 0.85, z: -Math.sin(angle) * 0.5 };
  const length = Math.hypot(raw.x, raw.y, raw.z);
  const sun = { x: raw.x / length, y: raw.y / length, z: raw.z / length };
  const high = Math.min(1, Math.max(0, sun.y / 0.5)); // 0 on the horizon, 1 well up
  return {
    zenith: mix(a.zenith, b.zenith, f), mid: mix(a.mid, b.mid, f), horizon: mix(a.horizon, b.horizon, f),
    sun, moon: { x: -sun.x, y: -sun.y, z: -sun.z },
    sunColor: mix(0xff8a4a, 0xfff4c8, high),
    day: t >= 0.25 && t < 0.75, // which of the two rooms the ground wears
    stars: Math.min(1, Math.max(0, (-sun.y - 0.05) / 0.2)), // 0 by day, 1 in full night
  };
}

// The colour of the sky at an elevation (radians above the horizon).
export function skyColor(sky, elevation) {
  if (elevation <= 0) return sky.horizon;
  if (elevation < 0.34) return mix(sky.horizon, sky.mid, elevation / 0.34);
  if (elevation < 1.1) return mix(sky.mid, sky.zenith, (elevation - 0.34) / 0.76);
  return sky.zenith;
}

// Fixed stars over the upper sky: [x, y, z] unit directions and a size,
// the same for everyone (a small generator, seeded).
export function stars(count = 160) {
  let seed = 0x5f3759df;
  const next = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296);
  return Array.from({ length: count }, () => {
    const turn = next() * Math.PI * 2, y = 0.08 + next() * 0.9, flat = Math.sqrt(1 - y * y);
    return { x: Math.cos(turn) * flat, y, z: Math.sin(turn) * flat, size: 5 + next() * 6, bright: next() > 0.7 };
  });
}
