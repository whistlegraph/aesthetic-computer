// 🎥 The film camera: where it stands and where it looks, for one shot at
// one moment. Pure arithmetic, shared by the page that films and the tests.
//
// A shot is a plain object:
//   { kind: 'still',  at: [x, y, z], look }
//   { kind: 'move',   from: [x, y, z], to: [x, y, z], look, seconds, ease }
//   { kind: 'follow', who, back, up, side, look }    behind an actor's shoulders
//   { kind: 'orbit',  around, radius, height, period, phase }
// `look`, `who` and `around` are a handle (the actor's head) or a point.
// Any shot takes `zoom` (1 is the room's own lens), `zoomTo` with `seconds`,
// and `smooth` (how quickly the camera catches up, per second; 0 is locked).
// `rides: true` puts the camera in the elevator car's frame: it travels with a ride.
// `within` keeps the camera inside a square room of that half-width (the
// director sets 7.6, a rax room's walls, unless a shot says `within: false`).
const RAD = Math.PI / 180;
export const EASES = {
  linear: (t) => t,
  in: (t) => t * t,
  out: (t) => 1 - (1 - t) ** 2,
  inOut: (t) => (t < 0.5 ? 2 * t * t : 1 - (-2 * t + 2) ** 2 / 2),
};
const clamp01 = (t) => Math.max(0, Math.min(1, t));
const lerp = (a, b, t) => a + (b - a) * t;
const point = (p) => (Array.isArray(p) ? { x: p[0], y: p[1], z: p[2] } : p);
// The short way round from one bearing to another, in degrees.
export const turn = (from, to) => ((((to - from) % 360) + 540) % 360) - 180;
// Yaw 0 looks along +z and 90 along +x, as world.mjs's direction().
export function aim(from, to) {
  const dx = to.x - from.x, dy = to.y - from.y, dz = to.z - from.z;
  return { yaw: (Math.atan2(dx, dz) / RAD + 360) % 360, pitch: Math.atan2(dy, Math.hypot(dx, dz)) / RAD };
}

// `find(handle)` answers an actor as the world has them ({ x, y, z, yaw },
// y at the eyes) or null while they are elsewhere.
function target(what, find) {
  if (what == null) return null;
  if (typeof what === 'string') return find(what);
  return point(what);
}

// The camera's pose `t` seconds into `shot`. `previous` and `dt` let a
// smoothed shot ease from where the camera was last frame.
export function pose(shot, t, find = () => null, previous = null, dt = 1 / 60) {
  const k = shot.seconds ? EASES[shot.ease || 'inOut'](clamp01(t / shot.seconds)) : 1;
  let at = null, look = target(shot.look, find);
  if (shot.kind === 'still') at = point(shot.at);
  else if (shot.kind === 'move') {
    const a = point(shot.from), b = point(shot.to);
    at = { x: lerp(a.x, b.x, k), y: lerp(a.y, b.y, k), z: lerp(a.z, b.z, k) };
  } else if (shot.kind === 'follow') {
    const who = find(shot.who);
    if (!who) return previous;
    const yaw = (who.yaw || 0) * RAD, back = shot.back ?? 4, side = shot.side ?? 0;
    at = {
      x: who.x - Math.sin(yaw) * back + Math.cos(yaw) * side,
      y: who.y + (shot.up ?? 1),
      z: who.z - Math.cos(yaw) * back - Math.sin(yaw) * side,
    };
    look ||= who;
  } else if (shot.kind === 'orbit') {
    const centre = target(shot.around, find);
    if (!centre) return previous;
    const angle = ((shot.phase ?? 0) + (360 * t) / (shot.period ?? 20)) * RAD, r = shot.radius ?? 6;
    at = { x: centre.x + Math.sin(angle) * r, y: (shot.height ?? 3), z: centre.z + Math.cos(angle) * r };
    look ||= centre;
  }
  if (!at) return previous;
  if (shot.within) {
    at.x = Math.max(-shot.within, Math.min(shot.within, at.x));
    at.z = Math.max(-shot.within, Math.min(shot.within, at.z));
  }
  const facing = look ? aim(at, look) : { yaw: shot.yaw ?? previous?.yaw ?? 0, pitch: shot.pitch ?? previous?.pitch ?? 0 };
  const zoom = shot.zoomTo ? lerp(shot.zoom ?? 1, shot.zoomTo, k) : shot.zoom ?? 1;
  const want = { ...at, ...facing, zoom, rides: !!shot.rides };
  if (!shot.smooth || !previous) return want;
  const s = 1 - Math.exp(-dt * shot.smooth);
  return {
    x: lerp(previous.x, want.x, s), y: lerp(previous.y, want.y, s), z: lerp(previous.z, want.z, s),
    yaw: (previous.yaw + turn(previous.yaw, want.yaw) * s + 360) % 360,
    pitch: lerp(previous.pitch, want.pitch, s),
    zoom: lerp(previous.zoom ?? 1, zoom, s),
    rides: !!shot.rides,
  };
}
