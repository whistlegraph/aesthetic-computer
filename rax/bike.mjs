// 🛵 The cabin bike: everyone's own small aircraft, a scooter with a glass
// cabin over the seat. B gets on and off. On it there is no gravity: it goes
// where the eye looks, space lifts it, shift lets it sink, and it never sets
// down on the ground — it hovers a little over whatever is under it. Off it
// in the air, the body falls as any jump does.
//
// Pure, so the client's prediction and the server run the same flight. This
// is how a body gets between the planets of orbits.mjs later; for now it
// flies over the one it is on, across the blocks as a walk does.
// (world.mjs's own `direction`, kept here so world.mjs can import this file.)
const direction = (yaw, pitch = 0) => {
  const y = (yaw * Math.PI) / 180, p = (pitch * Math.PI) / 180;
  return { x: Math.sin(y) * Math.cos(p), y: Math.sin(p), z: Math.cos(y) * Math.cos(p) };
};

export const BIKE = {
  cruise: 14, // metres a second, flat out ahead
  strafe: 0.6, // of cruise, sideways
  lift: 7, // metres a second, straight up or down
  ease: 2.6, // how quickly it comes to the speed asked (1/s): a bike drifts
  hover: 0.55, // its keel's height over what is under it, metres
  seat: 0.45, // the rider sits this much higher than they stand
  top: 120, // highest it goes over the land, metres
};

// Where a bike may be ridden: not in a game's side-on arena or the inner sea,
// and not under a low roof (the cellar's). Judged where the body stands
// (`at`: { x, y, z }, y at the eyes), not by the room: the plaza's lobby is a
// twelve-metre hall and its courtyard open sky. A roof is a ceiling of some
// size (a glass wall's rails are ceilings too, a few centimetres deep).
// (Until 2026-11-24 any ceiling or window in a room refused the bike there,
// the plaza included, where everyone starts.)
const LOW = 2.5; // metres over the eyes: a roof nearer than this is no place to fly
export const rideable = (fixed = [], flat = false, at = null) =>
  !flat && !fixed.some((b) => b.water) &&
  !(at && fixed.some((b) => b.ceiling && (b.xMax - b.xMin) * (b.zMax - b.zMin) > 4 &&
    at.x >= b.xMin && at.x <= b.xMax && at.z >= b.zMin && at.z <= b.zMax && b.yMin >= at.y - 0.2 && b.yMin - at.y < LOW));

// One tick of flight. `floor` is the height of what is under the bike,
// `eye` the eye's height over the feet (world.mjs CFG.eyeHeight).
// Back comes the next body (position, velocity, on the bike).
export function fly(player, cmd, floor, dt, eye = 1.7) {
  const look = direction(cmd.yaw ?? player.yaw, cmd.pitch ?? player.pitch ?? 0);
  const side = direction((cmd.yaw ?? player.yaw) - 90);
  const fwd = cmd.fwd || 0, right = cmd.right || 0;
  const up = (cmd.buttons & 1 ? 1 : 0) - (cmd.duck ? 1 : 0);
  const want = {
    x: look.x * fwd * BIKE.cruise + side.x * right * BIKE.cruise * BIKE.strafe,
    y: look.y * fwd * BIKE.cruise + up * BIKE.lift,
    z: look.z * fwd * BIKE.cruise + side.z * right * BIKE.cruise * BIKE.strafe,
  };
  const k = 1 - Math.exp(-dt * BIKE.ease);
  const vx = (player.vx || 0) + (want.x - (player.vx || 0)) * k;
  const vy = (player.vy || 0) + (want.y - (player.vy || 0)) * k;
  const vz = (player.vz || 0) + (want.z - (player.vz || 0)) * k;
  const next = { ...player, yaw: cmd.yaw ?? player.yaw, pitch: cmd.pitch ?? player.pitch,
    x: player.x + vx * dt, y: player.y + vy * dt, z: player.z + vz * dt, vx, vy, vz, onGround: false, bike: true };
  const lowest = floor + BIKE.hover + BIKE.seat + eye, highest = floor + BIKE.top;
  if (next.y < lowest) { next.y = lowest; next.vy = Math.max(0, next.vy); }
  if (next.y > highest) { next.y = highest; next.vy = Math.min(0, next.vy); }
  return next;
}
