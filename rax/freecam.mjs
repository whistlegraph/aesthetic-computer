import { direction, clamp } from './world.mjs';
import { PLANET } from './planet.mjs';
// 🚁 The eye of someone looking on flies as a small drone does: it leans
// into where it is asked to go and takes a moment to get there, and let go
// it drifts to a hover. (Others see it as one: view.mjs draws the drone.)
const PUSH = 5, DRIFT = 2.2; // how quickly it takes up a speed asked for, and loses one let go (per second)
// air: what the air is doing where it is (air.mjs), metres a second. It holds
// its place against most of a steady wind, as a drone does, and rides the rest:
// a fifth of the wind across, a third of what rises and sinks: a slow float, as
// a gyro holds it, never a shake.
const RIDE = 0.2, HEAVE = 0.35;
export const REACH = 4000; // how far out over the land an eye may fly, metres from its room's middle
export const CEILING = 50000; // over the top of the tallest tower there can be (tower.mjs TOP)
// 🕳️ The ground holds it up, FLOOR over it. Held down against the ground for
// HOLD seconds, it is let through into the hollow planet (planet.mjs), and
// may sink to just over the inside of the far side; coming back up it meets
// nothing. `hollow` is false over a floor of the tower, which is only a floor.
export const FLOOR = 0.15, HOLD = 0.7, DEEP = -(2 * PLANET - 4);
export function moveFreecam(camera, input, seconds, air = null, hollow = true) {
  const f = direction(camera.yaw, camera.pitch),
    r = direction(camera.yaw - 90);
  const x = f.x * input.fwd + r.x * input.right,
    y = f.y * input.fwd + input.up,
    z = f.z * input.fwd + r.z * input.right;
  // The speed asked for: never more on a diagonal than straight ahead.
  const length = Math.max(1, Math.hypot(x, y, z)), top = (input.boost ? 16 : 6) / length;
  // Above the rooftops it climbs and sinks as fast as it is high (the tower is
  // kilometres tall); across, it flies as it does anywhere.
  const climb = 1 + Math.min(60, Math.max(0, camera.y - 20) / 40);
  const dt = Math.min(0.1, Math.max(0, seconds)), asked = Math.hypot(x, y, z) > 0.01;
  const k = 1 - Math.exp(-dt * (asked ? PUSH : DRIFT));
  const ax = (air?.x || 0) * RIDE, ay = (air?.y || 0) * HEAVE, az = (air?.z || 0) * RIDE;
  let vx = (camera.vx || 0) + (x * top + ax - (camera.vx || 0)) * k,
    // (Only the up and down asked for outright: flying ahead with the eye tipped
    // a little keeps its line at any height.)
    vy = (camera.vy || 0) + ((f.y * input.fwd + input.up * climb) * top + ay - (camera.vy || 0)) * k,
    vz = (camera.vz || 0) + (z * top + az - (camera.vz || 0)) * k;
  if (!asked && !air && Math.hypot(vx, vy, vz) < 0.02) vx = vy = vz = 0; // at a hover (in still air)
  // (How long it has been pressed down on the ground; it starts over the moment it is let up.)
  const landing = camera.y >= FLOOR && camera.y + vy * dt < FLOOR;
  const pressed = landing && hollow && input.up < -0.5 ? (camera.pressed || 0) + dt : 0;
  const nx = clamp(camera.x + vx * dt, -REACH, REACH), ny = clamp(camera.y + vy * dt, landing && pressed < HOLD ? FLOOR : hollow ? DEEP : FLOOR, CEILING), nz = clamp(camera.z + vz * dt, -REACH, REACH);
  // Against the edge of the air it stops, it does not push.
  if (nx !== camera.x + vx * dt) vx = 0;
  if (ny !== camera.y + vy * dt) vy = 0;
  if (nz !== camera.z + vz * dt) vz = 0;
  return { ...camera, x: nx, y: ny, z: nz, vx, vy, vz, pressed };
}
