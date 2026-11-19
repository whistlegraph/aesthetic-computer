import { direction, clamp } from './world.mjs';
// 🚁 The eye of someone looking on flies as a small drone does: it leans
// into where it is asked to go and takes a moment to get there, and let go
// it drifts to a hover. (Others see it as one: view.mjs draws the drone.)
const PUSH = 5, DRIFT = 2.2; // how quickly it takes up a speed asked for, and loses one let go (per second)
// air: what the air is doing where it is (air.mjs), metres a second. It holds
// its place against most of a steady wind, as a drone does, and rides the rest:
// a quarter of the wind across, more of what rises and sinks.
const RIDE = 0.25, HEAVE = 0.6;
export const CEILING = 50000; // over the top of the tallest tower there can be (tower.mjs TOP)
export function moveFreecam(camera, input, seconds, air = null) {
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
    vy = (camera.vy || 0) + (y * top * climb + ay - (camera.vy || 0)) * k,
    vz = (camera.vz || 0) + (z * top + az - (camera.vz || 0)) * k;
  if (!asked && !air && Math.hypot(vx, vy, vz) < 0.02) vx = vy = vz = 0; // at a hover (in still air)
  const nx = clamp(camera.x + vx * dt, -60, 60), ny = clamp(camera.y + vy * dt, 0.15, CEILING), nz = clamp(camera.z + vz * dt, -60, 60);
  // Against the edge of the air it stops, it does not push.
  if (nx !== camera.x + vx * dt) vx = 0;
  if (ny !== camera.y + vy * dt) vy = 0;
  if (nz !== camera.z + vz * dt) vz = 0;
  return { ...camera, x: nx, y: ny, z: nz, vx, vy, vz };
}
