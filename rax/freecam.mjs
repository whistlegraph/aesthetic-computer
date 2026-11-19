import { direction, clamp } from './world.mjs';
// 🚁 The eye of someone looking on flies as a small drone does: it leans
// into where it is asked to go and takes a moment to get there, and let go
// it drifts to a hover. (Others see it as one: view.mjs draws the drone.)
const PUSH = 5, DRIFT = 2.2; // how quickly it takes up a speed asked for, and loses one let go (per second)
export function moveFreecam(camera, input, seconds) {
  const f = direction(camera.yaw, camera.pitch),
    r = direction(camera.yaw - 90);
  const x = f.x * input.fwd + r.x * input.right,
    y = f.y * input.fwd + input.up,
    z = f.z * input.fwd + r.z * input.right;
  // The speed asked for: never more on a diagonal than straight ahead.
  const length = Math.max(1, Math.hypot(x, y, z)), top = (input.boost ? 16 : 6) / length;
  const dt = Math.min(0.1, Math.max(0, seconds)), asked = Math.hypot(x, y, z) > 0.01;
  const k = 1 - Math.exp(-dt * (asked ? PUSH : DRIFT));
  let vx = (camera.vx || 0) + (x * top - (camera.vx || 0)) * k,
    vy = (camera.vy || 0) + (y * top - (camera.vy || 0)) * k,
    vz = (camera.vz || 0) + (z * top - (camera.vz || 0)) * k;
  if (!asked && Math.hypot(vx, vy, vz) < 0.02) vx = vy = vz = 0; // at a hover
  const nx = clamp(camera.x + vx * dt, -60, 60), ny = clamp(camera.y + vy * dt, 0.15, 40), nz = clamp(camera.z + vz * dt, -60, 60);
  // Against the edge of the air it stops, it does not push.
  if (nx !== camera.x + vx * dt) vx = 0;
  if (ny !== camera.y + vy * dt) vy = 0;
  if (nz !== camera.z + vz * dt) vz = 0;
  return { ...camera, x: nx, y: ny, z: nz, vx, vy, vz };
}
