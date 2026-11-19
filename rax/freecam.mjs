import { direction, clamp } from './world.mjs';
export function moveFreecam(camera, input, seconds) {
  const f = direction(camera.yaw, camera.pitch),
    r = direction(camera.yaw - 90);
  const x = f.x * input.fwd + r.x * input.right,
    y = f.y * input.fwd + input.up,
    z = f.z * input.fwd + r.z * input.right;
  const length = Math.max(1, Math.hypot(x, y, z)),
    step =
      (Math.min(0.1, Math.max(0, seconds)) * (input.boost ? 16 : 6)) / length;
  return {
    ...camera,
    x: clamp(camera.x + x * step, -60, 60),
    y: clamp(camera.y + y * step, 0.15, 40),
    z: clamp(camera.z + z * step, -60, 60),
  };
}
