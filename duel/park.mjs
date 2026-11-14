// Shared park geometry and server-authorized edge transitions.
export const PARK_EDGES = ['east', 'west', 'north', 'south'];
export function edgeAt(p) {
  if (!p || p.y > 2.3) return null;
  if (Math.abs(p.z) < 0.6) {
    if (p.x > 7.25) return 'east';
    if (p.x < -7.25) return 'west';
  }
  if (Math.abs(p.x) < 0.6) {
    if (p.z > 7.25) return 'north';
    if (p.z < -7.25) return 'south';
  }
  return null;
}
export function landing(p, edge) {
  const out = { x: p.x, z: p.z, yaw: p.yaw, pitch: p.pitch };
  if (edge === 'east') out.x = -6.9;
  if (edge === 'west') out.x = 6.9;
  if (edge === 'north') out.z = -6.9;
  if (edge === 'south') out.z = 6.9;
  return out;
}
// A front facing the shared east-west path, with room for a profile tag.
export function podFront(rect) {
  const north = rect.zMin > 0;
  return { x: (rect.xMin + rect.xMax) / 2, z: north ? rect.zMin + 0.3 : rect.zMax - 0.3,
    yaw: north ? 0 : Math.PI, north };
}

// Enter only by walking across the front from the public path. A layout
// refresh under a stationary visitor must not teleport them into a pod.
export function crossedPod(from, to, lots) {
  if (!from || !to || to.y > 2.3) return null;
  for (const h of lots?.homes || []) {
    if (!h.pod || !lots.rects[h.lot]) continue;
    const f = podFront(lots.rects[h.lot]);
    if (Math.abs(to.x - f.x) > 0.65) continue;
    if (f.north ? from.z < f.z && to.z >= f.z : from.z > f.z && to.z <= f.z) return h.pod;
  }
  return null;
}
