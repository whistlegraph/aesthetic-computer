// Shared park geometry and server-authorized edge transitions.
export const PARK_EDGES = ['east', 'west', 'north', 'south'];
// Blocks meet along their whole edge, not only where the lanes cross: walk
// off any side with a neighbour and you are in the neighbour. EDGE sits just
// inside the walking bound (world.mjs keeps bodies within 7.6); an arrival is
// set down at REACH, clear of the edge it came through, or it would be handed
// straight back.
export const EDGE = 7.5, REACH = 7.3, SPAN = 16;
export function edgeAt(p) {
  if (!p || p.y > 2.3) return null;
  if (p.x > EDGE) return 'east';
  if (p.x < -EDGE) return 'west';
  if (p.z > EDGE) return 'north';
  if (p.z < -EDGE) return 'south';
  return null;
}
// The same spot seen from the next block: one block over, held inside the
// arrival line. The ground is drawn across the join, so this reads as a
// short step forward.
export function landing(p, edge) {
  const out = { x: p.x, z: p.z, yaw: p.yaw, pitch: p.pitch };
  const hold = (n) => Math.max(-REACH, Math.min(REACH, n));
  if (edge === 'east') out.x = hold(p.x - SPAN);
  if (edge === 'west') out.x = hold(p.x + SPAN);
  if (edge === 'north') out.z = hold(p.z - SPAN);
  if (edge === 'south') out.z = hold(p.z + SPAN);
  return out;
}
// 🏠 The house on a lot. It stands back from the lanes by a front yard and
// from the block's edge by a deeper one, so a body arriving from the next
// block (REACH, with its 0.4 m radius) lands in the yard, never in a wall.
// The door faces the east-west lane.
export const HOUSE = { front: 0.6, back: 1.2, height: 3, wall: 0.2, door: 1.6, doorHeight: 2.2 };
export function house(rect) {
  const east = rect.xMin > 0, north = rect.zMin > 0;
  const xMin = east ? rect.xMin + HOUSE.front : rect.xMin + HOUSE.back;
  const xMax = east ? rect.xMax - HOUSE.back : rect.xMax - HOUSE.front;
  const zMin = north ? rect.zMin + HOUSE.front : rect.zMin + HOUSE.back;
  const zMax = north ? rect.zMax - HOUSE.back : rect.zMax - HOUSE.front;
  return { xMin, xMax, zMin, zMax, north, frontZ: north ? zMin : zMax, doorX: (xMin + xMax) / 2 };
}
// Its walls as solid boxes: the back, the two sides, and the front either
// side of the doorway. (The lintel over the door is drawn, not solid.)
export function houseWalls(rect) {
  const h = house(rect), t = HOUSE.wall / 2, d = HOUSE.door / 2;
  const slab = (xMin, xMax, zMin, zMax) => ({ type: 'box', xMin, xMax, yMin: 0, yMax: HOUSE.height, zMin, zMax });
  const backZ = h.north ? h.zMax : h.zMin;
  return [
    slab(h.xMin - t, h.xMax + t, backZ - t, backZ + t),
    slab(h.xMin - t, h.xMin + t, h.zMin, h.zMax),
    slab(h.xMax - t, h.xMax + t, h.zMin, h.zMax),
    slab(h.xMin - t, h.doorX - d, h.frontZ - t, h.frontZ + t),
    slab(h.doorX + d, h.xMax + t, h.frontZ - t, h.frontZ + t),
  ];
}
// Every settled home's walls in a block ({ rects, homes } as the server sends).
export const blockWalls = (lots) =>
  (lots?.homes || []).flatMap((h) => (lots.rects[h.lot] ? houseWalls(lots.rects[h.lot]) : []));

// The front door, facing the shared east-west path, with the handle over it.
export function podFront(rect) {
  const h = house(rect);
  return { x: h.doorX, z: h.frontZ, yaw: h.north ? 0 : Math.PI, north: h.north };
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
