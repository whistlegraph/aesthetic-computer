// The basement uses metres above its own floor. Its upper landing meets the plaza.
export const BASEMENT = 'basement';
export const DEPTH = 4;
export const STAIR = { left: 3.2, right: 5.6, lip: 2.2, end: 6.4, landing: 1.4, steps: 16 };
const { left, right, lip, end, landing, steps } = STAIR;
const tread = (end - lip) / steps;
export const stairHeight = z => DEPTH * Math.max(0, Math.min(steps, Math.ceil((end - z) / tread))) / steps;

const block = (xMin, xMax, zMin, zMax, yMin, yMax, ink) =>
  ({ type: 'box', xMin, xMax, zMin, zMax, yMin, yMax, ink });
const stone = 0x415266, edge = 0x748a98;
export const treads = Array.from({ length: steps }, (_, i) =>
  ({ ...block(left, right, lip + i * tread, lip + (i + 1) * tread, 0, DEPTH * (steps - i) / steps, i % 2 ? stone : edge), stair: true }));
treads.push({ ...block(left, right, landing, lip, 0, DEPTH, edge), stair: true });
export const cellarWalls = [
  block(-8,-7.8,-8,8,0,DEPTH,stone), block(7.8,8,-8,8,0,DEPTH,stone),
  block(-7.8,7.8,-8,-7.8,0,DEPTH,stone), block(-7.8,7.8,7.8,8,0,DEPTH,stone),
  block(left-0.12,left,lip,end,0,DEPTH+0.35,edge), block(right,right+0.12,lip,end,0,DEPTH+0.35,edge),
  ...[-4,0].map(x => block(x-0.24,x+0.24,-2.25,-1.75,0,DEPTH,edge)),
];
export const ceiling = [
  [-8,left,-8,8], [right,8,-8,8], [left,right,-8,landing], [left,right,end,8],
].map(([a,b,c,d]) => ({ ...block(a,b,c,d,DEPTH-0.18,DEPTH,0x354352), ceiling: true }));

export function stairPassage(room, p) {
  if (!p?.alive || p.x < left || p.x > right) return null;
  if (room === 'lobby' && p.z >= lip + 0.06 && p.z < end && p.y <= 2.15)
    return { room: BASEMENT, y: p.y + DEPTH };
  if (room === BASEMENT && p.z >= landing - 0.25 && p.z < lip - 0.06 && p.y >= DEPTH + 1.5)
    return { room: 'lobby', y: Math.max(1.7, p.y - DEPTH) };
  return null;
}
