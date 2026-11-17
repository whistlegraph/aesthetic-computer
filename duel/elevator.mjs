// 🛗 The tower's elevator and the tower's own measurements, as the server and
// the page both know them (tower.mjs holds who lives on which floor).
//
// The tower stands over the plaza on four legs. Its first floor is ATRIUM
// metres up, so the plaza keeps its sky, and every floor after that is one
// STOREY higher. A floor is a real place at a real height: floor 89 is
// 364 m above the plaza.
export const STOREY = 4, ATRIUM = 12;
export const altitude = (number) => (number > 0 ? ATRIUM + (number - 1) * STOREY : 0);
// The floor a height falls in (0 below the first).
export const floorAt = (y) => (y < ATRIUM ? 0 : 1 + Math.floor((y - ATRIUM) / STOREY));

// The tower's frame, around the 16 m room every floor is: SLAB is a floor
// slab's half-width, and LEG is where the four legs (and every storey's
// corner columns) stand from the middle and how thick a leg is.
export const SLAB = 8.35, LEG = { at: 8.15, wide: 0.7 };

// The car: a glass box against the north edge of the plaza and of every
// floor, its door to the south and the world behind its back wall. CAR is
// its half-width inside the walls; CAR_Z is where its middle stands.
export const CAR = 1.5, CAR_HEIGHT = 3, CAR_WALL = 0.2, CAR_Z = 6.1;

// The car's three solid walls (the fourth side is the door).
export function carWalls() {
  const t = CAR_WALL, o = CAR + t, z = CAR_Z;
  const slab = (xMin, xMax, zMin, zMax) => ({ type: 'box', xMin, xMax, yMin: 0, yMax: CAR_HEIGHT, zMin, zMax });
  return [slab(-o, o, z + CAR, z + o), slab(-o, -CAR, z - CAR, z + CAR), slab(CAR, o, z - CAR, z + CAR)];
}
// Standing in the car, feet on its floor.
export const inCar = (p) => !!p && Math.abs(p.x) < CAR && Math.abs(p.z - CAR_Z) < CAR && p.y < 2.3;
// Where a rider steps out: the car's middle, facing the door.
export const CAR_ARRIVAL = { x: 0, z: CAR_Z, yaw: 180, pitch: 0 };
// How long a ride between two heights takes: a floor or two is a few seconds,
// the whole tower well under ten. The car eases off its floor and into the next.
export const rideSeconds = (metres) => Math.min(9, 2.6 + 1.1 * Math.log2(1 + Math.abs(metres) / STOREY));
export const rideEase = (t) => (t <= 0 ? 0 : t >= 1 ? 1 : t < 0.5 ? 4 * t * t * t : 1 - (-2 * t + 2) ** 3 / 2);

// 🚪 The lobby: the tower's ground floor, a glass hall under the first floor
// across the north half of the plaza. Its front stands LOBBY_FRONT metres
// north of the plaza's middle with the entrance in its centre; the elevator
// is at the back. DOOR is the entrance's half-width.
export const LOBBY_FRONT = 1.2, DOOR = 1.6, DOOR_HEIGHT = 2.7;
export const DESK = { x: -4.6, z: 4.6, w: 2.8, d: 0.9, h: 1.1 };
// 🌳 The courtyard: the plaza's south half, before the entrance. A paved walk
// runs up the middle to the door between two planters, each with a tree.
export const PLANTERS = [{ x: -5, z: -3.4 }, { x: 5, z: -3.4 }], PLANTER = 2.2;
// The ground floor's solid parts: the front either side of the entrance, the
// two ends, the desk, and the courtyard's planters.
export function lobbyWalls() {
  const t = 0.1, f = LOBBY_FRONT, e = 7.9;
  const slab = (xMin, xMax, zMin, zMax, yMax = ATRIUM) => ({ type: 'box', xMin, xMax, yMin: 0, yMax, zMin, zMax });
  return [
    slab(-8, -DOOR, f - t, f + t),
    slab(DOOR, 8, f - t, f + t),
    slab(-8, -e, f + t, 8),
    slab(e, 8, f + t, 8),
    slab(DESK.x - DESK.w / 2, DESK.x + DESK.w / 2, DESK.z - DESK.d / 2, DESK.z + DESK.d / 2, DESK.h),
    ...PLANTERS.map((p) => slab(p.x - PLANTER / 2, p.x + PLANTER / 2, p.z - PLANTER / 2, p.z + PLANTER / 2, 0.55)),
  ];
}
