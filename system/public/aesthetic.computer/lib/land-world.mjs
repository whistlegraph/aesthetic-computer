// land-world.mjs — shared static meadow geometry + movement tuning.
// Imported by both disks/land.mjs (client render + prediction) and
// session-server/world-manager.mjs (authoritative collision). Keep this
// dep-free so it works in Node and the browser.
//
// Unlike arena (which duplicates its cfg client/server with a MUST-MATCH
// comment), land keeps the WHOLE world cfg here so there is exactly one
// copy. Coordinates are world-space, Y is up, the meadow sits at -1.5.

const GROUND_Y = -1.5;

// Half-size of the meadow. Larger than the arena — this is a stroll, not a
// deathmatch. GRID lives client-side (render tessellation only).
export const LAND_SIZE = 24;

// 🌳 Trees are trunk cylinders (the only colliders — canopies are visual
// only, tall enough to walk under at the edges). 🪨 Boulders are boxes you
// can hop onto. `tree: true` tells the client to draw a leaf canopy on top.
export const LAND_OBSTACLES = Object.freeze([
  // Trees — scattered, none within ~4u of spawn ring.
  { type: "cylinder", x:  10, z:   6, r: 0.45, yMin: GROUND_Y, yMax: GROUND_Y + 5.0, tree: true },
  { type: "cylinder", x: -12, z:   9, r: 0.50, yMin: GROUND_Y, yMax: GROUND_Y + 5.5, tree: true },
  { type: "cylinder", x:   7, z: -14, r: 0.40, yMin: GROUND_Y, yMax: GROUND_Y + 4.5, tree: true },
  { type: "cylinder", x:  -9, z: -11, r: 0.45, yMin: GROUND_Y, yMax: GROUND_Y + 5.0, tree: true },
  { type: "cylinder", x:  17, z:  -4, r: 0.55, yMin: GROUND_Y, yMax: GROUND_Y + 6.0, tree: true },
  { type: "cylinder", x: -18, z:  -2, r: 0.45, yMin: GROUND_Y, yMax: GROUND_Y + 5.0, tree: true },
  { type: "cylinder", x:   3, z:  17, r: 0.50, yMin: GROUND_Y, yMax: GROUND_Y + 5.5, tree: true },
  { type: "cylinder", x: -14, z:  18, r: 0.40, yMin: GROUND_Y, yMax: GROUND_Y + 4.5, tree: true },
  { type: "cylinder", x:  19, z:  14, r: 0.45, yMin: GROUND_Y, yMax: GROUND_Y + 5.0, tree: true },

  // Boulders — low boxes, hop-on-able (yMax ≈ jump height).
  { type: "box", xMin:  4.5, xMax:  6.5, zMin:  -7.0, zMax:  -5.2, yMin: GROUND_Y, yMax: GROUND_Y + 1.1 },
  { type: "box", xMin: -7.5, xMax: -5.0, zMin:   2.5, zMax:   4.5, yMin: GROUND_Y, yMax: GROUND_Y + 0.9 },
  { type: "box", xMin: 12.0, xMax: 14.5, zMin:  10.0, zMax:  12.0, yMin: GROUND_Y, yMax: GROUND_Y + 1.3 },
]);

// Visual palette — indices match LAND_OBSTACLES order. Trunks bark-brown,
// boulders cool gray. (Canopy greens live client-side in land.mjs.)
export const LAND_OBSTACLE_COLORS = Object.freeze([
  [0.42, 0.30, 0.20, 1.0],   // tree trunk
  [0.38, 0.27, 0.18, 1.0],
  [0.45, 0.33, 0.22, 1.0],
  [0.40, 0.29, 0.19, 1.0],
  [0.36, 0.26, 0.17, 1.0],
  [0.43, 0.31, 0.21, 1.0],
  [0.41, 0.30, 0.20, 1.0],
  [0.44, 0.32, 0.21, 1.0],
  [0.39, 0.28, 0.19, 1.0],
  [0.55, 0.55, 0.58, 1.0],   // boulders
  [0.50, 0.51, 0.55, 1.0],
  [0.58, 0.57, 0.60, 1.0],
]);

// 🚶 Movement — same Q3 integrator as arena but tuned for a stroll:
// slower run, softer jump, floatier gravity. Friction/air-accel kept from
// arena so the feel stays familiar across worlds.
export const LAND_PHYSICS = Object.freeze({
  airAccel: 70,
  groundAccel: 80,
  airCapSpeed: 1.5,
  groundFriction: 10,
  playerRadius: 0.4,
});

// Complete world cfg — the ONE copy shared by client prediction, client
// reconcile, and server pmove. simHz matches the server tick.
export const LAND_CFG = Object.freeze({
  runSpeed: 8,
  walkSpeed: 4,
  jumpVelocity: 7,
  gravity: 40,
  groundY: GROUND_Y,
  eyeHeight: 2.0,
  crouchEyeHeight: 1.2,
  crouchLerp: 0.25,
  groundBounds: { xMin: -LAND_SIZE, xMax: LAND_SIZE, zMin: -LAND_SIZE, zMax: LAND_SIZE },
  deathFloorY: -30,
  deathFloorClearance: 0.3,
  simHz: 60,
  hVelDecay: 0.9,
  airAccel: LAND_PHYSICS.airAccel,
  groundAccel: LAND_PHYSICS.groundAccel,
  airCapSpeed: LAND_PHYSICS.airCapSpeed,
  groundFriction: LAND_PHYSICS.groundFriction,
  obstacles: LAND_OBSTACLES,
  playerRadius: LAND_PHYSICS.playerRadius,
});

// Spawn ring — a loose circle near the middle of the meadow.
export const LAND_SPAWNS = Object.freeze([
  { x:  3, z:  0 }, { x: -3, z:  0 }, { x:  0, z:  3 }, { x:  0, z: -3 },
  { x:  2, z:  2 }, { x: -2, z: -2 }, { x:  2, z: -2 }, { x: -2, z:  2 },
]);
