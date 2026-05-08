// arena-world.mjs — shared static arena geometry.
// Imported by both disks/arena.mjs (client render + prediction) and
// session-server/arena-manager.mjs (authoritative collision). Keep this
// dep-free so it works in Node and the browser.
//
// Coordinates are world-space. Y is up, ground sits at -1.5. Obstacles are
// flat-top extrusions: pillars are cylinders, walls are axis-aligned boxes.
// Heights are tuned so a stand-jump can clear the platform edge but not the
// obstacle tops — strafe-jumping skill earns elevation.

const GROUND_Y = -1.5;

// Horizontal extents of obstacle layout. Players can run between them.
// Pillars are placed in the four "shoulders" of the arena, walls form a
// central S-curve so cover lines aren't symmetric.
export const ARENA_OBSTACLES = Object.freeze([
  // Four corner pillars (cylinders). r=0.8, h=4.
  { type: "cylinder", x:  8, z:  8, r: 0.8, yMin: GROUND_Y, yMax: GROUND_Y + 4 },
  { type: "cylinder", x: -8, z:  8, r: 0.8, yMin: GROUND_Y, yMax: GROUND_Y + 4 },
  { type: "cylinder", x:  8, z: -8, r: 0.8, yMin: GROUND_Y, yMax: GROUND_Y + 4 },
  { type: "cylinder", x: -8, z: -8, r: 0.8, yMin: GROUND_Y, yMax: GROUND_Y + 4 },

  // Center totem: tall thin pillar at origin.
  { type: "cylinder", x: 0, z: 0, r: 0.5, yMin: GROUND_Y, yMax: GROUND_Y + 5 },

  // Two staggered cover walls forming a soft S-curve. Thickness 0.6, height
  // 2.4 — tall enough to break sightlines, short enough that strafe-jumps
  // can peek over the top.
  {
    type: "box",
    xMin: -5.5, xMax:  -0.5,
    zMin: -3.3, zMax:  -2.7,
    yMin: GROUND_Y, yMax: GROUND_Y + 2.4,
  },
  {
    type: "box",
    xMin:  0.5, xMax:  5.5,
    zMin:  2.7, zMax:  3.3,
    yMin: GROUND_Y, yMax: GROUND_Y + 2.4,
  },
]);

// Per-obstacle visual color palette (paint helpers; not used by physics).
// Indices match ARENA_OBSTACLES order so render code can pick per-shape tints.
export const ARENA_OBSTACLE_COLORS = Object.freeze([
  [0.55, 0.42, 0.30, 1.0],   // NE pillar — warm sand
  [0.42, 0.50, 0.55, 1.0],   // NW pillar — cool slate
  [0.48, 0.40, 0.45, 1.0],   // SE pillar — dusty plum
  [0.40, 0.46, 0.36, 1.0],   // SW pillar — moss
  [0.62, 0.55, 0.32, 1.0],   // center totem — brassy ochre
  [0.38, 0.34, 0.42, 1.0],   // wall A — deep indigo-stone
  [0.44, 0.36, 0.32, 1.0],   // wall B — terracotta
]);

// 🏃 Quake-style movement tuning for the arena (shared client/server).
export const ARENA_PHYSICS = Object.freeze({
  airAccel: 70,        // u/s² along wishdir while airborne
  groundAccel: 80,     // u/s² along wishdir on the ground
  airCapSpeed: 1.5,    // Q3 air-control window
  groundFriction: 6,
  playerRadius: 0.4,
});
