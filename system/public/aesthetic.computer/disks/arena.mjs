// arena, 2025.4.7
// Quake-style arena with large tessellated ground, player shadow, and speedometer.

/* #region 🏁 TODO
  - [] Add more arena geometry (walls, pillars)
  - [] Strafe-jumping / bunny-hop acceleration
  + Done
  - [x] Fork from fps.mjs
  - [x] Large pre-tessellated ground plane
  - [x] Speed meter HUD + FPS counter
  - [x] Gravity + space-to-jump + shift-to-crouch (Quake-style)
#endregion */

let groundPlane;
let shadowGround;  // ring + cross — standing on the ground
let shadowAir;     // diagonal X — airborne
let shadowCrouch;  // dense inner dot + outer ring — crouched
let plumbLine;     // vertical line from ground to the player's feet
let bodyFeet;      // two foot wireframes, anchored to ground + yaw
let bodyArms;      // two arm wireframes, anchored to eye + yaw
let platformEdge;  // bright outline at the ground's perimeter
let FormRef;       // captured at boot so sim/paint can build transient forms
let penLocked = false;

// Walk-cycle phase (advanced in sim while moving) for gentle arm/foot bob.
let walkPhase = 0;

// 💀 Death / respawn state
let playerAlive = true;
let deathTickAge = 0; // how long we've been dead (sim ticks, for UI fade-in)

// 🟨 Per-tile highlight state.
// hoverTile: { row, col } of tile under the crosshair, or null.
// walkedTiles: Map<tileKey, ageTicks>. A tile just stepped onto starts at
// WALK_AGE_TICKS and decays each sim tick; drawn with alpha = age/TICKS.
const WALK_AGE_TICKS = 90; // ≈ 0.75 s at 120 Hz
let hoverTile = null;
let prevPlayerTile = null;
const walkedTiles = new Map();

function tileKey(row, col) { return row * GRID + col; }
function tileFromKey(k) { return { row: Math.floor(k / GRID), col: k % GRID }; }

// World XZ → tile (row, col) or null if outside the GRID bounds.
function tileAt(worldX, worldZ) {
  const step = (GROUND_SIZE * 2) / GRID;
  const col = Math.floor((worldX + GROUND_SIZE) / step);
  const row = Math.floor((worldZ + GROUND_SIZE) / step);
  if (col < 0 || col >= GRID || row < 0 || row >= GRID) return null;
  return { row, col };
}

// AC sim runs at a fixed 120 Hz (see lib/loop.mjs updateFps). All physics and
// state updates happen in sim(), so the game runs at constant speed regardless
// of paint FPS — the CPU rasterizer dropping to 30 fps while looking at the
// ground will slow the *visual* update but not the simulation.
const SIM_HZ = 120;

// Speed tracking
let prevX = 0, prevY = 0, prevZ = 0;
let speedSmoothed = 0;
const SPEED_SMOOTH = 0.15;

// FPS tracking
let frameTimes = [];
let lastFrameTime = 0;

// Ground config. GRID is odd so there's a centre tile under the player at the
// origin (even grids put the player on a 4-way corner junction, which makes
// tile highlights look offset by half a tile).
const GROUND_SIZE = 14;
const GRID = 15;
const GROUND_Y = -1.5;
const FOG_START_SQ = 7 * 7;
const FOG_END_SQ = 14 * 14;

// 🏃 Quake-style movement tuning (AC-scaled ≈ Quake ÷ 32).
//   Canonical Quake: FOV 90°, run 320 u/s, jump 270 u/s, gravity 800 u/s².
// Here AC units ~ 32 Quake units, so numbers shrink proportionally.
const FOV = 90;
const RUN_SPEED = 10;      // u/s (≈ 320 Quake u/s)
const WALK_SPEED = 5;      // u/s crouched
const JUMP_VELOCITY = 8;   // u/s initial → peak jump = jv²/(2g) AC units
const GRAVITY = 50;        // u/s² (heavier / less lofty than Quake's 800-scaled ≈25)
const EYE_HEIGHT = 2.0;    // AC units above ground (matches prior default cam.y=-0.5)
const CROUCH_EYE = 1.2;

// 💀 Death pit — declared before fpsOpts so it can be passed as deathFloorY.
const DEATH_FLOOR_Y = -30;
const DEATH_PAD = 60;        // half-size of the lava floor (generous)
const DEATH_TILES = 14;      // (unused since donut build; kept for reference)
const DEATH_STRIP_FREQ = 0.22;
const DEATH_FLOW_SPEED = 1.8;

export const fpsOpts = {
  fov: FOV,
  y: 0,
  z: 0,
  sensitivity: 0.002,
  runSpeed: RUN_SPEED,
  walkSpeed: WALK_SPEED,
  jumpVelocity: JUMP_VELOCITY,
  gravity: GRAVITY,
  groundY: GROUND_Y,
  eyeHeight: EYE_HEIGHT,
  crouchEyeHeight: CROUCH_EYE,
  // Outside this XZ rectangle the floor clamp is disabled so the player
  // falls into the pit below. Matches the ground plane's half-size.
  groundBounds: {
    xMin: -GROUND_SIZE,
    xMax: GROUND_SIZE,
    zMin: -GROUND_SIZE,
    zMax: GROUND_SIZE,
  },
  // Dead players stop on the lava instead of falling forever.
  deathFloorY: DEATH_FLOOR_Y,
};

const BG = [45 / 255, 48 / 255, 55 / 255];
const COLOR_A = [0.38, 0.35, 0.30, 1.0];
const COLOR_B = [0.22, 0.20, 0.19, 1.0];

// Build a fresh lava floor Form with time-animated stripe colors. The lava
// is a DONUT — no geometry sits under the main ground plane, so tile-edge
// gaps in the ground rasterization can't expose lava. Only rendered outside
// the arena bounds where it's actually visible from the pit.
function buildLavaFloor(t) {
  if (!FormRef) return null;
  const positions = [];
  const colors = [];
  const deathY = DEATH_FLOOR_Y;

  const lavaTile = (x0, z0, x1, z1) => {
    const cx = (x0 + x1) / 2;
    const cz = (z0 + z1) / 2;
    const w1 = Math.sin(cx * DEATH_STRIP_FREQ - t * DEATH_FLOW_SPEED + cz * 0.08);
    const w2 = Math.sin(cx * 0.09 + cz * 0.27 - t * 0.9);
    const glow = (w1 + w2 * 0.6) * 0.5;
    const hot = 0.35 + Math.max(0, glow) * 0.65;
    const r = 0.45 + hot * 0.55;
    const g = hot * hot * 0.45;
    const b = hot * hot * hot * 0.08;
    const c = [r, g, b, 1.0];
    positions.push(
      [x0, deathY, z0, 1], [x0, deathY, z1, 1], [x1, deathY, z1, 1],
      [x0, deathY, z0, 1], [x1, deathY, z1, 1], [x1, deathY, z0, 1],
    );
    for (let i = 0; i < 6; i++) colors.push(c);
  };

  // 4 strips forming a frame around the main arena. Each strip gets its own
  // local tessellation density so the stripes still look organic.
  const STEP = 4; // lava tile size (AC units)
  const gs = GROUND_SIZE;
  // North strip: z in [gs, DEATH_PAD], x in [-DEATH_PAD, DEATH_PAD]
  for (let z = gs; z < DEATH_PAD; z += STEP) {
    for (let x = -DEATH_PAD; x < DEATH_PAD; x += STEP) {
      lavaTile(x, z, Math.min(x + STEP, DEATH_PAD), Math.min(z + STEP, DEATH_PAD));
    }
  }
  // South strip
  for (let z = -DEATH_PAD; z < -gs; z += STEP) {
    for (let x = -DEATH_PAD; x < DEATH_PAD; x += STEP) {
      lavaTile(x, z, Math.min(x + STEP, DEATH_PAD), Math.min(z + STEP, -gs));
    }
  }
  // West strip (within arena's Z range)
  for (let x = -DEATH_PAD; x < -gs; x += STEP) {
    for (let z = -gs; z < gs; z += STEP) {
      lavaTile(x, z, Math.min(x + STEP, -gs), Math.min(z + STEP, gs));
    }
  }
  // East strip
  for (let x = gs; x < DEATH_PAD; x += STEP) {
    for (let z = -gs; z < gs; z += STEP) {
      lavaTile(x, z, Math.min(x + STEP, DEATH_PAD), Math.min(z + STEP, gs));
    }
  }

  const f = new FormRef(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  return f;
}

// Ground fog: RGB lerps toward BG with distance so the arena "dissolves" into
// the sky, but alpha stays a full 1.0. Dropping alpha lets the red death floor
// show through the ground (no depth buffer → painter's order only), and the
// RGB lerp already gives the visual fade.
function fogColor(base, distSq) {
  if (distSq <= FOG_START_SQ) return base;
  if (distSq >= FOG_END_SQ) return [BG[0], BG[1], BG[2], 1.0];
  const t = (distSq - FOG_START_SQ) / (FOG_END_SQ - FOG_START_SQ);
  return [
    base[0] + (BG[0] - base[0]) * t,
    base[1] + (BG[1] - base[1]) * t,
    base[2] + (BG[2] - base[2]) * t,
    1.0,
  ];
}

function boot({ Form, penLock, system }) {
  penLock();
  FormRef = Form;

  const cam = system?.fps?.doll?.cam;
  if (cam) { prevX = cam.x; prevY = cam.y; prevZ = cam.z; }
  lastFrameTime = performance.now();

  // --- Vertex-colored checkerboard ground with fog ---
  const positions = [];
  const colors = [];
  const step = (GROUND_SIZE * 2) / GRID;

  for (let row = 0; row < GRID; row++) {
    for (let col = 0; col < GRID; col++) {
      const x0 = -GROUND_SIZE + col * step;
      const z0 = -GROUND_SIZE + row * step;
      const x1 = x0 + step;
      const z1 = z0 + step;

      const cx = (x0 + x1) / 2;
      const cz = (z0 + z1) / 2;
      const dSq = cx * cx + cz * cz;

      const base = (row + col) % 2 === 0 ? COLOR_A : COLOR_B;
      const c = fogColor(base, dSq);

      positions.push(
        [x0, GROUND_Y, z0, 1], [x0, GROUND_Y, z1, 1], [x1, GROUND_Y, z1, 1],
        [x0, GROUND_Y, z0, 1], [x1, GROUND_Y, z1, 1], [x1, GROUND_Y, z0, 1],
      );
      colors.push(c, c, c, c, c, c);
    }
  }

  groundPlane = new Form(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  groundPlane.noFade = true;

  // 🧱 Platform edge — a glowing rim so you can see where the floor ends.
  // Four line segments slightly above the ground, plus short drops at each
  // corner so the edge reads in depth even when approaching at a shallow
  // angle.
  const edgeY = GROUND_Y + 0.04;
  const edgeDrop = GROUND_Y - 0.2;
  const gs = GROUND_SIZE;
  const rim = [1, 0.9, 0.45, 0.95];
  const drop = [1, 0.35, 0.15, 0.85];
  platformEdge = new Form(
    {
      type: "line",
      positions: [
        // Perimeter
        [-gs, edgeY, -gs, 1], [ gs, edgeY, -gs, 1],
        [ gs, edgeY, -gs, 1], [ gs, edgeY,  gs, 1],
        [ gs, edgeY,  gs, 1], [-gs, edgeY,  gs, 1],
        [-gs, edgeY,  gs, 1], [-gs, edgeY, -gs, 1],
        // Corner drops (short lines falling into the pit)
        [-gs, edgeY, -gs, 1], [-gs, edgeDrop, -gs, 1],
        [ gs, edgeY, -gs, 1], [ gs, edgeDrop, -gs, 1],
        [ gs, edgeY,  gs, 1], [ gs, edgeDrop,  gs, 1],
        [-gs, edgeY,  gs, 1], [-gs, edgeDrop,  gs, 1],
      ],
      colors: [
        rim, rim, rim, rim, rim, rim, rim, rim,
        rim, drop, rim, drop, rim, drop, rim, drop,
      ],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  platformEdge.noFade = true;

  // 🫥 Feet reference — three shadow symbols swapped based on physics state,
  // plus a vertical plumb line from the ring up to the eye.
  const ringR = 0.35;
  const ringSegs = 16;

  const mkRing = (r, segs, color) => {
    const pos = [], col = [];
    for (let i = 0; i < segs; i++) {
      const a0 = (i / segs) * Math.PI * 2;
      const a1 = ((i + 1) / segs) * Math.PI * 2;
      pos.push(
        [Math.cos(a0) * r, 0, Math.sin(a0) * r, 1],
        [Math.cos(a1) * r, 0, Math.sin(a1) * r, 1],
      );
      col.push(color, color);
    }
    return { pos, col };
  };

  // Ground: ring + small cross inside.
  const g = mkRing(ringR, ringSegs, [1, 1, 1, 0.75]);
  const crossArm = ringR * 0.6;
  g.pos.push(
    [-crossArm, 0, 0, 1], [crossArm, 0, 0, 1],
    [0, 0, -crossArm, 1], [0, 0, crossArm, 1],
  );
  const gx = [1, 1, 1, 0.6];
  g.col.push(gx, gx, gx, gx);
  shadowGround = new FormRef(
    { type: "line", positions: g.pos, colors: g.col },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  shadowGround.noFade = true;

  // Air: diagonals only (bright X at feet so you can track your fall point).
  const xArm = ringR * 0.9;
  const airColor = [1, 0.9, 0.4, 0.85];
  shadowAir = new FormRef(
    {
      type: "line",
      positions: [
        [-xArm, 0, -xArm, 1], [xArm, 0, xArm, 1],
        [-xArm, 0, xArm, 1], [xArm, 0, -xArm, 1],
      ],
      colors: [airColor, airColor, airColor, airColor],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  shadowAir.noFade = true;

  // Crouch: smaller dense ring with a centre dot — reads as "compressed".
  const c = mkRing(ringR * 0.75, ringSegs, [1, 0.7, 0.3, 0.85]);
  const dotR = 0.06;
  const dot = [1, 0.85, 0.5, 0.95];
  c.pos.push(
    [-dotR, 0, 0, 1], [dotR, 0, 0, 1],
    [0, 0, -dotR, 1], [0, 0, dotR, 1],
  );
  c.col.push(dot, dot, dot, dot);
  shadowCrouch = new FormRef(
    { type: "line", positions: c.pos, colors: c.col },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  shadowCrouch.noFade = true;

  // Plumb line: unit-length along +Y at local origin. Scale.y each frame to
  // the current eye-above-ground distance. Fade alpha bottom→top so the line
  // doesn't poke into the pupil as a stark white segment.
  plumbLine = new Form(
    {
      type: "line",
      positions: [[0, 0, 0, 1], [0, 1, 0, 1]],
      colors: [[1, 1, 1, 0.45], [1, 1, 1, 0.0]],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: [1, 1, 1] },
  );
  plumbLine.noFade = true;

  // (Death floor is rebuilt each paint — see buildLavaFloor.)

  // 🦶 Feet — two stubby boxes (top + side edges as lines) anchored under the
  // player. Yaw rotates the whole form each frame so they face where you look.
  const footBox = (dx, ySpan, color) => {
    // Rectangle outline on top of the foot (local y = ySpan.max).
    const xL = dx - 0.08, xR = dx + 0.08;
    const zT = 0.35, zB = 0.05; // toe + heel in local forward (+Z)
    const yT = ySpan; // boot height
    const pts = [
      // top rectangle
      [xL, yT, zB, 1], [xR, yT, zB, 1],
      [xR, yT, zB, 1], [xR, yT, zT, 1],
      [xR, yT, zT, 1], [xL, yT, zT, 1],
      [xL, yT, zT, 1], [xL, yT, zB, 1],
      // four corners dropping to ground
      [xL, 0, zB, 1], [xL, yT, zB, 1],
      [xR, 0, zB, 1], [xR, yT, zB, 1],
      [xR, 0, zT, 1], [xR, yT, zT, 1],
      [xL, 0, zT, 1], [xL, yT, zT, 1],
    ];
    const cols = pts.map(() => color);
    return { pts, cols };
  };
  const footColor = [0.9, 0.9, 1.0, 0.85];
  const leftFoot = footBox(-0.18, 0.15, footColor);
  const rightFoot = footBox(0.18, 0.15, footColor);
  bodyFeet = new Form(
    {
      type: "line",
      positions: [...leftFoot.pts, ...rightFoot.pts],
      colors: [...leftFoot.cols, ...rightFoot.cols],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  bodyFeet.noFade = true;

  // 🤲 Arms — two angled line segments from shoulder to wrist, extending
  // forward and down from the eye. Color fades darker at the wrists so they
  // don't scream at the reader.
  const armLocal = (side) => {
    const sx = 0.28 * side;
    const shoulder = [sx, -0.35, 0.05, 1];
    const elbow = [sx * 0.9, -0.55, 0.45, 1];
    const wrist = [sx * 0.75, -0.65, 0.75, 1];
    const shoulderCol = [1, 1, 1, 0.7];
    const wristCol = [1, 0.9, 0.7, 0.9];
    return {
      pts: [shoulder, elbow, elbow, wrist],
      cols: [shoulderCol, wristCol, wristCol, wristCol],
    };
  };
  const leftArm = armLocal(-1);
  const rightArm = armLocal(+1);
  bodyArms = new Form(
    {
      type: "line",
      positions: [...leftArm.pts, ...rightArm.pts],
      colors: [...leftArm.cols, ...rightArm.cols],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  bodyArms.noFade = true;
}

function sim({ system }) {
  const doll = system?.fps?.doll;
  const cam = doll?.cam;
  if (!cam) return;

  // Always use the *logical* player position (not the render camera) for
  // gameplay state. In 3P mode cam.x/z is offset behind the player.
  const phys = doll?.physics;
  const playerCamX = phys?.playerCamX ?? cam.x;
  const playerCamY = phys?.playerCamY ?? cam.y;
  const playerCamZ = phys?.playerCamZ ?? cam.z;

  // Horizontal speed only (ignore vertical so jump bursts don't spike the bar).
  const dx = playerCamX - prevX;
  const dz = playerCamZ - prevZ;
  const speed = Math.sqrt(dx * dx + dz * dz);
  speedSmoothed += (speed - speedSmoothed) * SPEED_SMOOTH;
  prevX = playerCamX; prevY = playerCamY; prevZ = playerCamZ;

  // Walk cycle phase advances with horizontal speed; fallback to slow drift.
  walkPhase += Math.max(0.002, speedSmoothed) * 8;

  // Player world position — cam stores negated world coords (see Camera
  // #transform). cam.y is the one exception and uses opposite inversion.
  const pWorldX = -playerCamX;
  const pWorldZ = -playerCamZ;
  const pWorldY = -playerCamY;

  // --- 💀 Death detection: hitting the red floor kills the player. ---
  if (playerAlive && pWorldY <= DEATH_FLOOR_Y + EYE_HEIGHT + 0.05) {
    playerAlive = false;
    deathTickAge = 0;
    doll.setFrozen?.(true);
    doll.clearHeldKeys?.();
  }
  if (!playerAlive) deathTickAge += 1;

  // --- Walked-tile trail: when the player's current tile changes, stamp it. ---
  const curTile = tileAt(pWorldX, pWorldZ);
  if (curTile) {
    const key = tileKey(curTile.row, curTile.col);
    if (!prevPlayerTile || prevPlayerTile !== key) {
      walkedTiles.set(key, WALK_AGE_TICKS);
      prevPlayerTile = key;
    } else {
      // Also refresh the age while standing still so the glow lingers under you.
      walkedTiles.set(key, WALK_AGE_TICKS);
    }
  }
  // Age + prune.
  for (const [k, age] of walkedTiles) {
    const next = age - 1;
    if (next <= 0) walkedTiles.delete(k);
    else walkedTiles.set(k, next);
  }

  // --- Hover tile: raycast camera-forward onto the ground plane. ---
  // Forward in world (rotY=0 → +Z). rotX positive = look up, so forward.y
  // = sin(rotX). If forward.y >= 0 we aren't looking at the floor.
  const rx = cam.rotX * Math.PI / 180;
  const ry = cam.rotY * Math.PI / 180;
  const cosPitch = Math.cos(rx);
  const fx = Math.sin(ry) * cosPitch;
  const fy = Math.sin(rx);
  const fz = Math.cos(ry) * cosPitch;
  hoverTile = null;
  if (fy < -0.05) {
    const camWorldY = -cam.y;
    const t = (GROUND_Y - camWorldY) / fy;
    if (t > 0 && t < 200) {
      const hitX = pWorldX + t * fx;
      const hitZ = pWorldZ + t * fz;
      hoverTile = tileAt(hitX, hitZ);
    }
  }

  // --- Anchor shadow / plumb / body at the logical player position (see top
  //     of sim). Reusing playerCam* captured above. ---
  const playerWorldY = pWorldY;
  const feetY = GROUND_Y + 0.01;

  const shadows = [shadowGround, shadowAir, shadowCrouch];
  for (const s of shadows) {
    if (!s) continue;
    s.position[0] = playerCamX;
    s.position[1] = feetY;
    s.position[2] = playerCamZ;
  }
  if (plumbLine) {
    plumbLine.position[0] = playerCamX;
    plumbLine.position[1] = feetY;
    plumbLine.position[2] = playerCamZ;
    plumbLine.scale[1] = Math.max(0, playerWorldY - GROUND_Y - 0.02);
  }

  if (bodyFeet) {
    const footBaseY = playerAlive
      ? GROUND_Y
      : playerWorldY - EYE_HEIGHT;
    bodyFeet.position[0] = playerCamX;
    bodyFeet.position[1] = footBaseY;
    bodyFeet.position[2] = playerCamZ;
    bodyFeet.rotation[1] = cam.rotY;
  }
  if (bodyArms) {
    const crouchDrop = (phys?.crouch ?? 0) * 0.2;
    const bob = Math.sin(walkPhase) * 0.03 * Math.min(1, speedSmoothed * 40);
    bodyArms.position[0] = playerCamX;
    bodyArms.position[1] = playerWorldY - crouchDrop + bob;
    bodyArms.position[2] = playerCamZ;
    bodyArms.rotation[1] = cam.rotY;
  }
}

function paint({ wipe, ink, screen, write, box, system }) {
  // FPS calc
  const now = performance.now();
  const dt = now - lastFrameTime;
  lastFrameTime = now;
  frameTimes.push(dt);
  if (frameTimes.length > 60) frameTimes.shift();
  const avgDt = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const fps = Math.round(1000 / avgDt);

  // --- Tile highlights: build a single transient Form containing one quad
  // per visible highlight (hover + walked trail). Drawn *after* the ground,
  // just above floor-Y, with additive-ish semi-transparent tint.
  const step = (GROUND_SIZE * 2) / GRID;
  const hiPos = [];
  const hiCol = [];
  const pushQuad = (row, col, color) => {
    const x0 = -GROUND_SIZE + col * step;
    const z0 = -GROUND_SIZE + row * step;
    const x1 = x0 + step;
    const z1 = z0 + step;
    const y = GROUND_Y + 0.015;
    hiPos.push(
      [x0, y, z0, 1], [x0, y, z1, 1], [x1, y, z1, 1],
      [x0, y, z0, 1], [x1, y, z1, 1], [x1, y, z0, 1],
    );
    for (let i = 0; i < 6; i++) hiCol.push(color);
  };
  // Walked trail (bright yellow, fades with age — kept highly visible).
  // Skip the tile under the player's feet so the active standing tile reads as
  // "present" rather than already part of the trail.
  for (const [k, age] of walkedTiles) {
    if (k === prevPlayerTile) continue;
    const { row, col } = tileFromKey(k);
    const alpha = (age / WALK_AGE_TICKS) * 0.85;
    pushQuad(row, col, [1.0, 0.95, 0.3, alpha]);
  }
  // Hover (cyan, steady, steady alpha).
  if (hoverTile) {
    pushQuad(hoverTile.row, hoverTile.col, [0.4, 0.95, 1.0, 0.55]);
  }

  // Render scene — lava donut first (never under the main ground), then the
  // ground, its glowing edge, tile highlights, then the feet shadow + body.
  wipe(45, 48, 55);
  const lava = buildLavaFloor(now / 1000);
  if (lava) ink(255).form(lava);
  ink(255).form(groundPlane);
  if (platformEdge) ink(255).form(platformEdge);
  if (hiPos.length > 0 && FormRef) {
    const hi = new FormRef(
      { type: "triangle", positions: hiPos, colors: hiCol },
      { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
    );
    hi.noFade = true;
    ink(255, 255, 255).form(hi);
  }

  // Pick the correct shadow symbol for the current physics state.
  const phys = system?.fps?.doll?.physics;
  let activeShadow = shadowGround;
  if (phys) {
    if (!phys.onGround) activeShadow = shadowAir;
    else if (phys.crouch > 0.5) activeShadow = shadowCrouch;
  }
  // Only draw ground-anchored shadow/plumb while on solid ground.
  const onSolidGround = phys?.onGround;
  if (activeShadow && onSolidGround) ink(255, 255, 255).form(activeShadow);
  if (plumbLine && onSolidGround && plumbLine.scale[1] > 0.05) {
    ink(255, 255, 255).form(plumbLine);
  }
  // Feet + arms render regardless of ground state (they fall with you).
  if (bodyFeet) ink(255).form(bodyFeet);
  if (bodyArms) ink(255).form(bodyArms);

  // --- HUD (top-right) ---
  const font = "MatrixChunky8";
  const margin = 4;
  const lineH = 10;
  const rX = screen.width - margin; // right edge
  const rightLabel = (txt, y) => {
    // MatrixChunky8 glyphs are ~4px wide; right-align by char count.
    write(txt, { x: rX - txt.length * 4, y }, undefined, undefined, false, font);
  };

  // FPS
  ink(fps >= 30 ? "lime" : fps >= 15 ? "yellow" : "red");
  rightLabel(`${fps} FPS`, margin);

  // Frame time
  ink(180, 180, 180);
  rightLabel(`${avgDt.toFixed(1)}ms`, margin + lineH);

  // FOV + run speed (Quake-style spec)
  ink(150, 200, 255);
  rightLabel(`FOV ${FOV}`, margin + lineH * 2);
  rightLabel(`RUN ${RUN_SPEED.toFixed(1)}u/s`, margin + lineH * 3);

  // Grounded / airborne indicator (reuses `phys` captured above).
  if (phys) {
    const airborne = !phys.onGround;
    const crouching = phys.crouch > 0.5;
    ink(airborne ? "yellow" : crouching ? "orange" : "lime");
    rightLabel(
      airborne ? "AIR" : crouching ? "CROUCH" : "GROUND",
      margin + lineH * 4,
    );

    // POV indicator — 1P or 3P (middle-mouse toggles).
    ink(phys.thirdPerson ? "magenta" : "cyan");
    rightLabel(phys.thirdPerson ? "3P" : "1P", margin + lineH * 5);
  }

  // Speed meter (bottom-center). speedSmoothed is per-sim-tick position delta;
  // sim runs at SIM_HZ, so ups = perTickDelta * SIM_HZ.
  const ups = speedSmoothed * SIM_HZ;
  const barMaxUPS = RUN_SPEED * 1.2; // headroom for strafe-jump bonuses later
  const barFill = Math.min(1, ups / barMaxUPS);

  const barW = Math.min(160, Math.floor(screen.width * 0.4));
  const barH = 6;
  const barX = Math.floor((screen.width - barW) / 2);
  const barY = screen.height - 16;

  ink(0, 0, 0, 140);
  box(barX - 1, barY - 1, barW + 2, barH + 2);

  const cr = barFill > 0.5 ? Math.floor(255 * ((barFill - 0.5) * 2)) : 0;
  const cg = barFill < 0.5 ? 255 : Math.floor(255 * (1 - (barFill - 0.5) * 2));
  ink(cr, cg, 50, 220);
  box(barX, barY, Math.floor(barW * barFill), barH);

  ink("white");
  write(`${ups.toFixed(1)} u/s`, { x: barX + barW + 4, y: barY - 1 }, undefined, undefined, false, font);

  // --- 💀 Death screen overlay (fade-in) ---
  if (!playerAlive) {
    const fade = Math.min(1, deathTickAge / 24); // ~0.2 s ramp
    ink(60, 0, 0, Math.floor(fade * 160));
    box(0, 0, screen.width, screen.height);

    const cx = screen.width / 2;
    const cy = screen.height / 2;
    ink(255, 80, 80, Math.floor(fade * 255));
    const died = "YOU DIED";
    // MatrixChunky8 ≈ 4px/char; centre roughly.
    write(died, { x: Math.floor(cx - died.length * 4), y: Math.floor(cy - 12) }, undefined, undefined, false, font);
    if (deathTickAge > 30) {
      ink(230, 230, 230, 220);
      const prompt = "TAP TO RESPAWN";
      write(prompt, { x: Math.floor(cx - prompt.length * 4), y: Math.floor(cy + 6) }, undefined, undefined, false, font);
    }
  }
}

function act({ event: e, penLock, system }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;

  // 🎥 Middle-mouse toggles third-person (press once to enter, press again
  // to exit). Only trigger on touch so the release doesn't also flip.
  if (e.device === "mouse" && e.button === 1 && e.is("touch")) {
    system?.fps?.doll?.toggleThirdPerson?.();
  }

  // While dead, any touch respawns; otherwise the first touch re-locks the pen.
  if (e.is("touch")) {
    if (!playerAlive && deathTickAge > 30) {
      playerAlive = true;
      deathTickAge = 0;
      walkedTiles.clear();
      prevPlayerTile = null;
      system?.fps?.doll?.respawn?.(0, 0);
      return;
    }
    // Don't re-lock on middle-click — user is using it for 3P toggle.
    if (!penLocked && e.button !== 1) penLock();
  }
}

export const system = "fps";
export { boot, sim, paint, act };
