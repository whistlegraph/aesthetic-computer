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
let groundSkirt;   // solid opaque plate just under the ground that blocks
                   // any lava bleed-through between ground tile seams
let platformBlock;  // bottom and side faces for platform volume
let shadowGround;  // ring + cross — standing on the ground
let shadowAir;     // diagonal X — airborne
let shadowCrouch;  // dense inner dot + outer ring — crouched
let plumbLine;     // vertical line from ground to the player's feet
let bodyFeet;      // two foot wireframes, anchored to ground + yaw
let bodyArms;      // two arm wireframes, anchored to eye + yaw
let platformEdge;  // bright outline at the ground's perimeter
let FormRef;       // captured at boot so sim/paint can build transient forms
let penLocked = false;

// 📱 Mobile control buttons using TextButton UI component
let mobileButtons = {}; // { up, down, left, right, jump, crouch }
let mobileButtonStates = {}; // track which buttons are pressed

// ⌨️ Keyboard state tracking (for lighting up buttons when keys are held)
let keyboardState = {
  w: false, a: false, s: false, d: false,
  arrowup: false, arrowdown: false, arrowleft: false, arrowright: false,
  space: false, shift: false,
};

// 🎯 Custom cursor for locked (FPS) mode
let cursorClickTime = 0;
const CURSOR_CLICK_DURATION = 150; // ms to expand on click

// Create SVG cursor strings
const createCursor = (size = 24) => {
  // Thin crosshair cursor
  const svg = `<svg width="${size}" height="${size}" xmlns="http://www.w3.org/2000/svg">
    <defs>
      <style>
        line { stroke: #fff; stroke-width: 1; opacity: 0.8; }
        circle { fill: none; stroke: #fff; stroke-width: 1; opacity: 0.6; }
      </style>
    </defs>
    <!-- Vertical line -->
    <line x1="${size/2}" y1="2" x2="${size/2}" y2="${size-2}" />
    <!-- Horizontal line -->
    <line x1="2" y1="${size/2}" x2="${size-2}" y2="${size/2}" />
    <!-- Center dot -->
    <circle cx="${size/2}" cy="${size/2}" r="1.5" />
  </svg>`;

  const encoded = btoa(svg).replace(/\+/g, '-').replace(/\//g, '_');
  return `url('data:image/svg+xml;base64,${encoded}') ${size/2} ${size/2}, auto`;
};

const cursorNormal = createCursor(24);
const cursorExpanded = createCursor(32); // larger for click feedback

// Walk-cycle phase (advanced in sim while moving) for gentle arm/foot bob.
let walkPhase = 0;

// 🐛 Debug: dump a snapshot of scene state once per second so it can be
// pasted back verbatim when something looks off.
let debugDumpTimer = 0;
const DEBUG_DUMP_INTERVAL = 120; // sim ticks (= 1 s at SIM_HZ=120)

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

// 🔍 Diagnostic: stash the last hit point + pen coords so paint() can draw a
// visible crosshair and the snapshot can log exactly where the ray landed.
let lastHitWorld = null;   // [x, z] or null
let lastPenScreen = null;  // [x, y] or null

// Axis-sign experiment toggle. Press F to cycle. See the hover raycast for
// what each bit flips.
//   0 = baseline          1 = flipX
//   2 = flipZ             3 = flip both
let hoverFlipMode = 3; // flip both X and Z per recent experiments

// ⚡ Adaptive-quality flags driven by measured render FPS. Auto-toggle in
// paint() based on the rolling frame-time average. Pieces can override via the
// HUD labels (future: click to pin). "LOW" = coarser tile, static lava, skip
// body wireframes. "MED" = static lava only. "HIGH" = everything on.
let perfLowMode = false;
let perfMedMode = false;
const PERF_LOW_MS = 25;   // below ~40fps → drop to LOW
const PERF_MED_MS = 18;   // below ~55fps → drop to MED
const PERF_HIGH_MS = 14;  // above ~70fps → return to HIGH
let perfSamplesSinceSwitch = 0;

// 🔎 Camera zoom — wheel scroll steps between 1P and 3P at discrete distances.
// Level 0 = first person. Levels 1..N = third person, pulling the camera back
// further each click. More close-in steps for shoulder-camera views.
// Middle-mouse still toggles between 1P and a default 3P.
const ZOOM_DISTANCES = [0, 0.5, 1, 1.5, 2, 3, 4.5, 6, 9, 12, 16, 24, 32];
let zoomLevel = 2; // Start at 1 unit back (shoulder camera)

// 🎥 Player facing direction (decoupled from camera rotation)
let playerFacing = 0; // Player body Y rotation (degrees), independent from camera

// 🎥 Right-click camera orbit (3P mode only): rotate camera around player
// without changing player body rotation. Orbits by modifying XZ offset.
let orbitAngle = 0;   // extra Y rotation for camera only (degrees)
let orbiting = false; // currently dragging with right button
let orbitDistance = 0; // captured XZ distance when orbit starts, stays constant
let appliedOrbitOffset = [0, 0]; // track X,Z offset we applied so we can undo it
let baseRotY = 0;     // cam.rotY when orbit started, to prevent player spinning
let orbitSnapped = false; // true if orbit was released; reset on next left-click

function applyZoom(doll) {
  if (!doll) return;
  const d = ZOOM_DISTANCES[zoomLevel];
  if (zoomLevel === 0) doll.setThirdPerson(false);
  else doll.setThirdPerson(true, d);
}

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
  // Disable built-in camdoll touch controls; arena handles custom mobile UI instead
  disableTouchControls: true,
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

// Build a fresh lava floor Form with time-animated stripe colors. Now that
// the Z-buffer correctly occludes, the lava is a FULL plane covering the
// entire pit area — no donut cut-out needed. The ground depth-tests above it.
// When perfLowMode is on, we build the lava once and cache it (no animation).
let lavaCache = null;
let lavaCacheFrame = -1;

function buildLavaFloor(t) {
  if (!FormRef) return null;
  // When perf mode is low, return a cached static lava (skip stripe animation).
  if (perfLowMode && lavaCache) return lavaCache;

  const positions = [];
  const colors = [];
  const deathY = DEATH_FLOOR_Y;
  const STEP = perfLowMode ? 8 : 4; // coarser tessellation in low-perf mode
  for (let z = -DEATH_PAD; z < DEATH_PAD; z += STEP) {
    for (let x = -DEATH_PAD; x < DEATH_PAD; x += STEP) {
      const x0 = x, z0 = z;
      const x1 = Math.min(x + STEP, DEATH_PAD);
      const z1 = Math.min(z + STEP, DEATH_PAD);
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
    }
  }
  const f = new FormRef(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  if (perfLowMode) lavaCache = f;
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

function boot({ Form, penLock, system, screen, ui, canvas }) {
  penLock();
  FormRef = Form;

  const cam = system?.fps?.doll?.cam;
  if (cam) { prevX = cam.x; prevY = cam.y; prevZ = cam.z; }
  lastFrameTime = performance.now();

  // 🎯 Set up custom cursor on canvas
  if (canvas) {
    canvas.style.cursor = 'auto';
  }

  // 📱 Create mobile control buttons using ui.Button (always enabled for testing/development)
  if (screen && ui?.Button) {
    const btnSize = 28;
    const btnSizeLarge = 32;
    const gap = 4;  // gap between buttons (no border overlap)
    const padding = 6;
    const bottomMargin = 6;

    // Movement buttons (bottom-left): D-pad style, compact layout
    const moveX = padding;
    const moveY = screen.height - (btnSize * 3 + gap * 2 + bottomMargin);

    // Action buttons (bottom-right): compact
    const actionX = screen.width - btnSizeLarge - padding;
    const actionY = screen.height - (btnSizeLarge * 2 + gap + bottomMargin);

    // D-pad layout:
    //     UP
    //  LT MD RT
    //    DOWN
    mobileButtons = {
      up: { btn: new ui.Button(moveX + btnSize + gap, moveY, btnSize, btnSize), key: "forward", label: "UP" },
      down: { btn: new ui.Button(moveX + btnSize + gap, moveY + (btnSize + gap) * 2, btnSize, btnSize), key: "back", label: "DN" },
      left: { btn: new ui.Button(moveX, moveY + btnSize + gap, btnSize, btnSize), key: "left", label: "LT" },
      right: { btn: new ui.Button(moveX + (btnSize + gap) * 2, moveY + btnSize + gap, btnSize, btnSize), key: "right", label: "RT" },
      jump: { btn: new ui.Button(actionX, actionY, btnSizeLarge, btnSizeLarge), key: "jump", label: "JUMP" },
      crouch: { btn: new ui.Button(actionX, actionY + btnSizeLarge + gap, btnSizeLarge, btnSizeLarge), key: "crouch", label: "CRCH" },
    };
  }

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

  // 🔲 Ground skirt — a single opaque dark quad at the EXACT SAME Y as the
  // main ground plane, covering a slightly oversized XZ footprint. Drawn
  // BEFORE the ground so any rasterizer seams between ground tiles (painter's
  // order, no depth buffer) reveal the dark skirt instead of the lava far
  // below. The +0.5 AC-unit pad ensures the skirt also catches seams at the
  // platform outer edge.
  // Skirt sits 0.02 below the ground so painter's order places ground clearly
  // on top; oversized by 0.5 AC units on each side so the outer edge of the
  // arena also has a backstop.
  const skirtY = GROUND_Y - 0.02;
  const skirtR = GROUND_SIZE + 0.5;
  const skirtColor = [0.06, 0.06, 0.08, 1.0];
  groundSkirt = new Form(
    {
      type: "triangle",
      positions: [
        [-skirtR, skirtY, -skirtR, 1],
        [-skirtR, skirtY,  skirtR, 1],
        [ skirtR, skirtY,  skirtR, 1],
        [-skirtR, skirtY, -skirtR, 1],
        [ skirtR, skirtY,  skirtR, 1],
        [ skirtR, skirtY, -skirtR, 1],
      ],
      colors: [skirtColor, skirtColor, skirtColor, skirtColor, skirtColor, skirtColor],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  groundSkirt.noFade = true;

  // 🧱 Platform block — bottom and side faces to give the arena volume
  const platformDepth = 2.0; // thickness of the platform block
  const bottomY = GROUND_Y - platformDepth;
  const sideColor = [0.18, 0.16, 0.14, 1.0]; // darker than ground for depth
  const sideStriped = [0.28, 0.25, 0.22, 1.0]; // lighter stripe for pattern
  const platformGs = GROUND_SIZE;

  const platformPositions = [];
  const platformColors = [];

  // Bottom face (two triangles, full area)
  platformPositions.push(
    [-platformGs, bottomY, -platformGs, 1], [-platformGs, bottomY, platformGs, 1], [platformGs, bottomY, platformGs, 1],
    [-platformGs, bottomY, -platformGs, 1], [platformGs, bottomY, platformGs, 1], [platformGs, bottomY, -platformGs, 1],
  );
  for (let i = 0; i < 6; i++) platformColors.push(sideColor);

  // Side faces with alternating stripe pattern for visual interest
  const sideStep = (platformGs * 2) / 8; // stripe width

  // North side (-Z direction)
  for (let i = 0; i < 8; i++) {
    const x0 = -platformGs + i * sideStep;
    const x1 = x0 + sideStep;
    const stripeColor = (i & 1) ? sideStriped : sideColor;
    platformPositions.push(
      [x0, GROUND_Y, -platformGs, 1], [x0, bottomY, -platformGs, 1], [x1, bottomY, -platformGs, 1],
      [x0, GROUND_Y, -platformGs, 1], [x1, bottomY, -platformGs, 1], [x1, GROUND_Y, -platformGs, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(stripeColor);
  }

  // South side (+Z direction)
  for (let i = 0; i < 8; i++) {
    const x0 = -platformGs + i * sideStep;
    const x1 = x0 + sideStep;
    const stripeColor = (i & 1) ? sideStriped : sideColor;
    platformPositions.push(
      [x0, GROUND_Y, platformGs, 1], [x1, bottomY, platformGs, 1], [x1, GROUND_Y, platformGs, 1],
      [x0, GROUND_Y, platformGs, 1], [x0, bottomY, platformGs, 1], [x1, bottomY, platformGs, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(stripeColor);
  }

  // East side (+X direction)
  for (let i = 0; i < 8; i++) {
    const z0 = -platformGs + i * sideStep;
    const z1 = z0 + sideStep;
    const stripeColor = (i & 1) ? sideStriped : sideColor;
    platformPositions.push(
      [platformGs, GROUND_Y, z0, 1], [platformGs, bottomY, z0, 1], [platformGs, bottomY, z1, 1],
      [platformGs, GROUND_Y, z0, 1], [platformGs, bottomY, z1, 1], [platformGs, GROUND_Y, z1, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(stripeColor);
  }

  // West side (-X direction)
  for (let i = 0; i < 8; i++) {
    const z0 = -platformGs + i * sideStep;
    const z1 = z0 + sideStep;
    const stripeColor = (i & 1) ? sideStriped : sideColor;
    platformPositions.push(
      [-platformGs, GROUND_Y, z0, 1], [-platformGs, bottomY, z1, 1], [-platformGs, GROUND_Y, z1, 1],
      [-platformGs, GROUND_Y, z0, 1], [-platformGs, bottomY, z0, 1], [-platformGs, bottomY, z1, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(stripeColor);
  }

  platformBlock = new Form(
    { type: "triangle", positions: platformPositions, colors: platformColors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  platformBlock.noFade = true;

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

function sim({ system, pen, screen }) {
  const doll = system?.fps?.doll;
  const cam = doll?.cam;
  if (!cam) return;

  // 📱 Update mobile button states
  if (mobileButtons && doll) {
    for (const [name, btnData] of Object.entries(mobileButtons)) {
      const isPressed = btnData.btn?.down ?? false;
      const wasPressed = mobileButtonStates[name] ?? false;

      if (isPressed && !wasPressed) {
        doll.setMovement(btnData.key, true);
        mobileButtonStates[name] = true;
      } else if (!isPressed && wasPressed) {
        doll.setMovement(btnData.key, false);
        mobileButtonStates[name] = false;
      }
    }
  }

  // Undo any orbit offset we applied last frame so it doesn't affect physics
  cam.x -= appliedOrbitOffset[0];
  cam.z -= appliedOrbitOffset[1];
  appliedOrbitOffset[0] = 0;
  appliedOrbitOffset[1] = 0;

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
  // Apply the same hoverFlipMode flip to player position so walked tiles match raycast.
  const flipX = (hoverFlipMode & 1) !== 0;
  const flipZ = (hoverFlipMode & 2) !== 0;
  const playerX = flipX ? -pWorldX : pWorldX;
  const playerZ = flipZ ? -pWorldZ : pWorldZ;
  const curTile = tileAt(playerX, playerZ);
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

  // --- Hover tile: fire a proper mouse ray from the pen's screen pixel into
  // the 3D scene, then intersect with the ground plane. When pen-locked the
  // pen stops updating so we fall back to screen-centre (the crosshair). ---
  //
  // Pipeline: screen (px,py) → NDC → camera-space ray dir (u,v,1) → rotate
  // by camera orientation → world ray → plane intersect.
  const rx = cam.rotX * Math.PI / 180;
  const ry = cam.rotY * Math.PI / 180;
  const sinRotX = Math.sin(rx), cosRotX = Math.cos(rx);
  const sinRotY = Math.sin(ry), cosRotY = Math.cos(ry);

  // Use pen position when available, otherwise centre of screen.
  const sw = screen?.width ?? 1, sh = screen?.height ?? 1;
  const mx = penLocked ? sw / 2 : (pen?.x ?? sw / 2);
  const my = penLocked ? sh / 2 : (pen?.y ?? sh / 2);
  const ndcX = (2 * mx) / sw - 1;
  const ndcY = 1 - (2 * my) / sh;
  const tanHalfFov = Math.tan((FOV * Math.PI / 180) / 2);
  const aspect = sw / sh;
  // Camera-space ray direction at this screen pixel (before rotating).
  const u = ndcX * aspect * tanHalfFov;
  const v = ndcY * tanHalfFov;
  // Rotate camera-space (u, v, 1) to world using (rotY(+rotY) ∘ rotX(-rotX)).
  const fx = u * cosRotY - v * sinRotY * sinRotX + sinRotY * cosRotX;
  const fy = v * cosRotX + sinRotX;
  const fz = -u * sinRotY - v * cosRotY * sinRotX + cosRotY * cosRotX;

  hoverTile = null;
  lastHitWorld = null;
  lastPenScreen = penLocked ? null : [mx, my];
  if (fy < -0.001) {
    // Ray origin + direction. The hoverFlipMode toggle lets us A/B all four
    // axis-sign combinations without editing source — press F to cycle.
    //   0 = no flip              (baseline derivation)
    //   1 = flip X (hit.x)
    //   2 = flip Z (hit.z)
    //   3 = flip both
    const flipX = (hoverFlipMode & 1) !== 0;
    const flipZ = (hoverFlipMode & 2) !== 0;
    const camWorldX = flipX ? cam.x  : -cam.x;
    const camWorldY = -cam.y;
    const camWorldZ = flipZ ? cam.z  : -cam.z;
    const fxAdj     = flipX ? -fx    : fx;
    const fzAdj     = flipZ ? -fz    : fz;
    const t = (GROUND_Y - camWorldY) / fy;
    if (t > 0 && t < 200) {
      const hitX = camWorldX + t * fxAdj;
      const hitZ = camWorldZ + t * fzAdj;
      hoverTile = tileAt(hitX, hitZ);
      lastHitWorld = [hitX, hitZ];
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

  // Update player facing direction (camera rotY when not orbiting)
  if (!orbiting) {
    playerFacing = cam.rotY;
  }

  if (bodyFeet) {
    const footBaseY = playerAlive
      ? GROUND_Y
      : playerWorldY - EYE_HEIGHT;
    bodyFeet.position[0] = playerCamX;
    bodyFeet.position[1] = footBaseY;
    bodyFeet.position[2] = playerCamZ;
    bodyFeet.rotation[1] = playerFacing;
  }
  if (bodyArms) {
    const crouchDrop = (phys?.crouch ?? 0) * 0.2;
    const bob = Math.sin(walkPhase) * 0.03 * Math.min(1, speedSmoothed * 40);
    bodyArms.position[0] = playerCamX;
    bodyArms.position[1] = playerWorldY - crouchDrop + bob;
    bodyArms.position[2] = playerCamZ;
    bodyArms.rotation[1] = playerFacing;
  }

  // 🐛 Once-per-second debug dump. Paste this back into chat to diagnose.
  debugDumpTimer += 1;
  if (debugDumpTimer >= DEBUG_DUMP_INTERVAL) {
    debugDumpTimer = 0;
    const f2 = (n) => (typeof n === "number" ? n.toFixed(2) : String(n));
    const hoverStr = hoverTile
      ? `${hoverTile.row},${hoverTile.col}`
      : "none";
    const penStr = pen
      ? `pen=(${f2(pen.x)}, ${f2(pen.y)})`
      : "pen=null";
    const hitStr = lastHitWorld
      ? `(${f2(lastHitWorld[0])}, ${f2(lastHitWorld[1])})`
      : "none";
    console.log(
      "🏟️ arena snapshot:",
      JSON.stringify({
        player: { x: f2(pWorldX), y: f2(pWorldY), z: f2(pWorldZ) },
        cam: {
          x: f2(cam.x), y: f2(cam.y), z: f2(cam.z),
          rotX: f2(cam.rotX), rotY: f2(cam.rotY),
        },
        phys: phys ? {
          onGround: phys.onGround,
          crouch: f2(phys.crouch),
          yVel: f2(phys.worldYVel),
          thirdPerson: phys.thirdPerson,
        } : null,
        state: { alive: playerAlive, penLocked, walkPhase: f2(walkPhase) },
        tiles: {
          current: prevPlayerTile,
          hover: hoverStr,
          hitWorldXZ: hitStr,
          trailCount: walkedTiles.size,
        },
        mouse: `${penStr} screen=${f2(screen?.width)}x${f2(screen?.height)}`,
        rayDir: `fx=${f2(fx)} fy=${f2(fy)} fz=${f2(fz)}`,
        speed: f2(speedSmoothed * SIM_HZ) + " u/s",
      }, null, 2),
    );
  }

  // 🎥 Camera orbit effect (3P mode only): rotate camera around player by
  // modifying the XZ offset without changing cam.rotY (so player body stays put).
  // Orbit angle persists after release until left-click is pressed.

  if (zoomLevel > 0 && Math.abs(orbitAngle) > 0.01 && orbitDistance > 0) {
    const pCamX = phys?.playerCamX ?? cam.x;
    const pCamZ = phys?.playerCamZ ?? cam.z;
    // Use captured orbit distance (constant throughout orbit) to maintain radius
    const orbitRad = orbitAngle * Math.PI / 180;
    const newX = pCamX + orbitDistance * Math.sin(orbitRad);
    const newZ = pCamZ + orbitDistance * Math.cos(orbitRad);
    // Track the total change we're making so we can undo it next frame
    appliedOrbitOffset[0] = newX - cam.x;
    appliedOrbitOffset[1] = newZ - cam.z;
    cam.x = newX;
    cam.z = newZ;
  }
}

function paint({ wipe, ink, screen, write, box, system, pen, canvas, now }) {
  // FPS calc (now is passed as parameter from bios)
  const dt = now - lastFrameTime;
  lastFrameTime = now;
  frameTimes.push(dt);
  if (frameTimes.length > 60) frameTimes.shift();
  const avgDt = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const fps = Math.round(1000 / avgDt);

  // ⚡ Adaptive quality — switch modes with hysteresis so we don't flip every
  // frame. Require several samples of sustained FPS before changing state.
  perfSamplesSinceSwitch += 1;
  if (perfSamplesSinceSwitch > 30) {
    if (avgDt > PERF_LOW_MS && !perfLowMode) {
      perfLowMode = true; perfMedMode = true; perfSamplesSinceSwitch = 0;
      lavaCache = null; // force rebuild at low tessellation
    } else if (avgDt > PERF_MED_MS && !perfMedMode) {
      perfMedMode = true; perfSamplesSinceSwitch = 0;
    } else if (avgDt < PERF_HIGH_MS && perfLowMode) {
      perfLowMode = false; perfSamplesSinceSwitch = 0;
      lavaCache = null; // rebuild each frame again
    } else if (avgDt < PERF_HIGH_MS && perfMedMode) {
      perfMedMode = false; perfSamplesSinceSwitch = 0;
    }
  }

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
    // Add dithering: per-vertex noise to break up solid color fields.
    for (let i = 0; i < 6; i++) {
      const noise = (Math.random() - 0.5) * 0.08; // ±4% noise on each channel
      hiCol.push([
        Math.max(0, Math.min(1, color[0] + noise)),
        Math.max(0, Math.min(1, color[1] + noise)),
        Math.max(0, Math.min(1, color[2] + noise)),
        color[3],
      ]);
    }
  };
  // Walked trail (bright yellow, fades with age — kept highly visible).
  // Skip the tile under the player's feet so the active standing tile reads as
  // "present" rather than already part of the trail.
  for (const [k, age] of walkedTiles) {
    if (k === prevPlayerTile) continue;
    const { row, col } = tileFromKey(k);
    const alpha = (age / WALK_AGE_TICKS) * 0.35; // More subtle fade
    pushQuad(row, col, [0.95, 0.85, 0.4, alpha]); // Muted warm gold
  }
  // Hover (muted cyan, subtle).
  if (hoverTile) {
    pushQuad(hoverTile.row, hoverTile.col, [0.5, 0.8, 0.9, 0.25]);
  }
  // Current standing tile (very subtle white glow).
  if (prevPlayerTile !== null) {
    const { row, col } = tileFromKey(prevPlayerTile);
    pushQuad(row, col, [0.95, 0.95, 0.95, 0.15]); // Barely visible
  }

  // Render scene — lava donut first (never under the main ground), then the
  // dark skirt that seals any tile-seam gaps, then the ground, its glowing
  // edge, tile highlights, feet shadow + body.
  // 🎯 Apply custom cursor based on pen lock state
  if (canvas) {
    if (penLocked) {
      // Determine if we're in click expansion window
      const timeSinceClick = now - cursorClickTime;
      const isExpanded = timeSinceClick < CURSOR_CLICK_DURATION && timeSinceClick >= 0;
      canvas.style.cursor = isExpanded ? cursorExpanded : cursorNormal;
    } else {
      canvas.style.cursor = 'auto';
    }
  }

  wipe(45, 48, 55);
  const lava = buildLavaFloor(now / 1000);
  if (lava) ink(255).form(lava);
  if (platformBlock) ink(255).form(platformBlock);  // bottom and side faces
  if (groundSkirt) ink(255).form(groundSkirt);
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
  // Dropped entirely in LOW perf mode — wireframes are nice-to-have.
  if (!perfLowMode) {
    if (bodyFeet) ink(255).form(bodyFeet);
    if (bodyArms) ink(255).form(bodyArms);
  }

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

    // POV indicator — shows 1P or 3P plus the current zoom distance.
    ink(phys.thirdPerson ? "magenta" : "cyan");
    const povStr = phys.thirdPerson
      ? `3P ×${ZOOM_DISTANCES[zoomLevel]}`
      : "1P";
    rightLabel(povStr, margin + lineH * 5);
  }

  // ⚡ Perf mode label — shows which adaptive-quality tier is active. Flashes
  // briefly after a tier change (perfSamplesSinceSwitch < 6).
  const perfLabel = perfLowMode ? "PERF LOW" : perfMedMode ? "PERF MED" : "PERF HIGH";
  const perfFresh = perfSamplesSinceSwitch < 6;
  ink(perfLowMode ? "red" : perfMedMode ? "orange" : "lime");
  if (perfFresh) ink("white");
  rightLabel(perfLabel, margin + lineH * 6);
  // Show which features are currently disabled by the perf tier.
  ink(150, 150, 150);
  if (perfLowMode) rightLabel("-BODY -ANIM", margin + lineH * 7);
  else if (perfMedMode) rightLabel("-LAVA-ANIM", margin + lineH * 7);

  // Hover-flip experiment — press F to cycle, label shows current mode.
  const flipNames = ["F:NONE", "F:X", "F:Z", "F:XZ"];
  ink(255, 200, 80);
  rightLabel(flipNames[hoverFlipMode], margin + lineH * 8);

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

  // 🎯 Debug crosshair at the current pen position (unlocked mode only) so
  // we can visually compare it against the highlighted hover tile.
  if (pen && !penLocked) {
    const px = Math.floor(pen.x);
    const py = Math.floor(pen.y);
    ink("magenta");
    box(px - 6, py, 13, 1);
    box(px, py - 6, 1, 13);
  }

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

  // 📱 Draw mobile control buttons
  if (mobileButtons) {
    // Check if keyboard key is held for each button
    const keyHeld = {
      up: keyboardState.w || keyboardState.arrowup,
      down: keyboardState.s || keyboardState.arrowdown,
      left: keyboardState.a || keyboardState.arrowleft,
      right: keyboardState.d || keyboardState.arrowright,
      jump: keyboardState.space,
      crouch: keyboardState.shift,
    };

    for (const [name, btnData] of Object.entries(mobileButtons)) {
      const btn = btnData.btn;
      if (!btn) continue;

      // Draw button with pressed state (button click OR keyboard key held)
      const isPressed = btn.down || keyHeld[name];
      const bgColor = isPressed ? [100, 140, 180, 220] : [60, 75, 95, 150];
      const borderColor = isPressed ? [200, 220, 255] : [110, 130, 160];
      const textColor = isPressed ? [255, 255, 255] : [180, 200, 230];

      btn.paint((b) => {
        ink(...bgColor).box(b.box, "fill");
        ink(...borderColor).box(b.box, "outline");
        ink(...textColor).write(btnData.label,
          { x: Math.floor(b.box.x + 4), y: Math.floor(b.box.y + 2) },
          undefined, undefined, false);
      });
    }
  }
}

function act({ event: e, penLock, system }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;

  // ⌨️ Track keyboard state for visual feedback on buttons
  if (e.is("keyboard:down")) {
    if (e.key === "w") keyboardState.w = true;
    if (e.key === "a") keyboardState.a = true;
    if (e.key === "s") keyboardState.s = true;
    if (e.key === "d") keyboardState.d = true;
    if (e.key === "arrowup") keyboardState.arrowup = true;
    if (e.key === "arrowdown") keyboardState.arrowdown = true;
    if (e.key === "arrowleft") keyboardState.arrowleft = true;
    if (e.key === "arrowright") keyboardState.arrowright = true;
    if (e.key === " ") keyboardState.space = true;
    if (e.key === "shift") keyboardState.shift = true;
  } else if (e.is("keyboard:up")) {
    if (e.key === "w") keyboardState.w = false;
    if (e.key === "a") keyboardState.a = false;
    if (e.key === "s") keyboardState.s = false;
    if (e.key === "d") keyboardState.d = false;
    if (e.key === "arrowup") keyboardState.arrowup = false;
    if (e.key === "arrowdown") keyboardState.arrowdown = false;
    if (e.key === "arrowleft") keyboardState.arrowleft = false;
    if (e.key === "arrowright") keyboardState.arrowright = false;
    if (e.key === " ") keyboardState.space = false;
    if (e.key === "shift") keyboardState.shift = false;
  }

  // 📱 Trigger button input handling
  if (mobileButtons) {
    for (const btnData of Object.values(mobileButtons)) {
      btnData.btn?.act?.(e);
    }
  }


  // F cycles the hover axis-flip experiment (0 = no flip, 1 = X, 2 = Z, 3 = both).
  if (e.is("keyboard:down:f")) {
    hoverFlipMode = (hoverFlipMode + 1) & 3;
    console.log("🔄 hoverFlipMode →", hoverFlipMode,
      ["baseline", "flipX", "flipZ", "flipBoth"][hoverFlipMode]);
  }

  // 🎥 Middle-mouse toggles third-person (press once to enter, press again
  // to exit). Only trigger on touch so the release doesn't also flip.
  if (e.device === "mouse" && e.button === 1 && e.is("touch")) {
    zoomLevel = zoomLevel === 0 ? 2 : 0; // flip between 1P and default 3P
    applyZoom(system?.fps?.doll);
  }

  // 🔎 Scroll wheel — steps through discrete zoom levels (1P → closer 3P →
  // further 3P). dir > 0 = zoom in (toward 1P); dir < 0 = zoom out.
  if (e.is("wheel")) {
    if (e.dir > 0) zoomLevel = Math.max(0, zoomLevel - 1);
    else if (e.dir < 0) zoomLevel = Math.min(ZOOM_DISTANCES.length - 1, zoomLevel + 1);
    applyZoom(system?.fps?.doll);
  }

  // 🎥 Right-click drag to orbit camera around player (3P mode only).
  const cam = system?.fps?.doll?.cam;
  if (e.is("touch") && e.button === 2 && cam && zoomLevel > 0) {
    orbiting = true;
    playerFacing = cam.rotY; // lock player facing to current heading immediately
    baseRotY = cam.rotY;
    orbitSnapped = false; // start orbiting fresh
    // Capture current XZ distance to maintain constant orbit radius
    const phys = system?.fps?.doll?.physics;
    const pCamX = phys?.playerCamX ?? cam.x;
    const pCamZ = phys?.playerCamZ ?? cam.z;
    const dx = cam.x - pCamX;
    const dz = cam.z - pCamZ;
    orbitDistance = Math.sqrt(dx * dx + dz * dz);
  } else if (e.is("lift") && (e.button === 2 || orbiting)) {
    // Lift ends orbit (handle both proper e.button===2 and fallback for button detection issues)
    orbiting = false;
    orbitSnapped = true; // mark that orbit was released; wait for left-click to reset
  } else if (e.is("draw") && orbiting && zoomLevel > 0) {
    // Drag: accumulate orbit angle during right-click drag (3P mode only)
    // This handles cases where the touch API doesn't properly set e.button on drag events
    orbitAngle += e.delta.x * 0.4;
  }

  // Left-click resets orbit angle (only if orbit was previously snapped/released)
  // Also cancel any in-progress orbit if left-click happens
  if (e.is("touch") && e.button === 0) {
    if (orbiting) orbiting = false; // force-stop any active orbit on left-click
    if (orbitSnapped) {
      orbitAngle = 0;
      orbitSnapped = false;
    }
    // 🎯 Trigger cursor expansion animation on click (when in FPS mode)
    if (penLocked) {
      cursorClickTime = performance.now();
    }
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
    // Don't re-lock on middle-click (1) or right-click (2) — reserved for camera control.
    if (!penLocked && e.button !== 1 && e.button !== 2) penLock();
  }
}

export const system = "fps";
export { boot, sim, paint, act };
