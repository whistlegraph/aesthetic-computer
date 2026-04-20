// pmove.mjs — pure player-movement function, shared by client prediction
// and server authority. Node- and browser-compatible. No deps.
//
// The math mirrors lib/cam-doll.mjs's physics pass so a client running
// cam-doll locally and the server running pmove on authoritative state
// converge within float rounding.
//
// Coordinate convention: WORLD coordinates (not cam.* which is negated).
//   +X right, +Y up, +Z forward (player faces along yaw).
//
// All mutation is functional: pmove returns a *new* state object; the
// caller decides when to commit. Keep this file dep-free — importable
// from both @ac.mjs in the browser and from session-server in Node.

export const DEFAULT_CFG = Object.freeze({
  runSpeed: 10,          // units/sec
  walkSpeed: 5,          // units/sec while crouched
  jumpVelocity: 8,       // initial upward velocity on jump (u/s)
  gravity: 50,           // u/s²
  groundY: 0,            // world Y of the solid ground plane
  eyeHeight: 2.0,        // stand eye height above groundY
  crouchEyeHeight: 1.2,  // crouched eye height
  crouchLerp: 0.25,      // per-tick lerp toward crouch target
  groundBounds: null,    // { xMin, xMax, zMin, zMax } or null
  deathFloorY: null,     // world Y clamp for frozen players (lava pit)
  deathFloorClearance: 0.3,
  simHz: 120,
  // Dolly-style horizontal damping. cam-doll uses a 0.9 decay + push that
  // settles at `speed` units/sec. We fold that into a direct integration
  // here so the server doesn't need the Dolly object: hVelDecay per tick
  // and a push matching the same steady state.
  hVelDecay: 0.9,
});

// Buttons bitmask (matches usercmd wire format).
export const BTN = Object.freeze({
  JUMP: 1 << 0,
  CROUCH: 1 << 1,
  SHOOT: 1 << 2,
  DASH: 1 << 3,
});

/**
 * Build a fresh neutral player state at the given spawn.
 */
export function newState({ x = 0, z = 0, yaw = 0, pitch = 0, cfg = DEFAULT_CFG } = {}) {
  return {
    x,
    y: cfg.groundY + cfg.eyeHeight, // eye position in world Y
    z,
    vx: 0,
    vz: 0,
    vy: 0,         // world-up velocity
    yaw,           // degrees
    pitch,         // degrees, clamped ±89
    crouchT: 0,
    onGround: true,
    frozen: false,
    alive: true,
  };
}

/**
 * Apply one usercmd to a state. Pure: returns new state object.
 *
 *   state : player state (see newState)
 *   cmd   : {
 *     fwd:   -1 | 0 | 1   // forward/back intent
 *     right: -1 | 0 | 1   // strafe intent
 *     yaw, pitch          // camera angles (degrees)
 *     buttons: bitmask    // BTN.JUMP etc.
 *     dt: seconds         // elapsed wall time since last cmd
 *   }
 *   cfg   : movement tuning (see DEFAULT_CFG)
 */
export function pmove(state, cmd, cfg = DEFAULT_CFG) {
  const s = { ...state };
  const dt = clamp(cmd.dt ?? 1 / cfg.simHz, 0, 0.25); // cap dt so a pause doesn't teleport

  // --- Look: accept cmd-provided yaw/pitch verbatim, but clamp pitch. ---
  if (typeof cmd.yaw === "number") s.yaw = cmd.yaw;
  if (typeof cmd.pitch === "number") s.pitch = clamp(cmd.pitch, -89, 89);

  const crouching = (cmd.buttons & BTN.CROUCH) !== 0;
  const jumping   = (cmd.buttons & BTN.JUMP)   !== 0;

  // --- Horizontal movement: rotate (right, fwd) by yaw, integrate. ---
  const speed = crouching ? cfg.walkSpeed : cfg.runSpeed;

  // Normalise input vector so diagonals aren't faster.
  let ix = cmd.right || 0;
  let iz = cmd.fwd   || 0;
  const ilen = Math.hypot(ix, iz);
  if (ilen > 1) { ix /= ilen; iz /= ilen; }

  // Rotate input into world space by yaw.
  //   yaw 0 → facing +Z; +yaw rotates clockwise looking down.
  const yr = s.yaw * Math.PI / 180;
  const sy = Math.sin(yr), cy = Math.cos(yr);
  // Desired world velocity this frame from input:
  const wx = (ix * cy + iz * sy) * speed;
  const wz = (-ix * sy + iz * cy) * speed;

  // cam-doll uses Dolly's decay + push which converges to `speed` at full
  // stick. We approximate the same feel with a per-tick lerp toward the
  // input velocity.
  const decay = Math.pow(cfg.hVelDecay, dt * cfg.simHz);
  s.vx = s.vx * decay + wx * (1 - decay);
  s.vz = s.vz * decay + wz * (1 - decay);

  s.x += s.vx * dt;
  s.z += s.vz * dt;

  // --- Crouch lerp. ---
  const crouchTarget = (!s.frozen && crouching) ? 1 : 0;
  s.crouchT += (crouchTarget - s.crouchT) * cfg.crouchLerp;
  if (s.crouchT < 0.0005 && crouchTarget === 0) s.crouchT = 0;
  if (s.crouchT > 0.9995 && crouchTarget === 1) s.crouchT = 1;
  const effEye = cfg.eyeHeight + (cfg.crouchEyeHeight - cfg.eyeHeight) * s.crouchT;

  // --- Jump: initiate on edge, only when grounded and not frozen. ---
  if (!s.frozen && jumping && s.onGround) {
    s.vy = cfg.jumpVelocity;
    s.onGround = false;
  }

  // --- Vertical integration: always run gravity; frozen players still fall. ---
  if (!s.onGround || s.frozen) {
    s.vy -= cfg.gravity * dt;
    s.y += s.vy * dt;
  }

  // --- Ground clamp: only when over the solid ground rectangle. ---
  let onSolid = true;
  if (cfg.groundBounds) {
    const b = cfg.groundBounds;
    onSolid = s.x >= b.xMin && s.x <= b.xMax && s.z >= b.zMin && s.z <= b.zMax;
  }

  const floorY = cfg.groundY + effEye;
  if (onSolid && !s.frozen) {
    if (s.y <= floorY) {
      s.y = floorY;
      if (s.vy < 0) s.vy = 0;
      s.onGround = true;
    } else if (s.onGround) {
      // Crouch release bumped eye above floor — stick to it; no fall.
      s.y = floorY;
    }
  } else {
    s.onGround = false;
  }

  // --- Death floor clamp (lava pit). ---
  if (s.frozen && cfg.deathFloorY !== null && cfg.deathFloorY !== undefined) {
    const lavaY = cfg.deathFloorY + cfg.deathFloorClearance;
    if (s.y <= lavaY) {
      s.y = lavaY;
      s.vy = 0;
    }
  }

  return s;
}

/**
 * Encode a usercmd to a compact JSON object for the wire.
 * (Bit-packing is an optional later optimization — see plan §5.3.)
 */
export function packCmd(cmd) {
  return {
    ms: cmd.ms | 0,
    f: cmd.fwd | 0,
    r: cmd.right | 0,
    y: round2(cmd.yaw),
    p: round2(cmd.pitch),
    b: cmd.buttons | 0,
  };
}

export function unpackCmd(w) {
  return {
    ms: w.ms | 0,
    fwd: w.f | 0,
    right: w.r | 0,
    yaw: +w.y || 0,
    pitch: +w.p || 0,
    buttons: w.b | 0,
    // dt is derived by the consumer: (this.ms - prevCmd.ms) / 1000.
  };
}

function clamp(v, lo, hi) { return v < lo ? lo : v > hi ? hi : v; }
function round2(n) { return Math.round(n * 100) / 100; }
