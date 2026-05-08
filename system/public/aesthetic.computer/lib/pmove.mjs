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
  // 🏃 Quake-style physics (opt-in). When `airAccel` is set, the legacy
  // hVelDecay lerp is replaced by Q3 friction+accelerate on the ground and
  // air-accelerate (with a perpendicular wishspeed cap) in the air. This
  // is what makes strafe-jumping / bunny-hopping work: in air, velocity
  // perpendicular to the existing motion vector is added without subtracting
  // current speed, so turning slightly off-axis lets the player accumulate
  // velocity above runSpeed.
  airAccel: null,        // u/s² along wishdir while airborne
  groundAccel: null,     // u/s² along wishdir while on ground
  airCapSpeed: 1.5,      // max wishspeed used during air-accel (Q3 air-control window)
  groundFriction: 6,     // Q3-style friction coefficient
  // Static obstacles: array of { type:"box", xMin, xMax, yMin, yMax, zMin, zMax }
  // or { type:"cylinder", x, z, r, yMin, yMax }. Resolved as horizontal
  // push-out after integration so prediction (client) and authority (server)
  // converge. yMin/yMax bound the vertical span — only collide when the
  // player's feet→eye column overlaps the obstacle's Y range.
  obstacles: null,
  playerRadius: 0.4,     // horizontal radius for obstacle push-out
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

  // --- Jump: edge-trigger BEFORE friction so a bunny-hop landing keeps
  // its horizontal speed (Q3 PMF_TIME_LAND trick). The flag is checked
  // off-frame so holding jump auto-rebounds the next time we touch ground.
  if (!s.frozen && jumping && s.onGround) {
    s.vy = cfg.jumpVelocity;
    s.onGround = false;
  }

  if (typeof cfg.airAccel === "number") {
    // 🏃 Quake-style: ground friction + ground/air accelerate.
    const wishLen = Math.hypot(wx, wz);
    const wishSpeed = wishLen;          // already speed-scaled above
    const wdx = wishLen > 1e-6 ? wx / wishLen : 0;
    const wdz = wishLen > 1e-6 ? wz / wishLen : 0;

    // Friction is applied only on the ground.
    if (s.onGround && !s.frozen) qFriction(s, cfg, dt);

    // Accelerate along wishdir.
    if (wishLen > 1e-6 && !s.frozen) {
      if (s.onGround) {
        const accel = cfg.groundAccel ?? 80;
        qAccel(s, wdx, wdz, wishSpeed, accel, dt);
      } else {
        // Air: cap wishspeed but keep accel — perpendicular input adds
        // velocity without subtracting from forward speed (strafe-jump).
        const cap = cfg.airCapSpeed ?? 1.5;
        const airWish = Math.min(wishSpeed, cap);
        qAccel(s, wdx, wdz, airWish, cfg.airAccel, dt);
      }
    }
  } else {
    // Legacy: lerp toward target velocity. Hard-caps at `speed`.
    const decay = Math.pow(cfg.hVelDecay, dt * cfg.simHz);
    s.vx = s.vx * decay + wx * (1 - decay);
    s.vz = s.vz * decay + wz * (1 - decay);
  }

  s.x += s.vx * dt;
  s.z += s.vz * dt;

  // Static obstacle resolution (horizontal push-out + velocity clip).
  if (cfg.obstacles) resolveObstacles(s, cfg);

  // --- Crouch lerp. ---
  const crouchTarget = (!s.frozen && crouching) ? 1 : 0;
  s.crouchT += (crouchTarget - s.crouchT) * cfg.crouchLerp;
  if (s.crouchT < 0.0005 && crouchTarget === 0) s.crouchT = 0;
  if (s.crouchT > 0.9995 && crouchTarget === 1) s.crouchT = 1;
  const effEye = cfg.eyeHeight + (cfg.crouchEyeHeight - cfg.eyeHeight) * s.crouchT;

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

// --- Quake physics helpers (shared with cam-doll for client prediction). ---

function qFriction(s, cfg, dt) {
  const speed = Math.hypot(s.vx, s.vz);
  if (speed < 1e-4) { s.vx = 0; s.vz = 0; return; }
  const stop = Math.max(speed, cfg.runSpeed * 0.5); // Q3-style pm_stopspeed
  const drop = stop * cfg.groundFriction * dt;
  const k = Math.max(0, speed - drop) / speed;
  s.vx *= k;
  s.vz *= k;
}

function qAccel(s, wdx, wdz, wishSpeed, accel, dt) {
  const cur = s.vx * wdx + s.vz * wdz;
  const addspeed = wishSpeed - cur;
  if (addspeed <= 0) return;
  let mag = accel * dt * wishSpeed;
  if (mag > addspeed) mag = addspeed;
  s.vx += wdx * mag;
  s.vz += wdz * mag;
}

/**
 * Resolve horizontal collisions against a flat list of static obstacles.
 * Mutates `s.x`, `s.z` and the velocity components along contact normals.
 * Box and cylinder obstacles are checked against the player's vertical
 * column [feetY, eyeY]: if their Y span doesn't overlap, they're skipped
 * (so the player can leap above a low wall, fall through a missing roof,
 * etc.). Pure horizontal — no top/bottom collision.
 */
export function resolveObstacles(s, cfg) {
  if (!cfg?.obstacles?.length) return;
  const r = cfg.playerRadius ?? 0.4;
  const eyeY = s.y;
  const feetY = s.y - cfg.eyeHeight;
  for (const o of cfg.obstacles) {
    if (typeof o.yMax === "number" && feetY > o.yMax) continue;
    if (typeof o.yMin === "number" && eyeY  < o.yMin) continue;
    if (o.type === "cylinder") {
      const dx = s.x - o.x;
      const dz = s.z - o.z;
      const d  = Math.hypot(dx, dz);
      const minD = (o.r ?? 1) + r;
      if (d >= minD) continue;
      if (d < 1e-4) {
        // Degenerate centre-overlap: punt along +X.
        s.x += minD; continue;
      }
      const nx = dx / d, nz = dz / d;
      const push = minD - d;
      s.x += nx * push;
      s.z += nz * push;
      const vd = s.vx * nx + s.vz * nz;
      if (vd < 0) { s.vx -= vd * nx; s.vz -= vd * nz; }
    } else if (o.type === "box") {
      const cx = (o.xMin + o.xMax) * 0.5;
      const cz = (o.zMin + o.zMax) * 0.5;
      const hx = (o.xMax - o.xMin) * 0.5 + r;
      const hz = (o.zMax - o.zMin) * 0.5 + r;
      const dx = s.x - cx;
      const dz = s.z - cz;
      const ox = hx - Math.abs(dx);
      const oz = hz - Math.abs(dz);
      if (ox <= 0 || oz <= 0) continue;
      // Push along the axis of smallest penetration.
      if (ox < oz) {
        s.x += dx >= 0 ? ox : -ox;
        if ((dx >= 0 && s.vx < 0) || (dx < 0 && s.vx > 0)) s.vx = 0;
      } else {
        s.z += dz >= 0 ? oz : -oz;
        if ((dz >= 0 && s.vz < 0) || (dz < 0 && s.vz > 0)) s.vz = 0;
      }
    }
  }
}
