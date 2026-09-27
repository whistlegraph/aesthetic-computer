// lairk-world.mjs — the shared ground rules of `lairk`, the Laer Klokken place.
// Imported by disks/lairk.mjs (prediction + drawing) and
// session-server/lairk-manager.mjs (authority). Dep-free apart from pmove,
// so it runs in Node and the browser alike.

/* 📝 Notes
  Movement is arena's: Quake-style pmove (lib/pmove.mjs) with arena's tuning,
  on a square platform with a clock tower in the middle.

  Netcode follows oskiewar (oskiewar-world/ARCHITECTURE.md §5): one fixed
  60 Hz step, `lairkStep`, runs on both sides. The client predicts its own
  body from its inputs the frame you press; the server re-runs the same
  inputs by tick and is the only truth; when its answer arrives the client
  snaps to it and replays the inputs the server hasn't seen yet. Other people
  are drawn ~100 ms in the past between snapshots, never guessed ahead.
 */

import { pmove, newState, BTN, DEFAULT_CFG } from "./pmove.mjs";
import { ARENA_PHYSICS } from "./arena-world.mjs";

export { BTN };

export const LAIRK_TICK_HZ = 60;
export const LAIRK_DT = 1 / LAIRK_TICK_HZ;
export const LAIRK_HALF = 24; // The platform spans ±24 on x and z.
export const LAIRK_THICKNESS = 1.6; // How deep the platform's skirt reads.
export const LAIRK_TOWER_HALF = 1.1; // Tower footprint (plinth adds 0.3).
export const LAIRK_TOWER_TOP = 7;
export const LAIRK_FALL_Y = -30; // Fell off the edge: back to your spot.

export const LAIRK_CFG = Object.freeze({
  ...DEFAULT_CFG,
  ...ARENA_PHYSICS,
  simHz: LAIRK_TICK_HZ,
  groundY: 0,
  groundBounds: { xMin: -LAIRK_HALF, xMax: LAIRK_HALF, zMin: -LAIRK_HALF, zMax: LAIRK_HALF },
  obstacles: [
    {
      type: "box",
      xMin: -LAIRK_TOWER_HALF - 0.3, xMax: LAIRK_TOWER_HALF + 0.3,
      zMin: -LAIRK_TOWER_HALF - 0.3, zMax: LAIRK_TOWER_HALF + 0.3,
      yMin: 0, yMax: LAIRK_TOWER_TOP + 2,
    },
  ],
});

// A standing body at a spot (x, z are feet; y is the eye, as in pmove).
export function lairkSpawn(x = 0, z = 6, yaw = 180) {
  return newState({ x, z, yaw, cfg: LAIRK_CFG });
}

// One fixed tick. `cmd` = { fwd, right, yaw, pitch, buttons }. Pure: returns
// a new state. Falling far below the platform puts you back at `home`.
export function lairkStep(state, cmd, home) {
  const s = pmove(state, { ...cmd, dt: LAIRK_DT }, LAIRK_CFG);
  if (s.y < LAIRK_FALL_Y) {
    const back = lairkSpawn(home?.x ?? 0, home?.z ?? 6, s.yaw);
    back.pitch = s.pitch;
    return back;
  }
  return s;
}

// Wire form of one tick's input: { t, f, r, y, p, b }.
export function packInput(tick, cmd) {
  return {
    t: tick | 0,
    f: clampInt(cmd.fwd),
    r: clampInt(cmd.right),
    y: Math.round((cmd.yaw || 0) * 100) / 100,
    p: Math.round((cmd.pitch || 0) * 100) / 100,
    b: (cmd.buttons | 0) & (BTN.JUMP | BTN.CROUCH),
  };
}

export function unpackInput(w) {
  return {
    tick: w?.t | 0,
    fwd: clampInt(w?.f),
    right: clampInt(w?.r),
    yaw: Number.isFinite(+w?.y) ? +w.y : 0,
    pitch: Number.isFinite(+w?.p) ? +w.p : 0,
    buttons: (w?.b | 0) & (BTN.JUMP | BTN.CROUCH),
  };
}

// The part of a state a snapshot carries (and a client adopts).
export const STATE_FIELDS = ["x", "y", "z", "vx", "vy", "vz", "yaw", "pitch", "crouchT", "onGround"];

export function packState(s) {
  const out = {};
  for (const k of STATE_FIELDS) {
    const v = s[k];
    out[k] = typeof v === "number" ? Math.round(v * 1000) / 1000 : v;
  }
  return out;
}

export function unpackState(w) {
  const s = lairkSpawn(0, 0);
  for (const k of STATE_FIELDS) if (w?.[k] !== undefined) s[k] = w[k];
  return s;
}

function clampInt(n) {
  const v = Math.round(+n || 0);
  return v > 1 ? 1 : v < -1 ? -1 : v;
}
