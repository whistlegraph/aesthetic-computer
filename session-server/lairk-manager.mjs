// Lairk Manager, 26.09.23
// Who is walking in `lairk` — the Laer Klokken chat as a place — and where.
//
// Anyone with the piece open is a *watcher*: they get every position.
// Only a *walker* may move, and a walker is a verified handle (Auth0 token →
// handle, never a handle the client names) that the roster gives a spot:
// spoken in Laer Klokken and @mentioned there by someone else
// (/api/lairk-roster). Positions are client-reported, so the server only
// keeps them honest — inside the ground, no faster than a run — and relays
// them to watchers. The last position of each handle is remembered, which
// is what "a handle's position within lairk" is.
//
// Wire (all WS):
//   in   lairk:hello                 watch
//   in   lairk:auth  { token }       ask to walk
//   in   lairk:move  { x, z, facing } walkers only, ~10 Hz while moving
//   out  lairk:state { positions }   to a new watcher: { handle: {x,z,facing} }
//   out  lairk:auth:ok { handle, at } / lairk:auth:no { reason }
//   out  lairk:pos   { handle, x, z, facing }
//   out  lairk:still { handle }      a walker left; they stay where they stood

export const LAIRK_BOUNDS = 26; // Ground radius the server allows, world units.
export const LAIRK_TOWER = 1.6; // Nobody stands inside the tower.
export const LAIRK_SPEED = 9; // Fastest allowed, units per second (with slack).
const ROSTER_MS = 60_000;

export class LairkManager {
  constructor({ verify, fetchRoster, save, now = () => Date.now() } = {}) {
    this.verify = verify; // async (token) => { handle } | null
    this.fetchRoster = fetchRoster; // async () => string[] of handles
    this.save = save; // (handle, pos) => void — persistence, optional
    this.now = now;
    this.watchers = new Set(); // wsIds with lairk open
    this.walkers = new Map(); // wsId -> handle
    this.positions = new Map(); // handle -> { x, z, facing, at }
    this.roster = null; // Set of handles, refreshed every ROSTER_MS
    this.rosterAt = 0;
    this.sendFn = null; // (wsId, type, content)
  }

  setSendFunction(fn) {
    this.sendFn = fn;
  }

  // Remembered positions, e.g. loaded from Redis at startup.
  restore(entries) {
    for (const [handle, pos] of entries) {
      if (Number.isFinite(pos?.x) && Number.isFinite(pos?.z)) {
        this.positions.set(handle, { x: pos.x, z: pos.z, facing: pos.facing || 0, at: pos.at || 0 });
      }
    }
  }

  hello(wsId) {
    this.watchers.add(wsId);
    const positions = {};
    for (const [handle, p] of this.positions) positions[handle] = { x: p.x, z: p.z, facing: p.facing };
    this.send(wsId, "lairk:state", { positions });
  }

  async auth(wsId, token) {
    const identity = await this.verify(token);
    if (!identity?.handle) return this.send(wsId, "lairk:auth:no", { reason: "login" });
    const handle = identity.handle.replace(/^@/, "").toLowerCase();
    const roster = await this.currentRoster();
    if (!roster) return this.send(wsId, "lairk:auth:no", { reason: "unavailable" });
    if (!roster.has(handle)) return this.send(wsId, "lairk:auth:no", { reason: "mention" });
    this.watchers.add(wsId);
    this.walkers.set(wsId, handle);
    const at = this.positions.get(handle) || null;
    this.send(wsId, "lairk:auth:ok", { handle, at: at && { x: at.x, z: at.z, facing: at.facing } });
  }

  move(wsId, content) {
    const handle = this.walkers.get(wsId);
    if (!handle) return; // Watchers can't move; the client already told them why.
    let x = Number(content?.x);
    let z = Number(content?.z);
    const facing = Number(content?.facing) || 0;
    if (!Number.isFinite(x) || !Number.isFinite(z)) return;

    const now = this.now();
    const last = this.positions.get(handle);
    if (last?.at) {
      // No faster than a run: clamp the step to what the elapsed time allows.
      const dt = Math.min(1, Math.max(0.05, (now - last.at) / 1000));
      const reach = LAIRK_SPEED * dt * 1.5;
      const dx = x - last.x;
      const dz = z - last.z;
      const d = Math.hypot(dx, dz);
      if (d > reach) {
        x = last.x + (dx / d) * reach;
        z = last.z + (dz / d) * reach;
      }
    }
    ({ x, z } = keepOnGround(x, z));

    const pos = { x: round(x), z: round(z), facing: round(facing), at: now };
    this.positions.set(handle, pos);
    this.save?.(handle, pos);
    for (const id of this.watchers) {
      if (id !== wsId) this.send(id, "lairk:pos", { handle, x: pos.x, z: pos.z, facing: pos.facing });
    }
  }

  leave(wsId) {
    this.watchers.delete(wsId);
    const handle = this.walkers.get(wsId);
    if (!handle) return;
    this.walkers.delete(wsId);
    // Another tab of the same handle may still be walking.
    if ([...this.walkers.values()].includes(handle)) return;
    for (const id of this.watchers) this.send(id, "lairk:still", { handle });
  }

  async currentRoster() {
    if (this.roster && this.now() - this.rosterAt < ROSTER_MS) return this.roster;
    try {
      const handles = await this.fetchRoster();
      if (Array.isArray(handles)) {
        this.roster = new Set(handles.map((h) => String(h).toLowerCase()));
        this.rosterAt = this.now();
      }
    } catch {}
    return this.roster; // A stale roster beats none when the API blips.
  }

  send(wsId, type, content) {
    this.sendFn?.(wsId, type, content);
  }
}

// Inside the ground disc and outside the tower's footprint.
export function keepOnGround(x, z) {
  const d = Math.hypot(x, z);
  if (d > LAIRK_BOUNDS) return { x: (x / d) * LAIRK_BOUNDS, z: (z / d) * LAIRK_BOUNDS };
  if (Math.abs(x) < LAIRK_TOWER && Math.abs(z) < LAIRK_TOWER) {
    // Push out along the nearer axis.
    if (Math.abs(x) > Math.abs(z)) return { x: Math.sign(x || 1) * LAIRK_TOWER, z };
    return { x, z: Math.sign(z || 1) * LAIRK_TOWER };
  }
  return { x, z };
}

const round = (n) => Math.round(n * 100) / 100;
