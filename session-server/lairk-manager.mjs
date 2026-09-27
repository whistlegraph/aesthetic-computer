// Lairk Manager, 26.09.23
// Who is walking in `lairk` — the Laer Klokken chat as a place — and where.
//
// Anyone with the piece open is a *watcher*: they get snapshots of everyone.
// Only a *walker* may move, and a walker is a verified handle (Auth0 token →
// handle, never a handle the client names) that the roster gives a spot:
// spoken in Laer Klokken and @mentioned there by someone else
// (/api/lairk-roster).
//
// Netcode is oskiewar's (oskiewar-world/ARCHITECTURE.md §5). Walkers send
// their *inputs* by tick, never positions. The server runs each input through
// the same fixed step the client predicts with (lib/lairk-world.mjs →
// lairkStep, arena's Quake pmove) and is the only truth. Snapshots go out at
// 20 Hz carrying every walker's state and, for the walker receiving it, the
// last tick the server has run — the client snaps to that and replays the
// inputs still in flight. Where each handle last stood is remembered.
//
// Wire (all WS):
//   in   lairk:hello                   watch
//   in   lairk:auth   { token }        ask to walk
//   in   lairk:input  { inputs: [..] } walkers only; each { t, f, r, y, p, b },
//                                      redundant (the last few ticks again)
//   out  lairk:state  { positions }    to a new watcher: { handle: {x,z,facing} }
//   out  lairk:auth:ok { handle, state, home } / lairk:auth:no { reason }
//   out  lairk:snap   { ms, players: [{ h, s }], ack? }
//   out  lairk:still  { handle }       a walker left; they stay where they stood

import {
  lairkSpawn,
  lairkStep,
  unpackInput,
  packState,
  LAIRK_TICK_HZ,
} from "../system/public/aesthetic.computer/lib/lairk-world.mjs";

const ROSTER_MS = 60_000;
const SNAP_MS = 50; // 20 Hz.
const SAVE_MS = 2_000; // How often a moved walker's spot is persisted.
const TICK_SLACK = 30; // Ticks a client may run ahead of wall time (jitter).
const MAX_GAP = 12; // Missing ticks filled by repeating the last input.

export class LairkManager {
  constructor({ verify, fetchRoster, save, now = () => Date.now() } = {}) {
    this.verify = verify; // async (token) => { handle } | null
    this.fetchRoster = fetchRoster; // async () => string[] of handles
    this.save = save; // (handle, pos) => void — persistence, optional
    this.now = now;
    this.watchers = new Set(); // wsIds with lairk open
    this.walkers = new Map(); // wsId -> Walker (see auth)
    this.positions = new Map(); // handle -> { x, z, facing, at }
    this.roster = null; // Set of handles, refreshed every ROSTER_MS
    this.rosterAt = 0;
    this.sendFn = null; // (wsId, type, content)
    this.lastSnapAt = 0;
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

    const spot = this.positions.get(handle);
    const home = spot ? { x: spot.x, z: spot.z } : { x: 0, z: 6 };
    const state = lairkSpawn(home.x, home.z, spot?.facing ?? 180);
    this.watchers.add(wsId);
    this.walkers.set(wsId, {
      handle,
      state,
      home,
      lastTick: 0, // Client ticks start at 1 after auth:ok.
      lastCmd: null,
      since: this.now(),
      ran: 0, // Ticks run, checked against wall time.
      dirty: false,
      savedAt: 0,
    });
    this.send(wsId, "lairk:auth:ok", { handle, state: packState(state), home });
  }

  // Run a walker's inputs, in tick order, through the shared step.
  input(wsId, content) {
    const w = this.walkers.get(wsId);
    if (!w || !Array.isArray(content?.inputs)) return;
    const inputs = content.inputs
      .map(unpackInput)
      .filter((c) => c.tick > w.lastTick)
      .sort((a, b) => a.tick - b.tick);

    for (const cmd of inputs) {
      // A lost packet: repeat the last input across a short gap (Q3-style);
      // a long one is a stall, so just carry on from here.
      const gap = cmd.tick - w.lastTick - 1;
      if (gap > 0 && gap <= MAX_GAP && w.lastCmd) {
        for (let i = 0; i < gap; i++) if (!this.run(w, w.lastCmd)) return;
      }
      if (!this.run(w, cmd)) return;
      w.lastTick = cmd.tick;
      w.lastCmd = cmd;
    }
  }

  // One tick for one walker, unless they're running faster than time allows.
  run(w, cmd) {
    const allowed = ((this.now() - w.since) / 1000) * LAIRK_TICK_HZ + TICK_SLACK;
    if (w.ran >= allowed) return false; // Speedhack or a flood: drop the rest.
    w.ran += 1;
    w.state = lairkStep(w.state, cmd, w.home);
    w.dirty = true;
    return true;
  }

  // Called on a timer (~every 10 ms is fine): sends snapshots at 20 Hz and
  // persists moved walkers every SAVE_MS.
  tick() {
    const now = this.now();
    if (now - this.lastSnapAt < SNAP_MS) return;
    this.lastSnapAt = now;
    if (this.walkers.size === 0 || this.watchers.size === 0) return;

    const players = [];
    const seen = new Set();
    for (const w of this.walkers.values()) {
      if (seen.has(w.handle)) continue; // Two tabs: one body.
      seen.add(w.handle);
      players.push({ h: w.handle, s: packState(w.state) });
      if (w.dirty && now - w.savedAt > SAVE_MS) {
        const pos = { x: round(w.state.x), z: round(w.state.z), facing: round(w.state.yaw), at: now };
        this.positions.set(w.handle, pos);
        this.save?.(w.handle, pos);
        w.savedAt = now;
        w.dirty = false;
      }
    }

    for (const id of this.watchers) {
      const mine = this.walkers.get(id);
      this.send(id, "lairk:snap", mine ? { ms: now, players, ack: mine.lastTick } : { ms: now, players });
    }
  }

  leave(wsId) {
    this.watchers.delete(wsId);
    const w = this.walkers.get(wsId);
    if (!w) return;
    this.walkers.delete(wsId);
    const pos = { x: round(w.state.x), z: round(w.state.z), facing: round(w.state.yaw), at: this.now() };
    this.positions.set(w.handle, pos);
    this.save?.(w.handle, pos);
    // Another tab of the same handle may still be walking.
    if ([...this.walkers.values()].some((o) => o.handle === w.handle)) return;
    for (const id of this.watchers) this.send(id, "lairk:still", { handle: w.handle, ...pos });
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

const round = (n) => Math.round(n * 100) / 100;
