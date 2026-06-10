// Arena Manager, 2026.04.20
// Server-authoritative state for arena.mjs. Quake 3-inspired:
//   - fixed 60 Hz sim tick
//   - clients send usercmd packets over UDP (geckos.io)
//   - server broadcasts per-client snapshots at 30 Hz
//   - per-client snapshot ring enables future delta compression (M9)
//   - reliable lifecycle events (join/leave/kill/chat/probe) ride WS
//
// Starts simple: full snapshots (no delta yet), no lag compensation, no
// command backup decode. Those are later milestones — the wire formats
// already carry the fields (messageNum, deltaNum, ackCmdSeq) so turning
// them on is additive.

import { newState, pmove, unpackCmd, DEFAULT_CFG, BTN } from "../system/public/aesthetic.computer/lib/pmove.mjs";
import { ARENA_OBSTACLES, ARENA_PHYSICS } from "../system/public/aesthetic.computer/lib/arena-world.mjs";

const PLAYER_FIELDS = ["h","x","y","z","vx","vy","vz","yaw","pitch","c","g","a"];

const TICK_RATE = 60;             // sim ticks/sec
const SNAP_RATE = 30;             // snapshots/sec (per client)
const SNAP_EVERY = TICK_RATE / SNAP_RATE;
const SNAP_RING = 32;             // per-client snapshot history depth
const POS_HISTORY_MS = 500;       // rolling pos history for lag comp
const STALE_TIMEOUT_MS = 30_000;  // evict players idle this long

// Default arena world config — must match disks/arena.mjs.
export const ARENA_CFG = Object.freeze({
  ...DEFAULT_CFG,
  runSpeed: 10,
  walkSpeed: 5,
  jumpVelocity: 8,
  gravity: 50,
  groundY: -1.5,
  eyeHeight: 2.0,
  crouchEyeHeight: 1.2,
  groundBounds: { xMin: -14, xMax: 14, zMin: -14, zMax: 14 },
  deathFloorY: -30,
  simHz: TICK_RATE,
  // 🏃 Quake-style strafe-jumping + bunny-hop.
  airAccel: ARENA_PHYSICS.airAccel,
  groundAccel: ARENA_PHYSICS.groundAccel,
  airCapSpeed: ARENA_PHYSICS.airCapSpeed,
  groundFriction: ARENA_PHYSICS.groundFriction,
  // 🧱 Static walls + pillars (shared with the client).
  obstacles: ARENA_OBSTACLES,
  playerRadius: ARENA_PHYSICS.playerRadius,
});

// Spawn ring — spread players around the arena.
const SPAWNS = [
  { x:  6, z:  0 }, { x: -6, z:  0 }, { x:  0, z:  6 }, { x:  0, z: -6 },
  { x:  5, z:  5 }, { x: -5, z: -5 }, { x:  5, z: -5 }, { x: -5, z:  5 },
];

export class ArenaManager {
  constructor() {
    this.players = new Map();  // handle -> PlayerRecord
    this.probes = new Map();   // handle -> { wsId } — text-only spectators
    this.tick = 0;
    this.startMs = Date.now();
    this.tickInterval = null;

    // Transport callbacks (set by session.mjs)
    this.sendUDP = null;        // (channelId, event, data) -> bool
    this.sendWS = null;         // (wsId, type, content)
    this.broadcastWS = null;    // (type, content)
    this.resolveUdpForHandle = null; // (handle) -> channelId|null
    this.isLive = null;         // (wsId) -> bool; used to tell reconnect
                                //   (old ws dead) apart from a genuine
                                //   takeover (old ws still alive).
  }

  setSendFunctions({ sendUDP, sendWS, broadcastWS, resolveUdpForHandle, isLive }) {
    this.sendUDP = sendUDP;
    this.sendWS = sendWS;
    this.broadcastWS = broadcastWS;
    this.resolveUdpForHandle = resolveUdpForHandle;
    this.isLive = isLive;
  }

  now() { return Date.now() - this.startMs; }

  // -- Lifecycle (WS-reliable) --

  playerJoin(handle, wsId, opts = {}) {
    if (!handle) return;

    // Text-only spectator / probe: no player body, just receive snaps.
    if (opts.probe) {
      this.probes.set(handle, { wsId });
      this.sendWS?.(wsId, "arena:welcome", {
        you: handle,
        probe: true,
        cfg: ARENA_CFG,
        serverMs: this.now(),
        tick: this.tick,
        roster: [...this.players.keys()],
      });
      console.log(`🏟️  probe joined: ${handle} (${this.probes.size} probes)`);
      this.ensureTick();
      return;
    }

    let rec = this.players.get(handle);
    if (rec) {
      // Re-join with same handle. Two cases:
      //   (a) same wsId  → page reload / reconnect on same socket; just
      //       refresh seq bookkeeping.
      //   (b) different wsId → another tab is joining under the same
      //       handle. Do a *takeover*: tell the old wsId it's been
      //       displaced (so the old tab can flip to spectator UI), then
      //       register the old wsId as a probe so it keeps receiving snaps
      //       and the tab stays alive / watchable.
      if (rec.wsId != null && rec.wsId !== wsId) {
        const oldWsId = rec.wsId;
        // Only treat as a takeover if the old socket is *still* live.
        // Otherwise this is a reconnect (tab reload / transient network
        // drop) and the old connection is already gone — no spectating
        // needed, just refresh bookkeeping silently.
        const oldAlive = this.isLive ? !!this.isLive(oldWsId) : true;
        if (oldAlive) {
          console.log(`🏟️  takeover: ${handle} (old wsId=${oldWsId} → new ${wsId})`);
          this.sendWS?.(oldWsId, "arena:takeover", { handle, by: wsId });
          this.probes.set(`${handle}#spec${oldWsId}`, { wsId: oldWsId });
        } else {
          console.log(`🏟️  reconnect: ${handle} (old wsId=${oldWsId} dead → new ${wsId})`);
        }
      }
      rec.wsId = wsId;
      rec.udpChannelId = this.resolveUdpForHandle?.(handle) ?? null;
      rec.lastCmdSeq = 0;
      rec.lastCmdMs = 0;
      rec.lastAckMessageNum = 0;
      rec.nextMessageNum = 1;
      rec.snapHistory.fill(null);
      rec.lastSeenMs = this.now();
    } else {
      const spawn = SPAWNS[this.players.size % SPAWNS.length];
      rec = {
        handle,
        wsId,
        udpChannelId: this.resolveUdpForHandle?.(handle) ?? null,
        state: newState({ x: spawn.x, z: spawn.z, cfg: ARENA_CFG }),
        lastCmdMs: this.now(),         // for dt computation between cmds
        lastCmdSeq: 0,                 // highest client cmd seq processed
        snapHistory: new Array(SNAP_RING).fill(null),
        nextMessageNum: 1,             // monotonic snap counter for this client
        lastAckMessageNum: 0,          // highest snap the client acked
        posHistory: [],                // [{ ms, x, y, z }] for lag comp
        lastSeenMs: this.now(),        // used for timeout/presence
      };
      this.players.set(handle, rec);
    }

    this.sendWS?.(wsId, "arena:welcome", {
      you: handle,
      probe: false,
      cfg: ARENA_CFG,
      serverMs: this.now(),
      tick: this.tick,
      initialState: rec.state,
      roster: [...this.players.keys()],
    });

    this.broadcastWS?.("arena:join", { handle });
    console.log(`🏟️  joined: ${handle} (${this.players.size} players)`);
    this.ensureTick();
  }

  /**
   * Leave. `onlyIfWsId` (optional) guards the reload race: when a tab
   * reloads quickly, the new ws may hello before the old ws's close
   * handler fires. Without this guard, the close would delete the
   * freshly-rebound player. Pass the wsId that was on the closing socket
   * and we only delete if it's still the active one.
   */
  playerLeave(handle, onlyIfWsId = undefined) {
    if (!handle) return;

    // Cleanup any spectator-probe entries for this handle bound to the
    // closing wsId. (These are tabs that were displaced by a takeover.)
    if (onlyIfWsId !== undefined) {
      for (const key of this.probes.keys()) {
        if (!key.startsWith(`${handle}#spec`)) continue;
        const p = this.probes.get(key);
        if (p?.wsId === onlyIfWsId) this.probes.delete(key);
      }
    }

    if (this.probes.delete(handle)) {
      console.log(`🏟️  probe left: ${handle} (${this.probes.size} probes)`);
      this.maybeStopTick();
      return;
    }
    const rec = this.players.get(handle);
    if (!rec) { this.maybeStopTick(); return; }
    if (onlyIfWsId !== undefined && rec.wsId !== onlyIfWsId) {
      // Stale leave (reload race OR this was a spectator tab for a takeover).
      console.log(`🏟️  stale leave for ${handle} (wsId=${onlyIfWsId} ≠ active ${rec.wsId}) — ignored`);
      return;
    }
    this.players.delete(handle);
    this.broadcastWS?.("arena:leave", { handle });
    console.log(`🏟️  left: ${handle} (${this.players.size} players)`);
    this.maybeStopTick();
  }

  resolveUdpChannel(handle, channelId) {
    const rec = this.players.get(handle);
    if (rec) rec.udpChannelId = channelId;
  }

  // -- Input (UDP, high-frequency) --

  receiveCmd(handle, frame) {
    const rec = this.players.get(handle);
    if (!rec) return;
    if (!rec._firstCmdLogged) {
      console.log(`🏟️  cmd:first handle=${handle} cmds=${(frame.cmds || []).length}`);
      rec._firstCmdLogged = true;
    }
    rec._cmdRxCount = (rec._cmdRxCount || 0) + 1;
    rec.lastSeenMs = this.now();

    // Snap-ack: client tells us which snap they last saw.
    if (typeof frame.ack === "number" && frame.ack > rec.lastAckMessageNum) {
      rec.lastAckMessageNum = frame.ack;
    }

    const cmds = Array.isArray(frame.cmds) ? frame.cmds : [];
    const firstSeq = typeof frame.firstSeq === "number" ? frame.firstSeq : null;

    // Q3-style cmd processing: each cmd in the batch has an implicit seq
    // = firstSeq + index. Skip anything already applied (the cmd backup
    // window means most batches overlap with ones we've already seen).
    for (let i = 0; i < cmds.length; i++) {
      const c = unpackCmd(cmds[i]);
      const seq = firstSeq != null ? firstSeq + i : null;

      // De-dupe: prefer seq when present, fall back to ms monotonicity.
      if (seq != null) {
        if (seq <= rec.lastCmdSeq) continue;
      } else {
        if (c.ms <= rec.lastCmdMs) continue;
      }

      // dt from the previous applied cmd's ms; first cmd gets one tick.
      const dt = rec.lastCmdMs > 0
        ? Math.min((c.ms - rec.lastCmdMs) / 1000, 0.25)
        : 1 / TICK_RATE;
      rec.state = pmove(rec.state, { ...c, dt }, ARENA_CFG);
      rec.lastCmdMs = c.ms;
      if (seq != null && seq > rec.lastCmdSeq) rec.lastCmdSeq = seq;
    }
  }

  // -- Tick loop --

  ensureTick() {
    if (this.tickInterval) return;
    this.tickInterval = setInterval(() => this.serverTick(), 1000 / TICK_RATE);
    console.log(`🏟️  arena tick loop started (${TICK_RATE}Hz, snap ${SNAP_RATE}Hz)`);
  }

  maybeStopTick() {
    if (this.players.size === 0 && this.probes.size === 0 && this.tickInterval) {
      clearInterval(this.tickInterval);
      this.tickInterval = null;
      console.log(`🏟️  arena tick loop stopped (idle)`);
    }
  }

  serverTick() {
    this.tick++;
    const nowMs = this.now();

    // For each player with no fresh input this tick, advance using their
    // last-seen cmd (zero input => decays naturally via pmove's damping).
    // This keeps positions progressing during input starvation without
    // teleporting when input resumes.
    for (const rec of this.players.values()) {
      // No automatic pmove here — we only step on real cmds. This matches
      // Q3: server integrates usercmds as they arrive, not on empty ticks.
      // Append current position to history for lag comp.
      rec.posHistory.push({ ms: nowMs, x: rec.state.x, y: rec.state.y, z: rec.state.z });
      // Trim old history beyond POS_HISTORY_MS.
      const cutoff = nowMs - POS_HISTORY_MS;
      while (rec.posHistory.length && rec.posHistory[0].ms < cutoff) {
        rec.posHistory.shift();
      }
    }

    // Stale sweep: if we haven't seen a hello / cmd / ack from a player in
    // STALE_TIMEOUT_MS, evict. Belt-and-suspenders for cases the ws close
    // handler misses (crashed tabs, NATed mobile backgrounds, etc).
    if (this.tick % TICK_RATE === 0) this.sweepStale(nowMs);

    if (this.tick % SNAP_EVERY === 0) this.broadcastSnapshots();

    // Periodic rate log (every 5s at TICK_RATE=60).
    if (this.tick % (TICK_RATE * 5) === 0 && this.players.size > 0) {
      const rows = [];
      for (const rec of this.players.values()) {
        const cmdRx = rec._cmdRxCount || 0;
        const snapTx = rec._snapTxCount || 0;
        const transport = rec.udpChannelId != null ? "UDP" : "WS";
        rows.push(`${rec.handle}(${transport} rx=${cmdRx} tx=${snapTx})`);
        rec._cmdRxCount = 0;
        rec._snapTxCount = 0;
      }
      console.log(`🏟️  stats[5s] ${rows.join(" ")}`);
    }
  }

  sweepStale(nowMs) {
    for (const [handle, rec] of this.players) {
      if (nowMs - rec.lastSeenMs > STALE_TIMEOUT_MS) {
        console.log(`🏟️  sweep ${handle} (idle ${((nowMs - rec.lastSeenMs) / 1000).toFixed(1)}s)`);
        this.players.delete(handle);
        this.broadcastWS?.("arena:leave", { handle });
      }
    }
    for (const [handle, p] of this.probes) {
      // Probes also expire; lastSeen isn't tracked for them today, so use
      // a simple heuristic: if the ws is gone (we don't know), skip. No-op.
      void handle; void p;
    }
    this.maybeStopTick();
  }

  // -- Snapshots --

  composePlayersBlob() {
    const blob = [];
    for (const rec of this.players.values()) {
      const s = rec.state;
      blob.push({
        h: rec.handle,
        x: round3(s.x), y: round3(s.y), z: round3(s.z),
        vx: round3(s.vx), vy: round3(s.vy), vz: round3(s.vz),
        yaw: round2(s.yaw), pitch: round2(s.pitch),
        c: round3(s.crouchT),
        g: s.onGround ? 1 : 0,
        a: s.alive ? 1 : 0,
      });
    }
    return blob;
  }

  /**
   * Q3-style delta of `current` players vs `base` players (keyed by h).
   * Returns { delta, removed, changedCount } — one "delta entry" per handle:
   *   - first time seen:       __new: {full blob}
   *   - steady state:          only changed fields (h is always present)
   *   - unchanged:             { h } only (bare handle marker)
   * Plus `removed: [h, ...]` for handles that existed in base but are gone.
   * The JSON representation is compact at small player counts; we
   * intentionally skip bit-packing (see plan §5.7 — premature at 8 peers).
   */
  deltaPlayers(current, base) {
    const byHandleBase = new Map();
    for (const p of base) byHandleBase.set(p.h, p);
    const delta = [];
    const seen = new Set();
    let changedCount = 0;
    for (const p of current) {
      seen.add(p.h);
      const bp = byHandleBase.get(p.h);
      if (!bp) { delta.push({ h: p.h, __new: p }); changedCount++; continue; }
      // Compare each field; emit only changed values.
      const d = { h: p.h };
      let any = false;
      for (const k of PLAYER_FIELDS) {
        if (k === "h") continue;
        if (p[k] !== bp[k]) { d[k] = p[k]; any = true; }
      }
      if (any) { delta.push(d); changedCount++; }
      else delta.push({ h: p.h });
    }
    const removed = [];
    for (const [h] of byHandleBase) if (!seen.has(h)) removed.push(h);
    return { delta, removed, changedCount };
  }

  broadcastSnapshots() {
    const serverMs = this.now();
    const players = this.composePlayersBlob();

    // Build one snapshot body per *player* because messageNum is per-client.
    for (const rec of this.players.values()) {
      const messageNum = rec.nextMessageNum++;

      // M9: delta-compress against the last snap the client confirmed
      // receiving (if it's still in our ring — falls off after SNAP_RING).
      let snap;
      const ack = rec.lastAckMessageNum;
      const base = ack > 0 ? rec.snapHistory[ack % SNAP_RING] : null;
      if (base && base.messageNum === ack) {
        const { delta, removed } = this.deltaPlayers(players, base.players);
        snap = {
          messageNum,
          deltaNum: ack,
          tick: this.tick,
          serverMs,
          ackCmdSeq: rec.lastCmdSeq,
          ackCmdMs: rec.lastCmdMs,
          you: rec.handle,
          delta,
          ...(removed.length ? { removed } : {}),
        };
      } else {
        snap = {
          messageNum,
          deltaNum: 0, // full snap
          tick: this.tick,
          serverMs,
          ackCmdSeq: rec.lastCmdSeq,
          ackCmdMs: rec.lastCmdMs,
          you: rec.handle,
          players,
        };
      }
      // Write to ring for future delta base lookup.
      rec.snapHistory[messageNum % SNAP_RING] = { messageNum, serverMs, players };

      // Prefer UDP, fall back to WS.
      let ok = false;
      if (rec.udpChannelId != null && this.sendUDP) {
        ok = this.sendUDP(rec.udpChannelId, "arena:snap", snap);
      }
      if (!ok && rec.wsId != null && this.sendWS) {
        this.sendWS(rec.wsId, "arena:snap", snap);
      }
      if (ok || rec.wsId != null) rec._snapTxCount = (rec._snapTxCount || 0) + 1;
    }

    // Probes: always WS, full snap, messageNum=0 (they don't ack).
    for (const [handle, p] of this.probes) {
      if (p.wsId == null || !this.sendWS) continue;
      this.sendWS(p.wsId, "arena:snap", {
        messageNum: 0,
        deltaNum: 0,
        tick: this.tick,
        serverMs,
        ackCmdSeq: 0,
        you: handle,
        players,
        probe: true,
      });
    }
  }

  // -- Probe-specific --

  handlePing(handle, ts, wsId) {
    this.sendWS?.(wsId, "arena:pong", { ts, serverMs: this.now() });
  }
}

function round2(n) { return Math.round(n * 100) / 100; }
function round3(n) { return Math.round(n * 1000) / 1000; }
