// Duel Manager, 2026.03.30
// Server-authoritative game logic for dumduel.
// Quake 3-style: server owns state, clients send inputs, server broadcasts snapshots.

const ARENA_W = 220;
const ARENA_H = 220;
const BULLET_SPEED = 0.7;
const MOVE_SPEED = 1.0;
const HIT_R = 7;
const COUNTDOWN_TICKS = 180; // 3s at 60Hz
const ROUND_OVER_TICKS = 120; // 2s
const TICK_RATE = 60; // server sim Hz
const SNAPSHOT_INTERVAL = 3; // send snapshot every N ticks (~20Hz)
const BULLET_MAX_AGE = 200;
const DUMMY_HANDLE = "dummy";

function norm(dx, dy) {
  const len = Math.sqrt(dx * dx + dy * dy);
  if (len < 0.001) return { nx: 0, ny: 0 };
  return { nx: dx / len, ny: dy / len };
}

export class DuelManager {
  constructor() {
    this.players = new Map(); // handle -> PlayerRecord
    this.roster = []; // handles in queue order
    this.phase = "waiting";
    this.tick = 0;
    this.countdownTimer = 0;
    this.roundOverTimer = 0;
    this.roundWinner = null;
    this.bullets = [];
    this.tickInterval = null;

    // Send function callbacks (set by session.mjs)
    this.sendUDP = null; // (channelId, event, data)
    this.sendWS = null; // (wsId, type, content)
    this.broadcastWS = null; // (type, content)
    this.resolveUdpForHandle = null; // (handle) -> channelId|null
  }

  // Called by session.mjs to wire up transport
  setSendFunctions({ sendUDP, sendWS, broadcastWS, resolveUdpForHandle }) {
    this.sendUDP = sendUDP;
    this.sendWS = sendWS;
    this.broadcastWS = broadcastWS;
    this.resolveUdpForHandle = resolveUdpForHandle;
  }

  // -- Player Management --

  playerJoin(handle, wsId) {
    if (!handle) return;
    // Only allow handled users (not guest_XXXX)
    if (handle.startsWith("guest_")) return;

    // Update existing or create new
    let player = this.players.get(handle);
    if (player) {
      player.wsId = wsId;
    } else {
      player = {
        handle,
        wsId,
        udpChannelId: null,
        x: 0, y: 0,
        targetX: 0, targetY: 0,
        alive: true,
        wasMoving: false,
        lastInputSeq: 0,
        ping: 0,
        pingTs: 0,
      };
      this.players.set(handle, player);
    }

    // Add to roster if not already present
    if (!this.roster.includes(handle)) {
      this.roster.push(handle);
    }

    // Try to resolve UDP channel
    this.tryResolveUdp(handle);

    // Send current state to joiner
    this.sendWS?.(wsId, "duel:joined", {
      roster: this.roster,
      phase: this.phase,
    });

    // Broadcast updated roster to all
    this.broadcastWS?.("duel:roster", { roster: this.roster, phase: this.phase });

    console.log(`🎯 Duel: ${handle} joined. Roster: [${this.roster.join(", ")}]`);

    // Start game if we have enough players
    this.checkStart();
  }

  playerLeave(handle) {
    if (!handle) return;
    const wasInRoster = this.roster.includes(handle);
    const wasDueling = this.isDuelist(handle);

    this.roster = this.roster.filter((h) => h !== handle);
    this.players.delete(handle);

    // Remove dummy if it was paired with the leaving player
    if (this.roster.includes(DUMMY_HANDLE) && this.roster.length <= 1) {
      this.roster = this.roster.filter((h) => h !== DUMMY_HANDLE);
      this.players.delete(DUMMY_HANDLE);
    }

    if (wasDueling && (this.phase === "fight" || this.phase === "countdown")) {
      // Opponent wins by default
      const remaining = this.getDuelists().find((h) => h !== handle);
      if (remaining && remaining !== DUMMY_HANDLE) {
        this.endRound(remaining);
      } else {
        this.resetToWaiting();
      }
    }

    if (wasInRoster) {
      this.broadcastWS?.("duel:roster", { roster: this.roster, phase: this.phase });
      console.log(`🎯 Duel: ${handle} left. Roster: [${this.roster.join(", ")}]`);
    }

    this.checkStart();

    // Stop tick if nobody left
    if (this.roster.filter((h) => h !== DUMMY_HANDLE).length === 0) {
      this.stopTick();
    }
  }

  resolveUdpChannel(handle, channelId) {
    const player = this.players.get(handle);
    if (player) {
      player.udpChannelId = channelId;
    }
  }

  tryResolveUdp(handle) {
    if (!this.resolveUdpForHandle) return;
    const channelId = this.resolveUdpForHandle(handle);
    if (channelId) {
      const player = this.players.get(handle);
      if (player) player.udpChannelId = channelId;
    }
  }

  // -- Input Processing --

  receiveInput(handle, input) {
    const player = this.players.get(handle);
    if (!player || !player.alive) return;
    if (!this.isDuelist(handle)) return;
    if (this.phase !== "fight" && this.phase !== "countdown") return;

    player.targetX = Math.max(6, Math.min(ARENA_W - 6, input.targetX));
    player.targetY = Math.max(6, Math.min(ARENA_H - 6, input.targetY));
    if (input.seq > player.lastInputSeq) {
      player.lastInputSeq = input.seq;
    }
    // Log first few inputs
    if (input.seq <= 3) {
      console.log(`🎯 Input from ${handle}: seq=${input.seq} target=(${input.targetX.toFixed(1)}, ${input.targetY.toFixed(1)})`);
    }
  }

  handlePing(handle, ts, wsId) {
    const player = this.players.get(handle);
    if (player) {
      player.ping = Date.now() - ts;
    }
    this.sendWS?.(wsId, "duel:pong", { ts, serverTime: Date.now() });
  }

  // -- Game Logic --

  getDuelists() {
    if (this.roster.length < 2) return [];
    return [this.roster[0], this.roster[1]];
  }

  isDuelist(handle) {
    const d = this.getDuelists();
    return d.includes(handle);
  }

  checkStart() {
    const realPlayers = this.roster.filter((h) => h !== DUMMY_HANDLE);

    if (realPlayers.length === 0) {
      this.resetToWaiting();
      this.stopTick();
      return;
    }

    if (realPlayers.length === 1) {
      // Solo — start practice with dummy (regardless of current phase)
      // Remove dummy first if stale
      if (this.roster.includes(DUMMY_HANDLE)) {
        this.roster = this.roster.filter((h) => h !== DUMMY_HANDLE);
        this.players.delete(DUMMY_HANDLE);
      }
      this.bullets = [];
      this.phase = "waiting"; // reset phase so startPractice works
      this.startPractice(realPlayers[0]);
      return;
    }

    // Remove dummy if real opponent available
    if (realPlayers.length >= 2 && this.roster.includes(DUMMY_HANDLE)) {
      this.roster = this.roster.filter((h) => h !== DUMMY_HANDLE);
      this.players.delete(DUMMY_HANDLE);
      this.bullets = [];
    }

    if (this.roster.length >= 2 && (this.phase === "waiting" || this.phase === "roundover")) {
      this.startCountdown();
    }
  }

  startPractice(handle) {
    // Add dummy
    if (!this.roster.includes(DUMMY_HANDLE)) {
      this.roster.push(DUMMY_HANDLE);
      this.players.set(DUMMY_HANDLE, {
        handle: DUMMY_HANDLE,
        wsId: null,
        udpChannelId: null,
        x: ARENA_W - 30, y: ARENA_H - 30,
        targetX: ARENA_W - 30, targetY: ARENA_H - 30,
        alive: true,
        wasMoving: false,
        lastInputSeq: 0,
        ping: 0,
        pingTs: 0,
      });
    }

    // Ensure handle is first in roster
    this.roster = this.roster.filter((h) => h !== handle && h !== DUMMY_HANDLE);
    this.roster.unshift(handle);
    this.roster.push(DUMMY_HANDLE);

    this.startCountdown();
  }

  startCountdown() {
    this.phase = "countdown";
    this.countdownTimer = COUNTDOWN_TICKS;
    this.bullets = [];
    this.roundWinner = null;

    const duelists = this.getDuelists();
    // Deterministic slots: alphabetical order
    const sorted = [...duelists].sort();
    const spawnA = { x: 30, y: 30 };
    const spawnB = { x: ARENA_W - 30, y: ARENA_H - 30 };

    for (const h of duelists) {
      const p = this.players.get(h);
      if (!p) continue;
      const spawn = h === sorted[0] ? spawnA : spawnB;
      p.x = spawn.x; p.y = spawn.y;
      p.targetX = spawn.x; p.targetY = spawn.y;
      p.alive = true;
      p.wasMoving = false;
    }

    this.broadcastWS?.("duel:countdown", {
      duelists,
      timer: this.countdownTimer,
    });

    console.log(`🎯 Duel countdown: ${duelists.join(" vs ")} (phase: ${this.phase})`);
    this.ensureTick();
  }

  startFight() {
    this.phase = "fight";
    this.broadcastWS?.("duel:fight", {});
    console.log(`🎯 Duel fight started! Tick loop active.`);
  }

  endRound(winnerHandle) {
    this.roundWinner = winnerHandle;
    this.phase = "roundover";
    this.roundOverTimer = ROUND_OVER_TICKS;

    const duelists = this.getDuelists();
    const loser = duelists.find((h) => h !== winnerHandle) || "???";

    // Mark loser dead
    const loserPlayer = this.players.get(loser);
    if (loserPlayer) loserPlayer.alive = false;

    this.broadcastWS?.("duel:death", { victim: loser, killer: winnerHandle });
    this.broadcastWS?.("duel:roundover", { winner: winnerHandle, loser });

    console.log(`🎯 Duel round: ${winnerHandle} killed ${loser}`);
  }

  advanceStack() {
    if (this.roster.length >= 2 && this.roundWinner) {
      // Loser goes to bottom
      const duelists = this.getDuelists();
      const loserHandle = duelists.find((h) => h !== this.roundWinner);
      if (loserHandle) {
        this.roster = this.roster.filter((h) => h !== loserHandle);
        this.roster.push(loserHandle);
      }
    }

    this.roundWinner = null;
    this.bullets = [];

    this.broadcastWS?.("duel:advance", { roster: this.roster });

    // Check what to do next
    const realPlayers = this.roster.filter((h) => h !== DUMMY_HANDLE);
    if (realPlayers.length >= 2) {
      this.startCountdown();
    } else if (realPlayers.length === 1) {
      // Remove dummy, restart practice
      this.roster = this.roster.filter((h) => h !== DUMMY_HANDLE);
      this.players.delete(DUMMY_HANDLE);
      this.phase = "waiting";
      this.startPractice(realPlayers[0]);
    } else {
      this.resetToWaiting();
    }
  }

  resetToWaiting() {
    this.phase = "waiting";
    this.bullets = [];
    this.roundWinner = null;
    this.countdownTimer = 0;
    this.roundOverTimer = 0;
  }

  // -- Server Tick --

  // Purge stale guest handles from roster
  purgeGuests() {
    const guests = this.roster.filter((h) => h.startsWith("guest_"));
    for (const g of guests) {
      this.roster = this.roster.filter((h) => h !== g);
      this.players.delete(g);
      console.log(`🎯 Purged stale guest: ${g}`);
    }
  }

  ensureTick() {
    if (!this.tickInterval) {
      this.purgeGuests(); // Clean up any stale guests before starting
      this.tickInterval = setInterval(() => this.serverTick(), 1000 / TICK_RATE);
      console.log(`🎯 Duel tick loop started (${TICK_RATE}Hz, snapshot every ${SNAPSHOT_INTERVAL} ticks)`);
    }
  }

  stopTick() {
    if (this.tickInterval) {
      clearInterval(this.tickInterval);
      this.tickInterval = null;
      console.log(`🎯 Duel tick loop stopped`);
    }
    this.resetToWaiting();
  }

  serverTick() {
    this.tick++;

    if (this.phase === "countdown") {
      this.countdownTimer--;
      this.tickDummy();
      this.tickMovement();
      if (this.countdownTimer <= 0) this.startFight();
    }

    if (this.phase === "fight") {
      this.tickDummy();
      this.tickMovement();
      this.tickFireOnStop();
      this.tickBullets();
      this.tickHitDetection();
    }

    if (this.phase === "roundover") {
      this.roundOverTimer--;
      if (this.roundOverTimer <= 0) this.advanceStack();
    }

    // Broadcast snapshot at reduced rate
    if (this.tick % SNAPSHOT_INTERVAL === 0) {
      this.broadcastSnapshot();
    }
  }

  tickDummy() {
    const dummy = this.players.get(DUMMY_HANDLE);
    if (!dummy || !dummy.alive) return;

    // Wander every ~90 ticks
    if (this.tick % 90 === 0) {
      dummy.targetX = 20 + Math.random() * (ARENA_W - 40);
      dummy.targetY = 20 + Math.random() * (ARENA_H - 40);
    }
  }

  tickMovement() {
    const duelists = this.getDuelists();
    for (const h of duelists) {
      const p = this.players.get(h);
      if (!p || !p.alive) continue;

      const dx = p.targetX - p.x;
      const dy = p.targetY - p.y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist > 2) {
        const speed = h === DUMMY_HANDLE ? MOVE_SPEED * 0.7 : MOVE_SPEED;
        p.x += (dx / dist) * speed;
        p.y += (dy / dist) * speed;
      }
    }
  }

  tickFireOnStop() {
    const duelists = this.getDuelists();
    for (const h of duelists) {
      if (h === DUMMY_HANDLE) continue; // dummy doesn't fire
      const p = this.players.get(h);
      if (!p || !p.alive) continue;

      const dx = p.targetX - p.x;
      const dy = p.targetY - p.y;
      const isMoving = dx * dx + dy * dy > 4;

      // Fire when transitioning from moving to stopped
      if (p.wasMoving && !isMoving) {
        // Check no bullet already in flight
        const hasBullet = this.bullets.some((b) => b.ownerHandle === h);
        if (!hasBullet) {
          // Find opponent
          const opHandle = duelists.find((d) => d !== h);
          const op = opHandle ? this.players.get(opHandle) : null;
          if (op && op.alive) {
            const { nx, ny } = norm(op.x - p.x, op.y - p.y);
            this.bullets.push({
              x: p.x + nx * 6,
              y: p.y + ny * 6,
              vx: nx * BULLET_SPEED,
              vy: ny * BULLET_SPEED,
              ownerHandle: h,
              age: 0,
            });
            console.log(`🎯 ${h} fired! bullets=${this.bullets.length}`);
            // Immediately broadcast so client sees the bullet ASAP
            // (don't wait for next SNAPSHOT_INTERVAL tick)
            this.broadcastSnapshot();
          }
        }
      }

      // Update wasMoving AFTER the fire check
      p.wasMoving = isMoving;
    }
  }

  tickBullets() {
    for (let i = this.bullets.length - 1; i >= 0; i--) {
      const b = this.bullets[i];
      b.x += b.vx;
      b.y += b.vy;
      b.age++;

      // Remove if off-arena or too old
      if (
        b.age > BULLET_MAX_AGE ||
        b.x < -10 || b.x > ARENA_W + 10 ||
        b.y < -10 || b.y > ARENA_H + 10
      ) {
        this.bullets.splice(i, 1);
      }
    }
  }

  tickHitDetection() {
    const duelists = this.getDuelists();
    for (let i = this.bullets.length - 1; i >= 0; i--) {
      const b = this.bullets[i];
      // Check against non-owner duelist
      for (const h of duelists) {
        if (h === b.ownerHandle) continue;
        const p = this.players.get(h);
        if (!p || !p.alive) continue;

        const dx = b.x - p.x;
        const dy = b.y - p.y;
        if (dx * dx + dy * dy < HIT_R * HIT_R) {
          // Hit! Server-authoritative kill
          this.bullets.splice(i, 1);
          this.endRound(b.ownerHandle);
          return; // only one kill per tick
        }
      }
    }
  }

  // -- Snapshot Broadcasting --

  broadcastSnapshot() {
    const duelists = this.getDuelists();
    const playersData = duelists.map((h) => {
      const p = this.players.get(h);
      if (!p) return null;
      return {
        handle: h,
        x: Math.round(p.x * 10) / 10,
        y: Math.round(p.y * 10) / 10,
        targetX: Math.round(p.targetX * 10) / 10,
        targetY: Math.round(p.targetY * 10) / 10,
        alive: p.alive,
        ping: p.ping,
      };
    }).filter(Boolean);

    const bulletsData = this.bullets.map((b) => ({
      x: Math.round(b.x * 10) / 10,
      y: Math.round(b.y * 10) / 10,
      vx: b.vx,
      vy: b.vy,
      owner: b.ownerHandle,
      age: b.age,
    }));

    const lastInputSeq = {};
    for (const h of duelists) {
      const p = this.players.get(h);
      if (p) lastInputSeq[h] = p.lastInputSeq;
    }

    const snapshot = {
      tick: this.tick,
      phase: this.phase,
      countdownTimer: this.countdownTimer,
      roundOverTimer: this.roundOverTimer,
      roundWinner: this.roundWinner,
      players: playersData,
      bullets: bulletsData,
      roster: this.roster,
      lastInputSeq,
    };

    const data = JSON.stringify(snapshot);

    // Log periodic snapshot info + always log when bullets present
    if (this.tick % 300 === 0 || bulletsData.length > 0) {
      const channels = [];
      for (const [h, p] of this.players) {
        if (h === DUMMY_HANDLE) continue;
        channels.push(`${h}:${p.udpChannelId ? "UDP" : p.wsId != null ? "WS" : "NONE"}`);
      }
      console.log(`🎯 Duel snapshot #${this.tick} phase=${this.phase} bullets=${bulletsData.length} via [${channels.join(", ")}]`);
    }

    // Always send via WS for reliability (UDP was silently dropping packets)
    for (const [handle, player] of this.players) {
      if (handle === DUMMY_HANDLE) continue;
      if (player.wsId != null && this.sendWS) {
        this.sendWS(player.wsId, "duel:snapshot", snapshot);
      }
    }
  }
}
