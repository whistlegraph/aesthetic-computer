// Dumduel, 2026.03.30
// Top-down stick figure shootout. Tap to run, dodge bullets, instant death.
// Two duelists at a time — everyone else waits in the stack.

const ARENA_W = 220;
const ARENA_H = 220;
const BULLET_SPEED = 0.7;
const BULLET_R = 2;
const MOVE_SPEED = 1.0;
const COUNTDOWN_FRAMES = 180;
const ROUND_OVER_FRAMES = 120;
const HIT_R = 7;
const BODY_R = 4;

// -- State --
let server, udpChannel;
let myHandle = "guest";
let myId = null;
let sw = 0, sh = 0;
let frameCount = 0;
let synth = null; // sound.synth reference
let sendFn = null; // send() for window:reload
let updateAvailable = false;

let roster = []; // [{ id, handle }] — first two duel, rest wait
let phase = "waiting"; // waiting | countdown | fight | roundover
let countdownTimer = 0;
let roundOverTimer = 0;
let roundWinner = null;

let me = null; // { x, y, targetX, targetY, alive, wasMoving }
let opponent = null; // { x, y, targetX, targetY, alive, handle }
let bullets = []; // { x, y, vx, vy, owner: "me"|"them" }
let mySlot = -1;
let dummy = false;

function isDueling() {
  return roster.length >= 2 &&
    (roster[0].handle === myHandle || roster[1].handle === myHandle);
}

function myRosterIdx() {
  return roster.findIndex((r) => r.handle === myHandle);
}

function spawnDuelists() {
  bullets = [];
  // Spawn in opposite corners
  const pos0 = { x: 30, y: 30 };
  const pos1 = { x: ARENA_W - 30, y: ARENA_H - 30 };
  const myPos = mySlot === 0 ? pos0 : pos1;
  const opPos = mySlot === 0 ? pos1 : pos0;

  me = {
    x: myPos.x, y: myPos.y,
    targetX: myPos.x, targetY: myPos.y,
    alive: true, wasMoving: false,
  };
  // Find opponent handle from roster (whoever isn't me among first two)
  const opHandle = roster.length >= 2
    ? (roster[0].handle === myHandle ? roster[1].handle : roster[0].handle)
    : "???";
  opponent = {
    x: opPos.x, y: opPos.y,
    targetX: opPos.x, targetY: opPos.y,
    alive: true,
    handle: opHandle,
  };
}

function startPractice() {
  dummy = true;
  if (!roster.find((r) => r.handle === "dummy")) {
    roster.push({ id: "dummy", handle: "dummy" });
  }
  mySlot = 0;
  phase = "countdown";
  countdownTimer = COUNTDOWN_FRAMES;
  spawnDuelists();
  if (opponent) opponent.handle = "dummy";
}

function stopPractice() {
  dummy = false;
  roster = roster.filter((r) => r.handle !== "dummy");
  me = null; opponent = null; bullets = [];
}

function startCountdown() {
  phase = "countdown";
  countdownTimer = COUNTDOWN_FRAMES;

  // Deterministic slot: lower handle alphabetically = slot 0
  if (roster.length >= 2) {
    const h0 = roster[0].handle;
    const h1 = roster[1].handle;
    if (myHandle === h0) mySlot = h0 < h1 ? 0 : 1;
    else if (myHandle === h1) mySlot = h1 < h0 ? 0 : 1;
    else mySlot = -1; // spectating
  } else {
    mySlot = -1;
  }

  if (isDueling()) spawnDuelists();
}

function startFight() {
  phase = "fight";
  if (isDueling() && !me) spawnDuelists();
  // Fight start sound — short rising tone
  synth?.({ type: "square", tone: 440, volume: 0.7, attack: 0.01, decay: 0.15, duration: 0.2 });
}

function endRound(winnerHandle) {
  roundWinner = winnerHandle;
  phase = "roundover";
  roundOverTimer = ROUND_OVER_FRAMES;
  // Death sound
  const won = winnerHandle === myHandle;
  if (won) {
    synth?.({ type: "triangle", tone: 660, volume: 0.8, attack: 0.01, decay: 0.3, duration: 0.35 });
  } else {
    synth?.({ type: "sawtooth", tone: 120, volume: 0.6, attack: 0.01, decay: 0.4, duration: 0.5 });
  }
}

function advanceStack() {
  if (roster.length >= 2) {
    const loserIdx = roster[0].handle === roundWinner ? 1 : 0;
    const loser = roster.splice(loserIdx, 1)[0];
    roster.push(loser);
  }
  roundWinner = null;
  me = null; opponent = null; bullets = [];
  if (roster.length >= 2) startCountdown();
  else startPractice();
}

// Normalize a 2D vector
function norm(dx, dy) {
  const len = Math.sqrt(dx * dx + dy * dy);
  if (len < 0.001) return { nx: 0, ny: 0 };
  return { nx: dx / len, ny: dy / len };
}

function boot({ wipe, screen, net: { socket, udp }, handle, sound, send, net }) {
  sw = screen.width;
  sh = screen.height;
  myHandle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);
  synth = sound.synth;
  sendFn = send;

  // Version polling — auto-reload on new deploy
  const pollVersion = async () => {
    try {
      const res = await fetch("/api/version");
      if (!res.ok) return;
      const info = await res.json();
      const current = info.deployed;
      // Long-poll for changes
      while (true) {
        try {
          const r = await fetch(`/api/version?current=${current}`);
          if (!r.ok) break;
          const data = await r.json();
          if (data.changed !== false) {
            // Auto-reload on new deploy
            sendFn?.({ type: "window:reload" });
            break;
          }
        } catch { break; }
      }
    } catch {}
  };
  pollVersion();

  udpChannel = udp((type, content) => {
    const d = typeof content === "string" ? JSON.parse(content) : content;
    if (d.handle === myHandle) return;
    if (type === "duel:pos" && opponent) {
      opponent.x = d.x; opponent.y = d.y;
      opponent.targetX = d.tx; opponent.targetY = d.ty;
    }
  });

  server = socket((id, type, content) => {
    if (type.startsWith("connected")) {
      myId = id;
      server.send("duel:join", { handle: myHandle });
      return;
    }

    if (type === "left") {
      const idx = roster.findIndex((r) => r.id === id);
      if (idx >= 0) {
        const wasDueling = idx < 2 && roster.length >= 2;
        roster.splice(idx, 1);
        if (wasDueling && (phase === "fight" || phase === "countdown")) {
          if (isDueling()) endRound(myHandle);
          else { phase = "waiting"; me = null; opponent = null; bullets = []; }
        }
      }
      return;
    }

    const msg = typeof content === "string" ? JSON.parse(content) : content;

    if (type === "duel:join" && msg.handle !== myHandle) {
      if (dummy) stopPractice();
      // Update ID if handle already exists (reconnect), otherwise add
      const existing = roster.find((r) => r.handle === msg.handle);
      if (existing) {
        existing.id = id;
      } else {
        roster.push({ id, handle: msg.handle });
      }
      server.send("duel:roster", {
        handle: myHandle,
        roster: roster.map((r) => ({ handle: r.handle })),
        phase,
      });
      if (roster.length >= 2 && phase === "waiting") {
        startCountdown();
        server.send("duel:countdown", { handle: myHandle });
      }
    }

    if (type === "duel:roster" && msg.handle !== myHandle) {
      if (roster.length <= 1) {
        for (const r of msg.roster) {
          if (!roster.find((x) => x.handle === r.handle)) {
            roster.push({ id: r.handle === msg.handle ? id : "?", handle: r.handle });
          }
        }
        if (msg.phase === "countdown" || msg.phase === "fight") startCountdown();
      }
    }

    if (type === "duel:countdown" && msg.handle !== myHandle) {
      if (roster.length >= 2 && phase === "waiting") startCountdown();
    }

    if (type === "duel:fire" && phase === "fight" && msg.handle !== myHandle) {
      bullets.push({ x: msg.x, y: msg.y, vx: msg.vx, vy: msg.vy, owner: "them" });
    }

    if (type === "duel:hit" && phase === "fight" && msg.handle !== myHandle) {
      if (msg.victim === myHandle) {
        if (me) me.alive = false;
        endRound(msg.handle);
      }
    }

    if (type === "duel:roundover" && phase === "fight" && msg.handle !== myHandle) {
      endRound(msg.winner);
      if (opponent && msg.winner === myHandle) opponent.alive = false;
      else if (me && msg.winner !== myHandle) me.alive = false;
    }

    if (type === "duel:advance" && msg.handle !== myHandle) advanceStack();
  });

  roster.push({ id: myId, handle: myHandle });
  startPractice();
  wipe(240, 238, 232);
}

function sim() {
  frameCount++;

  if (phase === "countdown") {
    countdownTimer--;
    // Tick each second
    if (countdownTimer > 0 && countdownTimer % 60 === 0) {
      synth?.({ type: "sine", tone: 330, volume: 0.5, attack: 0.005, decay: 0.1, duration: 0.12 });
    }
    if (countdownTimer <= 0) startFight();
  }

  if (phase === "roundover") {
    roundOverTimer--;
    if (roundOverTimer <= 0) {
      advanceStack();
      server?.send("duel:advance", { handle: myHandle });
    }
  }

  if (phase !== "fight" || !me || (!isDueling() && !dummy)) return;

  // Dummy AI — just wander (no shooting)
  if (dummy && opponent?.alive) {
    if (frameCount % 90 === 0) {
      opponent.targetX = 20 + Math.random() * (ARENA_W - 40);
      opponent.targetY = 20 + Math.random() * (ARENA_H - 40);
    }
    const odx = opponent.targetX - opponent.x;
    const ody = opponent.targetY - opponent.y;
    const dist = Math.sqrt(odx * odx + ody * ody);
    if (dist > 1) {
      opponent.x += (odx / dist) * MOVE_SPEED * 0.7;
      opponent.y += (ody / dist) * MOVE_SPEED * 0.7;
    }
  }

  // Move me toward target
  if (me.alive) {
    const dx = me.targetX - me.x;
    const dy = me.targetY - me.y;
    const dist = Math.sqrt(dx * dx + dy * dy);
    const isMoving = dist > 2;
    if (isMoving) {
      me.x += (dx / dist) * MOVE_SPEED;
      me.y += (dy / dist) * MOVE_SPEED;
    }

    // Fire on stop (was moving, now stopped, no bullet in flight)
    const myBulletOut = bullets.some((b) => b.owner === "me");
    if (me.wasMoving && !isMoving && !myBulletOut && opponent) {
      const { nx, ny } = norm(opponent.x - me.x, opponent.y - me.y);
      const bx = me.x + nx * 6;
      const by = me.y + ny * 6;
      bullets.push({ x: bx, y: by, vx: nx * BULLET_SPEED, vy: ny * BULLET_SPEED, owner: "me" });
      synth?.({ type: "square", tone: 800, volume: 0.35, attack: 0.001, decay: 0.06, duration: 0.07 });
      server?.send("duel:fire", {
        handle: myHandle, x: bx, y: by, vx: nx * BULLET_SPEED, vy: ny * BULLET_SPEED,
      });
    }
    me.wasMoving = isMoving;
  }

  // Interpolate opponent (non-dummy)
  if (!dummy && opponent) {
    const dx = opponent.targetX - opponent.x;
    const dy = opponent.targetY - opponent.y;
    const dist = Math.sqrt(dx * dx + dy * dy);
    if (dist > 1) {
      opponent.x += (dx / dist) * MOVE_SPEED;
      opponent.y += (dy / dist) * MOVE_SPEED;
    }
  }

  // Update bullets
  for (let i = bullets.length - 1; i >= 0; i--) {
    const b = bullets[i];
    b.x += b.vx;
    b.y += b.vy;
    b.age = (b.age || 0) + 1;

    // Fade out after traveling a while
    if (b.age > 200) {
      bullets.splice(i, 1);
      continue;
    }

    // Off arena
    if (b.x < -10 || b.x > ARENA_W + 10 || b.y < -10 || b.y > ARENA_H + 10) {
      bullets.splice(i, 1);
      continue;
    }

    // Hit detection
    if (b.owner === "them" && me.alive) {
      const dx = b.x - me.x, dy = b.y - me.y;
      if (dx * dx + dy * dy < HIT_R * HIT_R) {
        me.alive = false;
        bullets.splice(i, 1);
        endRound(opponent?.handle || "???");
        server?.send("duel:roundover", { handle: myHandle, winner: opponent?.handle || "???" });
        break;
      }
    }
    if (b.owner === "me" && opponent?.alive) {
      const dx = b.x - opponent.x, dy = b.y - opponent.y;
      if (dx * dx + dy * dy < HIT_R * HIT_R) {
        opponent.alive = false;
        bullets.splice(i, 1);
        endRound(myHandle);
        server?.send("duel:hit", { handle: myHandle, victim: opponent.handle });
        server?.send("duel:roundover", { handle: myHandle, winner: myHandle });
        break;
      }
    }
  }

  // Send position via UDP every 3 frames
  if (frameCount % 3 === 0 && udpChannel?.connected && me.alive) {
    udpChannel.send("duel:pos", {
      handle: myHandle, x: me.x, y: me.y, tx: me.targetX, ty: me.targetY,
    });
  }
}

function act({ event: e, screen }) {
  sw = screen.width;
  sh = screen.height;

  if (e.is("touch")) {
    const ox = Math.floor(sw / 2 - ARENA_W / 2);
    const oy = Math.floor(sh / 2 - ARENA_H / 2);

    // Tap arena to move
    if (phase === "fight" && (isDueling() || dummy) && me?.alive) {
      me.targetX = Math.max(6, Math.min(ARENA_W - 6, e.x - ox));
      me.targetY = Math.max(6, Math.min(ARENA_H - 6, e.y - oy));
    }
  }
}

function paint({ wipe, ink, box, write, circle, line, screen }) {
  sw = screen.width;
  sh = screen.height;
  wipe(240, 238, 232);

  const ox = Math.floor(sw / 2 - ARENA_W / 2);
  const oy = Math.floor(sh / 2 - ARENA_H / 2);

  // Arena
  ink(252, 250, 245).box(ox, oy, ARENA_W, ARENA_H);
  ink(180, 175, 165).box(ox, oy, ARENA_W, ARENA_H, "outline");

  // Center cross mark
  ink(230, 225, 218).box(ox + ARENA_W / 2 - 1, oy + ARENA_H / 2 - 6, 2, 12);
  ink(230, 225, 218).box(ox + ARENA_W / 2 - 6, oy + ARENA_H / 2 - 1, 12, 2);

  if (phase === "countdown") {
    const secs = Math.ceil(countdownTimer / 60);
    ink(60, 55, 45).write("" + secs, {
      x: ox + Math.floor(ARENA_W / 2 - 3),
      y: oy + Math.floor(ARENA_H / 2 - 3),
    });

    if (me && opponent) {
      drawFigure(ink, circle, box, line, ox, oy, me, [50, 120, 200], frameCount);
      drawFigure(ink, circle, box, line, ox, oy, opponent, [200, 70, 60], frameCount);
    }

    const d0 = roster[0]?.handle || "?";
    const d1 = roster[1]?.handle || "?";
    const vs = d0 + " vs " + d1;
    ink(160, 155, 145).write(vs, {
      x: ox + Math.floor(ARENA_W / 2 - vs.length * 3),
      y: oy + ARENA_H + 4,
    });
  }

  if (phase === "fight" || phase === "roundover") {
    // Bullets (fade with age)
    for (const b of bullets) {
      const alpha = Math.max(40, 255 - (b.age || 0) * 1.2);
      if (b.owner === "me") ink(50, 120, 200, alpha);
      else ink(200, 70, 60, alpha);
      circle(ox + Math.floor(b.x), oy + Math.floor(b.y), BULLET_R, true);
    }

    // Target indicator (my tap destination)
    if (me?.alive && phase === "fight") {
      ink(50, 120, 200, 60).circle(
        ox + Math.floor(me.targetX),
        oy + Math.floor(me.targetY),
        3, false,
      );
    }

    if (me && opponent) {
      drawFigure(ink, circle, box, line, ox, oy, me, [50, 120, 200], frameCount);
      drawFigure(ink, circle, box, line, ox, oy, opponent, [200, 70, 60], frameCount);
    }

    // Handle labels (MatrixChunky8, centered)
    if (me) {
      ink(50, 120, 200, 150).write(myHandle, {
        x: ox + Math.floor(me.x) - Math.floor(myHandle.length * 2),
        y: oy + Math.floor(me.y) + 9,
      }, undefined, undefined, false, "MatrixChunky8");
    }
    if (opponent) {
      ink(200, 70, 60, 150).write(opponent.handle, {
        x: ox + Math.floor(opponent.x) - Math.floor(opponent.handle.length * 2),
        y: oy + Math.floor(opponent.y) + 9,
      }, undefined, undefined, false, "MatrixChunky8");
    }

    if (phase === "roundover" && roundWinner) {
      const won = roundWinner === myHandle;
      const msg = won ? "you got em!" : "you died!";
      if (won) ink(50, 160, 80); else ink(200, 70, 60);
      write(msg, {
        x: ox + Math.floor(ARENA_W / 2 - msg.length * 3),
        y: oy - 12,
      });
    }
  }

  // Practice label
  if (dummy && (phase === "fight" || phase === "countdown")) {
    ink(200, 195, 185).write("practice", {
      x: ox + Math.floor(ARENA_W / 2 - 24),
      y: oy - 12,
    });
  }

  // Stack
  const stackX = ox + ARENA_W + 8;
  const stackY = oy;
  ink(160, 155, 145).write("stack", { x: stackX, y: stackY });
  let si = 0;
  for (let i = 0; i < roster.length; i++) {
    const r = roster[i];
    if (r.handle === "dummy") continue;
    const isDuelist = i < 2 && roster.length >= 2;
    if (isDuelist) ink(60, 55, 45);
    else if (r.handle === myHandle) ink(120, 115, 105);
    else ink(170, 165, 155);
    write(r.handle, { x: stackX, y: stackY + 12 + si * 10 });
    si++;
  }
}

function drawFigure(ink, circle, box, line, ox, oy, fig, col, fc) {
  const fx = ox + Math.floor(fig.x);
  const fy = oy + Math.floor(fig.y);

  if (!fig.alive) {
    ink(col[0], col[1], col[2], 60);
    line(fx - 3, fy - 3, fx + 3, fy + 3);
    line(fx + 3, fy - 3, fx - 3, fy + 3);
    return;
  }

  // Leg animation based on movement
  const dx = (fig.targetX || fig.x) - fig.x;
  const dy = (fig.targetY || fig.y) - fig.y;
  const moving = dx * dx + dy * dy > 4;
  const legSwing = moving ? Math.sin(fc * 0.3) * 3 : 0;

  ink(...col);
  // Legs
  line(fx, fy + 1, fx - 4 + legSwing, fy + 6);
  line(fx, fy + 1, fx + 4 - legSwing, fy + 6);
  // Arms
  line(fx - 1, fy - 1, fx - 5 - legSwing * 0.5, fy + 2);
  line(fx + 1, fy - 1, fx + 5 + legSwing * 0.5, fy + 2);
  // Head
  circle(fx, fy - 2, 3, true);
  // Eye dot
  ink(255, 255, 255).box(fx, fy - 3, 1, 1);
}

function meta() {
  return {
    title: "Dumduel",
    desc: "Top-down stick figure shootout. Tap to dodge. Instant death.",
  };
}

export { boot, sim, act, paint, meta };
export const desc = "Stick figure shootout.";
