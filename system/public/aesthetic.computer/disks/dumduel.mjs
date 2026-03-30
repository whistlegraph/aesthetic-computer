// Dumduel, 2026.03.30
// Stick figure shootout. Tap to run, dodge bullets, instant death.
// Two duelists at a time — everyone else waits in the stack.

const ARENA_W = 200;
const ARENA_H = 140;
const GROUND_Y = ARENA_H - 16;
const FIGURE_H = 16;
const FIGURE_W = 6;
const BULLET_SPEED = 2.5;
const BULLET_R = 2;
const MOVE_SPEED = 1.2;
const FIRE_INTERVAL = 90; // frames between auto-shots (~1.5s)
const COUNTDOWN_FRAMES = 180; // 3 seconds
const ROUND_OVER_FRAMES = 120; // 2 seconds pause after kill
const HIT_R = 5; // hitbox radius

// -- State --
let server, udpChannel;
let myHandle = "guest";
let myId = null;
let sw = 0, sh = 0;
let frameCount = 0;

// All connected players in order: [{ id, handle }]
// First two are the duelists, rest are the stack
let roster = [];

// Game phase: "waiting" | "countdown" | "fight" | "roundover"
let phase = "waiting";
let countdownTimer = 0;
let roundOverTimer = 0;
let roundWinner = null; // handle of winner

// Local duelist state (only active when I'm dueling)
let me = null; // { x, y, targetX, alive, fireTimer }
let opponent = null; // { x, y, targetX, alive, fireTimer, handle }
let bullets = []; // { x, y, vx, owner: "me"|"them" }
let mySlot = -1; // 0 = left, 1 = right
let dummy = false; // true when practicing solo against a dummy

function isDueling() {
  return roster.length >= 2 &&
    (roster[0].handle === myHandle || roster[1].handle === myHandle);
}

function myRosterIdx() {
  return roster.findIndex((r) => r.handle === myHandle);
}

function spawnDuelists() {
  bullets = [];
  me = {
    x: mySlot === 0 ? 30 : ARENA_W - 30,
    y: GROUND_Y,
    targetX: mySlot === 0 ? 30 : ARENA_W - 30,
    alive: true,
    fireTimer: FIRE_INTERVAL,
  };
  opponent = {
    x: mySlot === 0 ? ARENA_W - 30 : 30,
    y: GROUND_Y,
    targetX: mySlot === 0 ? ARENA_W - 30 : 30,
    alive: true,
    fireTimer: FIRE_INTERVAL,
    handle: roster[mySlot === 0 ? 1 : 0]?.handle || "???",
  };
}

function startPractice() {
  dummy = true;
  // Add dummy to roster as slot 1
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
  me = null;
  opponent = null;
  bullets = [];
}

function startCountdown() {
  phase = "countdown";
  countdownTimer = COUNTDOWN_FRAMES;

  // Determine slots
  const idx = myRosterIdx();
  if (idx === 0) mySlot = 0;
  else if (idx === 1) mySlot = 1;
  else mySlot = -1; // spectating

  if (isDueling()) spawnDuelists();
}

function startFight() {
  phase = "fight";
  if (isDueling() && !me) spawnDuelists();
}

function endRound(winnerHandle) {
  roundWinner = winnerHandle;
  phase = "roundover";
  roundOverTimer = ROUND_OVER_FRAMES;
}

function advanceStack() {
  // Loser goes to bottom of stack
  if (roster.length >= 2) {
    const loserIdx = roster[0].handle === roundWinner ? 1 : 0;
    const loser = roster.splice(loserIdx, 1)[0];
    roster.push(loser);
  }
  roundWinner = null;
  me = null;
  opponent = null;
  bullets = [];

  if (roster.length >= 2) {
    startCountdown();
  } else {
    // Back to solo — restart practice
    startPractice();
  }
}

function boot({ wipe, screen, net: { socket, udp }, handle }) {
  sw = screen.width;
  sh = screen.height;
  myHandle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);

  udpChannel = udp((type, content) => {
    const d = typeof content === "string" ? JSON.parse(content) : content;
    if (d.handle === myHandle) return;

    if (type === "duel:pos" && opponent) {
      opponent.x = d.x;
      opponent.y = d.y;
      opponent.targetX = d.targetX;
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
          // Opponent left mid-duel — current player wins
          if (isDueling()) {
            endRound(myHandle);
          } else {
            phase = "waiting";
            me = null; opponent = null; bullets = [];
          }
        }
      }
      return;
    }

    const msg = typeof content === "string" ? JSON.parse(content) : content;

    if (type === "duel:join" && msg.handle !== myHandle) {
      // Stop practice if a real player joins
      if (dummy) stopPractice();
      if (!roster.find((r) => r.handle === msg.handle)) {
        roster.push({ id, handle: msg.handle });
      }
      // Send full roster to newcomer
      server.send("duel:roster", {
        handle: myHandle,
        roster: roster.map((r) => ({ handle: r.handle })),
        phase,
      });
      // If we now have 2+ and were waiting, start
      if (roster.length >= 2 && phase === "waiting") {
        startCountdown();
        server.send("duel:countdown", { handle: myHandle });
      }
    }

    if (type === "duel:roster") {
      // Sync roster from existing player
      if (roster.length <= 1) {
        for (const r of msg.roster) {
          if (!roster.find((x) => x.handle === r.handle)) {
            roster.push({ id: r.handle === msg.handle ? id : "?", handle: r.handle });
          }
        }
        if (msg.phase === "countdown" || msg.phase === "fight") {
          startCountdown();
        }
      }
    }

    if (type === "duel:countdown") {
      if (roster.length >= 2 && phase === "waiting") {
        startCountdown();
      }
    }

    if (type === "duel:fire") {
      // Opponent fired a bullet
      if (phase === "fight") {
        bullets.push({
          x: msg.x, y: msg.y, vx: msg.vx, owner: "them",
        });
      }
    }

    if (type === "duel:hit") {
      // Someone got hit — trust the shooter's claim
      if (phase === "fight") {
        if (msg.victim === myHandle) {
          if (me) me.alive = false;
          endRound(msg.handle);
          server.send("duel:roundover", { winner: msg.handle });
        }
      }
    }

    if (type === "duel:roundover") {
      if (phase === "fight") {
        endRound(msg.winner);
        if (opponent && msg.winner === myHandle) {
          opponent.alive = false;
        } else if (me && msg.winner !== myHandle) {
          me.alive = false;
        }
      }
    }

    if (type === "duel:advance") {
      advanceStack();
    }
  });

  // Add self to roster and start practice
  roster.push({ id: myId, handle: myHandle });
  startPractice();

  wipe(240, 238, 232);
}

function sim() {
  frameCount++;

  if (phase === "countdown") {
    countdownTimer--;
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

  // Dummy AI — wander randomly
  if (dummy && opponent?.alive) {
    opponent.fireTimer--;
    // Pick a new random target every ~2s
    if (frameCount % 120 === 0) {
      opponent.targetX = 20 + Math.random() * (ARENA_W - 40);
    }
    const odx = opponent.targetX - opponent.x;
    if (Math.abs(odx) > 1) opponent.x += Math.sign(odx) * MOVE_SPEED * 0.8;
    // Dummy fires
    if (opponent.fireTimer <= 0) {
      opponent.fireTimer = FIRE_INTERVAL;
      const dir = Math.sign(me.x - opponent.x);
      bullets.push({
        x: opponent.x + dir * 4,
        y: opponent.y - FIGURE_H / 2,
        vx: dir * BULLET_SPEED,
        owner: "them",
      });
    }
  }

  // Move toward target
  if (me.alive) {
    const dx = me.targetX - me.x;
    if (Math.abs(dx) > 1) me.x += Math.sign(dx) * MOVE_SPEED;
    else me.x = me.targetX;
  }

  // Opponent movement (interpolated from UDP)
  if (opponent) {
    const dx = opponent.targetX - opponent.x;
    if (Math.abs(dx) > 1) opponent.x += Math.sign(dx) * MOVE_SPEED;
  }

  // Auto-fire
  if (me.alive) {
    me.fireTimer--;
    if (me.fireTimer <= 0) {
      me.fireTimer = FIRE_INTERVAL;
      const dir = opponent ? Math.sign(opponent.x - me.x) : (mySlot === 0 ? 1 : -1);
      const bx = me.x + dir * 4;
      const by = me.y - FIGURE_H / 2;
      bullets.push({ x: bx, y: by, vx: dir * BULLET_SPEED, owner: "me" });
      server?.send("duel:fire", {
        handle: myHandle, x: bx, y: by, vx: dir * BULLET_SPEED,
      });
    }
  }

  // Update bullets
  for (let i = bullets.length - 1; i >= 0; i--) {
    const b = bullets[i];
    b.x += b.vx;

    // Off screen
    if (b.x < -10 || b.x > ARENA_W + 10) {
      bullets.splice(i, 1);
      continue;
    }

    // Hit detection
    if (b.owner === "them" && me.alive) {
      const dx = b.x - me.x;
      const dy = b.y - (me.y - FIGURE_H / 2);
      if (dx * dx + dy * dy < HIT_R * HIT_R) {
        me.alive = false;
        bullets.splice(i, 1);
        endRound(opponent?.handle || "???");
        server?.send("duel:roundover", { winner: opponent?.handle || "???" });
        break;
      }
    }
    if (b.owner === "me" && opponent?.alive) {
      const dx = b.x - opponent.x;
      const dy = b.y - (opponent.y - FIGURE_H / 2);
      if (dx * dx + dy * dy < HIT_R * HIT_R) {
        opponent.alive = false;
        bullets.splice(i, 1);
        endRound(myHandle);
        server?.send("duel:hit", {
          handle: myHandle, victim: opponent.handle,
        });
        server?.send("duel:roundover", { winner: myHandle });
        break;
      }
    }
  }

  // Send position via UDP every 3 frames
  if (frameCount % 3 === 0 && udpChannel?.connected && me.alive) {
    udpChannel.send("duel:pos", {
      handle: myHandle,
      x: me.x,
      y: me.y,
      targetX: me.targetX,
    });
  }
}

function act({ event: e, screen }) {
  sw = screen.width;
  sh = screen.height;

  if (e.is("touch") && phase === "fight" && (isDueling() || dummy) && me?.alive) {
    // Tap to set run target (screen -> arena coords)
    const ox = Math.floor(sw / 2 - ARENA_W / 2);
    const tx = e.x - ox;
    me.targetX = Math.max(8, Math.min(ARENA_W - 8, tx));
  }
}

function paint({ wipe, ink, box, write, line, circle, screen }) {
  sw = screen.width;
  sh = screen.height;
  wipe(240, 238, 232);

  const ox = Math.floor(sw / 2 - ARENA_W / 2);
  const oy = Math.floor(sh / 2 - ARENA_H / 2);

  // Arena bg
  ink(255, 253, 248).box(ox, oy, ARENA_W, ARENA_H);
  // Ground
  ink(220, 215, 205).box(ox, oy + GROUND_Y, ARENA_W, ARENA_H - GROUND_Y);
  // Arena border
  ink(180, 175, 165).box(ox, oy, ARENA_W, ARENA_H, "outline");

  if (phase === "countdown") {
    const secs = Math.ceil(countdownTimer / 60);
    const numStr = "" + secs;
    ink(60, 55, 45).write(numStr, {
      x: ox + Math.floor(ARENA_W / 2 - numStr.length * 3),
      y: oy + 30,
    });

    if (me && opponent) {
      drawStickFigure(ink, line, circle, box, ox, oy, me, [50, 120, 200]);
      drawStickFigure(ink, line, circle, box, ox, oy, opponent, [200, 70, 60]);
    }

    const d0 = roster[0]?.handle || "?";
    const d1 = roster[1]?.handle || "?";
    const vsStr = d0 + " vs " + d1;
    ink(140, 135, 125).write(vsStr, {
      x: ox + Math.floor(ARENA_W / 2 - vsStr.length * 3),
      y: oy + 50,
    });
  }

  if (phase === "fight" || phase === "roundover") {
    // Bullets
    for (const b of bullets) {
      if (b.owner === "me") ink(50, 120, 200);
      else ink(200, 70, 60);
      circle(ox + Math.floor(b.x), oy + Math.floor(b.y), BULLET_R, true);
    }

    if (me && opponent) {
      drawStickFigure(ink, line, circle, box, ox, oy, me, [50, 120, 200]);
      drawStickFigure(ink, line, circle, box, ox, oy, opponent, [200, 70, 60]);
    }

    if (phase === "roundover" && roundWinner) {
      const won = roundWinner === myHandle;
      const msg = won ? "you got em!" : "you died!";
      if (won) ink(50, 160, 80); else ink(200, 70, 60);
      write(msg, {
        x: ox + Math.floor(ARENA_W / 2 - msg.length * 3),
        y: oy + 20,
      });
    }

    if (roster.length >= 2) {
      const d0 = roster[0]?.handle || "?";
      const d1 = roster[1]?.handle || "?";
      ink(140, 135, 125).write(d0, { x: ox + 2, y: oy + ARENA_H + 4 });
      ink(140, 135, 125).write(d1, {
        x: ox + ARENA_W - d1.length * 6 - 2, y: oy + ARENA_H + 4,
      });
    }
  }

  // Practice label
  if (dummy && (phase === "fight" || phase === "countdown")) {
    ink(190, 185, 175).write("practice", {
      x: ox + Math.floor(ARENA_W / 2 - 24),
      y: oy - 10,
    });
  }

  // -- Stack (queue) display --
  const stackX = ox + ARENA_W + 8;
  const stackY = oy;
  ink(160, 155, 145).write("stack", { x: stackX, y: stackY });
  for (let i = 0; i < roster.length; i++) {
    const r = roster[i];
    if (r.handle === "dummy") continue; // don't show dummy in stack
    const isMe = r.handle === myHandle;
    const isDuelist = i < 2 && roster.length >= 2;
    if (isDuelist) ink(60, 55, 45);
    else if (isMe) ink(120, 115, 105);
    else ink(170, 165, 155);
    write(r.handle, { x: stackX, y: stackY + 12 + i * 10 });
  }
}

function drawStickFigure(ink, line, circle, box, ox, oy, fig, col) {
  const fx = ox + Math.floor(fig.x);
  const fy = oy + Math.floor(fig.y);

  if (!fig.alive) {
    // Dead — fallen over
    ink(col[0], col[1], col[2], 80);
    line(fx - 6, fy - 1, fx + 6, fy - 1);
    circle(fx + 7, fy - 2, 2, true);
    return;
  }

  ink(...col);
  // Head
  circle(fx, fy - FIGURE_H, 3, true);
  // Body
  line(fx, fy - FIGURE_H + 3, fx, fy - 4);
  // Arms
  line(fx - 4, fy - FIGURE_H + 7, fx + 4, fy - FIGURE_H + 7);
  // Legs
  line(fx, fy - 4, fx - 3, fy);
  line(fx, fy - 4, fx + 3, fy);
}

function meta() {
  return {
    title: "Dumduel",
    desc: "Stick figure shootout. Tap to dodge. Instant death. Winner stays.",
  };
}

export { boot, sim, act, paint, meta };
export const desc = "Stick figure shootout.";
