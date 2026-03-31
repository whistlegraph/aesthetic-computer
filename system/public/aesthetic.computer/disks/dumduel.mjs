// Dumduel, 2026.03.30
// Top-down stick figure shootout — server-authoritative netcode.
// Client sends inputs, server owns state, snapshots broadcast via UDP.

const ARENA_W = 220;
const ARENA_H = 220;
const MOVE_SPEED = 1.0;
const BULLET_R = 2;
const BODY_R = 4;

// -- State --
let server, udpChannel;
let myHandle = "guest";
let sendFn = null;
let synth = null;
let sw = 0, sh = 0;
let frameCount = 0;

// Input prediction
let inputSeq = 0;
let pendingInputs = []; // { seq, targetX, targetY }
let localX = 0, localY = 0;
let localTargetX = 0, localTargetY = 0;
let localWasMoving = false;

// Server state (from snapshots)
let snap = null; // latest snapshot
let roster = [];
let phase = "waiting";
let countdownTimer = 0;
let roundWinner = null;
let ping = 0;

function applySnapshot(s) {
  snap = s;
  phase = s.phase;
  countdownTimer = s.countdownTimer;
  roundWinner = s.roundWinner;
  roster = (s.roster || []).map((h) => ({ handle: h }));

  // Reconcile prediction
  const myAck = s.lastInputSeq?.[myHandle] || 0;
  pendingInputs = pendingInputs.filter((inp) => inp.seq > myAck);

  const meServer = s.players?.find((p) => p.handle === myHandle);
  if (meServer) {
    localX = meServer.x;
    localY = meServer.y;
    localTargetX = meServer.targetX;
    localTargetY = meServer.targetY;

    for (const inp of pendingInputs) {
      localTargetX = inp.targetX;
      localTargetY = inp.targetY;
    }

    ping = meServer.ping || 0;
  }
}

function boot({ wipe, screen, net: { socket, udp }, handle, sound, send }) {
  sw = screen.width;
  sh = screen.height;
  myHandle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);
  synth = sound.synth;
  sendFn = send;

  // Version polling — auto-reload on new deploy
  (async () => {
    try {
      const res = await fetch("/api/version");
      if (!res.ok) return;
      const info = await res.json();
      const current = info.deployed;
      while (true) {
        try {
          const r = await fetch(`/api/version?current=${current}`);
          if (!r.ok) break;
          const data = await r.json();
          if (data.changed !== false) { sendFn?.({ type: "window:reload" }); break; }
        } catch { break; }
      }
    } catch {}
  })();

  // Ping measurement every 2s
  setInterval(() => {
    server?.send("duel:ping", { handle: myHandle, ts: Date.now() });
  }, 2000);

  // UDP — receive snapshots from server
  udpChannel = udp((type, content) => {
    if (type === "duel:snapshot") {
      const s = typeof content === "string" ? JSON.parse(content) : content;
      applySnapshot(s);
    }
  });

  // WebSocket — reliable game events
  server = socket((id, type, content) => {
    if (type.startsWith("connected")) {
      server.send("duel:join", { handle: myHandle });
      return;
    }

    const msg = typeof content === "string" ? JSON.parse(content) : content;

    // Snapshot fallback via WS (when UDP not available)
    if (type === "duel:snapshot") {
      applySnapshot(msg);
      return;
    }

    if (type === "duel:joined" || type === "duel:roster") {
      roster = (msg.roster || []).map((h) => ({ handle: h }));
      if (msg.phase) phase = msg.phase;
    }

    if (type === "duel:countdown") {
      phase = "countdown";
      countdownTimer = msg.timer || 180;
    }

    if (type === "duel:fight") {
      phase = "fight";
      synth?.({ type: "square", tone: 440, volume: 0.7, attack: 0.01, decay: 0.15, duration: 0.2 });
    }

    if (type === "duel:death") {
      const won = msg.killer === myHandle;
      if (won) {
        synth?.({ type: "triangle", tone: 660, volume: 0.8, attack: 0.01, decay: 0.3, duration: 0.35 });
      } else if (msg.victim === myHandle) {
        synth?.({ type: "sawtooth", tone: 120, volume: 0.6, attack: 0.01, decay: 0.4, duration: 0.5 });
      }
    }

    if (type === "duel:roundover") {
      phase = "roundover";
      roundWinner = msg.winner;
    }

    if (type === "duel:advance") {
      roster = (msg.roster || []).map((h) => ({ handle: h }));
      roundWinner = null;
    }

    if (type === "duel:pong") {
      ping = Date.now() - msg.ts;
    }
  });

  wipe(240, 238, 232);
}

function sim() {
  frameCount++;

  // Predict local movement
  if (phase === "fight" || phase === "countdown") {
    const dx = localTargetX - localX;
    const dy = localTargetY - localY;
    const dist = Math.sqrt(dx * dx + dy * dy);
    const isMoving = dist > 2;
    if (isMoving) {
      localX += (dx / dist) * MOVE_SPEED;
      localY += (dy / dist) * MOVE_SPEED;
    }

    // Optimistic fire sound (predict when we stop)
    if (localWasMoving && !isMoving && snap) {
      const myBulletOut = snap.bullets?.some((b) => b.owner === myHandle);
      if (!myBulletOut) {
        synth?.({ type: "square", tone: 800, volume: 0.35, attack: 0.001, decay: 0.06, duration: 0.07 });
      }
    }
    localWasMoving = isMoving;
  }
}

function act({ event: e, screen }) {
  sw = screen.width;
  sh = screen.height;

  if (e.is("touch") && (phase === "fight" || phase === "countdown")) {
    const ox = Math.floor(sw / 2 - ARENA_W / 2);
    const oy = Math.floor(sh / 2 - ARENA_H / 2);
    const tx = Math.max(6, Math.min(ARENA_W - 6, e.x - ox));
    const ty = Math.max(6, Math.min(ARENA_H - 6, e.y - oy));

    inputSeq++;
    localTargetX = tx;
    localTargetY = ty;
    pendingInputs.push({ seq: inputSeq, targetX: tx, targetY: ty });

    // Send to server via UDP
    udpChannel?.send("duel:input", { seq: inputSeq, targetX: tx, targetY: ty });
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

  // Center cross
  ink(230, 225, 218).box(ox + ARENA_W / 2 - 1, oy + ARENA_H / 2 - 6, 2, 12);
  ink(230, 225, 218).box(ox + ARENA_W / 2 - 6, oy + ARENA_H / 2 - 1, 12, 2);

  const players = snap?.players || [];
  const bullets = snap?.bullets || [];

  if (phase === "waiting") {
    ink(110, 105, 130).write("dumduel", { x: ox + 78, y: oy + 95 });
    ink(90, 85, 110).write("waiting...", { x: ox + 72, y: oy + 110 });
  }

  if (phase === "countdown") {
    const secs = Math.ceil(countdownTimer / 60);
    ink(60, 55, 45).write("" + secs, {
      x: ox + Math.floor(ARENA_W / 2 - 3),
      y: oy + Math.floor(ARENA_H / 2 - 3),
    });

    // Draw figures at spawn
    for (const p of players) {
      const col = isMe(p.handle) ? [50, 120, 200] : [200, 70, 60];
      drawFigure(ink, circle, box, line, ox, oy, p, col, frameCount);
    }

    // VS text
    if (players.length >= 2) {
      const vs = players[0].handle + " vs " + players[1].handle;
      ink(160, 155, 145).write(vs, {
        x: ox + Math.floor(ARENA_W / 2 - vs.length * 3),
        y: oy + ARENA_H + 4,
      });
    }

    // Countdown tick sound
    if (countdownTimer > 0 && countdownTimer % 60 === 0) {
      synth?.({ type: "sine", tone: 330, volume: 0.5, attack: 0.005, decay: 0.1, duration: 0.12 });
    }
  }

  if (phase === "fight" || phase === "roundover") {
    // Bullets (extrapolated client-side for smoothness)
    for (const b of bullets) {
      const alpha = Math.max(40, 255 - (b.age || 0) * 1.2);
      if (b.owner === myHandle) ink(50, 120, 200, alpha);
      else ink(200, 70, 60, alpha);
      circle(ox + Math.floor(b.x), oy + Math.floor(b.y), BULLET_R, true);
    }

    // Target indicator
    if (phase === "fight") {
      const meAlive = players.find((p) => p.handle === myHandle)?.alive;
      if (meAlive) {
        ink(50, 120, 200, 60).circle(
          ox + Math.floor(localTargetX),
          oy + Math.floor(localTargetY),
          3, false,
        );
      }
    }

    // Draw figures — use predicted position for self
    for (const p of players) {
      const col = isMe(p.handle) ? [50, 120, 200] : [200, 70, 60];
      const drawP = isMe(p.handle)
        ? { ...p, x: localX, y: localY, targetX: localTargetX, targetY: localTargetY }
        : p;
      drawFigure(ink, circle, box, line, ox, oy, drawP, col, frameCount);

      // Handle label (MatrixChunky8, centered) + ping
      const label = p.handle;
      const pingStr = p.ping > 0 ? ` ${p.ping}` : "";
      const fullLabel = label + pingStr;
      const lx = (isMe(p.handle) ? localX : p.x);
      const ly = (isMe(p.handle) ? localY : p.y);
      ink(...col, 150).write(fullLabel, {
        x: ox + Math.floor(lx) - Math.floor(fullLabel.length * 2),
        y: oy + Math.floor(ly) + 9,
      }, undefined, undefined, false, "MatrixChunky8");
    }

    // Round over text
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
  const isDummy = roster.some((r) => r.handle === "dummy");
  if (isDummy && (phase === "fight" || phase === "countdown")) {
    ink(200, 195, 185).write("practice", {
      x: ox + Math.floor(ARENA_W / 2 - 24),
      y: oy - 12,
    });
  }

  // Stack (right side)
  const stackX = ox + ARENA_W + 8;
  const stackY = oy;
  ink(160, 155, 145).write("stack", { x: stackX, y: stackY });
  let si = 0;
  for (const r of roster) {
    if (r.handle === "dummy") continue;
    const isDuelist = si < 2 && roster.length >= 2;
    if (isDuelist) ink(60, 55, 45);
    else if (r.handle === myHandle) ink(120, 115, 105);
    else ink(170, 165, 155);
    write(r.handle, { x: stackX, y: stackY + 12 + si * 10 });
    si++;
  }
}

function isMe(handle) {
  return handle === myHandle;
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

  const dx = (fig.targetX || fig.x) - fig.x;
  const dy = (fig.targetY || fig.y) - fig.y;
  const moving = dx * dx + dy * dy > 4;
  const legSwing = moving ? Math.sin(fc * 0.3) * 3 : 0;

  ink(...col);
  line(fx, fy + 1, fx - 4 + legSwing, fy + 6);
  line(fx, fy + 1, fx + 4 - legSwing, fy + 6);
  line(fx - 1, fy - 1, fx - 5 - legSwing * 0.5, fy + 2);
  line(fx + 1, fy - 1, fx + 5 + legSwing * 0.5, fy + 2);
  circle(fx, fy - 2, 3, true);
  ink(255, 255, 255).box(fx, fy - 3, 1, 1);
}

function meta() {
  return {
    title: "Dumduel",
    desc: "Top-down stick figure shootout. Server-authoritative netcode.",
  };
}

export { boot, sim, act, paint, meta };
export const desc = "Stick figure shootout.";
