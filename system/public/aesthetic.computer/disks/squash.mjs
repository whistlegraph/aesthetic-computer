// Squash, 2026.03.14
// A round-based 2D platformer for two players.
// Uses WebSocket for reliable game events + UDP for low-latency position sync.

/* #region 📚 README
  Squash is a competitive 2D platformer where players stomp on each other's
  heads to score points. Jump on your opponent from above to squash them!

  - WebSocket: join/leave, round control, scoring, invites
  - UDP: real-time position/velocity sync
  - Lobby with chat invite system
  - Best of 3 rounds, first to 3 kills per round
#endregion */

/* #region 🏁 TODO
  - [] Add sound effects
  - [] Add more platform layouts per round
  - [] Add power-ups
  + Done
  - [x] Basic networking (WebSocket + UDP)
  - [x] Lobby / waiting room
  - [x] Round-based scoring
  - [x] 2D platformer physics
  - [x] Stomp mechanic
#endregion */

// -- Constants --
const GRAVITY = 0.4;
const JUMP_FORCE = -7;
const MOVE_SPEED = 2.5;
const DASH_SPEED = 5;
const DASH_DURATION = 8;
const DASH_COOLDOWN = 30;
const PLAYER_W = 10;
const PLAYER_H = 14;
const STOMP_THRESHOLD = 2; // How far above to count as a stomp
const KILLS_TO_WIN_ROUND = 3;
const ROUNDS_TO_WIN = 2;
const ROUND_START_DELAY = 90; // frames (~1.5s)
const RESPAWN_DELAY = 60; // frames (~1s)
const INVINCIBLE_FRAMES = 45; // after respawn
const UDP_SEND_INTERVAL = 3; // send every N frames

// -- Platform layouts --
const LAYOUTS = [
  // Layout 0: Classic
  [
    { x: 0, y: 0, w: 1, h: 0.05 }, // floor
    { x: 0.15, y: 0.65, w: 0.25, h: 0.03 },
    { x: 0.6, y: 0.65, w: 0.25, h: 0.03 },
    { x: 0.3, y: 0.4, w: 0.4, h: 0.03 },
    { x: 0.05, y: 0.25, w: 0.18, h: 0.03 },
    { x: 0.77, y: 0.25, w: 0.18, h: 0.03 },
  ],
  // Layout 1: Towers
  [
    { x: 0, y: 0, w: 1, h: 0.05 }, // floor
    { x: 0.1, y: 0.75, w: 0.15, h: 0.03 },
    { x: 0.75, y: 0.75, w: 0.15, h: 0.03 },
    { x: 0.1, y: 0.5, w: 0.15, h: 0.03 },
    { x: 0.75, y: 0.5, w: 0.15, h: 0.03 },
    { x: 0.35, y: 0.6, w: 0.3, h: 0.03 },
    { x: 0.4, y: 0.3, w: 0.2, h: 0.03 },
  ],
  // Layout 2: Staircase
  [
    { x: 0, y: 0, w: 1, h: 0.05 }, // floor
    { x: 0.0, y: 0.78, w: 0.3, h: 0.03 },
    { x: 0.2, y: 0.6, w: 0.3, h: 0.03 },
    { x: 0.4, y: 0.42, w: 0.3, h: 0.03 },
    { x: 0.6, y: 0.24, w: 0.3, h: 0.03 },
    { x: 0.1, y: 0.3, w: 0.15, h: 0.03 },
  ],
];

// -- State --
let state = "connecting"; // connecting | lobby | countdown | playing | round-end | game-over
let server, udpChannel;
let frameCount = 0;
let countdownTimer = 0;
let respawnTimer = 0;
let roundEndTimer = 0;

// Match state
let currentRound = 0;
let currentLayout = 0;
let roundWins = [0, 0]; // [me, them]
let roundKills = [0, 0]; // kills this round [me, them]
let platforms = [];

// Local player
let me = {
  x: 0, y: 0,
  vx: 0, vy: 0,
  w: PLAYER_W, h: PLAYER_H,
  handle: "player",
  grounded: false,
  facing: 1, // 1 = right, -1 = left
  jumps: 0, maxJumps: 2,
  dashTimer: 0, dashCooldown: 0, dashDir: 0,
  alive: true,
  invincible: 0,
  kills: 0,
  color: [80, 180, 255],
  slot: 0, // 0 = left spawn, 1 = right spawn
};

// Remote player
let them = null;
let lastUdpSend = 0;
let wsConnected = false;

// Particles
let particles = [];

// Screen dimensions (set in boot)
let sw = 0, sh = 0;

// -- Helpers --
function spawnPos(slot, screenW, screenH) {
  const floorY = screenH * (1 - 0.05);
  if (slot === 0) return { x: screenW * 0.15, y: floorY - PLAYER_H };
  return { x: screenW * 0.8, y: floorY - PLAYER_H };
}

function buildPlatforms(layout, screenW, screenH) {
  return layout.map(p => ({
    x: Math.floor(p.x * screenW),
    y: Math.floor((1 - p.y - p.h) * screenH),
    w: Math.floor(p.w * screenW),
    h: Math.max(Math.floor(p.h * screenH), 3),
  }));
}

function resetPlayer(player, slot) {
  const pos = spawnPos(slot, sw, sh);
  player.x = pos.x;
  player.y = pos.y;
  player.vx = 0;
  player.vy = 0;
  player.grounded = false;
  player.alive = true;
  player.invincible = INVINCIBLE_FRAMES;
  player.facing = slot === 0 ? 1 : -1;
  player.jumps = 0;
  player.dashTimer = 0;
  player.dashCooldown = 0;
}

function spawnParticles(x, y, color, count = 8) {
  for (let i = 0; i < count; i++) {
    particles.push({
      x, y,
      vx: (Math.random() - 0.5) * 4,
      vy: -Math.random() * 4 - 1,
      life: 20 + Math.random() * 15,
      color,
      size: 1 + Math.floor(Math.random() * 2),
    });
  }
}

function rectOverlap(a, b) {
  return a.x < b.x + b.w && a.x + a.w > b.x &&
         a.y < b.y + b.h && a.y + a.h > b.y;
}

// -- Lifecycle --

function boot({ wipe, screen, net: { socket, udp }, handle, params }) {
  sw = screen.width;
  sh = screen.height;

  me.handle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);

  // Pick starting layout
  currentLayout = 0;
  platforms = buildPlatforms(LAYOUTS[currentLayout], sw, sh);

  // UDP channel for position sync
  udpChannel = udp((type, content) => {
    if (type === "squash:move" && them) {
      const d = typeof content === "string" ? JSON.parse(content) : content;
      if (d.handle !== me.handle) {
        them.x = d.x; them.y = d.y;
        them.vx = d.vx; them.vy = d.vy;
        them.facing = d.facing;
        them.alive = d.alive;
        them.grounded = d.grounded;
        them.dashTimer = d.dashTimer;
      }
    }
  });

  // WebSocket for reliable game events
  server = socket((id, type, content) => {
    if (type.startsWith("connected")) {
      wsConnected = true;
      me.slot = 0;
      state = "lobby";
      server.send("squash:join", {
        handle: me.handle,
        slot: me.slot,
      });
      return;
    }

    if (type === "left") {
      if (them && them.id === id) {
        them = null;
        if (state !== "lobby") {
          state = "lobby";
          roundWins = [0, 0];
          roundKills = [0, 0];
          currentRound = 0;
        }
      }
      return;
    }

    const msg = typeof content === "string" ? JSON.parse(content) : content;

    if (type === "squash:join" && msg.handle !== me.handle) {
      // Another player joined
      them = {
        id, handle: msg.handle,
        x: 0, y: 0, vx: 0, vy: 0,
        w: PLAYER_W, h: PLAYER_H,
        grounded: false, facing: -1,
        alive: true, invincible: 0,
        dashTimer: 0,
        color: [255, 100, 80],
        slot: 1,
        kills: 0,
      };
      // Assign slots: first joiner = 0, second = 1
      if (me.slot === 0) {
        them.slot = 1;
      } else {
        them.slot = 0;
      }
      // Host starts countdown if both present
      if (state === "lobby") {
        startCountdown();
      }
      // Send our join back so they know about us
      server.send("squash:join", {
        handle: me.handle,
        slot: me.slot,
      });
    }

    if (type === "squash:countdown" && msg.handle !== me.handle) {
      if (state === "lobby" || state === "round-end" || state === "game-over") {
        startCountdown();
      }
    }

    if (type === "squash:stomp" && msg.handle !== me.handle) {
      // We got stomped
      if (me.alive && me.invincible <= 0) {
        me.alive = false;
        respawnTimer = RESPAWN_DELAY;
        roundKills[1]++;
        spawnParticles(me.x + me.w / 2, me.y, me.color, 12);
        checkRoundEnd();
      }
    }

    if (type === "squash:round-end" && msg.handle !== me.handle) {
      // Sync round state
      if (msg.winner !== undefined) {
        roundWins[msg.winner === me.handle ? 0 : 1]++;
      }
    }

    if (type === "squash:new-round" && msg.handle !== me.handle) {
      beginNewRound();
    }

    if (type === "squash:rematch") {
      if (state === "game-over") {
        roundWins = [0, 0];
        currentRound = 0;
        startCountdown();
      }
    }

    if (type === "squash:invite") {
      // Invite received (rendered in chat as a clickable link)
      console.log(`🎾 Game invite from ${msg.handle}!`);
    }
  });

  // Initial spawn
  resetPlayer(me, me.slot);
  wipe(20, 20, 30);
}

function startCountdown() {
  state = "countdown";
  countdownTimer = ROUND_START_DELAY;
  roundKills = [0, 0];
  currentLayout = currentRound % LAYOUTS.length;
  platforms = buildPlatforms(LAYOUTS[currentLayout], sw, sh);
  resetPlayer(me, me.slot);
  if (them) {
    them.alive = true;
    them.invincible = INVINCIBLE_FRAMES;
  }
}

function beginNewRound() {
  currentRound++;
  startCountdown();
}

function checkRoundEnd() {
  // Check if either player hit the kill target this round
  if (roundKills[0] >= KILLS_TO_WIN_ROUND) {
    roundWins[0]++;
    endRound(me.handle);
  } else if (roundKills[1] >= KILLS_TO_WIN_ROUND) {
    roundWins[1]++;
    endRound(them?.handle || "opponent");
  }
}

function endRound(winnerHandle) {
  state = "round-end";
  roundEndTimer = 120; // 2 seconds
  server.send("squash:round-end", {
    handle: me.handle,
    winner: winnerHandle,
  });

  // Check match winner
  if (roundWins[0] >= ROUNDS_TO_WIN || roundWins[1] >= ROUNDS_TO_WIN) {
    state = "game-over";
    roundEndTimer = 180;
  }
}

function sim() {
  frameCount++;

  if (state === "countdown") {
    countdownTimer--;
    if (countdownTimer <= 0) {
      state = "playing";
    }
    return;
  }

  if (state === "round-end" || state === "game-over") {
    roundEndTimer--;
    if (state === "round-end" && roundEndTimer <= 0) {
      beginNewRound();
      server.send("squash:new-round", { handle: me.handle });
    }
    // Update particles even during pause
    updateParticles();
    return;
  }

  if (state !== "playing") return;

  // -- Physics for local player --
  if (me.alive) {
    // Dash
    if (me.dashTimer > 0) {
      me.vx = me.dashDir * DASH_SPEED;
      me.dashTimer--;
      if (me.dashTimer <= 0) me.dashCooldown = DASH_COOLDOWN;
    }
    if (me.dashCooldown > 0) me.dashCooldown--;

    // Gravity
    me.vy += GRAVITY;

    // Apply velocity
    me.x += me.vx;
    me.y += me.vy;

    // Friction (when not dashing)
    if (me.dashTimer <= 0) {
      me.vx *= me.grounded ? 0.7 : 0.9;
    }

    // Platform collisions
    me.grounded = false;
    for (const p of platforms) {
      if (me.vy >= 0 && // falling
          me.x + me.w > p.x && me.x < p.x + p.w &&
          me.y + me.h >= p.y && me.y + me.h <= p.y + p.h + me.vy + 2) {
        me.y = p.y - me.h;
        me.vy = 0;
        me.grounded = true;
        me.jumps = 0;
      }
    }

    // Screen bounds (wrap horizontally)
    if (me.x + me.w < 0) me.x = sw;
    if (me.x > sw) me.x = -me.w;
    // Ceiling
    if (me.y < 0) { me.y = 0; me.vy = 0; }
    // Fall off bottom = death
    if (me.y > sh + 20) {
      me.alive = false;
      respawnTimer = RESPAWN_DELAY;
      roundKills[1]++;
      spawnParticles(sw / 2, sh - 10, me.color, 6);
      checkRoundEnd();
    }

    // Invincibility countdown
    if (me.invincible > 0) me.invincible--;

    // -- Stomp detection --
    if (them?.alive && me.invincible <= 0 && them.invincible <= 0) {
      const meBox = { x: me.x, y: me.y, w: me.w, h: me.h };
      const themBox = { x: them.x, y: them.y, w: them.w, h: them.h };

      if (rectOverlap(meBox, themBox)) {
        // Check who is on top
        const myBottom = me.y + me.h;
        const theirTop = them.y;
        const myTop = me.y;
        const theirBottom = them.y + them.h;

        if (myBottom <= theirTop + STOMP_THRESHOLD + 4 && me.vy > 0) {
          // I stomped them!
          me.vy = JUMP_FORCE * 0.6; // bounce up
          roundKills[0]++;
          spawnParticles(them.x + them.w / 2, them.y, them.color, 12);
          server.send("squash:stomp", { handle: me.handle });
          checkRoundEnd();
        } else if (theirBottom <= myTop + STOMP_THRESHOLD + 4 && them.vy > 0) {
          // They stomped me
          me.alive = false;
          respawnTimer = RESPAWN_DELAY;
          roundKills[1]++;
          spawnParticles(me.x + me.w / 2, me.y, me.color, 12);
          checkRoundEnd();
        } else {
          // Side collision - push apart
          const pushDir = me.x < them.x ? -1 : 1;
          me.vx = pushDir * 3;
        }
      }
    }
  } else {
    // Dead - respawn timer
    respawnTimer--;
    if (respawnTimer <= 0 && state === "playing") {
      resetPlayer(me, me.slot);
    }
  }

  // -- Send UDP position --
  if (frameCount % UDP_SEND_INTERVAL === 0 && udpChannel?.connected) {
    udpChannel.send("squash:move", {
      handle: me.handle,
      x: me.x, y: me.y,
      vx: me.vx, vy: me.vy,
      facing: me.facing,
      alive: me.alive,
      grounded: me.grounded,
      dashTimer: me.dashTimer,
    });
  }

  // Fallback: send via WebSocket less frequently
  if (frameCount % (UDP_SEND_INTERVAL * 4) === 0 && !udpChannel?.connected) {
    server?.send("squash:move", {
      handle: me.handle,
      x: me.x, y: me.y,
      vx: me.vx, vy: me.vy,
      facing: me.facing,
      alive: me.alive,
      grounded: me.grounded,
      dashTimer: me.dashTimer,
    });
  }

  updateParticles();
}

function updateParticles() {
  for (let i = particles.length - 1; i >= 0; i--) {
    const p = particles[i];
    p.x += p.vx;
    p.y += p.vy;
    p.vy += 0.15;
    p.life--;
    if (p.life <= 0) particles.splice(i, 1);
  }
}

function paint({ wipe, ink, line, box, write, screen, num: { randInt } }) {
  sw = screen.width;
  sh = screen.height;
  wipe(20, 20, 30);

  // -- Draw platforms --
  for (const p of platforms) {
    ink(60, 60, 80).box(p.x, p.y, p.w, p.h);
    // Highlight top edge
    ink(90, 90, 120).box(p.x, p.y, p.w, 1);
  }

  if (state === "connecting") {
    ink(150).write("Connecting...", { center: "xy", size: 1 });
    return;
  }

  if (state === "lobby") {
    ink(255).write("SQUASH", { x: sw / 2 - 24, y: sh * 0.2 });
    ink(180).write("Waiting for opponent...", { x: sw / 2 - 64, y: sh * 0.35 });
    if (them) {
      ink(120, 255, 120).write(`${them.handle} joined!`, { x: sw / 2 - 50, y: sh * 0.45 });
    }
    ink(100).write("Invite: /squash in chat", { x: sw / 2 - 70, y: sh * 0.7 });
    drawPlayer(ink, box, me, true);
    return;
  }

  // -- Draw players --
  if (them) drawPlayer(ink, box, them, false);
  drawPlayer(ink, box, me, true);

  // -- Draw particles --
  for (const p of particles) {
    const alpha = Math.floor((p.life / 35) * 255);
    ink(p.color[0], p.color[1], p.color[2], alpha).box(p.x, p.y, p.size, p.size);
  }

  // -- HUD --
  // Round indicators
  const hudY = 4;
  ink(200).write(`Round ${currentRound + 1}`, { x: sw / 2 - 20, y: hudY });

  // Score: round wins as dots
  for (let i = 0; i < ROUNDS_TO_WIN; i++) {
    ink(i < roundWins[0] ? me.color : [50, 50, 60]).box(4 + i * 8, hudY, 5, 5);
  }
  for (let i = 0; i < ROUNDS_TO_WIN; i++) {
    ink(i < roundWins[1] ? (them?.color || [255, 100, 80]) : [50, 50, 60])
      .box(sw - 8 - i * 8, hudY, 5, 5);
  }

  // Kill count this round
  ink(180).write(`${roundKills[0]}`, { x: 4, y: hudY + 10 });
  ink(180).write(`${roundKills[1]}`, { x: sw - 10, y: hudY + 10 });

  // Handles
  ink(...me.color).write(me.handle, { x: 4, y: sh - 10 });
  if (them) {
    ink(...them.color).write(them.handle, { x: sw - them.handle.length * 6 - 4, y: sh - 10 });
  }

  // -- State overlays --
  if (state === "countdown") {
    const sec = Math.ceil(countdownTimer / 60);
    if (sec > 0) {
      ink(0, 0, 0, 150).box(sw / 2 - 30, sh / 2 - 15, 60, 30);
      ink(255, 255, 100).write(`${sec}`, { x: sw / 2 - 3, y: sh / 2 - 5 });
    } else {
      ink(255, 255, 100).write("GO!", { x: sw / 2 - 10, y: sh / 2 - 5 });
    }
  }

  if (state === "round-end") {
    const winner = roundKills[0] >= KILLS_TO_WIN_ROUND ? "You" : (them?.handle || "Opponent");
    ink(0, 0, 0, 180).box(sw / 2 - 55, sh / 2 - 20, 110, 40);
    ink(255, 220, 50).write(`${winner} wins!`, { x: sw / 2 - 40, y: sh / 2 - 10 });
    ink(180).write(`Round ${currentRound + 1}`, { x: sw / 2 - 25, y: sh / 2 + 5 });
  }

  if (state === "game-over") {
    const matchWinner = roundWins[0] >= ROUNDS_TO_WIN ? "YOU WIN!" : "YOU LOSE";
    const winColor = roundWins[0] >= ROUNDS_TO_WIN ? [100, 255, 100] : [255, 80, 80];
    ink(0, 0, 0, 200).box(sw / 2 - 55, sh / 2 - 25, 110, 50);
    ink(...winColor).write(matchWinner, { x: sw / 2 - 30, y: sh / 2 - 15 });
    ink(180).write(`${roundWins[0]} - ${roundWins[1]}`, { x: sw / 2 - 12, y: sh / 2 });
    ink(140).write("Press ENTER", { x: sw / 2 - 30, y: sh / 2 + 14 });
  }

  // Dash cooldown indicator
  if (me.dashCooldown > 0 && me.alive) {
    const cdW = 20;
    const cdFill = Math.floor((1 - me.dashCooldown / DASH_COOLDOWN) * cdW);
    ink(40, 40, 50).box(me.x - 5, me.y - 6, cdW, 2);
    ink(100, 200, 255).box(me.x - 5, me.y - 6, cdFill, 2);
  }

  if (!me.alive && state === "playing") {
    const sec = Math.ceil(respawnTimer / 60);
    ink(255, 80, 80, 180).write(`${sec}`, { x: sw / 2 - 3, y: sh / 2 - 5 });
  }
}

function drawPlayer(ink, box, player, isLocal) {
  if (!player.alive) return;

  const flash = player.invincible > 0 && Math.floor(player.invincible / 4) % 2;
  if (flash) return; // blink during invincibility

  const c = player.color;

  // Body
  ink(c[0], c[1], c[2]).box(player.x, player.y + 3, player.w, player.h - 3);
  // Head
  ink(c[0] + 30, c[1] + 30, c[2] + 30).box(player.x + 1, player.y, player.w - 2, 5);
  // Eyes
  const eyeX = player.facing === 1 ? player.x + 6 : player.x + 2;
  ink(255).box(eyeX, player.y + 1, 2, 2);
  // Feet
  ink(c[0] - 30, c[1] - 30, c[2] - 30).box(player.x + 1, player.y + player.h - 2, 3, 2);
  ink(c[0] - 30, c[1] - 30, c[2] - 30).box(player.x + player.w - 4, player.y + player.h - 2, 3, 2);

  // Dash trail
  if (player.dashTimer > 0) {
    for (let i = 1; i <= 3; i++) {
      const alpha = 80 - i * 20;
      ink(c[0], c[1], c[2], alpha)
        .box(player.x - player.facing * i * 4, player.y + 3, player.w, player.h - 3);
    }
  }
}

function act({ event: e, jump }) {
  // -- Movement input --
  if (state === "playing" && me.alive) {
    if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft")) {
      me.vx = -MOVE_SPEED;
      me.facing = -1;
    }
    if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright")) {
      me.vx = MOVE_SPEED;
      me.facing = 1;
    }

    // Continuous movement via held keys
    if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft") ||
        e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) {
      // Handled by friction in sim
    }

    // Jump
    if (e.is("keyboard:down:w") || e.is("keyboard:down:arrowup") ||
        e.is("keyboard:down: ")) {
      if (me.jumps < me.maxJumps) {
        me.vy = JUMP_FORCE;
        me.jumps++;
        me.grounded = false;
        if (me.jumps === 2) {
          // Double jump particles
          spawnParticles(me.x + me.w / 2, me.y + me.h, [200, 200, 255], 4);
        }
      }
    }

    // Dash
    if (e.is("keyboard:down:shift") || e.is("keyboard:down:z")) {
      if (me.dashCooldown <= 0 && me.dashTimer <= 0) {
        me.dashTimer = DASH_DURATION;
        me.dashDir = me.facing;
        spawnParticles(me.x + me.w / 2, me.y + me.h / 2, [150, 220, 255], 5);
      }
    }
  }

  // Rematch
  if (state === "game-over" && e.is("keyboard:down:enter")) {
    server?.send("squash:rematch", { handle: me.handle });
    roundWins = [0, 0];
    currentRound = 0;
    startCountdown();
  }

  // Touch controls (mobile)
  if (state === "playing" && me.alive) {
    if (e.is("touch")) {
      const tx = e.x;
      const ty = e.y;
      // Left third = move left, right third = move right, tap upper half = jump
      if (ty < sh * 0.5) {
        // Jump
        if (me.jumps < me.maxJumps) {
          me.vy = JUMP_FORCE;
          me.jumps++;
          me.grounded = false;
        }
      } else if (tx < sw * 0.33) {
        me.vx = -MOVE_SPEED;
        me.facing = -1;
      } else if (tx > sw * 0.66) {
        me.vx = MOVE_SPEED;
        me.facing = 1;
      }
    }
  }

  // Continuous key state for smooth movement
  if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft")) {
    me._moveLeft = true;
  }
  if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft")) {
    me._moveLeft = false;
  }
  if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright")) {
    me._moveRight = true;
  }
  if (e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) {
    me._moveRight = false;
  }

  // Apply held keys in sim
  if (state === "playing" && me.alive && me.dashTimer <= 0) {
    if (me._moveLeft) { me.vx = -MOVE_SPEED; me.facing = -1; }
    if (me._moveRight) { me.vx = MOVE_SPEED; me.facing = 1; }
  }
}

function leave() {
  server = null;
  udpChannel = null;
}

function meta() {
  return {
    title: "Squash",
    desc: "A round-based 2D platformer. Stomp your opponent!",
  };
}

export { boot, sim, paint, act, leave, meta };

// 📚 Library
