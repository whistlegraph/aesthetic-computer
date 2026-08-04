const floorY = 815;
const worldLeft = 0;
const worldRight = 12000;
const stageLeft = 55;
const stageRight = 1865;
const platformLeft = 4500;
const platformRight = 7500;
const platformY = 665;
const doubleTapUs = 280000;
const grenadeBlastDuration = .68;
const grenadeBlastRadius = 620;
const roundDurationUs = 30000000;
const roundResultUs = 3000000;
const matchResultUs = 5000000;
const matchWins = 5;
const worldScale = (stageRight - stageLeft) / (worldRight - worldLeft);
const screenX = (x) => stageLeft + (x - worldLeft) * worldScale;
const players = [
  { name: "JEFFREY", pad: 0, spawnX: 2000, x: 2000, y: floorY, vx: 0, vy: 0, facing: 1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 105, 190], hit: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    lastTap: {}, dashUntil: 0, dashVx: 0, roundWins: 0 },
  { name: "OSKIE", pad: 1, spawnX: 10000, x: 10000, y: floorY, vx: 0, vy: 0, facing: -1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 232, 92], hit: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    lastTap: {}, dashUntil: 0, dashVx: 0, roundWins: 0 },
];
const bullets = [];
const grenades = [];
const impacts = [];
let padSnapshots = [null, null];
let startedAt = 0;
let lastSimAt = 0;
let roundElapsedUs = 0;
let roundOverAt = 0;
let roundResult = "";
let matchOver = false;

function emitSignal(event, player = -1, value = 0, value2 = 0) {
  if (typeof gameSignal === "function") gameSignal(event, player, value, value2);
}

const buttonLabel = (button) => ({
  ArrowUp: "UP", ArrowDown: "DOWN", ArrowLeft: "LEFT", ArrowRight: "RIGHT",
  LeftShoulder: "LB", RightShoulder: "RB", LeftStick: "LEFT STICK",
  RightStick: "RIGHT STICK", View: "VIEW", Menu: "MENU",
}[button] || String(button).toUpperCase());

function boot() {
  startedAt = runtime().monotonicUs;
  lastSimAt = startedAt;
  roundElapsedUs = 0;
  emitSignal("hello", -1, 1, 0);
}

function resetRound(now, resetMatch = false) {
  bullets.length = 0;
  grenades.length = 0;
  impacts.length = 0;
  for (const player of players) {
    player.x = player.spawnX;
    player.y = floorY;
    player.vx = 0;
    player.vy = 0;
    player.facing = player.pad === 0 ? 1 : -1;
    player.grounded = true;
    player.ducking = false;
    player.alive = true;
    player.respawnAt = 0;
    player.score = 0;
    if (resetMatch) player.roundWins = 0;
    player.inputX = 0;
    player.inputY = 0;
    player.dashUntil = 0;
    player.previous = padSnapshots[player.pad]?.down?.slice() || [];
    player.lastButton = "NONE";
    player.lastButtonAt = -10000000;
  }
  roundResult = "";
  matchOver = false;
  roundElapsedUs = 0;
  lastSimAt = now;
}

function finishRound(now) {
  if (roundResult) return;
  if (players[0].score === players[1].score) {
    roundResult = "TIE";
    emitSignal("tie", -1, players[0].score, players[1].score);
  }
  else {
    const winner = players[0].score > players[1].score ? players[0] : players[1];
    winner.roundWins += 1;
    matchOver = winner.roundWins >= matchWins;
    roundResult = winner.name + (matchOver ? " WINS MATCH" : " WINS ROUND");
    emitSignal(matchOver ? "matchwin" : "roundwin", winner.pad,
      winner.roundWins, winner.score);
  }
  roundOverAt = now;
  for (const player of players) player.vx = 0;
  drum("clap", 1.2, 0);
}

function quantizedInput(pad) {
  const held = pad.down;
  let horizontal = (held.includes("ArrowRight") ? 1 : 0) -
    (held.includes("ArrowLeft") ? 1 : 0);
  let vertical = (held.includes("ArrowUp") ? 1 : 0) -
    (held.includes("ArrowDown") ? 1 : 0);
  if (!horizontal && Math.abs(pad.leftX) >= 0.48) horizontal = pad.leftX > 0 ? 1 : -1;
  if (!vertical && Math.abs(pad.leftY) >= 0.48) vertical = pad.leftY > 0 ? 1 : -1;
  return { horizontal, vertical };
}

function remember(player, button) {
  player.lastButton = buttonLabel(button);
  player.lastButtonAt = runtime().monotonicUs;
  telemetry("FIGHT_BUTTON", player.name + " " + player.lastButton);
}

function playButtonDrum(button, player) {
  const pan = player.pad === 0 ? -0.4 : 0.4;
  if (button === "A") drum("hat", 1.05, pan);
  else if (button === "B") drum("kick", 0.95, pan);
  else if (button === "X") drum("hat", 0.9, pan);
  else if (button === "Y") drum("clap", 0.95, pan);
  else if (!button.startsWith("Arrow")) drum("block", 0.75, pan);
}

function fire(player) {
  bullets.push({
    x: player.x + player.facing * 70,
    y: player.y - (player.ducking ? 22 : 36),
    vx: player.facing * 2100,
    owner: player.pad,
    life: 1.6,
  });
  while (bullets.length > 24) bullets.shift();
  emitSignal("bullet", player.pad, player.facing, player.ducking ? 1 : 0);
}

function throwGrenade(player) {
  grenades.push({ x: player.x + player.facing * 65,
    y: player.y - (player.ducking ? 25 : 42), vx: player.facing * 1850,
    vy: -720, owner: player.pad, fuse: 1.15, alive: true,
    exploding: false, blastAge: 0, blastRadius: 0 });
  while (grenades.length > 12) grenades.shift();
  emitSignal("grenade", player.pad, player.facing, player.ducking ? 1 : 0);
}

function directionTap(player, direction, now) {
  const previousTap = player.lastTap[direction] || -10000000;
  player.lastTap[direction] = now;
  if (now - previousTap > doubleTapUs) return;
  player.lastTap[direction] = -10000000;
  player.pendingMoveLabel = direction === "UP" ? "ULTRA JUMP" : "DASH " + direction;
  drum("clap", 1.05, player.pad === 0 ? -0.4 : 0.4);
  if (direction === "UP") {
    player.vy = -1250;
    player.grounded = false;
    emitSignal("ultrajump", player.pad, 1, 0);
  } else if (direction === "DOWN") {
    player.vy = 1400;
    player.grounded = false;
    player.ducking = false;
    emitSignal("fastdrop", player.pad, 1, 0);
  } else {
    player.facing = direction === "RIGHT" ? 1 : -1;
    player.dashVx = player.facing * 2800;
    player.dashUntil = now + 190000;
    emitSignal("dash", player.pad, player.facing, 0);
  }
}

function killPlayer(target, killerPad, now) {
  if (!target.alive) return;
  target.alive = false;
  target.respawnAt = now + 1200000;
  target.vx = 0;
  target.vy = 0;
  target.lastButton = "KO";
  target.lastButtonAt = now;
  if (killerPad !== target.pad) players[killerPad].score += 1;
  emitSignal("ko", killerPad, target.pad, players[killerPad]?.score || 0);
  impacts.push({ x: target.x, y: target.y - 36, life: .55,
    duration: .55, death: true, explosion: false });
  drum("snare", 1.15, target.pad === 0 ? -0.4 : 0.4);
}

function updatePlayer(player, pad, dt, now) {
  if (!player.alive) {
    player.previous = pad.down.slice();
    if (now >= player.respawnAt) {
      player.x = player.spawnX;
      player.y = floorY;
      player.vx = 0;
      player.vy = 0;
      player.grounded = true;
      player.ducking = false;
      player.inputX = 0;
      player.inputY = 0;
      player.dashUntil = 0;
      player.alive = true;
    }
    return;
  }
  const input = quantizedInput(pad);
  if ((input.horizontal !== player.inputX || input.vertical !== player.inputY) &&
      (input.horizontal || input.vertical))
    emitSignal("move", player.pad, input.horizontal, input.vertical);
  player.pendingMoveLabel = "";
  const upPressed = input.vertical > 0 && !player.previous.includes("MOVE_UP");
  player.ducking = input.vertical < 0 && player.grounded;

  if (input.horizontal && input.horizontal !== player.inputX)
    directionTap(player, input.horizontal > 0 ? "RIGHT" : "LEFT", now);
  if (input.vertical && input.vertical !== player.inputY)
    directionTap(player, input.vertical > 0 ? "UP" : "DOWN", now);

  if (input.horizontal) player.facing = input.horizontal;
  // Fighting-game directions are digital: full movement begins and ends on
  // the sampled edge. The analog stick is only an eight-way gate.
  player.vx = now < player.dashUntil
    ? player.dashVx
    : player.ducking ? 0 : input.horizontal * 1500;

  if (upPressed && player.grounded) {
    player.vy = -850;
    player.grounded = false;
    player.ducking = false;
    drum("block", 0.72, player.pad === 0 ? -0.4 : 0.4);
    emitSignal("jump", player.pad, 1, 0);
  }

  for (const button of pad.down) {
    if (!player.previous.includes(button)) {
      remember(player, button);
      playButtonDrum(button, player);
      if (button === "A") fire(player);
      else if (button === "B") throwGrenade(player);
    }
  }
  if (player.pendingMoveLabel) remember(player, player.pendingMoveLabel);

  const previousY = player.y;
  player.vy += 1900 * dt;
  player.x += player.vx * dt;
  player.y += player.vy * dt;
  if (player.x < worldLeft + 40) {
    player.x = worldLeft + 40;
    player.vx = Math.abs(player.vx) * 0.28;
  } else if (player.x > worldRight - 40) {
    player.x = worldRight - 40;
    player.vx = -Math.abs(player.vx) * 0.28;
  }
  player.grounded = false;
  if (player.vy >= 0 && previousY <= platformY && player.y >= platformY &&
      player.x >= platformLeft && player.x <= platformRight) {
    player.y = platformY;
    player.vy = 0;
    player.grounded = true;
  } else if (player.y >= floorY) {
    player.y = floorY;
    player.vy = 0;
    player.grounded = true;
  }
  player.hit = Math.max(0, player.hit - dt * 4);
  player.previous = pad.down.slice();
  if (input.vertical > 0) player.previous.push("MOVE_UP");
  player.inputX = input.horizontal;
  player.inputY = input.vertical;
}

function sim() {
  const now = runtime().monotonicUs;
  const dt = Math.min(0.04, Math.max(0.001, (now - lastSimAt) / 1000000));
  lastSimAt = now;
  padSnapshots[0] = gamepad(0);
  padSnapshots[1] = gamepad(1);
  if (roundResult) {
    const resultDuration = matchOver ? matchResultUs : roundResultUs;
    if (now - roundOverAt >= resultDuration) resetRound(now, matchOver);
    return;
  }
  roundElapsedUs += dt * 1000000;
  updatePlayer(players[0], padSnapshots[0], dt, now);
  updatePlayer(players[1], padSnapshots[1], dt, now);

  for (const bullet of bullets) {
    bullet.x += bullet.vx * dt;
    bullet.life -= dt;
    if (bullet.x < worldLeft || bullet.x > worldRight) bullet.life = 0;
  }

  for (let left = 0; left < bullets.length; left++) {
    const a = bullets[left];
    if (a.life <= 0) continue;
    for (let right = left + 1; right < bullets.length; right++) {
      const b = bullets[right];
      if (b.life <= 0 || a.owner === b.owner) continue;
      if (Math.abs(a.x - b.x) <= 48 && Math.abs(a.y - b.y) <= 18) {
        a.life = 0;
        b.life = 0;
        impacts.push({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2,
          life: .18, duration: .18, death: false, explosion: false });
        drum("hat", 1.0, 0);
        emitSignal("cancel", -1, a.owner, b.owner);
        break;
      }
    }
  }

  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const target = players[bullet.owner === 0 ? 1 : 0];
    if (!target.alive) continue;
    const poseTime = (now - startedAt) / 1000000;
    if (runnerDistanceToPoint(target, poseTime, screenX(bullet.x), bullet.y) <= 7) {
      bullet.life = 0;
      killPlayer(target, bullet.owner, now);
    }
  }

  for (const grenade of grenades) {
    if (!grenade.alive) continue;
    if (grenade.exploding) {
      grenade.blastAge += dt;
      grenade.blastRadius = grenadeBlastRadius *
        Math.min(1, grenade.blastAge / grenadeBlastDuration);
      const poseTime = (now - startedAt) / 1000000;
      for (const player of players) {
        const geometryDistance = runnerDistanceToPoint(
          player, poseTime, screenX(grenade.x), grenade.y);
        if (player.alive && geometryDistance <= grenade.blastRadius * worldScale)
          killPlayer(player, grenade.owner, now);
      }
      if (grenade.blastAge >= grenadeBlastDuration) grenade.alive = false;
      continue;
    }
    const previousY = grenade.y;
    grenade.vy += 1800 * dt;
    grenade.x += grenade.vx * dt;
    grenade.y += grenade.vy * dt;
    grenade.fuse -= dt;
    if (grenade.x < worldLeft + 25) {
      grenade.x = worldLeft + 25;
      grenade.vx = Math.abs(grenade.vx) * .65;
    } else if (grenade.x > worldRight - 25) {
      grenade.x = worldRight - 25;
      grenade.vx = -Math.abs(grenade.vx) * .65;
    }
    if (grenade.vy >= 0 && previousY <= platformY - 8 &&
        grenade.y >= platformY - 8 && grenade.x >= platformLeft &&
        grenade.x <= platformRight) {
      grenade.y = platformY - 8;
      grenade.vy = -Math.abs(grenade.vy) * .55;
      grenade.vx *= .82;
    } else if (grenade.y >= floorY - 8) {
      grenade.y = floorY - 8;
      grenade.vy = -Math.abs(grenade.vy) * .55;
      grenade.vx *= .82;
    }
    if (grenade.fuse <= 0) {
      grenade.exploding = true;
      grenade.blastAge = 0;
      grenade.blastRadius = 0;
      grenade.vx = 0;
      grenade.vy = 0;
      drum("kick", 1.25, grenade.owner === 0 ? -0.4 : 0.4);
      emitSignal("blast", grenade.owner, grenade.x / worldRight, grenade.y / floorY);
    }
  }
  for (const impact of impacts) impact.life -= dt;
  while (bullets.length && bullets[0].life <= 0) bullets.shift();
  while (grenades.length && !grenades[0].alive) grenades.shift();
  while (impacts.length && impacts[0].life <= 0) impacts.shift();
  if (roundElapsedUs >= roundDurationUs) finishRound(now);
}

function circle(x, y, radius, width, color) {
  let lastX = x + radius;
  let lastY = y;
  for (let i = 1; i <= 12; i++) {
    const angle = i * Math.PI * 2 / 12;
    const nextX = x + Math.cos(angle) * radius;
    const nextY = y + Math.sin(angle) * radius;
    line(lastX, lastY, nextX, nextY, width, ...color);
    lastX = nextX;
    lastY = nextY;
  }
}

function runnerGeometry(player, t) {
  const speed = Math.min(1, Math.abs(player.vx) / 1500);
  const idle = player.grounded && !player.ducking && speed < .03;
  const breath = idle ? Math.sin(t * 2.4 + player.pad * .7) : 0;
  const idleSway = idle ? Math.sin(t * 1.55 + player.pad) * 2 : 0;
  const stride = Math.sin(t * (7 + speed * 9) + player.pad * Math.PI) * 9 * speed;
  const height = player.ducking ? 30 : 50;
  const lean = player.facing * speed * 3.5;
  const x = screenX(player.x);
  const feet = player.y;
  const hipY = feet - (player.ducking ? 11 : 16);
  const neckX = x + lean;
  const neckY = feet - height + 15 - breath;
  const head = { x: neckX + lean * .2, y: feet - height + 6 - breath * 1.6, radius: 6 };
  const segments = [];
  const segment = (x1, y1, x2, y2, width) => segments.push({ x1, y1, x2, y2, width });
  segment(neckX, neckY, x, hipY, 3);
  if (player.ducking) {
    segment(x, hipY, x - 10, feet - 6, 3);
    segment(x - 10, feet - 6, x - 1, feet, 3);
    segment(x, hipY, x + 10, feet - 6, 3);
    segment(x + 10, feet - 6, x + 16, feet, 3);
  } else if (player.grounded) {
    segment(x, hipY, x - 3 + stride * .45, feet - 8, 3);
    segment(x - 3 + stride * .45, feet - 8, x + stride, feet, 3);
    segment(x, hipY, x + 3 - stride * .45, feet - 8, 3);
    segment(x + 3 - stride * .45, feet - 8, x - stride, feet, 3);
  } else {
    segment(x, hipY, x - 9, feet - 9, 3);
    segment(x - 9, feet - 9, x - 2, feet - 3, 3);
    segment(x, hipY, x + 9, feet - 12, 3);
    segment(x + 9, feet - 12, x + 14, feet - 6, 3);
  }
  const arm = idle ? idleSway : player.grounded ? -stride * .7 : 12;
  const elbowY = feet - (player.ducking ? 21 : 26) - breath;
  const handY = feet - (player.ducking ? 14 : 18) - breath * .5;
  segment(neckX, neckY + 3, x - 7 + arm * .65, elbowY, 3);
  segment(x - 7 + arm * .65, elbowY, x - 3 + arm * .65, handY, 3);
  segment(neckX, neckY + 3, x + 7 - arm * .65, elbowY, 3);
  segment(x + 7 - arm * .65, elbowY, x + 3 - arm * .65, handY, 3);
  return { head, segments };
}

function pointSegmentDistance(px, py, segment) {
  const dx = segment.x2 - segment.x1;
  const dy = segment.y2 - segment.y1;
  const lengthSquared = dx * dx + dy * dy;
  const amount = lengthSquared > 0
    ? Math.max(0, Math.min(1,
      ((px - segment.x1) * dx + (py - segment.y1) * dy) / lengthSquared))
    : 0;
  return Math.hypot(px - (segment.x1 + dx * amount), py - (segment.y1 + dy * amount));
}

function runnerDistanceToPoint(player, t, px, py) {
  const geometry = runnerGeometry(player, t);
  let distance = Math.max(0,
    Math.hypot(px - geometry.head.x, py - geometry.head.y) - geometry.head.radius);
  for (const segment of geometry.segments)
    distance = Math.min(distance,
      Math.max(0, pointSegmentDistance(px, py, segment) - segment.width / 2));
  return distance;
}

function drawRunner(player, t) {
  if (!player.alive) return;
  const geometry = runnerGeometry(player, t);
  const color = player.hit > 0 ? [255, 255, 255] : player.color;
  circle(geometry.head.x, geometry.head.y, geometry.head.radius, 3, color);
  for (const segment of geometry.segments)
    line(segment.x1, segment.y1, segment.x2, segment.y2, segment.width, ...color);

  const labelX = geometry.head.x - (player.name === "JEFFREY" ? 34 : 25);
  systemWrite(player.name, labelX, geometry.head.y - 27, 16, ...color);
}

function drawPlayerHud(player, x, pad) {
  const age = (runtime().monotonicUs - player.lastButtonAt) / 1000000;
  const pulse = Math.max(0, 1 - age * 2.2);
  const color = player.color;
  box(x, 842, 790, 176, 14 + Math.round(pulse * 20), 18, 45 + Math.round(pulse * 35));
  systemWrite(player.name + "  " + player.roundWins + "/" + matchWins,
    x + 36, 862, 34, ...color);
  write(player.lastButton, x + 36, 920, 54 + Math.round(pulse * 10), ...color);
  const held = pad.down.length ? pad.down.map(buttonLabel).join(" ") : "NONE";
  write("KOS " + player.score, x + 300, 876, 24, ...color);
  write("HELD " + held, x + 450, 876, 24, 195, 210, 230);
  write(pad.connected ? "CONTROLLER " + (player.pad + 1) : "CONNECT CONTROLLER " + (player.pad + 1),
    x + 450, 936, 20, pad.connected ? 115 : 255, pad.connected ? 225 : 105,
    pad.connected ? 165 : 105);
}

function paint() {
  const run = runtime();
  const t = (run.monotonicUs - startedAt) / 1000000;
  const titleX = 720 + Math.sin(t * 1.05) * 80;
  const titleY = 76 + Math.sin(t * 2.1) * 7;
  wipe(7, 8, 28);
  for (let i = 0; i < 8; i++) {
    const x = ((i * 310 + t * (38 + i * 3)) % 2300) - 190;
    box(x, 0, 5, 1080, 20 + i * 3, 32 + i * 4, 72 + i * 6);
  }
  box(titleX - 20, titleY - 18, 475, 96, 22, 28, 104);
  write("OSKIEWAR", titleX, titleY, 64, 245, 248, 255);

  box(35, 202, 1850, 633, 10, 13, 30);
  box(stageLeft, floorY, stageRight - stageLeft, 20, 72, 90, 125);
  box(stageLeft, 250, 20, floorY - 250, 72, 90, 125);
  box(stageRight - 20, 250, 20, floorY - 250, 72, 90, 125);
  box(screenX(platformLeft), platformY,
    (platformRight - platformLeft) * worldScale, 18, 72, 90, 125);
  const remaining = roundResult ? 0 : Math.max(0,
    Math.ceil((roundDurationUs - roundElapsedUs) / 1000000));
  const clockInk = remaining <= 10 ? [255, 105, 190] : [245, 248, 255];
  box(850, 218, 220, 78, 22, 28, 104);
  write(String(remaining).padStart(2, "0"), 912, 226, 56, ...clockInk);
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const color = players[bullet.owner].color;
    const x = screenX(bullet.x);
    line(x - Math.sign(bullet.vx) * 34, bullet.y, x, bullet.y, 8, ...color);
    box(x - 7, bullet.y - 7, 14, 14, ...color);
  }
  for (const grenade of grenades) {
    if (!grenade.alive) continue;
    const x = screenX(grenade.x);
    if (grenade.exploding) {
      const radius = grenade.blastRadius * worldScale;
      circle(x, grenade.y, radius, 6, [255, 232, 92]);
      line(x - radius, grenade.y, x + radius, grenade.y, 3, 255, 105, 190);
      line(x, grenade.y - radius, x, grenade.y + radius, 3, 255, 255, 255);
      continue;
    }
    const blink = grenade.fuse < .45 && Math.floor(grenade.fuse * 20) % 2 === 0;
    const color = blink ? [255, 255, 255] : players[grenade.owner].color;
    box(x - 10, grenade.y - 10, 20, 20, ...color);
    line(x - Math.sign(grenade.vx) * 24, grenade.y - 4, x, grenade.y, 5, ...color);
  }
  for (const impact of impacts) {
    const x = screenX(impact.x);
    const radius = 8 + (1 - impact.life / impact.duration) *
      (impact.explosion ? 120 : impact.death ? 75 : 30);
    line(x - radius, impact.y, x + radius, impact.y, 5, 255, 255, 255);
    line(x, impact.y - radius, x, impact.y + radius, 5, 255, 255, 255);
    line(x - radius * .7, impact.y - radius * .7,
      x + radius * .7, impact.y + radius * .7, 4, 255, 232, 92);
    line(x + radius * .7, impact.y - radius * .7,
      x - radius * .7, impact.y + radius * .7, 4, 255, 105, 190);
  }
  drawRunner(players[0], t);
  drawRunner(players[1], t);
  if (roundResult) {
    const resultSize = 72;
    const resultWidth = roundResult.length * 60;
    const resultX = (1920 - resultWidth) / 2;
    box(resultX - 36, 432, resultWidth + 72, 126, 22, 28, 104);
    write(roundResult, resultX, 454, resultSize, 245, 248, 255);
  }
  drawPlayerHud(players[0], 120, padSnapshots[0]);
  drawPlayerHud(players[1], 1010, padSnapshots[1]);
}

function act() {}
function leave() {}
