const floorY = 12000;
const ceilingY = 0;
const wallThickness = 80;
const worldLeft = 0;
const worldRight = 12000;
const worldNear = -1800;
const worldFar = 1800;
const stageLeft = 55;
const stageRight = 1865;
const stageTop = 350;
const stageBottom = 1020;
const cameraAspect = (stageRight - stageLeft) / (stageBottom - stageTop);
const platformLeft = 4500;
const platformRight = 7500;
const platformY = 10400;
const doubleTapUs = 280000;
const doubleTapReleaseUs = 40000;
const grenadeBlastDuration = .68;
const grenadeBlastRadius = 620;
const roundDurationUs = 30000000;
const roundResultUs = 3000000;
const matchResultUs = 5000000;
const matchWins = 5;
const pickupRespawnUs = 7000000;
let cameraCenter = (worldLeft + worldRight) / 2;
let cameraWidth = worldRight - worldLeft;
let cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
const cameraScale = () => (stageRight - stageLeft) / cameraWidth;
function projectPoint(x, y, z = 0) {
  return {
    x: (stageLeft + stageRight) / 2 +
      (x - cameraCenter + z * .32) * cameraScale(),
    y: (stageTop + stageBottom) / 2 +
      (y - cameraCenterY - z * .18) * cameraScale(),
  };
}
const screenX = (x, z = 0) => projectPoint(x, cameraCenterY, z).x;
const screenY = (y, z = 0) => projectPoint(cameraCenter, y, z).y;
const players = [
  { name: "JEFFREY", pad: 0, spawnX: 2000, x: 2000, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: 1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 105, 190], hit: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, roundWins: 0,
    gunAmmo: 0, grenadeAmmo: 0, attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false },
  { name: "OSKIE", pad: 1, spawnX: 10000, x: 10000, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: -1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 232, 92], hit: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, roundWins: 0,
    gunAmmo: 0, grenadeAmmo: 0, attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false },
];
const bullets = [];
const grenades = [];
const impacts = [];
const pickups = [
  { type: "gun", amount: 6, x: 3000, y: floorY - 70, z: 0 },
  { type: "grenade", amount: 2, x: 4200, y: floorY - 70, z: 0 },
  { type: "gun", amount: 6, x: 9000, y: floorY - 70, z: 0 },
  { type: "grenade", amount: 2, x: 7800, y: floorY - 70, z: 0 },
  { type: "gun", amount: 4, x: 6000, y: platformY - 70, z: 0 },
  { type: "grenade", amount: 2, x: 3500, y: 7200, z: 0 },
  { type: "gun", amount: 4, x: 8500, y: 4400, z: 0 },
];
for (const pickup of pickups) {
  pickup.active = true;
  pickup.respawnAt = 0;
}
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
    player.z = 0;
    player.vx = 0;
    player.vy = 0;
    player.vz = 0;
    player.facing = player.pad === 0 ? 1 : -1;
    player.grounded = true;
    player.ducking = false;
    player.alive = true;
    player.respawnAt = 0;
    player.score = 0;
    if (resetMatch) player.roundWins = 0;
    player.inputX = 0;
    player.inputY = 0;
    player.lastTap = {};
    player.lastRelease = {};
    player.dashUntil = 0;
    player.gunAmmo = 0;
    player.grenadeAmmo = 0;
    player.attackKind = "";
    player.attackUntil = 0;
    player.attackHit = false;
    player.previous = padSnapshots[player.pad]?.down?.slice() || [];
    player.lastButton = "NONE";
    player.lastButtonAt = -10000000;
  }
  for (const pickup of pickups) {
    pickup.active = true;
    pickup.respawnAt = 0;
  }
  roundResult = "";
  matchOver = false;
  roundElapsedUs = 0;
  lastSimAt = now;
  cameraCenter = (worldLeft + worldRight) / 2;
  cameraWidth = worldRight - worldLeft;
  cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
}

function updateCamera(dt) {
  const left = Math.min(players[0].x, players[1].x);
  const right = Math.max(players[0].x, players[1].x);
  const top = Math.min(players[0].y - 220, players[1].y - 220);
  const bottom = Math.max(players[0].y, players[1].y);
  const maxWidth = Math.max(worldRight - worldLeft,
    (floorY - ceilingY) * cameraAspect);
  const desiredWidth = Math.max(1800, Math.min(maxWidth,
    Math.max(right - left + 900, (bottom - top + 450) * cameraAspect)));
  const widthBlend = Math.min(1, dt * 3.2);
  cameraWidth += (desiredWidth - cameraWidth) * widthBlend;
  const halfWidth = cameraWidth / 2;
  const halfHeight = cameraWidth / cameraAspect / 2;
  const desiredCenter = cameraWidth >= worldRight - worldLeft
    ? (worldLeft + worldRight) / 2
    : Math.max(worldLeft + halfWidth,
      Math.min(worldRight - halfWidth, (left + right) / 2));
  const desiredCenterY = halfHeight * 2 >= floorY - ceilingY
    ? (ceilingY + floorY) / 2
    : Math.max(ceilingY + halfHeight,
      Math.min(floorY - halfHeight, (top + bottom) / 2));
  cameraCenter += (desiredCenter - cameraCenter) * Math.min(1, dt * 4.5);
  cameraCenterY += (desiredCenterY - cameraCenterY) * Math.min(1, dt * 4.5);
  if (cameraWidth < worldRight - worldLeft)
    cameraCenter = Math.max(worldLeft + halfWidth,
      Math.min(worldRight - halfWidth, cameraCenter));
  else cameraCenter = (worldLeft + worldRight) / 2;
  if (halfHeight * 2 < floorY - ceilingY)
    cameraCenterY = Math.max(ceilingY + halfHeight,
      Math.min(floorY - halfHeight, cameraCenterY));
  else cameraCenterY = (ceilingY + floorY) / 2;
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
  if (button === "X") drum("hat", 0.9, pan);
  else if (button === "Y") drum("clap", 0.95, pan);
  else if (button !== "A" && button !== "B" && !button.startsWith("Arrow"))
    drum("block", 0.75, pan);
}

function fire(player, input) {
  const aimX = input.horizontal || player.facing;
  const aimY = -input.vertical;
  const length = Math.hypot(aimX, aimY) || 1;
  const dx = aimX / length;
  const dy = aimY / length;
  bullets.push({
    x: player.x + dx * 180,
    y: player.y - (player.ducking ? 75 : 130) + dy * 80,
    z: player.z,
    vx: dx * 2100,
    vy: dy * 2100,
    owner: player.pad,
    life: 1.6,
  });
  while (bullets.length > 24) bullets.shift();
  player.gunAmmo -= 1;
  drum("hat", 1.05, player.pad === 0 ? -.4 : .4);
  emitSignal("bullet", player.pad, aimX, aimY);
}

function throwGrenade(player) {
  grenades.push({ x: player.x + player.facing * 190,
    y: player.y - (player.ducking ? 80 : 145), z: player.z,
    vx: player.facing * 1850,
    vy: -720, owner: player.pad, fuse: 1.15, alive: true,
    exploding: false, blastAge: 0, blastRadius: 0 });
  while (grenades.length > 12) grenades.shift();
  player.grenadeAmmo -= 1;
  drum("kick", .95, player.pad === 0 ? -.4 : .4);
  emitSignal("grenade", player.pad, player.facing, player.ducking ? 1 : 0);
}

function startMelee(player, kind, now) {
  player.attackKind = kind;
  player.attackStartedAt = now;
  player.attackUntil = now + 220000;
  player.attackHit = false;
  player.pendingMoveLabel = kind;
  const pan = player.pad === 0 ? -.4 : .4;
  drum(kind === "KICK" ? "kick" : "snare", 1.05, pan);
  emitSignal(kind.toLowerCase(), player.pad, player.facing, 0);
}

function meleePulse(player, now) {
  if (now >= player.attackUntil || player.attackUntil <= player.attackStartedAt) return 0;
  const phase = (now - player.attackStartedAt) /
    (player.attackUntil - player.attackStartedAt);
  return Math.sin(Math.max(0, Math.min(1, phase)) * Math.PI);
}

function meleeStrike(player, now) {
  const pulse = meleePulse(player, now);
  const reach = 70 + 150 * pulse;
  return {
    x: player.x + player.facing * reach,
    y: player.y - (player.attackKind === "KICK" ? 55 : 115),
    z: player.z,
    radius: player.attackKind === "KICK" ? 35 : 28,
  };
}

function directionTap(player, direction, now) {
  const previousTap = player.lastTap[direction] || -10000000;
  const releasedAt = player.lastRelease[direction] || -10000000;
  player.lastTap[direction] = now;
  if (now - previousTap > doubleTapUs || releasedAt <= previousTap ||
      now - releasedAt < doubleTapReleaseUs) return;
  player.lastTap[direction] = -10000000;
  player.pendingMoveLabel = direction === "UP" ? "ULTRA JUMP" : "DASH " + direction;
  drum("clap", 1.05, player.pad === 0 ? -0.4 : 0.4);
  if (direction === "UP") {
    player.vy = -2500;
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
  impacts.push({ x: target.x, y: target.y - 120, z: target.z, life: .55,
    duration: .55, death: true, explosion: false });
  drum("snare", 1.15, target.pad === 0 ? -0.4 : 0.4);
}

function resolveMelee(now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const attacker of players) {
    if (!attacker.alive || attacker.attackHit || now >= attacker.attackUntil) continue;
    const target = players[attacker.pad === 0 ? 1 : 0];
    if (!target.alive) continue;
    const strike = meleeStrike(attacker, now);
    if (runnerDistanceToPoint(target, poseTime,
      strike.x, strike.y, strike.z) <= strike.radius) {
      attacker.attackHit = true;
      impacts.push({ x: strike.x, y: strike.y, z: strike.z,
        life: .2, duration: .2, death: false, explosion: false });
      killPlayer(target, attacker.pad, now);
    }
  }
}

function updatePickups(now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const pickup of pickups) {
    if (!pickup.active) {
      if (now >= pickup.respawnAt) pickup.active = true;
      else continue;
    }
    for (const player of players) {
      if (!player.alive || runnerDistanceToPoint(player, poseTime,
        pickup.x, pickup.y, pickup.z) > 90) continue;
      if (pickup.type === "gun") player.gunAmmo = Math.min(12,
        player.gunAmmo + pickup.amount);
      else player.grenadeAmmo = Math.min(4, player.grenadeAmmo + pickup.amount);
      pickup.active = false;
      pickup.respawnAt = now + pickupRespawnUs;
      remember(player, pickup.type.toUpperCase() + " +" + pickup.amount);
      drum("clap", 1.1, player.pad === 0 ? -.4 : .4);
      emitSignal("pickup", player.pad, pickup.type === "gun" ? 1 : 2,
        pickup.amount);
      break;
    }
  }
}

function updatePlayer(player, pad, dt, now) {
  if (!player.alive) {
    player.previous = pad.down.slice();
    if (now >= player.respawnAt) {
      player.x = player.spawnX;
      player.y = floorY;
      player.z = 0;
      player.vx = 0;
      player.vy = 0;
      player.vz = 0;
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

  if (player.inputX && input.horizontal !== player.inputX)
    player.lastRelease[player.inputX > 0 ? "RIGHT" : "LEFT"] = now;
  if (player.inputY && input.vertical !== player.inputY)
    player.lastRelease[player.inputY > 0 ? "UP" : "DOWN"] = now;
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

  if (upPressed) {
    player.vy = Math.min(player.vy, -1050);
    player.grounded = false;
    player.ducking = false;
    drum("block", 0.72, player.pad === 0 ? -0.4 : 0.4);
    emitSignal("jump", player.pad, 1, 0);
  }

  for (const button of pad.down) {
    if (!player.previous.includes(button)) {
      remember(player, button);
      playButtonDrum(button, player);
      if (button === "A") {
        if (player.gunAmmo > 0) fire(player, input);
        else startMelee(player, "KICK", now);
      }
      else if (button === "B") {
        if (player.grenadeAmmo > 0) throwGrenade(player);
        else startMelee(player, "PUNCH", now);
      }
    }
  }
  if (player.pendingMoveLabel) remember(player, player.pendingMoveLabel);

  const previousY = player.y;
  player.vy += 1900 * dt;
  player.x += player.vx * dt;
  player.y += player.vy * dt;
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
  resolveRunnerBounds(player, (now - startedAt) / 1000000);
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
  resolveMelee(now);
  updatePickups(now);
  updateCamera(dt);

  for (const bullet of bullets) {
    bullet.x += bullet.vx * dt;
    bullet.y += bullet.vy * dt;
    bullet.life -= dt;
    if (bullet.x - 24 <= worldLeft + wallThickness ||
        bullet.x + 24 >= worldRight - wallThickness ||
        bullet.y - 24 <= ceilingY + wallThickness ||
        bullet.y + 24 >= floorY - wallThickness) bullet.life = 0;
  }

  for (let left = 0; left < bullets.length; left++) {
    const a = bullets[left];
    if (a.life <= 0) continue;
    for (let right = left + 1; right < bullets.length; right++) {
      const b = bullets[right];
      if (b.life <= 0 || a.owner === b.owner) continue;
      if (Math.abs(a.x - b.x) <= 96 && Math.abs(a.y - b.y) <= 48 &&
          Math.abs(a.z - b.z) <= 48) {
        a.life = 0;
        b.life = 0;
        impacts.push({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2,
          z: (a.z + b.z) / 2,
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
    if (runnerDistanceToPoint(target, poseTime,
      bullet.x, bullet.y, bullet.z) <= 24) {
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
          player, poseTime, grenade.x, grenade.y, grenade.z);
        if (player.alive && geometryDistance <= grenade.blastRadius)
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
    const grenadeWallInset = wallThickness + 35;
    if (grenade.x < worldLeft + grenadeWallInset) {
      grenade.x = worldLeft + grenadeWallInset;
      grenade.vx = Math.abs(grenade.vx) * .65;
    } else if (grenade.x > worldRight - grenadeWallInset) {
      grenade.x = worldRight - grenadeWallInset;
      grenade.vx = -Math.abs(grenade.vx) * .65;
    }
    if (grenade.y < ceilingY + wallThickness + 35) {
      grenade.y = ceilingY + wallThickness + 35;
      grenade.vy = Math.abs(grenade.vy) * .65;
    }
    if (grenade.vy >= 0 && previousY <= platformY - 30 &&
        grenade.y >= platformY - 30 && grenade.x >= platformLeft &&
        grenade.x <= platformRight) {
      grenade.y = platformY - 30;
      grenade.vy = -Math.abs(grenade.vy) * .55;
      grenade.vx *= .82;
    } else if (grenade.y >= floorY - 30) {
      grenade.y = floorY - 30;
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
  if (players.some((player) => !player.alive) || roundElapsedUs >= roundDurationUs)
    finishRound(now);
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

function runnerWorldGeometry(player, t) {
  const speed = Math.min(1, Math.abs(player.vx) / 1500);
  const idle = player.grounded && !player.ducking && speed < .03;
  const breath = idle ? Math.sin(t * 2.4 + player.pad * .7) * 5 : 0;
  const idleSway = idle ? Math.sin(t * 1.55 + player.pad) * 7 : 0;
  const stride = Math.sin(t * (7 + speed * 9) + player.pad * Math.PI) * 32 * speed;
  const height = player.ducking ? 108 : 180;
  const lean = player.facing * speed * 12;
  const x = player.x;
  const feet = player.y;
  const z = player.z;
  const hipY = feet - (player.ducking ? 40 : 58);
  const neckX = x + lean;
  const neckY = feet - height + 54 - breath;
  const attackPulse = meleePulse(player, runtime().monotonicUs);
  const head = { x: neckX + lean * .2, y: feet - height + 22 - breath * 1.6,
    z, radius: 22 };
  const segments = [];
  const segment = (x1, y1, x2, y2, width) =>
    segments.push({ x1, y1, z1: z, x2, y2, z2: z, width });
  segment(neckX, neckY, x, hipY, 10);
  if (player.attackKind === "KICK" && attackPulse > 0) {
    const footX = x + player.facing * (70 + 150 * attackPulse);
    segment(x, hipY, x + player.facing * 88, feet - 76, 12);
    segment(x + player.facing * 88, feet - 76, footX, feet - 55, 12);
    segment(x, hipY, x - player.facing * 28, feet - 32, 10);
    segment(x - player.facing * 28, feet - 32, x - player.facing * 8, feet, 10);
  } else if (player.ducking) {
    segment(x, hipY, x - 36, feet - 22, 10);
    segment(x - 36, feet - 22, x - 4, feet, 10);
    segment(x, hipY, x + 36, feet - 22, 10);
    segment(x + 36, feet - 22, x + 58, feet, 10);
  } else if (player.grounded) {
    segment(x, hipY, x - 11 + stride * .45, feet - 29, 10);
    segment(x - 11 + stride * .45, feet - 29, x + stride, feet, 10);
    segment(x, hipY, x + 11 - stride * .45, feet - 29, 10);
    segment(x + 11 - stride * .45, feet - 29, x - stride, feet, 10);
  } else {
    segment(x, hipY, x - 32, feet - 32, 10);
    segment(x - 32, feet - 32, x - 7, feet - 11, 10);
    segment(x, hipY, x + 32, feet - 43, 10);
    segment(x + 32, feet - 43, x + 50, feet - 22, 10);
  }
  const arm = idle ? idleSway : player.grounded ? -stride * .7 : 12;
  const elbowY = feet - (player.ducking ? 76 : 94) - breath;
  const handY = feet - (player.ducking ? 50 : 65) - breath * .5;
  if (player.attackKind === "PUNCH" && attackPulse > 0) {
    const handX = x + player.facing * (70 + 150 * attackPulse);
    const punchY = feet - 115;
    segment(neckX, neckY + 11,
      x + player.facing * (42 + 48 * attackPulse), punchY + 12, 12);
    segment(x + player.facing * (42 + 48 * attackPulse), punchY + 12,
      handX, punchY, 12);
    segment(neckX, neckY + 11, x - player.facing * 28, elbowY, 10);
    segment(x - player.facing * 28, elbowY,
      x - player.facing * 12, handY, 10);
  } else {
    segment(neckX, neckY + 11, x - 25 + arm * .65, elbowY, 10);
    segment(x - 25 + arm * .65, elbowY, x - 11 + arm * .65, handY, 10);
    segment(neckX, neckY + 11, x + 25 - arm * .65, elbowY, 10);
    segment(x + 25 - arm * .65, elbowY, x + 11 - arm * .65, handY, 10);
  }
  return { head, segments };
}

function runnerGeometry(player, t) {
  const world = runnerWorldGeometry(player, t);
  const headPoint = projectPoint(world.head.x, world.head.y, world.head.z);
  return {
    head: { x: headPoint.x, y: headPoint.y,
      radius: Math.max(1.5, world.head.radius * cameraScale()) },
    segments: world.segments.map((segment) => {
      const a = projectPoint(segment.x1, segment.y1, segment.z1);
      const b = projectPoint(segment.x2, segment.y2, segment.z2);
      return { x1: a.x, y1: a.y, x2: b.x, y2: b.y,
        width: Math.max(1.5, segment.width * cameraScale()) };
    }),
  };
}

function runnerBounds(player, t) {
  const geometry = runnerWorldGeometry(player, t);
  let left = geometry.head.x - geometry.head.radius;
  let right = geometry.head.x + geometry.head.radius;
  let top = geometry.head.y - geometry.head.radius;
  let bottom = geometry.head.y + geometry.head.radius;
  for (const segment of geometry.segments) {
    const radius = segment.width / 2;
    left = Math.min(left, segment.x1 - radius, segment.x2 - radius);
    right = Math.max(right, segment.x1 + radius, segment.x2 + radius);
    top = Math.min(top, segment.y1 - radius, segment.y2 - radius);
    bottom = Math.max(bottom, segment.y1 + radius, segment.y2 + radius);
  }
  return { left, right, top, bottom };
}

function resolveRunnerBounds(player, t) {
  let bounds = runnerBounds(player, t);
  const leftWall = worldLeft + wallThickness;
  const rightWall = worldRight - wallThickness;
  if (bounds.left < leftWall) {
    player.x += leftWall - bounds.left;
    player.vx = Math.max(0, player.vx);
    bounds = runnerBounds(player, t);
  }
  if (bounds.right > rightWall) {
    player.x -= bounds.right - rightWall;
    player.vx = Math.min(0, player.vx);
    bounds = runnerBounds(player, t);
  }
  const ceiling = ceilingY + wallThickness;
  if (bounds.top < ceiling) {
    player.y += ceiling - bounds.top;
    if (player.vy < 0) player.vy = 0;
  }
}

function pointSegmentDistance(px, py, pz, segment) {
  const dx = segment.x2 - segment.x1;
  const dy = segment.y2 - segment.y1;
  const dz = segment.z2 - segment.z1;
  const lengthSquared = dx * dx + dy * dy + dz * dz;
  const amount = lengthSquared > 0
    ? Math.max(0, Math.min(1,
      ((px - segment.x1) * dx + (py - segment.y1) * dy +
        (pz - segment.z1) * dz) / lengthSquared))
    : 0;
  return Math.hypot(px - (segment.x1 + dx * amount),
    py - (segment.y1 + dy * amount), pz - (segment.z1 + dz * amount));
}

function runnerDistanceToPoint(player, t, px, py, pz = 0) {
  const geometry = runnerWorldGeometry(player, t);
  let distance = Math.max(0,
    Math.hypot(px - geometry.head.x, py - geometry.head.y,
      pz - geometry.head.z) - geometry.head.radius);
  for (const segment of geometry.segments)
    distance = Math.min(distance,
      Math.max(0, pointSegmentDistance(px, py, pz, segment) - segment.width / 2));
  return distance;
}

function drawRunner(player, t) {
  if (!player.alive) return;
  const geometry = runnerGeometry(player, t);
  const color = player.hit > 0 ? [255, 255, 255] : player.color;
  circle(geometry.head.x, geometry.head.y, geometry.head.radius, 3, color);
  for (const segment of geometry.segments)
    line(segment.x1, segment.y1, segment.x2, segment.y2, segment.width, ...color);

  const labelSize = Math.max(10, Math.min(16, Math.round(cameraScale() * 48)));
  const labelX = geometry.head.x - player.name.length * labelSize * .3;
  systemWrite(player.name, labelX, geometry.head.y - labelSize - 8,
    labelSize, ...color);
}

function drawPlayerHud(player, x, pad) {
  const age = (runtime().monotonicUs - player.lastButtonAt) / 1000000;
  const pulse = Math.max(0, 1 - age * 2.2);
  const color = player.color;
  box(x, 210, 740, 112, 14 + Math.round(pulse * 20), 18, 45 + Math.round(pulse * 35));
  box(x, 210, 740, 8, ...color);
  systemWrite("P" + (player.pad + 1) + "  " + player.name + "  " +
    player.roundWins + "/" + matchWins, x + 24, 226, 30, ...color);
  systemWrite(player.lastButton, x + 24, 270, 28, ...color);
  const held = pad.down.length ? pad.down.map(buttonLabel).join(" ") : "NONE";
  systemWrite("KOS " + player.score + "  GUN " + player.gunAmmo +
    "  GREN " + player.grenadeAmmo, x + 330, 229, 17, ...color);
  systemWrite("HELD " + held, x + 330, 270, 15, 195, 210, 230);
  systemWrite(pad.connected ? "READY" : "CONNECT",
    x + 610, 230, 18, pad.connected ? 115 : 255, pad.connected ? 225 : 105,
    pad.connected ? 165 : 105);
}

function worldLine(x1, y1, z1, x2, y2, z2, width, color) {
  const a = projectPoint(x1, y1, z1);
  const b = projectPoint(x2, y2, z2);
  line(a.x, a.y, b.x, b.y, width, ...color);
}

function drawPickup(pickup, t) {
  if (!pickup.active) return;
  const bobY = pickup.y + Math.sin(t * 3 + pickup.x * .001) * 24;
  const point = projectPoint(pickup.x, bobY, pickup.z);
  if (point.x < stageLeft || point.x > stageRight ||
      point.y < stageTop || point.y > stageBottom) return;
  const scale = cameraScale();
  const color = pickup.type === "gun" ? [245, 248, 255] : [255, 232, 92];
  if (pickup.type === "gun") {
    worldLine(pickup.x - 62, bobY, pickup.z,
      pickup.x + 62, bobY, pickup.z, Math.max(3, 13 * scale), color);
    worldLine(pickup.x + 5, bobY, pickup.z,
      pickup.x + 35, bobY + 52, pickup.z, Math.max(3, 12 * scale), color);
  } else {
    circle(point.x, point.y, Math.max(4, 34 * scale),
      Math.max(2, 10 * scale), color);
    worldLine(pickup.x, bobY - 34, pickup.z,
      pickup.x + 28, bobY - 62, pickup.z, Math.max(2, 8 * scale), color);
  }
  const labelSize = Math.max(10, Math.min(18, Math.round(42 * scale)));
  systemWrite(pickup.type === "gun" ? "GUN" : "GREN",
    point.x - labelSize * 1.25, point.y - Math.max(18, 65 * scale),
    labelSize, ...color);
}

function paint() {
  const run = runtime();
  const t = (run.monotonicUs - startedAt) / 1000000;
  const titleX = 785 + Math.sin(t * 1.05) * 55;
  const titleY = 88 + Math.sin(t * 2.1) * 5;
  wipe(7, 8, 28);
  for (let i = 0; i < 8; i++) {
    const x = ((i * 310 + t * (38 + i * 3)) % 2300) - 190;
    box(x, 0, 5, 1080, 20 + i * 3, 32 + i * 4, 72 + i * 6);
  }
  box(titleX - 14, titleY - 13, 365, 74, 22, 28, 104);
  write("OSKIEWAR", titleX, titleY, 48, 245, 248, 255);

  box(35, 202, 1850, stageBottom - 202, 10, 13, 30);
  const worldInk = [72, 90, 125];
  const gridInk = [35, 49, 82];
  const edgeWidth = Math.max(2, wallThickness * cameraScale() * .14);
  for (let x = worldLeft; x <= worldRight; x += 2000)
    worldLine(x, floorY, worldNear, x, floorY, worldFar, 2, gridInk);
  for (let z = worldNear; z <= worldFar; z += 600)
    worldLine(worldLeft, floorY, z, worldRight, floorY, z, 2, gridInk);
  for (const z of [worldNear, worldFar]) {
    worldLine(worldLeft, ceilingY, z, worldRight, ceilingY, z, edgeWidth, worldInk);
    worldLine(worldLeft, floorY, z, worldRight, floorY, z, edgeWidth, worldInk);
    worldLine(worldLeft, ceilingY, z, worldLeft, floorY, z, edgeWidth, worldInk);
    worldLine(worldRight, ceilingY, z, worldRight, floorY, z, edgeWidth, worldInk);
  }
  for (const x of [worldLeft, worldRight]) {
    worldLine(x, ceilingY, worldNear, x, ceilingY, worldFar, edgeWidth, worldInk);
    worldLine(x, floorY, worldNear, x, floorY, worldFar, edgeWidth, worldInk);
  }
  const platformNear = -520;
  const platformFar = 520;
  worldLine(platformLeft, platformY, platformNear,
    platformRight, platformY, platformNear, 5, worldInk);
  worldLine(platformLeft, platformY, platformFar,
    platformRight, platformY, platformFar, 5, worldInk);
  worldLine(platformLeft, platformY, platformNear,
    platformLeft, platformY, platformFar, 5, worldInk);
  worldLine(platformRight, platformY, platformNear,
    platformRight, platformY, platformFar, 5, worldInk);
  for (const pickup of pickups) drawPickup(pickup, t);
  const remaining = roundResult ? 0 : Math.max(0,
    Math.ceil((roundDurationUs - roundElapsedUs) / 1000000));
  const clockInk = remaining <= 10 ? [255, 105, 190] : [245, 248, 255];
  box(850, 218, 220, 78, 22, 28, 104);
  write(String(remaining).padStart(2, "0"), 912, 226, 56, ...clockInk);
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const color = players[bullet.owner].color;
    const point = projectPoint(bullet.x, bullet.y, bullet.z);
    const speed = Math.hypot(bullet.vx, bullet.vy) || 1;
    const tail = projectPoint(bullet.x - bullet.vx / speed * 120,
      bullet.y - bullet.vy / speed * 120, bullet.z);
    const radius = Math.max(3, 24 * cameraScale());
    line(tail.x, tail.y, point.x, point.y, Math.max(2, radius * .7), ...color);
    box(point.x - radius, point.y - radius, radius * 2, radius * 2, ...color);
  }
  for (const grenade of grenades) {
    if (!grenade.alive) continue;
    const point = projectPoint(grenade.x, grenade.y, grenade.z);
    if (grenade.exploding) {
      const radius = grenade.blastRadius * cameraScale();
      circle(point.x, point.y, radius, 6, [255, 232, 92]);
      line(point.x - radius, point.y, point.x + radius, point.y,
        3, 255, 105, 190);
      line(point.x, point.y - radius, point.x, point.y + radius,
        3, 255, 255, 255);
      continue;
    }
    const blink = grenade.fuse < .45 && Math.floor(grenade.fuse * 20) % 2 === 0;
    const color = blink ? [255, 255, 255] : players[grenade.owner].color;
    const radius = Math.max(3, 30 * cameraScale());
    box(point.x - radius, point.y - radius, radius * 2, radius * 2, ...color);
    const tail = projectPoint(grenade.x - Math.sign(grenade.vx) * 90,
      grenade.y - 14, grenade.z);
    line(tail.x, tail.y, point.x, point.y, Math.max(2, radius * .5), ...color);
  }
  for (const impact of impacts) {
    const point = projectPoint(impact.x, impact.y, impact.z || 0);
    const radius = (30 + (1 - impact.life / impact.duration) *
      (impact.explosion ? 420 : impact.death ? 260 : 100)) * cameraScale();
    line(point.x - radius, point.y, point.x + radius, point.y,
      5, 255, 255, 255);
    line(point.x, point.y - radius, point.x, point.y + radius,
      5, 255, 255, 255);
    line(point.x - radius * .7, point.y - radius * .7,
      point.x + radius * .7, point.y + radius * .7, 4, 255, 232, 92);
    line(point.x + radius * .7, point.y - radius * .7,
      point.x - radius * .7, point.y + radius * .7, 4, 255, 105, 190);
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
  drawPlayerHud(players[0], 55, padSnapshots[0]);
  drawPlayerHud(players[1], 1125, padSnapshots[1]);
}

function act() {}
function leave() {}
