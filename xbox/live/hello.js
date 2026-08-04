const floorY = 12000;
const ceilingY = 0;
const wallThickness = 80;
const worldLeft = 0;
const worldRight = 12000;
const worldNear = -1800;
const worldFar = 1800;
const stageLeft = 0;
const stageRight = 1920;
const stageTop = 112;
const stageBottom = 1032;
const cameraAspect = (stageRight - stageLeft) / (stageBottom - stageTop);
const platformLeft = 4500;
const platformRight = 7500;
const platformY = 10400;
const doubleTapUs = 280000;
const doubleTapReleaseUs = 40000;
const roundDurationUs = 30000000;
const roundResultUs = 3000000;
const matchResultUs = 5000000;
const introDurationUs = 3000000;
const matchWins = 5;
let cameraCenter = (worldLeft + worldRight) / 2;
let cameraWidth = worldRight - worldLeft;
let cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
const mixColor = (dark, light, amount) => dark.map((value, index) =>
  Math.round(value + (light[index] - value) * amount));
const lerp = (from, to, amount) => from + (to - from) * amount;
const normalize3 = (point) => {
  const length = Math.hypot(point.x, point.y, point.z) || 1;
  return { x: point.x / length, y: point.y / length, z: point.z / length };
};
const cross3 = (a, b) => ({ x: a.y * b.z - a.z * b.y,
  y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x });
const dot3 = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z;

class FightCamDoll {
  constructor() {
    this.position = { x: cameraCenter, y: cameraCenterY, z: -cameraWidth * 1.4 };
    this.target = { x: cameraCenter, y: cameraCenterY, z: 0 };
    this.width = cameraWidth;
    this.perspective = 0;
    this.fov = 55;
    this.dirty = true;
    this.view = null;
  }

  track(spec, dt, speed = 5) {
    const amount = Math.min(1, dt * speed);
    for (const axis of ["x", "y", "z"]) {
      this.position[axis] = lerp(this.position[axis], spec.position[axis], amount);
      this.target[axis] = lerp(this.target[axis], spec.target[axis], amount);
    }
    this.width = lerp(this.width, spec.width, amount);
    this.perspective = lerp(this.perspective, spec.perspective, amount);
    this.fov = lerp(this.fov, spec.fov || 55, amount);
    this.dirty = true;
  }

  prepare() {
    const forward = normalize3({ x: this.target.x - this.position.x,
      y: this.target.y - this.position.y, z: this.target.z - this.position.z });
    const right = normalize3(cross3(forward, { x: 0, y: -1, z: 0 }));
    const up = normalize3(cross3(right, forward));
    this.view = { forward, right, up,
      centerX: (stageLeft + stageRight) / 2,
      centerY: (stageTop + stageBottom) / 2,
      orthoScale: (stageRight - stageLeft) / this.width,
      focal: (stageRight - stageLeft) /
        (2 * Math.tan(this.fov * Math.PI / 360)) };
    this.dirty = false;
  }

  project(point) {
    if (this.dirty || !this.view) this.prepare();
    const { forward, right, up, centerX, centerY, orthoScale, focal } = this.view;
    const delta = { x: point.x - this.position.x, y: point.y - this.position.y,
      z: point.z - this.position.z };
    const viewX = dot3(delta, right);
    const viewY = dot3(delta, up);
    const viewZ = Math.max(80, dot3(delta, forward));
    const orthoX = centerX + viewX * orthoScale;
    const orthoY = centerY - viewY * orthoScale;
    const perspectiveX = centerX + viewX * focal / viewZ;
    const perspectiveY = centerY - viewY * focal / viewZ;
    return { x: lerp(orthoX, perspectiveX, this.perspective),
      y: lerp(orthoY, perspectiveY, this.perspective) };
  }
}

const cameraDoll = new FightCamDoll();
const cameraScale = () => (stageRight - stageLeft) / cameraDoll.width;
function projectPoint(x, y, z = 0) {
  return cameraDoll.project({ x, y, z });
}
const screenX = (x, z = 0) => projectPoint(x, cameraCenterY, z).x;
const screenY = (y, z = 0) => projectPoint(cameraCenter, y, z).y;
const panAt = (x, z = 0) => clamp(
  (projectPoint(x, cameraCenterY, z).x - 960) / 905, -1, 1);
const panPlayer = (player) => panAt(player.x, player.z);
let visualTheme = { light: 0, sunset: 0 };

const fighterRoster = [
  { handle: "@JEFFREY", color: [255, 105, 190], colors: [
    [255,105,190],[111,232,210],[255,105,190],[255,232,92],
    [130,150,255],[255,105,190],[111,232,210],[255,232,92]],
    mood: "New Media Instruments",
    lastChat: "hmm this chat could use some ui love" },
  { handle: "@FIFI", color: [209, 100, 216], colors: [
    [209,100,216],[204,253,71],[35,100,255],[82,67,17],[5,249,137]],
    mood: "nap", lastChat: "" },
  { handle: "@OSKIE", color: [255, 232, 92], colors: [
    [255,232,92],[255,105,190],[111,232,210],[255,232,92],[130,150,255],[255,105,190]],
    mood: "", lastChat: "" },
  { handle: "@SAT", color: [130, 204, 213], colors: [
    [130,204,213],[161,232,0],[253,122,2],[222,90,205]],
    mood: "i hope its not ai generated", lastChat: "meow" },
];
const npcFighter = { handle: "DUMMY", color: [105, 125, 150],
  colors: [[105,125,150],[135,155,180],[105,125,150]],
  mood: "TRAINING DUMMY · NO BOT AI", lastChat: "" };

function losAngelesSun() {
  const radians = Math.PI / 180;
  const days = Date.now() / 86400000 + 2440587.5 - 2451545;
  const meanLongitude = (280.46 + .9856474 * days) % 360;
  const meanAnomaly = (357.528 + .9856003 * days) % 360;
  const eclipticLongitude = meanLongitude + 1.915 * Math.sin(meanAnomaly * radians) +
    .02 * Math.sin(2 * meanAnomaly * radians);
  const obliquity = 23.439 - .0000004 * days;
  const rightAscension = Math.atan2(Math.cos(obliquity * radians) *
    Math.sin(eclipticLongitude * radians), Math.cos(eclipticLongitude * radians)) /
    radians;
  const declination = Math.asin(Math.sin(obliquity * radians) *
    Math.sin(eclipticLongitude * radians));
  const siderealHours = 18.697374558 + 24.06570982441908 * days;
  let hourAngle = ((siderealHours * 15 - 118.2437 - rightAscension + 540) % 360) - 180;
  const latitude = 34.0522 * radians;
  const altitude = Math.asin(Math.sin(latitude) * Math.sin(declination) +
    Math.cos(latitude) * Math.cos(declination) *
    Math.cos(hourAngle * radians)) / radians;
  const light = clamp((altitude + 6) / 14, 0, 1);
  const sunset = clamp(1 - Math.abs(altitude - 1) / 11, 0, 1) *
    (hourAngle > 0 ? 1 : .42);
  return { light: light * light * (3 - 2 * light), sunset };
}
const players = [
  { name: "@JEFFREY", rosterIndex: 0, handleColors: fighterRoster[0].colors,
    pad: 0, spawnX: 2000, x: 2000, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: 1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 105, 190], hit: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, roundWins: 0,
    attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false, blocking: false, blockFlash: 0,
    windVx: 0, knockVx: 0 },
  { name: "DUMMY", rosterIndex: -1, handleColors: npcFighter.colors, npc: true,
    pad: 1, spawnX: 10000, x: 10000, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: -1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 232, 92], hit: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, roundWins: 0,
    attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false, blocking: false, blockFlash: 0,
    windVx: 0, knockVx: 0 },
];
const impacts = [];
const ball = { x: 6000, y: floorY - 55, z: 0, vx: 0, vy: 0,
  radius: 55, active: true, serveAt: 0, lastHitBy: -1, safeUntil: 0,
  safePlayers: 0 };
let ballEnabled = true;
let padSnapshots = [null, null];
let startedAt = 0;
let roundStartedAt = 0;
let lastSimAt = 0;
let roundElapsedUs = 0;
let roundOverAt = 0;
let roundResult = "";
let matchOver = false;
let roundCause = "";
let acFeed = {};
let selecting = true;
const selectionReady = [false, false];
const selectionPrevious = [[], []];
let windMph = 0;
let windDirection = 1;
let windAcceleration = 0;

function emitSignal(event, player = -1, value = 0, value2 = 0) {
  if (typeof gameSignal === "function") gameSignal(event, player, value, value2);
}

function fighterProfile(handle) {
  const live = Array.isArray(acFeed.fighters)
    ? acFeed.fighters.find((profile) => profile.handle.toUpperCase() === handle.toUpperCase())
    : null;
  const fallback = handle === "DUMMY" ? npcFighter
    : fighterRoster.find((profile) => profile.handle === handle);
  return {
    mood: live?.mood || (handle === "@JEFFREY" && acFeed.moodHandle === "@jeffrey"
      ? acFeed.mood : "") || fallback?.mood || "",
    lastChat: live?.lastChat || fallback?.lastChat || "",
    colors: live?.colors?.length
      ? live.colors.map((color) => [color.r, color.g, color.b])
      : fallback?.colors || [],
  };
}

function applyRoster(player, index) {
  if (player.npc) {
    player.rosterIndex = -1;
    player.name = npcFighter.handle;
    player.color = npcFighter.color.slice();
    player.handleColors = npcFighter.colors;
    return;
  }
  const rosterIndex = (index + fighterRoster.length) % fighterRoster.length;
  const fighter = fighterRoster[rosterIndex];
  const profile = fighterProfile(fighter.handle);
  player.rosterIndex = rosterIndex;
  player.name = fighter.handle;
  player.color = fighter.color.slice();
  player.handleColors = profile.colors;
}

function beginSelect(now) {
  selecting = true;
  selectionReady[0] = false;
  selectionReady[1] = true;
  selectionPrevious[0] = [];
  selectionPrevious[1] = [];
  roundResult = "";
  roundCause = "";
  matchOver = false;
  roundElapsedUs = 0;
  roundStartedAt = now;
  for (const player of players) {
    player.roundWins = 0;
    player.score = 0;
    player.alive = true;
  }
}

function updateSelect(now) {
  for (const player of players) {
    const down = padSnapshots[player.pad]?.down || [];
    if (player.npc) {
      selectionPrevious[player.pad] = down.slice();
      continue;
    }
    const previous = selectionPrevious[player.pad];
    const pressed = (button) => down.includes(button) && !previous.includes(button);
    if (pressed("B") && selectionReady[player.pad]) selectionReady[player.pad] = false;
    if (!selectionReady[player.pad] && (pressed("ArrowLeft") || pressed("ArrowRight"))) {
      applyRoster(player, player.rosterIndex + (pressed("ArrowRight") ? 1 : -1));
      drum("hat", .8, player.pad === 0 ? -.65 : .65);
    }
    if (!selectionReady[player.pad] && pressed("A")) {
      selectionReady[player.pad] = true;
      drum("clap", 1, player.pad === 0 ? -.65 : .65);
    }
    selectionPrevious[player.pad] = down.slice();
  }
  if (selectionReady[0] && selectionReady[1]) {
    selecting = false;
    resetRound(now, true);
    emitSignal("fighters", -1, players[0].rosterIndex, players[1].rosterIndex);
  }
}

function rollWind() {
  windMph = 4 + Math.floor(Math.random() * 21);
  windDirection = Math.random() < .5 ? -1 : 1;
  windAcceleration = windDirection * windMph * 16;
  emitSignal("wind", -1, windDirection, windMph);
}

function resetBall(now) {
  const target = Math.random() < .5 ? 0 : 1;
  ball.x = (worldLeft + worldRight) / 2;
  ball.y = floorY - ball.radius;
  ball.z = 0;
  ball.vx = target === 0 ? -900 : 900;
  ball.vy = 0;
  ball.active = ballEnabled;
  ball.serveAt = now + introDurationUs + 850000;
  ball.lastHitBy = target === 0 ? 1 : 0;
  ball.safeUntil = ball.serveAt;
  ball.safePlayers = 1 << ball.lastHitBy;
  emitSignal("ballserve", -1, target, windMph);
}

const buttonLabel = (button) => ({
  ArrowUp: "UP", ArrowDown: "DOWN", ArrowLeft: "LEFT", ArrowRight: "RIGHT",
  LeftShoulder: "LB", RightShoulder: "RB", LeftStick: "LEFT STICK",
  RightStick: "RIGHT STICK", View: "VIEW", Menu: "MENU",
}[button] || String(button).toUpperCase());

function boot() {
  startedAt = runtime().monotonicUs;
  roundStartedAt = startedAt;
  lastSimAt = startedAt;
  roundElapsedUs = 0;
  emitSignal("hello", -1, 1, 0);
  beginSelect(startedAt);
}

function resetRound(now, resetMatch = false) {
  impacts.length = 0;
  for (const player of players) {
    applyRoster(player, player.rosterIndex);
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
    player.attackKind = "";
    player.attackUntil = 0;
    player.attackHit = false;
    player.blocking = false;
    player.blockFlash = 0;
    player.windVx = 0;
    player.knockVx = 0;
    player.previous = padSnapshots[player.pad]?.down?.slice() || [];
    player.lastButton = "NONE";
    player.lastButtonAt = -10000000;
  }
  roundResult = "";
  roundCause = "";
  matchOver = false;
  roundElapsedUs = 0;
  lastSimAt = now;
  roundStartedAt = now;
  rollWind();
  resetBall(now);
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

function updateCameraDoll(dt, now) {
  const introAge = now - roundStartedAt;
  if (roundResult) {
    const victim = players.find((player) => !player.alive);
    const age = Math.max(0, (now - roundOverAt) / 1000000);
    if (!victim) {
      const target = { x: (players[0].x + players[1].x) / 2,
        y: (players[0].y + players[1].y) / 2 - 90, z: 0 };
      cameraDoll.track({ target,
        position: { x: target.x + Math.sin(age) * 900,
          y: target.y - 160, z: -2200 },
        width: Math.abs(players[1].x - players[0].x) + 1100,
        perspective: clamp(age / .7, 0, .8), fov: 52 }, dt, 6);
      return;
    }
    const angle = age * 1.35;
    const radius = 720;
    const target = { x: victim.x, y: victim.y - 95, z: victim.z };
    cameraDoll.track({ target,
      position: { x: target.x + Math.sin(angle) * radius,
        y: target.y - 80 + Math.sin(age * .8) * 55,
        z: target.z - Math.cos(angle) * radius },
      width: 680, perspective: clamp(age / .55, 0, 1), fov: 48 }, dt, 7);
    return;
  }
  if (introAge < introDurationUs) {
    const age = introAge / 1000000;
    if (age < 1.9) {
      const index = age < .95 ? 0 : 1;
      const local = (age % .95) / .95;
      const player = players[index];
      const target = { x: player.x, y: player.y - 92, z: player.z };
      const angle = (index === 0 ? -.72 : .72) + (local - .5) * .42;
      cameraDoll.track({ target,
        position: { x: target.x + Math.sin(angle) * 820,
          y: target.y - 55, z: target.z - Math.cos(angle) * 820 },
        width: 650, perspective: clamp(local / .42, 0, 1), fov: 46 }, dt, 10);
    } else {
      const target = { x: (players[0].x + players[1].x) / 2,
        y: (players[0].y + players[1].y) / 2 - 90, z: 0 };
      const span = Math.abs(players[1].x - players[0].x) + 900;
      cameraDoll.track({ target,
        position: { x: target.x, y: target.y - 180, z: -span * .62 },
        width: span, perspective: 1, fov: 53 }, dt, 8);
    }
    return;
  }
  const target = { x: cameraCenter, y: cameraCenterY, z: 0 };
  cameraDoll.track({ target,
    position: { x: cameraCenter - cameraWidth * .06,
      y: cameraCenterY - cameraWidth * .04, z: -cameraWidth * 1.35 },
    width: cameraWidth, perspective: 0, fov: 55 }, dt, 4.5);
}

function finishRound(now) {
  if (roundResult) return;
  let roundPan = 0;
  if (players[0].score === players[1].score) {
    roundResult = "TIE";
    emitSignal("tie", -1, players[0].score, players[1].score);
  }
  else {
    const winner = players[0].score > players[1].score ? players[0] : players[1];
    roundPan = panPlayer(winner);
    winner.roundWins += 1;
    matchOver = winner.roundWins >= matchWins;
    roundResult = winner.name + (matchOver ? " WINS MATCH" : " WINS ROUND");
    emitSignal(matchOver ? "matchwin" : "roundwin", winner.pad,
      winner.roundWins, winner.score);
  }
  roundOverAt = now;
  for (const player of players) player.vx = 0;
  drum("clap", 1.2, roundPan);
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
  const pan = panPlayer(player);
  if (button === "Y") drum("clap", 0.95, pan);
  else if (button !== "A" && button !== "B" && button !== "X" &&
      !button.startsWith("Arrow"))
    drum("block", 0.75, pan);
}

function startMelee(player, kind, now) {
  player.attackKind = kind;
  player.attackStartedAt = now;
  player.attackUntil = now + 220000;
  player.attackHit = false;
  player.pendingMoveLabel = kind;
  const pan = panPlayer(player);
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

function returnBall(player, now, shielded, intensity = 1) {
  const incomingVx = ball.vx;
  const incomingVy = ball.vy;
  const direction = ball.x >= player.x ? 1 : -1;
  const currentSpeed = Math.hypot(ball.vx, ball.vy);
  const speed = Math.min(4800, (currentSpeed * (shielded ? 1.05 : 1.34) +
    (shielded ? 120 : 720)) * intensity);
  ball.vx = direction * speed;
  const lift = player.inputY > 0 ? .58
    : shielded ? .14 : player.attackKind === "KICK" ? .34 : .2;
  ball.vy = -speed * lift;
  ball.x = player.x + direction * (ball.radius + 85);
  ball.y = Math.min(ball.y, player.y - 55);
  ball.lastHitBy = player.pad;
  ball.safeUntil = now + 140000;
  ball.safePlayers = 1 << player.pad;
  if (!shielded) {
    player.attackHit = true;
    player.lastButton = "WACK";
    player.lastButtonAt = now;
  }
  else {
    player.blockFlash = 1;
    player.knockVx += incomingVx * .55;
    player.vy = Math.min(player.vy + incomingVy * .16, -Math.max(520, speed * .28));
    player.grounded = false;
  }
  impacts.push({ x: ball.x, y: ball.y, z: ball.z,
    life: .22, duration: .22, death: false, explosion: false });
  drum(shielded ? "block" : "clap", shielded ? 1.1 : 1.25,
    panAt(ball.x, ball.z));
  emitSignal(shielded ? "ballblock" : "wack", player.pad,
    direction, Math.round(speed));
}

function crossWackBall(hitters, now) {
  const first = hitters[0];
  const second = hitters[1];
  const fighterDistance = Math.abs(first.player.x - second.player.x);
  const convergence = clamp(1 - fighterDistance / 520, 0, 1);
  const contact = (first.contact + second.contact) * .5;
  const currentSpeed = Math.hypot(ball.vx, ball.vy);
  const speed = Math.min(7600, 4100 + currentSpeed * .72 +
    convergence * 2300 + contact * 900);
  const stronger = first.contact >= second.contact ? first.player : second.player;
  const direction = Math.sign(ball.vx) || stronger.facing || 1;
  ball.vx = direction * speed;
  ball.vy = -speed * (.3 + convergence * .18);
  ball.x += direction * 35;
  ball.y = Math.min(ball.y, floorY - ball.radius - 8);
  ball.lastHitBy = -1;
  ball.safeUntil = now + 180000;
  ball.safePlayers = 3;
  for (const hit of hitters) {
    hit.player.attackHit = true;
    hit.player.lastButton = "CROSS WACK";
    hit.player.lastButtonAt = now;
  }
  impacts.push({ x: ball.x, y: ball.y, z: ball.z,
    life: .32, duration: .32, death: false, explosion: true });
  drum("clap", 1.55, panAt(ball.x, ball.z));
  emitSignal("crosswack", -1, direction, Math.round(speed));
}

function bootBall(player, now) {
  const direction = Math.sign(ball.x - player.x) || player.facing || 1;
  const speed = Math.min(3600, 1150 + Math.abs(player.vx) * .95);
  ball.vx = direction * speed;
  ball.vy = -Math.max(220, speed * .2);
  ball.x = player.x + direction * (ball.radius + 58);
  ball.y = Math.min(ball.y, floorY - ball.radius - 2);
  ball.lastHitBy = player.pad;
  ball.safeUntil = now + 180000;
  ball.safePlayers = 1 << player.pad;
  player.lastButton = "BOOT";
  player.lastButtonAt = now;
  impacts.push({ x: ball.x, y: ball.y, z: ball.z,
    life: .16, duration: .16, death: false, explosion: false });
  drum("kick", 1.12, panAt(ball.x, ball.z));
  emitSignal("boot", player.pad, direction, Math.round(speed));
}

function updateBall(dt, now) {
  if (!ball.active || now < ball.serveAt) return;
  const grounded = ball.y >= floorY - ball.radius - 1 && Math.abs(ball.vy) < 180;
  if (!grounded) ball.vx += windAcceleration * .45 * dt;
  ball.vy += 1900 * dt;
  ball.x += ball.vx * dt;
  ball.y += ball.vy * dt;
  const inset = wallThickness + ball.radius;
  if (ball.x < worldLeft + inset) {
    ball.x = worldLeft + inset;
    ball.vx = Math.abs(ball.vx);
  } else if (ball.x > worldRight - inset) {
    ball.x = worldRight - inset;
    ball.vx = -Math.abs(ball.vx);
  }
  if (ball.y < ceilingY + inset) {
    ball.y = ceilingY + inset;
    ball.vy = Math.abs(ball.vy);
  } else if (ball.y > floorY - ball.radius) {
    ball.y = floorY - ball.radius;
    ball.vy = Math.abs(ball.vy) > 180 ? -Math.abs(ball.vy) * .62 : 0;
    ball.vx *= .992;
  }
  const onFloor = ball.y >= floorY - ball.radius - 1 && Math.abs(ball.vy) < 180;
  const poseTime = (now - startedAt) / 1000000;
  const hitters = [];
  for (const player of players) {
    if (!player.alive || ((ball.safePlayers & (1 << player.pad)) &&
        now < ball.safeUntil))
      continue;
    if (!player.attackHit && now < player.attackUntil) {
      const strike = meleeStrike(player, now);
      const distance = Math.hypot(ball.x - strike.x, ball.y - strike.y,
        ball.z - strike.z);
      if (distance <= ball.radius + strike.radius)
        hitters.push({ player, contact: clamp(1 - distance /
          (ball.radius + strike.radius), 0, 1) });
    }
  }
  if (hitters.length >= 2) {
    crossWackBall(hitters, now);
    return;
  }
  if (hitters.length === 1) {
    returnBall(hitters[0].player, now, false, .92 + hitters[0].contact * .38);
    return;
  }
  for (const player of players) {
    if (!player.alive || ((ball.safePlayers & (1 << player.pad)) &&
        now < ball.safeUntil))
      continue;
    if (runnerDistanceToPoint(player, poseTime,
      ball.x, ball.y, ball.z) > ball.radius) continue;
    if (onFloor) {
      bootBall(player, now);
      return;
    }
    if (player.blocking) {
      returnBall(player, now, true);
      return;
    }
    else {
      ball.active = false;
      const scorer = ball.lastHitBy >= 0 && ball.lastHitBy !== player.pad
        ? ball.lastHitBy : player.pad === 0 ? 1 : 0;
      killPlayer(player, scorer, now, "BALLED");
      return;
    }
  }
}

function directionTap(player, direction, now) {
  const previousTap = player.lastTap[direction] || -10000000;
  const releasedAt = player.lastRelease[direction] || -10000000;
  player.lastTap[direction] = now;
  if (now - previousTap > doubleTapUs || releasedAt <= previousTap ||
      now - releasedAt < doubleTapReleaseUs) return;
  player.lastTap[direction] = -10000000;
  player.pendingMoveLabel = direction === "UP" ? "ULTRA JUMP" : "DASH " + direction;
  drum("clap", 1.05, panPlayer(player));
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

function killPlayer(target, killerPad, now, cause = "KO") {
  if (!target.alive) return;
  target.alive = false;
  target.respawnAt = now + 1200000;
  target.vx = 0;
  target.vy = 0;
  target.lastButton = cause;
  target.lastButtonAt = now;
  if (killerPad !== target.pad) players[killerPad].score += 1;
  emitSignal(cause === "BALLED" ? "balled" : "ko",
    killerPad, target.pad, players[killerPad]?.score || 0);
  roundCause = cause;
  impacts.push({ x: target.x, y: target.y - 120, z: target.z, life: .55,
    duration: .55, death: true, explosion: false });
  drum("snare", 1.15, panPlayer(target));
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
      const away = Math.sign(target.x - attacker.x) || -attacker.facing;
      const backBlocking = target.inputX === away;
      if (target.blocking || backBlocking) {
        target.blockFlash = 1;
        target.lastButton = target.blocking ? "BLOCK" : "BACK BLOCK";
        target.lastButtonAt = now;
        target.vx = 0;
        attacker.vx = -attacker.facing * 420;
        drum("block", 1.2, panPlayer(target));
        emitSignal("block", target.pad, attacker.pad, target.blocking ? 1 : 2);
      } else killPlayer(target, attacker.pad, now);
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
      player.windVx = 0;
      player.knockVx = 0;
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
  player.blocking = pad.down.includes("X");

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
  if (player.grounded) player.windVx *= Math.max(0, 1 - dt * 10);
  else player.windVx = clamp(player.windVx + windAcceleration * dt, -900, 900);
  player.knockVx *= Math.max(0, 1 - dt * (player.grounded ? 7 : 1.8));
  const controlledVx = player.blocking ? 0 : now < player.dashUntil
    ? player.dashVx
    : player.ducking ? 0 : input.horizontal * 1500;
  player.vx = controlledVx + player.windVx + player.knockVx;

  if (upPressed) {
    player.vy = Math.min(player.vy, -1050);
    player.grounded = false;
    player.ducking = false;
    drum("block", 0.72, panPlayer(player));
    emitSignal("jump", player.pad, 1, 0);
  }

  for (const button of pad.down) {
    if (!player.previous.includes(button)) {
      remember(player, button);
      playButtonDrum(button, player);
      if (button === "X") {
        player.pendingMoveLabel = "SHIELD";
        drum("block", .7, panPlayer(player));
        emitSignal("shield", player.pad, 1, 0);
      }
      else if (!player.blocking && button === "A") startMelee(player, "KICK", now);
      else if (!player.blocking && button === "B") startMelee(player, "PUNCH", now);
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
  player.blockFlash = Math.max(0, player.blockFlash - dt * 6);
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
    updateCameraDoll(dt, now);
    const resultDuration = matchOver ? matchResultUs : roundResultUs;
    if (now - roundOverAt >= resultDuration) {
      if (matchOver) beginSelect(now);
      else resetRound(now, false);
    }
    return;
  }
  if (selecting) {
    updateSelect(now);
    return;
  }
  if (now - roundStartedAt < introDurationUs) {
    updateCameraDoll(dt, now);
    return;
  }
  roundElapsedUs += dt * 1000000;
  updatePlayer(players[0], padSnapshots[0], dt, now);
  updatePlayer(players[1], players[1].npc
    ? { connected: true, down: [], leftX: 0, leftY: 0 }
    : padSnapshots[1], dt, now);
  updateBall(dt, now);
  resolveMelee(now);
  updateCamera(dt);
  updateCameraDoll(dt, now);
  for (const impact of impacts) impact.life -= dt;
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
    player.knockVx = Math.max(0, player.knockVx);
    bounds = runnerBounds(player, t);
  }
  if (bounds.right > rightWall) {
    player.x -= bounds.right - rightWall;
    player.vx = Math.min(0, player.vx);
    player.knockVx = Math.min(0, player.knockVx);
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

function handleWidth(handle, size) {
  return handle.length ? size * (handle[0] === "@" ? .88 : .58) +
    (handle.length - 1) * size * .58 : 0;
}

function typeWrite(text, x, y, size, ...color) {
  if (typeof ywftWrite === "function") ywftWrite(text, x, y, size, ...color);
  else systemWrite(text, x, y, size, ...color);
}

function drawHandle(handle, x, y, size, colors, fallback) {
  let cursor = x;
  for (let index = 0; index < handle.length; index++) {
    const color = colors?.[index] || fallback;
    typeWrite(handle[index], cursor, y, size, ...color);
    cursor += size * (handle[index] === "@" ? .88 : .58);
  }
}

function drawFace(player, head, color, t) {
  if (head.radius < 5) return;
  const r = head.radius;
  const direction = player.facing || 1;
  const eyeY = head.y - r * .18;
  const eyeGap = r * .34;
  const eyeWidth = Math.max(1.4, r * .1);
  const lineWidth = Math.max(1.2, r * .1);
  const blink = player.alive && !player.blocking && !player.attackKind &&
    Math.sin(t * .73 + player.pad * 2.1) > .985;
  if (!player.alive || player.hit > .6) {
    for (const offset of [-eyeGap, eyeGap]) {
      line(head.x + offset - eyeWidth, eyeY - eyeWidth,
        head.x + offset + eyeWidth, eyeY + eyeWidth, lineWidth, ...color);
      line(head.x + offset + eyeWidth, eyeY - eyeWidth,
        head.x + offset - eyeWidth, eyeY + eyeWidth, lineWidth, ...color);
    }
  } else if (player.blocking || blink) {
    line(head.x - eyeGap - eyeWidth, eyeY, head.x - eyeGap + eyeWidth,
      eyeY, lineWidth, ...color);
    line(head.x + eyeGap - eyeWidth, eyeY, head.x + eyeGap + eyeWidth,
      eyeY, lineWidth, ...color);
  } else {
    line(head.x - eyeGap, eyeY - eyeWidth, head.x - eyeGap,
      eyeY + eyeWidth, lineWidth, ...color);
    line(head.x + eyeGap, eyeY - eyeWidth, head.x + eyeGap,
      eyeY + eyeWidth, lineWidth, ...color);
  }
  const mouthY = head.y + r * .3;
  if (player.attackKind && meleePulse(player, runtime().monotonicUs) > 0) {
    circle(head.x + direction * r * .12, mouthY, Math.max(1.8, r * .13),
      lineWidth, color);
  } else if (player.blocking) {
    line(head.x - r * .26, mouthY, head.x + r * .26, mouthY,
      lineWidth, ...color);
  } else if (!player.alive || player.hit > .6) {
    line(head.x - r * .24, mouthY + r * .08, head.x,
      mouthY - r * .06, lineWidth, ...color);
    line(head.x, mouthY - r * .06, head.x + r * .24,
      mouthY + r * .08, lineWidth, ...color);
  } else {
    const smile = Math.sin(t * 2.4 + player.pad) * r * .035;
    line(head.x - r * .23, mouthY - smile, head.x,
      mouthY + r * .09, lineWidth, ...color);
    line(head.x, mouthY + r * .09, head.x + r * .23,
      mouthY - smile, lineWidth, ...color);
  }
}

function drawRunner(player, t, showLabel = true) {
  if (!player.alive && !roundResult) return;
  const geometry = runnerGeometry(player, t);
  const color = player.hit > 0 ? [255, 255, 255] : player.color;
  circle(geometry.head.x, geometry.head.y, geometry.head.radius, 3, color);
  drawFace(player, geometry.head, color, t);
  for (const segment of geometry.segments)
    line(segment.x1, segment.y1, segment.x2, segment.y2, segment.width, ...color);
  if (player.blocking) {
    const shield = projectPoint(player.x, player.y - 90, player.z);
    const radius = Math.max(18, 112 * cameraScale());
    const shieldColor = player.blockFlash > 0 ? [255, 255, 255] : player.color;
    circle(shield.x, shield.y, radius, Math.max(3, 9 * cameraScale()), shieldColor);
    circle(shield.x, shield.y, radius * .78,
      Math.max(2, 5 * cameraScale()), shieldColor);
    line(shield.x + player.facing * radius * .25, shield.y - radius * .72,
      shield.x + player.facing * radius * .72, shield.y + radius * .72,
      Math.max(2, 6 * cameraScale()), ...shieldColor);
  }

  if (showLabel) {
    const labelSize = Math.max(10, Math.min(16, Math.round(cameraScale() * 48)));
    const labelX = geometry.head.x - handleWidth(player.name, labelSize) / 2;
    drawHandle(player.name, labelX, geometry.head.y - labelSize - 8,
      labelSize, player.handleColors, color);
  }
}

function drawPlayerHud(player, x, pad) {
  pad = pad || { connected: false, down: [] };
  const age = (runtime().monotonicUs - player.lastButtonAt) / 1000000;
  const pulse = Math.max(0, 1 - age * 2.2);
  const color = visualTheme.light > .55
    ? player.pad === 0 ? [155, 34, 108] : [105, 78, 0]
    : player.color;
  line(x, 8, x + 625, 8, 5 + pulse * 4, ...player.color);
  typeWrite("P" + (player.pad + 1) + " " + player.name + "  " +
    player.roundWins + "/" + matchWins + "  PTS " + player.score,
    x, 14, 24, ...color);
  const held = pad.down.length ? pad.down.map(buttonLabel).join(" ") : "NONE";
  const secondary = mixColor([195, 210, 230], [48, 58, 78], visualTheme.light);
  const status = player.npc ? "DUMMY" : pad.connected ? "READY" : "CONNECT";
  typeWrite(player.lastButton + "  ·  " + held + "  ·  " + status,
    x, 48, 16, ...(status === "CONNECT" ? [255, 105, 105] : secondary));
}

function drawFighterData(player, x) {
  const profile = fighterProfile(player.name);
  const mood = profile.mood ? "M " + profile.mood.slice(0, 24) : "M —";
  const chat = profile.lastChat ? "CHAT " + profile.lastChat.slice(0, 34) : "CHAT —";
  const ink = mixColor([190, 205, 235], [55, 66, 90], visualTheme.light);
  typeWrite(mood + "  ·  " + chat, x, 82, 15, ...ink);
}

function worldLine(x1, y1, z1, x2, y2, z2, width, color) {
  const a = projectPoint(x1, y1, z1);
  const b = projectPoint(x2, y2, z2);
  line(a.x, a.y, b.x, b.y, width, ...color);
}

function drawWindFlag(t, color) {
  const poleX = 820;
  const poleTop = 18;
  const poleBottom = 72;
  const direction = windDirection;
  const length = 30 + windMph * 3;
  const gust = Math.sin(t * (4 + windMph * .16)) * (3 + windMph * .22);
  const tipX = poleX + direction * length;
  const tipY = poleTop + 10 + gust;
  line(poleX, poleTop, poleX, poleBottom, 3, ...color);
  line(poleX, poleTop + 2, tipX, tipY, 4, ...color);
  line(tipX, tipY, poleX, poleTop + 24, 4, ...color);
  line(poleX, poleTop + 24, poleX, poleTop + 2, 4, ...color);
  line(poleX - 8, poleBottom, poleX + 8, poleBottom, 3, ...color);
  const textX = direction < 0 ? 842 : 705;
  typeWrite(windMph + " MPH", textX, 38, 17, ...color);
}

function drawSelectPortrait(player, x, y, scale, t) {
  const color = player.color;
  const head = { x, y: y - 130 * scale, radius: 34 * scale };
  circle(head.x, head.y, head.radius, Math.max(3, 5 * scale), color);
  line(x, y - 94 * scale, x, y + 20 * scale, 12 * scale, ...color);
  line(x, y - 65 * scale, x - 62 * scale, y - 8 * scale, 10 * scale, ...color);
  line(x, y - 65 * scale, x + 62 * scale, y - 8 * scale, 10 * scale, ...color);
  line(x, y + 20 * scale, x - 48 * scale, y + 112 * scale, 11 * scale, ...color);
  line(x, y + 20 * scale, x + 48 * scale, y + 112 * scale, 11 * scale, ...color);
  drawFace(player, head, color, t);
}

function drawSelectionScreen(t, ink, panel) {
  typeWrite("CHOOSE YOUR FIGHTER", 735, 76, 32, ...ink);
  for (let index = 0; index < fighterRoster.length; index++) {
    const fighter = fighterRoster[index];
    const selected = players.some((player) => player.rosterIndex === index);
    typeWrite(fighter.handle, 270 + index * 390, 235, selected ? 31 : 23,
      ...(selected ? fighter.color : mixColor([105,115,145],[130,140,155],visualTheme.light)));
  }
  for (const player of players) {
    const left = player.pad === 0 ? 90 : 990;
    const profile = fighterProfile(player.name);
    box(left, 320, 840, 570, ...panel);
    box(left, 320, 840, 8, ...player.color);
    drawSelectPortrait(player, left + 190, 590, 1.35, t);
    drawHandle(player.name, left + 355, 395, 46,
      profile.colors, player.color);
    const mood = profile.mood ? "MOOD  " + profile.mood.slice(0, 30) : "MOOD  —";
    const chat = profile.lastChat ? "CHAT  " + profile.lastChat.slice(0, 37) : "CHAT  —";
    typeWrite(mood + "\n" + chat, left + 355, 490, 22, ...ink);
    write(player.npc ? "STANDING BY" : selectionReady[player.pad] ? "READY" : "SELECT",
      left + 355, 720, 52, ...player.color);
    typeWrite(player.npc ? "NON-PLAYER" : "P1", left + 355, 805, 24, ...ink);
  }
  write("LEFT RIGHT SELECT     A READY     B BACK", 430, 965, 29, ...ink);
}

function paint() {
  const run = runtime();
  const t = (run.monotonicUs - startedAt) / 1000000;
  if (typeof ac === "function") acFeed = ac();
  for (const player of players) player.handleColors = fighterProfile(player.name).colors;
  visualTheme = losAngelesSun();
  const skyDay = mixColor([176, 215, 245], [255, 160, 112],
    visualTheme.sunset * .7);
  const sky = mixColor([7, 8, 28], skyDay, visualTheme.light);
  const arenaDay = mixColor([230, 239, 247], skyDay, visualTheme.sunset * .55);
  const arena = mixColor([10, 13, 30], arenaDay, visualTheme.light);
  const titlePanel = mixColor([22, 28, 104], [245, 248, 252], visualTheme.light);
  const titleInk = mixColor([245, 248, 255], [24, 35, 72], visualTheme.light);
  wipe(...sky);
  box(0, 0, 1920, 1080, ...arena);
  if (selecting) {
    drawSelectionScreen(t, titleInk, titlePanel);
    return;
  }
  cameraDoll.prepare();
  const worldInk = mixColor([72, 90, 125], [45, 63, 92], visualTheme.light);
  const floorBase = mixColor([17, 22, 36], [188, 181, 164], visualTheme.light);
  const floorAlt = mixColor([21, 27, 43], [174, 167, 151], visualTheme.light);
  const edgeWidth = Math.max(2, wallThickness * cameraScale() * .14);
  for (let x = worldLeft, column = 0; x < worldRight; x += 1000, column++) {
    for (let z = worldNear, row = 0; z < worldFar; z += 600, row++) {
      const a = projectPoint(x, floorY, z);
      const b = projectPoint(Math.min(worldRight, x + 1000), floorY, z);
      const c = projectPoint(Math.min(worldRight, x + 1000), floorY,
        Math.min(worldFar, z + 600));
      const d = projectPoint(x, floorY, Math.min(worldFar, z + 600));
      const color = (column + row) % 2 ? floorAlt : floorBase;
      triangle(a.x, a.y, b.x, b.y, c.x, c.y, ...color);
      triangle(a.x, a.y, c.x, c.y, d.x, d.y, ...color);
    }
  }
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
  const remaining = roundResult ? 0 : Math.max(0,
    Math.ceil((roundDurationUs - roundElapsedUs) / 1000000));
  const clockInk = remaining <= 10 ? [255, 105, 190] : titleInk;
  typeWrite(String(remaining).padStart(2, "0"), 928, 5, 48, ...clockInk);
  drawWindFlag(t, titleInk);
  if (ball.active) {
    const point = projectPoint(ball.x, ball.y, ball.z);
    const radius = Math.max(8, ball.radius * cameraScale());
    const speed = Math.hypot(ball.vx, ball.vy) || 1;
    const tail = projectPoint(ball.x - ball.vx / speed * 150,
      ball.y - ball.vy / speed * 150, ball.z);
    line(tail.x, tail.y, point.x, point.y, Math.max(3, radius * .35),
      255, 105, 190);
    circle(point.x, point.y, radius, Math.max(3, radius * .22), [245, 248, 255]);
    const inner = [];
    for (let side = 0; side < 5; side++) {
      const angle = -Math.PI / 2 + side * Math.PI * 2 / 5 + t * 1.4;
      inner.push({ x: point.x + Math.cos(angle) * radius * .38,
        y: point.y + Math.sin(angle) * radius * .38 });
    }
    for (let side = 0; side < 5; side++) {
      const next = (side + 1) % 5;
      line(inner[side].x, inner[side].y, inner[next].x, inner[next].y,
        Math.max(2, radius * .12), 255, 105, 190);
      line(inner[side].x, inner[side].y,
        point.x + (inner[side].x - point.x) * 2.15,
        point.y + (inner[side].y - point.y) * 2.15,
        Math.max(2, radius * .09), 255, 105, 190);
    }
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
  const introAge = run.monotonicUs - roundStartedAt;
  const showRunnerLabels = Boolean(roundResult) || introAge >= introDurationUs;
  drawRunner(players[0], t, showRunnerLabels);
  drawRunner(players[1], t, showRunnerLabels);
  if (!roundResult && introAge < introDurationUs) {
    const introSeconds = introAge / 1000000;
    const introText = introSeconds < .95 ? players[0].name
      : introSeconds < 1.9 ? players[1].name
      : players[0].name + "  VS  " + players[1].name;
    const size = introSeconds < 1.9 ? 76 : 62;
    const width = introText.length * size * .62;
    box((1920 - width) / 2 - 30, 824, width + 60, 145, ...titlePanel);
    typeWrite(introText, (1920 - width) / 2, 838, size, ...titleInk);
    const windLabel = windMph + " MPH WIND " +
      (windDirection < 0 ? "LEFT" : "RIGHT");
    typeWrite(windLabel, 960 - windLabel.length * 7.5, 932, 25, ...titleInk);
  }
  if (roundResult) {
    const cause = roundCause || "ROUND";
    const causeWidth = cause.length * 78;
    box((1920 - causeWidth) / 2 - 36, 790, causeWidth + 72, 126, ...titlePanel);
    typeWrite(cause, (1920 - causeWidth) / 2, 810, 92, ...titleInk);
    const resultWidth = roundResult.length * 28;
    typeWrite(roundResult, (1920 - resultWidth) / 2, 930, 34, ...titleInk);
  }
  drawPlayerHud(players[0], 20, padSnapshots[0]);
  drawPlayerHud(players[1], 1275, padSnapshots[1]);
  drawFighterData(players[0], 20);
  drawFighterData(players[1], 1035);
}

function act() {}
function leave() {}
