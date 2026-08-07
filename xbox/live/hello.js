// @bundle-qr
const buildTimestamp = "2026.08.06.2245 PDT";
const floorY = 12000;
const ceilingY = 0;
const wallThickness = 80;
const worldLeft = 0;
const worldRight = 12000;
const worldNear = -1800;
const worldFar = 1800;
const stageLeft = 0;
let stageRight = 1920;
let stageTop = 112;
// Leave a narrow projection gutter beneath the floor for the screen-edge HUD.
let stageBottom = 930;
let viewHeight = 1080;
let cameraAspect = (stageRight - stageLeft) / (stageBottom - stageTop);
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
const powerupIntervalUs = 10000000;
const shieldRadius = 160;
const shieldForward = 30;
const grenadeBlastDuration = .68;
const grenadeBlastRadius = 620;
const replayTickUs = 16667;
const replayCheckpointUs = 1000000;
const instantReplayStepUs = 33333;
const instantReplayMaxFrames = 240;
const walkSpeed = 1060;
const hudTypeSize = 42;
const replayButtons = ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown",
  "A", "B", "X", "Y"];
let cameraCenter = (worldLeft + worldRight) / 2;
let cameraWidth = worldRight - worldLeft;
let cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
let cameraContainFloor = 0;
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
const mixColor = (dark, light, amount) => dark.map((value, index) =>
  Math.round(value + (light[index] - value) * amount));
const lerp = (from, to, amount) => from + (to - from) * amount;
const normalize3 = (point) => {
  const length = Math.hypot(point.x, point.y, point.z) || 1;
  return { x: point.x / length, y: point.y / length, z: point.z / length };
};
const globalLight = normalize3({ x: -.42, y: 1, z: -.28 });
const cross3 = (a, b) => ({ x: a.y * b.z - a.z * b.y,
  y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x });
const dot3 = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z;
const viewWidth = () => stageRight;
const viewCenterX = () => (stageLeft + stageRight) / 2;
const viewOffsetX = () => (stageRight - 1920) / 2;
const compactLayout = () => stageRight < 1500;
// Keep the HUD inside a traditional five-percent title-safe frame. On a
// 16:9 television this is deliberately deeper than the action-safe camera so
// a bezel, soundbar, or object in front of the screen does not hide status.
const hudSafeInset = () => clamp(
  Math.round(Math.min(stageRight, viewHeight) * .05),
  compactLayout() ? 24 : 48, 72);
const hudSafeRect = () => {
  const inset = hudSafeInset();
  return { left: stageLeft + inset, top: inset,
    right: stageRight - inset, bottom: viewHeight - inset };
};
const actionSafeRect = () => {
  const marginX = compactLayout() ? 34 : 64;
  const marginY = 26;
  return { left: stageLeft + marginX, top: stageTop + marginY,
    right: stageRight - marginX, bottom: stageBottom - marginY };
};

function syncGameView() {
  const next = typeof gameView === "function" ? gameView() : null;
  const width = clamp(Math.round(Number(next?.width) || 1920), 480, 2880);
  const height = clamp(Math.round(Number(next?.height) || 1080), 480, 2160);
  const inputFamily = typeof capabilities === "function"
    ? capabilities().inputFamily : "xbox";
  const touch = inputFamily === "touch";
  const compact = width < 1500;
  const inset = compact ? 22 : 30;
  const nextTop = Math.max(82, inset + hudTypeSize + 16);
  const bottomReserve = touch
    ? clamp(height * .36, 300, 390)
    : clamp(height * .13, 112, 150);
  const nextBottom = Math.max(nextTop + 280, height - bottomReserve);
  if (width === stageRight && height === viewHeight &&
      nextTop === stageTop && nextBottom === stageBottom) return;
  stageRight = width;
  viewHeight = height;
  stageTop = nextTop;
  stageBottom = nextBottom;
  cameraAspect = (stageRight - stageLeft) / (stageBottom - stageTop);
  if (typeof cameraDoll !== "undefined") cameraDoll.dirty = true;
}

class FightCamDoll {
  constructor() {
    this.position = { x: cameraCenter, y: cameraCenterY, z: -cameraWidth * 1.4 };
    this.target = { x: cameraCenter, y: cameraCenterY, z: 0 };
    this.width = cameraWidth;
    this.perspective = 0;
    this.fov = 55;
    this.roll = 0;
    this.dirty = true;
    this.view = null;
  }

  track(spec, dt, speed = 5) {
    // Exponential easing is continuous across frame rates. Move the dolly and
    // its look target together so follow motion cannot shear into a sudden
    // swivel when fighters cross a framing threshold.
    const amount = 1 - Math.exp(-Math.max(0, dt) * speed);
    const previousTarget = { ...this.target };
    for (const axis of ["x", "y", "z"])
      this.target[axis] = lerp(this.target[axis], spec.target[axis], amount);
    for (const axis of ["x", "y", "z"])
      this.position[axis] += this.target[axis] - previousTarget[axis];
    for (const axis of ["x", "y", "z"]) {
      this.position[axis] = lerp(this.position[axis], spec.position[axis], amount);
    }
    // Pull back promptly but never in a single frame; return close more
    // slowly so small changes at the action-safe edge do not pump the lens.
    const zoomSpeed = spec.width > this.width ? speed * 1.45 : speed * .58;
    const zoomAmount = 1 - Math.exp(-Math.max(0, dt) * zoomSpeed);
    this.width = lerp(this.width, spec.width, zoomAmount);
    this.perspective = lerp(this.perspective, spec.perspective, amount);
    this.fov = lerp(this.fov, spec.fov || 55, amount);
    this.roll = lerp(this.roll, spec.roll || 0, amount);
    this.dirty = true;
  }

  prepare() {
    const forward = normalize3({ x: this.target.x - this.position.x,
      y: this.target.y - this.position.y, z: this.target.z - this.position.z });
    const baseRight = normalize3(cross3(forward, { x: 0, y: -1, z: 0 }));
    const baseUp = normalize3(cross3(baseRight, forward));
    const rollCos = Math.cos(this.roll);
    const rollSin = Math.sin(this.roll);
    const right = { x: baseRight.x * rollCos + baseUp.x * rollSin,
      y: baseRight.y * rollCos + baseUp.y * rollSin,
      z: baseRight.z * rollCos + baseUp.z * rollSin };
    const up = { x: baseUp.x * rollCos - baseRight.x * rollSin,
      y: baseUp.y * rollCos - baseRight.y * rollSin,
      z: baseUp.z * rollCos - baseRight.z * rollSin };
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
      y: lerp(orthoY, perspectiveY, this.perspective),
      z: clamp(viewZ / 16000 * 2.8 - 1.4, -1.4, 1.4) };
  }
}

const cameraDoll = new FightCamDoll();
const cameraScale = () => (stageRight - stageLeft) / cameraDoll.width;
let triangleDepth = -1.4;
function screenTriangle(x1, y1, x2, y2, x3, y3, ...color) {
  if (typeof triangle3d === "function")
    triangle3d(x1, y1, triangleDepth, x2, y2, triangleDepth,
      x3, y3, triangleDepth, ...color);
  else triangle(x1, y1, x2, y2, x3, y3, ...color);
}
function screenRect(x, y, width, height, color) {
  screenTriangle(x, y, x + width, y, x + width, y + height, ...color);
  screenTriangle(x, y, x + width, y + height, x, y + height, ...color);
}
function projectedTriangle(a, b, c, color) {
  if (typeof triangle3d === "function")
    triangle3d(a.x, a.y, a.z, b.x, b.y, b.z,
      c.x, c.y, c.z, ...color);
  else triangle(a.x, a.y, b.x, b.y, c.x, c.y, ...color);
}
function projectPoint(x, y, z = 0) {
  return cameraDoll.project({ x, y, z });
}
const screenX = (x, z = 0) => projectPoint(x, cameraCenterY, z).x;
const screenY = (y, z = 0) => projectPoint(cameraCenter, y, z).y;
const panAt = (x, z = 0) => clamp(
  (projectPoint(x, cameraCenterY, z).x - viewCenterX()) /
    Math.max(1, (stageRight - stageLeft) / 2 - 55), -1, 1);
const panPlayer = (player) => panAt(player.x, player.z);
let visualTheme = { light: 0, sunset: 0 };

const fighterRoster = [
  { handle: "@JEFFREY", color: [190, 42, 58], colors: [
    [255,105,190],[111,232,210],[255,105,190],[255,232,92],
    [130,150,255],[255,105,190],[111,232,210],[255,232,92]],
    mood: "New Media Instruments",
    lastChat: "hmm this chat could use some ui love" },
  { handle: "@FIFI", color: [209, 100, 216], colors: [
    [209,100,216],[204,253,71],[35,100,255],[82,67,17],[5,249,137]],
    mood: "nap", lastChat: "" },
  { handle: "@OSKIE", color: [38, 82, 176], colors: [
    [255,232,92],[255,105,190],[111,232,210],[255,232,92],[130,150,255],[255,105,190]],
    mood: "", lastChat: "" },
  { handle: "@SAT", color: [130, 204, 213], colors: [
    [130,204,213],[161,232,0],[253,122,2],[222,90,205]],
    mood: "i hope its not ai generated", lastChat: "meow" },
];
const npcFighter = { handle: "DUMMY", color: [105, 125, 150],
  colors: [[105,125,150],[135,155,180],[105,125,150]],
  mood: "TRAINING DUMMY · NO BOT AI", lastChat: "" };
const botFighter = { handle: "BOT", color: [205, 48, 72],
  colors: [[205,48,72],[255,102,92],[125,24,48]],
  mood: "ANGRY TRAINING BOT", lastChat: "" };

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
    pad: 0, spawnX: 5700, x: 5700, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: 1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [190, 42, 58], hit: 0,
    hitSegment: -1, hitSegmentUntil: 0, hitStunUntil: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    suppressedDirections: [],
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, roundWins: 0,
    attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false, blocking: false, blockFlash: 0,
    windVx: 0, knockVx: 0, gunAmmo: 0, grenadeAmmo: 0, stance: "NEUTRAL",
    heldBall: -1, grabHeld: false, crouchBlend: 0, standingOn: -1,
    partDamage: {}, removedParts: [], pogoHit: false,
    commandStream: [],
    jumpLaunchAt: 0, jumpPoseUntil: 0, landPoseUntil: 0 },
  { name: "@OSKIE", rosterIndex: 2, handleColors: fighterRoster[2].colors,
    npc: false, bot: false,
    pad: 1, spawnX: 6300, x: 6300, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: -1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [38, 82, 176], hit: 0,
    hitSegment: -1, hitSegmentUntil: 0, hitStunUntil: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    suppressedDirections: [],
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, roundWins: 0,
    attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false, blocking: false, blockFlash: 0,
    windVx: 0, knockVx: 0, gunAmmo: 0, grenadeAmmo: 0, stance: "NEUTRAL",
    heldBall: -1, grabHeld: false, crouchBlend: 0, standingOn: -1,
    partDamage: {}, removedParts: [], pogoHit: false,
    commandStream: [],
    jumpLaunchAt: 0, jumpPoseUntil: 0, landPoseUntil: 0 },
];
const impacts = [];
const detachedParts = [];
const bullets = [];
const grenades = [];
const gunPickups = [
  { amount: 6, x: 6000, y: platformY - 70, z: 0 },
];
const grenadePickups = [
  { amount: 2, x: 6000, y: platformY - 70, z: 0 },
];
for (const pickup of [...gunPickups, ...grenadePickups]) {
  pickup.active = false;
  pickup.respawnAt = 0;
}
const ballKinds = [
  { type: "soccer", radius: 38, mass: .72, hitScale: 1.12,
    bounce: .58, drag: .994, windFactor: .58 },
  { type: "basketball", radius: 42, mass: 1.08, hitScale: .86,
    bounce: .76, drag: .989, windFactor: .34 },
];
const balls = ballKinds.map((kind, spawnOwner) => ({
  ...kind, x: players[spawnOwner].spawnX, y: floorY - kind.radius,
  z: 0, vx: 0, vy: 0, active: true, serveAt: 0,
  lastHitBy: spawnOwner, safeUntil: 0, safePlayers: 0,
  spawnOwner, rotation: 0, heldBy: -1,
}));
// Version-one replay/spectator consumers still read the first ball by name.
const ball = balls[0];
let ballEnabled = true;
let padSnapshots = [null, null];
let startedAt = 0;
let roundStartedAt = 0;
let lastSimAt = 0;
let roundElapsedUs = 0;
let lastCountdownSecond = -1;
let roundOverAt = 0;
let roundResult = "";
let matchOver = false;
let roundCause = "";
let deathCinematic = null;
let impactHitboxesUntil = 0;
let nextPowerupAtUs = powerupIntervalUs;
let powerupSequence = 0;
let acFeed = {};
let selecting = true;
const selectionReady = [false, false];
const selectionPrevious = [[], []];
let touchSelectPad = 0;
let windMph = 0;
let windDirection = 1;
let windAcceleration = 0;
let replay = null;
let replayLastCommand = [-1, -1];
let replayNextCheckpointAt = 0;
let matchName = "";
let seriesName = "";
let previousRoundName = "";
let roundReplayFrames = [];
let roundReplayLastAt = 0;
let instantReplay = null;
let replayOfferPrevious = [];
let shellMode = "MENU";
let shellPrevious = [];
let navigationPrevious = [[], []];
// Temporary live combat inspector. Keep this explicit so the production view
// can return to a clean presentation without changing combat geometry.
let debugHitboxes = true;
let nextInputDebugAt = 0;
let frameTelemetry = [];
let frameTelemetryFlushAt = 0;
let lastPaintAt = 0;
let displayFps = 0;
let liveSequence = 0;
let liveNextAt = 0;
let spectatorQr = null;
let roundViewer = null;
let roundViewerStop = null;
let roundViewerMode = "";
let roundViewerStatus = "CONNECTING";
let roundViewerDemo = null;
let roundViewerDemoStartedAt = 0;
let livePublishFailed = false;

function pronounceableMatchName() {
  const onsets = ["b", "d", "f", "g", "k", "l", "m", "n", "p", "r",
    "s", "t", "v", "z", "ch", "sh", "th"];
  const consonants = "bdfgklmnprstvz";
  const vowels = "aeiou";
  const onset = onsets[Math.floor(Math.random() * onsets.length)];
  const vowel = vowels[Math.floor(Math.random() * vowels.length)];
  const middle = consonants[Math.floor(Math.random() * consonants.length)];
  const ending = (vowels + "y")[Math.floor(Math.random() * (vowels.length + 1))];
  return onset + vowel + middle + middle + ending +
    Math.floor(Math.random() * 1000);
}

function demoTick(now) {
  return replay ? Math.max(0, Math.round((now - replay.startedMonotonicUs) /
    replayTickUs)) : 0;
}

function trackMatchStarted() {
  if (typeof analytics !== "function") return;
  const device = typeof capabilities === "function" ? capabilities() : {};
  const platform = String(device.platform || "").toLowerCase();
  const surface = platform === "xbox-uwp" || platform === "xbox"
    ? "xbox"
    : platform === "macos" ? "macos" : "web";
  const family = String(device.inputFamily || "").toLowerCase();
  analytics("match_started", {
    source_system: "browser",
    surface,
    input_family: ["gamepad", "keyboard", "xbox"].includes(family)
      ? family
      : "unknown",
    opponent_type: players[1].bot ? "bot"
      : players[1].npc ? "dummy" : "local-player",
  });
}

function startReplay(now) {
  const run = runtime();
  seriesName = pronounceableMatchName();
  matchName = "";
  previousRoundName = "";
  replay = {
    format: "ac.oskiedemo", version: 1, game: "oskiewar",
    simulation: "oskiewar-physics-1", tickRate: 60,
    matchId: "ow-" + seriesName, matchName: seriesName,
    seriesId: "ow-" + seriesName, seriesName, roundIds: [],
    startedAt: run.unixMs || 0, startedMonotonicUs: now,
    fighters: players.map((player) => player.name),
    commands: [], events: [], checkpoints: [], rounds: [],
  };
  replayLastCommand = [-1, -1];
  replayNextCheckpointAt = now;
  liveSequence = 0;
  liveNextAt = now;
  livePublishFailed = false;
  spectatorQr = null;
  trackMatchStarted();
}

function roundIsTimed() {
  return !(players[1].npc && !players[1].bot);
}

function spectatorState(now, nextRoundId = "") {
  const introAge = now - roundStartedAt;
  const phase = instantReplay ? "replay" : matchOver ? "match"
    : roundResult ? "round" : introAge < introDurationUs ? "intro" : "fight";
  const timed = roundIsTimed();
  const remainingMs = roundResult ? 0 : timed ? Math.max(0,
    Math.round((roundDurationUs - roundElapsedUs) / 1000)) : null;
  const state = {
    format: "ac.oskiewar.live", version: 1, seq: liveSequence++,
    at: runtime().unixMs || 0, phase,
    seriesId: "ow-" + seriesName, roundId: "ow-" + matchName,
    previousRoundId: previousRoundName ? "ow-" + previousRoundName : "",
    fighters: players.map((player) => ({
      name: player.name, color: player.color, x: player.x, y: player.y,
      z: player.z, vx: player.vx, vy: player.vy, vz: player.vz,
      facing: player.facing, alive: player.alive,
      grounded: player.grounded, ducking: player.ducking,
      blocking: player.blocking, score: player.score,
      roundWins: player.roundWins, attack: player.attackKind || "",
      removedParts: player.removedParts.slice(),
    })),
    ball: { active: ball.active, x: ball.x, y: ball.y,
      z: ball.z, radius: ball.radius, type: ball.type, mass: ball.mass },
    balls: balls.map((item) => ({ active: item.active, x: item.x,
      y: item.y, z: item.z, radius: item.radius, type: item.type,
      mass: item.mass, heldBy: item.heldBy,
      spawnOwner: item.spawnOwner })),
    camera: { x: cameraCenter, y: cameraCenterY, width: cameraWidth,
      position: { ...cameraDoll.position }, target: { ...cameraDoll.target },
      perspective: cameraDoll.perspective, fov: cameraDoll.fov,
      roll: cameraDoll.roll },
    wind: { direction: windDirection, mph: windMph },
    round: { remainingMs, timed, result: roundResult || "",
      cause: roundCause || "" },
    replayUrl: "/api/oskiewar-replays?id=ow-" + matchName,
  };
  if (nextRoundId) state.nextRoundId = nextRoundId;
  return state;
}

function publishSpectator(now, { target = matchName, nextRoundId = "",
  force = false } = {}) {
  if (!target || livePublishFailed || typeof publishLive !== "function" ||
      (!force && now < liveNextAt)) return;
  liveNextAt = now + 50000;
  try {
    publishLive("ow-" + target,
      JSON.stringify(spectatorState(now, nextRoundId)));
  } catch (error) {
    // A native host with an older room-ID contract must never take down play.
    // Disable only spectator publishing until the next match/host upgrade.
    livePublishFailed = true;
    telemetry("OSKIEWAR_LIVE_DISABLED", String(error?.message || error));
  }
}

function inputCommand(pad) {
  const input = quantizedInput(pad);
  let mask = 0;
  if (input.horizontal < 0) mask |= 1;
  if (input.horizontal > 0) mask |= 2;
  if (input.vertical > 0) mask |= 4;
  if (input.vertical < 0) mask |= 8;
  for (let index = 4; index < replayButtons.length; index++)
    if (pad.down.includes(replayButtons[index])) mask |= 1 << index;
  return mask;
}

function recordReplayCommands(now, inputs = padSnapshots) {
  if (!replay) return;
  for (let pad = 0; pad < players.length; pad++) {
    const command = players[pad].npc && !players[pad].bot
      ? 0 : inputCommand(inputs[pad]);
    if (command !== replayLastCommand[pad]) {
      replay.commands.push([demoTick(now), pad, command]);
      replayLastCommand[pad] = command;
    }
  }
}

function replayFlags(player) {
  return (player.alive ? 1 : 0) | (player.grounded ? 2 : 0) |
    (player.ducking ? 4 : 0) | (player.blocking ? 8 : 0) |
    [...limbParts, "torso"].reduce((flags, part, index) =>
      flags | (hasPart(player, part) ? 0 : 1 << (index + 4)), 0);
}

function recordReplayCheckpoint(now, force = false) {
  if (!replay || (!force && now < replayNextCheckpointAt)) return;
  replayNextCheckpointAt = now + replayCheckpointUs;
  const values = [demoTick(now)];
  for (const player of players) values.push(
    Math.round(player.x), Math.round(player.y), Math.round(player.z),
    Math.round(player.vx), Math.round(player.vy), replayFlags(player),
    player.score, player.roundWins);
  values.push(Math.round(ball.x), Math.round(ball.y), Math.round(ball.z),
    Math.round(ball.vx), Math.round(ball.vy), ball.active ? 1 : 0,
    Math.round(cameraCenter), Math.round(cameraCenterY), Math.round(cameraWidth));
  replay.checkpoints.push(values);
}

function makeRoundReplayFrame(now) {
  const poseTime = (now - startedAt) / 1000000;
  return {
    players: players.map((player) => ({
      x: player.x, y: player.y, z: player.z,
      vx: player.vx, vy: player.vy, vz: player.vz,
      facing: player.facing, grounded: player.grounded,
      ducking: player.ducking, alive: player.alive,
      blocking: player.blocking, blockFlash: player.blockFlash, hit: player.hit,
      attackKind: player.attackKind,
      attackStartedOffset: player.attackStartedAt - now,
      attackUntilOffset: player.attackUntil - now,
      geometry: runnerWorldGeometry(player, poseTime),
    })),
    ball: { x: ball.x, y: ball.y, z: ball.z, vx: ball.vx, vy: ball.vy,
      active: ball.active },
    balls: balls.map((item) => ({ x: item.x, y: item.y, z: item.z,
      vx: item.vx, vy: item.vy, active: item.active })),
    camera: { center: cameraCenter, centerY: cameraCenterY, width: cameraWidth },
  };
}

function captureRoundReplay(now, force = false) {
  if (!force && now - roundReplayLastAt < instantReplayStepUs) return;
  roundReplayLastAt = now;
  roundReplayFrames.push(makeRoundReplayFrame(now));
  while (roundReplayFrames.length > instantReplayMaxFrames)
    roundReplayFrames.shift();
}

const frameTelemetrySchema = ["us", "cameraX", "cameraY", "cameraWidth",
  "dollX", "dollY", "dollZ", "targetX", "targetY", "targetZ",
  "dollWidth", "roll", "p1x", "p1y", "p1z", "p1vx", "p1vy",
  "p2x", "p2y", "p2z", "p2vx", "p2vy"];

function captureFrameTelemetry(now, force = false) {
  if (selecting || shellMode === "MENU") return;
  const round = (value) => Math.round(Number(value || 0) * 100) / 100;
  frameTelemetry.push([
    Math.round(now - roundStartedAt), round(cameraCenter), round(cameraCenterY),
    round(cameraWidth), round(cameraDoll.position.x), round(cameraDoll.position.y),
    round(cameraDoll.position.z), round(cameraDoll.target.x),
    round(cameraDoll.target.y), round(cameraDoll.target.z),
    round(cameraDoll.width), round(cameraDoll.roll),
    ...players.flatMap((player) => [round(player.x), round(player.y),
      round(player.z), round(player.vx), round(player.vy)]),
  ]);
  if (!force && now < frameTelemetryFlushAt) return;
  telemetry("FIGHT_TRACE", JSON.stringify({
    format: "ac.oskiewar.frames", version: 1, round: "ow-" + matchName,
    schema: frameTelemetrySchema, frames: frameTelemetry,
  }));
  frameTelemetry = [];
  frameTelemetryFlushAt = now + 1000000;
}

function applyRoundReplayFrame(frame, now) {
  for (let index = 0; index < players.length; index++) {
    const player = players[index];
    const state = frame.players[index];
    for (const key of ["x", "y", "z", "vx", "vy", "vz", "facing",
      "grounded", "ducking", "alive", "blocking", "blockFlash", "hit",
      "attackKind"]) player[key] = state[key];
    player.attackStartedAt = now + state.attackStartedOffset;
    player.attackUntil = now + state.attackUntilOffset;
    player.replayGeometry = state.geometry;
  }
  const frameBalls = frame.balls || [frame.ball];
  for (let index = 0; index < balls.length; index++) {
    const state = frameBalls[index];
    if (!state) continue;
    for (const key of ["x", "y", "z", "vx", "vy", "active"])
      balls[index][key] = state[key];
  }
  cameraCenter = frame.camera.center;
  cameraCenterY = frame.camera.centerY;
  cameraWidth = frame.camera.width;
}

function finishInstantReplay(now) {
  if (!instantReplay) return;
  applyRoundReplayFrame(instantReplay.endFrame, now);
  for (const player of players) delete player.replayGeometry;
  instantReplay = null;
  roundOverAt = now;
  replayOfferPrevious = padSnapshots[0]?.down?.slice() || [];
}

function startInstantReplay(now) {
  if (roundReplayFrames.length < 2) return false;
  const frames = roundReplayFrames.slice();
  instantReplay = { frames, cursor: 0, lastAt: now, paused: false,
    previous: padSnapshots[0]?.down?.slice() || [],
    endFrame: frames[frames.length - 1] };
  impacts.length = 0;
  detachedParts.length = 0;
  applyRoundReplayFrame(frames[0], now);
  telemetry("ROUND_REPLAY", "start frames=" + frames.length);
  return true;
}

function updateInstantReplay(now, dt) {
  if (!instantReplay) return;
  const down = padSnapshots[0]?.down || [];
  const pressed = (button) => down.includes(button) &&
    !instantReplay.previous.includes(button);
  if (pressed("B")) {
    finishInstantReplay(now);
    return;
  }
  if (pressed("A")) instantReplay.paused = !instantReplay.paused;
  if (pressed("ArrowLeft") || pressed("ArrowRight")) {
    instantReplay.paused = true;
    instantReplay.cursor += pressed("ArrowLeft") ? -15 : 15;
  }
  if (!instantReplay.paused)
    instantReplay.cursor += (now - instantReplay.lastAt) / instantReplayStepUs;
  instantReplay.lastAt = now;
  instantReplay.cursor = clamp(instantReplay.cursor, 0,
    instantReplay.frames.length);
  if (instantReplay.cursor >= instantReplay.frames.length) {
    finishInstantReplay(now);
    return;
  }
  applyRoundReplayFrame(instantReplay.frames[Math.floor(instantReplay.cursor)], now);
  instantReplay.previous = down.slice();
  const target = { x: cameraCenter, y: cameraCenterY, z: 0 };
  cameraDoll.track({ target,
    position: { x: cameraCenter - cameraWidth * .06,
      y: cameraCenterY - cameraWidth * .04, z: -cameraWidth * 1.35 },
    width: cameraWidth, perspective: 0, fov: 55 }, dt, 18);
}

function saveRoundReplay(now) {
  if (!replay) return;
  recordReplayCheckpoint(now, true);
  const demo = JSON.parse(JSON.stringify(replay));
  demo.matchId = "ow-" + matchName;
  demo.matchName = matchName;
  demo.roundId = demo.matchId;
  demo.roundName = matchName;
  demo.roundIndex = Math.max(0, replay.roundIds.length - 1);
  demo.previousRoundId = previousRoundName ? "ow-" + previousRoundName : "";
  demo.durationTicks = demoTick(now);
  demo.winner = players[0].score === players[1].score ? null
    : players[0].score > players[1].score ? players[0].name : players[1].name;
  demo.finalRoundWins = players.map((player) => player.roundWins);
  delete demo.startedMonotonicUs;
  const payload = JSON.stringify(demo);
  if (payload.length <= 524288 && typeof saveReplay === "function") {
    saveReplay(payload);
    telemetry("REPLAY", "queued " + demo.roundId + " bytes=" + payload.length);
  } else telemetry("REPLAY", "not-saved bytes=" + payload.length);
}

function finishReplay() {
  replay = null;
}

function emitSignal(event, player = -1, value = 0, value2 = 0) {
  if (replay) replay.events.push([demoTick(runtime().monotonicUs), event,
    player, Math.round(value * 1000) / 1000, Math.round(value2 * 1000) / 1000]);
  if (typeof gameSignal === "function") gameSignal(event, player, value, value2);
}

// Revision 37 exposes native bell/woosh voices in the mixer but its QuickJS
// allowlist predates those names. Fall back without stopping the match; newer
// hosts and the browser still receive the authored voice unchanged.
function playDrum(name, velocity = 1, pan = 0) {
  if (typeof drum !== "function") return;
  try {
    drum(name, velocity, pan);
  } catch (error) {
    if (name !== "bell" && name !== "whoosh") throw error;
    if (name === "bell" && typeof synth === "function") {
      try { synth(880, .12); } catch (_) {}
    }
    try {
      drum(name === "bell" ? "hat" : "block", velocity * .65, pan);
    } catch (_) {}
  }
}

let clientError = "";

function captureClientError(phase, error) {
  const detail = error && (error.stack || error.message)
    ? String(error.stack || error.message) : String(error || "unknown error");
  clientError = (phase + ": " + detail)
    .replace(/[^\x20-\x7e]+/g, " ").replace(/\s+/g, " ").trim();
  try { telemetry("CLIENT_ERROR", clientError); } catch (_) {}
}

function clientErrorLines(text, limit = 58) {
  const words = String(text).split(" ");
  const lines = [];
  let line = "";
  for (const word of words) {
    if (!line) line = word;
    else if (line.length + word.length + 1 <= limit) line += " " + word;
    else { lines.push(line); line = word; }
  }
  if (line) lines.push(line);
  return lines.slice(0, 9);
}

function errorTypeWrite(text, x, y, size, ...color) {
  try {
    typeWrite(text, x, y, size, ...color);
  } catch (_) {
    // Error reporting must survive a font renderer failure of its own.
    try { systemWrite(String(text).toLowerCase(), x, y, size, ...color); }
    catch (_) {}
  }
}

function drawClientError() {
  let width = 1920;
  let height = 1080;
  try {
    const view = typeof gameView === "function" ? gameView() : null;
    if (view && Number.isFinite(view.width)) width = view.width;
    if (view && Number.isFinite(view.height)) height = view.height;
  } catch (_) {}
  wipe(7, 9, 18);
  box(48, 48, width - 96, height - 96, 42, 16, 32);
  errorTypeWrite("client error", 92, 92, 54, 255, 112, 140);
  const lines = clientErrorLines(clientError);
  for (let index = 0; index < lines.length; index++)
    errorTypeWrite(lines[index], 92, 188 + index * 58, 34, 248, 244, 255);
  errorTypeWrite("relaunch or deploy an update", 92, height - 126,
    28, 190, 202, 230);
}

function fighterProfile(handle) {
  const live = Array.isArray(acFeed.fighters)
    ? acFeed.fighters.find((profile) => profile.handle.toUpperCase() === handle.toUpperCase())
    : null;
  const fallback = handle === "DUMMY" ? npcFighter
    : handle === "BOT" ? botFighter
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
    const fighter = player.bot ? botFighter : npcFighter;
    player.rosterIndex = -1;
    player.name = fighter.handle;
    player.color = fighter.color.slice();
    player.handleColors = fighter.colors;
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
  touchSelectPad = 0;
  selectionReady[0] = false;
  selectionReady[1] = false;
  selectionPrevious[0] = padSnapshots[0]?.down?.slice() || [];
  selectionPrevious[1] = padSnapshots[1]?.down?.slice() || [];
  roundResult = "";
  roundCause = "";
  deathCinematic = null;
  matchOver = false;
  roundElapsedUs = 0;
  lastCountdownSecond = -1;
  roundStartedAt = now;
  for (const player of players) {
    player.roundWins = 0;
    player.score = 0;
    player.alive = true;
  }
}

function returnToSelectPressed(now) {
  let pressed = false;
  for (let index = 0; index < padSnapshots.length; index++) {
    const down = padSnapshots[index]?.down || [];
    const previous = navigationPrevious[index];
    if (down.includes("View") && !previous.includes("View")) {
      debugHitboxes = !debugHitboxes;
      telemetry("FIGHT_DEBUG", debugHitboxes ? "on" : "off");
    }
    if (down.includes("Menu") && !previous.includes("Menu")) pressed = true;
    navigationPrevious[index] = down.slice();
  }
  if (!pressed || shellMode !== "GAME" || selecting) return false;
  beginSelect(now);
  playDrum("block", .9, 0);
  telemetry("SHELL", "game->select " + now);
  return true;
}

function enterGame(now) {
  shellMode = "GAME";
  shellPrevious = padSnapshots[0]?.down?.slice() || [];
  beginSelect(now);
}

function updateShell(now) {
  const down = padSnapshots[0]?.down || [];
  if (down.some((button) => !shellPrevious.includes(button))) {
    playDrum("hat", .55, 0);
    if (typeof titleBeep === "function") titleBeep();
    if (typeof titleVoice === "function") titleVoice();
    emitSignal("select", -1, 1, 0);
    enterGame(now);
  }
  shellPrevious = down.slice();
}

function cycleOpponentMode() {
  const opponent = players[1];
  if (!opponent.npc) {
    opponent.npc = true;
    opponent.bot = false;
    selectionReady[1] = true;
  } else if (!opponent.bot) {
    opponent.bot = true;
    selectionReady[1] = true;
  } else {
    opponent.npc = false;
    opponent.bot = false;
    applyRoster(opponent, 2);
    selectionReady[1] = false;
  }
  if (opponent.npc) applyRoster(opponent, -1);
  playDrum("clap", .8, 0);
}

function selectionTouchLayout() {
  if (compactLayout()) {
    const margin = 24;
    const width = viewWidth() - margin * 2;
    const rosterTop = 174;
    const rosterHeight = 42;
    const rosterGap = 5;
    const cardsTop = 382;
    const cardHeight = 128;
    const cardGap = 12;
    return {
      roster: fighterRoster.map((fighter, index) => ({ index,
        x: margin, y: rosterTop + index * (rosterHeight + rosterGap),
        width, height: rosterHeight })),
      cards: players.map((player) => {
        const y = cardsTop + player.pad * (cardHeight + cardGap);
        return { pad: player.pad, x: margin, y, width, height: cardHeight,
          ready: { x: margin + 100, y: y + 58,
            width: width - 215, height: 58 },
          mode: player.pad === 1 ? { x: margin + width - 105, y: y + 58,
            width: 95, height: 58 } : null };
      }),
    };
  }
  const ox = viewOffsetX();
  return {
    roster: fighterRoster.map((fighter, index) => ({ index,
      x: ox + 205 + index * 390, y: 170, width: 300, height: 90 })),
    cards: players.map((player) => {
      const x = ox + (player.pad === 0 ? 90 : 990);
      return { pad: player.pad, x, y: 320, width: 840, height: 570,
        ready: { x: x + 330, y: 675, width: 350, height: 100 },
        mode: player.pad === 1
          ? { x: x + 660, y: 785, width: 150, height: 70 } : null };
    }),
  };
}

const pointInRect = (point, rect) => rect && point.x >= rect.x &&
  point.x <= rect.x + rect.width && point.y >= rect.y &&
  point.y <= rect.y + rect.height;

function selectionHover(layout = selectionTouchLayout()) {
  const pointer = globalThis.__oskiewarTouch?.pointer;
  if (!pointer?.active || !Number.isFinite(pointer.x) ||
      !Number.isFinite(pointer.y)) return null;
  const roster = layout.roster.find((rect) => pointInRect(pointer, rect));
  if (roster) return { roster: roster.index };
  const card = layout.cards.find((rect) => pointInRect(pointer, rect));
  if (!card) return null;
  return { card: card.pad,
    ready: pointInRect(pointer, card.ready) ? card.pad : -1,
    mode: pointInRect(pointer, card.mode) ? card.pad : -1 };
}

function consumeSelectTouches(now) {
  const queue = globalThis.__oskiewarTouch?.taps;
  if (!Array.isArray(queue) || !queue.length) return;
  const touches = queue.splice(0);
  const layout = selectionTouchLayout();
  for (const point of touches) {
    const roster = layout.roster.find((rect) => pointInRect(point, rect));
    if (roster) {
      const player = players[touchSelectPad];
      if (player.pad === 1 && player.npc) {
        player.npc = false;
        player.bot = false;
      }
      applyRoster(player, roster.index);
      selectionReady[player.pad] = false;
      playDrum("hat", .8, player.pad === 0 ? -.65 : .65);
      continue;
    }
    const card = layout.cards.find((rect) => pointInRect(point, rect));
    if (!card) continue;
    touchSelectPad = card.pad;
    if (card.mode && pointInRect(point, card.mode)) {
      cycleOpponentMode();
      continue;
    }
    const player = players[card.pad];
    if (pointInRect(point, card.ready) && !player.npc) {
      selectionReady[player.pad] = !selectionReady[player.pad];
      playDrum(selectionReady[player.pad] ? "clap" : "hat", 1,
        player.pad === 0 ? -.65 : .65);
      const other = players[player.pad === 0 ? 1 : 0];
      if (selectionReady[player.pad] && !selectionReady[other.pad] && !other.npc)
        touchSelectPad = other.pad;
    }
  }
}

function updateSelect(now) {
  consumeSelectTouches(now);
  for (const player of players) {
    const down = padSnapshots[player.pad]?.down || [];
    if (player.npc) {
      selectionPrevious[player.pad] = down.slice();
      continue;
    }
    const previous = selectionPrevious[player.pad];
    const pressed = (button) => down.includes(button) && !previous.includes(button);
    if (player.pad === 0 && pressed("X")) {
      cycleOpponentMode();
    }
    if (pressed("B") && selectionReady[player.pad]) selectionReady[player.pad] = false;
    if (!selectionReady[player.pad] && (pressed("ArrowLeft") || pressed("ArrowRight"))) {
      applyRoster(player, player.rosterIndex + (pressed("ArrowRight") ? 1 : -1));
      playDrum("hat", .8, player.pad === 0 ? -.65 : .65);
    }
    if (!selectionReady[player.pad] && pressed("A")) {
      selectionReady[player.pad] = true;
      playDrum("clap", 1, player.pad === 0 ? -.65 : .65);
    }
    selectionPrevious[player.pad] = down.slice();
  }
  if (selectionReady[0] && selectionReady[1]) {
    selecting = false;
    startReplay(now);
    resetRound(now, true);
    emitSignal("fighters", -1, players[0].rosterIndex, players[1].rosterIndex);
  }
}

function rollWind() {
  windMph = 4 + Math.floor(Math.random() * 21);
  windDirection = windAcceleration === 0
    ? (Math.random() < .5 ? -1 : 1) : -windDirection;
  windAcceleration = windDirection * windMph * 16;
  emitSignal("wind", -1, windDirection, windMph);
}

function resetBalls(now) {
  for (const item of balls) {
    const owner = players[item.spawnOwner];
    item.x = owner.spawnX + owner.facing * 180;
    item.y = floorY - item.radius;
    // Players have no depth-axis control, so a served ball must begin in the
    // same playable lane. The former +/-60 offset put its body outside the
    // narrow leg capsules even when the sprites visibly overlapped.
    item.z = owner.z;
    item.vx = 0;
    item.vy = 0;
    item.rotation = 0;
    item.heldBy = -1;
    item.active = ballEnabled;
    item.serveAt = now + introDurationUs + 150000;
    item.lastHitBy = owner.pad;
    item.safeUntil = item.serveAt;
    item.safePlayers = 1 << owner.pad;
    emitSignal("ballserve", owner.pad, owner.facing, windMph);
  }
}

const buttonLabel = (button) => ({
  ArrowUp: "UP", ArrowDown: "DOWN", ArrowLeft: "LEFT", ArrowRight: "RIGHT",
  LeftShoulder: "LB", RightShoulder: "RB", LeftStick: "LEFT STICK",
  RightStick: "RIGHT STICK", View: "VIEW", Menu: "MENU",
}[button] || String(button).toUpperCase());

function roundDemoState(demo, now) {
  const checkpoints = demo?.checkpoints || [];
  if (!checkpoints.length) return null;
  const roundIndex = Number.isInteger(demo.roundIndex) ? demo.roundIndex
    : Math.max(0, (demo.rounds?.length || 1) - 1);
  const startTick = demo.rounds?.[roundIndex]?.[0] || 0;
  const finalCheckpoint = checkpoints[checkpoints.length - 1];
  const endTick = Math.max(startTick + 1, demo.durationTicks || finalCheckpoint[0]);
  const elapsed = Math.floor((now - roundViewerDemoStartedAt) / replayTickUs);
  const tick = startTick + elapsed % (endTick - startTick + 1);
  let before = checkpoints[0], after = finalCheckpoint;
  for (const row of checkpoints) {
    if (row[0] <= tick) before = row;
    if (row[0] >= tick) { after = row; break; }
  }
  const amount = clamp((tick - before[0]) / Math.max(1, after[0] - before[0]), 0, 1);
  const value = (index) => lerp(before[index], after[index], amount);
  const recentAttack = (pad) => {
    let kind = "";
    for (const event of demo.events || []) {
      if (event[0] > tick || event[0] < tick - 18 || event[2] !== pad) continue;
      if (event[1] === "punch" || event[1] === "kick") kind = event[1].toUpperCase();
    }
    return kind;
  };
  const fighters = [0, 1].map((pad) => {
    const offset = 1 + pad * 8;
    const flags = before[offset + 5];
    const name = demo.fighters?.[pad] || `P${pad + 1}`;
    const profile = name === "DUMMY" ? npcFighter : name === "BOT" ? botFighter
      : fighterRoster.find((fighter) => fighter.handle === name);
    const vx = value(offset + 3);
    return { name, color: profile?.color || players[pad].color,
      x: value(offset), y: value(offset + 1), z: value(offset + 2),
      vx, vy: value(offset + 4), vz: 0,
      facing: vx ? Math.sign(vx) : pad ? -1 : 1,
      alive: Boolean(flags & 1), grounded: Boolean(flags & 2),
      ducking: Boolean(flags & 4), blocking: Boolean(flags & 8),
      removedParts: [...limbParts, "torso"].filter((part, index) =>
        Boolean(flags & (1 << (index + 4)))),
      score: Math.round(value(offset + 6)),
      roundWins: Math.round(value(offset + 7)), attack: recentAttack(pad) };
  });
  const round = demo.rounds?.[roundIndex] || [startTick, 1, 0, 1];
  const nearEnd = tick >= endTick - 20;
  const replayBall = { x: value(17), y: value(18), z: value(19),
    radius: 42, active: Boolean(before[22]), spawnOwner: 0 };
  return { phase: "replay", fighters, ball: replayBall, balls: [replayBall],
    camera: { x: value(23), y: value(24), width: Math.max(100, value(25)) },
    wind: { direction: round[1], mph: round[2] },
    round: { remainingMs: Math.max(0, Math.round((endTick - tick) * 1000 /
      (demo.tickRate || 60))), result: nearEnd
        ? demo.winner ? `${demo.winner} WINS ROUND` : "TIE" : "" } };
}

function applyRoundViewerState(state, now, dt = 1 / 60) {
  if (!state?.fighters?.length || !state.camera || !state.round) return;
  for (let index = 0; index < players.length; index++) {
    const source = state.fighters[index];
    const player = players[index];
    const previousX = player.x;
    const previousY = player.y;
    for (const key of ["name", "color", "x", "y", "z", "facing", "alive",
      "grounded", "ducking", "blocking", "score", "roundWins", "removedParts"])
      if (source[key] !== undefined) player[key] = source[key];
    player.vx = source.vx ?? (player.x - previousX) / Math.max(.001, dt);
    player.vy = source.vy ?? (player.y - previousY) / Math.max(.001, dt);
    player.vz = source.vz || 0;
    player.attackKind = source.attack || "";
    player.attackStartedAt = player.attackKind ? now - 80000 : 0;
    player.attackUntil = player.attackKind ? now + 120000 : 0;
    player.hit = 0;
    player.blockFlash = 0;
  }
  const sources = state.balls || [state.ball];
  for (let index = 0; index < balls.length; index++) {
    const source = sources[index];
    if (!source) { balls[index].active = false; continue; }
    for (const key of ["x", "y", "z", "radius", "active", "spawnOwner",
      "type", "mass", "heldBy"])
      if (source[key] !== undefined) balls[index][key] = source[key];
  }
  cameraCenter = state.camera.x;
  cameraCenterY = state.camera.y;
  cameraWidth = state.camera.width;
  if (state.wind) {
    windDirection = state.wind.direction;
    windMph = state.wind.mph;
    windAcceleration = windDirection * windMph * 16;
  }
  const hadResult = Boolean(roundResult);
  roundResult = state.round.result || "";
  roundCause = state.round.cause ||
    (roundResult.includes("BALLED") ? "BALLED" : roundResult ? "ROUND" : "");
  roundElapsedUs = Math.max(0, roundDurationUs - state.round.remainingMs * 1000);
  matchOver = state.phase === "match";
  roundStartedAt = now - introDurationUs - roundElapsedUs;
  if (roundResult && !hadResult) roundOverAt = now;
  const target = state.camera.target || { x: cameraCenter, y: cameraCenterY, z: 0 };
  cameraDoll.track({ target,
    position: state.camera.position ||
      { x: cameraCenter, y: cameraCenterY, z: -cameraWidth * 1.35 },
    width: cameraWidth, perspective: state.camera.perspective || 0,
    fov: state.camera.fov || 55, roll: state.camera.roll || 0 }, dt, 1000);
}

function handleRoundViewer(message) {
  const now = runtime().monotonicUs;
  matchName = message.roundName || matchName;
  if (message.type === "round") {
    roundViewerDemo = null;
    roundViewerMode = "";
    roundViewerStatus = "CONNECTING";
    return;
  }
  if (message.type === "status") {
    roundViewerStatus = String(message.content?.label || "waiting").toUpperCase();
    return;
  }
  if (message.type === "demo") {
    roundViewerDemo = message.content;
    roundViewerDemoStartedAt = now;
    roundViewerMode = "DEMO";
    return;
  }
  if (message.type === "state") {
    if (roundViewerDemo && !message.live) return;
    if (roundViewerDemo && message.content?.phase === "match") return;
    roundViewerMode = "LIVE";
    applyRoundViewerState(message.content, now);
  }
}

function updateRoundViewer(now, dt) {
  if (roundViewerDemo && roundViewerMode === "DEMO") {
    const state = roundDemoState(roundViewerDemo, now);
    if (state) applyRoundViewerState(state, now, dt);
  }
}

function boot() {
  syncGameView();
  startedAt = runtime().monotonicUs;
  roundStartedAt = startedAt;
  lastSimAt = startedAt;
  roundElapsedUs = 0;
  lastCountdownSecond = -1;
  emitSignal("hello", -1, 1, 0);
  shellMode = "MENU";
  spectatorQr = typeof qrcode === "function"
    ? qrcode("https://oskiewar.com", { errorCorrectLevel: 1 }) : null;
  shellPrevious = [];
  navigationPrevious = [[], []];
  roundViewer = globalThis.__oskiewarRoundBridge || null;
  if (roundViewer?.start) {
    shellMode = "GAME";
    selecting = false;
    roundResult = "";
    matchOver = false;
    matchName = roundViewer.name || "";
    spectatorQr = typeof qrcode === "function"
      ? qrcode("https://oskiewar.com/" + matchName,
        { errorCorrectLevel: 1 }) : spectatorQr;
    roundStartedAt = startedAt - introDurationUs;
    roundViewerStop = roundViewer.start(handleRoundViewer);
    return;
  }
  beginSelect(startedAt);
}

function resetRound(now, resetMatch = false) {
  if (replay) {
    const nextRoundName = pronounceableMatchName();
    if (matchName) publishSpectator(now, {
      target: matchName, nextRoundId: "ow-" + nextRoundName, force: true,
    });
    previousRoundName = matchName;
    matchName = nextRoundName;
    replay.roundIds.push("ow-" + matchName);
    spectatorQr = typeof qrcode === "function"
      ? qrcode("https://oskiewar.com/" + matchName,
        { errorCorrectLevel: 1 }) : null;
  }
  impacts.length = 0;
  detachedParts.length = 0;
  bullets.length = 0;
  grenades.length = 0;
  roundReplayFrames = [];
  roundReplayLastAt = 0;
  instantReplay = null;
  replayOfferPrevious = [];
  frameTelemetry = [];
  frameTelemetryFlushAt = now + 1000000;
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
    player.shieldVx = 0;
    player.gunAmmo = 0;
    player.grenadeAmmo = 0;
    player.stance = "NEUTRAL";
    player.itemAction = "";
    player.itemActionStartedAt = 0;
    player.itemActionUntil = 0;
    player.heldBall = -1;
    player.grabHeld = false;
    player.commandStream = [];
    player.hitSegment = -1;
    player.hitSegmentUntil = 0;
    player.hitStunUntil = 0;
    player.partDamage = {};
    player.removedParts = [];
    player.pogoHit = false;
    player.standingOn = -1;
    player.previousY = floorY;
    player.crouchBlend = 0;
    player.jumpLaunchAt = 0;
    player.jumpPoseUntil = 0;
    player.landPoseUntil = 0;
    player.botAttackAt = now + 420000;
    player.botAttackUntil = 0;
    player.botAttackButton = "";
    player.botAttackSequence = 0;
    player.botJumpAt = now + 900000;
    delete player.frozenGeometry;
    delete player.frozenAt;
    delete player.headBustedAt;
    player.previous = padSnapshots[player.pad]?.down?.slice() || [];
    player.suppressedDirections = player.previous.filter((button) =>
      button.startsWith("Arrow"));
    player.lastButton = "NONE";
    player.lastButtonAt = -10000000;
  }
  for (const pickup of [...gunPickups, ...grenadePickups]) {
    pickup.active = false;
    pickup.respawnAt = 0;
  }
  nextPowerupAtUs = powerupIntervalUs;
  powerupSequence = 0;
  roundResult = "";
  roundCause = "";
  deathCinematic = null;
  matchOver = false;
  roundElapsedUs = 0;
  lastCountdownSecond = -1;
  lastSimAt = now;
  roundStartedAt = now;
  rollWind();
  resetBalls(now);
  if (replay) replay.rounds.push([demoTick(now), windDirection, windMph,
    balls.length]);
  cameraCenter = (worldLeft + worldRight) / 2;
  cameraWidth = 1120;
  cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
  cameraContainFloor = 0;
}

function updateCamera(dt) {
  // Look slightly ahead of fast movement so zoom starts before a fighter
  // reaches the safe edge instead of reacting after the crossing.
  const lookAhead = .2;
  const future = players.map((player) => ({
    x: player.x + (player.vx + (player.windVx || 0) +
      (player.knockVx || 0) + (player.shieldVx || 0)) * lookAhead,
    y: player.y + player.vy * lookAhead,
  }));
  const left = Math.min(players[0].x, players[1].x,
    future[0].x, future[1].x);
  const right = Math.max(players[0].x, players[1].x,
    future[0].x, future[1].x);
  const top = Math.min(players[0].y - 220, players[1].y - 220,
    future[0].y - 220, future[1].y - 220);
  const bottom = Math.max(players[0].y, players[1].y,
    future[0].y, future[1].y);
  const maxWidth = Math.max(worldRight - worldLeft,
    (floorY - ceilingY) * cameraAspect);
  const horizontalPadding = clamp(260 + cameraAspect * 160, 340, 620);
  const verticalPadding = clamp(120 + cameraAspect * 55, 155, 230);
  const minimumWidth = clamp(900 * cameraAspect / 2.1, 480, 900);
  const desiredWidth = Math.max(minimumWidth, Math.min(maxWidth,
    Math.max(right - left + horizontalPadding,
      (bottom - top + verticalPadding) * cameraAspect)));
  const widthSpeed = desiredWidth > cameraWidth ? 13 : 5.5;
  const widthBlend = 1 - Math.exp(-Math.max(0, dt) * widthSpeed);
  cameraWidth += (desiredWidth - cameraWidth) * widthBlend;
  const halfWidth = cameraWidth / 2;
  const halfHeight = cameraWidth / cameraAspect / 2;
  let desiredCenter = cameraWidth >= worldRight - worldLeft
    ? (worldLeft + worldRight) / 2
    : Math.max(worldLeft + halfWidth,
      Math.min(worldRight - halfWidth, (left + right) / 2));
  let desiredCenterY = halfHeight * 2 >= floorY - ceilingY
    ? (ceilingY + floorY) / 2
    : Math.max(ceilingY + halfHeight,
      Math.min(floorY - halfHeight, (top + bottom) / 2));
  // Fold containment into the target before easing. Clamping the live camera
  // after easing caused a one-frame reset whenever a fighter crossed the safe
  // edge; predictive width now absorbs that motion while the center remains
  // continuous.
  const containLeft = right + 350 - halfWidth;
  const containRight = left - 350 + halfWidth;
  if (containLeft <= containRight)
    desiredCenter = clamp(desiredCenter, containLeft, containRight);
  const containTop = bottom + 130 - halfHeight;
  const containBottom = top - 130 + halfHeight;
  if (containTop <= containBottom)
    desiredCenterY = clamp(desiredCenterY, containTop, containBottom);
  const centerBlend = 1 - Math.exp(-Math.max(0, dt) * 9);
  cameraCenter += (desiredCenter - cameraCenter) * centerBlend;
  cameraCenterY += (desiredCenterY - cameraCenterY) * centerBlend;
}

function updateCameraDoll(dt, now) {
  const introAge = now - roundStartedAt;
  if (roundResult) {
    const age = Math.max(0, (now - roundOverAt) / 1000000);
    if (deathCinematic && age < 1.45) {
      if (age < .11) return;
      const loser = players[deathCinematic.loserPad];
      const winner = players[deathCinematic.winnerPad];
      const loserHead = loser.frozenGeometry?.head ||
        runnerWorldGeometry(loser, (now - startedAt) / 1000000).head;
      const winnerHead = winner?.frozenGeometry?.head || (winner
        ? runnerWorldGeometry(winner, (now - startedAt) / 1000000).head
        : { x: cameraCenter, y: cameraCenterY, z: 0 });
      if (age < .86) {
        cameraDoll.track({ target: loserHead,
          position: { x: winnerHead.x, y: winnerHead.y,
            z: winnerHead.z - 720 }, width: 680,
          perspective: .82, fov: 48, roll: 0 }, dt, 11);
        return;
      }
      const returnAmount = clamp((age - .86) / .59, 0, 1);
      const midpoint = { x: (players[0].x + players[1].x) / 2,
        y: (players[0].y + players[1].y) / 2 - 95,
        z: (players[0].z + players[1].z) / 2 };
      const span = Math.max(900, Math.abs(players[1].x - players[0].x) + 540);
      cameraDoll.track({ target: midpoint,
        position: { x: lerp(winnerHead.x, midpoint.x, returnAmount),
          y: lerp(winnerHead.y, midpoint.y - span * .08, returnAmount),
          z: lerp(winnerHead.z - 720, midpoint.z - span * 1.2, returnAmount) },
        width: lerp(680, span, returnAmount),
        perspective: lerp(.82, .72, returnAmount), fov: 50, roll: 0 }, dt, 10);
      return;
    }
    const target = { x: (players[0].x + players[1].x) / 2,
      y: (players[0].y + players[1].y) / 2 - 95,
      z: (players[0].z + players[1].z) / 2 };
    const horizontalSpan = Math.abs(players[1].x - players[0].x);
    const verticalSpan = Math.abs(players[1].y - players[0].y) * cameraAspect;
    const closeWidth = Math.max(820, horizontalSpan + 540, verticalSpan + 520);
    const orbit = Math.sin(age * .72) * closeWidth * .075;
    cameraDoll.track({ target,
      position: { x: target.x + orbit,
        y: target.y - closeWidth * .08 + Math.sin(age * .8) * closeWidth * .035,
        z: target.z - closeWidth * 1.2 },
      width: closeWidth, perspective: clamp(age / .7, 0, .72), fov: 50,
      roll: Math.sin(age * .9) * .008 }, dt, 7);
    return;
  }
  if (introAge < introDurationUs) {
    const age = introAge / 1000000;
    const progress = clamp(age / (introDurationUs / 1000000), 0, 1);
    const eased = progress * progress * (3 - progress * 2);
    const target = { x: (players[0].x + players[1].x) / 2,
      y: (players[0].y + players[1].y) / 2 - 90,
      z: (players[0].z + players[1].z) / 2 };
    const span = Math.max(980, Math.abs(players[1].x - players[0].x) + 760);
    const angle = lerp(-.3, .3, eased);
    cameraDoll.track({ target,
      position: { x: target.x + Math.sin(angle) * span * .34,
        y: target.y - lerp(90, 150, eased),
        z: target.z - Math.cos(angle) * span * .72 },
      width: span, perspective: clamp(progress / .34, 0, 1), fov: 50 }, dt, 7);
    return;
  }
  const target = { x: cameraCenter, y: cameraCenterY, z: 0 };
  // Measure complete animated silhouettes before rendering. This used to run
  // as a final paint-time correction, which made the viewport skip a frame at
  // the safe-zone edge.
  const containmentWidth = fighterContainmentRequiredWidth(
    (now - startedAt) / 1000000) * 1.08;
  cameraContainFloor = Math.max(cameraContainFloor, containmentWidth);
  // A small overscan absorbs animated hands, feet, and perspective before
  // they reach the action-safe edge without loosening the close fight shot.
  const naturalWidth = cameraWidth * 1.04;
  // Hysteresis prevents a fighter hovering at the safe edge from repeatedly
  // switching between close and wide framing.
  if (cameraContainFloor > naturalWidth &&
      naturalWidth < cameraContainFloor * .84) {
    const release = 1 - Math.exp(-Math.max(0, dt) * 1.6);
    cameraContainFloor = lerp(cameraContainFloor, naturalWidth, release);
  }
  const framedWidth = Math.max(naturalWidth, cameraContainFloor);
  // Gameplay camera is intentionally inertial but not handheld. No procedural
  // swivel, roll, or dolly motion is allowed to move a stationary viewport.
  const tilt = .026;
  const dolly = 1.35;
  cameraDoll.track({ target,
    position: { x: cameraCenter,
      y: cameraCenterY - framedWidth * tilt, z: -framedWidth * dolly },
      width: framedWidth, perspective: .1, fov: 55,
      roll: 0 }, dt, 10);
}

function freezeFinalFrame(now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const player of players) {
    player.frozenGeometry = runnerWorldGeometry(player, poseTime);
    player.frozenAt = now;
  }
  impactHitboxesUntil = Math.max(impactHitboxesUntil,
    now + (matchOver ? matchResultUs : roundResultUs));
}

function finishRound(now) {
  if (roundResult) return;
  freezeFinalFrame(now);
  captureRoundReplay(now, true);
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
  impactHitboxesUntil = Math.max(impactHitboxesUntil,
    now + (matchOver ? matchResultUs : roundResultUs));
  roundOverAt = now;
  for (const player of players) player.vx = 0;
  playDrum("clap", 1.2, roundPan);
  captureFrameTelemetry(now, true);
  saveRoundReplay(now);
  if (matchOver) finishReplay();
}

function resultCardText() {
  if (roundResult === "TIE") return { winner: "tie", action: "" };
  const encoded = roundResult.match(/^(@\S+)\s+WINS\b/i);
  const winner = encoded?.[1] ||
    (players[0].score > players[1].score ? players[0].name : players[1].name);
  const actions = { BALLED: "balled", KO: "knocked out", TRADE: "trade",
    TIME: "time", ROUND: "" };
  return { winner: winner.toLowerCase() + " wins!",
    action: actions[roundCause] ?? roundCause.toLowerCase() };
}

function quantizedInput(pad, suppressed = []) {
  const held = pad.down;
  const active = (button) => held.includes(button) && !suppressed.includes(button);
  let horizontal = (active("ArrowRight") ? 1 : 0) -
    (active("ArrowLeft") ? 1 : 0);
  let vertical = (active("ArrowUp") ? 1 : 0) -
    (active("ArrowDown") ? 1 : 0);
  if (!horizontal && Math.abs(pad.leftX) >= 0.48) horizontal = pad.leftX > 0 ? 1 : -1;
  if (!vertical && Math.abs(pad.leftY) >= 0.48) vertical = pad.leftY > 0 ? 1 : -1;
  return { horizontal, vertical };
}

function remember(player, button) {
  player.lastButton = buttonLabel(button);
  player.lastButtonAt = runtime().monotonicUs;
  if (["ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight",
      "A", "B", "X", "Y"].includes(button))
    recordCommand(player, player.lastButton, player.lastButtonAt);
  telemetry("FIGHT_BUTTON", player.name + " " + player.lastButton);
}

function recordCommand(player, label, now) {
  const previous = player.commandStream.at(-1);
  if (previous?.label === label && now - previous.at < 40000) return;
  player.commandStream.push({ label, at: now });
  if (player.commandStream.length > 8) player.commandStream.shift();
}

function fireGun(player, input) {
  const now = runtime().monotonicUs;
  const aimX = input.horizontal || player.facing;
  const aimY = -input.vertical;
  const length = Math.hypot(aimX, aimY) || 1;
  const dx = aimX / length;
  const dy = aimY / length;
  bullets.push({
    x: player.x + dx * 180,
    y: player.y - (player.ducking ? 75 : 130) + dy * 80,
    z: player.z, vx: dx * 2600, vy: dy * 2600,
    owner: player.pad, life: 1.8,
  });
  while (bullets.length > 24) bullets.shift();
  player.gunAmmo -= 1;
  player.itemAction = "FIRE";
  player.itemActionStartedAt = now;
  player.itemActionUntil = now + 170000;
  player.pendingMoveLabel = "FIRE " + player.gunAmmo;
  playDrum("hat", 1.05, panPlayer(player));
  emitSignal("bullet", player.pad, aimX, aimY);
}

function throwGrenade(player) {
  const now = runtime().monotonicUs;
  grenades.push({ x: player.x + player.facing * 150,
    y: player.y - (player.ducking ? 80 : 145), z: player.z,
    vx: player.facing * 1850, vy: -720, owner: player.pad,
    fuse: 1.15, alive: true, exploding: false, blastAge: 0, blastRadius: 0,
    hitPlayers: 0 });
  while (grenades.length > 12) grenades.shift();
  player.grenadeAmmo -= 1;
  player.itemAction = "THROW";
  player.itemActionStartedAt = now;
  player.itemActionUntil = now + 260000;
  player.pendingMoveLabel = "GRENADE " + player.grenadeAmmo;
  playDrum("kick", .95, panPlayer(player));
  emitSignal("grenade", player.pad, player.facing, player.ducking ? 1 : 0);
}

function updateGunPickups(now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const pickup of gunPickups) {
    if (!pickup.active) continue;
    for (const player of players) {
      if (!player.alive || runnerDistanceToPoint(player, poseTime,
        pickup.x, pickup.y, pickup.z) > 90) continue;
      player.gunAmmo = Math.min(12, player.gunAmmo + pickup.amount);
      pickup.active = false;
      remember(player, "GUN +" + pickup.amount);
      playDrum("clap", 1.1, panPlayer(player));
      emitSignal("pickup", player.pad, 1, pickup.amount);
      break;
    }
  }
}

function updateGrenadePickups(now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const pickup of grenadePickups) {
    if (!pickup.active) continue;
    for (const player of players) {
      if (!player.alive || runnerDistanceToPoint(player, poseTime,
        pickup.x, pickup.y, pickup.z) > 90) continue;
      player.grenadeAmmo = Math.min(4, player.grenadeAmmo + pickup.amount);
      pickup.active = false;
      remember(player, "GRENADE +" + pickup.amount);
      playDrum("clap", 1.1, panPlayer(player));
      emitSignal("pickup", player.pad, 2, pickup.amount);
      break;
    }
  }
}

function updatePowerups(now) {
  while (roundElapsedUs >= nextPowerupAtUs) {
    const occupied = [...gunPickups, ...grenadePickups]
      .some((pickup) => pickup.active);
    if (!occupied) {
      const choices = [gunPickups[0], grenadePickups[0]];
      const pickup = choices[powerupSequence % choices.length];
      pickup.active = true;
      pickup.x = (platformLeft + platformRight) / 2;
      pickup.y = platformY - 70;
      pickup.z = 0;
      powerupSequence += 1;
      emitSignal("powerup", -1, powerupSequence, nextPowerupAtUs / 1000000);
      playDrum("clap", .9, 0);
    }
    nextPowerupAtUs += powerupIntervalUs;
  }
  updateGunPickups(now);
  updateGrenadePickups(now);
}

function updateBullets(dt, now) {
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    bullet.vx += windAcceleration * .12 * dt;
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
      if (Math.abs(a.x - b.x) <= 96 && Math.abs(a.y - b.y) <= 52 &&
          Math.abs(a.z - b.z) <= 48) {
        a.life = 0;
        b.life = 0;
        impacts.push({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2,
          z: (a.z + b.z) / 2, life: .18, duration: .18,
          death: false, explosion: false });
        playDrum("hat", 1, 0);
        emitSignal("cancel", -1, a.owner, b.owner);
        break;
      }
    }
  }
  const poseTime = (now - startedAt) / 1000000;
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const target = players[bullet.owner === 0 ? 1 : 0];
    if (!target.alive) continue;
    const contact = runnerContactToPoint(target, poseTime,
      bullet.x, bullet.y, bullet.z);
    if (Math.min(contact.headDistance, contact.bodyDistance) <= 24) {
      bullet.life = 0;
      impacts.push({ x: bullet.x, y: bullet.y, z: bullet.z,
        life: .2, duration: .2, death: contact.headDistance <= 24,
        explosion: false });
      impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 350000);
      if (contact.headDistance <= 24)
        killPlayer(target, bullet.owner, now, "SHOT");
      else {
        applyBodyHit(target, contact.segmentIndex,
          bullet.x - bullet.vx, bullet.owner, now, 1180, 125);
        playDrum("block", .9, panPlayer(target));
      }
    }
  }
  for (let index = bullets.length - 1; index >= 0; index--)
    if (bullets[index].life <= 0) bullets.splice(index, 1);
}

function updateGrenades(dt, now) {
  for (const grenade of grenades) {
    if (!grenade.alive) continue;
    if (grenade.exploding) {
      grenade.blastAge += dt;
      grenade.blastRadius = grenadeBlastRadius *
        Math.min(1, grenade.blastAge / grenadeBlastDuration);
      const poseTime = (now - startedAt) / 1000000;
      for (const player of players) {
        if (!player.alive || (grenade.hitPlayers & (1 << player.pad))) continue;
        const contact = runnerContactToPoint(player, poseTime,
          grenade.x, grenade.y, grenade.z);
        if (Math.min(contact.headDistance, contact.bodyDistance) >
            grenade.blastRadius) continue;
        grenade.hitPlayers |= 1 << player.pad;
        if (contact.headDistance <= grenade.blastRadius)
          killPlayer(player, grenade.owner, now, "BLASTED");
        else {
          const blastForce = 1250 + 850 *
            (1 - clamp(contact.bodyDistance / grenadeBlastRadius, 0, 1));
          applyBodyHit(player, contact.segmentIndex, grenade.x,
            grenade.owner, now, blastForce, 300);
        }
      }
      if (grenade.blastAge >= grenadeBlastDuration) grenade.alive = false;
      continue;
    }
    const previousY = grenade.y;
    grenade.vx += windAcceleration * .45 * dt;
    grenade.vy += 1800 * dt;
    grenade.x += grenade.vx * dt;
    grenade.y += grenade.vy * dt;
    grenade.fuse -= dt;
    const inset = wallThickness + 35;
    if (grenade.x < worldLeft + inset) {
      grenade.x = worldLeft + inset;
      grenade.vx = Math.abs(grenade.vx) * .65;
    } else if (grenade.x > worldRight - inset) {
      grenade.x = worldRight - inset;
      grenade.vx = -Math.abs(grenade.vx) * .65;
    }
    if (grenade.y < ceilingY + inset) {
      grenade.y = ceilingY + inset;
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
      playDrum("kick", 1.25, panPlayer(players[grenade.owner]));
      emitSignal("blast", grenade.owner,
        grenade.x / worldRight, grenade.y / floorY);
    }
  }
  for (let index = grenades.length - 1; index >= 0; index--)
    if (!grenades[index].alive) grenades.splice(index, 1);
}

function startMelee(player, kind, now) {
  if (isHeadOnly(player) || isPogo(player)) return;
  const attackingPart = kind === "KICK"
    ? player.facing > 0 ? "right-leg" : "left-leg"
    : player.facing > 0 ? "right-arm" : "left-arm";
  if (!hasPart(player, attackingPart)) return;
  player.attackKind = kind;
  player.attackStartedAt = now;
  player.attackUntil = now + 220000;
  player.attackHit = false;
  player.stance = "ATTACK";
  player.pendingMoveLabel = kind;
  const pan = panPlayer(player);
  playDrum(kind === "KICK" ? "kick" : "snare", 1.05, pan);
  emitSignal(kind.toLowerCase(), player.pad, player.facing, 0);
}

const limbParts = ["left-arm", "right-arm", "left-leg", "right-leg"];
const hasPart = (player, part) => !player.removedParts?.includes(part);
const isPogo = (player) => hasPart(player, "torso") &&
  limbParts.every((part) => !hasPart(player, part));
const isHeadOnly = (player) => !hasPart(player, "torso");

function meleePulse(player, now) {
  if (now >= player.attackUntil || player.attackUntil <= player.attackStartedAt) return 0;
  const phase = (now - player.attackStartedAt) /
    (player.attackUntil - player.attackStartedAt);
  return Math.sin(Math.max(0, Math.min(1, phase)) * Math.PI);
}

function meleeTarget(player, now) {
  const pulse = meleePulse(player, now);
  const kick = player.attackKind === "KICK";
  return {
    x: player.x + player.facing *
      (kick ? 75 + 62 * pulse : 58 + 50 * pulse),
    y: player.y - (kick ? 55 : 115), z: player.z,
  };
}

function itemActionPulse(player, now) {
  if (!player.itemAction || now >= player.itemActionUntil) return 0;
  const phase = (now - player.itemActionStartedAt) /
    Math.max(1, player.itemActionUntil - player.itemActionStartedAt);
  return Math.sin(clamp(phase, 0, 1) * Math.PI);
}

function itemHandTarget(player, now) {
  const pulse = itemActionPulse(player, now);
  if (player.itemAction === "THROW") return {
    x: player.x + player.facing * (42 + 52 * pulse),
    y: player.y - 118 - 52 * pulse, z: player.z,
  };
  return { x: player.x + player.facing * 108,
    y: player.y - 115, z: player.z };
}

function meleeStrike(player, now) {
  const target = meleeTarget(player, now);
  return {
    x: target.x, y: target.y, z: target.z,
    radius: player.attackKind === "KICK" ? 35 : 28,
  };
}

function shieldGeometry(player) {
  return {
    x: player.x + player.facing * shieldForward,
    y: player.y - 90,
    z: player.z,
    radius: shieldRadius,
  };
}

function returnBall(ball, player, now, shielded, intensity = 1) {
  const incomingVx = ball.vx;
  const incomingVy = ball.vy;
  const direction = ball.x >= player.x ? 1 : -1;
  const currentSpeed = Math.hypot(ball.vx, ball.vy);
  const response = ball.hitScale || 1;
  const normalSpeed = (currentSpeed * 1.34 + 720) * intensity * response;
  const shieldMinimum = (1800 + clamp(intensity, 0, 1) * 1000) * response;
  const speed = shielded
    ? Math.min(4200 * response, Math.max(shieldMinimum, normalSpeed * 1.65))
    : Math.min(4800, normalSpeed);
  ball.vx = direction * speed;
  const lift = shielded ? .72 : player.inputY > 0 ? .58
    : player.attackKind === "KICK" ? .34 : .2;
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
  impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 350000);
  playDrum(shielded ? "block" : "clap", shielded ? 1.1 : 1.25,
    panAt(ball.x, ball.z));
  emitSignal(shielded ? "ballblock" : "wack", player.pad,
    direction, Math.round(speed));
}

function crossWackBall(ball, hitters, now) {
  const first = hitters[0];
  const second = hitters[1];
  const fighterDistance = Math.abs(first.player.x - second.player.x);
  const convergence = clamp(1 - fighterDistance / 520, 0, 1);
  const contact = (first.contact + second.contact) * .5;
  const currentSpeed = Math.hypot(ball.vx, ball.vy);
  const speed = Math.min(7600, (4100 + currentSpeed * .72 +
    convergence * 2300 + contact * 900) * (ball.hitScale || 1));
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
  impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 350000);
  playDrum("clap", 1.55, panAt(ball.x, ball.z));
  emitSignal("crosswack", -1, direction, Math.round(speed));
}

function bootBall(ball, player, now) {
  // Walking contact carries the grounded ball in the player's travel
  // direction. A melee strike remains the intentionally stronger launch.
  const direction = Math.sign(player.vx) || Math.sign(ball.x - player.x) ||
    player.facing || 1;
  const speed = Math.min(1450, (420 + Math.abs(player.vx) * .32) *
    (ball.hitScale || 1));
  ball.vx = direction * speed;
  ball.vy = -Math.max(80, speed * .06);
  ball.x = player.x + direction * (ball.radius + 58);
  ball.y = Math.min(ball.y, floorY - ball.radius - 2);
  ball.lastHitBy = player.pad;
  ball.safeUntil = now + 180000;
  ball.safePlayers = 1 << player.pad;
  player.lastButton = "BOOT";
  player.lastButtonAt = now;
  impacts.push({ x: ball.x, y: ball.y, z: ball.z,
    life: .16, duration: .16, death: false, explosion: false });
  impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 300000);
  playDrum("kick", 1.12, panAt(ball.x, ball.z));
  emitSignal("boot", player.pad, direction, Math.round(speed));
}

function bounceBallOffBody(ball, player, now, segmentIndex = -1) {
  const direction = Math.sign(ball.x - player.x) || -Math.sign(ball.vx) ||
    player.facing || 1;
  const incomingSpeed = Math.hypot(ball.vx, ball.vy);
  const speed = Math.max(760, Math.min(3600, incomingSpeed * .86 /
    Math.sqrt(ball.mass || 1)));
  ball.vx = direction * speed;
  ball.vy = -Math.max(260, Math.abs(ball.vy) * .42 + speed * .12);
  ball.x += direction * (ball.radius + 18);
  ball.lastHitBy = player.pad === 0 ? 1 : 0;
  ball.safeUntil = now + 160000;
  ball.safePlayers = 1 << player.pad;
  applyBodyHit(player, segmentIndex, ball.x - ball.vx,
    ball.lastHitBy, now, 760, 110, false);
  impacts.push({ x: ball.x, y: ball.y, z: ball.z,
    life: .16, duration: .16, death: false, explosion: false });
  impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 300000);
  playDrum("block", .82, panAt(ball.x, ball.z));
  emitSignal("bodybounce", player.pad, direction, Math.round(speed));
}

function updateBall(ball, dt, now) {
  if (!ball.active || now < ball.serveAt) return;
  if (ball.heldBy >= 0) {
    const carrier = players[ball.heldBy];
    if (!carrier?.alive || carrier.heldBall !== balls.indexOf(ball)) {
      ball.heldBy = -1;
    } else {
      ball.x = carrier.x + carrier.facing * (ball.radius + 54);
      ball.y = carrier.y - (carrier.ducking ? 58 : 94);
      ball.z = carrier.z - 6;
      ball.vx = carrier.vx;
      ball.vy = carrier.vy;
      return;
    }
  }
  const platformSupported = ball.x >= platformLeft + ball.radius &&
    ball.x <= platformRight - ball.radius &&
    ball.y >= platformY - ball.radius - 2 &&
    ball.y <= platformY - ball.radius + 2;
  const floorSupported = ball.y >= floorY - ball.radius - 2;
  const grounded = (platformSupported || floorSupported) && Math.abs(ball.vy) < 180;
  if (!grounded) ball.vx += windAcceleration * (ball.windFactor || .45) * dt;
  ball.vy += 1900 * dt;
  const previous = { x: ball.x, y: ball.y, z: ball.z };
  ball.x += ball.vx * dt;
  ball.y += ball.vy * dt;
  if (Math.abs(ball.vx) > 20)
    ball.rotation += ball.vx * dt / Math.max(1, ball.radius);
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
  }
  const platformTop = platformY - ball.radius;
  if (ball.vy >= 0 && previous.y <= platformTop && ball.y >= platformTop &&
      ball.x >= platformLeft + ball.radius &&
      ball.x <= platformRight - ball.radius) {
    ball.y = platformTop;
    ball.vy = Math.abs(ball.vy) > 180
      ? -Math.abs(ball.vy) * (ball.bounce || .58) : 0;
    ball.vx *= ball.drag || .992;
  } else if (ball.y > floorY - ball.radius) {
    ball.y = floorY - ball.radius;
    ball.vy = Math.abs(ball.vy) > 180
      ? -Math.abs(ball.vy) * (ball.bounce || .62) : 0;
    ball.vx *= ball.drag || .992;
  }
  const onSurface = ((ball.x >= platformLeft + ball.radius &&
    ball.x <= platformRight - ball.radius &&
    Math.abs(ball.y - (platformY - ball.radius)) <= 2) ||
    ball.y >= floorY - ball.radius - 2) && Math.abs(ball.vy) < 180;
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
    crossWackBall(ball, hitters, now);
    return;
  }
  if (hitters.length === 1) {
    returnBall(ball, hitters[0].player, now, false,
      .92 + hitters[0].contact * .38);
    return;
  }
  for (const player of players) {
    if (!player.alive || ((ball.safePlayers & (1 << player.pad)) &&
        now < ball.safeUntil))
      continue;
    if (player.blocking) {
      const shield = shieldGeometry(player);
      const centerDistance = Math.hypot(ball.x - shield.x, ball.y - shield.y,
        ball.z - shield.z);
      const surfaceDistance = Math.max(0, centerDistance - ball.radius);
      if (surfaceDistance <= shield.radius) {
        const proximity = clamp(1 - surfaceDistance / shield.radius, 0, 1);
        returnBall(ball, player, now, true, proximity);
        return;
      }
    }
    const geometry = runnerWorldGeometry(player, poseTime);
    const currentHeadDistance = Math.max(0, Math.hypot(
      ball.x - geometry.head.x, ball.y - geometry.head.y,
      ball.z - geometry.head.z) - geometry.head.radius);
    const sweptHeadDistance = Math.max(0, pointSegmentDistance(
      geometry.head.x, geometry.head.y, geometry.head.z,
      { x1: previous.x, y1: previous.y, z1: previous.z,
        x2: ball.x, y2: ball.y, z2: ball.z }) - geometry.head.radius);
    const headDistance = Math.min(currentHeadDistance, sweptHeadDistance);
    const bodyContact = runnerContactToPoint(player, poseTime,
      ball.x, ball.y, ball.z);
    const bodyDistance = bodyContact.bodyDistance;
    if (Math.min(headDistance, bodyDistance) > ball.radius) continue;
    const runningContact = player.grounded && Math.abs(player.vx) > 40 &&
      (onSurface || ball.y >= player.y - ball.radius - 55);
    if (runningContact) {
      bootBall(ball, player, now);
      return;
    }
    if (headDistance <= ball.radius) {
      const sourcePad = ball.lastHitBy >= 0 && ball.lastHitBy !== player.pad
        ? ball.lastHitBy : player.pad === 0 ? 1 : 0;
      damageAllLimbs(player, ball.x - ball.vx, sourcePad, now);
      bounceBallOffBody(ball, player, now, -1);
      player.lastButton = "HEAD HIT";
      player.lastButtonAt = now;
      return;
    }
    bounceBallOffBody(ball, player, now, bodyContact.segmentIndex);
    return;
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
  playDrum("clap", 1.05, panPlayer(player));
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
    player.dashVx = player.facing * 2400;
    player.dashUntil = now + 110000;
    emitSignal("dash", player.pad, player.facing, 0);
  }
}

function killPlayer(target, killerPad, now, cause = "KO") {
  if (!target.alive) return;
  releaseCarriedBall(target, now);
  freezeFinalFrame(now);
  if (!deathCinematic && killerPad !== target.pad)
    deathCinematic = { startedAt: now, loserPad: target.pad,
      winnerPad: killerPad, cause };
  target.alive = false;
  target.headBustedAt = now;
  target.respawnAt = now + 1200000;
  target.vx = 0;
  target.vy = 0;
  target.stance = "HIT";
  target.lastButton = cause;
  target.lastButtonAt = now;
  if (killerPad !== target.pad) players[killerPad].score += 1;
  emitSignal(cause === "BALLED" ? "balled" : "ko",
    killerPad, target.pad, players[killerPad]?.score || 0);
  roundCause = cause;
  impacts.push({ x: target.x, y: target.y - 120, z: target.z, life: .55,
    duration: .55, death: true, explosion: false });
  playDrum("whoosh", 1.15, panPlayer(target));
  emitSignal("killcam", killerPad, target.pad, 1);
  playDrum("snare", 1.15, panPlayer(target));
}

function resolveMelee(now) {
  const poseTime = (now - startedAt) / 1000000;
  const contacts = [];
  for (const attacker of players) {
    if (!attacker.alive || attacker.attackHit || now >= attacker.attackUntil) continue;
    const target = players[attacker.pad === 0 ? 1 : 0];
    if (!target.alive) continue;
    const contact = meleeLimbContact(attacker, target, poseTime);
    if (contact?.separation <= 3) {
      attacker.attackHit = true;
      contacts.push({ attacker, target,
        strike: { x: contact.x, y: contact.y, z: contact.z },
        headshot: contact.headshot, segmentIndex: contact.segmentIndex });
    }
  }
  for (const { attacker, target, strike, headshot, segmentIndex } of contacts) {
    if (!target.alive && contacts.length < 2) continue;
    impacts.push({ x: strike.x, y: strike.y, z: strike.z,
      life: .2, duration: .2, death: false, explosion: false });
    impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 350000);
    const away = Math.sign(target.x - attacker.x) || -attacker.facing;
    const backBlocking = target.inputX === away;
    if (target.blocking || backBlocking) {
      target.stance = "DEFEND";
      target.blockFlash = 1;
      target.lastButton = target.blocking ? "BLOCK" : "BACK BLOCK";
      target.lastButtonAt = now;
      // An explicit shield returns the attacker's commitment as durable
      // knockback. Keep it separate from controlled velocity so holding toward
      // the defender cannot erase the recoil on the next simulation frame.
      const recoil = target.blocking ? 2400 : 900;
      attacker.dashUntil = 0;
      attacker.dashVx = 0;
      attacker.knockVx = -attacker.facing * recoil;
      attacker.vx = attacker.knockVx + attacker.windVx;
      if (target.blocking) attacker.vy = Math.min(attacker.vy, -260);
      else target.vx = 0;
      playDrum("block", 1.2, panPlayer(target));
      emitSignal("block", target.pad, attacker.pad, target.blocking ? 1 : 2);
    } else if (headshot) killPlayer(target, attacker.pad, now,
      contacts.length >= 2 ? "TRADE" : "KO");
    else {
      const force = attacker.attackKind === "KICK" ? 1550 : 1200;
      applyBodyHit(target, segmentIndex, attacker.x,
        attacker.pad, now, force, attacker.attackKind === "KICK" ? 220 : 140);
      playDrum("block", 1, panPlayer(target));
    }
  }
}

function resolvePogoAttacks(now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const attacker of players) {
    if (!attacker.alive || !isPogo(attacker) || attacker.grounded ||
        attacker.pogoHit) continue;
    const geometry = runnerWorldGeometry(attacker, poseTime);
    const torso = geometry.segments.find((segment) => segment.role === "torso");
    if (!torso) continue;
    const bottom = { x1: torso.x2, y1: torso.y2, z1: torso.z2,
      x2: torso.x2, y2: torso.y2, z2: torso.z2,
      width: torso.width + 6, role: "attack-pogo", part: "torso" };
    const target = players[attacker.pad === 0 ? 1 : 0];
    if (!target.alive) continue;
    const contact = attackCapsuleContact([bottom], target, poseTime);
    if (!contact || contact.separation > 3) continue;
    attacker.pogoHit = true;
    attacker.vy = Math.min(attacker.vy, -620);
    attacker.lastButton = "POGO";
    attacker.lastButtonAt = now;
    impacts.push({ x: contact.x, y: contact.y, z: contact.z,
      life: .2, duration: .2, death: contact.headshot, explosion: false });
    if (contact.headshot) killPlayer(target, attacker.pad, now, "POGO");
    else applyBodyHit(target, contact.segmentIndex, attacker.x,
      attacker.pad, now, 1350, 260);
    playDrum("kick", 1.1, panPlayer(attacker));
    emitSignal("pogo", attacker.pad, contact.headshot ? 1 : 0, 0);
  }
}

function resolvePlayerPushboxes() {
  if (!players[0].alive || !players[1].alive) return;
  const poseTime = (runtime().monotonicUs - startedAt) / 1000000;
  const firstBounds = runnerBounds(players[0], poseTime);
  const secondBounds = runnerBounds(players[1], poseTime);
  const verticalOverlap = Math.min(firstBounds.bottom, secondBounds.bottom) -
    Math.max(firstBounds.top, secondBounds.top);
  // Grounded fighters nudge one another. Once a jumper is clearly above the
  // other fighter, the pushboxes separate so cross-over jumps are possible.
  if (verticalOverlap <= 18 ||
      ((!players[0].grounded || !players[1].grounded) &&
       Math.abs(players[0].y - players[1].y) > 58)) return;
  const left = players[0].x <= players[1].x ? players[0] : players[1];
  const right = left === players[0] ? players[1] : players[0];
  const pushRadius = (player) => isHeadOnly(player) ? 24 : isPogo(player) ? 38 : 69;
  const minimumGap = pushRadius(left) + pushRadius(right);
  const overlap = minimumGap - (right.x - left.x);
  if (overlap <= 0) return;
  const leftAdvancing = left.vx > 30;
  const rightAdvancing = right.vx < -30;
  const leftShare = leftAdvancing && !rightAdvancing ? .25
    : rightAdvancing && !leftAdvancing ? .75 : .5;
  left.x -= overlap * leftShare;
  right.x += overlap * (1 - leftShare);
  if (leftAdvancing && !rightAdvancing) {
    right.knockVx += Math.min(180, left.vx * .12);
    left.vx *= .35;
  } else if (rightAdvancing && !leftAdvancing) {
    left.knockVx += Math.max(-180, right.vx * .12);
    right.vx *= .35;
  } else {
    if (left.vx > 0) left.vx *= .35;
    if (right.vx < 0) right.vx *= .35;
  }
  resolveRunnerBounds(left, 0);
  resolveRunnerBounds(right, 0);
}

function resolvePlayerStanding(now) {
  const poseTime = (now - startedAt) / 1000000;
  const previousStanding = players.map((player) => player.standingOn);
  for (const player of players) player.standingOn = -1;
  for (const rider of players) {
    const base = players[rider.pad === 0 ? 1 : 0];
    if (!rider.alive || !base.alive || rider.vy < 0) continue;
    const head = runnerWorldGeometry(base, poseTime).head;
    const top = head.y - head.radius;
    const horizontal = Math.abs(rider.x - head.x) <= head.radius + 42;
    const depth = Math.abs(rider.z - head.z) <= head.radius + 32;
    const crossed = (rider.previousY ?? rider.y) <= top + 8 &&
      rider.y >= top - 3;
    const stayed = previousStanding[rider.pad] === base.pad &&
      rider.y <= top + 34;
    if (!horizontal || !depth || (!crossed && !stayed)) continue;
    const wasGrounded = rider.grounded;
    rider.y = top;
    rider.vy = Math.min(0, base.vy);
    rider.grounded = true;
    rider.standingOn = base.pad;
    if (!wasGrounded) rider.landPoseUntil = now + 110000;
  }
}

function updateStance(player, input, now) {
  const opponent = players[player.pad === 0 ? 1 : 0];
  const toward = Math.sign(opponent.x - player.x) || player.facing || 1;
  player.stance = !player.alive ? "HIT"
    : now < player.hitStunUntil ? "STUN"
    : player.blocking ? "DEFEND"
    : player.heldBall >= 0 ? "HOLDING"
    : player.grabHeld ? "REACHING"
    : player.attackKind ? "ATTACK"
    : player.ducking ? "CROUCH"
    : !player.grounded ? "AIR"
    : now < player.dashUntil ? "DASH"
    : input.horizontal === toward ? "ADVANCE"
    : input.horizontal === -toward ? "RETREAT"
    : "NEUTRAL";
}

function grabNearestBall(player, now) {
  let nearest = null;
  let nearestDistance = 240;
  for (let index = 0; index < balls.length; index++) {
    const item = balls[index];
    if (!item.active || item.heldBy >= 0) continue;
    const distance = Math.hypot(item.x - player.x,
      item.y - (player.y - 82), item.z - player.z);
    if (distance >= nearestDistance) continue;
    nearest = { item, index };
    nearestDistance = distance;
  }
  if (!nearest) return false;
  player.heldBall = nearest.index;
  nearest.item.heldBy = player.pad;
  nearest.item.safeUntil = now + 180000;
  nearest.item.safePlayers = 1 << player.pad;
  player.lastButton = "HOLDING";
  player.lastButtonAt = now;
  emitSignal("grab", player.pad, nearest.index, nearest.item.mass);
  playDrum("clap", .72, panPlayer(player));
  return true;
}

function releaseCarriedBall(player, now) {
  if (player.heldBall < 0) return;
  const item = balls[player.heldBall];
  if (item) {
    item.heldBy = -1;
    item.x = player.x + player.facing * (item.radius + 70);
    item.y = player.y - 88;
    item.z = player.z;
    item.vx = player.vx;
    item.vy = player.vy;
    item.safeUntil = now + 120000;
    item.safePlayers = 1 << player.pad;
    emitSignal("release", player.pad, player.heldBall, item.mass);
  }
  player.heldBall = -1;
}

function updatePlayer(player, pad, dt, now) {
  player.previousY = player.y;
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
      player.stance = "NEUTRAL";
      player.crouchBlend = 0;
      player.jumpLaunchAt = 0;
      player.jumpPoseUntil = 0;
      player.landPoseUntil = 0;
      player.hitSegment = -1;
      player.hitSegmentUntil = 0;
      player.hitStunUntil = 0;
      player.alive = true;
    }
    return;
  }
  player.suppressedDirections = player.suppressedDirections.filter((button) =>
    pad.down.includes(button));
  const headOnly = isHeadOnly(player);
  const pogo = isPogo(player);
  const legCount = ["left-leg", "right-leg"]
    .filter((part) => hasPart(player, part)).length;
  const armCount = ["left-arm", "right-arm"]
    .filter((part) => hasPart(player, part)).length;
  const rawInput = quantizedInput(pad, player.suppressedDirections);
  const hitStunned = now < player.hitStunUntil;
  const wasBlocking = player.blocking;
  player.blocking = !headOnly && pad.down.includes("B");
  if (player.blocking && !wasBlocking) {
    player.shieldVx = player.vx - player.windVx - player.knockVx;
    player.dashUntil = 0;
    player.dashVx = 0;
    player.lastTap = {};
    player.lastRelease = {};
  }
  const input = player.blocking ? { horizontal: 0, vertical: 0 } : rawInput;
  const grabHeld = armCount > 0 && !pogo && !hitStunned && !player.blocking &&
    pad.down.includes("A") && pad.down.includes("X");
  if (grabHeld && !player.grabHeld && player.heldBall < 0) {
    if (!grabNearestBall(player, now)) {
      player.lastButton = "REACHING";
      player.lastButtonAt = now;
      emitSignal("reach", player.pad, player.facing, 0);
    }
  }
  else if (!grabHeld && player.heldBall >= 0)
    releaseCarriedBall(player, now);
  const inputChanged = input.horizontal !== player.inputX ||
    input.vertical !== player.inputY;
  if (inputChanged &&
      (input.horizontal || input.vertical))
    emitSignal("move", player.pad, input.horizontal, input.vertical);
  player.pendingMoveLabel = "";
  const upPressed = input.vertical > 0 && !player.previous.includes("MOVE_UP");
  const crouchTarget = input.vertical < 0 && player.grounded ? 1 : 0;
  const crouchStep = dt * (crouchTarget ? 9 : 11);
  player.crouchBlend += clamp(crouchTarget - player.crouchBlend,
    -crouchStep, crouchStep);
  player.ducking = player.grounded && player.crouchBlend >= .52;
  if (player.attackKind && now >= player.attackUntil) {
    player.attackKind = "";
    player.attackHit = false;
  }
  if (player.itemAction && now >= player.itemActionUntil)
    player.itemAction = "";
  if (player.inputX && input.horizontal !== player.inputX)
    player.lastRelease[player.inputX > 0 ? "RIGHT" : "LEFT"] = now;
  if (player.inputY && input.vertical !== player.inputY)
    player.lastRelease[player.inputY > 0 ? "UP" : "DOWN"] = now;
  if (input.horizontal && input.horizontal !== player.inputX)
    {
      const direction = input.horizontal > 0 ? "RIGHT" : "LEFT";
      recordCommand(player, direction, now);
      directionTap(player, direction, now);
    }
  if (input.vertical && input.vertical !== player.inputY)
    {
      const direction = input.vertical > 0 ? "UP" : "DOWN";
      recordCommand(player, direction, now);
      directionTap(player, direction, now);
    }

  if (input.horizontal) player.facing = input.horizontal;
  // Fighting-game directions are digital: full movement begins and ends on
  // the sampled edge. The analog stick is only an eight-way gate.
  if (player.grounded) player.windVx *= Math.max(0, 1 - dt * 10);
  else player.windVx = clamp(player.windVx + windAcceleration * dt, -900, 900);
  player.knockVx *= Math.max(0, 1 - dt * (player.grounded ? 7 : 1.8));
  if (now < player.dashUntil && input.horizontal &&
      Math.sign(player.dashVx) !== input.horizontal) {
    player.dashUntil = 0;
    player.dashVx = 0;
  }
  const mobility = headOnly ? .55 : pogo ? .68 : legCount === 1 ? .72 : 1;
  const controlledVx = player.blocking ? player.shieldVx || 0
    : now < player.dashUntil && Math.abs(player.dashVx) > 0
    ? player.dashVx
    : player.ducking ? 0 : input.horizontal * walkSpeed * mobility;
  player.vx = controlledVx + player.windVx + player.knockVx;
  if (inputChanged) telemetry("FIGHT_MOVE", player.name +
    " pad=" + (player.pad + 1) +
    " held=" + (pad.down.filter((button) => button.startsWith("Arrow")).join("+") || "NONE") +
    " stick=" + pad.leftX.toFixed(2) + "," + pad.leftY.toFixed(2) +
    " quant=" + input.horizontal + "," + input.vertical +
    " controlled=" + Math.round(controlledVx) +
    " shield=" + (player.blocking ? 1 : 0) +
    " wind=" + Math.round(player.windVx) +
    " knock=" + Math.round(player.knockVx) +
    " vx=" + Math.round(player.vx) +
    " x=" + Math.round(player.x));

  if (!headOnly && upPressed && player.grounded && !player.jumpLaunchAt) {
    player.jumpLaunchAt = now + 85000;
    player.pendingMoveLabel = "JUMP";
  }
  if (player.jumpLaunchAt && now >= player.jumpLaunchAt) {
    player.jumpLaunchAt = 0;
    player.jumpPoseUntil = now + 125000;
    const jumpScale = pogo ? .88 : legCount === 1 ? .78 : 1;
    player.vy = Math.min(player.vy, -1050 * jumpScale);
    player.pogoHit = false;
    player.grounded = false;
    player.ducking = false;
    playDrum("block", 0.72, panPlayer(player));
    emitSignal("jump", player.pad, 1, 0);
  }

  for (const button of pad.down) {
    if (!player.previous.includes(button)) {
      remember(player, button);
      if (button === "B" && !headOnly) {
        player.pendingMoveLabel = "SHIELD";
        playDrum("block", .7, panPlayer(player));
        emitSignal("shield", player.pad, 1, 0);
      }
      else if (!headOnly && !pogo && !hitStunned && !player.blocking &&
          !grabHeld && button === "A") {
        if (player.gunAmmo > 0) fireGun(player, input);
        else startMelee(player, "KICK", now);
      }
      else if (!headOnly && !pogo && !hitStunned && !player.blocking &&
          !grabHeld && button === "X") {
        startMelee(player, "PUNCH", now);
      }
      else if (armCount > 0 && !headOnly && !pogo && !hitStunned &&
          !player.blocking && button === "Y" &&
          player.grenadeAmmo > 0) {
        throwGrenade(player);
      }
    }
  }
  if (grabHeld) {
    player.lastButton = player.heldBall >= 0 ? "HOLDING" : "REACHING";
    player.lastButtonAt = now;
  }
  if (player.pendingMoveLabel) remember(player, player.pendingMoveLabel);

  const previousY = player.y;
  const wasGrounded = player.grounded;
  player.vy += 1900 * dt;
  player.x += player.vx * dt;
  player.y += player.vy * dt;
  player.grounded = false;
  if (player.vy >= 0 && previousY <= platformY && player.y >= platformY &&
      player.x >= platformLeft && player.x <= platformRight) {
    player.y = platformY;
    if (headOnly) {
      player.vy = -690;
      player.grounded = false;
      player.stance = "BOUNCE";
    } else {
      player.vy = 0;
      player.grounded = true;
    }
  } else if (player.y >= floorY) {
    player.y = floorY;
    if (headOnly) {
      player.vy = -690;
      player.grounded = false;
      player.stance = "BOUNCE";
    } else {
      player.vy = 0;
      player.grounded = true;
    }
  }
  if (!wasGrounded && player.grounded) player.landPoseUntil = now + 110000;
  resolveRunnerBounds(player, (now - startedAt) / 1000000);
  player.hit = Math.max(0, player.hit - dt * 4);
  player.blockFlash = Math.max(0, player.blockFlash - dt * 6);
  player.previous = pad.down.slice();
  if (input.vertical > 0) player.previous.push("MOVE_UP");
  player.inputX = input.horizontal;
  player.inputY = input.vertical;
  player.grabHeld = grabHeld;
  updateStance(player, input, now);
}

function botPad(player, opponent, now) {
  const down = [];
  if (!player.bot || !player.alive || !opponent.alive)
    return { connected: true, down, leftX: 0, leftY: 0 };

  const dx = opponent.x - player.x;
  const distance = Math.abs(dx);
  const toward = Math.sign(dx) || player.facing || -1;
  const opponentThreatening = opponent.attackKind &&
    now < opponent.attackUntil && distance < 245 &&
    opponent.facing === -toward;

  if (opponentThreatening && now >= player.botAttackUntil) {
    down.push("B");
  } else {
    if (distance > 155)
      down.push(toward > 0 ? "ArrowRight" : "ArrowLeft");
    if (distance < 225 && now >= player.botAttackAt) {
      player.botAttackButton = player.botAttackSequence++ % 2 ? "A" : "X";
      player.botAttackUntil = now + 70000;
      player.botAttackAt = now + 330000 + (player.botAttackSequence % 3) * 70000;
    }
    if (now < player.botAttackUntil && player.botAttackButton)
      down.push(player.botAttackButton);
  }

  if (player.grounded && opponent.y < player.y - 180 &&
      now >= player.botJumpAt) {
    down.push("ArrowUp");
    player.botJumpAt = now + 1250000;
  }
  return { connected: true, down, leftX: 0, leftY: 0 };
}

function gameSim() {
  syncGameView();
  const now = runtime().monotonicUs;
  const dt = Math.min(0.04, Math.max(0.001, (now - lastSimAt) / 1000000));
  lastSimAt = now;
  if (roundViewer) {
    updateRoundViewer(now, dt);
    return;
  }
  padSnapshots[0] = gamepad(0);
  padSnapshots[1] = gamepad(1);
  if (!selecting && Array.isArray(globalThis.__oskiewarTouch?.taps))
    globalThis.__oskiewarTouch.taps.length = 0;
  if (returnToSelectPressed(now)) return;
  if (debugHitboxes && now >= nextInputDebugAt) {
    nextInputDebugAt = now + 500000;
    const values = players.map((player) => {
      const pad = padSnapshots[player.pad];
      const input = quantizedInput(pad, player.suppressedDirections);
      return "P" + (player.pad + 1) + " down=" +
        (pad.down.join("+") || "NONE") + " stick=" +
        pad.leftX.toFixed(2) + "," + pad.leftY.toFixed(2) + " q=" +
        input.horizontal + "," + input.vertical + " vx=" +
        Math.round(player.vx);
    });
    telemetry("FIGHT_INPUT", values.join(" | "));
  }
  if (shellMode === "MENU") {
    updateShell(now);
    return;
  }
  const opponentPad = players[1].bot
    ? botPad(players[1], players[0], now)
    : players[1].npc
      ? { connected: true, down: [], leftX: 0, leftY: 0 }
      : padSnapshots[1];
  recordReplayCommands(now, [padSnapshots[0], opponentPad]);
  publishSpectator(now);
  if (roundResult) {
    if (instantReplay) {
      updateInstantReplay(now, dt);
      return;
    }
    const replayDown = padSnapshots[0]?.down || [];
    if (replayDown.includes("Y") && !replayOfferPrevious.includes("Y") &&
        startInstantReplay(now)) return;
    replayOfferPrevious = replayDown.slice();
    updateCameraDoll(dt, now);
    captureFrameTelemetry(now);
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
    captureFrameTelemetry(now);
    return;
  }
  roundElapsedUs += dt * 1000000;
  const timedRound = roundIsTimed();
  if (timedRound) {
    const countdownSecond = Math.max(0,
      Math.ceil((roundDurationUs - roundElapsedUs) / 1000000));
    if (countdownSecond > 0 && countdownSecond <= 10 &&
        countdownSecond !== lastCountdownSecond) {
      lastCountdownSecond = countdownSecond;
      playDrum("bell", 1 + (10 - countdownSecond) * .025, 0);
      emitSignal("countdown", -1, countdownSecond, 0);
    }
  }
  updatePlayer(players[0], padSnapshots[0], dt, now);
  updatePlayer(players[1], opponentPad, dt, now);
  resolvePlayerStanding(now);
  resolvePlayerPushboxes();
  updatePowerups(now);
  updateBullets(dt, now);
  updateGrenades(dt, now);
  resolveMelee(now);
  resolvePogoAttacks(now);
  for (const item of balls) updateBall(item, dt, now);
  updateDetachedParts(dt);
  updateCamera(dt);
  updateCameraDoll(dt, now);
  captureFrameTelemetry(now);
  captureRoundReplay(now);
  recordReplayCheckpoint(now);
  for (const impact of impacts) impact.life -= dt;
  while (impacts.length && impacts[0].life <= 0) impacts.shift();
  if (players.some((player) => !player.alive) ||
      (timedRound && roundElapsedUs >= roundDurationUs)) {
    if (timedRound && roundElapsedUs >= roundDurationUs &&
        players.every((player) => player.alive))
      roundCause = "TIME";
    finishRound(now);
  }
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

function filledDisc(x, y, radius, color) {
  const sides = 10;
  for (let side = 0; side < sides; side++) {
    const a = side * Math.PI * 2 / sides;
    const b = (side + 1) * Math.PI * 2 / sides;
    screenTriangle(x, y,
      x + Math.cos(a) * radius, y + Math.sin(a) * radius,
      x + Math.cos(b) * radius, y + Math.sin(b) * radius,
      ...color);
  }
}

function filledRing(x, y, outerRadius, innerRadius, color) {
  const sides = 14;
  const inner = Math.max(0, Math.min(outerRadius, innerRadius));
  for (let side = 0; side < sides; side++) {
    const a = side * Math.PI * 2 / sides;
    const b = (side + 1) * Math.PI * 2 / sides;
    const outerA = { x: x + Math.cos(a) * outerRadius,
      y: y + Math.sin(a) * outerRadius };
    const outerB = { x: x + Math.cos(b) * outerRadius,
      y: y + Math.sin(b) * outerRadius };
    const innerA = { x: x + Math.cos(a) * inner,
      y: y + Math.sin(a) * inner };
    const innerB = { x: x + Math.cos(b) * inner,
      y: y + Math.sin(b) * inner };
    screenTriangle(outerA.x, outerA.y, innerA.x, innerA.y,
      outerB.x, outerB.y, ...color);
    screenTriangle(innerA.x, innerA.y, innerB.x, innerB.y,
      outerB.x, outerB.y, ...color);
  }
}

function filledCapsule(x1, y1, x2, y2, width, color) {
  const dx = x2 - x1;
  const dy = y2 - y1;
  const length = Math.hypot(dx, dy);
  const radius = width / 2;
  if (length < .001) {
    filledDisc(x1, y1, radius, color);
    return;
  }
  const nx = -dy / length * radius;
  const ny = dx / length * radius;
  screenTriangle(x1 + nx, y1 + ny, x1 - nx, y1 - ny,
    x2 + nx, y2 + ny, ...color);
  screenTriangle(x1 - nx, y1 - ny, x2 - nx, y2 - ny,
    x2 + nx, y2 + ny, ...color);
  filledDisc(x1, y1, radius, color);
  filledDisc(x2, y2, radius, color);
}

// Xbox batches GPU triangles above its D2D line layer, so every bone and joint
// must share this triangle path. The wider silhouette pass and color pass form
// conventional rounded capsules without the native renderer reordering them.
function drawSkeletonSegments(segments, color, outline) {
  const edge = Math.max(1.25, Math.min(3, cameraScale() * 1.8));
  for (const segment of segments)
    filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
      segment.width + edge * 2, outline);
  for (const segment of segments)
    filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
      segment.width, color);
}

function drawFighterSilhouette(geometry, color, outline) {
  drawSkeletonSegments(geometry.segments, color, outline);
  const headEdge = Math.max(1.25, Math.min(3, cameraScale() * 1.8));
  // The neck connector and solid head are emitted into the same triangle
  // silhouette pass, so the head cannot detach as a separate line-layer ring.
  filledDisc(geometry.head.x, geometry.head.y,
    geometry.head.radius + headEdge, outline);
  filledDisc(geometry.head.x, geometry.head.y, geometry.head.radius, color);
}

function runnerWorldGeometry(player, t) {
  // Combat silhouettes update on a deliberate 12 fps pose clock while the
  // physics and camera continue at full rate.
  const poseT = Math.floor(t * 12) / 12;
  const poseNow = runtime().monotonicUs;
  const speed = Math.min(1, Math.abs(player.vx) / 1500);
  const idle = player.grounded && !player.ducking && speed < .03;
  const breath = idle ? Math.sin(poseT * 2.4 + player.pad * .7) * 5 : 0;
  const idleSway = idle ? Math.sin(poseT * 1.55 + player.pad) * 7 : 0;
  const stride = Math.sin(poseT * (7 + speed * 9) + player.pad * Math.PI) *
    32 * speed;
  const pogo = isPogo(player);
  const headOnly = isHeadOnly(player);
  const noLegs = !hasPart(player, "left-leg") &&
    !hasPart(player, "right-leg") && !headOnly;
  const jumpAnticipation = player.jumpLaunchAt > poseNow
    ? clamp(1 - (player.jumpLaunchAt - poseNow) / 85000, 0, 1) : 0;
  const landingRecovery = player.landPoseUntil > poseNow
    ? clamp((player.landPoseUntil - poseNow) / 110000, 0, 1) : 0;
  const crouchPose = clamp(Math.max(player.crouchBlend || 0,
    Math.sin(jumpAnticipation * Math.PI) * .72,
    landingRecovery * .45, noLegs ? .45 : 0), 0, 1);
  const height = lerp(180, 108, crouchPose);
  const formDrop = pogo ? 54 : noLegs ? 31 : 0;
  const lean = player.facing * (idle ? 5 : 3 + speed * 10);
  const x = player.x;
  const feet = player.y;
  const z = player.z;
  // With neither leg present the pelvis itself becomes the ground contact.
  // Keeping its capsule tangent to the floor prevents a floating torso pose.
  const hipY = noLegs ? feet - 5
    : feet - lerp(58, 40, crouchPose) + formDrop;
  const neckX = x + lean;
  const neckY = feet - height + 54 - breath + formDrop;
  const attackPulse = meleePulse(player, runtime().monotonicUs);
  const head = headOnly
    ? { x, y: feet - 22, z, radius: 22 }
    : { x: neckX + lean * .2,
      y: feet - height + 22 - breath * 1.6 + formDrop, z, radius: 22 };
  const segments = [];
  const actionArm = player.facing > 0 ? "right-arm" : "left-arm";
  const rearArm = actionArm === "right-arm" ? "left-arm" : "right-arm";
  const actionLeg = player.facing > 0 ? "right-leg" : "left-leg";
  const rearLeg = actionLeg === "right-leg" ? "left-leg" : "right-leg";
  const partForRole = (role, startX) => {
    if (["neck", "torso", "shoulders"].includes(role)) return "torso";
    if (role.startsWith("attack-") || role.startsWith("item-"))
      return role.includes("arm") || role.includes("forearm")
        ? actionArm : actionLeg;
    if (role.startsWith("rest-")) return rearArm;
    if (role.startsWith("lead-")) return actionLeg;
    if (role.startsWith("rear-"))
      return role.includes("arm") || role.includes("forearm")
        ? rearArm : rearLeg;
    if (role.startsWith("grab-"))
      return startX <= neckX ? "left-arm" : "right-arm";
    if (role.startsWith("left-"))
      return role.includes("arm") || role.includes("forearm")
        ? "left-arm" : "left-leg";
    if (role.startsWith("right-"))
      return role.includes("arm") || role.includes("forearm")
        ? "right-arm" : "right-leg";
    return "torso";
  };
  const segment = (x1, y1, x2, y2, width, role = "body") => {
    const part = partForRole(role, x1);
    if (!hasPart(player, part)) return;
    segments.push({ x1, y1, z1: z, x2, y2, z2: z, width, role, part });
  };
  const twoBone = (startX, startY, targetX, targetY, length, bend) => {
    let dx = targetX - startX;
    let dy = targetY - startY;
    let distance = Math.hypot(dx, dy) || 1;
    const maximum = length * 1.94;
    if (distance > maximum) {
      targetX = startX + dx / distance * maximum;
      targetY = startY + dy / distance * maximum;
      dx = targetX - startX;
      dy = targetY - startY;
      distance = maximum;
    }
    const middleX = (startX + targetX) / 2;
    const middleY = (startY + targetY) / 2;
    const height = Math.sqrt(Math.max(0, length * length -
      distance * distance / 4));
    return { jointX: middleX - dy / distance * height * bend,
      jointY: middleY + dx / distance * height * bend,
      targetX, targetY };
  };
  segment(head.x, head.y + head.radius * .78, neckX, neckY, 10, "neck");
  segment(neckX, neckY, x, hipY, 10, "torso");
  const shoulderY = neckY + 11;
  const shoulderSpread = 12;
  const leftShoulderX = neckX - shoulderSpread;
  const rightShoulderX = neckX + shoulderSpread;
  segment(leftShoulderX, shoulderY, rightShoulderX, shoulderY, 10,
    "shoulders");
  if (player.attackKind === "KICK" && attackPulse > 0) {
    const target = meleeTarget(player, runtime().monotonicUs);
    const leg = twoBone(x, hipY, target.x, target.y, 74, -player.facing);
    segment(x, hipY, leg.jointX, leg.jointY, 12, "attack-thigh");
    segment(leg.jointX, leg.jointY, leg.targetX, leg.targetY, 12,
      "attack-shin");
    segment(x, hipY, x - player.facing * 28, feet - 32, 10, "rear-thigh");
    segment(x - player.facing * 28, feet - 32, x - player.facing * 8, feet,
      10, "rear-shin");
  } else if (crouchPose > .08) {
    segment(x, hipY, x - 36, feet - 22, 10, "left-thigh");
    segment(x - 36, feet - 22, x - 4, feet, 10, "left-shin");
    segment(x, hipY, x + 36, feet - 22, 10, "right-thigh");
    segment(x + 36, feet - 22, x + 58, feet, 10, "right-shin");
  } else if (player.grounded) {
    // The facing-side foot is visibly planted forward even at rest.
    const leadKnee = x + player.facing * 18 + stride * .38;
    const leadFoot = x + player.facing * 42 + stride;
    const rearKnee = x - player.facing * 16 - stride * .38;
    const rearFoot = x - player.facing * 28 - stride;
    segment(x, hipY, leadKnee, feet - 30, 10, "lead-thigh");
    segment(leadKnee, feet - 30, leadFoot, feet, 10, "lead-shin");
    segment(x, hipY, rearKnee, feet - 30, 10, "rear-thigh");
    segment(rearKnee, feet - 30, rearFoot, feet, 10, "rear-shin");
  } else {
    segment(x, hipY, x - 32, feet - 32, 10, "left-thigh");
    segment(x - 32, feet - 32, x - 7, feet - 11, 10, "left-shin");
    segment(x, hipY, x + 32, feet - 43, 10, "right-thigh");
    segment(x + 32, feet - 43, x + 50, feet - 22, 10, "right-shin");
  }
  const arm = idle ? idleSway : player.grounded ? -stride * .7 : 12;
  const elbowY = feet - lerp(94, 76, crouchPose) - breath;
  const handY = feet - lerp(65, 50, crouchPose) - breath * .5;
  const actionNow = runtime().monotonicUs;
  if (player.grabHeld) {
    const held = player.heldBall >= 0 ? balls[player.heldBall] : null;
    const clutchX = held?.x ?? x + player.facing * 126;
    const clutchY = held?.y ?? feet - 92;
    const spread = held ? Math.max(12, held.radius * .34) : 20;
    const hands = [
      { shoulderX: leftShoulderX, x: clutchX - player.facing * 5,
        y: clutchY - spread },
      { shoulderX: rightShoulderX, x: clutchX - player.facing * 5,
        y: clutchY + spread },
    ];
    for (const hand of hands) {
      const pose = twoBone(hand.shoulderX, shoulderY,
        hand.x, hand.y, 58, player.facing);
      segment(hand.shoulderX, shoulderY, pose.jointX, pose.jointY, 12,
        "grab-upper-arm");
      segment(pose.jointX, pose.jointY, pose.targetX, pose.targetY, 12,
        "grab-forearm");
    }
  } else if (player.itemAction && actionNow < player.itemActionUntil) {
    const target = itemHandTarget(player, actionNow);
    const actionShoulderX = player.facing > 0 ? rightShoulderX : leftShoulderX;
    const restShoulderX = player.facing > 0 ? leftShoulderX : rightShoulderX;
    const armPose = twoBone(actionShoulderX, shoulderY,
      target.x, target.y, 58, player.facing);
    segment(actionShoulderX, shoulderY, armPose.jointX, armPose.jointY, 12,
      "item-upper-arm");
    segment(armPose.jointX, armPose.jointY,
      armPose.targetX, armPose.targetY, 12, "item-forearm");
    segment(restShoulderX, shoulderY, x - player.facing * 32, elbowY, 10,
      "rest-upper-arm");
    segment(x - player.facing * 32, elbowY,
      x - player.facing * 36, handY, 10, "rest-forearm");
  } else if (player.attackKind === "PUNCH" && attackPulse > 0) {
    const target = meleeTarget(player, runtime().monotonicUs);
    const actionShoulderX = player.facing > 0 ? rightShoulderX : leftShoulderX;
    const restShoulderX = player.facing > 0 ? leftShoulderX : rightShoulderX;
    const armPose = twoBone(actionShoulderX, shoulderY,
      target.x, target.y, 58, player.facing);
    segment(actionShoulderX, shoulderY, armPose.jointX, armPose.jointY, 12,
      "attack-upper-arm");
    segment(armPose.jointX, armPose.jointY,
      armPose.targetX, armPose.targetY, 12, "attack-forearm");
    segment(restShoulderX, shoulderY, x - player.facing * 32, elbowY, 10,
      "rest-upper-arm");
    segment(x - player.facing * 32, elbowY,
      x - player.facing * 36, handY, 10, "rest-forearm");
  } else {
    segment(leftShoulderX, shoulderY, x - 30 + arm * .45, elbowY, 10,
      "left-upper-arm");
    segment(x - 30 + arm * .45, elbowY, x - 36 + arm * .6, handY, 10,
      "left-forearm");
    segment(rightShoulderX, shoulderY, x + 30 - arm * .45, elbowY, 10,
      "right-upper-arm");
    segment(x + 30 - arm * .45, elbowY, x + 36 - arm * .6, handY, 10,
      "right-forearm");
  }
  return { head, segments };
}

function runnerGeometry(player, t) {
  return projectRunnerWorldGeometry(runnerWorldGeometry(player, t));
}

function projectRunnerWorldGeometry(world) {
  const headPoint = projectPoint(world.head.x, world.head.y, world.head.z);
  return {
    head: { x: headPoint.x, y: headPoint.y,
      radius: Math.max(1.5, world.head.radius * cameraScale()) },
    segments: world.segments.map((segment) => {
      const a = projectPoint(segment.x1, segment.y1, segment.z1);
      const b = projectPoint(segment.x2, segment.y2, segment.z2);
      return { x1: a.x, y1: a.y, x2: b.x, y2: b.y,
        width: Math.max(1.5, segment.width * cameraScale()),
        role: segment.role, part: segment.part };
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

function runnerScreenBounds(player, t) {
  const world = player.replayGeometry || player.frozenGeometry ||
    runnerWorldGeometry(player, t);
  const geometry = projectRunnerWorldGeometry(world);
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

function fighterContainmentRequiredWidth(t) {
  let left = Infinity;
  let right = -Infinity;
  let top = Infinity;
  let bottom = -Infinity;
  const include = (x, y, radius) => {
    left = Math.min(left, x - radius);
    right = Math.max(right, x + radius);
    top = Math.min(top, y - radius);
    bottom = Math.max(bottom, y + radius);
  };
  for (const player of players) {
    const world = player.replayGeometry || player.frozenGeometry ||
      runnerWorldGeometry(player, t);
    include(world.head.x, world.head.y, world.head.radius);
    for (const segment of world.segments) {
      const radius = segment.width / 2;
      include(segment.x1, segment.y1, radius);
      include(segment.x2, segment.y2, radius);
    }
  }
  const safe = actionSafeRect();
  const safeWidth = Math.max(1, safe.right - safe.left);
  const safeHeight = Math.max(1, safe.bottom - safe.top);
  return Math.max(
    (right - left) * (stageRight - stageLeft) / safeWidth,
    (bottom - top) * cameraAspect *
      (stageBottom - stageTop) / safeHeight,
    compactLayout() ? 720 : 900,
  );
}

// Final render invariant: both complete animated fighter geometries must fit
// inside the action-safe viewport. Camera modes may orbit or focus, but this
// aspect-aware correction recenters their shared frame and moves the dolly
// back far enough for landscape, portrait, live, and replay projection alike.
function containFighters(t) {
  const gameplayContainment = !roundResult &&
    runtime().monotonicUs - roundStartedAt >= introDurationUs;
  if (gameplayContainment) {
    cameraContainFloor = Math.max(cameraContainFloor,
      fighterContainmentRequiredWidth(t) * 1.08);
    return;
  }
  const worlds = players.map((player) => player.replayGeometry ||
    player.frozenGeometry || runnerWorldGeometry(player, t));
  if (!worlds.length) return;
  const points = [];
  for (const world of worlds) {
    const { head } = world;
    points.push(
      { x: head.x - head.radius, y: head.y, z: head.z },
      { x: head.x + head.radius, y: head.y, z: head.z },
      { x: head.x, y: head.y - head.radius, z: head.z },
      { x: head.x, y: head.y + head.radius, z: head.z },
    );
    for (const segment of world.segments) {
      const radius = segment.width / 2;
      for (const endpoint of [
        { x: segment.x1, y: segment.y1, z: segment.z1 },
        { x: segment.x2, y: segment.y2, z: segment.z2 },
      ]) {
        points.push(
          { x: endpoint.x - radius, y: endpoint.y, z: endpoint.z },
          { x: endpoint.x + radius, y: endpoint.y, z: endpoint.z },
          { x: endpoint.x, y: endpoint.y - radius, z: endpoint.z },
          { x: endpoint.x, y: endpoint.y + radius, z: endpoint.z },
        );
      }
    }
  }
  const minX = Math.min(...points.map((point) => point.x));
  const maxX = Math.max(...points.map((point) => point.x));
  const minY = Math.min(...points.map((point) => point.y));
  const maxY = Math.max(...points.map((point) => point.y));
  // Keep the smoothly tracked root target. Animated limb extrema are allowed
  // to affect containment scale, but never camera aim.
  const target = cameraDoll.target;

  const safe = actionSafeRect();
  const safeWidth = Math.max(1, safe.right - safe.left);
  const safeHeight = Math.max(1, safe.bottom - safe.top);
  const requiredWidth = Math.max(
    (maxX - minX) * (stageRight - stageLeft) / safeWidth,
    (maxY - minY) * cameraAspect *
      (stageBottom - stageTop) / safeHeight,
    compactLayout() ? 720 : 900,
  );
  if (requiredWidth > cameraDoll.width) {
    const amount = requiredWidth / Math.max(1, cameraDoll.width) * 1.04;
    for (const axis of ["x", "y", "z"])
      cameraDoll.position[axis] = target[axis] +
        (cameraDoll.position[axis] - target[axis]) * amount;
    cameraDoll.width *= amount;
  }

  // Perspective and z-depth can make the world-space estimate asymmetric.
  // Iteratively measure the actual projection and expand along the existing
  // camera ray until every point satisfies the screen-space safe rectangle.
  const centerX = (safe.left + safe.right) / 2;
  const centerY = (safe.top + safe.bottom) / 2;
  for (let pass = 0; pass < 4; pass++) {
    cameraDoll.dirty = true;
    cameraDoll.prepare();
    const projected = points.map((point) => cameraDoll.project(point));
    const left = Math.min(...projected.map((point) => point.x));
    const right = Math.max(...projected.map((point) => point.x));
    const top = Math.min(...projected.map((point) => point.y));
    const bottom = Math.max(...projected.map((point) => point.y));
    const factor = Math.max(1,
      (centerX - left) / (safeWidth / 2),
      (right - centerX) / (safeWidth / 2),
      (centerY - top) / (safeHeight / 2),
      (bottom - centerY) / (safeHeight / 2));
    if (factor <= 1.001) break;
    const amount = factor * 1.025;
    for (const axis of ["x", "y", "z"])
      cameraDoll.position[axis] = target[axis] +
        (cameraDoll.position[axis] - target[axis]) * amount;
    cameraDoll.width *= amount;
  }
  cameraDoll.dirty = true;
}

function resolveRunnerBounds(player, t) {
  // Walls use a stable fighting-game pushbox. Animated hands and feet remain
  // the actual hit geometry, but cannot shove the root back and forth at an
  // arena edge as a pose changes.
  const halfWidth = isHeadOnly(player) ? 24 : isPogo(player) ? 38
    : player.ducking ? 76 : 62;
  const leftWall = worldLeft + wallThickness;
  const rightWall = worldRight - wallThickness;
  if (player.x - halfWidth < leftWall) {
    player.x = leftWall + halfWidth;
    player.vx = Math.max(0, player.vx);
    player.knockVx = Math.max(0, player.knockVx);
    player.dashUntil = 0;
    player.dashVx = 0;
  }
  if (player.x + halfWidth > rightWall) {
    player.x = rightWall - halfWidth;
    player.vx = Math.min(0, player.vx);
    player.knockVx = Math.min(0, player.knockVx);
    player.dashUntil = 0;
    player.dashVx = 0;
  }
  const ceiling = ceilingY + wallThickness;
  const standingTop = runnerBounds(player, t).top;
  if (standingTop < ceiling) {
    player.y += ceiling - standingTop;
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

// Closest points between two finite 3D line segments. Fighter limbs use the
// rendered capsule width around these lines, so collision and silhouette share
// the same geometry instead of maintaining a hidden rectangular hitbox.
function segmentSegmentClosest(first, second) {
  const d1x = first.x2 - first.x1;
  const d1y = first.y2 - first.y1;
  const d1z = first.z2 - first.z1;
  const d2x = second.x2 - second.x1;
  const d2y = second.y2 - second.y1;
  const d2z = second.z2 - second.z1;
  const rx = first.x1 - second.x1;
  const ry = first.y1 - second.y1;
  const rz = first.z1 - second.z1;
  const a = d1x * d1x + d1y * d1y + d1z * d1z;
  const e = d2x * d2x + d2y * d2y + d2z * d2z;
  const b = d1x * d2x + d1y * d2y + d1z * d2z;
  const c = d1x * rx + d1y * ry + d1z * rz;
  const f = d2x * rx + d2y * ry + d2z * rz;
  const epsilon = .000000001;
  let firstAmount = 0;
  let secondAmount = 0;
  if (a <= epsilon && e <= epsilon) {
    firstAmount = 0;
    secondAmount = 0;
  } else if (a <= epsilon) {
    secondAmount = clamp(f / e, 0, 1);
  } else if (e <= epsilon) {
    firstAmount = clamp(-c / a, 0, 1);
  } else {
    const denominator = a * e - b * b;
    firstAmount = Math.abs(denominator) > epsilon
      ? clamp((b * f - c * e) / denominator, 0, 1) : 0;
    const secondNumerator = b * firstAmount + f;
    if (secondNumerator < 0) {
      secondAmount = 0;
      firstAmount = clamp(-c / a, 0, 1);
    } else if (secondNumerator > e) {
      secondAmount = 1;
      firstAmount = clamp((b - c) / a, 0, 1);
    } else secondAmount = secondNumerator / e;
  }
  const firstPoint = {
    x: first.x1 + d1x * firstAmount,
    y: first.y1 + d1y * firstAmount,
    z: first.z1 + d1z * firstAmount,
  };
  const secondPoint = {
    x: second.x1 + d2x * secondAmount,
    y: second.y1 + d2y * secondAmount,
    z: second.z1 + d2z * secondAmount,
  };
  return { firstPoint, secondPoint,
    distance: Math.hypot(firstPoint.x - secondPoint.x,
      firstPoint.y - secondPoint.y, firstPoint.z - secondPoint.z) };
}

function attackCapsuleContact(attackingLimbs, target, t) {
  const targetGeometry = runnerWorldGeometry(target, t);
  if (!attackingLimbs.length) return null;
  const head = targetGeometry.head;
  const headCapsule = { x1: head.x, y1: head.y, z1: head.z,
    x2: head.x, y2: head.y, z2: head.z, width: head.radius * 2 };
  let headContact = null;
  let bodyContact = null;
  for (const attackingLimb of attackingLimbs) {
    const headClosest = segmentSegmentClosest(attackingLimb, headCapsule);
    const headSeparation = headClosest.distance -
      (attackingLimb.width + headCapsule.width) / 2;
    if (!headContact || headSeparation < headContact.separation)
      headContact = { closest: headClosest, separation: headSeparation };
    for (let index = 0; index < targetGeometry.segments.length; index++) {
      const targetLimb = targetGeometry.segments[index];
      const closest = segmentSegmentClosest(attackingLimb, targetLimb);
      const separation = closest.distance -
        (attackingLimb.width + targetLimb.width) / 2;
      if (!bodyContact || separation < bodyContact.separation)
        bodyContact = { closest, separation, segmentIndex: index };
    }
  }
  const headshot = headContact.separation <= 3;
  const contact = headshot ? headContact : bodyContact;
  if (!contact) return null;
  return {
    x: (contact.closest.firstPoint.x + contact.closest.secondPoint.x) / 2,
    y: (contact.closest.firstPoint.y + contact.closest.secondPoint.y) / 2,
    z: (contact.closest.firstPoint.z + contact.closest.secondPoint.z) / 2,
    separation: contact.separation,
    segmentIndex: headshot ? -1 : contact.segmentIndex,
    headshot,
  };
}

function meleeLimbContact(attacker, target, t) {
  const attackingGeometry = runnerWorldGeometry(attacker, t);
  const attackingLimbs = attackingGeometry.segments.filter((segment) =>
    segment.role?.startsWith("attack-"));
  return attackCapsuleContact(attackingLimbs, target, t);
}

function runnerDistanceToPoint(player, t, px, py, pz = 0) {
  const contact = runnerContactToPoint(player, t, px, py, pz);
  return Math.min(contact.headDistance, contact.bodyDistance);
}

function runnerContactToPoint(player, t, px, py, pz = 0) {
  const geometry = runnerWorldGeometry(player, t);
  const headDistance = Math.max(0,
    Math.hypot(px - geometry.head.x, py - geometry.head.y,
      pz - geometry.head.z) - geometry.head.radius);
  let bodyDistance = Infinity;
  let segmentIndex = -1;
  for (let index = 0; index < geometry.segments.length; index++) {
    const segment = geometry.segments[index];
    const distance = Math.max(0,
      pointSegmentDistance(px, py, pz, segment) - segment.width / 2);
    if (distance < bodyDistance) {
      bodyDistance = distance;
      segmentIndex = index;
    }
  }
  return { headDistance, bodyDistance, segmentIndex };
}

function detachPart(player, part, geometry, sourceX, now) {
  if (!hasPart(player, part)) return;
  const direction = Math.sign(player.x - sourceX) || player.facing || 1;
  for (const segment of geometry.segments.filter((item) => item.part === part)) {
    detachedParts.push({ ...segment, color: player.color.slice(),
      vx: direction * (420 + detachedParts.length % 3 * 90),
      vy: -520 - detachedParts.length % 2 * 120,
      spin: direction * (3.5 + detachedParts.length % 4),
      life: 2.6, part, owner: player.pad });
  }
  player.removedParts.push(part);
  player.partDamage[part] = Math.max(player.partDamage[part] || 0,
    part === "torso" ? 3 : 2);
  emitSignal("partremoved", player.pad,
    [...limbParts, "torso"].indexOf(part), 1);
}

function damagePart(target, segmentIndex, sourceX, sourcePad, now) {
  const geometry = runnerWorldGeometry(target, (now - startedAt) / 1000000);
  const segment = geometry.segments[segmentIndex];
  const part = segment?.part;
  if (!part || !hasPart(target, part)) return;
  const durability = part === "torso" ? 3 : 2;
  target.partDamage[part] = (target.partDamage[part] || 0) + 1;
  emitSignal("partdamage", target.pad,
    [...limbParts, "torso"].indexOf(part), target.partDamage[part] / durability);
  if (target.partDamage[part] < durability) return;
  if (part === "torso") {
    // Removing the body's attachment point releases every surviving limb;
    // the circular head remains as the final playable form.
    for (const limb of limbParts)
      detachPart(target, limb, geometry, sourceX, now);
  }
  detachPart(target, part, geometry, sourceX, now);
  target.attackKind = "";
  target.attackUntil = 0;
  target.attackHit = false;
  target.lastButton = part.toUpperCase() + " LOST";
  target.lastButtonAt = now;
  playDrum("clap", 1.2, panPlayer(target));
}

function damageAllLimbs(target, sourceX, sourcePad, now) {
  for (const part of limbParts) {
    if (!hasPart(target, part)) continue;
    const geometry = runnerWorldGeometry(target, (now - startedAt) / 1000000);
    const index = geometry.segments.findIndex((segment) => segment.part === part);
    if (index >= 0) damagePart(target, index, sourceX, sourcePad, now);
  }
}

function applyBodyHit(target, segmentIndex, sourceX, sourcePad, now,
    force = 1100, lift = 150, damageParts = true) {
  const direction = Math.sign(target.x - sourceX) ||
    (sourcePad === target.pad ? -target.facing : target.facing) || 1;
  releaseCarriedBall(target, now);
  target.attackKind = "";
  target.attackUntil = 0;
  target.attackHit = false;
  target.dashUntil = 0;
  target.dashVx = 0;
  target.hit = Math.max(target.hit, .52);
  target.hitSegment = segmentIndex;
  target.hitSegmentUntil = Math.max(target.hitSegmentUntil, now + 190000);
  // Balls transfer recoil but are sporting hazards, not damaging attacks.
  if (damageParts)
    damagePart(target, segmentIndex, sourceX, sourcePad, now);
  target.hitStunUntil = Math.max(target.hitStunUntil, now + 145000);
  target.knockVx += direction * force;
  target.vx = target.knockVx + target.windVx;
  target.vy = Math.min(target.vy, -lift);
  target.grounded = false;
  target.stance = "STUN";
  target.lastButton = "BODY HIT";
  target.lastButtonAt = now;
  emitSignal("bodyhit", sourcePad, target.pad, segmentIndex);
}

function updateDetachedParts(dt) {
  for (const fragment of detachedParts) {
    fragment.vy += 1900 * dt;
    const dx = fragment.vx * dt;
    const dy = fragment.vy * dt;
    fragment.x1 += dx;
    fragment.y1 += dy;
    fragment.x2 += dx;
    fragment.y2 += dy;
    const centerX = (fragment.x1 + fragment.x2) / 2;
    const centerY = (fragment.y1 + fragment.y2) / 2;
    const angle = fragment.spin * dt;
    const cosine = Math.cos(angle);
    const sine = Math.sin(angle);
    for (const endpoint of [1, 2]) {
      const xKey = "x" + endpoint;
      const yKey = "y" + endpoint;
      const localX = fragment[xKey] - centerX;
      const localY = fragment[yKey] - centerY;
      fragment[xKey] = centerX + localX * cosine - localY * sine;
      fragment[yKey] = centerY + localX * sine + localY * cosine;
    }
    const bottom = Math.max(fragment.y1, fragment.y2) + fragment.width / 2;
    if (bottom > floorY) {
      const correction = floorY - bottom;
      fragment.y1 += correction;
      fragment.y2 += correction;
      fragment.vy = -Math.abs(fragment.vy) * .42;
      fragment.vx *= .82;
      fragment.spin *= .78;
    }
    fragment.life -= dt;
  }
  for (let index = detachedParts.length - 1; index >= 0; index--)
    if (detachedParts[index].life <= 0) detachedParts.splice(index, 1);
}

function runnerBodyDistanceToPoint(geometry, px, py, pz = 0) {
  let distance = Infinity;
  for (const segment of geometry.segments)
    distance = Math.min(distance,
      Math.max(0, pointSegmentDistance(px, py, pz, segment) - segment.width / 2));
  return distance;
}

function comicGlyphAdvance(character, size) {
  if (character === "@") return size * .92;
  if ("MW%&".includes(character.toUpperCase())) return size * .88;
  if ("I1!.,:;'|".includes(character)) return size * .34;
  if ("JLTF".includes(character.toUpperCase())) return size * .52;
  return size * .65;
}

function handleWidth(handle, size) {
  return [...handle].reduce((width, character) =>
    width + comicGlyphAdvance(character, size), 0);
}

function typeWrite(text, x, y, size, ...color) {
  const visibleText = String(text).toLowerCase();
  if (typeof comicWrite === "function") comicWrite(visibleText, x, y, size, ...color);
  else if (typeof ywftWrite === "function")
    ywftWrite(visibleText, x, y, size, ...color);
  else systemWrite(visibleText, x, y, size, ...color);
}

function contrastShadow(color) {
  const luminance = color[0] * .2126 + color[1] * .7152 + color[2] * .0722;
  return luminance > 142 ? [8, 12, 24] : [232, 238, 248];
}

function controlLocale() {
  const caps = typeof capabilities === "function" ? capabilities() : {};
  const keyboard = caps.inputFamily === "keyboard";
  const touch = caps.inputFamily === "touch";
  if (touch) return {
    title: "start", select: "", replayPaused: "paused",
    replayPlaying: "", replay: "",
  };
  return keyboard ? {
    title: "start",
    select: "P1 A/D + SPACE     P2 LEFT/RIGHT + K     H P2/DUMMY/BOT     G BACK",
    replayPaused: "PAUSED   F PLAY   A D SCRUB   G EXIT",
    replayPlaying: "F PAUSE   A D SCRUB   G EXIT",
    replay: "Q REPLAY",
  } : {
    title: "start",
    select: "LEFT RIGHT SELECT     A READY     X P2 / DUMMY / BOT     B BACK",
    replayPaused: "PAUSED   A PLAY   LEFT RIGHT SCRUB   B EXIT",
    replayPlaying: "A PAUSE   LEFT RIGHT SCRUB   B EXIT",
    replay: "Y REPLAY",
  };
}

function drawHandle(handle, x, y, size, colors, fallback) {
  let cursor = x;
  for (let index = 0; index < handle.length; index++) {
    const color = colors?.[index] || fallback;
    typeWrite(handle[index], cursor, y, size, ...color);
    cursor += size * (handle[index] === "@" ? .88 : .58);
  }
}

function drawFace(player, head, color, t, now = runtime().monotonicUs) {
  if (head.radius < 5) return;
  const r = head.radius;
  const direction = player.facing || 1;
  const stroke = (x1, y1, x2, y2, width, ink = color) =>
    filledCapsule(x1, y1, x2, y2, width, ink);
  if (player.name === "@FIFI") {
    const hair = visualTheme.light > .55 ? [105, 38, 116] : [245, 118, 230];
    const sway = Math.sin(t * 2.2 + player.pad) * r * .08;
    const width = Math.max(2, r * .13);
    stroke(head.x - r * .72, head.y - r * .48,
      head.x - r * .32, head.y - r * .9, width, hair);
    stroke(head.x - r * .32, head.y - r * .9,
      head.x + r * .35, head.y - r * .88, width, hair);
    stroke(head.x + r * .35, head.y - r * .88,
      head.x + r * .76, head.y - r * .38, width, hair);
    stroke(head.x - r * .76, head.y - r * .38,
      head.x - r * .7 + sway, head.y + r * .92, width, hair);
    stroke(head.x + r * .76, head.y - r * .38,
      head.x + r * .7 + sway, head.y + r * .92, width, hair);
    stroke(head.x - r * .48, head.y - r * .46,
      head.x - r * .08, head.y - r * .18, width * .72, hair);
    stroke(head.x - r * .08, head.y - r * .18,
      head.x + r * .42, head.y - r * .5, width * .72, hair);
  }
  const eyeY = head.y - r * .18;
  const eyeGap = r * .34;
  const faceX = head.x + direction * r * .08;
  const eyeWidth = Math.max(1.4, r * .1);
  const lineWidth = Math.max(1.2, r * .1);
  const blink = player.alive && !player.blocking && !player.attackKind &&
    Math.sin(t * .73 + player.pad * 2.1) > .985;
  const inertDummy = player.npc && !player.bot;
  if (!player.alive || player.hit > .6 || inertDummy) {
    for (const offset of [-eyeGap, eyeGap]) {
      stroke(faceX + offset - eyeWidth, eyeY - eyeWidth,
        faceX + offset + eyeWidth, eyeY + eyeWidth, lineWidth);
      stroke(faceX + offset + eyeWidth, eyeY - eyeWidth,
        faceX + offset - eyeWidth, eyeY + eyeWidth, lineWidth);
    }
  } else if (player.blocking || blink) {
    stroke(faceX - eyeGap - eyeWidth, eyeY, faceX - eyeGap + eyeWidth,
      eyeY, lineWidth);
    stroke(faceX + eyeGap - eyeWidth, eyeY, faceX + eyeGap + eyeWidth,
      eyeY, lineWidth);
  } else {
    filledDisc(faceX - eyeGap + direction * eyeWidth * .35,
      eyeY, eyeWidth * 1.05, color);
    filledDisc(faceX + eyeGap + direction * eyeWidth * .35,
      eyeY, eyeWidth * 1.05, color);
  }
  if (player.bot && player.alive && !player.blocking) {
    const browY = eyeY - r * .32;
    stroke(faceX - eyeGap - eyeWidth * 1.25, browY - r * .08,
      faceX - eyeGap + eyeWidth * 1.25, browY + r * .1,
      lineWidth * 1.15);
    stroke(faceX + eyeGap - eyeWidth * 1.25, browY + r * .1,
      faceX + eyeGap + eyeWidth * 1.25, browY - r * .08,
      lineWidth * 1.15);
  }
  const mouthY = head.y + r * .3;
  if (player.attackKind && meleePulse(player, now) > 0) {
    filledRing(faceX + direction * r * .12, mouthY,
      Math.max(1.8, r * .13), Math.max(.4, r * .05), color);
  } else if (player.blocking) {
    stroke(faceX - r * .26, mouthY, faceX + r * .26, mouthY, lineWidth);
  } else if (!player.alive || player.hit > .6) {
    stroke(faceX - r * .24, mouthY + r * .08, faceX,
      mouthY - r * .06, lineWidth);
    stroke(faceX, mouthY - r * .06, faceX + r * .24,
      mouthY + r * .08, lineWidth);
  } else {
    const smile = Math.sin(t * 2.4 + player.pad) * r * .035;
    stroke(faceX - r * .23, mouthY - smile, faceX,
      mouthY + r * .09, lineWidth);
    stroke(faceX, mouthY + r * .09, faceX + r * .23,
      mouthY - smile, lineWidth);
  }
}

function drawInventory(player, now) {
  const scale = cameraScale();
  const pip = Math.max(3, Math.min(7, 5 * scale));
  const belt = projectPoint(player.x, player.y - 58, player.z);
  const gunColor = [255, 220, 72];
  const grenadeColor = [255, 105, 105];
  const firing = player.itemAction === "FIRE" && now < player.itemActionUntil;
  const throwing = player.itemAction === "THROW" && now < player.itemActionUntil;
  if (player.gunAmmo > 0 || firing) {
    const held = firing ? itemHandTarget(player, now)
      : { x: player.x - player.facing * 25, y: player.y - 82, z: player.z };
    const hand = projectPoint(held.x, held.y, held.z);
    const barrel = projectPoint(held.x + player.facing * 54,
      held.y, held.z);
    line(hand.x, hand.y, barrel.x, barrel.y,
      Math.max(3, 9 * scale), ...gunColor);
    line(hand.x, hand.y, hand.x - player.facing * 8 * scale,
      hand.y + 20 * scale, Math.max(2, 6 * scale), ...gunColor);
    if (firing) {
      line(barrel.x, barrel.y, barrel.x + player.facing * 28 * scale,
        barrel.y - 18 * scale, Math.max(2, 5 * scale), 255, 248, 190);
      line(barrel.x, barrel.y, barrel.x + player.facing * 28 * scale,
        barrel.y + 18 * scale, Math.max(2, 5 * scale), 255, 248, 190);
    }
  }
  const gunPips = Math.min(12, player.gunAmmo);
  for (let index = 0; index < gunPips; index++) {
    const row = Math.floor(index / 6);
    const column = index % 6;
    box(belt.x - 16 * scale + column * (pip + 2),
      belt.y + row * (pip + 2), pip, pip, ...gunColor);
  }
  const grenadePips = Math.min(4, player.grenadeAmmo);
  for (let index = 0; index < grenadePips; index++)
    box(belt.x - 18 * scale + index * (pip + 4), belt.y - 13 * scale,
      pip + 2, pip + 2, ...grenadeColor);
  if (throwing) {
    const target = itemHandTarget(player, now);
    const hand = projectPoint(target.x, target.y, target.z);
    circle(hand.x, hand.y, Math.max(5, 15 * scale),
      Math.max(2, 5 * scale), grenadeColor);
  }
}

function drawDigitalHeadBurst(player, headWorld, age) {
  const burstAge = Math.max(0, age - .11);
  const origin = projectPoint(headWorld.x, headWorld.y, headWorld.z);
  if (![origin.x, origin.y, origin.z].every(Number.isFinite) ||
      Math.abs(origin.x) > 30000 || Math.abs(origin.y) > 30000) return;
  const palette = [
    [255, 48, 96], [176, 18, 54], [255, 92, 126],
    player.color, [116, 8, 38],
  ];
  const directions = [-2.74, -2.18, -1.68, -1.18, -.58, .08, .64, 1.18];
  for (let index = 0; index < directions.length; index++) {
    const angle = directions[index] + (player.pad ? .14 : -.14);
    const speed = 92 + index * 13;
    const distance = 10 + speed * burstAge;
    const fall = 24 * burstAge + 170 * burstAge * burstAge;
    const depth = (index % 3 - 1) * (9 + burstAge * 24);
    const startWorld = {
      x: headWorld.x + Math.cos(angle) * distance * .16,
      y: headWorld.y + Math.sin(angle) * distance * .16,
      z: headWorld.z + depth * .16,
    };
    const endWorld = {
      x: headWorld.x + Math.cos(angle) * distance,
      y: headWorld.y + Math.sin(angle) * distance + fall,
      z: headWorld.z + depth,
    };
    const start = projectPoint(startWorld.x, startWorld.y, startWorld.z);
    const end = projectPoint(endWorld.x, endWorld.y, endWorld.z);
    const values = [start.x, start.y, end.x, end.y, end.z];
    if (!values.every(Number.isFinite) ||
        values.some((value) => Math.abs(value) > 30000)) continue;
    const color = palette[index % palette.length];
    triangleDepth = end.z;
    const width = Math.max(2, (5 - index % 3) * cameraScale());
    filledCapsule(start.x, start.y, end.x, end.y, width, color);
    const pixel = Math.max(3, (7 - index % 2) * cameraScale());
    screenRect(end.x - pixel / 2, end.y - pixel / 2, pixel, pixel, color);
    if (index % 2 === 0 && burstAge > .08) {
      const echoX = lerp(start.x, end.x, .62);
      const echoY = lerp(start.y, end.y, .62);
      const echo = pixel * .55;
      screenRect(echoX - echo / 2, echoY - echo / 2, echo, echo,
        palette[(index + 2) % palette.length]);
    }
  }
  // The face becomes a small broken core rather than remaining an intact disc.
  const core = Math.max(3, headWorld.radius * cameraScale() *
    Math.max(.18, 1 - burstAge * 4.2));
  triangleDepth = origin.z;
  for (let index = 0; index < 4; index++) {
    const side = index % 2 ? 1 : -1;
    const row = index < 2 ? -1 : 1;
    const size = core * (.62 + index * .05);
    screenRect(origin.x + side * core * .32 - size / 2,
      origin.y + row * core * .3 - size / 2, size, size,
      palette[index % palette.length]);
  }
}

function drawBrokenRunner(player, age) {
  const world = player.frozenGeometry ||
    runnerWorldGeometry(player, (runtime().monotonicUs - startedAt) / 1000000);
  const slow = Math.max(0, age - .11) * .32;
  const gravity = 420 * slow * slow;
  const outline = [8, 12, 24];
  for (let index = 0; index < world.segments.length; index++) {
    const segment = world.segments[index];
    const midX = (segment.x1 + segment.x2) / 2;
    const midY = (segment.y1 + segment.y2) / 2;
    const side = index % 2 ? 1 : -1;
    const dx = (side * (72 + index * 9) - player.facing * 34) * slow;
    const dy = -(150 + index * 13) * slow + gravity;
    const dz = side * (24 + index * 4) * slow;
    const angle = side * slow * (1.15 + index * .08);
    const cosine = Math.cos(angle);
    const sine = Math.sin(angle);
    const move = (x, y, z) => {
      const localX = x - midX;
      const localY = y - midY;
      return { x: midX + localX * cosine - localY * sine + dx,
        y: midY + localX * sine + localY * cosine + dy,
        z: z + dz };
    };
    const first = move(segment.x1, segment.y1, segment.z1);
    const second = move(segment.x2, segment.y2, segment.z2);
    const a = projectPoint(first.x, first.y, first.z);
    const b = projectPoint(second.x, second.y, second.z);
    if (![a.x, a.y, b.x, b.y].every(Number.isFinite) ||
        [a.x, a.y, b.x, b.y].some((value) => Math.abs(value) > 30000))
      continue;
    triangleDepth = projectPoint(midX + dx, midY + dy,
      (segment.z1 + segment.z2) / 2 + dz).z;
    const width = Math.max(2, segment.width * cameraScale());
    filledCapsule(a.x, a.y, b.x, b.y, width + 3, outline);
    filledCapsule(a.x, a.y, b.x, b.y, width, player.color);
  }
  drawDigitalHeadBurst(player, world.head, age);
}

function deathCinematicAge(now = runtime().monotonicUs) {
  return deathCinematic ? Math.max(0, (now - deathCinematic.startedAt) / 1000000) : -1;
}

function drawDeathFlash() {
  const age = deathCinematicAge();
  if (age < 0 || age >= .11) return;
  const previousDepth = triangleDepth;
  triangleDepth = -1.425;
  const flash = age < .055 ? [255, 255, 255] : [255, 226, 116];
  screenRect(0, 0, viewWidth(), viewHeight, flash);
  triangleDepth = previousDepth;
}

function drawRunner(player, t, showLabel = true) {
  if (!player.alive && !roundResult) return;
  const cinematicAge = deathCinematicAge();
  const headBurstAge = player.headBustedAt
    ? Math.max(0, (runtime().monotonicUs - player.headBustedAt) / 1000000) : -1;
  if (!player.alive && headBurstAge >= .11) {
    drawBrokenRunner(player, headBurstAge);
    return;
  }
  const geometry = player.replayGeometry
    ? projectRunnerWorldGeometry(player.replayGeometry)
    : player.frozenGeometry
      ? projectRunnerWorldGeometry(player.frozenGeometry)
    : runnerGeometry(player, t);
  // Preserve the fighter's identity color during hit flash. A pure white body
  // disappeared against the daylight arena, so impact now changes only its rim.
  const color = player.color;
  const outline = player.hit > 0
    ? mixColor([255, 232, 92], [28, 34, 52], visualTheme.light)
    : [8, 12, 24];
  const displayNow = player.frozenAt || runtime().monotonicUs;
  drawFighterSilhouette(geometry, color, outline);
  for (const segment of geometry.segments) {
    const damage = player.partDamage?.[segment.part] || 0;
    if (!damage) continue;
    const dx = segment.x2 - segment.x1;
    const dy = segment.y2 - segment.y1;
    const length = Math.hypot(dx, dy) || 1;
    const middleX = (segment.x1 + segment.x2) / 2;
    const middleY = (segment.y1 + segment.y2) / 2;
    const reach = segment.width * (.7 + damage * .16);
    const nx = -dy / length * reach;
    const ny = dx / length * reach;
    filledCapsule(middleX - nx, middleY - ny,
      middleX + nx, middleY + ny, Math.max(2, segment.width * .24),
      [255, 126, 72]);
  }
  const hitNow = runtime().monotonicUs;
  if (player.hitSegment >= 0 && hitNow < player.hitSegmentUntil &&
      Math.floor(hitNow / 45000) % 2 === 0) {
    const segment = geometry.segments[player.hitSegment];
    if (segment) {
      filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
        segment.width + Math.max(3, 5 * cameraScale()), [255, 238, 102]);
    }
  }
  drawFace(player, geometry.head, contrastShadow(color), t, displayNow);
  drawInventory(player, displayNow);
  if (player.blocking) {
    const worldShield = shieldGeometry(player);
    const shield = projectPoint(worldShield.x, worldShield.y, worldShield.z);
    const radius = Math.max(18, worldShield.radius * cameraScale());
    const shieldColor = player.blockFlash > 0 ? [255, 255, 255] : player.color;
    const outerWidth = Math.max(4, 11 * cameraScale());
    const innerWidth = Math.max(3, 7 * cameraScale());
    filledRing(shield.x, shield.y, radius,
      Math.max(0, radius - outerWidth), shieldColor);
    filledRing(shield.x, shield.y, radius * .76,
      Math.max(0, radius * .76 - innerWidth), shieldColor);
    filledCapsule(shield.x + player.facing * radius * .25, shield.y - radius * .72,
      shield.x + player.facing * radius * .72, shield.y + radius * .72,
      Math.max(4, 8 * cameraScale()), shieldColor);
  }

}

function drawDebugHitboxes(player, t) {
  const now = runtime().monotonicUs;
  const impactDebug = !roundResult && now < impactHitboxesUntil;
  if ((!debugHitboxes && !impactDebug) || (!player.alive && !roundResult)) return;
  const cinematicAge = deathCinematicAge(now);
  if ((deathCinematic?.loserPad === player.pad && cinematicAge >= .11) ||
      (deathCinematic?.winnerPad === player.pad && cinematicAge >= .11 &&
       cinematicAge < .86))
    return;
  const world = player.replayGeometry || player.frozenGeometry ||
    runnerWorldGeometry(player, t);
  const geometry = projectRunnerWorldGeometry(world);
  const bodyColor = [58, 222, 255];
  const headColor = [255, 62, 82];
  const pushColor = [105, 255, 118];
  const attackColor = [255, 86, 220];

  for (const segment of geometry.segments)
    filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
      Math.max(2, segment.width * .22), bodyColor);
  filledRing(geometry.head.x, geometry.head.y, geometry.head.radius,
    Math.max(0, geometry.head.radius - Math.max(2,
      geometry.head.radius * .12)), headColor);

  const halfWidth = player.ducking ? 76 : 62;
  const top = player.y - (player.ducking ? 132 : 174);
  const corners = [
    projectPoint(player.x - halfWidth, top, player.z),
    projectPoint(player.x + halfWidth, top, player.z),
    projectPoint(player.x + halfWidth, player.y, player.z),
    projectPoint(player.x - halfWidth, player.y, player.z),
  ];
  for (let index = 0; index < corners.length; index++) {
    const next = corners[(index + 1) % corners.length];
    filledCapsule(corners[index].x, corners[index].y,
      next.x, next.y, 2, pushColor);
  }

  const displayNow = player.frozenAt || now;
  if (player.attackKind && displayNow < player.attackUntil) {
    for (const segment of geometry.segments) {
      if (!segment.role?.startsWith("attack-")) continue;
      filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
        segment.width + 5, attackColor);
    }
  }

  if (!debugHitboxes) return;

  const mode = "P" + (player.pad + 1) + " " + player.stance +
    (player.attackKind ? "/" + player.attackKind : "");
  const bodyBottom = projectedBodyBottom(geometry);
  const labelY = Math.min(viewHeight - 168, bodyBottom + 12);
  const labelWidth = Math.max(162, mode.length * 15);
  const pad = padSnapshots[player.pad] ||
    { connected: false, down: [], leftX: 0, leftY: 0 };
  const input = quantizedInput(pad, player.suppressedDirections);
  const inputLabel = "IN " + input.horizontal + "," + input.vertical +
    "  STK " + pad.leftX.toFixed(2) + "  VX " + Math.round(player.vx);
  const inputWidth = inputLabel.length * 10;
  const panelWidth = Math.max(labelWidth, inputWidth + 16);
  typeWrite(mode, geometry.head.x - labelWidth / 2 + 8,
    labelY, 24, ...player.color);
  typeWrite(inputLabel, geometry.head.x - panelWidth / 2 + 8,
    labelY + 30, 18, 215, 224, 240);
}

function drawPlayerHud(player, x, pad) {
  const color = visualTheme.light > .55
    ? player.pad === 0 ? [155, 34, 108] : [105, 78, 0]
    : player.color;
  let label = "P" + (player.pad + 1) + "  " + player.roundWins + "/" +
    matchWins + "  PTS " + player.score;
  typeWrite(label, x, 14, 22, ...color);
}

function drawFighterData(player, x) {
  const profile = fighterProfile(player.name);
  const mood = profile.mood ? "M " + profile.mood.slice(0, 24) : "M —";
  const chat = profile.lastChat ? "CHAT " + profile.lastChat.slice(0, 34) : "CHAT —";
  const ink = mixColor([190, 205, 235], [55, 66, 90], visualTheme.light);
  typeWrite(mood + "  ·  " + chat, x, 82, 15, ...ink);
}

function projectedBodyBottom(geometry) {
  let bottom = geometry.head.y + geometry.head.radius;
  for (const segment of geometry.segments) {
    const radius = segment.width / 2;
    bottom = Math.max(bottom, segment.y1 + radius, segment.y2 + radius);
  }
  return bottom;
}

function visibleHandle(player) {
  return player.name.toLowerCase();
}

function playerHandleLayout(player, side) {
  const safe = hudSafeRect();
  const touch = typeof capabilities === "function" &&
    capabilities().inputFamily === "touch";
  const size = hudTypeSize;
  const width = handleWidth(visibleHandle(player), size);
  const x = side === 0 ? safe.left + 8 : safe.right - 8 - width;
  const y = safe.bottom - size - (touch ? 250 : 18);
  return { x, y, size, width };
}

function drawFloatingHandle(player, x, y, size) {
  const handle = visibleHandle(player);
  const shadows = player.handleColors?.map(contrastShadow);
  drawHandle(handle, x + 3, y + 4, size,
    shadows, contrastShadow(player.color));
  drawHandle(handle, x, y, size, player.handleColors, player.color);
}

function drawPlayerHandle(player, t, side) {
  const { x, y, size } = playerHandleLayout(player, side);
  const handle = visibleHandle(player);
  const drawGlyphs = (dx, dy, colors, fallback) => {
    let cursor = x + dx;
    for (let index = 0; index < handle.length; index++) {
      const character = handle[index];
      const color = colors?.[index] || fallback;
      typeWrite(character, cursor, y + dy, size, ...color);
      cursor += comicGlyphAdvance(character, size);
    }
  };
  // Shadow contrast follows each foreground glyph instead of assuming that
  // every handle color is light.
  drawGlyphs(3, 4, player.handleColors?.map(contrastShadow),
    contrastShadow(player.color));
  drawGlyphs(0, 0, player.handleColors, player.color);
}

function drawCommandStream(player, side) {
  const glyph = { LEFT: "<", RIGHT: ">", UP: "^", DOWN: "v" };
  const text = player.commandStream.map((entry) => glyph[entry.label] || entry.label)
    .join("  ");
  if (!text) return;
  const safe = hudSafeRect();
  const size = hudTypeSize;
  const width = handleWidth(text, size);
  const x = side === 0 ? safe.left + 8 : safe.right - 8 - width;
  const handle = playerHandleLayout(player, side);
  const y = handle.y - size - 15;
  const shadow = contrastShadow(player.color);
  typeWrite(text, x + 3, y + 4, size, ...shadow);
  typeWrite(text, x, y, size, ...player.color);
}

function drawFightIntro(introSeconds, titleInk, statusShadow) {
  const centerX = viewCenterX();
  const centerY = (stageTop + stageBottom) / 2;
  const nameSize = compactLayout() ? 48 : 74;
  const drawCenteredName = (player) => {
    const width = handleWidth(visibleHandle(player), nameSize);
    drawFloatingHandle(player, centerX - width / 2, centerY - nameSize / 2,
      nameSize);
  };
  if (introSeconds < .65) {
    drawCenteredName(players[0]);
    return;
  }
  if (introSeconds < 1.3) {
    drawCenteredName(players[1]);
    return;
  }

  const andText = "and";
  const andSize = nameSize * .72;
  const firstWidth = handleWidth(visibleHandle(players[0]), nameSize);
  const secondWidth = handleWidth(visibleHandle(players[1]), nameSize);
  const andWidth = handleWidth(andText, andSize);
  const pairGap = compactLayout() ? 20 : 32;
  const pairWidth = firstWidth + pairGap + andWidth + pairGap + secondWidth;
  const pairLeft = centerX - pairWidth / 2;
  const pairStarts = [pairLeft,
    pairLeft + firstWidth + pairGap + andWidth + pairGap];
  const travel = clamp((introSeconds - 1.3) / .6, 0, 1);
  const eased = travel * travel * (3 - travel * 2);
  for (let side = 0; side < players.length; side++) {
    const player = players[side];
    const target = playerHandleLayout(player, side);
    const startX = pairStarts[side];
    const startY = centerY - target.size / 2;
    drawFloatingHandle(player, lerp(startX, target.x, eased),
      lerp(startY, target.y, eased), lerp(nameSize, target.size, eased));
  }
  const andY = centerY - andSize / 2;
  if (introSeconds < 2.45) {
    const andX = centerX - andWidth / 2;
    typeWrite(andText, andX + 4, andY + 5, andSize, ...statusShadow);
    typeWrite(andText, andX, andY, andSize, ...titleInk);
    return;
  }
  const fightAge = clamp((introSeconds - 2.45) / .55, 0, 1);
  const fightSize = lerp(72, 96, Math.sin(fightAge * Math.PI));
  const startText = "start";
  const startWidth = startText.length * fightSize * .58;
  typeWrite(startText, centerX - startWidth / 2 + 5,
    centerY - fightSize / 2 + 6, fightSize, ...statusShadow);
  typeWrite(startText, centerX - startWidth / 2,
    centerY - fightSize / 2, fightSize, ...titleInk);
}

function worldLine(x1, y1, z1, x2, y2, z2, width, color) {
  const a = projectPoint(x1, y1, z1);
  const b = projectPoint(x2, y2, z2);
  line(a.x, a.y, b.x, b.y, width, ...color);
}

function worldQuad(a, b, c, d, color) {
  const points = [a, b, c, d].map((point) =>
    projectPoint(point.x, point.y, point.z));
  if (points.some((point) => !Number.isFinite(point.x) ||
      !Number.isFinite(point.y) || Math.abs(point.x) > 30000 ||
      Math.abs(point.y) > 30000)) return;
  const ab = { x: b.x - a.x, y: b.y - a.y, z: b.z - a.z };
  const ac = { x: c.x - a.x, y: c.y - a.y, z: c.z - a.z };
  const normal = normalize3(cross3(ab, ac));
  const light = { x: -globalLight.x, y: -globalLight.y, z: -globalLight.z };
  const illumination = .72 + Math.max(0, dot3(normal, light)) * .28;
  const lit = color.map((channel) => Math.round(channel * illumination));
  projectedTriangle(points[0], points[1], points[2], lit);
  projectedTriangle(points[0], points[2], points[3], lit);
}

function shadowSurfaceY(x, y) {
  return x >= platformLeft && x <= platformRight && y <= platformY + 1
    ? platformY : floorY;
}

function drawSpotShadow(x, y, z, radius, color) {
  const surfaceY = shadowSurfaceY(x, y);
  const height = Math.max(0, surfaceY - y);
  const reach = Math.min(520, height * .22);
  const focus = clamp(1 - height / 5200, .42, 1);
  const shadowX = x + globalLight.x * reach;
  const shadowZ = z + globalLight.z * reach;
  const center = projectPoint(shadowX, surfaceY - 2, shadowZ);
  const edge = projectPoint(shadowX + radius, surfaceY - 2, shadowZ);
  if (![center.x, center.y, edge.x, edge.y].every(Number.isFinite) ||
      [center.x, center.y, edge.x, edge.y].some((value) => Math.abs(value) > 30000))
    return;
  const radiusX = Math.max(5, Math.abs(edge.x - center.x) *
    (.72 + .28 * focus));
  const radiusY = Math.max(3, radiusX * (.24 + .1 * focus));
  // Bind the shadow to the owning object's depth, then bias it away from the
  // camera. It remains above the terrain pass but can never win against the
  // object that casts it.
  triangleDepth = projectPoint(x, y, z).z + .018;
  const sides = 14;
  for (let side = 0; side < sides; side++) {
    const a = side * Math.PI * 2 / sides;
    const b = (side + 1) * Math.PI * 2 / sides;
    screenTriangle(center.x, center.y,
      center.x + Math.cos(a) * radiusX,
      center.y + Math.sin(a) * radiusY,
      center.x + Math.cos(b) * radiusX,
      center.y + Math.sin(b) * radiusY, ...color);
  }
}

function projectedBallRadius(ball) {
  const center = projectPoint(ball.x, ball.y, ball.z);
  const edge = projectPoint(ball.x + ball.radius, ball.y, ball.z);
  return Math.max(8, Math.abs(edge.x - center.x));
}

function drawBall(ball) {
  if (!ball.active) return;
  const point = projectPoint(ball.x, ball.y, ball.z);
  // A ball is a sphere in the orthographic world. Measuring its projected
  // world radius keeps the screen hit shape circular while still respecting
  // the perspective intro and loss cameras.
  const radius = projectedBallRadius(ball);
  if (!Number.isFinite(point.x) || !Number.isFinite(point.y) ||
      !Number.isFinite(radius) || radius > 12000 || point.x + radius < 0 ||
      point.x - radius > viewWidth() || point.y + radius < 0 ||
      point.y - radius > viewHeight) return;
  const soccer = ball.type === "soccer";
  filledDisc(point.x, point.y, radius * .92,
    soccer ? [226, 232, 224] : [232, 104, 28]);
  const seam = [42, 31, 29];
  if (soccer) {
    filledDisc(point.x, point.y, radius * .2, seam);
    for (let patch = 0; patch < 5; patch++) {
      const angle = ball.rotation + patch * Math.PI * 2 / 5;
      filledDisc(point.x + Math.cos(angle) * radius * .58,
        point.y + Math.sin(angle) * radius * .58,
        radius * .105, seam);
    }
  } else {
    const width = Math.max(2, radius * .075);
    for (const angle of [ball.rotation, ball.rotation + Math.PI / 2]) {
      const dx = Math.cos(angle) * radius * .84;
      const dy = Math.sin(angle) * radius * .84;
      filledCapsule(point.x - dx, point.y - dy,
        point.x + dx, point.y + dy, width, seam);
    }
    filledDisc(point.x, point.y, Math.max(1.5, width * .55), seam);
  }
}

function drawBallHitboxes() {
  const impactDebug = !roundResult &&
    runtime().monotonicUs < impactHitboxesUntil;
  if (!debugHitboxes && !impactDebug) return;
  const previousDepth = triangleDepth;
  triangleDepth = -1.46;
  for (const item of balls) {
    if (!item.active) continue;
    const point = projectPoint(item.x, item.y, item.z);
    const radius = projectedBallRadius(item);
    if (![point.x, point.y, radius].every(Number.isFinite) || radius > 5000 ||
        Math.abs(point.x) > 30000 || Math.abs(point.y) > 30000) continue;
    filledRing(point.x, point.y, radius + 5, radius + 2, [58, 222, 255]);
  }
  triangleDepth = previousDepth;
}

function drawGunPickup(pickup, t) {
  if (!pickup.active) return;
  const bobY = pickup.y + Math.sin(t * 3 + pickup.x * .001) * 24;
  const point = projectPoint(pickup.x, bobY, pickup.z);
  const scale = cameraScale();
  const color = [255, 220, 72];
  circle(point.x, point.y, Math.max(12, 72 * scale),
    Math.max(3, 10 * scale), color);
  worldLine(pickup.x - 62, bobY, pickup.z,
    pickup.x + 70, bobY, pickup.z, Math.max(4, 17 * scale), color);
  worldLine(pickup.x + 5, bobY, pickup.z,
    pickup.x + 36, bobY + 58, pickup.z, Math.max(4, 15 * scale), color);
  const labelSize = Math.max(13, Math.min(22, Math.round(48 * scale)));
  typeWrite("GUN", point.x - labelSize * 1.05,
    point.y - Math.max(24, 88 * scale),
    labelSize, ...color);
}

function drawBullet(bullet) {
  const color = players[bullet.owner].color;
  const point = projectPoint(bullet.x, bullet.y, bullet.z);
  const speed = Math.hypot(bullet.vx, bullet.vy) || 1;
  const tail = projectPoint(bullet.x - bullet.vx / speed * 130,
    bullet.y - bullet.vy / speed * 130, bullet.z);
  filledCapsule(tail.x, tail.y, point.x, point.y,
    Math.max(3, 13 * cameraScale()), color);
  filledDisc(point.x, point.y, Math.max(3, 12 * cameraScale()), color);
}

function drawGrenadePickup(pickup, t) {
  if (!pickup.active) return;
  const bobY = pickup.y + Math.sin(t * 3.2 + pickup.x * .001) * 24;
  const point = projectPoint(pickup.x, bobY, pickup.z);
  const scale = cameraScale();
  const color = [255, 105, 105];
  circle(point.x, point.y, Math.max(5, 34 * scale),
    Math.max(2, 10 * scale), color);
  worldLine(pickup.x, bobY - 34, pickup.z,
    pickup.x + 28, bobY - 62, pickup.z, Math.max(2, 8 * scale), color);
  typeWrite("GRENADE", point.x - 48, point.y - Math.max(20, 68 * scale),
    Math.max(10, Math.min(18, Math.round(42 * scale))), ...color);
}

function drawGrenade(grenade) {
  const point = projectPoint(grenade.x, grenade.y, grenade.z);
  if (grenade.exploding) {
    const radius = grenade.blastRadius * cameraScale();
    const width = Math.max(3, 10 * cameraScale());
    filledRing(point.x, point.y, radius,
      Math.max(0, radius - width), [255, 105, 105]);
    return;
  }
  const blink = grenade.fuse < .45 && Math.floor(grenade.fuse * 20) % 2 === 0;
  const color = blink ? [255, 255, 255] : players[grenade.owner].color;
  filledDisc(point.x, point.y, Math.max(4, 22 * cameraScale()), color);
  const tail = projectPoint(grenade.x - Math.sign(grenade.vx) * 90,
    grenade.y - 14, grenade.z);
  filledCapsule(tail.x, tail.y, point.x, point.y,
    Math.max(2, 6 * cameraScale()), color);
}

function drawWindFlag(t, color) {
  const poleX = (platformLeft + platformRight) / 2;
  const poleZ = 420;
  const poleBottom = platformY;
  const poleTop = platformY - 430;
  const length = 125 + windMph * 8;
  const gust = Math.sin(t * (4 + windMph * .16)) * (10 + windMph * .8);
  const tipX = poleX + windDirection * length;
  const tipY = poleTop + 45 + gust;
  const width = Math.max(2, 11 * cameraScale());
  worldLine(poleX, poleBottom, poleZ, poleX, poleTop, poleZ, width, color);
  worldLine(poleX, poleTop, poleZ, tipX, tipY, poleZ, width * .72, color);
  worldLine(tipX, tipY, poleZ, poleX, poleTop + 120, poleZ,
    width * .72, color);
  worldLine(poleX, poleTop + 120, poleZ, poleX, poleTop, poleZ,
    width * .72, color);
  worldLine(poleX - 55, poleBottom, poleZ, poleX + 55, poleBottom, poleZ,
    width, color);
}

function seededWindValue(index, channel = 0) {
  const text = (matchName || seriesName || "oskiewar") + ":" +
    index + ":" + channel;
  let hash = 2166136261;
  for (let cursor = 0; cursor < text.length; cursor++) {
    hash ^= text.charCodeAt(cursor);
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0) / 4294967295;
}

function drawWindLines(t, color) {
  const count = 7 + Math.floor(windMph / 3);
  const safe = actionSafeRect();
  const safeWidth = safe.right - safe.left;
  const safeHeight = safe.bottom - safe.top;
  const speed = .012 + windMph * .0082;
  const baseLength = 24 + windMph * 2.5;
  const previousDepth = triangleDepth;
  for (let index = 0; index < count; index++) {
    // Wind lives in screen-stable layers behind the fighters. Camera zoom may
    // reveal more world, but it no longer stretches streak length or speed.
    const depthAmount = seededWindValue(index, 0);
    const z = 120 + depthAmount * (worldFar - 240);
    const depthScale = .62 + (1 - depthAmount) * .55;
    const phase = seededWindValue(index, 1);
    const pace = .72 + seededWindValue(index, 2) * .7;
    const cycle = ((phase + t * speed * depthScale * pace * windDirection) % 1 + 1) % 1;
    const length = baseLength * depthScale *
      (.62 + seededWindValue(index, 3) * .9);
    const x = safe.left - length + cycle * (safeWidth + length * 2);
    const row = seededWindValue(index, 4);
    const flutterRate = 1.1 + seededWindValue(index, 5) * 2.6;
    const flutterPhase = seededWindValue(index, 6) * Math.PI * 2;
    const y = safe.top + 20 + row * Math.max(1, safeHeight - 40) +
      Math.sin(t * flutterRate + flutterPhase) * (2 + windMph * .22);
    const tailX = x - windDirection * length;
    const bend = Math.sin(t * (flutterRate + .5) + flutterPhase) *
      (1 + windMph * .08);
    const width = depthAmount < .35 && windMph > 12 ? 2 : 1;
    const ink = mixColor([18, 24, 48], color, .42 + (1 - depthAmount) * .4);
    triangleDepth = projectPoint(cameraCenter, cameraCenterY, z).z;
    filledCapsule(tailX, y - bend, x, y + bend, width, ink);
  }
  triangleDepth = previousDepth;
}

function drawSelectPortrait(player, x, y, scale, t) {
  const color = player.color;
  const head = { x, y: y - 130 * scale, radius: 34 * scale };
  filledDisc(head.x, head.y, head.radius + Math.max(2, 3 * scale), [8, 12, 24]);
  filledDisc(head.x, head.y, head.radius, color);
  line(x, y - 94 * scale, x, y + 20 * scale, 12 * scale, ...color);
  line(x, y - 65 * scale, x - 62 * scale, y - 8 * scale, 10 * scale, ...color);
  line(x, y - 65 * scale, x + 62 * scale, y - 8 * scale, 10 * scale, ...color);
  line(x, y + 20 * scale, x - 48 * scale, y + 112 * scale, 11 * scale, ...color);
  line(x, y + 20 * scale, x + 48 * scale, y + 112 * scale, 11 * scale, ...color);
  drawFace(player, head, contrastShadow(color), t);
}

function drawSelectionScreen(t, ink, panel) {
  const controls = controlLocale();
  if (compactLayout()) {
    const layout = selectionTouchLayout();
    const hover = selectionHover(layout);
    if (globalThis.__oskiewarTouch) globalThis.__oskiewarTouch.hover = hover;
    const margin = layout.roster[0].x;
    const width = layout.roster[0].width;
    typeWrite("SELECT A PAL", viewCenterX() - 145, 28, 36, ...ink);
    for (const row of layout.roster) {
      const fighter = fighterRoster[row.index];
      const chosen = players.filter((player) => player.rosterIndex === row.index);
      const hovered = hover?.roster === row.index;
      box(row.x, row.y, row.width, row.height,
        ...mixColor(panel, fighter.color, hovered ? .38 : chosen.length ? .22 : .06));
      box(row.x, row.y, hovered ? 10 : 6, row.height, ...fighter.color);
      typeWrite(fighter.handle, row.x + 20, row.y + (hovered ? 5 : 7),
        hovered ? 27 : 25,
        ...(chosen.length ? fighter.color : ink));
      const owners = chosen.map((player) => "p" + (player.pad + 1)).join(" ");
      if (owners) typeWrite(owners, row.x + row.width - 72,
        row.y + 11, 18, ...fighter.color);
    }
    for (const card of layout.cards) {
      const player = players[card.pad];
      const top = card.y;
      const profile = fighterProfile(player.name);
      const cardHovered = hover?.card === player.pad;
      const readyHovered = hover?.ready === player.pad;
      const modeHovered = hover?.mode === player.pad;
      box(margin, top, width, card.height,
        ...mixColor(panel, player.color, cardHovered ? .12 : 0));
      box(margin, top, width, touchSelectPad === player.pad ? 7 : 3,
        ...player.color);
      drawSelectPortrait(player, margin + 54, top + 72, .36, t);
      drawHandle(player.name, margin + 105, top + 21, 27,
        profile.colors, player.color);
      if (readyHovered) box(card.ready.x - 7, card.ready.y,
        card.ready.width + 14, card.ready.height,
        ...mixColor(panel, player.color, .26));
      typeWrite(player.bot ? "READY TO FIGHT" : player.npc ? "STANDING BY" : selectionReady[player.pad]
        ? "READY" : "SELECT", card.ready.x, card.ready.y + (readyHovered ? 7 : 9),
        readyHovered ? 29 : 27,
        ...player.color);
      if (modeHovered && card.mode) box(card.mode.x, card.mode.y,
        card.mode.width, card.mode.height,
        ...mixColor(panel, player.color, .28));
      typeWrite(player.bot ? "BOT" : player.npc ? "DUMMY" : "P" + (player.pad + 1),
        margin + width - 96, top + 76, 18, ...ink);
    }
    return;
  }
  const ox = viewOffsetX();
  const layout = selectionTouchLayout();
  const hover = selectionHover(layout);
  if (globalThis.__oskiewarTouch) globalThis.__oskiewarTouch.hover = hover;
  typeWrite("SELECT A PAL", ox + 810, 66, 42, ...ink);
  for (let index = 0; index < fighterRoster.length; index++) {
    const fighter = fighterRoster[index];
    const selected = players.some((player) => player.rosterIndex === index);
    const hovered = hover?.roster === index;
    const row = layout.roster[index];
    if (hovered) box(row.x, row.y, row.width, row.height,
      ...mixColor(panel, fighter.color, .32));
    typeWrite(fighter.handle, ox + 260 + index * 390, 225,
      hovered ? 40 : selected ? 38 : 28,
      ...(selected ? fighter.color : mixColor([105,115,145],[130,140,155],visualTheme.light)));
  }
  for (const player of players) {
    const left = ox + (player.pad === 0 ? 90 : 990);
    const card = layout.cards[player.pad];
    const cardHovered = hover?.card === player.pad;
    const readyHovered = hover?.ready === player.pad;
    const modeHovered = hover?.mode === player.pad;
    const profile = fighterProfile(player.name);
    box(left, 320, 840, 570,
      ...mixColor(panel, player.color, cardHovered ? .1 : 0));
    if (cardHovered) box(left, 320, 840, 7, ...player.color);
    drawSelectPortrait(player, left + 190, 590, 1.35, t);
    drawHandle(player.name, left + 355, 395, 46,
      profile.colors, player.color);
    const mood = profile.mood ? "MOOD  " + profile.mood.slice(0, 30) : "MOOD  —";
    const chat = profile.lastChat ? "CHAT  " + profile.lastChat.slice(0, 32) : "CHAT  —";
    typeWrite(mood + "\n" + chat, left + 355, 490, 25, ...ink);
    if (readyHovered) box(card.ready.x, card.ready.y,
      card.ready.width, card.ready.height,
      ...mixColor(panel, player.color, .24));
    typeWrite(player.bot ? "READY TO FIGHT" : player.npc ? "STANDING BY"
      : selectionReady[player.pad] ? "READY" : "SELECT",
      left + 355, readyHovered ? 716 : 720,
      readyHovered ? 56 : 52, ...player.color);
    if (modeHovered && card.mode) box(card.mode.x, card.mode.y,
      card.mode.width, card.mode.height,
      ...mixColor(panel, player.color, .26));
    typeWrite(player.bot ? "BOT" : player.npc ? "DUMMY" : "P" + (player.pad + 1),
      left + 355, 805, 30, ...ink);
  }
  typeWrite(controls.select, ox + 225, 958, 28, ...ink);
}

const titlePaletteNight = [
  [255, 105, 190], [111, 232, 210], [255, 232, 92], [130, 150, 255],
  [255, 126, 92], [188, 128, 255], [92, 205, 255], [246, 248, 255],
];
const titlePaletteDay = [
  [190, 32, 118], [0, 126, 124], [176, 112, 0], [73, 82, 190],
  [204, 59, 43], [114, 68, 185], [24, 107, 181], [47, 56, 82],
];

function animatedTitleColor(index, t, daylight = visualTheme.light) {
  const count = titlePaletteNight.length;
  // Native monotonic time can begin a fraction before startedAt settles.
  // Normalize the remainder so that first frame still lands in the palette.
  const phase = ((index + t * .42) % count + count) % count;
  const from = Math.floor(phase);
  const amount = phase - from;
  const eased = amount * amount * (3 - amount * 2);
  const night = mixColor(titlePaletteNight[from],
    titlePaletteNight[(from + 1) % count], eased);
  const day = mixColor(titlePaletteDay[from],
    titlePaletteDay[(from + 1) % count], eased);
  return mixColor(night, day, daylight);
}

function drawTitleScreen(t, ink) {
  const compact = compactLayout();
  const title = "oskiewar";
  const breath = 1 + Math.sin(t * .9) * .018;
  const titleSize = (compact ? 88 : 154) * breath;
  const titleWidth = handleWidth(title, titleSize);
  const titleX = viewCenterX() - titleWidth / 2;
  const titleY = viewHeight * (compact ? .38 : .35);

  // Sparse orbiting motes give attract mode some life without adding panels
  // or stripes behind the wordmark.
  const moteCount = compact ? 8 : 12;
  for (let index = 0; index < moteCount; index++) {
    const angle = t * (.18 + index % 3 * .025) + index * 2.39996;
    const reach = titleWidth * (.55 + Math.sin(t * .31 + index) * .06);
    const x = viewCenterX() + Math.cos(angle) * reach;
    const y = titleY + titleSize * .42 +
      Math.sin(angle * 1.17) * titleSize * (compact ? .88 : 1.05);
    const radius = (compact ? 2 : 3) + (index % 3);
    circle(x, y, radius, Math.max(1.5, radius * .48),
      animatedTitleColor(index, t * .7));
  }

  let cursor = titleX;
  for (let index = 0; index < title.length; index++) {
    const character = title[index];
    const bob = Math.sin(t * 2.05 + index * .72) * (compact ? 5 : 8);
    const drift = Math.cos(t * 1.12 + index * .91) * (compact ? 1.5 : 2.5);
    typeWrite(character, cursor + drift, titleY + bob, titleSize,
      ...animatedTitleColor(index, t));
    cursor += comicGlyphAdvance(character, titleSize);
  }

  const prompt = "start";
  const promptSize = hudTypeSize;
  const promptWidth = handleWidth(prompt, promptSize);
  const promptPulse = .68 + (Math.sin(t * 3.2) + 1) * .16;
  const promptInk = mixColor([196, 142, 18], [255, 238, 82], promptPulse);
  if (Math.floor(t * 2.4) % 2 === 0)
    typeWrite(prompt, viewCenterX() - promptWidth / 2,
      viewHeight * (compact ? .61 : .64), promptSize, ...promptInk);
  const titleNow = pacificTimeLabel(runtime().unixMs || Date.now());
  const stamp = buildTimestamp.match(/^(\d{4})\.(\d{2})\.(\d{2})\.(\d{2})(\d{2})/);
  const version = stamp
    ? "build " + stamp[2] + "." + stamp[3] + " " + stamp[4] + ":" + stamp[5]
    : "build " + buildTimestamp;
  const safe = hudSafeRect();
  typeWrite(titleNow, safe.left + 8, safe.bottom - hudTypeSize * 2 - 12,
    hudTypeSize, ...ink);
  typeWrite(version, safe.left + 8, safe.bottom - hudTypeSize - 4,
    hudTypeSize, ...ink);
}

function pacificTimeLabel(unixMs) {
  const date = new Date(unixMs);
  const year = date.getUTCFullYear();
  const nthSunday = (month, occurrence, hour) => {
    const first = new Date(Date.UTC(year, month, 1));
    const day = 1 + ((7 - first.getUTCDay()) % 7) + (occurrence - 1) * 7;
    return Date.UTC(year, month, day, hour);
  };
  const daylight = unixMs >= nthSunday(2, 2, 10) &&
    unixMs < nthSunday(10, 1, 9);
  const zone = daylight ? "PDT" : "PST";
  const local = new Date(unixMs - (daylight ? 7 : 8) * 3600000);
  const hour = local.getUTCHours();
  const minute = String(local.getUTCMinutes()).padStart(2, "0");
  return String(hour % 12 || 12) + ":" + minute + (hour < 12 ? "am " : "pm ") + zone;
}

function drawSpectatorQr(ink) {
  const safe = hudSafeRect();
  const compact = compactLayout();
  const metaSize = compact ? Math.max(20, hudTypeSize * .58) : hudTypeSize;
  if (debugHitboxes) {
    const fpsLabel = Math.round(displayFps || 0) + " fps";
    typeWrite(fpsLabel, safe.left + 4, safe.top + 2, metaSize, ...ink);
  }
  if (!spectatorQr || typeof spectatorQr.getModuleCount !== "function") return;
  const count = spectatorQr.getModuleCount();
  const quiet = 2;
  const targetSize = compact ? 108 : 158;
  const cell = Math.max(2, Math.floor(targetSize / (count + quiet * 2)));
  const size = (count + quiet * 2) * cell;
  const label = matchName;
  const labelSize = hudTypeSize;
  const labelWidth = label ? handleWidth(label, labelSize) : 0;
  const shadow = [24, 26, 34];
  const previousDepth = triangleDepth;
  triangleDepth = -1.43;
  const left = safe.right - size;
  const top = safe.top;
  const gameLabel = "oskiewar";
  const gameLabelWidth = handleWidth(gameLabel, metaSize);
  const gameLabelTop = compact ? top + hudTypeSize + 10 : top + 2;
  if (shellMode === "GAME" && !selecting)
    typeWrite(gameLabel, left - gameLabelWidth - 16, gameLabelTop,
      metaSize, ...ink);
  screenRect(left + 3, top + 3, size, size, shadow);
  screenRect(left, top, size, size, [250, 250, 247]);
  for (let row = 0; row < count; row++) {
    for (let column = 0; column < count; column++) {
      if (spectatorQr.isDark(row, column))
        screenRect(left + (column + quiet) * cell,
          top + (row + quiet) * cell, cell, cell, [7, 8, 14]);
    }
  }
  if (label) {
    const labelLeft = safe.right - labelWidth;
    const labelTop = top + size + 7;
    typeWrite(label, labelLeft + 3, labelTop + 4,
      labelSize, ...shadow);
    typeWrite(label, labelLeft, labelTop, labelSize, 250, 250, 247);
  }
  triangleDepth = previousDepth;
}

function drawRectOutline(rect, width, color) {
  filledCapsule(rect.left, rect.top, rect.right, rect.top, width, color);
  filledCapsule(rect.right, rect.top, rect.right, rect.bottom, width, color);
  filledCapsule(rect.right, rect.bottom, rect.left, rect.bottom, width, color);
  filledCapsule(rect.left, rect.bottom, rect.left, rect.top, width, color);
}

function drawImpacts() {
  for (const impact of impacts) {
    const point = projectPoint(impact.x, impact.y, impact.z || 0);
    const radius = (30 + (1 - impact.life / impact.duration) *
      (impact.explosion ? 420 : impact.death ? 260 : 100)) * cameraScale();
    if (![point.x, point.y, radius].every(Number.isFinite) || radius > 10000 ||
        Math.abs(point.x) + radius > 30000 || Math.abs(point.y) + radius > 30000)
      continue;
    filledCapsule(point.x - radius, point.y, point.x + radius, point.y,
      5, [255, 255, 255]);
    filledCapsule(point.x, point.y - radius, point.x, point.y + radius,
      5, [255, 255, 255]);
    filledCapsule(point.x - radius * .7, point.y - radius * .7,
      point.x + radius * .7, point.y + radius * .7, 4, [255, 232, 92]);
    filledCapsule(point.x + radius * .7, point.y - radius * .7,
      point.x - radius * .7, point.y + radius * .7, 4, [255, 105, 190]);
  }
}

function drawDetachedPart(fragment) {
  const first = projectPoint(fragment.x1, fragment.y1, fragment.z1);
  const second = projectPoint(fragment.x2, fragment.y2, fragment.z2);
  const width = Math.max(2, fragment.width * cameraScale());
  const values = [first.x, first.y, second.x, second.y, width];
  if (!values.every(Number.isFinite) || width > 2000) return;
  const margin = width + 4;
  if (Math.abs(first.x) + margin > 30000 ||
      Math.abs(first.y) + margin > 30000 ||
      Math.abs(second.x) + margin > 30000 ||
      Math.abs(second.y) + margin > 30000 ||
      Math.hypot(second.x - first.x, second.y - first.y) > 30000) return;
  filledCapsule(first.x, first.y, second.x, second.y, width + 3, [9, 12, 22]);
  filledCapsule(first.x, first.y, second.x, second.y, width, fragment.color);
}

function drawSafeZones() {
  if (!debugHitboxes) return;
  const hud = hudSafeRect();
  const border = mixColor([112, 136, 190], [25, 38, 72], visualTheme.light);
  drawRectOutline(hud, 3, border);
  const action = actionSafeRect();
  const hudDebug = [255, 214, 84];
  const actionDebug = [105, 255, 118];
  drawRectOutline(hud, 1, hudDebug);
  drawRectOutline(action, 2, actionDebug);
  typeWrite("hud safe", hud.left + 10, hud.top + 7, 17, ...hudDebug);
  typeWrite("fighter safe", action.left + 10, action.top + 7,
    17, ...actionDebug);
}

function gamePaint() {
  syncGameView();
  const run = runtime();
  if (lastPaintAt > 0 && run.monotonicUs > lastPaintAt) {
    const sample = clamp(1000000 / (run.monotonicUs - lastPaintAt), 1, 240);
    displayFps = displayFps ? lerp(displayFps, sample, .12) : sample;
  }
  lastPaintAt = run.monotonicUs;
  const t = (run.monotonicUs - startedAt) / 1000000;
  if (typeof ac === "function") acFeed = ac();
  for (const player of players) player.handleColors = fighterProfile(player.name).colors;
  visualTheme = losAngelesSun();
  triangleDepth = -1.4;
  const skyDay = mixColor([176, 215, 245], [255, 160, 112],
    visualTheme.sunset * .7);
  const sky = mixColor([7, 8, 28], skyDay, visualTheme.light);
  // Match the clear color to the arena sky. Camera framing can reveal the
  // clear layer during a jump; a different clear color looked like a flash.
  const outside = sky;
  const arenaDay = mixColor([230, 239, 247], skyDay, visualTheme.sunset * .55);
  const arena = mixColor([10, 13, 30], arenaDay, visualTheme.light);
  const ground = mixColor([14, 19, 31], [183, 194, 185], visualTheme.light);
  const platformColor = mixColor([24, 29, 46], [211, 198, 171],
    visualTheme.light);
  const titleInk = mixColor([245, 248, 255], [24, 35, 72], visualTheme.light);
  const statusShadow = contrastShadow(titleInk);
  const menuArena = [7, 10, 26];
  const menuPanel = [20, 28, 56];
  const menuInk = [245, 248, 255];
  wipe(...outside);
  if (shellMode === "MENU") {
    box(0, 0, viewWidth(), viewHeight, ...menuArena);
    drawTitleScreen(t, menuInk);
    drawSpectatorQr(menuInk);
    return;
  }
  if (selecting) {
    box(0, 0, viewWidth(), viewHeight, ...menuArena);
    drawSelectionScreen(t, menuInk, menuPanel);
    drawSpectatorQr(menuInk);
    return;
  }
  const cinematicAge = deathCinematicAge(run.monotonicUs);
  if (cinematicAge < 0 || cinematicAge >= 1.45) containFighters(t);
  cameraDoll.prepare();
  worldQuad(
    { x: worldLeft, y: ceilingY, z: worldFar },
    { x: worldRight, y: ceilingY, z: worldFar },
    { x: worldRight, y: floorY, z: worldFar },
    { x: worldLeft, y: floorY, z: worldFar }, sky);
  worldQuad(
    { x: worldLeft, y: floorY, z: worldNear },
    { x: worldRight, y: floorY, z: worldNear },
    { x: worldRight, y: floorY, z: worldFar },
    { x: worldLeft, y: floorY, z: worldFar }, ground);
  const boundary = mixColor([12, 17, 38], arena, .72);
  worldQuad(
    { x: worldLeft, y: ceilingY, z: worldNear },
    { x: worldLeft, y: ceilingY, z: worldFar },
    { x: worldRight, y: ceilingY, z: worldFar },
    { x: worldRight, y: ceilingY, z: worldNear }, boundary);
  worldQuad(
    { x: worldLeft, y: ceilingY, z: worldNear },
    { x: worldLeft, y: floorY, z: worldNear },
    { x: worldLeft, y: floorY, z: worldFar },
    { x: worldLeft, y: ceilingY, z: worldFar }, boundary);
  worldQuad(
    { x: worldRight, y: ceilingY, z: worldFar },
    { x: worldRight, y: floorY, z: worldFar },
    { x: worldRight, y: floorY, z: worldNear },
    { x: worldRight, y: ceilingY, z: worldNear }, boundary);
  const platformNear = -520;
  const platformFar = 520;
  worldQuad(
    { x: platformLeft, y: platformY, z: platformNear },
    { x: platformRight, y: platformY, z: platformNear },
    { x: platformRight, y: platformY, z: platformFar },
    { x: platformLeft, y: platformY, z: platformFar }, platformColor);
  const shadowInk = mixColor([3, 5, 14], [92, 99, 101],
    visualTheme.light * .72);
  for (const player of players)
    if (player.alive || roundResult)
      drawSpotShadow(player.x, player.y, player.z, player.ducking ? 72 : 92,
        shadowInk);
  for (const item of balls)
    if (item.active)
      drawSpotShadow(item.x, item.y, item.z, item.radius * 1.18, shadowInk);
  const windInk = windDirection < 0
    ? mixColor([72, 174, 255], [28, 88, 188], visualTheme.light)
    : mixColor([255, 92, 132], [184, 35, 62], visualTheme.light);
  drawWindLines(t, windInk);
  drawWindFlag(t, windInk);
  const timedRound = roundIsTimed();
  const remainingSeconds = roundResult || !timedRound ? 0 : Math.max(0,
    Math.ceil((roundDurationUs - roundElapsedUs) / 1000000));
  const timerText = roundResult
    ? roundResult === "TIE" ? "tie!" : ""
    : timedRound ? String(remainingSeconds).padStart(2, "0") : "∞";
  const hud = hudSafeRect();
  const timerSize = hudTypeSize;
  const timerWidth = handleWidth(timerText, timerSize);
  const timerDanger = timedRound && remainingSeconds > 0 &&
    remainingSeconds <= 10;
  const timerShake = timerDanger
    ? Math.sin(t * 35) * (11 - remainingSeconds) * .45 : 0;
  const timerInk = timerDanger
    ? mixColor(titleInk, [235, 38, 58], (11 - remainingSeconds) / 10)
    : titleInk;
  typeWrite(timerText, viewCenterX() - timerWidth / 2 + timerShake,
    hud.top + 2, timerSize, ...timerInk);
  if (roundViewer) {
    const viewerLabel = roundViewerMode || roundViewerStatus;
    typeWrite(viewerLabel, hud.right - viewerLabel.length * 18, hud.top + 7, 24,
      ...(roundViewerMode === "LIVE" ? [210, 42, 62] : titleInk));
    typeWrite(matchName, hud.left + 8, hud.top + 7, 20, ...titleInk);
  }
  for (const pickup of gunPickups) drawGunPickup(pickup, t);
  for (const pickup of grenadePickups) drawGrenadePickup(pickup, t);
  const introAge = run.monotonicUs - roundStartedAt;
  const showRunnerLabels = Boolean(roundResult) || introAge >= introDurationUs;
  const viewDirection = normalize3({
    x: cameraDoll.target.x - cameraDoll.position.x,
    y: cameraDoll.target.y - cameraDoll.position.y,
    z: cameraDoll.target.z - cameraDoll.position.z,
  });
  const renderables = [
    ...bullets.map((item) => ({
      kind: "bullet", item, x: item.x, y: item.y, z: item.z })),
    ...grenades.filter((item) => item.alive).map((item) => ({
      kind: "grenade", item, x: item.x, y: item.y, z: item.z })),
    ...balls.filter((item) => item.active).map((item) => ({
      kind: "ball", item, x: item.x, y: item.y, z: item.z })),
    ...detachedParts.map((item) => ({ kind: "detached", item,
      x: (item.x1 + item.x2) / 2, y: (item.y1 + item.y2) / 2,
      z: (item.z1 + item.z2) / 2 })),
    ...players.filter((item) => !(cinematicAge >= .11 && cinematicAge < .86 &&
      deathCinematic?.winnerPad === item.pad)).map((item) => ({
      kind: "player", item, x: item.x, y: item.y, z: item.z })),
  ];
  const depth = (item) => dot3({
    x: item.x - cameraDoll.position.x,
    y: item.y - cameraDoll.position.y,
    z: item.z - cameraDoll.position.z,
  }, viewDirection);
  renderables.sort((a, b) => depth(b) - depth(a));
  for (const renderable of renderables) {
    triangleDepth = projectPoint(renderable.x, renderable.y, renderable.z).z;
    if (renderable.kind === "bullet") drawBullet(renderable.item);
    else if (renderable.kind === "grenade") drawGrenade(renderable.item);
    else if (renderable.kind === "ball") drawBall(renderable.item);
    else if (renderable.kind === "detached") drawDetachedPart(renderable.item);
    else drawRunner(renderable.item, t, showRunnerLabels);
  }
  triangleDepth = -1.42;
  drawDebugHitboxes(players[0], t);
  drawDebugHitboxes(players[1], t);
  drawBallHitboxes();
  drawImpacts();
  if (!roundResult && introAge < introDurationUs) {
    const introSeconds = introAge / 1000000;
    drawFightIntro(introSeconds, titleInk, statusShadow);
  }
  const resultUiReady = cinematicAge < 0 || cinematicAge >= 1.1;
  if (roundResult && resultUiReady) {
    if (instantReplay) {
      const frame = Math.min(instantReplay.frames.length,
        Math.floor(instantReplay.cursor) + 1);
      const replayLabel = "REPLAY  " + frame + "/" + instantReplay.frames.length;
      typeWrite(replayLabel, viewCenterX() - replayLabel.length * 10,
        820, 30, ...titleInk);
      const locale = controlLocale();
      const controls = instantReplay.paused
        ? locale.replayPaused : locale.replayPlaying;
      typeWrite(controls, viewCenterX() - controls.length * 7.5,
        948, 23, ...titleInk);
    } else {
      const result = resultCardText();
      const winnerSize = Math.min(92,
        Math.max(40, (viewWidth() - 72) / Math.max(1, result.winner.length * .85)));
      const winnerWidth = handleWidth(result.winner, winnerSize);
      typeWrite(result.winner, viewCenterX() - winnerWidth / 2 + 5, 816,
        winnerSize, ...statusShadow);
      typeWrite(result.winner, viewCenterX() - winnerWidth / 2, 810,
        winnerSize, ...titleInk);
      if (result.action) {
        const actionSize = Math.min(44, winnerSize * .54);
        const actionWidth = handleWidth(result.action, actionSize);
        typeWrite(result.action, viewCenterX() - actionWidth / 2 + 3, 900,
          actionSize, ...statusShadow);
        typeWrite(result.action, viewCenterX() - actionWidth / 2, 896,
          actionSize, ...titleInk);
      }
      if (!roundViewer) {
        const replayControl = controlLocale().replay;
        typeWrite(replayControl, viewCenterX() - replayControl.length * 7.5,
          948, 22, ...titleInk);
      }
    }
  }
  if ((roundResult && resultUiReady) || (!roundResult && introAge >= introDurationUs)) {
    drawPlayerHandle(players[0], t, 0);
    drawPlayerHandle(players[1], t, 1);
    drawCommandStream(players[0], 0);
    drawCommandStream(players[1], 1);
  }
  drawSafeZones();
  drawDeathFlash();
  drawSpectatorQr(titleInk);
}

function sim() {
  if (clientError) return;
  try {
    gameSim();
  } catch (error) {
    captureClientError("sim", error);
  }
}

function paint() {
  if (clientError) {
    drawClientError();
    return;
  }
  try {
    gamePaint();
  } catch (error) {
    captureClientError("paint", error);
    drawClientError();
  }
}

function act() {}
function leave() {
  roundViewerStop?.();
  roundViewerStop = null;
  roundViewer = null;
}
