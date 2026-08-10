// @bundle-qr
// The console's monotonic clock can hand back a negative number: App.cpp
// converts QPC ticks with `counter * 1000000`, which overflows int64 past
// about ten days of uptime. Every deadline in this piece starts at 0 and is
// compared with `now >=` or `now <`, so a negative clock made `hitStunUntil`
// permanently in the future -- no attack could fire and no round could be
// won -- stranded the title, silenced the input log, and indexed off the
// front of a palette. Rebase once, here, so time starts at zero whatever the
// host reports. The host builds a fresh object per call, so this rewrites
// that copy rather than allocating another.
const hostRuntime = runtime;
let clockEpoch = null;
runtime = function acRuntime() {
  const info = hostRuntime();
  const raw = Number(info.monotonicUs) || 0;
  if (clockEpoch === null) clockEpoch = raw;
  info.monotonicUs = raw - clockEpoch;
  if (typeof info.simMonotonicUs === "number")
    info.simMonotonicUs -= clockEpoch;
  return info;
};

// Monotonic count of committed revisions to this piece (next revision included).
const buildVersion = 25;
const floorY = 1800;
const ceilingY = 0;
const wallThickness = 80;
const worldLeft = 0;
// A long street gives the skateboard room to gather speed between the two
// quarter-pipe ends. Fighters still spawn close together near the left end;
// the camera reveals the rest only when play travels there.
const worldRight = 25000;
const worldNear = -900;
const worldFar = 900;
const terrainAmplitude = 46;
// Preserve roughly the same tessellation density as the former 5,000-unit
// arena so stretching the street does not stretch its floor polygons.
const terrainSamples = 240;
let terrainPhase = 0;
function terrainSeed(value) {
  let hash = 2166136261;
  for (const character of String(value || "oskiewar")) {
    hash ^= character.charCodeAt(0);
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0) / 4294967296 * Math.PI * 2;
}
function terrainFloorAt(x) {
  const nx = clamp((x - worldLeft) / (worldRight - worldLeft), 0, 1);
  const edge = Math.sin(nx * Math.PI) ** 2;
  const broad = Math.sin(nx * Math.PI * 3 + terrainPhase);
  const detail = Math.sin(nx * Math.PI * 7 - terrainPhase * .63) * .34;
  // Two circular quarter-pipes meet the floor tangentially and approach a
  // vertical tangent at each coping: an actual half-pipe cross-section.
  const transitionRadius = 720;
  const wallDistance = Math.min(x - worldLeft, worldRight - x);
  const transition = clamp((transitionRadius - wallDistance) /
    transitionRadius, 0, 1);
  const halfPipeRise = transitionRadius *
    (1 - Math.sqrt(Math.max(0, 1 - transition * transition)));
  const terrainNoise = (broad + detail) * terrainAmplitude * edge *
    (1 - transition);
  return floorY - terrainNoise - halfPipeRise;
}
const stageLeft = 0;
let stageRight = 1920;
let stageTop = 112;
// Leave a narrow projection gutter beneath the floor for the screen-edge HUD.
let stageBottom = 930;
let viewHeight = 1080;
let cameraAspect = (stageRight - stageLeft) / (stageBottom - stageTop);
// The stage furniture is flagged out for now — @jeffrey wants the arena to
// read as one floor. Flip PLATFORM true and the ledge comes back whole:
// drawing, collision, pickup lanes, grenade and ball bounces, the bot's
// sink-and-chase play. The wind flag rides its own switch the same way.
const PLATFORM = false;
const WIND_FLAG = false;
const platformLeft = 650;
const platformRight = 1350;
// A jump apexes 290 above the floor, so the ledge sits inside a single hop and
// inside the tight opening frame. At its old 1600 only an ultra jump reached
// it, which put every platform powerup out of play and off screen.
const platformY = 1560;
const doubleTapUs = 280000;
const doubleTapReleaseUs = 40000;
const roundDurationUs = 30000000;
const roundResultUs = 3000000;
const matchResultUs = 5000000;
const introDurationUs = 3000000;
const dummyGuideDurationUs = 150000000;
const matchWins = 5;
const errorRestartUs = 16000000;
const errorDumpBase = "https://oskiewar.com/api/oskiewar-dump?d=";
const powerupIntervalUs = 10000000;
const shieldRadius = 160;
const shieldForward = 30;
const grenadeBlastDuration = .68;
const grenadeBlastRadius = 620;
// Ground pound. Double-tapping DOWN in the air used to be a plain fast drop
// that did nothing on arrival; now it commits you. Holding DOWN through the
// fall buys speed, the crater scales with how far you actually fell, and the
// landing balls you — so the move is a trade, never a free hit.
const poundLaunchVelocity = 1500;
const poundHoldAcceleration = 5600;
const poundMaxVelocity = 4600;
// Roughly a jump-and-a-half of floor-to-ceiling travel. Falling further than
// this cannot buy a bigger crater, so the ceiling is not a weapon.
const poundFullFall = 900;
// A first-stage landing is deliberately body-sized: the visible ring is the
// attack, not a warning decal for a much larger invisible hit. Repeated DOWN
// taps still grow it, but never into a room-clearing shortcut.
const poundMinRadius = 165;
const poundMaxRadius = 420;
const boosterXs = [];
const boosterRadius = 115;
const boosterVelocity = 6500;
const replayTickUs = 16667;
const replayCheckpointUs = 1000000;
const liveSnapshotIntervalUs = 50000;
const fighterAnimationSpecs = {
  // WALK ran a whole stride in 12 ticks — five cycles a second, which the eye
  // reads as a vibrating fighter rather than as legs taking steps. Three ticks
  // a frame puts the cycle at 600ms, close to a real walk, so the steps count.
  IDLE: [72, 3, "BREATHE", true], WALK: [12, 3, "STRIDE", true],
  RUN: [10, 1, "RUN", true], MEDITATE: [24, 2, "STILL", true],
  DASH: [8, 1, "BURST", true], CROUCH: [8, 1, "TUCK", true],
  "AIR CROUCH": [8, 1, "TUCK", true], JUMP: [12, 1, "ASCEND", true],
  FALL: [12, 1, "DESCEND", true], PUNCH: [14, 1, "ATTACK", false],
  KICK: [14, 1, "ATTACK", false], SHIELD: [12, 2, "GUARD", true],
  REACH: [10, 1, "REACH", false], HOLD: [16, 2, "CLUTCH", true],
  FIRE: [11, 1, "FIRE", false], THROW: [16, 1, "THROW", false],
  WHIP: [11, 1, "ATTACK", false], BASH: [17, 1, "ATTACK", false],
  HIT: [10, 1, "STUN", false], POGO: [12, 1, "BOUNCE", true],
  "CROUCH HOP": [8, 1, "HOP", true], SINK: [10, 1, "SINK", true],
  KO: [16, 2, "BREAK", false],
};
// Every melee kind in one table: how far the limb reaches at rest and at full
// extension, how long each of its two bones is (the real reach ceiling), how
// high it lands, how wide it reads against the ball, how long the hitbox
// lives, and what it does on contact. A loaded hand cannot make a clean fist,
// so an item swing is its own kind rather than a punch modifier — a pistol
// lengthens the arm into a fast light lash, a grenade shortens it into a club.
const meleeSpecs = {
  PUNCH: { reach: 58, swell: 50, span: 58, height: 115, radius: 28,
    windowUs: 220000, force: 1200, lift: 140,
    cue: ["snare", 1.05], thud: ["block", 1] },
  KICK: { reach: 75, swell: 62, span: 74, height: 55, radius: 35,
    windowUs: 220000, force: 1550, lift: 220,
    cue: ["kick", 1.05], thud: ["block", 1] },
  WHIP: { reach: 92, swell: 74, span: 76, height: 122, radius: 24,
    windowUs: 190000, force: 1000, lift: 110,
    cue: ["whoosh", 1.15], thud: ["hat", 1.35] },
  BASH: { reach: 62, swell: 40, span: 58, height: 108, radius: 40,
    windowUs: 280000, force: 1750, lift: 210,
    cue: ["kick", 1.3], thud: ["kick", 1.4] },
};
// One lookup serves both Y and the loaded punch, so the thing you can see in
// the fighter's hand is always the thing that fires and the thing that swings.
const itemMelee = { GUN: "WHIP", GRENADE: "BASH" };
const instantReplayStepUs = 33333;
// Replay speed ramping: how far a full-force hit drags the playhead down, how
// many frames of warning the ramp gets, and how fast the speed itself may
// change — the last one is what keeps a slowdown from reading as a stutter.
const replaySlowest = .18;
const replayActionLead = 8;
const replayRampPerSecond = 3.2;
const instantReplayMaxFrames = 240;
const walkSpeed = 1060;
const runStartSpeed = 1320;
const runTopSpeed = 2350;
const runAcceleration = 820;
// Vertical feel. The apex is the design constant — how high a fighter can
// reach never changed — so every impulse here is paired with a gravity that
// spends less time getting there. Rise is lighter than fall so the arc reads
// as intent going up and commitment coming down.
const riseGravity = 4800;
const fallGravity = 7200;
const jumpVelocity = 1760;
const crouchJumpVelocity = 1960;
const ultraJumpVelocity = 3960;
const crouchHopVelocity = 980;
const pogoBounceVelocity = 2100;
const headBounceVelocity = 1100;
const jumpAnticipationUs = 50000;
// Letting go of up mid-rise snips the arc: a tap is a hop, a hold is a jump.
const jumpCutScale = .55;
const jumpPoseUs = 75000;
const crouchJumpPoseUs = 145000;
const crouchHopPoseUs = 190000;
const sinkDurationUs = 250000;
const hudTypeSize = 42;
// The command stream is a record of what you just did, so it is long enough to
// hold a whole exchange and it does not start dissolving while you are still
// playing. Only once the pad goes quiet does it age out, oldest glyph first,
// so the last thing you pressed is the last thing to leave.
const commandStreamDepth = 20;
const commandStreamRows = 5;
const commandStreamColumns = 8;
const commandHoldUs = 1100000;
const commandFadeUs = 1900000;
const replayButtons = ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown",
  "A", "B", "X", "Y"];
let cameraCenter = (worldLeft + worldRight) / 2;
let cameraWidth = worldRight - worldLeft;
let cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
let cameraContainFloor = 0;
// Two different distances, because they answer two different questions.
//
// `cameraPin` is where a single projected point stops being allowed to get any
// nearer. Nothing can clip a point — a shadow ellipse or a capsule end is one
// coordinate, not a face — so the best available answer is to hold it at a
// sane depth and accept that a thing behind the lens is drawn wrong. This is
// the distance the projection has always used.
const cameraPin = 80;
// `cameraNear` is the real near plane, and faces are cut at it. It is much
// closer than the pin on purpose. The cut is a straight line at constant
// depth, and where that line lands on screen is decided by how much the lens
// magnifies at the plane -- and this lens is only a tenth perspective in
// normal play, which dilutes that magnification tenfold. Clipping the floor at
// the pin distance put its cut line just inside the bottom of the frame and
// opened a band of sky under the fighters' feet. Ten times nearer drives the
// cut off screen for any camera that is meaningfully above the floor, and the
// coordinates it asks for in exchange are what the guard band is for.
const cameraNear = 8;
// Projected faces are cut to a band one viewport wide on every side before
// they are handed over. The scene is allowed to run off screen; it is not
// allowed to run off into coordinates the rasterizer cannot hold, which is
// what a vertex sitting on the near plane will otherwise ask for.
const guardBand = 1;
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
    const zoomSpeed = spec.width > this.width ? speed * 1.9 : speed * .58;
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

  // World space to camera space, with the near distance left alone. Anything
  // that wants to survive contact with the near plane has to see the honest
  // depth first — `project` is where it gets clamped, and a clamp is a lie for
  // anything behind the lens.
  toView(point) {
    if (this.dirty || !this.view) this.prepare();
    const { forward, right, up } = this.view;
    const delta = { x: point.x - this.position.x, y: point.y - this.position.y,
      z: point.z - this.position.z };
    return { x: dot3(delta, right), y: dot3(delta, up),
      z: dot3(delta, forward) };
  }

  projectView(view) {
    if (this.dirty || !this.view) this.prepare();
    const { centerX, centerY, orthoScale, focal } = this.view;
    const depth = Math.max(cameraNear, view.z);
    const orthoX = centerX + view.x * orthoScale;
    const orthoY = centerY - view.y * orthoScale;
    const perspectiveX = centerX + view.x * focal / depth;
    const perspectiveY = centerY - view.y * focal / depth;
    return { x: lerp(orthoX, perspectiveX, this.perspective),
      y: lerp(orthoY, perspectiveY, this.perspective),
      z: clamp(depth / 16000 * 2.8 - 1.4, -1.4, 1.4) };
  }

  // A lone point cannot be clipped, so it is held at the pin rather than
  // allowed to divide by a depth it does not have. Faces go through
  // `worldTriangle`, which cuts them at the near plane and then comes back
  // here with vertices that are already in front of it.
  project(point) {
    const view = this.toView(point);
    view.z = Math.max(cameraPin, view.z);
    return this.projectView(view);
  }
}

const cameraDoll = new FightCamDoll();
const cameraScale = () => (stageRight - stageLeft) / cameraDoll.width;
let playerCameraYaw = 0;
let playerCameraPitch = 0;
let triangleDepth = -1.4;
// A match frame submits ~2100 faces. Buffering them into a Float32Array first
// cost 5.7ms a frame where handing each one straight over costs 1.8ms —
// twelve typed-array element writes in QuickJS are dearer than twelve
// arguments to one C call. The batched host call itself measured 0.05ms, so
// the boundary crossing was never the expense.
const emitTriangle = typeof triangle3d === "function" ? triangle3d
  : (x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) =>
    triangle(x1, y1, x2, y2, x3, y3, r, g, b);
// r/g/b stay positional so the hottest path in the piece doesn't build a rest
// array per face; the host defaults every missing channel to 255 the same way.
//
// The native buffer speaks int16: a coordinate past ±32768 — or the NaN a
// projection returns after crossing the camera plane — is a RangeError and a
// dead frame on console. Geometry that broken was never going to be visible,
// so the seam culls it instead of crashing. NaN fails a comparison on its
// own, which keeps the guard to plain compares on the hottest path.
const triangleSafe = (value) => value > -32200 && value < 32200;
function screenTriangle(x1, y1, x2, y2, x3, y3, r = 255, g = 255, b = 255) {
  if (!(triangleSafe(x1) && triangleSafe(y1) && triangleSafe(x2) &&
      triangleSafe(y2) && triangleSafe(x3) && triangleSafe(y3))) return;
  emitTriangle(x1, y1, triangleDepth, x2, y2, triangleDepth,
    x3, y3, triangleDepth, r, g, b);
}
function screenRect(x, y, width, height, color) {
  const [r, g, b] = color;
  screenTriangle(x, y, x + width, y, x + width, y + height, r, g, b);
  screenTriangle(x, y, x + width, y + height, x, y + height, r, g, b);
}
function screenStrokeRect(x, y, width, height, thickness, color) {
  screenRect(x, y, width, thickness, color);
  screenRect(x, y + height - thickness, width, thickness, color);
  screenRect(x, y + thickness, thickness, height - thickness * 2, color);
  screenRect(x + width - thickness, y + thickness,
    thickness, height - thickness * 2, color);
}
function projectedTriangle(a, b, c, color) {
  if (!(triangleSafe(a.x) && triangleSafe(a.y) && triangleSafe(a.z) &&
      triangleSafe(b.x) && triangleSafe(b.y) && triangleSafe(b.z) &&
      triangleSafe(c.x) && triangleSafe(c.y) && triangleSafe(c.z))) return;
  emitTriangle(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, ...color);
}
// Sutherland-Hodgman, one plane at a time. Both clips below are the same walk:
// step the polygon's edges, keep the vertices that are inside, and whenever an
// edge crosses out or in, keep the crossing point too.
function clipPolygon(polygon, distance, mix) {
  const kept = [];
  for (let index = 0; index < polygon.length; index++) {
    const current = polygon[index];
    const next = polygon[(index + 1) % polygon.length];
    const here = distance(current);
    const there = distance(next);
    if (here >= 0) kept.push(current);
    if ((here >= 0) !== (there >= 0))
      kept.push(mix(current, next, here / (here - there)));
  }
  return kept;
}
const mixVertex = (a, b, amount) => ({ x: lerp(a.x, b.x, amount),
  y: lerp(a.y, b.y, amount), z: lerp(a.z, b.z, amount) });
// The near plane, in camera space, before the divide. This is the whole point:
// a vertex behind the lens has a negative depth, and pinning it to the near
// distance keeps its sideways offset while dividing by a distance it never
// had — so a face that crosses the plane used to shear halfway across the
// screen, or blow past the coordinate guard and vanish outright. Cutting it at
// the plane leaves only geometry that is honestly in front of the camera.
const clipViewNear = (polygon) =>
  clipPolygon(polygon, (vertex) => vertex.z - cameraNear, mixVertex);
// And the guard band, in screen space, after it. A vertex sitting exactly on
// the near plane projects to focal/near times its offset — twenty-three times,
// at this lens — so a legitimately visible floor can still ask for coordinates
// far outside anything the rasterizer will take. Cut it to a band a viewport
// wide instead of dropping the face and leaving a hole in the ground.
function clipScreenBand(polygon) {
  const width = viewWidth();
  const margin = guardBand;
  const edges = [
    (vertex) => vertex.x + width * margin,
    (vertex) => width * (1 + margin) - vertex.x,
    (vertex) => vertex.y + viewHeight * margin,
    (vertex) => viewHeight * (1 + margin) - vertex.y,
  ];
  let clipped = polygon;
  for (const distance of edges) {
    if (clipped.length < 3) return [];
    clipped = clipPolygon(clipped, distance, mixVertex);
  }
  return clipped;
}
// One world-space face, all the way through: to camera space, cut at the near
// plane, projected, cut to the guard band, then fanned into whatever triangles
// are left. Zero of them is a face entirely behind the camera, which is a
// perfectly good answer.
function worldTriangle(a, b, c, color) {
  const near = clipViewNear([cameraDoll.toView(a), cameraDoll.toView(b),
    cameraDoll.toView(c)]);
  if (near.length < 3) return;
  const projected = near.map((vertex) => cameraDoll.projectView(vertex));
  if (projected.some((point) => !Number.isFinite(point.x) ||
    !Number.isFinite(point.y))) return;
  // A near-plane fan can be a quad; fanning it before the band clip would
  // clip the same interior edge twice, so the band takes the whole polygon.
  const face = clipScreenBand(projected);
  for (let corner = 2; corner < face.length; corner++)
    projectedTriangle(face[0], face[corner - 1], face[corner], color);
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
// Per-glyph color is how a community @handle carries its owner's identity.
// The house opponents are not people, so they hold one flat color each and
// let the glyph walk fall back to it.
const npcFighter = { handle: "DUMMY", color: [105, 125, 150], colors: [],
  mood: "TRAINING DUMMY · NO BOT AI", lastChat: "" };
const anonymousFighter = { handle: "", color: [72, 176, 156], colors: [
  [72,176,156],[118,214,188],[54,128,168],[230,205,92]],
  mood: "ANONYMOUS", lastChat: "" };
const spiderDummyFighter = { handle: "SPIDERDUMMY", color: [76, 88, 108],
  colors: [], mood: "GIANT SEGMENTED TRAINING FOE · NO BOT AI", lastChat: "" };
const botFighter = { handle: "BOT", color: [205, 48, 72], colors: [],
  mood: "ANGRY TRAINING BOT", lastChat: "" };
const peopleFighter = { handle: "PPL", color: [116, 122, 136], colors: [],
  mood: "", lastChat: "" };
// Self-play needs two nameplates nobody can mix up, or the result card reads
// "BOT WINS" with no way to tell which bot won.
const selfPlayFighters = [
  { handle: "BOT 1", color: [205, 48, 72], colors: [],
    mood: "MECHANICAL TEST · SELF PLAY", lastChat: "" },
  { handle: "BOT 2", color: [48, 118, 205], colors: [],
    mood: "MECHANICAL TEST · SELF PLAY", lastChat: "" },
];

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

function displayTheme() {
  const sun = losAngelesSun();
  const caps = typeof capabilities === "function" ? capabilities() : {};
  if (caps.platform === "web" || caps.platform === "macos") {
    return { ...sun, light: caps.colorScheme === "light" ? 1 : 0 };
  }
  return sun;
}
const players = [
  { name: "@JEFFREY", rosterIndex: 0, handleColors: fighterRoster[0].colors,
    pad: 0, spawnX: 2240, x: 2240, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: 1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: 0, color: [190, 42, 58], hit: 0,
    hitSegment: -1, hitSegmentUntil: 0, hitStunUntil: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    skateboard: false, skateVx: 0,
    suppressedDirections: [],
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, runSince: 0,
    walkSince: 0, roundWins: 0,
    attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false, blocking: false, blockFlash: 0,
    shieldLocked: false, shieldBrokenAt: 0,
    shieldCrouched: false, shieldAimX: 0, shieldAimY: 0,
    windVx: 0, knockVx: 0, gunAmmo: 0, grenadeAmmo: 0,
    gunAimX: 1, gunAimY: 0, gunMode: "HANDGUN", nextGunShotAt: 0,
    stance: "NEUTRAL",
    heldBall: -1, heldPart: -1, heldPlayer: -1, grabbedBy: -1,
    grabHeld: false, crouchBlend: 0, standingOn: -1,
    partDamage: {}, removedParts: [], pogoHit: false, pogoDive: false,
    pounding: false, poundFrom: 0, poundLevel: 0,
    commandStream: [],
    botPresses: {},
    jumpLaunchAt: 0, jumpPoseUntil: 0, landPoseUntil: 0,
    jumpHeld: false, airJumpsUsed: 0, hopUntil: 0, sinkUntil: 0,
    crouchJump: false, attackMomentum: 1 },
  { name: "@OSKIE", rosterIndex: 2, handleColors: fighterRoster[2].colors,
    npc: false, bot: false,
    pad: 1, spawnX: 2760, x: 2760, y: floorY, z: 0,
    vx: 0, vy: 0, vz: 0, facing: -1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: 0, color: [38, 82, 176], hit: 0,
    hitSegment: -1, hitSegmentUntil: 0, hitStunUntil: 0,
    alive: true, respawnAt: 0, score: 0, inputX: 0, inputY: 0,
    skateboard: false, skateVx: 0,
    suppressedDirections: [],
    lastTap: {}, lastRelease: {}, dashUntil: 0, dashVx: 0, runSince: 0,
    walkSince: 0, roundWins: 0,
    attackKind: "", attackStartedAt: 0,
    attackUntil: 0, attackHit: false, blocking: false, blockFlash: 0,
    shieldLocked: false, shieldBrokenAt: 0,
    shieldCrouched: false, shieldAimX: 0, shieldAimY: 0,
    windVx: 0, knockVx: 0, gunAmmo: 0, grenadeAmmo: 0,
    gunAimX: -1, gunAimY: 0, gunMode: "HANDGUN", nextGunShotAt: 0,
    stance: "NEUTRAL",
    heldBall: -1, heldPart: -1, heldPlayer: -1, grabbedBy: -1,
    grabHeld: false, crouchBlend: 0, standingOn: -1,
    partDamage: {}, removedParts: [], pogoHit: false, pogoDive: false,
    pounding: false, poundFrom: 0, poundLevel: 0,
    commandStream: [],
    botPresses: {},
    jumpLaunchAt: 0, jumpPoseUntil: 0, landPoseUntil: 0,
    jumpHeld: false, airJumpsUsed: 0, hopUntil: 0, sinkUntil: 0,
    crouchJump: false, attackMomentum: 1 },
];
const impacts = [];
const detachedParts = [];
const bullets = [];
const grenades = [];
// Pickups belong to a surface the player can actually see. When the optional
// ledge is gone they sit just above the floor instead of floating at its old
// invisible height.
const gunPickups = [
  { kind: "HANDGUN", amount: 6, x: 2050, startsActive: true,
    y: PLATFORM ? platformY - 70 : floorY - 35, z: 0 },
  { kind: "RUBBER SMG", amount: 18, x: 850, startsActive: true,
    airborne: true, y: ceilingY + 240, z: 0 },
  { kind: "ROCKET LAUNCHER", amount: 3, x: 4150, startsActive: true,
    airborne: true, y: ceilingY + 240, z: 0 },
];
const grenadePickups = [
  { amount: 2, x: 2950, y: PLATFORM ? platformY - 70 : floorY - 35, z: 0 },
];
// Two trees grow out of the side walls, one per side, and they are the only
// thing in the round that gives a body back. A fighter who has been taken
// apart limb by limb can walk to the wall and be whole again — which is worth
// crossing the stage for, and worth the fifteen seconds it puts back on the
// clock. They take the whole round to ripen, so each one is picked at most
// once and the walk has to be timed.
const bodyTrees = [
  { x: worldLeft + wallThickness, y: floorY - 60, z: 0, growth: 0 },
  { x: worldRight - wallThickness, y: floorY - 60, z: 0, growth: 0 },
];
const treeRipenUs = 18000000;
const treeTimeBonusUs = 15000000;
const airParticles = [];
for (const pickup of [...gunPickups, ...grenadePickups]) {
  pickup.active = Boolean(pickup.startsActive);
  pickup.respawnAt = 0;
  pickup.y = pickup.airborne ? ceilingY + 330
    : PLATFORM ? platformY - 70 : terrainFloorAt(pickup.x) - 35;
}
// Every kind carries a gravity factor so redressing the match ball in place
// can never leave a previous kind's float behind.
const ballKinds = [
  { type: "soccer", spawnOwner: 0, radius: 38, mass: .72, hitScale: 1.12,
    bounce: .58, drag: .994, windFactor: .58, gravityFactor: 1 },
  { type: "basketball", spawnOwner: 1, radius: 42, mass: 1.08, hitScale: .86,
    bounce: .76, drag: .989, windFactor: .34, gravityFactor: 1 },
  { type: "beach", spawnOwner: -1, radius: 46, mass: .34, hitScale: 1.25,
    bounce: .82, drag: .998, windFactor: 1.35, gravityFactor: .62 },
  { type: "skateboard", spawnOwner: -1, radius: 52, mass: 1.4,
    hitScale: .82, bounce: .34, drag: .982, windFactor: .12,
    gravityFactor: 1 },
];
let matchBallType = ballKinds[0].type;
// A match inflates exactly one ball and keeps it for every round.
const balls = [{ ...ballKinds[0], z: 0, vx: 0, vy: 0, rotation: 0,
  x: players[0].spawnX, y: floorY - ballKinds[0].radius,
  active: true, serveAt: 0, lastHitBy: 0, safeUntil: 0, safePlayers: 0,
  heldBy: -1 }];
// Version-one replay/spectator consumers still read the first ball by name.
const ball = balls[0];
let ballEnabled = true;
let padSnapshots = [null, null];
// The pad each fighter actually feels. A bot writes its synthesized presses
// here, so the HUD, the debug read-out, the replay and the physics all read
// bot input off the same wire as a hand on a controller.
let inputPads = [null, null];
let startedAt = 0;
let roundStartedAt = 0;
let lastSimAt = 0;
let roundElapsedUs = 0;
let lastCountdownSecond = -1;
// The intro's own clock: which "3, 2, 1" second last rang, and whether the
// round-open accent has fired. -1 means the accent is still owed.
let lastIntroSecond = 0;
// The scored tail's clocks: the next heartbeat of the killcam dwell, and
// whether the result card has had its sting.
let resultPulseAt = 0;
let resultCardStung = false;
const resultReactionPrevious = [[], []];
let roundOverAt = 0;
let roundResult = "";
let matchOver = false;
let roundCause = "";
let deathCinematic = null;
let impactHitboxesUntil = 0;
let nextPowerupAtUs = powerupIntervalUs;
let powerupSequence = 0;
let acFeed = {};
// Pal select is retired: you are your own handle, so there is nobody to pick,
// and the dummy is already the default opponent. The whole two-step screen
// stays behind this one flag rather than being deleted, because the entry UI
// is still moving and this is the shape we may want to compare against. Flip
// it true and the wheel comes back exactly as it was.
const PAL_SELECT = false;
// The frame recorder remains intact for future review tooling, but replay is
// not part of the match flow. One flag restores the viewer, its controls and
// its offer together; production currently moves directly through results.
const INSTANT_REPLAY = false;
let selecting = false;
// Self-play is a harness mode: both fighters run the bot, no pad can enter or
// leave it, and rounds roll over on their own.
let selfPlay = false;
let hudLeftPad = 0;
// The title is also the attract screen. Half of sessions get the quiet,
// cross-legged tableau; half get a standing face-off. Both remain still so
// the wordmark stays readable until the player asks the fight to begin.
// Hashing the already-created round name keeps the split stable for a visit
// without spending a second Math.random call (reel seeding relies on one).
let titleAttractMode = "still";
let trainingOpponent = "";
function trainingOpponentKind() {
  const requested = String(globalThis.__oskiewarOpponent || "").toLowerCase();
  if (requested === "spiderdummy") return requested;
  if (!trainingOpponent) trainingOpponent = "dummy";
  return trainingOpponent;
}
const selectionReady = [false, false];
const selectionPrevious = [[], []];
let selectionStep = 0;
let selectionCursor = 0;
let windMph = 0;
let windDirection = 1;
let windAcceleration = 0;
let windTargetMph = 0;
let windTargetDirection = 1;
let nextWindChangeAt = 0;
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
let gameplayStarted = false;
let shellPrevious = [];
// Whether the stick was already leaned last frame, so a held lean reads as
// one gesture at the title rather than a machine-gun of entries.
let shellStickLive = false;
// Training teaches for two and a half minutes once the title lifts. This is
// session time, not round time: a knockout or a visit back to the title must
// not restart onboarding that the player has already outgrown.
let dummyGuideStartedAt = null;
// Unset is null, never -1: the console's monotonic clock can read negative
// (App.cpp overflows int64 converting QPC ticks past ~10 days of uptime),
// and a >= 0 sentinel then swallows every legitimate timestamp.
let titleTransitionAt = null;
// The wordmark is a toy: every letter of "oskiewar" and of "start" keeps its
// own swell and shudder, and the prompt keeps one shared bounce the letters
// take in turn. Paint owns these because they are pointer feel, not state.
const titleToys = [];
const promptToys = [];
let promptBounce = 0;
let titleToyAt = -1;
let navigationPrevious = [[], []];
// Temporary live combat inspector. Keep this explicit so the production view
// can return to a clean presentation without changing combat geometry.
let debugHitboxes = false;
let nextInputDebugAt = 0;
let frameTelemetry = [];
let frameTelemetryFlushAt = 0;
let lastPaintAt = 0;
let displayFps = 0;
let liveSequence = 0;
let liveNextAt = 0;
let spectatorQr = null;
// Encoding a code costs about 59ms and is the whole 50-100ms tail in the
// frame histogram: it ran at every round reset, for a URL that changes once
// per round. Same URL, same code -- so keep the last one.
let spectatorQrUrl = "";
let spectatorQrCache = null;
function spectatorCode(url) {
  if (typeof qrcode !== "function") return null;
  if (url === spectatorQrUrl && spectatorQrCache) return spectatorQrCache;
  spectatorQrUrl = url;
  spectatorQrCache = qrcode(url, { errorCorrectLevel: 1 });
  return spectatorQrCache;
}

let roundViewer = null;
let roundViewerStop = null;
let roundViewerMode = "";
let roundViewerStatus = "CONNECTING";
let roundViewerDemo = null;
let roundViewerDemoStartedAt = 0;
let livePublishFailed = false;

// Match names are public URLs and must not collide, so they take real
// entropy — but a recorded round has to reproduce, and the ball kind is
// hashed from the series name and carries radius and mass. So the entropy is
// drawn once per match, carried in the demo, and every name after it falls
// out deterministically.
let nameSeed = 1;
function seedNames(seed) {
  nameSeed = (seed >>> 0) || 1;
}
function nameRandom() {
  nameSeed = (nameSeed + 0x6d2b79f5) >>> 0;
  let value = nameSeed;
  value = Math.imul(value ^ (value >>> 15), value | 1);
  value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
  return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
}

function pronounceableMatchName() {
  const onsets = ["b", "d", "f", "g", "k", "l", "m", "n", "p", "r",
    "s", "t", "v", "z", "ch", "sh", "th"];
  const consonants = "bdfgklmnprstvz";
  const vowels = "aeiou";
  const onset = onsets[Math.floor(nameRandom() * onsets.length)];
  const vowel = vowels[Math.floor(nameRandom() * vowels.length)];
  const middle = consonants[Math.floor(nameRandom() * consonants.length)];
  const ending = (vowels + "y")[Math.floor(nameRandom() * (vowels.length + 1))];
  return onset + vowel + middle + middle + ending +
    Math.floor(nameRandom() * 1000);
}

function demoTick(now) {
  return replay ? Math.max(0, Math.round((now - replay.startedMonotonicUs) /
    replayTickUs)) : 0;
}

function trackMatchStarted() {
  if (!roundIsTimed() || typeof analytics !== "function") return;
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
    opponent_type: selfPlay ? "self-play"
      : players[1].bot ? "bot"
      : players[1].npc ? "dummy" : "local-player",
  });
}

function startReplay(now) {
  const run = runtime();
  if (!roundIsTimed()) {
    seriesName = "";
    matchName = "";
    previousRoundName = "";
    // Spiderdummy training carries exactly one board and no second match ball.
    matchBallType = "skateboard";
    replay = null;
    spectatorQr = null;
    return;
  }
  seedNames(Math.floor(Math.random() * 4294967296));
  const nameSeedUsed = nameSeed;
  seriesName = pronounceableMatchName();
  matchBallType = seriesBallType(seriesName);
  matchName = "";
  previousRoundName = "";
  replay = {
    format: "ac.oskiedemo", version: 1, game: "oskiewar",
    simulation: "oskiewar-physics-1", tickRate: 60,
    matchId: "ow-" + seriesName, matchName: seriesName,
    seriesId: "ow-" + seriesName, seriesName, roundIds: [],
    recordedDisplay: {
      width: Number(run.width) || viewWidth(),
      height: Number(run.height) || viewHeight,
      aspectRatio: (Number(run.width) || viewWidth()) /
        Math.max(1, Number(run.height) || viewHeight),
      surfaceWidth: Number(run.surfaceWidth) || Number(run.width) || viewWidth(),
      surfaceHeight: Number(run.surfaceHeight) || Number(run.height) || viewHeight,
      refreshHz: Number(run.refreshHz) || Number(run.measuredHz) || 0,
    },
    startedAt: run.unixMs || 0, startedMonotonicUs: now,
    nameSeed: nameSeedUsed, ballType: matchBallType,
    fighters: players.map((player) => player.name),
    nations: players.map((player) => player.nation || ""),
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
      name: player.name, nation: player.nation || "", color: player.color,
      x: player.x, y: player.y,
      z: player.z, vx: player.vx, vy: player.vy, vz: player.vz,
      facing: player.facing, alive: player.alive,
      grounded: player.grounded, ducking: player.ducking,
      sinking: now < player.sinkUntil,
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
  if (!roundIsTimed() || !target || livePublishFailed ||
      typeof publishLive !== "function" ||
      (!force && now < liveNextAt)) return;
  liveNextAt = now + liveSnapshotIntervalUs;
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
  if (!INSTANT_REPLAY) return;
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

// A replay is worth watching at the speed the eye needs, not the speed the
// clock kept. Frames already record what a hit looks like, so the playhead can
// read its own footage and slow itself into the action rather than being told
// where the action is — which means every replay already in the store gets
// this without being re-recorded.
function frameAction(frame) {
  let action = 0;
  for (const player of frame.players) {
    if (!player.alive) action = Math.max(action, 1);
    if (player.hit) action = Math.max(action, 1);
    if (player.blockFlash > 0) action = Math.max(action, .7 * player.blockFlash);
    // A swing that has started but not yet landed is the part worth stretching.
    if (player.attackKind && player.attackUntilOffset > 0)
      action = Math.max(action, .35);
  }
  return action;
}

// Look ahead, so the ramp is already down by the time the punch arrives. A
// slowdown that begins on the frame of impact has missed the impact.
function replayActionCurve(frames) {
  const raw = frames.map(frameAction);
  return raw.map((_, index) => {
    let peak = 0;
    for (let at = index; at < Math.min(raw.length, index + replayActionLead); at++)
      peak = Math.max(peak, raw[at] * (1 - (at - index) / replayActionLead * .35));
    return peak;
  });
}

// One step of the speed ramp, kept pure so it can be tested and so the demo
// viewer can share it when its playhead learns to accumulate. Easing rather
// than snapping — and capping how far the speed may travel per second of real
// time — is what keeps a dense run of hits reading as one long stretch rather
// than a stutter.
function replayRampStep(speed, action, elapsedSeconds) {
  const target = lerp(1, replaySlowest, clamp(action, 0, 1));
  return speed + (target - speed) *
    Math.min(1, Math.max(0, elapsedSeconds) * replayRampPerSecond);
}

function startInstantReplay(now) {
  if (!INSTANT_REPLAY) return false;
  if (roundReplayFrames.length < 2) return false;
  const frames = roundReplayFrames.slice();
  instantReplay = { frames, cursor: 0, lastAt: now, paused: false,
    previous: padSnapshots[0]?.down?.slice() || [],
    action: replayActionCurve(frames), speed: 1,
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
  const elapsed = Math.max(0, now - instantReplay.lastAt) / 1000000;
  instantReplay.speed = replayRampStep(instantReplay.speed,
    instantReplay.action?.[Math.floor(instantReplay.cursor)] || 0, elapsed);
  if (!instantReplay.paused)
    instantReplay.cursor += (now - instantReplay.lastAt) /
      instantReplayStepUs * instantReplay.speed;
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
    const upload = saveReplay(payload);
    // Only promise-returning hosts can prove the upload completed. The web
    // host does; older native hosts keep saving silently until they adopt the
    // same acknowledgement contract.
    if (upload && typeof upload.then === "function")
      upload.then((saved) => {
        if (saved !== true) return;
        playDrum("modem", .72, 0);
        telemetry("REPLAY", "uploaded " + demo.roundId);
      }).catch((error) => telemetry("REPLAY", "upload-error " + error.message));
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

function playSine(frequency, duration = .12) {
  if (typeof synth !== "function") return;
  try { synth(frequency, duration); } catch (_) {}
}

let clientError = "";
let clientErrorDetail = null;
let clientErrorAt = -1;
let clientErrorQr = null;
let clientErrorDumpUrl = "";

function clientStateDump() {
  try {
    const run = runtime();
    return {
      build: "v" + buildVersion,
      shell: shellMode,
      round: {
        id: matchName || "local",
        result: roundResult || "active",
        elapsedMs: Math.round(roundElapsedUs / 1000),
      },
      runtime: {
        sim: Number(run?.simCount || 0),
        paint: Number(run?.paintCount || 0),
        unixMs: Number(run?.unixMs || 0),
      },
      camera: {
        x: Math.round(cameraCenter), y: Math.round(cameraCenterY),
        width: Math.round(cameraWidth), aspect: Math.round(cameraAspect * 1000) / 1000,
      },
      players: players.map((player) => ({
        handle: player.name, stance: player.stance, alive: player.alive,
        x: Math.round(player.x), y: Math.round(player.y), z: Math.round(player.z),
        vx: Math.round(player.vx), vy: Math.round(player.vy),
        input: [player.inputX, player.inputY],
        removed: player.removedParts?.slice() || [],
      })),
      balls: balls.filter((item) => item.active).map((item) => ({
        type: item.type, x: Math.round(item.x), y: Math.round(item.y),
        vx: Math.round(item.vx), vy: Math.round(item.vy), heldBy: item.heldBy,
      })),
    };
  } catch (error) {
    return { dumpError: String(error?.message || error || "state unavailable") };
  }
}

function errorNowUs() {
  try {
    const value = Number(runtime()?.monotonicUs);
    if (Number.isFinite(value)) return value;
  } catch (_) {}
  return 0;
}

// btoa only speaks latin-1, so the dump flattens to ASCII before encoding.
function base64Url(text) {
  if (typeof btoa !== "function") return "";
  return btoa(String(text).replace(/[^\x20-\x7e]/g, "?"))
    .replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

// The QR carries the entire dump in its own URL, so a crash stays shareable
// even when the console never managed to post its report. Fat fields drop
// away until the payload fits a code that is still scannable off a TV.
function buildClientErrorDump(detail) {
  clientErrorQr = null;
  clientErrorDumpUrl = "";
  const stack = String(detail.stack || "").split(/\r?\n/).slice(0, 4)
    .map((line) => line.trim()).filter(Boolean).join(" | ").slice(0, 240);
  for (const trace of [stack, ""]) {
    const url = errorDumpBase + base64Url(JSON.stringify({
      v: 1, p: detail.phase, n: detail.name, m: detail.message,
      src: detail.source, k: trace || undefined, s: detail.state,
    }));
    if (url.length <= 2048 && typeof qrcode === "function") {
      try {
        clientErrorQr = qrcode(url, { errorCorrectLevel: 1 });
        clientErrorDumpUrl = url;
        return;
      } catch (_) {}
    }
  }
}

function errorRestartSeconds() {
  if (clientErrorAt < 0) return 0;
  return Math.max(0,
    Math.ceil((errorRestartUs - (errorNowUs() - clientErrorAt)) / 1000000));
}

// A crashed console should not sit on a dead screen forever. The error holds
// long enough to read and scan, then the piece boots itself again.
function restartAfterClientError() {
  if (clientErrorAt < 0 || errorNowUs() - clientErrorAt < errorRestartUs) return;
  clientError = "";
  clientErrorDetail = null;
  clientErrorAt = -1;
  clientErrorQr = null;
  clientErrorDumpUrl = "";
  impacts.length = 0;
  detachedParts.length = 0;
  bullets.length = 0;
  grenades.length = 0;
  try { telemetry("SHELL", "error->restart"); } catch (_) {}
  boot();
}

function errorSource(stack) {
  const match = String(stack || "").match(
    /(?:\(|\s|^)((?:[a-z]+:\/\/)?[^\s()]+):(\d+):(\d+)\)?/i);
  return match ? { file: match[1], line: Number(match[2]), column: Number(match[3]) }
    : null;
}

function captureClientError(phase, error) {
  if (clientErrorDetail) return;
  const stack = error?.stack ? String(error.stack) : "";
  const message = error?.message ? String(error.message)
    : String(error || "unknown error");
  const name = String(error?.name || "Error");
  const state = clientStateDump();
  clientErrorDetail = {
    phase: String(phase || "runtime"), name, message, stack,
    source: errorSource(stack), state,
    reportStatus: "queued for server",
  };
  const detail = stack || (name + ": " + message);
  clientError = (clientErrorDetail.phase + ": " + detail)
    .replace(/[^\x20-\x7e]+/g, " ").replace(/\s+/g, " ").trim();
  clientErrorAt = errorNowUs();
  try { buildClientErrorDump(clientErrorDetail); } catch (_) {
    clientErrorQr = null;
    clientErrorDumpUrl = "";
  }
  try {
    telemetry("CLIENT_ERROR", JSON.stringify({
      phase: clientErrorDetail.phase, name, message, stack,
      source: clientErrorDetail.source, state,
    }));
  } catch (_) {
    clientErrorDetail.reportStatus = "local report only";
  }
}

function clientErrorLines(text, limit = 58) {
  const words = String(text).split(" ");
  const lines = [];
  let line = "";
  for (let word of words) {
    if (word.length > limit) {
      if (line) { lines.push(line); line = ""; }
      while (word.length > limit) {
        lines.push(word.slice(0, limit));
        word = word.slice(limit);
      }
      if (!word) continue;
    }
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

// Diagnostic lines are dense, so every token gets its own ink: handles read as
// people, numbers as measurements, units as scale, and the rest as labels.
const dumpUnits = new Set(["px", "ms", "us", "fps", "vx", "vy", "vz", "in",
  "stk", "pos", "vel", "t", "pts"]);

function dumpTokens(text) {
  return String(text).match(/@[a-z0-9_]+|[-+]?\d*\.?\d+|[a-z]+|\s+|./gi) || [];
}

function dumpTokenInk(token) {
  if (token[0] === "@") return [255, 126, 205];
  if (/^[-+]?[\d.]+$/.test(token)) return [116, 255, 184];
  if (/^[a-z]+$/i.test(token)) return dumpUnits.has(token.toLowerCase())
    ? [255, 205, 74] : [188, 204, 230];
  return [112, 234, 255];
}

function writeTokens(write, text, x, y, size) {
  let cursor = x;
  for (const token of dumpTokens(text)) {
    if (token.trim()) write(token, cursor, y, size, ...dumpTokenInk(token));
    cursor += handleWidth(token, size);
  }
}

const writeDumpLine = (text, x, y, size) =>
  writeTokens(errorTypeWrite, text, x, y, size);

// The HUD sits over sky and ground, so each token carries its own contrast
// shadow the way a handle glyph does.
function writeHudLine(text, x, y, size) {
  writeTokens((token, tx, ty, ts, ...color) =>
    typeWrite(token, tx + 3, ty + 4, ts, ...runShadow(color)),
    text, x, y, size);
  writeTokens(typeWrite, text, x, y, size);
}

function errorReportStatus() {
  try {
    const status = String(runtime()?.clientErrorReportStatus || "").trim();
    if (status) return status;
  } catch (_) {}
  return clientErrorDetail?.reportStatus || "queued for server";
}

function stateDumpRows(state) {
  if (!state) return [{ icon: "mode", text: "state unavailable" }];
  if (state.dumpError)
    return [{ icon: "mode", text: "state unavailable: " + state.dumpError }];
  const playerRow = (player, index) => ({ icon: "player",
    text: "p" + (index + 1) + " " + player.handle + " " +
      player.stance.toLowerCase() + " pos " + player.x + "," + player.y +
      "," + player.z + " vel " + player.vx + "," + player.vy +
      (player.removed?.length ? " lost " + player.removed.join(",") : "") });
  return [
    { icon: "build", text: "build " + state.build },
    { icon: "mode", text: "mode " + state.shell.toLowerCase() +
      "  round " + state.round.id + "  " + state.round.result.toLowerCase() +
      "  " + state.round.elapsedMs + "ms" },
    { icon: "camera", text: "camera " + state.camera.x + "," + state.camera.y +
      "  width " + state.camera.width + "  aspect " + state.camera.aspect },
    ...(state.players || []).map(playerRow),
    { icon: "ball", text: "balls " + (state.balls || []).map((ball) =>
      ball.type + "@" + ball.x + "," + ball.y + " v" + ball.vx + "," + ball.vy)
      .join("  ") },
  ];
}

const dumpIconInk = { build: [116, 128, 156], mode: [255, 205, 74],
  camera: [112, 214, 255], player: [206, 166, 255], ball: [116, 255, 184] };

function drawDumpIcon(kind, x, y, size) {
  const ink = dumpIconInk[kind] || [112, 234, 255];
  const unit = Math.max(2, Math.round(size / 9));
  try {
    if (kind === "build")
      for (let row = 0; row < 3; row++)
        box(x, y + row * unit * 3, size - row * unit * 2, unit * 2, ...ink);
    else if (kind === "camera") {
      strokeBox(x, y + unit, size, size - unit * 2, unit, ink);
      box(x + unit * 3, y + unit * 3, size - unit * 6, size - unit * 6, ...ink);
    } else if (kind === "player") {
      box(x + unit * 2, y, size - unit * 4, unit * 3, ...ink);
      box(x, y + unit * 4, size, size - unit * 4, ...ink);
    } else if (kind === "ball") {
      box(x + unit * 2, y, size - unit * 4, size, ...ink);
      box(x, y + unit * 2, size, size - unit * 4, ...ink);
    } else strokeBox(x, y, size, size, unit, ink);
  } catch (_) {}
}

// Sized for a phone pointed at a television, so modules stay chunky and the
// state dump wraps narrower to keep clear of the code.
function errorQrGeometry(width, height) {
  if (!clientErrorQr || typeof clientErrorQr.getModuleCount !== "function")
    return null;
  const count = clientErrorQr.getModuleCount();
  const quiet = 2;
  const cell = Math.max(2,
    Math.floor(Math.min(width * .3, height * .52) / (count + quiet * 2)));
  const size = (count + quiet * 2) * cell;
  return { count, quiet, cell, size,
    left: width - 92 - size, top: height - 96 - size };
}

// Drawn with raw boxes rather than the projected screenRect the HUD uses —
// the triangle pipeline is exactly what may have just died. Dark modules
// coalesce into horizontal runs so a full code stays a few hundred draws.
function drawErrorQr(qr) {
  if (!qr) return;
  box(qr.left, qr.top, qr.size, qr.size, 250, 250, 247);
  for (let row = 0; row < qr.count; row++) {
    let run = 0;
    for (let column = 0; column <= qr.count; column++) {
      if (column < qr.count && clientErrorQr.isDark(row, column)) { run++; continue; }
      if (run) box(qr.left + (column - run + qr.quiet) * qr.cell,
        qr.top + (row + qr.quiet) * qr.cell, run * qr.cell, qr.cell, 7, 8, 14);
      run = 0;
    }
  }
}

// Same two-digit face as the round timer, reddening as the restart lands.
function drawErrorCountdown(width) {
  const remaining = errorRestartSeconds();
  const text = String(Math.min(99, remaining)).padStart(2, "0");
  const danger = remaining <= 10;
  const shake = danger
    ? Math.sin(errorNowUs() / 1000000 * 35) * (11 - remaining) * .45 : 0;
  let textWidth = 64;
  try { textWidth = handleWidth(text, 52); } catch (_) {}
  errorTypeWrite(text, width - 92 - textWidth + shake, 82, 52,
    ...(danger ? [255, 92, 116] : [255, 205, 74]));
}

function drawClientError() {
  let width = 1920;
  let height = 1080;
  try {
    const view = typeof gameView === "function" ? gameView() : null;
    if (view && Number.isFinite(view.width)) width = view.width;
    if (view && Number.isFinite(view.height)) height = view.height;
  } catch (_) {}
  const detail = clientErrorDetail || {
    phase: "runtime", name: "Error", message: clientError,
    stack: "", source: null, state: null,
  };
  const qr = errorQrGeometry(width, height);
  wipe(7, 9, 18);
  box(48, 48, width - 96, height - 96, 30, 14, 27);
  errorTypeWrite("the game needs a moment", 92, 82, 52, 255, 92, 116);
  drawErrorCountdown(width);
  errorTypeWrite("restarting safely", 94, 158, 31, 255, 205, 74);
  const messageLines = clientErrorLines(detail.message, 66).slice(0, 2);
  for (let index = 0; index < messageLines.length; index++)
    errorTypeWrite(messageLines[index], 94, 226 + index * 42,
      31, 248, 244, 255);
  let cursorY = 226 + messageLines.length * 42 + 28;
  if (detail.source) {
    errorTypeWrite("source", 94, cursorY, 29, 112, 234, 255);
    errorTypeWrite(detail.source.file, 214, cursorY, 29, 190, 216, 255);
    errorTypeWrite("line " + detail.source.line + "  column " + detail.source.column,
      Math.min(width - 430, 214 + detail.source.file.length * 17), cursorY,
      29, 116, 255, 184);
    cursorY += 48;
  }
  errorTypeWrite("state dump", 94, cursorY, 29, 116, 255, 184);
  cursorY += 42;
  for (const row of stateDumpRows(detail.state).slice(0, 6)) {
    let leading = true;
    for (const wrapped of clientErrorLines(row.text, qr ? 68 : 78).slice(0, 2)) {
      if (cursorY > height - 196) break;
      if (leading) drawDumpIcon(row.icon, 94, cursorY + 4, 22);
      writeDumpLine(wrapped, 132, cursorY, 27);
      cursorY += 36;
      leading = false;
    }
  }
  drawErrorQr(qr);
  if (qr) errorTypeWrite("scan to share this dump",
    qr.left, qr.top - 42, 27, 116, 255, 184);
  const report = errorReportStatus();
  const posted = report.startsWith("posted");
  errorTypeWrite(report, 94, height - 150, 30,
    ...(posted ? [116, 255, 184] : [255, 205, 74]));
  errorTypeWrite("relaunch or deploy an update", 94, height - 104,
    30, 190, 202, 230);
}

function drawClientErrorFallback() {
  try { wipe(7, 9, 18); } catch (_) {}
  try { systemWrite("aesthetic.computer error", 72, 72, 48, 255, 92, 116); } catch (_) {}
  try { systemWrite(errorReportStatus(), 72, 146, 30, 255, 205, 74); } catch (_) {}
  try {
    systemWrite("restart in " + errorRestartSeconds() + "s", 72, 200, 30,
      255, 205, 74);
  } catch (_) {}
}

function fighterProfile(handle) {
  const live = Array.isArray(acFeed.fighters)
    ? acFeed.fighters.find((profile) => profile.handle.toUpperCase() === handle.toUpperCase())
    : null;
  const fallback = handle === "DUMMY" ? npcFighter
    : handle === "SPIDERDUMMY" ? spiderDummyFighter
    : handle === "BOT" ? botFighter
    : selfPlayFighters.find((profile) => profile.handle === handle) ||
      fighterRoster.find((profile) => profile.handle === handle);
  return {
    mood: live?.mood || (handle === "@JEFFREY" && acFeed.moodHandle === "@jeffrey"
      ? acFeed.mood : "") || fallback?.mood || "",
    lastChat: live?.lastChat || fallback?.lastChat || "",
    colors: live?.colors?.length
      ? live.colors.map((color) => [color.r, color.g, color.b])
      : fallback?.colors || [],
  };
}

function syncSignedInFighter() {
  const identity = acFeed?.player;
  if (!identity?.handle || players[0].npc) return;
  const handle = String(identity.handle).toUpperCase();
  if (players[0].name !== handle) players[0].name = handle;
  const colors = Array.isArray(identity.colors)
    ? identity.colors.map((color) => Array.isArray(color)
      ? color.slice(0, 3) : [color.r, color.g, color.b])
      .filter((color) => color.every(Number.isFinite)) : [];
  if (colors.length) {
    players[0].handleColors = colors;
    players[0].color = colors.reduce((sum, color) => sum.map((value, index) =>
      value + color[index] / colors.length), [0, 0, 0]).map(Math.round);
  }
}

function applyRoster(player, index) {
  if (player.npc) {
    const fighter = selfPlay ? selfPlayFighters[player.pad]
      : player.bot ? botFighter
      : player.spiderDummy ? spiderDummyFighter : npcFighter;
    player.rosterIndex = -1;
    player.name = fighter.handle;
    player.color = fighter.color.slice();
    player.handleColors = fighter.colors;
    return;
  }
  if (player.pad === 0 && !acFeed?.player?.handle) {
    player.rosterIndex = -1;
    player.name = anonymousFighter.handle;
    player.color = anonymousFighter.color.slice();
    player.handleColors = anonymousFighter.colors;
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

// The one door into a fight, and the seam auth will attach to. Dummy is free
// and anonymous — it is the front door and must never ask for anything. Bot
// and ppl are what a handle buys, because they persist: a series, a published
// match, a replay, a ranking. Sign-in belongs on this call, not in front of
// the game. Nothing opens those doors yet, so only training reaches here.
function startFightAgainst(kind, now) {
  const opponent = players[1];
  opponent.npc = kind === "dummy" || kind === "spiderdummy" || kind === "bot";
  opponent.bot = kind === "bot";
  opponent.spiderDummy = kind === "spiderdummy";
  // Pad two is @OSKIE until ppl arrives carrying a handle of its own.
  applyRoster(opponent, opponent.npc ? -1 : 2);
  selecting = false;
  shellMode = "GAME";
  gameplayStarted = true;
  titleTransitionAt = null;
  startReplay(now);
  resetRound(now, true);
  emitSignal("fighters", -1, players[0].rosterIndex, players[1].rosterIndex);
}

// Whatever you land on is already a live training fight; the wordmark simply
// floats over it. The intro countdown is spent before the first frame so
// somebody arriving from a QR code is moving, not watching a number.
function beginTraining(now) {
  startFightAgainst(trainingOpponentKind(), now);
  shellMode = "MENU";
  gameplayStarted = false;
  roundStartedAt = now - introDurationUs;
  const forcedAttract = globalThis.__oskiewarAttractVariant;
  titleAttractMode = forcedAttract === "still" || forcedAttract === "action"
    ? forcedAttract : hashUnit(matchName) < .5 ? "still" : "action";
  if (titleAttractMode === "action") {
    // The title demonstration is a bot working a training dummy, not two
    // anonymous red bots. Preserve the dummy's cool neutral material so the
    // matchup is legible before START is pressed.
    players[0].npc = true;
    players[0].bot = true;
    players[0].spiderDummy = false;
    applyRoster(players[0], -1);
    players[1].npc = true;
    players[1].bot = false;
    players[1].spiderDummy = false;
    applyRoster(players[1], -1);
  }
}

// Deprecated with PAL_SELECT — see the flag.
function beginSelect(now) {
  selecting = true;
  selectionStep = 0;
  selectionCursor = Math.max(0, players[0].rosterIndex);
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
  lastIntroSecond = 0;
  resultPulseAt = 0;
  resultCardStung = false;
  roundStartedAt = now;
  for (const player of players) {
    player.roundWins = 0;
    player.score = 0;
    player.alive = true;
  }
}

function returnToTitle(now, reason = "back") {
  finishReplay();
  beginTraining(now);
  shellPrevious = padSnapshots[0]?.down?.slice() || [];
  selectionPrevious[0] = shellPrevious.slice();
  selectionPrevious[1] = padSnapshots[1]?.down?.slice() || [];
  spectatorQr = typeof qrcode === "function"
    ? spectatorCode("https://oskiewar.com") : null;
  playDrum("block", .8, 0);
  telemetry("SHELL", "game->title " + reason + " " + now);
}

// A mechanical test drives both fighters with the same bot and says so on the
// nameplates, instead of flying a handle nobody is holding. Entered through
// `__oskiewarSelfPlay` before boot or by calling this — never from a button,
// so normal play cannot fall into it.
function startSelfPlay(now) {
  selfPlay = true;
  shellMode = "GAME";
  gameplayStarted = true;
  selecting = false;
  titleTransitionAt = null;
  for (const player of players) {
    player.npc = true;
    player.bot = true;
    player.spiderDummy = false;
    player.rosterIndex = -1;
  }
  startReplay(now);
  resetRound(now, true);
}

function consumeSystemButtons(now) {
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
  // Self-play has no title to return to — a stray Menu press would strand the
  // loop on a screen nobody is there to start.
  if (!pressed || selfPlay || shellMode !== "GAME" || selecting) return false;
  returnToTitle(now, "menu");
  return true;
}

// Start lifts the wordmark off a fight that is already running underneath.
function enterGame(now) {
  if (titleAttractMode === "action") {
    selfPlay = false;
    players[0].npc = false;
    players[0].bot = false;
    players[0].spiderDummy = false;
    applyRoster(players[0], Math.max(0, players[0].rosterIndex));
    // Attract mode may borrow two bots to demonstrate motion, but Start always
    // opens the anonymous front door: one player versus an inert dummy.
    players[1].npc = true;
    players[1].bot = false;
    players[1].spiderDummy = false;
    applyRoster(players[1], -1);
  }
  shellMode = "GAME";
  gameplayStarted = true;
  if (dummyGuideStartedAt === null && players[1].npc && !players[1].bot)
    dummyGuideStartedAt = now;
  shellPrevious = padSnapshots[0]?.down?.slice() || [];
  if (PAL_SELECT) beginSelect(now);
}

function updateShell(now) {
  const pad = padSnapshots[0] || {};
  // View and Menu are the system's buttons — View toggles the debug HUD and
  // must not double as "start the game". Everything else, and a real lean on
  // the analog stick, says the player wants in.
  const down = (pad.down || [])
    .filter((button) => button !== "View" && button !== "Menu");
  const stickLive = Math.abs(pad.leftX || 0) > .5 || Math.abs(pad.leftY || 0) > .5;
  if (titleTransitionAt !== null) {
    if (now - titleTransitionAt >= 700000) {
      titleTransitionAt = null;
      enterGame(now);
    }
    shellPrevious = down.slice();
    shellStickLive = stickLive;
    return;
  }
  if (down.some((button) => !shellPrevious.includes(button)) ||
      (stickLive && !shellStickLive)) {
    playDrum("hat", .55, 0);
    if (typeof titleBeep === "function") titleBeep();
    if (typeof titleVoice === "function") titleVoice();
    emitSignal("select", -1, 1, 0);
    titleTransitionAt = now;
  }
  shellPrevious = down.slice();
  shellStickLive = stickLive;
}

// Everything from here through `updateSelect` is the deprecated pal select,
// held behind PAL_SELECT — see the flag.
function selectionOptions() {
  if (selectionStep === 0) return fighterRoster.map((fighter, rosterIndex) => ({
    kind: "pal", fighter, rosterIndex,
  }));
  return [
    { kind: "bot", fighter: botFighter, rosterIndex: -1 },
    { kind: "dummy", fighter: npcFighter, rosterIndex: -1 },
    { kind: "spiderdummy", fighter: spiderDummyFighter, rosterIndex: -1 },
    { kind: "people", fighter: peopleFighter, rosterIndex: -1, disabled: true },
  ];
}

function startSelectedRound(now) {
  selectionReady[0] = true;
  selectionReady[1] = true;
  startFightAgainst(
    players[1].bot ? "bot" : players[1].spiderDummy ? "spiderdummy"
    : players[1].npc ? "dummy" : "ppl", now);
}

function chooseSelection(index, now) {
  const option = selectionOptions()[index];
  if (!option) return;
  if (option.disabled) {
    playDrum("block", .45, 0);
    return;
  }
  if (selectionStep === 0) {
    applyRoster(players[0], option.rosterIndex);
    selectionReady[0] = true;
    selectionStep = 1;
    // The dummy is where a fight should start: it stands still and lets you
    // learn the buttons. Picking the bot is a decision, not a default.
    selectionCursor = Math.max(0,
      selectionOptions().findIndex((option) => option.kind === "dummy"));
    playDrum("hat", .9, -.55);
    emitSignal("select", 0, option.rosterIndex, 0);
    return;
  }
  const opponent = players[1];
  opponent.npc = option.kind === "dummy" || option.kind === "spiderdummy" ||
    option.kind === "bot";
  opponent.bot = option.kind === "bot";
  opponent.spiderDummy = option.kind === "spiderdummy";
  applyRoster(opponent, option.rosterIndex);
  playDrum("clap", 1, .55);
  emitSignal("select", 1, option.rosterIndex, opponent.bot ? 2 : opponent.npc ? 1 : 0);
  startSelectedRound(now);
}

function selectionBack(now) {
  if (selectionStep === 1) {
    selectionStep = 0;
    selectionCursor = Math.max(0, players[0].rosterIndex);
    selectionReady[0] = false;
    playDrum("block", .7, 0);
    return;
  }
  returnToTitle(now, "back");
}

// One pal at a time: the cursor holds the middle of the screen and its two
// neighbours recede to either side, so the roster reads as a wheel you turn
// rather than a board you scan.
function selectionTouchLayout() {
  const count = selectionOptions().length;
  const compact = compactLayout();
  const centerX = viewCenterX();
  const cardWidth = compact ? Math.min(280, viewWidth() * .44) : 680;
  const cardHeight = compact ? 360 : 720;
  const top = compact ? 176 : 190;
  const sideScale = compact ? .5 : .66;
  const reach = cardWidth * (1 + sideScale) / 2 + (compact ? 6 : 26);
  const slots = count > 2 ? [-1, 0, 1] : count > 1 ? [0, 1] : [0];
  return {
    back: compact
      ? { x: 24, y: 126, width: 112, height: 50 }
      : { x: viewOffsetX() + 90, y: 112, width: 170, height: 70 },
    options: slots.map((slot) => {
      const scale = slot === 0 ? 1 : sideScale;
      const width = cardWidth * scale;
      const height = cardHeight * scale;
      return { index: (selectionCursor + slot + count) % count, slot,
        x: centerX + slot * reach - width / 2,
        y: top + (cardHeight - height) / 2, width, height };
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
  if (pointInRect(pointer, layout.back)) return { back: true };
  const option = layout.options.find((rect) => pointInRect(pointer, rect));
  return option && !selectionOptions()[option.index]?.disabled
    ? { option: option.index } : null;
}

function consumeSelectTouches(now) {
  const queue = globalThis.__oskiewarTouch?.taps;
  if (!Array.isArray(queue) || !queue.length) return;
  const touches = queue.splice(0);
  const layout = selectionTouchLayout();
  const options = selectionOptions();
  for (const point of touches) {
    if (pointInRect(point, layout.back)) selectionBack(now);
    else {
      const rect = layout.options.find((option) => pointInRect(point, option));
      // A receding neighbour turns the wheel; the focused card commits. A
      // disabled card duds either way.
      if (rect && (rect.slot === 0 || options[rect.index]?.disabled))
        chooseSelection(rect.index, now);
      else if (rect) {
        selectionCursor = rect.index;
        playDrum("hat", .55, 0);
      }
    }
    if (!selecting || shellMode === "MENU") break;
  }
}

function updateSelect(now) {
  consumeSelectTouches(now);
  if (!selecting || shellMode === "MENU") return;
  const down = padSnapshots[0]?.down || [];
  const previous = selectionPrevious[0];
  const pressed = (button) => down.includes(button) && !previous.includes(button);
  const optionCount = selectionOptions().length;
  let movement = 0;
  if (pressed("ArrowLeft") || pressed("ArrowUp")) movement = -1;
  else if (pressed("ArrowRight") || pressed("ArrowDown")) movement = 1;
  if (movement) {
    let next = selectionCursor;
    for (let index = 0; index < optionCount; index++) {
      next = (next + movement + optionCount) % optionCount;
      if (!selectionOptions()[next]?.disabled) break;
    }
    if (next !== selectionCursor) {
      selectionCursor = next;
      playDrum("hat", .55, 0);
    }
  }
  if (pressed("B") || pressed("Menu")) selectionBack(now);
  else if (pressed("A")) chooseSelection(selectionCursor, now);
  selectionPrevious[0] = down.slice();
  selectionPrevious[1] = padSnapshots[1]?.down?.slice() || [];
}

function randomWindMph() {
  return 0;
}

function rollWind(now = runtime().monotonicUs, immediate = true) {
  windTargetMph = 0;
  windTargetDirection = 1;
  nextWindChangeAt = Infinity;
  windMph = 0;
  windDirection = 1;
  windAcceleration = 0;
  emitSignal("wind", -1, windTargetDirection, windTargetMph);
}

function updateWind(dt, now) {
  windMph = 0;
  windDirection = 1;
  windAcceleration = 0;
}

// The match ball is drawn from the series identity. That identity is itself
// seeded once per match and recorded, so re-running a demo's inputs inflates
// the same ball -- the name this hashes used to come straight from
// Math.random, which silently made ball radius and mass unreproducible.
function seriesBallType(name) {
  const roll = hashUnit("ball:" + (name || "oskiewar"));
  return ballKinds[Math.min(ballKinds.length - 1,
    Math.floor(roll * ballKinds.length))].type;
}

function resetBalls(now) {
  Object.assign(balls[0],
    ballKinds.find((kind) => kind.type === matchBallType) || ballKinds[0]);
  for (const item of balls) {
    const owner = item.spawnOwner >= 0 ? players[item.spawnOwner] : null;
    item.x = owner ? owner.spawnX + owner.facing * 180 : platformRight + 820;
    item.y = owner ? terrainFloorAt(item.x) - item.radius
      : terrainFloorAt(item.x) - 920;
    // The beach ball begins airborne beyond the platform edge, so its first
    // landing is in the open arena rather than on the center platform.
    item.z = owner ? owner.z : 0;
    item.vx = 0;
    item.vy = 0;
    item.rotation = 0;
    item.heldBy = -1;
    item.active = ballEnabled;
    item.serveAt = now + introDurationUs + 150000;
    item.lastHitBy = owner ? owner.pad : -1;
    item.safeUntil = item.serveAt;
    item.safePlayers = owner ? 1 << owner.pad : 0;
    emitSignal("ballserve", owner ? owner.pad : -1,
      owner ? owner.facing : 0, windMph);
  }
}

const buttonLabel = (button) => ({
  ArrowUp: "UP", ArrowDown: "DOWN", ArrowLeft: "LEFT", ArrowRight: "RIGHT",
  LeftShoulder: "LB", RightShoulder: "RB", LeftStick: "LEFT STICK",
  RightStick: "RIGHT STICK", View: "VIEW", Menu: "MENU",
  LeftTrigger: "LT", RightTrigger: "RT",
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
      if (meleeSpecs[event[1].toUpperCase()]) kind = event[1].toUpperCase();
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
    return { name, nation: demo.nations?.[pad] || "",
      color: profile?.color || players[pad].color,
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
    for (const key of ["name", "nation", "color", "x", "y", "z", "facing", "alive",
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

function gameBoot() {
  syncGameView();
  startedAt = runtime().monotonicUs;
  roundStartedAt = startedAt;
  lastSimAt = startedAt;
  roundElapsedUs = 0;
  lastCountdownSecond = -1;
  lastIntroSecond = 0;
  resultPulseAt = 0;
  resultCardStung = false;
  emitSignal("hello", -1, 1, 0);
  shellMode = "MENU";
  titleTransitionAt = null;
  spectatorQr = typeof qrcode === "function"
    ? spectatorCode("https://oskiewar.com") : null;
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
      ? spectatorCode("https://oskiewar.com/" + matchName) : spectatorQr;
    roundStartedAt = startedAt - introDurationUs;
    roundViewerStop = roundViewer.start(handleRoundViewer);
    return;
  }
  if (globalThis.__oskiewarSelfPlay) {
    startSelfPlay(startedAt);
    return;
  }
  beginTraining(startedAt);
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
      ? spectatorCode("https://oskiewar.com/" + matchName) : null;
  }
  // Terrain belongs to the simulation contract, not the spectator URL. A
  // series keeps one landscape across rounds and identical training sims get
  // identical ground even when their public room names differ.
  terrainPhase = terrainSeed("oskiewar-physics-1-hills");
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
    player.y = terrainFloorAt(player.spawnX);
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
    player.dashVx = 0;
    player.runSince = 0;
    player.walkSince = 0;
    player.attackKind = "";
    player.attackUntil = 0;
    player.attackHit = false;
    player.blocking = false;
    player.blockFlash = 0;
    player.shieldLocked = false;
    player.shieldCrouched = false;
    player.shieldAimX = 0;
    player.shieldAimY = 0;
    player.shieldBrokenAt = 0;
    player.windVx = 0;
    player.knockVx = 0;
    player.shieldVx = 0;
    player.gunAmmo = 0;
    player.grenadeAmmo = 0;
    player.gunAimX = player.facing;
    player.gunAimY = 0;
    player.gunMode = "HANDGUN";
    player.itemArm = "";
    player.nextGunShotAt = 0;
    player.nextSpitAt = 0;
    player.resultReaction = "";
    player.resultReactionAt = 0;
    resultReactionPrevious[player.pad] = [];
    player.stance = "NEUTRAL";
    player.itemAction = "";
    player.itemActionStartedAt = 0;
    player.itemActionUntil = 0;
    player.heldBall = -1;
    player.heldPart = -1;
    player.heldPlayer = -1;
    player.carryArm = "";
    player.grabbedBy = -1;
    player.skateboard = false;
    player.skateVx = 0;
    player.grabHeld = false;
    player.commandStream = [];
    player.hitSegment = -1;
    player.hitSegmentUntil = 0;
    player.hitStunUntil = 0;
    player.partDamage = {};
    player.removedParts = [];
    player.fallenBodyGeometry = null;
    player.pogoHit = false;
    player.pogoDive = false;
    player.pounding = false;
    player.poundFrom = 0;
    player.poundLevel = 0;
    player.standingOn = -1;
    player.previousY = player.y;
    player.crouchBlend = 0;
    player.jumpLaunchAt = 0;
    player.jumpPoseUntil = 0;
    player.landPoseUntil = 0;
    player.jumpHeld = false;
    player.airJumpsUsed = 0;
    player.doubleJumpLinesUntil = 0;
    player.hopUntil = 0;
    player.sinkUntil = 0;
    player.crouchJump = false;
    player.attackMomentum = 1;
    player.botPresses = {};
    player.botAttackAt = now + 420000;
    player.botItemAt = now + 600000;
    player.botSinkAt = now + 900000;
    player.botSinkTaps = 0;
    player.botSinkNextAt = 0;
    player.botAttackSequence = 0;
    player.botJumpAt = now + 900000;
    player.botThreatSeen = 0;
    player.botShieldAt = Infinity;
    player.botPunishedAt = 0;
    player.botRngState = (Math.round(now / 1000) ^
      Math.imul(player.pad + 1, 0x9e3779b9)) >>> 0;
    delete player.frozenGeometry;
    delete player.frozenAt;
    delete player.headBustedAt;
    // Only a hand can still be leaning on a button across the reset; a bot's
    // presses were just cleared, so inheriting pad two would suppress them.
    player.previous = player.npc || player.bot ? []
      : padSnapshots[player.pad]?.down?.slice() || [];
    player.suppressedDirections = player.previous.filter((button) =>
      button.startsWith("Arrow"));
    player.lastButton = "NONE";
    player.lastButtonAt = 0;
  }
  for (const pickup of [...gunPickups, ...grenadePickups]) {
    pickup.active = Boolean(pickup.startsActive);
    pickup.respawnAt = 0;
  }
  for (const tree of bodyTrees) tree.growth = 0;
  for (const tree of bodyTrees) tree.y = terrainFloorAt(tree.x) - 60;
  nextPowerupAtUs = powerupIntervalUs;
  powerupSequence = 0;
  roundResult = "";
  roundCause = "";
  deathCinematic = null;
  matchOver = false;
  roundElapsedUs = 0;
  lastCountdownSecond = -1;
  lastIntroSecond = 0;
  resultPulseAt = 0;
  resultCardStung = false;
  lastSimAt = now;
  roundStartedAt = now;
  rollWind(now);
  resetAirParticles();
  resetBalls(now);
  if (replay) replay.rounds.push([demoTick(now), windDirection, windMph,
    balls.length]);
  cameraCenter = (worldLeft + worldRight) / 2;
  cameraWidth = 960;
  cameraCenterY = floorY - cameraWidth / cameraAspect / 2;
  cameraContainFloor = 0;
}

// The box the camera packs. It is a fighting-game pushbox rather than the
// animated silhouette on purpose: breathing and stride would otherwise pump
// the lens once per idle cycle. Reach spans a fully extended punch or kick so
// throwing one never shoves the frame; live limbs still set the containment
// floor for anything this box misses.
function fighterFrameRect() {
  let left = Infinity;
  let right = -Infinity;
  let top = Infinity;
  let bottom = -Infinity;
  for (const player of players) {
    // Trimmed 2026-08-09 — @jeffrey wanted the lens closer to the fight.
    // The rect still tracks live position, so jumps and knockbacks widen
    // the frame as they happen; this is standing headroom, not arc room.
    const reach = isHeadOnly(player) ? 34 : isPogo(player) ? 56 : 104;
    const rise = isHeadOnly(player) ? 58 : isPogo(player) ? 148 : 210;
    left = Math.min(left, player.x - reach);
    right = Math.max(right, player.x + reach);
    top = Math.min(top, player.y - rise);
    // Feet swing below the terrain contact point on slopes. Pack their full
    // projected arc so camera yaw cannot shave them off at a safe-zone edge.
    bottom = Math.max(bottom, player.y + (isHeadOnly(player) ? 18 : 38));
  }
  // A shot remains physical until it hits something, so it remains a camera
  // subject too. Include both ends of its swept path to prevent a fast round
  // flickering outside the action-safe frame between simulation ticks.
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const previousX = bullet.previousX ?? bullet.x;
    const previousY = bullet.previousY ?? bullet.y;
    left = Math.min(left, previousX - 32, bullet.x - 32);
    right = Math.max(right, previousX + 32, bullet.x + 32);
    top = Math.min(top, previousY - 32, bullet.y - 32);
    bottom = Math.max(bottom, previousY + 32, bullet.y + 32);
  }
  return { left, right, top, bottom };
}

// Rect-pack: the camera width that makes a world rect exactly fill the
// action-safe frame. Whichever axis runs out first decides the lens, so a
// stacked pair frames as tightly as a spread one.
function rectPackWidth(rect) {
  const safe = actionSafeRect();
  return Math.max(
    (rect.right - rect.left) * (stageRight - stageLeft) /
      Math.max(1, safe.right - safe.left),
    (rect.bottom - rect.top) * cameraAspect * (stageBottom - stageTop) /
      Math.max(1, safe.bottom - safe.top));
}
// A fighter reduced to a bouncing head is a few dozen units tall. Without a
// floor the pack would keep closing until one limb filled the screen.
// Lowered with the reach/rise trim above: the closest the lens may sit.
const frameFloorWidth = () => compactLayout() ? 285 : 315;

// Terrain is flat color, so it only has to reach as far as the lens can see.
// Submitting the whole arena pushed its far corners past the native ±30000
// coordinate guard whenever the camera closed in or sat against a wall, which
// culled the ground quad and left the clear color where the stage should be.
function terrainSpan() {
  const reach = cameraDoll.width;
  return { left: Math.max(worldLeft, cameraCenter - reach),
    right: Math.min(worldRight, cameraCenter + reach),
    top: Math.max(ceilingY, cameraCenterY - reach),
    bottom: Math.min(floorY, cameraCenterY + reach) };
}

function updateCamera(dt) {
  const rect = fighterFrameRect();
  // Look slightly ahead of fast movement so zoom starts before a fighter
  // reaches the safe edge instead of reacting after the crossing.
  const lookAhead = .2;
  let leftDrift = 0;
  let rightDrift = 0;
  let upDrift = 0;
  let downDrift = 0;
  for (const player of players) {
    const dx = (player.vx + (player.windVx || 0) + (player.knockVx || 0) +
      (player.shieldVx || 0)) * lookAhead;
    const dy = player.vy * lookAhead;
    leftDrift = Math.min(leftDrift, dx);
    rightDrift = Math.max(rightDrift, dx);
    upDrift = Math.min(upDrift, dy);
    downDrift = Math.max(downDrift, dy);
  }
  rect.left += leftDrift;
  rect.right += rightDrift;
  rect.top += upDrift;
  rect.bottom += downDrift;
  const maxWidth = Math.max(worldRight - worldLeft,
    (floorY - ceilingY) * cameraAspect);
  // Perspective orbit and uneven ground skew the projected silhouette beyond
  // its orthographic pushbox. A small fixed overscan keeps complete bodies in
  // the action-safe rectangle without making the compact room feel distant.
  const desiredWidth = clamp(rectPackWidth(rect) * 1.12,
    frameFloorWidth(), maxWidth);
  const widthSpeed = desiredWidth > cameraWidth ? 13 : 5.5;
  const widthBlend = 1 - Math.exp(-Math.max(0, dt) * widthSpeed);
  cameraWidth += (desiredWidth - cameraWidth) * widthBlend;
  const halfWidth = cameraWidth / 2;
  const halfHeight = cameraWidth / cameraAspect / 2;
  // Center comes from the same rect the width did, so the frame pans down as
  // the action drops and rides up onto the platform with it.
  let desiredCenter = cameraWidth >= worldRight - worldLeft
    ? (worldLeft + worldRight) / 2
    : clamp((rect.left + rect.right) / 2,
      worldLeft + halfWidth, worldRight - halfWidth);
  // Feet plant a capsule radius under the floor line, and the ground plane
  // keeps covering that far past it, so the frame may sit that low rather
  // than widening just to recentre a standing pair.
  const footRoom = 40;
  let desiredCenterY = halfHeight * 2 >= floorY - ceilingY
    ? (ceilingY + floorY) / 2
    : clamp((rect.top + rect.bottom) / 2,
      ceilingY + halfHeight, floorY + footRoom - halfHeight);
  // Fold containment into the target before easing. Clamping the live camera
  // after easing caused a one-frame reset whenever a fighter crossed the safe
  // edge; the pack width now absorbs that motion while the center remains
  // continuous.
  const containLeft = rect.right - halfWidth * .92;
  const containRight = rect.left + halfWidth * .92;
  if (containLeft <= containRight)
    desiredCenter = clamp(desiredCenter, containLeft, containRight);
  const containTop = rect.bottom - halfHeight * .92;
  const containBottom = rect.top + halfHeight * .92;
  if (containTop <= containBottom)
    desiredCenterY = clamp(desiredCenterY, containTop, containBottom);
  const centerBlend = 1 - Math.exp(-Math.max(0, dt) * 9);
  cameraCenter += (desiredCenter - cameraCenter) * centerBlend;
  cameraCenterY += (desiredCenterY - cameraCenterY) * centerBlend;
}

function updateCameraDoll(dt, now) {
  const introAge = now - roundStartedAt;
  if (roundResult) {
    updateResultReactions(now);
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
      // The killcam used to sit inside the winner's own head, so the winner
      // had to be culled or their body would have filled the lens — which is
      // why the fighter who had just won blinked out for three quarters of a
      // second. It stands off their shoulder now: back along the line between
      // the two of them, raised, and aimed at a point most of the way to the
      // fighter going down. Both are in the frustum, so nothing is hidden to
      // make the shot and nobody vanishes.
      //
      // Every distance is drawn from the gap between the heads, because the
      // pair can be a step apart or the width of the stage apart, and a fixed
      // stand-off frames only one of those.
      const headGap = Math.max(320, Math.abs(loserHead.x - winnerHead.x));
      // Center the complete pair. Favoring the loser pushed the winner across
      // the viewport edge during the eased handoff.
      const focus = { x: lerp(winnerHead.x, loserHead.x, .5),
        y: lerp(winnerHead.y, loserHead.y, .5),
        z: lerp(winnerHead.z, loserHead.z, .5) };
      const shoulder = { x: focus.x, y: focus.y,
        // Stay outside the whole arena depth. The old close killcam entered
        // the world volume and near-clipped limbs, floor, and wall polygons.
        z: focus.z - Math.max(headGap * 1.3, Math.abs(worldNear) + headGap) };
      // Wider than the old close-up had to be, because it now has to hold two
      // fighters rather than one — the 64° lens buys most of that back. Past
      // about a 1300 gap this would open onto empty stage, so beyond there it
      // settles toward the span the returning wide shot uses.
      const shotWidth = Math.min(headGap * 1.7, headGap + 900);
      if (age < .86) {
        cameraDoll.track({ target: focus, position: shoulder,
          width: shotWidth, perspective: 0, fov: 55, roll: 0 }, dt, 11);
        return;
      }
      const returnAmount = clamp((age - .86) / .59, 0, 1);
      const midpoint = { x: (players[0].x + players[1].x) / 2,
        y: (players[0].y + players[1].y) / 2 - 95,
        z: (players[0].z + players[1].z) / 2 };
      const span = Math.max(900, Math.abs(players[1].x - players[0].x) + 540);
      cameraDoll.track({ target: midpoint,
        position: { x: midpoint.x, y: midpoint.y,
          z: lerp(shoulder.z, midpoint.z - span * 1.2, returnAmount) },
        width: lerp(shotWidth, span, returnAmount),
        perspective: 0, fov: 55, roll: 0 }, dt, 10);
      return;
    }
    const target = { x: (players[0].x + players[1].x) / 2,
      y: (players[0].y + players[1].y) / 2 - 95,
      z: (players[0].z + players[1].z) / 2 };
    const horizontalSpan = Math.abs(players[1].x - players[0].x);
    const verticalSpan = Math.abs(players[1].y - players[0].y) * cameraAspect;
    const closeWidth = Math.max(820, horizontalSpan + 540, verticalSpan + 520,
      rectPackWidth(fighterFrameRect()) * 1.22);
    cameraDoll.track({ target,
      position: { x: target.x,
        y: target.y,
        z: target.z - closeWidth * 1.2 },
      width: closeWidth, perspective: 0, fov: 55,
      roll: 0 }, dt, 7);
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
    cameraDoll.track({ target,
      position: { x: target.x, y: target.y, z: target.z - span * 1.2 },
      width: span, perspective: 0, fov: 55, roll: 0 }, dt, 7);
    return;
  }
  const target = { x: cameraCenter, y: cameraCenterY, z: 0 };
  // Measure complete animated silhouettes before rendering. This used to run
  // as a final paint-time correction, which made the viewport skip a frame at
  // the safe-zone edge.
  const containmentWidth = fighterContainmentRequiredWidth(
    (now - startedAt) / 1000000) * 1.04;
  cameraContainFloor = Math.max(cameraContainFloor, containmentWidth);
  // A small overscan absorbs animated hands and feet before
  // they reach the action-safe edge without loosening the close fight shot.
  const naturalWidth = cameraWidth * 1.015;
  // Hysteresis prevents a fighter hovering at the safe edge from repeatedly
  // switching between close and wide framing. The band and the pushbox above
  // are tuned together so the floor never has to release during an idle cycle;
  // if it does, the lens breathes in time with the fighter's own animation.
  if (cameraContainFloor > naturalWidth &&
      naturalWidth < cameraContainFloor * .92) {
    const release = 1 - Math.exp(-Math.max(0, dt) * 1.6);
    cameraContainFloor = lerp(cameraContainFloor, naturalWidth, release);
  }
  const framedWidth = Math.max(naturalWidth, cameraContainFloor);
  // Automatic framing stays orthographic, preventing camera movement from
  // bending the arena. The right stick may still rotate the diorama explicitly.
  const tilt = .026 + playerCameraPitch;
  const dolly = 1.35;
  cameraDoll.track({ target,
    position: {
      x: cameraCenter + Math.sin(playerCameraYaw) * framedWidth * dolly,
      y: cameraCenterY - framedWidth * tilt,
      z: -Math.cos(playerCameraYaw) * framedWidth * dolly },
      width: framedWidth, perspective: 0, fov: 55,
      roll: 0 }, dt, 10);
}

function freezeFinalFrame(now, livePad = -1) {
  const poseTime = (now - startedAt) / 1000000;
  for (const player of players) {
    if (player.pad === livePad) {
      delete player.frozenGeometry;
      delete player.frozenAt;
      continue;
    }
    player.frozenGeometry = runnerWorldGeometry(player, poseTime);
    player.frozenAt = now;
  }
  impactHitboxesUntil = Math.max(impactHitboxesUntil,
    now + (matchOver ? matchResultUs : roundResultUs));
}

function finishRound(now) {
  if (roundResult) return;
  freezeFinalFrame(now, deathCinematic?.winnerPad ?? -1);
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
  return { winner: winner.toLowerCase(), action: "" };
}

function updateResultReactions(now) {
  const winningPad = players[0].score === players[1].score ? -1
    : players[0].score > players[1].score ? 0 : 1;
  for (const player of players) {
    const down = inputPads[player.pad]?.down || [];
    const previous = resultReactionPrevious[player.pad];
    const laughChord = down.includes("A") && down.includes("B") &&
      (!previous.includes("A") || !previous.includes("B"));
    const pressed = laughChord ? "LAUGH" : down.find((button) =>
      !previous.includes(button));
    if (pressed) {
      const winner = player.pad === winningPad;
      if (pressed === "ArrowLeft") player.facing = -1;
      if (pressed === "ArrowRight") player.facing = 1;
      player.resultReaction = winner
        ? ({ LAUGH: "LAUGH", A: "KICK", B: "PUNCH", X: "POSE", Y: "DANCE",
            ArrowLeft: "DASH", ArrowRight: "DASH", ArrowUp: "JUMP",
            ArrowDown: "CROUCH" }[pressed] || "DANCE")
        : ({ A: "CRY", B: "WOE", X: "SULK", Y: "WIGGLE" }[pressed] ||
          "WOE");
      player.resultReactionAt = now;
      if (player.resultReaction === "LAUGH")
        emitSignal("laugh", player.pad, 1, 0);
      else {
        playSine(winner ? 520 : 145, .1);
        emitSignal("reaction", player.pad, winner ? 1 : -1, 0);
      }
    }
    resultReactionPrevious[player.pad] = down.slice();
  }
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
  const command = { A: "/", B: "*", X: ")", Y: "+",
    LeftShoulder: "+", RightShoulder: "+" }[button] || player.lastButton;
  if (["ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight",
      "A", "B", "X", "Y", "LeftShoulder", "RightShoulder"].includes(button))
    recordCommand(player, command, player.lastButtonAt);
  telemetry("FIGHT_BUTTON", player.name + " " + player.lastButton);
}

function recordCommand(player, label, now) {
  const previous = player.commandStream.at(-1);
  if (previous?.label === label && now - previous.at < 40000) return;
  player.commandStream.push({ label, at: now });
  while (player.commandStream.length > commandStreamDepth)
    player.commandStream.shift();
}

function fireGun(player, input) {
  const now = runtime().monotonicUs;
  const pose = gunPose(player, now, input);
  player.gunAimX = pose.dx;
  player.gunAimY = pose.dy;
  const smg = player.gunMode === "RUBBER SMG";
  const rocket = player.gunMode === "ROCKET LAUNCHER";
  if (rocket) {
    grenades.push({ x: pose.muzzle.x, y: pose.muzzle.y, z: pose.muzzle.z,
      vx: pose.dx * 2850, vy: pose.dy * 2850, owner: player.pad,
      fuse: 3.2, alive: true, exploding: false, blastAge: 0, blastRadius: 0,
      hitPlayers: 0, rocket: true });
    while (grenades.length > 12) grenades.shift();
    player.gunAmmo -= 1;
    player.nextGunShotAt = now + 650000;
    player.itemAction = "FIRE";
    player.itemActionStartedAt = now;
    player.itemActionUntil = now + 240000;
    player.pendingMoveLabel = "ROCKET " + player.gunAmmo;
    playDrum("kick", 1.15, panPlayer(player));
    playSine(92, .22);
    emitSignal("rocket", player.pad, pose.dx, pose.dy);
    return;
  }
  // Automatic means a fast succession of discrete rounds, never a shotgun
  // fan. Holding fire schedules the next SMG round below.
  const shots = 1;
  for (let shot = 0; shot < shots; shot++) {
    bullets.push({
      x: pose.muzzle.x, y: pose.muzzle.y, z: pose.muzzle.z,
      vx: pose.dx * (smg ? 5000 : 4200),
      vy: pose.dy * (smg ? 5000 : 4200),
      owner: player.pad, life: 1, rubber: smg, safeUntil: now + 100000,
    });
  }
  while (bullets.length > 24) bullets.shift();
  player.gunAmmo -= shots;
  player.nextGunShotAt = now + (smg ? 85000 : 220000);
  player.itemAction = "FIRE";
  player.itemActionStartedAt = now;
  player.itemActionUntil = now + 170000;
  player.pendingMoveLabel = (smg ? "RUBBER SMG " : "FIRE ") + player.gunAmmo;
  for (let shot = 0; shot < shots; shot++) {
    playDrum(smg ? "block" : "hat", smg ? .62 + shot * .08 : 1.05,
      panPlayer(player));
    playSine((smg ? 310 : 760) + shot * 55, smg ? .055 : .08);
  }
  emitSignal("bullet", player.pad, pose.dx, pose.dy);
}

function spit(player, heavy = false) {
  const now = runtime().monotonicUs;
  if (now < (player.nextSpitAt || 0)) return;
  const direction = player.facing || 1;
  bullets.push({
    x: player.x + direction * 34, y: player.y - 22, z: player.z,
    previousX: player.x + direction * 34, previousY: player.y - 22,
    vx: direction * (heavy ? 3100 : 3900), vy: heavy ? -170 : -45,
    owner: player.pad, life: 1, spit: true, heavy,
    safeUntil: now + 90000,
  });
  while (bullets.length > 24) bullets.shift();
  player.nextSpitAt = now + (heavy ? 520000 : 260000);
  player.lastButton = heavy ? "HEAVY SPIT" : "SPIT";
  player.lastButtonAt = now;
  player.pendingMoveLabel = player.lastButton;
  playSine(heavy ? 118 : 190, heavy ? .16 : .1);
  playDrum(heavy ? "kick" : "hat", heavy ? .82 : .55, panPlayer(player));
  emitSignal("spit", player.pad, heavy ? 2 : 1, direction);
}

function throwGrenade(player, input = null) {
  const now = runtime().monotonicUs;
  const aimX = input?.horizontal || player.facing;
  const aimY = input ? -input.vertical : -.25;
  const length = Math.hypot(aimX, aimY) || 1;
  grenades.push({ x: player.x + player.facing * 150,
    y: player.y - (player.ducking ? 80 : 145), z: player.z,
    vx: aimX / length * 1950, vy: aimY / length * 1950,
    owner: player.pad,
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
        pickup.x, pickup.y, pickup.z) > 90 || !availableArm(player)) continue;
      player.gunAmmo = Math.min(30, player.gunAmmo + pickup.amount);
      player.gunMode = pickup.kind || "HANDGUN";
      player.itemArm = availableArm(player);
      pickup.active = false;
      remember(player, player.gunMode + " +" + pickup.amount);
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
        pickup.x, pickup.y, pickup.z) > 90 || !availableArm(player)) continue;
      player.grenadeAmmo = Math.min(4, player.grenadeAmmo + pickup.amount);
      player.itemArm = availableArm(player);
      pickup.active = false;
      remember(player, "GRENADE +" + pickup.amount);
      playDrum("clap", 1.1, panPlayer(player));
      emitSignal("pickup", player.pad, 2, pickup.amount);
      break;
    }
  }
}

// Ripe fruit is a new body and fifteen seconds. The clock is `roundElapsedUs`
// counting up toward `roundDurationUs`, so winding it back is what "adds
// time" means here — and it is clamped at zero, because a tree cannot make
// the round longer than a round.
// The tree leans in off its wall, so both the picture and the pickup test have
// to agree about where the fruit ended up. One function owns that.
// The tree climbs its wall more than it leans off it, because a fighter pinned
// against that wall still has to be able to reach the fruit from a jump. One
// function owns the geometry so the picture and the pickup test agree.
function treeFruit(tree) {
  const toward = tree.x < (worldLeft + worldRight) / 2 ? 1 : -1;
  const reach = 90 + 330 * tree.growth;
  return { x: tree.x + toward * (reach * .5 + 60), y: tree.y - reach * .9,
    z: tree.z, toward, reach,
    tipX: tree.x + toward * reach * .5, tipY: tree.y - reach * .9 };
}

function updateBodyTrees(dt, now) {
  const poseTime = (now - startedAt) / 1000000;
  for (const tree of bodyTrees) {
    if (tree.growth < 1) {
      tree.growth = Math.min(1, tree.growth + dt * 1000000 / treeRipenUs);
      continue;
    }
    const fruit = treeFruit(tree);
    for (const player of players) {
      if (!player.alive) continue;
      if (runnerDistanceToPoint(player, poseTime, fruit.x, fruit.y, fruit.z) > 320)
        continue;
      tree.growth = 0;
      player.removedParts = [];
      player.partDamage = {};
      player.fallenBodyGeometry = null;
      roundElapsedUs = Math.max(0, roundElapsedUs - treeTimeBonusUs);
      remember(player, "NEW BODY");
      playDrum("bell", 1.15, panPlayer(player));
      emitSignal("powerup", player.pad, 3, treeTimeBonusUs / 1000000);
      break;
    }
  }
}

function drawBodyTree(tree, t) {
  const scale = cameraScale();
  const ripe = tree.growth >= 1;
  // Unripe reads as a sapling rather than a dimmed icon: the trunk and canopy
  // are literally shorter, so the stage tells you how long the walk can wait.
  const { x: fruitX, y: fruitY, toward, tipX, tipY: crownY } = treeFruit(tree);
  const sway = Math.sin(t * 1.4 + tree.x * .0007) * (3 + 9 * tree.growth);
  const bark = mixColor([120, 92, 64], [58, 46, 38], visualTheme.light);
  const leaf = ripe
    ? mixColor([120, 226, 138], [46, 150, 84], visualTheme.light)
    : mixColor([96, 140, 104], [44, 84, 60], visualTheme.light);
  const tipY = crownY + sway;
  worldCapsule(tree.x, tree.y, tree.z, tipX, tipY, tree.z,
    Math.max(2, (11 + 9 * tree.growth) * scale), bark);
  for (const spread of [-1, -.35, .35, 1]) {
    worldCapsule(tipX, tipY, tree.z,
      tipX + toward * (60 + 90 * tree.growth),
      tipY + spread * (60 + 70 * tree.growth), tree.z,
      Math.max(2, (8 + 8 * tree.growth) * scale), leaf);
  }
  if (!ripe) return;
  // The fruit is the part you aim at, so it only exists once it can be taken.
  const point = projectPoint(fruitX, fruitY + sway, tree.z);
  filledDisc(point.x, point.y, Math.max(3, 46 * scale),
    mixColor([236, 108, 132], [188, 62, 92], visualTheme.light));
}

function updatePowerups(now) {
  while (roundElapsedUs >= nextPowerupAtUs) {
    const occupied = [...gunPickups, ...grenadePickups]
      .some((pickup) => pickup.active);
    if (!occupied) {
      const choices = [...gunPickups, grenadePickups[0]];
      const pickup = choices[powerupSequence % choices.length];
      pickup.active = true;
      // The flag owns exact center. Alternating pickup lanes keep a real-sized
      // weapon from reading as hardware attached to its pole.
      pickup.x = (platformLeft + platformRight) / 2 +
        (powerupSequence % 2 ? 1400 : -1400);
      pickup.y = PLATFORM ? platformY - 70 : terrainFloorAt(pickup.x) - 35;
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

function updateBullets(dt, now, combat = true) {
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    bullet.vx += windAcceleration * .12 * dt;
    bullet.previousX = bullet.x;
    bullet.previousY = bullet.y;
    bullet.x += bullet.vx * dt;
    bullet.y += bullet.vy * dt;
    // Shots have no clock-based expiry. Arena surfaces ricochet them so a
    // long-travelling round remains part of the fight until contact.
    if (bullet.x - 24 <= worldLeft + wallThickness) {
      bullet.x = worldLeft + wallThickness + 24;
      bullet.vx = Math.abs(bullet.vx);
    } else if (bullet.x + 24 >= worldRight - wallThickness) {
      bullet.x = worldRight - wallThickness - 24;
      bullet.vx = -Math.abs(bullet.vx);
    }
    if (bullet.y - 24 <= ceilingY + wallThickness) {
      bullet.y = ceilingY + wallThickness + 24;
      bullet.vy = Math.abs(bullet.vy);
    } else if (bullet.y + 24 >= terrainFloorAt(bullet.x) - wallThickness) {
      bullet.y = terrainFloorAt(bullet.x) - wallThickness - 24;
      bullet.vy = -Math.abs(bullet.vy);
    }
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
  if (!combat) return;
  const poseTime = (now - startedAt) / 1000000;
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const shot = { x1: bullet.previousX ?? bullet.x,
      y1: bullet.previousY ?? bullet.y, z1: bullet.z,
      x2: bullet.x, y2: bullet.y, z2: bullet.z, width: 48 };
    for (const fragment of detachedParts) {
      const contact = segmentSegmentClosest(shot, fragment);
      if (contact.distance > (shot.width + fragment.width) / 2) continue;
      const impulse = bullet.heavy ? 1500 : bullet.spit ? 900 : 1200;
      fragment.vx += Math.sign(bullet.vx || 1) * impulse;
      fragment.vy -= bullet.heavy ? 520 : 300;
      fragment.owner = bullet.owner;
      fragment.hitAfter = now + 120000;
      bullet.life = 0;
      impacts.push({ x: contact.secondPoint.x, y: contact.secondPoint.y,
        z: contact.secondPoint.z, life: .18, duration: .18,
        death: false, explosion: false });
      playDrum("hat", .72, panAt(fragment.x1, fragment.z1));
      break;
    }
    if (bullet.life <= 0) continue;
    const targets = [players[bullet.owner === 0 ? 1 : 0], players[bullet.owner]];
    for (const target of targets) {
    if (!target?.alive || (target.pad === bullet.owner && now < bullet.safeUntil))
      continue;
    // Reflect across the circular shield's local surface normal. A square hit
    // returns home; a glancing hit leaves at the corresponding ricochet angle.
    // Flipping `owner` lets the returned shot hurt whoever fired it.
    if (target.blocking) {
      const shield = shieldGeometry(target);
      if (Math.hypot(bullet.x - shield.x, bullet.y - shield.y,
          bullet.z - shield.z) <= shield.radius + 24) {
        let nx = bullet.x - shield.x;
        let ny = bullet.y - shield.y;
        const normalLength = Math.hypot(nx, ny);
        if (normalLength > .001) {
          nx /= normalLength;
          ny /= normalLength;
        } else {
          const speed = Math.hypot(bullet.vx, bullet.vy) || 1;
          nx = -bullet.vx / speed;
          ny = -bullet.vy / speed;
        }
        const normalVelocity = bullet.vx * nx + bullet.vy * ny;
        bullet.vx -= 2 * normalVelocity * nx;
        bullet.vy -= 2 * normalVelocity * ny;
        bullet.x = shield.x + nx * (shield.radius + 25);
        bullet.y = shield.y + ny * (shield.radius + 25);
        bullet.owner = target.pad;
        bullet.life = Math.max(bullet.life, .55);
        impacts.push({ x: bullet.x, y: bullet.y, z: bullet.z,
          life: .18, duration: .18, death: false, explosion: false });
        breakShield(target, now);
        emitSignal("ballblock", target.pad, 1, 0);
        continue;
      }
    }
    const contact = attackCapsuleContact([{
      x1: bullet.previousX ?? bullet.x, y1: bullet.previousY ?? bullet.y,
      z1: bullet.z, x2: bullet.x, y2: bullet.y, z2: bullet.z,
      width: 48, role: "bullet", part: "bullet",
    }], target, poseTime);
    if (contact && contact.separation <= 3) {
      const pointContact = runnerContactToPoint(target, poseTime,
        bullet.x, bullet.y, bullet.z);
      const endpointHit = Math.min(pointContact.headDistance,
        pointContact.bodyDistance) <= 24;
      const headshot = endpointHit
        ? pointContact.headDistance <= pointContact.bodyDistance
        : contact.headshot;
      const segmentIndex = endpointHit && !headshot
        ? pointContact.segmentIndex : contact.segmentIndex;
      bullet.life = 0;
      impacts.push({ x: contact.x, y: contact.y, z: contact.z,
        life: .2, duration: .2, death: headshot,
        explosion: false });
      impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 350000);
      if (headshot)
        killPlayer(target, bullet.owner, now, "SHOT");
      else {
        applyBodyHit(target, segmentIndex,
          bullet.x - bullet.vx, bullet.owner, now,
          bullet.heavy ? 1680 : bullet.spit ? 920 : 1180,
          bullet.heavy ? 260 : bullet.spit ? 90 : 125);
        playDrum("block", .9, panPlayer(target));
      }
      break;
    }
    }
  }
  for (let index = bullets.length - 1; index >= 0; index--)
    if (bullets[index].life <= 0) bullets.splice(index, 1);
}

function updateGrenades(dt, now, combat = true) {
  for (const grenade of grenades) {
    if (!grenade.alive) continue;
    if (grenade.exploding) {
      grenade.blastAge += dt;
      grenade.blastRadius = grenadeBlastRadius *
        Math.min(1, grenade.blastAge / grenadeBlastDuration);
      const poseTime = (now - startedAt) / 1000000;
      for (const player of combat ? players : []) {
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
    if (!grenade.rocket) {
      grenade.vx += windAcceleration * .45 * dt;
      grenade.vy += 1800 * dt;
    }
    grenade.x += grenade.vx * dt;
    grenade.y += grenade.vy * dt;
    grenade.fuse -= dt;
    if (grenade.rocket) {
      const poseTime = (now - startedAt) / 1000000;
      for (const target of combat ? players : []) {
        if (!target.alive || target.pad === grenade.owner) continue;
        if (runnerDistanceToPoint(target, poseTime,
          grenade.x, grenade.y, grenade.z) > 34) continue;
        grenade.fuse = 0;
        break;
      }
    }
    const inset = wallThickness + 35;
    if (grenade.x < worldLeft + inset) {
      grenade.x = worldLeft + inset;
      grenade.vx = Math.abs(grenade.vx) * .65;
      if (grenade.rocket) grenade.fuse = 0;
    } else if (grenade.x > worldRight - inset) {
      grenade.x = worldRight - inset;
      grenade.vx = -Math.abs(grenade.vx) * .65;
      if (grenade.rocket) grenade.fuse = 0;
    }
    if (grenade.y < ceilingY + inset) {
      grenade.y = ceilingY + inset;
      grenade.vy = Math.abs(grenade.vy) * .65;
      if (grenade.rocket) grenade.fuse = 0;
    }
    if (PLATFORM && grenade.vy >= 0 && previousY <= platformY - 30 &&
        grenade.y >= platformY - 30 && grenade.x >= platformLeft &&
        grenade.x <= platformRight) {
      grenade.y = platformY - 30;
      grenade.vy = -Math.abs(grenade.vy) * .55;
      grenade.vx *= .82;
    } else if (grenade.y >= terrainFloorAt(grenade.x) - 30) {
      grenade.y = terrainFloorAt(grenade.x) - 30;
      grenade.vy = -Math.abs(grenade.vy) * .55;
      grenade.vx *= .82;
      if (grenade.rocket) grenade.fuse = 0;
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
  const spec = meleeSpecs[kind];
  if (!spec) return;
  const attackingPart = kind === "KICK"
    ? player.facing > 0 ? "right-leg" : "left-leg"
    : itemHand(player);
  if (!hasPart(player, attackingPart)) return;
  player.attackKind = kind;
  player.lowKick = kind === "KICK" &&
    (player.ducking || (player.crouchBlend || 0) > .35);
  player.attackStartedAt = now;
  player.attackUntil = now + spec.windowUs;
  player.attackHit = false;
  player.attackMomentum = now < player.dashUntil
    ? clamp(Math.abs(player.vx) / walkSpeed, 1.35, 2.25) : 1;
  player.stance = "ATTACK";
  player.pendingMoveLabel = kind;
  playDrum(spec.cue[0], spec.cue[1], panPlayer(player));
  emitSignal(kind.toLowerCase(), player.pad, player.facing, 0);
}

const limbParts = ["left-arm", "right-arm", "left-leg", "right-leg"];
const spiderLegParts = Array.from({ length: 8 }, (_, index) =>
  `spider-leg-${index + 1}`);
const hasPart = (player, part) => !player.removedParts?.includes(part);
const isPogo = (player) => hasPart(player, "torso") &&
  limbParts.every((part) => !hasPart(player, part));
const isHeadOnly = (player) => !hasPart(player, "torso");
const availableArm = (player) => {
  const lead = player.facing > 0 ? "right-arm" : "left-arm";
  if (hasPart(player, lead)) return lead;
  const other = lead === "right-arm" ? "left-arm" : "right-arm";
  return hasPart(player, other) ? other : "";
};
// Ownership stays attached to the arm that picked the object up; turning or
// boarding cannot teleport a gun between hands.
const itemHand = (player) => player.itemArm || availableArm(player);
// Gun before grenade, always: the pistol is the sustained item and the one
// drawn in the hand, so "use what you are holding" needs no mode toggle.
const heldItem = (player) =>
  isHeadOnly(player) ? player.gunAmmo > 0 ? "GUN" : ""
    : isPogo(player) || !itemHand(player) || !hasPart(player, itemHand(player))
    ? "" : player.gunAmmo > 0 ? "GUN"
    : player.grenadeAmmo > 0 ? "GRENADE" : "";
const itemSwinging = (player, now) =>
  (player.attackKind === "WHIP" || player.attackKind === "BASH") &&
  meleePulse(player, now) > 0;

function meleePulse(player, now) {
  if (now >= player.attackUntil || player.attackUntil <= player.attackStartedAt) return 0;
  const phase = (now - player.attackStartedAt) /
    (player.attackUntil - player.attackStartedAt);
  return Math.sin(Math.max(0, Math.min(1, phase)) * Math.PI);
}

function meleeTarget(player, now) {
  const pulse = meleePulse(player, now);
  const spec = meleeSpecs[player.attackKind] || meleeSpecs.PUNCH;
  const lowKick = player.attackKind === "KICK" && player.lowKick;
  return {
    x: player.x + player.facing * (spec.reach + spec.swell * pulse +
      (lowKick ? 34 * pulse : 0)),
    y: lowKick ? terrainFloorAt(player.x + player.facing *
      (spec.reach + spec.swell * pulse)) - 5 : player.y - spec.height,
    z: player.z,
  };
}

function itemActionPulse(player, now) {
  if (!player.itemAction || now >= player.itemActionUntil) return 0;
  const phase = (now - player.itemActionStartedAt) /
    Math.max(1, player.itemActionUntil - player.itemActionStartedAt);
  return Math.sin(clamp(phase, 0, 1) * Math.PI);
}

function itemHandTarget(player, now) {
  // A surviving head grips the pistol at the mouth. It can still aim and fire,
  // but grenades and melee weapons remain limb-dependent.
  if (isHeadOnly(player)) return {
    x: player.x + player.facing * 18,
    y: player.y - 20, z: player.z,
  };
  // A swung weapon rides the striking hand, so the drawn pistol or grenade
  // sits on the live attack capsule instead of floating out in front.
  if (itemSwinging(player, now)) return meleeTarget(player, now);
  const pulse = itemActionPulse(player, now);
  if (player.itemAction === "THROW") return {
    x: player.x + player.facing * (42 + 52 * pulse),
    y: player.y - 118 - 52 * pulse, z: player.z,
  };
  if (player.itemAimLocked ||
      (player.itemAction === "FIRE" && now < player.itemActionUntil)) {
    const aimX = player.gunAimX || player.facing;
    const aimY = player.gunAimY || 0;
    const length = Math.hypot(aimX, aimY) || 1;
    return { x: player.x + aimX / length * 108,
      y: player.y - 102 + aimY / length * 92, z: player.z };
  }
  return { x: player.x + player.facing * 108,
    y: player.y - 115, z: player.z };
}

function gunPose(player, now, input = null) {
  let aimX = input?.horizontal || player.facing;
  let aimY = input ? -input.vertical : 0;
  if (!input && (player.itemAimLocked ||
      (player.itemAction === "FIRE" && now < player.itemActionUntil))) {
    aimX = player.gunAimX || player.facing;
    aimY = player.gunAimY || 0;
  }
  const length = Math.hypot(aimX, aimY) || 1;
  const dx = aimX / length;
  const dy = aimY / length;
  const hand = itemHandTarget(player, now);
  return {
    hand, dx, dy,
    muzzle: { x: hand.x + dx * 54, y: hand.y + dy * 54, z: hand.z },
  };
}

function meleeStrike(player, now) {
  const target = meleeTarget(player, now);
  return {
    x: target.x, y: target.y, z: target.z,
    radius: (meleeSpecs[player.attackKind] || meleeSpecs.PUNCH).radius,
  };
}

function shieldGeometry(player) {
  const aimX = player.shieldAimX || player.facing;
  const aimY = player.shieldAimY || 0;
  const length = Math.hypot(aimX, aimY) || 1;
  return {
    x: player.x + aimX / length * shieldForward,
    y: player.y - (player.shieldCrouched ? 58 : 90) -
      aimY / length * shieldForward,
    z: player.z,
    radius: shieldRadius,
  };
}

// A shield is spent the moment it does its job. It eats exactly one hit, then
// drops — and `shieldLocked` keeps a still-held B from raising it again on the
// very next frame, which is what turns the break into a real opening instead
// of a flicker nobody can see. The shielder is deliberately left free to act:
// breaking the shield is what buys them the swing.
function breakShield(player, now) {
  player.blocking = false;
  player.shieldLocked = true;
  player.shieldBrokenAt = now;
  player.blockFlash = 1;
  player.stance = "SHIELD BREAK";
  player.pendingMoveLabel = "SHIELD BREAK";
  playDrum("bell", 1, panPlayer(player));
  emitSignal("shieldbreak", player.pad, 1, 0);
}

// Stun scales with what the shield ate, so a BASH buys a longer punish than a
// WHIP. The floor is still long enough to answer at 60fps (~8 frames) and the
// ceiling short enough (~18) that a blocked attacker is opened, not deleted.
function shieldStunUs(force) {
  return Math.round(140000 + clamp((force - 1000) / 750, 0, 1) * 160000);
}

function returnBall(ball, player, now, shielded, intensity = 1) {
  const incomingVx = ball.vx;
  const incomingVy = ball.vy;
  const direction = ball.x >= player.x ? 1 : -1;
  const currentSpeed = Math.hypot(ball.vx, ball.vy);
  const response = ball.hitScale || 1;
  const momentum = shielded ? 1 : player.attackMomentum || 1;
  const normalSpeed = (currentSpeed * 1.34 + 720) * intensity *
    momentum * response;
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
  ball.y = Math.min(ball.y, terrainFloorAt(ball.x) - ball.radius - 8);
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
  const dashing = now < player.dashUntil && Math.abs(player.dashVx) > walkSpeed;
  const speed = Math.min(dashing ? 2600 : 1450,
    (420 + Math.abs(player.vx) * (dashing ? .58 : .32)) *
    (ball.hitScale || 1));
  ball.vx = direction * speed;
  ball.vy = -Math.max(dashing ? 420 : 80, speed * (dashing ? .24 : .06));
  ball.x = player.x + direction * (ball.radius + 58);
  ball.y = Math.min(ball.y, terrainFloorAt(ball.x) - ball.radius - 2);
  ball.lastHitBy = player.pad;
  ball.safeUntil = now + 180000;
  ball.safePlayers = 1 << player.pad;
  player.lastButton = dashing ? "DASH BOOT" : "BOOT";
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
  // A popped ball is gone, not retired — a crater should cost the round its
  // ball for a moment, not for good. It re-inflates over the middle once its
  // serve time comes round.
  if (!ball.active && ballEnabled && ball.serveAt && now >= ball.serveAt) {
    ball.active = true;
    ball.x = (platformLeft + platformRight) / 2;
    ball.y = ceilingY + ball.radius + 120;
    ball.z = 0;
    ball.vx = 0;
    ball.vy = 0;
    ball.rotation = 0;
    ball.lastHitBy = -1;
    ball.safeUntil = now + 250000;
    ball.safePlayers = 0;
    emitSignal("ballserve", -1, 0, 0);
  }
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
  const platformSupported = PLATFORM && ball.x >= platformLeft + ball.radius &&
    ball.x <= platformRight - ball.radius &&
    ball.y >= platformY - ball.radius - 2 &&
    ball.y <= platformY - ball.radius + 2;
  const floorSupported = ball.y >= terrainFloorAt(ball.x) - ball.radius - 2;
  const grounded = (platformSupported || floorSupported) && Math.abs(ball.vy) < 180;
  if (!grounded) ball.vx += windAcceleration * (ball.windFactor || .45) * dt;
  ball.vy += 1900 * (ball.gravityFactor || 1) * dt;
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
  if (PLATFORM && ball.vy >= 0 && previous.y <= platformTop && ball.y >= platformTop &&
      ball.x >= platformLeft + ball.radius &&
      ball.x <= platformRight - ball.radius) {
    ball.y = platformTop;
    ball.vy = Math.abs(ball.vy) > 180
      ? -Math.abs(ball.vy) * (ball.bounce || .58) : 0;
    ball.vx *= ball.drag || .992;
  } else if (ball.y > terrainFloorAt(ball.x) - ball.radius) {
    ball.y = terrainFloorAt(ball.x) - ball.radius;
    ball.vy = Math.abs(ball.vy) > 180
      ? -Math.abs(ball.vy) * (ball.bounce || .62) : 0;
    ball.vx *= ball.drag || .992;
  }
  const onSurface = ((PLATFORM && ball.x >= platformLeft + ball.radius &&
    ball.x <= platformRight - ball.radius &&
    Math.abs(ball.y - (platformY - ball.radius)) <= 2) ||
    ball.y >= terrainFloorAt(ball.x) - ball.radius - 2) && Math.abs(ball.vy) < 180;
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
    if (ball.type === "skateboard" && headDistance > ball.radius &&
        bodyDistance <= ball.radius && !player.skateboard) {
      player.skateboard = true;
      ball.active = false;
      ball.heldBy = -1;
      player.lastButton = "SKATEBOARD";
      player.lastButtonAt = now;
      playDrum("clap", .9, panPlayer(player));
      emitSignal("skate-mount", player.pad, 1, 0);
      return;
    }
    const runningContact = player.grounded && Math.abs(player.vx) > 40 &&
      (onSurface || ball.y >= player.y - ball.radius - 55);
    if (runningContact) {
      bootBall(ball, player, now);
      return;
    }
    if (headDistance <= ball.radius) {
      const sourcePad = ball.lastHitBy >= 0 && ball.lastHitBy !== player.pad
        ? ball.lastHitBy : player.pad === 0 ? 1 : 0;
      dismountSkateboard(player, now);
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

// Returns whether the tap was spent on a double-tap move, so a single tap can
// still mean something else to the caller.
function directionTap(player, direction, now) {
  const previousTap = player.lastTap[direction] || -10000000;
  const releasedAt = player.lastRelease[direction] || -10000000;
  player.lastTap[direction] = now;
  if (now - previousTap > doubleTapUs || releasedAt <= previousTap ||
      now - releasedAt < doubleTapReleaseUs) return false;
  player.lastTap[direction] = -10000000;
  player.pendingMoveLabel = direction === "UP" ? "ULTRA JUMP" : "DASH " + direction;
  playDrum("clap", 1.05, panPlayer(player));
  if (direction === "UP") {
    player.vy = -ultraJumpVelocity;
    player.grounded = false;
    player.jumpHeld = false;
    emitSignal("ultrajump", player.pad, 1, 0);
  } else if (direction === "DOWN") {
    if (player.pounding) {
      player.poundLevel = Math.min(3, Math.max(1, player.poundLevel) + 1);
      const cap = poundMaxVelocity * (1 + .5 * (player.poundLevel - 1));
      player.vy = Math.min(cap, Math.max(poundLaunchVelocity, player.vy * 1.55));
      player.pendingMoveLabel = "GROUND POUND " + player.poundLevel;
      playSine(620 + player.poundLevel * 190, .12 + player.poundLevel * .04);
      emitSignal("fastdrop", player.pad, player.poundLevel, player.vy);
      return true;
    }
    // Grounded anywhere above the real floor and not riding a head means the
    // platform, wherever it has been placed. Double-crouch sinks through it.
    if (player.grounded && player.standingOn < 0 &&
        player.y < terrainFloorAt(player.x) - 2)
      sink(player, now);
    // Standing on the floor there is nothing under you to drop through and no
    // height to convert, so the double-tap is spent rather than sold cheap.
    else if (player.grounded) return true;
    else {
      // In the air this is a ground pound, and the height it starts from is
      // the whole economy of the move — remember it now, because the crater
      // is measured against where the fall actually began.
      player.pounding = true;
      player.poundFrom = player.y;
      player.poundLevel = 1;
      player.vy = Math.max(player.vy, poundLaunchVelocity);
      player.grounded = false;
      player.ducking = false;
      player.pendingMoveLabel = "GROUND POUND";
      playSine(520, .16);
      emitSignal("fastdrop", player.pad, 1, 0);
    }
  } else {
    player.facing = direction === "RIGHT" ? 1 : -1;
    player.dashVx = player.facing * 2400;
    player.dashUntil = now + 110000;
    emitSignal("dash", player.pad, player.facing, 0);
  }
  return true;
}

function sink(player, now) {
  player.sinkUntil = now + sinkDurationUs;
  player.grounded = false;
  player.jumpHeld = false;
  player.vy = Math.max(player.vy, 260);
  player.pendingMoveLabel = "SINK";
  playDrum("whoosh", .55, panPlayer(player));
  emitSignal("sink", player.pad, -1, 0);
}

// A short low hop out of crouch. Crouching pins horizontal movement, so a
// direction flick from a tuck has nothing else to mean.
function crouchHop(player, direction, now) {
  player.facing = direction;
  player.vy = -crouchHopVelocity;
  player.grounded = false;
  player.jumpHeld = false;
  player.hopUntil = now + crouchHopPoseUs;
  player.pendingMoveLabel = "CROUCH HOP";
  playDrum("block", .58, panPlayer(player));
  emitSignal("crouchhop", player.pad, direction, 0);
}

function killPlayer(target, killerPad, now, cause = "KO") {
  if (!target.alive) return;
  releaseCarriedBall(target, now);
  if (!deathCinematic && killerPad !== target.pad)
    deathCinematic = { startedAt: now, loserPad: target.pad,
      winnerPad: killerPad, cause };
  freezeFinalFrame(now, deathCinematic?.winnerPad ?? -1);
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
  // The dummy is the opponent everybody gets for free, and its fight is
  // already running under the title screen — so its head is the one number
  // the whole site can share without anybody signing in. Balling yourself
  // does not count; somebody has to have popped it.
  if (target.npc && !target.bot && killerPad !== target.pad)
    emitSignal("dummy-popped", killerPad, target.pad, 1);
}

// The crater. Everything about it is decided by the fall: a pound from a hop
// is a shove, a pound from the ceiling clears the stage. Whoever is standing
// inside the ring when it lands wears it; whoever is in the air over it has
// jumped it, which is the only defence and is meant to be readable from the
// silhouette alone. The ball in the ring is popped and re-served.
//
// The landing consumes the pounder's body but not their life. They continue as
// the existing controllable bouncing-head form, making the move a permanent
// mobility trade without prematurely ending the round.
function groundPound(player, now) {
  player.pounding = false;
  const poundLevel = Math.max(1, player.poundLevel || 1);
  player.poundLevel = 0;
  const fall = clamp(player.y - player.poundFrom, 0, poundFullFall);
  const power = clamp(fall / poundFullFall + (poundLevel - 1) * .22, 0, 1);
  const radius = Math.min(poundMaxRadius,
    poundMinRadius + power * 70 + (poundLevel - 1) * 88);
  const poseTime = (now - startedAt) / 1000000;
  player.stance = "HIT";
  player.lastButton = "GROUND POUND";
  player.lastButtonAt = now;
  const terrainY = terrainFloorAt(player.x);
  impacts.push({ x: player.x, y: terrainY, z: player.z,
    life: .58, duration: .58, death: false, explosion: true,
    blastRadius: radius, power });
  impactHitboxesUntil = Math.max(impactHitboxesUntil, now + 350000);
  playDrum("kick", 1.3, panPlayer(player));
  playSine(76 + power * 34 + poundLevel * 18, .32 + poundLevel * .09);
  emitSignal("blast", player.pad, player.x / worldRight, power);

  for (const target of players) {
    if (target.pad === player.pad || !target.alive) continue;
    const contact = runnerContactToPoint(target, poseTime,
      player.x, terrainY, player.z);
    if (Math.min(contact.headDistance, contact.bodyDistance) > radius) continue;
    if (!target.grounded) {
      // Airborne over the ring is the dodge. It still throws you.
      applyBodyHit(target, contact.segmentIndex, player.x, player.pad, now,
        900 + 700 * power, 620);
      continue;
    }
    killPlayer(target, player.pad, now, "BLASTED");
  }

  for (const item of balls) {
    if (!item.active) continue;
    if (Math.hypot(item.x - player.x, item.y - terrainY) > radius + item.radius)
      continue;
    item.active = false;
    item.heldBy = -1;
    item.vx = 0;
    item.vy = 0;
    item.serveAt = now + 1200000;
    impacts.push({ x: item.x, y: item.y, z: item.z,
      life: .22, duration: .22, death: false, explosion: false });
    emitSignal("ballblock", player.pad, 1, power);
  }

  const geometry = runnerWorldGeometry(player, poseTime);
  for (const limb of limbParts)
    detachPart(player, limb, geometry, player.x, now);
  detachPart(player, "torso", geometry, player.x, now);
  player.alive = true;
  player.stance = "BOUNCE";
  player.lastButton = "HEAD ONLY";
  player.lastButtonAt = now;
}

function resolveMelee(now) {
  const poseTime = (now - startedAt) / 1000000;
  const contacts = [];
  for (const attacker of players) {
    if (!attacker.alive || attacker.attackHit || now >= attacker.attackUntil) continue;
    const attackingLimbs = runnerWorldGeometry(attacker, poseTime).segments
      .filter((segment) => segment.role?.startsWith("attack-"));
    for (const fragment of detachedParts) {
      let closest = null;
      for (const limb of attackingLimbs) {
        const candidate = segmentSegmentClosest(limb, fragment);
        const separation = candidate.distance - (limb.width + fragment.width) / 2;
        if (!closest || separation < closest.separation)
          closest = { ...candidate, separation };
      }
      if (!closest || closest.separation > 3) continue;
      const spec = meleeSpecs[attacker.attackKind] || meleeSpecs.PUNCH;
      fragment.vx += attacker.facing * spec.force * .8;
      fragment.vy -= Math.max(220, spec.lift);
      fragment.owner = attacker.pad;
      fragment.hitAfter = now + 120000;
      attacker.attackHit = true;
      impacts.push({ x: closest.secondPoint.x, y: closest.secondPoint.y,
        z: closest.secondPoint.z, life: .18, duration: .18,
        death: false, explosion: false });
      playDrum("clap", .82, panPlayer(attacker));
      emitSignal("part-hit", attacker.pad, -1, spec.force);
      break;
    }
    if (attacker.attackHit) continue;
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
      if (target.blocking) attacker.vy = Math.min(attacker.vy, -415);
      else target.vx = 0;
      playDrum("block", 1.2, panPlayer(target));
      emitSignal("block", target.pad, attacker.pad, target.blocking ? 1 : 2);
      // Only a real shield trades. A back-block still just shoves, because it
      // costs nothing to hold a direction.
      if (target.blocking) {
        const spec = meleeSpecs[attacker.attackKind] || meleeSpecs.PUNCH;
        const stun = shieldStunUs(spec.force * (attacker.attackMomentum || 1));
        attacker.hitStunUntil = Math.max(attacker.hitStunUntil, now + stun);
        attacker.attackHit = true;
        attacker.attackKind = "";
        attacker.attackUntil = 0;
        attacker.stance = "STUN";
        breakShield(target, now);
      }
    } else if (headshot) {
      if (isHeadOnly(target)) {
        killPlayer(target, attacker.pad, now,
          contacts.length >= 2 ? "TRADE" : "KO");
        continue;
      }
      const poseTime = (now - startedAt) / 1000000;
      target.fallenBodyGeometry = runnerWorldGeometry(target, poseTime);
      target.removedParts = [...limbParts, "torso"];
      target.partDamage = {};
      target.vx = away * 1450;
      target.vy = -520;
      target.grounded = false;
      target.stance = "HEAD ONLY";
      target.lastButton = "HEAD KNOCKED OFF";
      target.lastButtonAt = now;
      playDrum("snare", 1.2, panPlayer(target));
      emitSignal("decapitate", attacker.pad, target.pad, 1);
    }
    else {
      const spec = meleeSpecs[attacker.attackKind] || meleeSpecs.PUNCH;
      const momentum = attacker.attackMomentum || 1;
      applyBodyHit(target, segmentIndex, attacker.x, attacker.pad, now,
        spec.force * momentum, spec.lift * momentum);
      playDrum(spec.thud[0], spec.thud[1], panPlayer(target));
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
    attacker.vy = Math.min(attacker.vy, -985);
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
    // A committed dive passes through a head contact and completes against
    // the terrain. Treating it as ordinary standing used to zero its velocity
    // and leave the fighter strangely paused on the opponent's scalp.
    if (!rider.alive || !base.alive || rider.vy < 0 || rider.pounding) continue;
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
    rider.sinkUntil = 0;
    rider.hopUntil = 0;
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
    : now < player.sinkUntil ? "SINK"
    : !player.grounded && now < player.hopUntil ? "CROUCH HOP"
    : player.ducking ? player.grounded ? "CROUCH" : "AIR CROUCH"
    : player.pogoDive ? "POGO DOWN"
    : player.skateboard && player.grounded ? "SKATE"
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
  player.carryArm = availableArm(player);
  nearest.item.heldBy = player.pad;
  nearest.item.safeUntil = now + 180000;
  nearest.item.safePlayers = 1 << player.pad;
  player.lastButton = "HOLDING";
  player.lastButtonAt = now;
  emitSignal("grab", player.pad, nearest.index, nearest.item.mass);
  playDrum("clap", .72, panPlayer(player));
  return true;
}

function grabNearestFighter(player, now) {
  const target = players.find((candidate) => candidate.pad !== player.pad &&
    candidate.alive && candidate.grabbedBy < 0 &&
    Math.hypot(candidate.x - player.x,
      candidate.y - (player.y - 72), candidate.z - player.z) < 210);
  if (!target) return false;
  player.heldPlayer = target.pad;
  player.carryArm = availableArm(player);
  target.grabbedBy = player.pad;
  target.vx = target.vy = target.vz = 0;
  target.lastButton = "GRABBED";
  target.lastButtonAt = now;
  player.lastButton = "HOLDING";
  player.lastButtonAt = now;
  playDrum("clap", .82, panPlayer(player));
  emitSignal("grab-player", player.pad, target.pad, isHeadOnly(target) ? 1 : 0);
  return true;
}

function grabNearestPart(player, now) {
  let bestIndex = -1;
  let bestDistance = 190;
  for (let index = 0; index < detachedParts.length; index++) {
    const part = detachedParts[index];
    if (part.heldBy >= 0) continue;
    const x = (part.x1 + part.x2) / 2;
    const y = (part.y1 + part.y2) / 2;
    const z = (part.z1 + part.z2) / 2;
    const distance = Math.hypot(x - player.x, y - (player.y - 78), z - player.z);
    if (distance >= bestDistance) continue;
    bestIndex = index;
    bestDistance = distance;
  }
  if (bestIndex < 0) return false;
  const part = detachedParts[bestIndex];
  player.heldPart = bestIndex;
  player.carryArm = availableArm(player);
  part.heldBy = player.pad;
  part.owner = player.pad;
  part.vx = part.vy = 0;
  player.lastButton = "HOLDING " + part.part.toUpperCase();
  player.lastButtonAt = now;
  emitSignal("grab-part", player.pad, bestIndex, 1);
  return true;
}

function stealHeldObject(player, now) {
  const target = players[player.pad === 0 ? 1 : 0];
  if (!target?.alive || Math.hypot(target.x - player.x,
      target.y - player.y, target.z - player.z) > 220) return false;
  let label = "";
  if (target.heldBall >= 0) {
    const index = target.heldBall;
    const item = balls[index];
    target.heldBall = -1;
    player.heldBall = index;
    if (item) item.heldBy = player.pad;
    label = "BALL";
  } else if (target.heldPart >= 0) {
    const index = target.heldPart;
    const part = detachedParts[index];
    target.heldPart = -1;
    player.heldPart = index;
    if (part) { part.heldBy = player.pad; part.owner = player.pad; }
    label = part?.part?.toUpperCase() || "LIMB";
  } else if (target.gunAmmo > 0) {
    player.gunAmmo = target.gunAmmo;
    player.gunMode = target.gunMode;
    target.gunAmmo = 0;
    player.itemArm = availableArm(player);
    target.itemArm = "";
    target.gunMode = "HANDGUN";
    label = player.gunMode;
  } else if (target.grenadeAmmo > 0) {
    player.grenadeAmmo = target.grenadeAmmo;
    target.grenadeAmmo = 0;
    player.itemArm = availableArm(player);
    target.itemArm = "";
    label = "GRENADES";
  } else if (target.skateboard) {
    target.skateboard = false;
    player.skateboard = true;
    label = "SKATEBOARD";
  }
  if (!label) return false;
  target.lastButton = label + " TAKEN";
  target.lastButtonAt = now;
  player.lastButton = "STOLE " + label;
  player.lastButtonAt = now;
  target.hitStunUntil = Math.max(target.hitStunUntil, now + 160000);
  playDrum("clap", 1, panPlayer(player));
  emitSignal("steal", player.pad, target.pad, 1);
  return true;
}

function releaseCarriedPart(player, now) {
  if (player.heldPart < 0) return;
  const part = detachedParts[player.heldPart];
  if (part) {
    part.heldBy = -1;
    part.owner = player.pad;
    part.vx = player.vx + player.facing * 1900;
    part.vy = Math.min(player.vy, -460);
    part.hitAfter = now + 90000;
    part.life = Math.max(part.life, 3.4);
    emitSignal("throw-part", player.pad, player.heldPart, player.facing);
  }
  player.heldPart = -1;
  player.carryArm = "";
}

function releaseCarriedFighter(player, now) {
  if (player.heldPlayer < 0) return;
  const target = players[player.heldPlayer];
  if (target) {
    target.grabbedBy = -1;
    target.x = player.x + player.facing * 118;
    target.y = player.y - 74;
    target.z = player.z;
    target.vx = player.vx + player.facing * 1500;
    target.vy = Math.min(player.vy, -620);
    target.grounded = false;
    target.hitStunUntil = now + 260000;
    emitSignal("throw-player", player.pad, target.pad, player.facing);
  }
  player.heldPlayer = -1;
  player.carryArm = "";
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
  player.carryArm = "";
}

function bouncePogoOnSurface(player, surfaceY, now) {
  player.y = surfaceY;
  player.vy = -pogoBounceVelocity;
  player.grounded = false;
  player.pogoDive = false;
  player.pogoHit = false;
  player.stance = "POGO BOUNCE";
  player.lastButton = "POGO BOUNCE";
  player.lastButtonAt = now;
  playDrum("kick", .9, panPlayer(player));
  emitSignal("pogo", player.pad, -2, pogoBounceVelocity);
}

function shieldBash(player, now) {
  const target = players[player.pad === 0 ? 1 : 0];
  if (!target?.alive || Math.abs(target.x - player.x) > shieldRadius * 1.35 ||
      Math.abs(target.y - player.y) > 170) return;
  const direction = Math.sign(target.x - player.x) || player.facing;
  player.shieldLocked = true;
  player.shieldBrokenAt = now;
  player.blocking = false;
  player.knockVx -= direction * 620;
  player.hitStunUntil = Math.max(player.hitStunUntil, now + 180000);
  target.knockVx += direction * 1250;
  target.hitStunUntil = Math.max(target.hitStunUntil, now + 240000);
  target.grounded = false;
  target.vy = Math.min(target.vy, -180);
  if (target.blocking) {
    target.shieldLocked = true;
    target.shieldBrokenAt = now;
    target.blocking = false;
    target.knockVx += direction * 420;
  }
  impacts.push({ x: (player.x + target.x) / 2, y: player.y - 90,
    z: (player.z + target.z) / 2, life: .26, duration: .26,
    death: false, explosion: true, blastRadius: 180, power: .35 });
  playDrum("block", 1.25, panPlayer(player));
  emitSignal("shield-bash", player.pad, target.pad, target.shieldLocked ? 1 : 0);
}

function updatePlayer(player, pad, dt, now) {
  player.previousY = player.y;
  if (player.grabbedBy >= 0) {
    const carrier = players[player.grabbedBy];
    if (!carrier?.alive || carrier.heldPlayer !== player.pad) {
      player.grabbedBy = -1;
    } else {
      player.x = carrier.x + carrier.facing * 108;
      player.y = carrier.y - 76;
      player.z = carrier.z;
      player.vx = player.vy = player.vz = 0;
      player.grounded = false;
      player.blocking = false;
      player.attackKind = "";
      player.previous = pad.down.slice();
      return;
    }
  }
  if (!player.alive) {
    player.previous = pad.down.slice();
    if (now >= player.respawnAt) {
      player.x = player.spawnX;
      player.y = terrainFloorAt(player.spawnX);
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
      player.jumpHeld = false;
      player.hopUntil = 0;
      player.sinkUntil = 0;
      player.crouchJump = false;
      player.attackMomentum = 1;
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
  const carrying = player.heldBall >= 0 || player.heldPart >= 0 ||
    player.heldPlayer >= 0 || Boolean(heldItem(player));
  const aimLocked = carrying && pad.down.includes("X");
  player.itemAimLocked = aimLocked;
  if (aimLocked && (rawInput.horizontal || rawInput.vertical)) {
    player.gunAimX = rawInput.horizontal;
    player.gunAimY = -rawInput.vertical;
    if (rawInput.horizontal) player.facing = rawInput.horizontal;
  }
  // A broken shield stays down until X is let go, so the opening it bought is
  // spent on attacking rather than on re-guarding by reflex.
  if (player.shieldLocked && !pad.down.includes("X")) player.shieldLocked = false;
  player.blocking = !carrying && !headOnly && pad.down.includes("X") &&
    !player.shieldLocked;
  if (player.blocking && !wasBlocking) {
    player.shieldCrouched = rawInput.vertical < 0 || player.ducking ||
      player.crouchBlend >= .35;
    player.shieldVx = 0;
    player.vx = player.windVx + player.knockVx;
    player.dashUntil = 0;
    player.dashVx = 0;
    player.lastTap = {};
    player.lastRelease = {};
    shieldBash(player, now);
  }
  if (player.blocking && rawInput.vertical < 0) player.shieldCrouched = true;
  if (player.blocking) {
    player.shieldAimX = rawInput.horizontal;
    player.shieldAimY = rawInput.vertical;
    if (rawInput.horizontal) player.facing = rawInput.horizontal;
  } else {
    player.shieldCrouched = false;
    player.shieldAimX = 0;
    player.shieldAimY = 0;
  }
  // Guard plants the fighter immediately, but DOWN remains meaningful: a
  // standing guard can settle into a crouching guard. Once crouched, the pose
  // latches until the shield itself drops even if DOWN is released first.
  const input = player.blocking
    ? { horizontal: 0, vertical: player.shieldCrouched ? -1 : 0 } : rawInput;
  const grabHeld = armCount > 0 && !pogo && !hitStunned && !player.blocking &&
    pad.down.includes("A") && pad.down.includes("B");
  if (grabHeld && !player.grabHeld && player.heldBall < 0 &&
      player.heldPart < 0 &&
      player.heldPlayer < 0) {
    if (!stealHeldObject(player, now) && !grabNearestBall(player, now) &&
        !grabNearestPart(player, now) &&
        !grabNearestFighter(player, now)) {
      player.lastButton = "REACHING";
      player.lastButtonAt = now;
      emitSignal("reach", player.pad, player.facing, 0);
    }
  }
  else if (!grabHeld && player.heldBall >= 0)
    releaseCarriedBall(player, now);
  else if (!grabHeld && player.heldPart >= 0)
    releaseCarriedPart(player, now);
  else if (!grabHeld && player.heldPlayer >= 0)
    releaseCarriedFighter(player, now);
  const inputChanged = input.horizontal !== player.inputX ||
    input.vertical !== player.inputY;
  if (inputChanged &&
      (input.horizontal || input.vertical))
    emitSignal("move", player.pad, input.horizontal, input.vertical);
  player.pendingMoveLabel = "";
  const upPressed = input.vertical > 0 && !player.previous.includes("MOVE_UP");
  const downPressed = input.vertical < 0 && player.inputY >= 0;
  const wasCrouched = player.ducking || player.crouchBlend >= .35;
  const crouchTarget = input.vertical < 0 ||
    (!player.grounded && player.crouchJump && now < player.jumpPoseUntil) ||
    (!player.grounded && now < player.hopUntil) ? 1 : 0;
  const crouchStep = dt * (crouchTarget ? 9 : 11);
  player.crouchBlend += clamp(crouchTarget - player.crouchBlend,
    -crouchStep, crouchStep);
  player.ducking = player.crouchBlend >= .52;
  if (player.attackKind && now >= player.attackUntil) {
    player.attackKind = "";
    player.attackHit = false;
    player.attackMomentum = 1;
  }
  if (player.itemAction && now >= player.itemActionUntil)
    player.itemAction = "";
  if (player.inputX && input.horizontal !== player.inputX)
    player.lastRelease[player.inputX > 0 ? "RIGHT" : "LEFT"] = now;
  if (player.inputY && input.vertical !== player.inputY)
    player.lastRelease[player.inputY > 0 ? "UP" : "DOWN"] = now;
  if (input.horizontal && input.horizontal !== player.inputX) {
    const direction = input.horizontal > 0 ? "RIGHT" : "LEFT";
    recordCommand(player, direction, now);
    if (!directionTap(player, direction, now) && player.grounded &&
        player.ducking && !headOnly && !pogo && !hitStunned)
      crouchHop(player, input.horizontal, now);
  }
  if (input.vertical && input.vertical !== player.inputY) {
    const direction = input.vertical > 0 ? "UP" : "DOWN";
    recordCommand(player, direction, now);
    directionTap(player, direction, now);
    if (headOnly) {
      const alternating = player.headPumpDirection &&
        player.headPumpDirection !== input.vertical &&
        now - (player.headPumpAt || 0) <= 520000;
      player.headBounceCharge = clamp((player.headBounceCharge || 0) +
        (alternating ? .2 : .035), 0, 1);
      player.headPumpDirection = input.vertical;
      player.headPumpAt = now;
      if (alternating && player.grounded) {
        player.vy = -lerp(420, headBounceVelocity * 1.7,
          player.headBounceCharge);
        player.grounded = false;
        player.stance = "BOUNCE " + Math.round(player.headBounceCharge * 5);
        playDrum("block", .45 + player.headBounceCharge * .45,
          panPlayer(player));
        emitSignal("head-bounce", player.pad, input.vertical,
          player.headBounceCharge);
      }
    }
  }

  if (headOnly) {
    const sincePump = now - (player.headPumpAt || 0);
    if (sincePump > 650000)
      player.headBounceCharge = Math.max(0,
        (player.headBounceCharge || 0) - dt * .22);
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
    player.runSince = 0;
  }
  const dashRunningOut = player.grounded && input.horizontal &&
    player.dashUntil > 0 && now >= player.dashUntil;
  if (dashRunningOut && !player.runSince) {
    player.runSince = now;
  }
  if (!player.grounded || !input.horizontal || player.blocking ||
      player.ducking || hitStunned) player.runSince = 0;
  const walkingCleanly = player.grounded && input.horizontal && !player.blocking &&
    !player.ducking && !hitStunned && now >= player.dashUntil;
  if (!walkingCleanly) player.walkSince = 0;
  else if (!player.walkSince) player.walkSince = now;
  else if (!player.runSince && now - player.walkSince >= 650000)
    player.runSince = now;
  const headAirControl = player.grounded ? .55
    : lerp(.55, .9, player.headBounceCharge || 0);
  const mobility = headOnly ? headAirControl
    : pogo ? .68 : legCount === 1 ? .72 : 1;
  const runSpeed = player.runSince
    ? Math.min(runTopSpeed, runStartSpeed +
      (now - player.runSince) / 1000000 * runAcceleration) : 0;
  let controlledVx = aimLocked ? 0
    : player.blocking ? player.shieldVx || 0
    : now < player.dashUntil && Math.abs(player.dashVx) > 0
    ? player.dashVx
    : player.runSince ? input.horizontal * runSpeed * mobility
    : player.ducking && player.grounded ? 0
    : input.horizontal * walkSpeed * mobility;
  if (player.skateboard && player.grounded && !player.blocking) {
    const skateTarget = input.horizontal * 2700;
    const turnRate = Math.sign(skateTarget) !== Math.sign(player.skateVx) ? 1.8 : 3.2;
    player.skateVx += (skateTarget - player.skateVx) *
      (1 - Math.exp(-dt * turnRate));
    const terrainSlope = clamp((terrainFloorAt(player.x + 8) -
      terrainFloorAt(player.x - 8)) / 16, -2.5, 2.5);
    player.skateVx += terrainSlope * 1900 * dt;
    player.skateVx = clamp(player.skateVx, -4200, 4200);
    if (!input.horizontal) player.skateVx *= Math.max(0, 1 - dt * .65);
    controlledVx = player.skateVx;
  } else if (!player.skateboard) player.skateVx = 0;
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

  // Checked before liftoff so the launch frame itself can never be cut.
  if (player.jumpHeld && player.vy < 0 && !player.blocking &&
      input.vertical <= 0) {
    player.vy *= jumpCutScale;
    player.jumpHeld = false;
  }
  if (!aimLocked && !headOnly && upPressed && !player.jumpLaunchAt &&
      (player.grounded || player.airJumpsUsed < 1)) {
    const airJump = !player.grounded;
    player.jumpLaunchAt = now + (airJump ? 1 : jumpAnticipationUs);
    player.crouchJump = !airJump && wasCrouched;
    if (airJump) {
      player.airJumpsUsed++;
      player.doubleJumpLinesUntil = now + 280000;
    }
    player.pendingMoveLabel = player.skateboard ? "OLLIE"
      : airJump ? "DOUBLE JUMP"
      : wasCrouched ? "CROUCH JUMP" : "JUMP";
  }
  if (player.jumpLaunchAt && now >= player.jumpLaunchAt) {
    player.jumpLaunchAt = 0;
    player.jumpPoseUntil = now +
      (player.crouchJump ? crouchJumpPoseUs : jumpPoseUs);
    const jumpScale = player.skateboard ? 1.12
      : pogo ? .88 : legCount === 1 ? .78 : 1;
    player.vy = Math.min(player.vy,
      -(player.crouchJump ? crouchJumpVelocity : jumpVelocity) * jumpScale);
    player.jumpHeld = true;
    player.pogoHit = false;
    player.pogoDive = false;
    player.pounding = false;
    player.grounded = false;
    player.ducking = player.crouchJump;
    playDrum("block", 0.72, panPlayer(player));
    emitSignal("jump", player.pad, 1, 0);
  }
  if (pogo && !player.grounded && !hitStunned && downPressed) {
    player.vy = Math.max(player.vy, 2250);
    player.pogoDive = true;
    player.pogoHit = false;
    player.pendingMoveLabel = "POGO DOWN";
    player.stance = "POGO DOWN";
    playDrum("kick", .78, panPlayer(player));
    playSine(620, .13);
    emitSignal("pogo", player.pad, -1, player.vy);
  }
  if (!player.grounded && !pogo && input.vertical < 0) {
    player.ducking = true;
    player.pendingMoveLabel ||= "AIR CROUCH";
  }

  // Y and either shoulder spend an item, so reaching for a kick can no
  // longer empty a magazine. A loaded hand still colors the punch — into a
  // swing, never a shot.
  const acting = !headOnly && !pogo && !hitStunned && !player.blocking;
  if (acting && player.gunMode === "RUBBER SMG" && player.gunAmmo > 0 &&
      pad.down.includes("Y") && player.previous.includes("Y") &&
      now >= player.nextGunShotAt)
    fireGun(player, input);
  for (const button of pad.down) {
    if (!player.previous.includes(button)) {
      remember(player, button);
      if (button === "X" && !headOnly) {
        player.pendingMoveLabel = "SHIELD";
        playDrum("block", .7, panPlayer(player));
        emitSignal("shield", player.pad, 1, 0);
      }
      else if (acting && !grabHeld && button === "A")
        startMelee(player, "KICK", now);
      else if (acting && !grabHeld && button === "B")
        startMelee(player, itemMelee[heldItem(player)] || "PUNCH", now);
      else if (headOnly && !hitStunned && button === "A")
        spit(player, false);
      else if (headOnly && !hitStunned && button === "B")
        spit(player, true);
      else if ((acting || (headOnly && !hitStunned)) &&
          ["Y", "LeftShoulder", "RightShoulder"].includes(button)) {
        const item = heldItem(player);
        if (item === "GUN") fireGun(player, input);
        else if (item === "GRENADE") throwGrenade(player, input);
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
  player.vy += (player.vy < 0 ? riseGravity : fallGravity) * dt;
  // Keeping DOWN held through a pound drives it down harder. The button is
  // the only thing that makes a pound faster, so the height you fell from and
  // the pressure you kept on it are the two dials on the crater.
  if (player.pounding && input.vertical < 0)
    player.vy = Math.min(poundMaxVelocity *
      (1 + .5 * Math.max(0, player.poundLevel - 1)),
      player.vy + poundHoldAcceleration * dt);
  player.x += player.vx * dt;
  player.y += player.vy * dt;
  player.grounded = false;
  // A sinking fighter is transparent to the platform but never to the floor.
  if (PLATFORM && now >= player.sinkUntil &&
      player.vy >= 0 && previousY <= platformY && player.y >= platformY &&
      player.x >= platformLeft && player.x <= platformRight) {
    player.y = platformY;
    if (headOnly) {
      player.vy = 0;
      player.grounded = true;
      player.stance = input.horizontal ? "ROLL" : "HEAD ONLY";
    } else if (pogo && player.pogoDive) {
      bouncePogoOnSurface(player, platformY, now);
    } else {
      player.vy = 0;
      player.grounded = true;
    }
  } else if (player.y >= terrainFloorAt(player.x)) {
    const terrainY = terrainFloorAt(player.x);
    player.y = terrainY;
    if (headOnly) {
      player.vy = 0;
      player.grounded = true;
      player.stance = input.horizontal ? "ROLL" : "HEAD ONLY";
    } else if (pogo && player.pogoDive) {
      bouncePogoOnSurface(player, terrainY, now);
    } else {
      player.vy = 0;
      player.grounded = true;
    }
  }
  if (!wasGrounded && player.grounded) {
    const onBooster = boosterXs.some((x) => Math.abs(player.x - x) <= boosterRadius);
    player.landPoseUntil = now + 110000;
    player.crouchJump = false;
    player.jumpHeld = false;
    player.hopUntil = 0;
    player.sinkUntil = 0;
    if (player.pounding) groundPound(player, now);
    else if (!headOnly && onBooster) {
      player.vy = -boosterVelocity;
      player.grounded = false;
      player.jumpHeld = true;
      player.airJumpsUsed = 0;
      player.lastButton = "BOOST";
      player.lastButtonAt = now;
      playSine(880, .22);
      playDrum("clap", 1.2, panPlayer(player));
      emitSignal("boost", player.pad, 1, 0);
    } else player.airJumpsUsed = 0;
  }
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

// How long a bot leans on a button, per action rather than one house number.
// A jump has to outlast the whole rise (1760 / 4800 ≈ .37s) or jumpCutScale
// clips the arc to a third of its height and the platform goes out of reach;
// a strike is a stab; a sink tap is half of a double-tap.
const botHoldUs = {
  walk: 90000, jump: 440000, sinkTap: 60000,
  strike: 70000, shield: 120000, item: 110000,
};
// A press cannot reopen until it has been up this long, so `lastRelease`
// always sees a real gap — that gap is what keeps dash, ultra jump and sink
// readable from synthetic input. Four frames clears doubleTapReleaseUs even
// when the release lands a frame late.
const botRestUs = 66668;
// A thumb cannot hold both ends of an axis, so opening one closes the other.
const botAxis = { ArrowLeft: "ArrowRight", ArrowRight: "ArrowLeft",
  ArrowUp: "ArrowDown", ArrowDown: "ArrowUp" };

// The bot's dice. Not Math.random, deliberately: two fresh sims must roll
// identically (the determinism test holds that line), and the reel factory
// reserves the global stream for the match name. Seeded per fighter at round
// start from the round clock, so rounds differ live while a replayed sim
// replays exactly.
function botRoll(player) {
  let s = player.botRngState = (player.botRngState + 0x6d2b79f5) >>> 0;
  s = Math.imul(s ^ (s >>> 15), s | 1);
  s ^= s + Math.imul(s ^ (s >>> 7), s | 61);
  return ((s ^ (s >>> 14)) >>> 0) / 4294967296;
}

// The one door every synthetic press goes through. Pressing a button that is
// already down extends it, so a pursuit reads as one lean instead of a
// stutter; pressing one that is still resting is refused.
function botPress(player, button, holdUs, now) {
  const press = player.botPresses[button];
  if (press && now < press.until) {
    press.until = Math.max(press.until, now + holdUs);
    return true;
  }
  if (press && now < press.until + botRestUs) return false;
  const other = player.botPresses[botAxis[button]];
  if (other && now < other.until) other.until = now;
  player.botPresses[button] = { until: now + holdUs };
  return true;
}

const botHeld = (player, button, now) =>
  now < (player.botPresses[button]?.until || 0);

function botDown(player, now) {
  return Object.keys(player.botPresses)
    .filter((button) => now < player.botPresses[button].until);
}

function botPad(player, opponent, now) {
  if (!player.bot) {
    player.botPresses = {};
    return { connected: true, down: [], leftX: 0, leftY: 0 };
  }
  // Downed, or standing over a downed opponent, the bot opens nothing new —
  // but presses already out still run their length instead of vanishing
  // mid-hold, which would read as a poke.
  if (!player.alive || !opponent.alive)
    return { connected: true, down: botDown(player, now), leftX: 0, leftY: 0 };

  // A scored bot plays its sheet instead of thinking: rows of {at, button,
  // hold} in µs from round start, through the same botPress door so holds
  // and releases stay human-shaped. An empty score is a dummy that stands
  // there and takes it — which is exactly what a sync lab needs on the
  // receiving end. Scores arrive via globalThis.__oskiewarBotScores at round
  // start; no score, and the brain below plays as always.
  if (player.botScore) {
    const at = now - roundStartedAt;
    while (player.botScoreAt < player.botScore.length &&
        player.botScore[player.botScoreAt].at <= at) {
      const row = player.botScore[player.botScoreAt];
      if (botPress(player, row.button, row.hold ?? botHoldUs.strike, now))
        player.botScoreAt += 1;
      else break;
    }
    return { connected: true, down: botDown(player, now), leftX: 0, leftY: 0 };
  }

  const dx = opponent.x - player.x;
  const distance = Math.abs(dx);
  const toward = Math.sign(dx) || player.facing || -1;
  const opponentThreatening = opponent.attackKind &&
    now < opponent.attackUntil && distance < 245 &&
    opponent.facing === -toward;
  const striking = botHeld(player, "A", now) || botHeld(player, "X", now);

  // A guard that answers every swing the frame it starts turns the round into
  // punch-into-shield until the clock dies — the signal census reads punch×6,
  // shield×6 per five seconds and nothing else. So the guard is human now:
  // each incoming swing is noticed once, answered late, and sometimes not at
  // all. The lapse is what lets damage through, and damage is what ends
  // rounds.
  if (opponentThreatening) {
    if (player.botThreatSeen !== opponent.attackStartedAt) {
      player.botThreatSeen = opponent.attackStartedAt;
      const roll = botRoll(player);
      player.botShieldAt = roll < 0.55
        ? now + 70000 + Math.round(roll * 250000) : Infinity;
    }
  } else player.botShieldAt = Infinity;

  // A shield that just ate a swing bought a stun. Swing into it now — the
  // neutral-game cooldown would let the opening close unanswered.
  if (player.shieldBrokenAt &&
      player.shieldBrokenAt !== player.botPunishedAt &&
      now - player.shieldBrokenAt < 80000) {
    player.botPunishedAt = player.shieldBrokenAt;
    player.botAttackAt = now;
  }

  if (opponentThreatening && !striking && now >= player.botShieldAt)
    botPress(player, "X", botHoldUs.shield, now);
  else {
    // The old spacing was tuned for a bigger fighter: stop walking at 155,
    // swing at 225. A punch capsule tops out near 130 of separation and a
    // kick near 170, so that band was whiff purgatory — the census read
    // punch after punch with no body hit until the clock died. Close to
    // where a swing actually lands, and swing only from there — and only on
    // the ground: steering mid-air wedged the bot against a platform lip it
    // used to clear.
    if (player.grounded && distance > 95)
      botPress(player, toward > 0 ? "ArrowRight" : "ArrowLeft",
        botHoldUs.walk, now);
    // Items are ranged, so the bot spends them while closing rather than
    // trading one for a swing it could land anyway. The grenade needs its arc.
    const item = heldItem(player);
    if (item && now >= player.botItemAt &&
        distance >= (item === "GUN" ? 320 : 1200) &&
        botPress(player, "Y", botHoldUs.item, now))
      player.botItemAt = now + 600000;
    if (distance < 165 && now >= player.botAttackAt &&
        botPress(player, player.botAttackSequence % 2 ? "A" : "B",
          botHoldUs.strike, now)) {
      player.botAttackSequence += 1;
      player.botAttackAt = now + 330000 + (player.botAttackSequence % 3) * 70000;
    }
  }

  if (player.grounded && opponent.y < player.y - 180 &&
      now >= player.botJumpAt &&
      botPress(player, "ArrowUp", botHoldUs.jump, now))
    player.botJumpAt = now + 1250000;
  // Knocked up onto the platform the bot would camp out of reach, so it plays
  // the same double-tap-down a player would: two real presses with a real
  // release between them.
  if (PLATFORM && player.grounded && player.standingOn < 0 &&
      player.y < floorY - 40 &&
      opponent.y > player.y + 200 && now >= player.botSinkAt) {
    player.botSinkAt = now + 900000;
    player.botSinkTaps = 2;
    player.botSinkNextAt = now;
  }
  if (player.botSinkTaps > 0 && now >= player.botSinkNextAt &&
      botPress(player, "ArrowDown", botHoldUs.sinkTap, now)) {
    player.botSinkTaps -= 1;
    player.botSinkNextAt = now + botHoldUs.sinkTap + botRestUs;
  }
  return { connected: true, down: botDown(player, now), leftX: 0, leftY: 0 };
}

function gameSim() {
  syncGameView();
  const now = runtime().monotonicUs;
  const dt = Math.min(0.04, Math.max(0.001, (now - lastSimAt) / 1000000));
  lastSimAt = now;
  simulateAirParticles(dt, now);
  if (roundViewer) {
    updateRoundViewer(now, dt);
    return;
  }
  padSnapshots[0] = gamepad(0);
  padSnapshots[1] = gamepad(1);
  inputPads[0] = padSnapshots[0];
  inputPads[1] = padSnapshots[1];
  const cameraPad = padSnapshots[0] || {};
  const cameraX = Number(cameraPad.rightX) || 0;
  const cameraY = Number(cameraPad.rightY) || 0;
  if (Math.abs(cameraX) > .08)
    playerCameraYaw = clamp(playerCameraYaw + cameraX * dt * 1.15, -.62, .62);
  if (Math.abs(cameraY) > .08)
    playerCameraPitch = clamp(playerCameraPitch + cameraY * dt * .72, -.24, .28);
  if (!selecting && Array.isArray(globalThis.__oskiewarTouch?.taps))
    globalThis.__oskiewarTouch.taps.length = 0;
  if (consumeSystemButtons(now)) return;
  // The wordmark screen is a live training round, so the shell reads start
  // and then falls straight through into the fight it is sitting on top of.
  if (shellMode === "MENU") updateShell(now);
  for (const player of players)
    inputPads[player.pad] = player.bot && shellMode === "GAME"
      ? botPad(player, players[player.pad ? 0 : 1], now)
      : player.npc ? { connected: true, down: [], leftX: 0, leftY: 0 }
        : padSnapshots[player.pad];
  if (debugHitboxes && now >= nextInputDebugAt) {
    nextInputDebugAt = now + 500000;
    const values = players.map((player) => {
      const pad = inputPads[player.pad];
      const input = quantizedInput(pad, player.suppressedDirections);
      return "P" + (player.pad + 1) + " down=" +
        (pad.down.join("+") || "NONE") + " stick=" +
        pad.leftX.toFixed(2) + "," + pad.leftY.toFixed(2) + " q=" +
        input.horizontal + "," + input.vertical + " vx=" +
        Math.round(player.vx);
    });
    telemetry("FIGHT_INPUT", values.join(" | "));
  }
  recordReplayCommands(now, inputPads);
  publishSpectator(now);
  if (roundResult) {
    // The scored tail. The killcam dwell used to be the reel's quietest
    // seconds under its most dramatic frames, so the dwell keeps a slow
    // heartbeat and the result card lands with a sting of its own. Voice on
    // the drum channel, record on unrouted signal names, as everywhere.
    if (!resultPulseAt) resultPulseAt = roundOverAt + 900000;
    if (now >= resultPulseAt) {
      resultPulseAt = now + 1050000;
      playDrum("kick", .3, 0);
      emitSignal("result-pulse", -1, 0, 0);
    }
    if (!resultCardStung && now - roundOverAt >= 1100000) {
      resultCardStung = true;
      playDrum("bell", .95, 0);
      emitSignal("result-card", -1, roundResult === "TIE" ? 0 : 1, 0);
    }
    if (INSTANT_REPLAY && instantReplay) {
      updateInstantReplay(now, dt);
      return;
    }
    if (INSTANT_REPLAY) {
      const replayDown = padSnapshots[0]?.down || [];
      if (replayDown.includes("Y") && !replayOfferPrevious.includes("Y") &&
          startInstantReplay(now)) return;
      replayOfferPrevious = replayDown.slice();
    }
    const cinematicWinner = deathCinematic
      ? players[deathCinematic.winnerPad] : null;
    if (cinematicWinner?.alive) updatePlayer(cinematicWinner,
      { connected: true, down: [], leftX: 0, leftY: 0 }, dt, now);
    // Severed pieces belong to the action too; the result clock must not pin
    // them in mid-air while the surviving body completes its landing.
    updateBullets(dt, now, false);
    updateGrenades(dt, now, false);
    updateDetachedParts(dt);
    updateResultImpactDebris(dt);
    updateCameraDoll(dt, now);
    captureFrameTelemetry(now);
    const resultDuration = matchOver ? matchResultUs : roundResultUs;
    if (now - roundOverAt >= resultDuration) {
      emitSignal("update-safe", -1, buildVersion, 0);
      if (selfPlay) startSelfPlay(now);
      else returnToTitle(now, "round-end");
    }
    return;
  }
  if (PAL_SELECT && selecting) {
    updateSelect(now);
    return;
  }
  if (now - roundStartedAt < introDurationUs) {
    // The intro used to pass in silence — a reel that opens on the countdown
    // opened on three mute seconds, and a player heard the round begin with
    // nothing. Ring the "3, 2, 1" on the same two channels the round clock
    // uses: the drum for the ear, the signal for the record.
    const introSecond = Math.ceil(
      (introDurationUs - (now - roundStartedAt)) / 1000000);
    if (introSecond !== lastIntroSecond) {
      lastIntroSecond = introSecond;
      // playDrum is the voice, the signal is the record — the "countdown"
      // name is deliberately not in the sfx routes, or the web would voice
      // every bell twice. value2 marks these as the intro's.
      playDrum("bell", .72 + (3 - introSecond) * .12, 0);
      emitSignal("countdown", -1, introSecond, 1);
    }
    updateCameraDoll(dt, now);
    captureFrameTelemetry(now);
    return;
  }
  if (lastIntroSecond > 0) {
    // The first live tick after the intro is the round opening — one accent,
    // so "go" is a sound and not just a vanished number.
    lastIntroSecond = -1;
    playDrum("block", 1.1, 0);
    emitSignal("fighters-lock", -1, 0, 0);
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
  updateWind(dt, now);
  updatePlayer(players[0], inputPads[0], dt, now);
  updatePlayer(players[1], inputPads[1], dt, now);
  resolvePlayerStanding(now);
  resolvePlayerPushboxes();
  updatePowerups(now);
  updateBodyTrees(dt, now);
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
  for (const impact of impacts) {
    if (!impact.debris) {
      const count = impact.explosion ? 24 : impact.death ? 10 : 6;
      impact.debris = Array.from({ length: count }, (_, index) => {
        const angle = index / count * Math.PI * 2 +
          Math.sin(impact.x * .013 + impact.y * .007) * .7;
        const force = (impact.explosion
          ? 1180 + (impact.power || 0) * 720
          : impact.death ? 660 : 420) *
          (.65 + (index % 3) * .17);
        return { x: impact.x, y: impact.y, z: impact.z || 0,
          vx: Math.cos(angle) * force, vy: Math.sin(angle) * force - 260,
          vz: Math.sin(angle * 1.7) * force * .22,
          radius: impact.explosion ? 9 : 6 };
      });
    }
    for (const mote of impact.debris) {
      mote.vy += fallGravity * .72 * dt;
      mote.x += mote.vx * dt;
      mote.y += mote.vy * dt;
      mote.z += mote.vz * dt;
      const surface = terrainFloorAt(mote.x) - mote.radius;
      if (mote.y > surface) {
        mote.y = surface;
        mote.vy = -Math.abs(mote.vy) * .32;
        mote.vx *= .76;
        mote.vz *= .76;
      }
    }
    impact.life -= dt;
  }
  while (impacts.length && impacts[0].life <= 0) impacts.shift();
  if (players.some((player) => !player.alive) ||
      (timedRound && roundElapsedUs >= roundDurationUs)) {
    if (timedRound && roundElapsedUs >= roundDurationUs &&
        players.every((player) => player.alive))
      roundCause = "TIME";
    finishRound(now);
  }
}

function updateResultImpactDebris(dt) {
  for (const impact of impacts) {
    if (!impact.debris) {
      const count = impact.death ? 20 : impact.explosion ? 24 : 6;
      impact.debris = Array.from({ length: count }, (_, index) => {
        const angle = index / count * Math.PI * 2 +
          Math.sin(impact.x * .013 + impact.y * .007) * .7;
        const force = (impact.death ? 920 : impact.explosion ? 1180 : 420) *
          (.65 + (index % 3) * .17);
        return { x: impact.x, y: impact.y, z: impact.z || 0,
          vx: Math.cos(angle) * force, vy: Math.sin(angle) * force - 360,
          vz: Math.sin(angle * 1.7) * force * .28,
          radius: impact.death ? 7 : impact.explosion ? 9 : 6 };
      });
    }
    for (const mote of impact.debris) {
      mote.vy += fallGravity * .72 * dt;
      mote.x += mote.vx * dt; mote.y += mote.vy * dt; mote.z += mote.vz * dt;
      const surface = terrainFloorAt(mote.x) - mote.radius;
      if (mote.y > surface) {
        mote.y = surface;
        mote.vy = -Math.abs(mote.vy) * .32;
        mote.vx *= .76; mote.vz *= .76;
      }
    }
    impact.life -= dt;
  }
  while (impacts.length && impacts[0].life <= 0) impacts.shift();
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

// Discs are the piece's bulk geometry — every capsule end, every joint. The
// unit ring is constant, so the trig runs once at load, and fanning from a rim
// vertex covers the identical decagon in 8 faces instead of 10 from the center
// (cull is off, depth is flat, colors are flat — the rasterized pixels match).
// A silhouette only has to be as fine as its size on screen. A limb cap a few
// pixels across reads round at six sides; a head at ninety does not, and used
// to show its corners. Rings are precomputed per step so no trig runs inside a
// frame, and the small end is cheaper than the one fixed ring it replaces.
const discRings = [6, 8, 12, 16, 24, 32].map((sides) => {
  const ring = [];
  for (let side = 0; side < sides; side++) {
    const a = side * Math.PI * 2 / sides;
    ring.push(Math.cos(a), Math.sin(a));
  }
  return ring;
});
const discRingFor = (radius) => discRings[
  radius < 6 ? 0 : radius < 13 ? 1 : radius < 26 ? 2
    : radius < 52 ? 3 : radius < 110 ? 4 : 5];

function filledDisc(x, y, radius, color) {
  const [r, g, b] = color;
  const ring = discRingFor(radius);
  const originX = x + ring[0] * radius, originY = y + ring[1] * radius;
  let lastX = x + ring[2] * radius, lastY = y + ring[3] * radius;
  for (let side = 4; side < ring.length; side += 2) {
    const nextX = x + ring[side] * radius;
    const nextY = y + ring[side + 1] * radius;
    screenTriangle(originX, originY, lastX, lastY, nextX, nextY, r, g, b);
    lastX = nextX;
    lastY = nextY;
  }
}

function filledRing(x, y, outerRadius, innerRadius, color) {
  const [r, g, b] = color;
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
      outerB.x, outerB.y, r, g, b);
    screenTriangle(innerA.x, innerA.y, innerB.x, innerB.y,
      outerB.x, outerB.y, r, g, b);
  }
}

function filledCapsule(x1, y1, x2, y2, width, color) {
  const [r, g, b] = color;
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
    x2 + nx, y2 + ny, r, g, b);
  screenTriangle(x1 - nx, y1 - ny, x2 - nx, y2 - ny,
    x2 + nx, y2 + ny, r, g, b);
  filledDisc(x1, y1, radius, color);
  filledDisc(x2, y2, radius, color);
}

// Xbox batches GPU triangles above its D2D line layer, so every bone and joint
// must share this triangle path. The wider silhouette pass and color pass form
// conventional rounded capsules without the native renderer reordering them.
function damagedPartColor(color, player, part) {
  const damage = Number(player?.partDamage?.[part] || 0);
  if (!damage) return color;
  const durability = part === "torso" ? 3 : 2;
  const amount = clamp(damage / durability * .92, 0, .92);
  return mixColor(color, [244, 34, 50], amount);
}

function drawSkeletonSegments(segments, color, outline, player = null) {
  const edge = Math.max(1.25, Math.min(3, cameraScale() * 1.8));
  for (const segment of segments.filter((item) => !item.hitboxOnly))
    filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
      segment.width + edge * 2, outline);
  for (const [index, segment] of segments.entries()) {
    if (segment.hitboxOnly) continue;
    drawPaletteCapsule(segment, player && !player.npc
      ? player.handleColors : null, index, color, player);
  }
}

function paletteColorAt(colors, coordinate, fallback) {
  if (!colors?.length) return fallback;
  const wrapped = ((coordinate % colors.length) + colors.length) % colors.length;
  const first = Math.floor(wrapped);
  const second = (first + 1) % colors.length;
  return mixColor(colors[first], colors[second], wrapped - first);
}

function drawPaletteCapsule(segment, colors, coordinate, fallback, player = null) {
  const bands = colors?.length > 1 ? 6 : 1;
  if (bands === 1) {
    filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
      segment.width, damagedPartColor(fallback, player, segment.part));
    return;
  }
  const dx = segment.x2 - segment.x1;
  const dy = segment.y2 - segment.y1;
  const length = Math.hypot(dx, dy) || 1;
  const radius = segment.width / 2;
  const nx = -dy / length * radius;
  const ny = dx / length * radius;
  let firstColor = fallback;
  let lastColor = fallback;
  for (let band = 0; band < bands; band++) {
    const from = band / bands;
    const to = (band + 1) / bands;
    // A small overlap keeps the gradient continuous after integer projection.
    const overlap = bands > 1 ? .012 : 0;
    const x1 = lerp(segment.x1, segment.x2, Math.max(0, from - overlap));
    const y1 = lerp(segment.y1, segment.y2, Math.max(0, from - overlap));
    const x2 = lerp(segment.x1, segment.x2, Math.min(1, to + overlap));
    const y2 = lerp(segment.y1, segment.y2, Math.min(1, to + overlap));
    const mapped = paletteColorAt(colors,
      coordinate + (from + to) * .5, fallback);
    const color = damagedPartColor(mapped, player, segment.part);
    if (band === 0) firstColor = color;
    if (band === bands - 1) lastColor = color;
    screenTriangle(x1 + nx, y1 + ny, x1 - nx, y1 - ny,
      x2 + nx, y2 + ny, ...color);
    screenTriangle(x1 - nx, y1 - ny, x2 - nx, y2 - ny,
      x2 + nx, y2 + ny, ...color);
  }
  filledDisc(segment.x1, segment.y1, radius, firstColor);
  filledDisc(segment.x2, segment.y2, radius, lastColor);
}

function drawFighterSilhouette(geometry, color, outline, player = null) {
  drawSkeletonSegments(geometry.segments, color, outline, player);
  const headEdge = Math.max(1.25, Math.min(3, cameraScale() * 1.8));
  // The neck connector and solid head are emitted into the same triangle
  // silhouette pass, so the head cannot detach as a separate line-layer ring.
  filledDisc(geometry.head.x, geometry.head.y,
    geometry.head.radius + headEdge, outline);
  // AC stores the @ glyph first; the face is the identity anchor for the same
  // palette that distorts continuously across the body below it.
  const headColor = player && !player.npc && player.handleColors?.length
    ? player.handleColors[0] : color;
  filledDisc(geometry.head.x, geometry.head.y, geometry.head.radius, headColor);
  if (player && isHeadOnly(player)) {
    // The shell rolls from distance travelled while the face is drawn later in
    // stable screen space. A visible chord makes circular geometry's rotation
    // readable without tumbling the eyes or changing their gaze.
    const angle = player.x / Math.max(12, geometry.head.radius) +
      player.facing * Math.PI * .18;
    const reach = geometry.head.radius * .7;
    const dx = Math.cos(angle) * reach;
    const dy = Math.sin(angle) * reach;
    filledCapsule(geometry.head.x - dx, geometry.head.y - dy,
      geometry.head.x + dx, geometry.head.y + dy,
      Math.max(2, geometry.head.radius * .1), mixColor(headColor, outline, .55));
  }
}

function fighterAnimationPhase(player, now = null) {
  const run = runtime();
  now ??= run.simMonotonicUs || run.monotonicUs;
  let state = "IDLE";
  let stateStartedAt = startedAt;
  if (!player.alive) {
    state = "KO";
    stateStartedAt = player.headBustedAt || player.lastButtonAt || now;
  } else if (now < player.hitStunUntil) {
    state = "HIT";
    stateStartedAt = player.lastButtonAt || now;
  } else if (roundResult && ["KICK", "PUNCH", "POSE", "DANCE", "DASH",
      "JUMP", "CROUCH"].includes(player.resultReaction)) {
    state = player.resultReaction === "POSE" || player.resultReaction === "DANCE"
      ? "MEDITATE" : player.resultReaction;
    stateStartedAt = player.resultReactionAt || now;
  } else if (player.blocking) {
    state = "SHIELD";
    stateStartedAt = player.lastButtonAt || now;
  } else if (player.heldBall >= 0) {
    state = "HOLD";
    stateStartedAt = player.lastButtonAt || now;
  } else if (player.grabHeld) {
    state = "REACH";
    stateStartedAt = player.lastButtonAt || now;
  } else if (player.itemAction && now < player.itemActionUntil) {
    state = player.itemAction;
    stateStartedAt = player.itemActionStartedAt || now;
  } else if (player.attackKind && now < player.attackUntil) {
    state = player.attackKind;
    stateStartedAt = player.attackStartedAt || now;
  } else if (isPogo(player)) {
    state = "POGO";
  } else if (now < player.sinkUntil) {
    state = "SINK";
    stateStartedAt = player.sinkUntil - sinkDurationUs;
  } else if (!player.grounded && now < player.hopUntil) {
    state = "CROUCH HOP";
    stateStartedAt = player.hopUntil - crouchHopPoseUs;
  } else if (player.ducking) {
    state = player.grounded ? "CROUCH" : "AIR CROUCH";
  } else if (!player.grounded) {
    state = player.vy < 0 ? "JUMP" : "FALL";
    stateStartedAt = player.jumpPoseUntil
      ? player.jumpPoseUntil - (player.crouchJump ? crouchJumpPoseUs : jumpPoseUs)
      : player.lastButtonAt || now;
  } else if (shellMode === "MENU" && titleAttractMode === "still") {
    state = "MEDITATE";
  } else if (now < player.dashUntil) {
    state = "DASH";
    stateStartedAt = player.lastButtonAt || now;
  } else if (player.runSince) {
    state = "RUN";
    stateStartedAt = player.runSince;
  } else if (Math.abs(player.vx) > 40) {
    state = "WALK";
  }
  const [steps, authoredTicksPerStep, basePhase, loop] =
    fighterAnimationSpecs[state] || fighterAnimationSpecs.IDLE;
  // Descending terrain advances planted-foot exchanges faster. The result is
  // still integer-tick animation, but the legs keep pace with downhill speed
  // instead of the body skating through a leisurely flat-ground cycle.
  const travel = Math.sign(player.vx);
  const downhill = player.grounded && travel
    ? Math.max(0, terrainFloorAt(player.x + travel * 150) -
      terrainFloorAt(player.x)) / 150 : 0;
  const ticksPerStep = ["WALK", "RUN", "DASH"].includes(state)
    ? Math.max(1, Math.round(authoredTicksPerStep /
      (1 + downhill * 1.9 + Math.min(1, Math.abs(player.vx) / 1800) * .28)))
    : authoredTicksPerStep;
  const rawTick = Math.max(0,
    Math.floor((now - stateStartedAt) / replayTickUs));
  const rawStep = Math.floor(rawTick / ticksPerStep);
  const step = loop ? rawStep % steps : Math.min(steps - 1, rawStep);
  let phase = basePhase;
  if (["PUNCH", "KICK", "WHIP", "BASH", "FIRE", "THROW", "REACH"]
      .includes(state)) {
    const section = step / Math.max(1, steps - 1);
    phase = section < .3 ? "STARTUP" : section < .68 ? "ACTIVE" : "RECOVERY";
  } else if (state === "JUMP" || state === "FALL") {
    phase = Math.abs(player.vy) < 150 ? "APEX" : state === "JUMP"
      ? "ASCEND" : "DESCEND";
  }
  // `frameNow` is the clock, not the cycle. It has to keep climbing with real
  // time — the looping states anchor `stateStartedAt` at match start, so a
  // cycle-position clock wraps back to the first second forever and every
  // "is this timestamp still in the future" test answers yes for the rest of
  // the round. That is what pinned a landed fighter in the crouch pose and
  // froze the legs. Quantizing to the tick grid keeps the pose on the sim's
  // 60 Hz phase without inventing in-between frames.
  return { state, phase, step: step + 1, steps, tick: rawTick,
    ticksPerStep, progress: step / Math.max(1, steps - 1),
    frameNow: stateStartedAt + rawTick * replayTickUs };
}

function runnerWorldGeometry(player, t) {
  if (player.spiderDummy) return spiderDummyWorldGeometry(player, t);
  // Rendering consumes the same fixed 60 Hz phase clock as simulation. The
  // display can remain uncapped without inventing in-between combat poses.
  const animation = fighterAnimationPhase(player);
  const poseNow = animation.frameNow;
  const poseCycle = animation.progress * Math.PI * 2;
  const speed = Math.min(1, Math.abs(player.vx) / 1500);
  const meditating = shellMode === "MENU" && titleTransitionAt === null &&
    titleAttractMode === "still";
  const idle = player.grounded && !player.ducking && speed < .03;
  const breath = idle ? Math.sin(poseCycle + player.pad * .7) * 5 : 0;
  const idleSway = idle ? Math.sin(poseCycle + player.pad) * 7 : 0;
  const stride = Math.sin(poseCycle + player.pad * Math.PI) *
    32 * speed;
  const pogo = isPogo(player);
  const headOnly = isHeadOnly(player);
  const noLegs = !hasPart(player, "left-leg") &&
    !hasPart(player, "right-leg") && !headOnly;
  const jumpAnticipation = player.jumpLaunchAt > poseNow
    ? clamp(1 - (player.jumpLaunchAt - poseNow) / jumpAnticipationUs, 0, 1) : 0;
  const landingRecovery = player.landPoseUntil > poseNow
    ? clamp((player.landPoseUntil - poseNow) / 110000, 0, 1) : 0;
  const crouchPose = clamp(Math.max(player.crouchBlend || 0,
    Math.sin(jumpAnticipation * Math.PI) * .72,
    landingRecovery * .45, noLegs ? .45 : 0), 0, 1);
  const height = lerp(180, 108, crouchPose);
  const formDrop = pogo ? 54 : noLegs ? 31 : 0;
  const aimPose = player.itemAimLocked ? player.gunAimX || player.facing : 0;
  const lean = player.facing * (idle ? 5 : 3 + speed * 10) + aimPose * 7;
  const x = player.x;
  const feet = player.y;
  const z = player.z;
  // With neither leg present the pelvis itself becomes the ground contact.
  // Keeping its capsule tangent to the floor prevents a floating torso pose.
  const hipY = noLegs ? feet - 5
    : feet - lerp(58, 40, crouchPose) + formDrop;
  const neckX = x + lean;
  const neckY = feet - height + 54 - breath + formDrop +
    (player.itemAimLocked ? (player.gunAimY || 0) * 8 : 0);
  const attackPulse = meleePulse(player, poseNow);
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
    const target = meleeTarget(player, poseNow);
    const lowKick = player.lowKick;
    const leg = twoBone(x, hipY, target.x, target.y,
      meleeSpecs.KICK.span, lowKick ? player.facing * .35 : -player.facing);
    segment(x, hipY, leg.jointX, leg.jointY, 12, "attack-thigh");
    segment(leg.jointX, leg.jointY, leg.targetX, leg.targetY, 12,
      "attack-shin");
    segment(x, hipY, x - player.facing * (lowKick ? 46 : 28),
      feet - (lowKick ? 18 : 32), 10, "rear-thigh");
    segment(x - player.facing * (lowKick ? 46 : 28),
      feet - (lowKick ? 18 : 32), x - player.facing * 8, feet,
      10, "rear-shin");
  } else if (meditating) {
    segment(x, hipY, x - 48, feet - 12, 11, "left-thigh");
    segment(x - 48, feet - 12, x + 8, feet, 11, "left-shin");
    segment(x, hipY, x + 48, feet - 12, 11, "right-thigh");
    segment(x + 48, feet - 12, x - 8, feet, 11, "right-shin");
  } else if (player.skateboard && player.grounded) {
    const plantedX = x + player.facing * 28;
    const push = Math.sin(poseCycle) * 34;
    segment(x, hipY, plantedX, feet - 27, 11, "lead-thigh");
    segment(plantedX, feet - 27, plantedX + player.facing * 18,
      feet - 6, 11, "lead-shin");
    const pushKnee = x - player.facing * (22 + push * .3);
    segment(x, hipY, pushKnee, feet - 31, 10, "rear-thigh");
    segment(pushKnee, feet - 31,
      x - player.facing * (46 + Math.max(0, push)), feet, 10, "rear-shin");
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
    // The animation phase still chooses the discrete stride; ground probes
    // constrain that frame onto the heightfield without adding new sim steps.
    const leadGround = terrainFloorAt(leadFoot);
    const rearGround = terrainFloorAt(rearFoot);
    segment(x, hipY, leadKnee, leadGround - 30, 10, "lead-thigh");
    segment(leadKnee, leadGround - 30, leadFoot, leadGround, 10, "lead-shin");
    segment(x, hipY, rearKnee, rearGround - 30, 10, "rear-thigh");
    segment(rearKnee, rearGround - 30, rearFoot, rearGround, 10, "rear-shin");
  } else {
    segment(x, hipY, x - 32, feet - 32, 10, "left-thigh");
    segment(x - 32, feet - 32, x - 7, feet - 11, 10, "left-shin");
    segment(x, hipY, x + 32, feet - 43, 10, "right-thigh");
    segment(x + 32, feet - 43, x + 50, feet - 22, 10, "right-shin");
  }
  const arm = idle ? idleSway : player.grounded ? -stride * .7 : 12;
  const elbowY = feet - lerp(94, 76, crouchPose) - breath;
  const handY = feet - lerp(65, 50, crouchPose) - breath * .5;
  const actionNow = poseNow;
  const armAttack = attackPulse > 0 && player.attackKind &&
    player.attackKind !== "KICK";
  if (player.skateboard && player.grounded && !armAttack && !heldItem(player)) {
    const balance = Math.sin(poseCycle) * 18;
    segment(leftShoulderX, shoulderY, x - 42, elbowY - 10 - balance,
      10, "left-upper-arm");
    segment(x - 42, elbowY - 10 - balance, x - 66, handY - balance,
      10, "left-forearm");
    segment(rightShoulderX, shoulderY, x + 42, elbowY - 10 + balance,
      10, "right-upper-arm");
    segment(x + 42, elbowY - 10 + balance, x + 66, handY + balance,
      10, "right-forearm");
  } else if (player.grabHeld) {
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
  } else if (armAttack) {
    // The striking arm outranks the carry pose: a whip or bash has to publish
    // attack capsules or an armed fighter could never land a hand strike.
    const target = meleeTarget(player, poseNow);
    const itemOnRight = itemHand(player) === "right-arm";
    const actionShoulderX = itemOnRight ? rightShoulderX : leftShoulderX;
    const restShoulderX = itemOnRight ? leftShoulderX : rightShoulderX;
    const armPose = twoBone(actionShoulderX, shoulderY, target.x, target.y,
      (meleeSpecs[player.attackKind] || meleeSpecs.PUNCH).span, player.facing);
    segment(actionShoulderX, shoulderY, armPose.jointX, armPose.jointY, 12,
      "attack-upper-arm");
    segment(armPose.jointX, armPose.jointY,
      armPose.targetX, armPose.targetY, 12, "attack-forearm");
    segment(restShoulderX, shoulderY, x - player.facing * 32, elbowY, 10,
      "rest-upper-arm");
    segment(x - player.facing * 32, elbowY,
      x - player.facing * 36, handY, 10, "rest-forearm");
  } else if ((player.itemAction && actionNow < player.itemActionUntil) ||
      (player.gunAmmo > 0 && player.itemAction !== "THROW")) {
    const target = itemHandTarget(player, actionNow);
    const itemOnRight = itemHand(player) === "right-arm";
    const actionShoulderX = itemOnRight ? rightShoulderX : leftShoulderX;
    const restShoulderX = itemOnRight ? leftShoulderX : rightShoulderX;
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

// The first spider dummy deliberately shares the inert dummy controller. Its
// threat is physical scale: four independently destructible leg-pairs spread
// across a daddy-long-legs silhouette around one large central body.
function spiderDummyWorldGeometry(player, t) {
  const x = player.x;
  const feet = player.y;
  const z = player.z;
  const bodyY = feet - 176;
  const sway = Math.sin(t * 1.7 + player.pad) * 5;
  const head = { x, y: bodyY + sway, z, radius: 22 };
  const segments = [];
  const add = (x1, y1, x2, y2, width, role, part,
    z1 = z, z2 = z, hitboxOnly = false) => {
    if (!hasPart(player, part)) return;
    segments.push({ x1, y1, z1, x2, y2, z2, width, role, part, hitboxOnly });
  };
  add(x - 42, bodyY, x + 42, bodyY, 38, "torso", "torso");
  add(x, bodyY - 30, x, bodyY + 31, 34, "spider-body", "torso");
  const legs = [
    [-1, -1], [-1, -.55], [-1, .25], [-1, .72],
    [1, -1], [1, -.55], [1, .25], [1, .72],
  ];
  for (let index = 0; index < legs.length; index++) {
    const [side, lane] = legs[index];
    const part = spiderLegParts[index];
    const rootX = x + side * 31;
    const rootY = bodyY + lane * 31 + sway;
    const lift = 34 + Math.abs(lane) * 28;
    const depthLane = (index - 3.5) * 72;
    const points = [
      [rootX, rootY, z + depthLane * .2],
      [x + side * 76, bodyY - lift, z + depthLane * .48],
      [x + side * 126, bodyY - lift * .42, z + depthLane * .72],
      [x + side * 171, bodyY + 24 + Math.abs(lane) * 19, z + depthLane],
      [x + side * 211, feet - 51 - (index % 2) * 12, z + depthLane * 1.08],
      [x + side * 246, feet, z + depthLane],
    ];
    for (let joint = 0; joint < points.length - 1; joint++) {
      const [x1, y1, z1] = points[joint];
      const [x2, y2, z2] = points[joint + 1];
      add(x1, y1, x2, y2, 13 - joint,
        `spider-leg-${index + 1}-segment-${joint + 1}`, part, z1, z2);
    }
    // One broad top-level combat proxy per complete leg keeps the boss
    // readable to melee even though the visible joints fan through Z.
    add(rootX, rootY, points.at(-1)[0], points.at(-1)[1], 32,
      `spider-hitbox-${index + 1}`, part, z, z, true);
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
        role: segment.role, part: segment.part,
        hitboxOnly: segment.hitboxOnly };
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
  return Math.max(rectPackWidth({ left, right, top, bottom }),
    frameFloorWidth());
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
      fighterContainmentRequiredWidth(t) * 1.11);
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
    rectPackWidth({ left: minX, right: maxX, top: minY, bottom: maxY }),
    frameFloorWidth());
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
  for (const segment of geometry.segments.filter((item) =>
    item.part === part && !item.hitboxOnly)) {
    const paletteCoordinate = geometry.segments.indexOf(segment);
    detachedParts.push({ ...segment, color: player.color.slice(),
      colors: player.npc ? [] : player.handleColors?.map((value) => value.slice()) || [],
      paletteCoordinate,
      vx: direction * (420 + detachedParts.length % 3 * 90),
      vy: -520 - detachedParts.length % 2 * 120,
      spin: direction * (3.5 + detachedParts.length % 4),
      life: 2.6, part, owner: player.pad, heldBy: -1, hitAfter: now + 180000 });
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
  const durability = target.spiderDummy
    ? part === "torso" ? 5 : 3
    : part === "torso" ? 3 : 2;
  target.partDamage[part] = (target.partDamage[part] || 0) + 1;
  emitSignal("partdamage", target.pad,
    [...limbParts, "torso"].indexOf(part), target.partDamage[part] / durability);
  if (target.partDamage[part] < durability) return;
  if (part === "torso") {
    // Removing the body's attachment point releases every surviving limb;
    // the circular head remains as the final playable form.
    for (const limb of target.spiderDummy ? spiderLegParts : limbParts)
      detachPart(target, limb, geometry, sourceX, now);
  }
  detachPart(target, part, geometry, sourceX, now);
  if (target.itemArm === part) {
    if (target.gunAmmo > 0) gunPickups.push({ kind: target.gunMode,
      amount: target.gunAmmo, x: target.x, y: target.y - 70, z: target.z,
      active: true, startsActive: false, respawnAt: Infinity });
    if (target.grenadeAmmo > 0) grenadePickups.push({
      amount: target.grenadeAmmo, x: target.x, y: target.y - 70, z: target.z,
      active: true, startsActive: false, respawnAt: Infinity });
    target.gunAmmo = 0;
    target.grenadeAmmo = 0;
    target.itemArm = "";
    emitSignal("item-drop", target.pad, sourcePad, 1);
  }
  if (target.carryArm === part) {
    if (target.heldBall >= 0) releaseCarriedBall(target, now);
    if (target.heldPart >= 0) releaseCarriedPart(target, now);
    if (target.heldPlayer >= 0) releaseCarriedFighter(target, now);
    target.carryArm = "";
  }
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

function dismountSkateboard(target, now) {
  if (!target.skateboard) return false;
  target.skateboard = false;
  target.skateVx = 0;
  const board = balls.find((item) => item.type === "skateboard") || balls[0];
  Object.assign(board, ballKinds.find((kind) => kind.type === "skateboard"));
  board.active = true;
  board.heldBy = -1;
  board.x = target.x - target.facing * 58;
  board.y = terrainFloorAt(board.x) - board.radius;
  board.z = target.z;
  board.vx = -target.facing * 720;
  board.vy = -260;
  board.safeUntil = now + 260000;
  board.safePlayers = 1 << target.pad;
  emitSignal("skate-dismount", target.pad, 1, 0);
  return true;
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
  if (dismountSkateboard(target, now)) damageParts = false;
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
  const now = runtime().monotonicUs;
  for (const fragment of detachedParts) {
    if (fragment.heldBy >= 0) {
      const carrier = players[fragment.heldBy];
      if (!carrier?.alive || carrier.heldPart !== detachedParts.indexOf(fragment)) {
        fragment.heldBy = -1;
      } else {
        const centerX = (fragment.x1 + fragment.x2) / 2;
        const centerY = (fragment.y1 + fragment.y2) / 2;
        const dx = carrier.x + carrier.facing * 112 - centerX;
        const dy = carrier.y - 78 - centerY;
        fragment.x1 += dx; fragment.x2 += dx;
        fragment.y1 += dy; fragment.y2 += dy;
        fragment.vx = fragment.vy = 0;
        fragment.life = Math.max(fragment.life, 1);
        continue;
      }
    }
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
    const terrainY = terrainFloorAt(centerX);
    if (bottom > terrainY) {
      const correction = terrainY - bottom;
      fragment.y1 += correction;
      fragment.y2 += correction;
      fragment.vy = -Math.abs(fragment.vy) * .42;
      fragment.vx *= .82;
      fragment.spin *= .78;
    }
    const speed = Math.hypot(fragment.vx, fragment.vy);
    if (speed > 650 && now >= (fragment.hitAfter || 0)) {
      for (const target of players) {
        if (!target.alive || target.pad === fragment.owner) continue;
        const contact = runnerContactToPoint(target,
          (now - startedAt) / 1000000, centerX, centerY,
          (fragment.z1 + fragment.z2) / 2);
        if (Math.min(contact.headDistance, contact.bodyDistance) >
            fragment.width / 2 + 18) continue;
        applyBodyHit(target, contact.segmentIndex, centerX, fragment.owner,
          now, Math.min(1800, speed * .72), 240);
        fragment.vx *= -.38;
        fragment.vy = -Math.abs(fragment.vy) * .45;
        fragment.hitAfter = now + 260000;
        playDrum("clap", 1, panPlayer(target));
        emitSignal("part-hit", fragment.owner, target.pad, speed);
        break;
      }
    }
    // Dismembered anatomy remains a physical arena object. It can settle,
    // be picked up again, thrown, and strike either fighter for the rest of
    // the round rather than evaporating on a cosmetic timer.
    fragment.life = Math.max(fragment.life, 1);
  }
  for (let index = detachedParts.length - 1; index >= 0; index--) {
    if (detachedParts[index].life > 0 || detachedParts[index].heldBy >= 0) continue;
    detachedParts.splice(index, 1);
    for (const player of players) {
      if (player.heldPart === index) player.heldPart = -1;
      else if (player.heldPart > index) player.heldPart--;
    }
  }
}

function runnerBodyDistanceToPoint(geometry, px, py, pz = 0) {
  let distance = Infinity;
  for (const segment of geometry.segments)
    distance = Math.min(distance,
      Math.max(0, pointSegmentDistance(px, py, pz, segment) - segment.width / 2));
  return distance;
}

// Comic Relief's packaged horizontal metrics, normalized from its 2048-unit
// em. Colored text is drawn glyph-by-glyph on every host, so using the font's
// real advances here keeps the web canvas and Xbox DirectWrite runs identical.
const comicAdvanceEm = {
  a:.512, b:.593, c:.514, d:.587, e:.548, f:.508, g:.531, h:.578,
  i:.28, j:.403, k:.54, l:.274, m:.777, n:.523, o:.526, p:.535,
  q:.52, r:.48, s:.487, t:.471, u:.52, v:.486, w:.684, x:.59,
  y:.521, z:.538, "0":.61, "1":.45, "2":.61, "3":.61, "4":.61,
  "5":.61, "6":.61, "7":.61, "8":.61, "9":.61, "@":.931,
  "!":.238, "?":.524, ".":.249, ",":.277, ":":.299, ";":.299,
  "<":.381, ">":.381, "^":.581, " ":.299, "/":.512, "-":.417,
  "+":.48, "∞":.837,
};

function comicGlyphAdvance(character, size) {
  return size * (comicAdvanceEm[String(character).toLowerCase()] ?? .58);
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

// Per-glyph contrast flips a single mid-tone letter to the opposite shadow
// while its neighbors keep theirs, which reads as a mistake rather than as
// depth. A run of type picks its direction once, from the sky behind it, and
// each shadow keeps a trace of its own glyph's hue.
function runShadow(color) {
  // Match disk.mjs corner labels: the shadow is always dark, with a trace of
  // the foreground hue. Dark mode must never turn a dark glyph's shadow white.
  return mixColor(color, visualTheme.light > .5 ? [30, 20, 50] : [6, 4, 16],
    .78);
}

// Handle colors are authored per name and can be shorter than the name they
// dress. Cycling keeps every glyph on the same palette instead of dropping
// the tail onto a different color source.
function glyphColor(colors, index, fallback) {
  return colors?.length ? colors[index % colors.length] : fallback;
}

function controlLocale() {
  const caps = typeof capabilities === "function" ? capabilities() : {};
  const keyboard = caps.inputFamily === "keyboard";
  const touch = caps.inputFamily === "touch";
  if (touch) return {
    title: "start", select: "", replayPaused: "paused",
    replayPlaying: "", replay: "",
    combat: "A KICK   B PUNCH   X SHIELD   Y USE ITEM",
  };
  return keyboard ? {
    title: "start",
    select: "P1 A/D + SPACE     P2 LEFT/RIGHT + K     H P2/DUMMY/BOT     G BACK",
    replayPaused: "PAUSED   F PLAY   A D SCRUB   G EXIT",
    replayPlaying: "F PAUSE   A D SCRUB   G EXIT",
    replay: "Q REPLAY",
    combat: "SPACE KICK   ENTER PUNCH   SHIFT SHIELD   ALT USE ITEM   W JUMP",
  } : {
    title: "start",
    select: "LEFT RIGHT SELECT     A READY     X P2 / DUMMY / BOT     B BACK",
    replayPaused: "PAUSED   A PLAY   LEFT RIGHT SCRUB   B EXIT",
    replayPlaying: "A PAUSE   LEFT RIGHT SCRUB   B EXIT",
    replay: "Y REPLAY",
    combat: "A KICK   B PUNCH   X SHIELD   Y USE ITEM   UP JUMP",
  };
}

// Each entry is the cap to draw, what it does, and the button that lights it,
// so the legend can show a key rather than spell one.
function combatKeys() {
  const caps = typeof capabilities === "function" ? capabilities() : {};
  if (caps.inputFamily === "keyboard") return [
    [["A", "D"], "MOVE", "ArrowLeft"], [["D", "D"], ">> DASH", "ArrowRight"],
    ["SPACE", "KICK", "A"], ["ENTER", "PUNCH", "B"],
    ["SHIFT", "SHIELD", "X"], ["ALT", "USE ITEM", "Y"],
    ["W", "JUMP", "ArrowUp"]];
  if (caps.inputFamily === "touch") return [
    ["A", "KICK", "A"], ["B", "PUNCH", "B"],
    ["X", "SHIELD", "X"], ["Y", "USE ITEM", "Y"]];
  return [
    [["LEFT", "RIGHT"], "MOVE", "ArrowLeft"],
    [["RIGHT", "RIGHT"], ">> DASH", "ArrowRight"],
    ["A", "KICK", "A"], ["B", "PUNCH", "B"], ["X", "SHIELD", "X"],
    ["Y", "USE ITEM", "Y"], ["STICK_UP", "JUMP", "ArrowUp"]];
}

// What the buttons do, named on the way into a round. B changes meaning with
// what the hand is carrying, so the legend reads the fighter rather than
// reciting a fixed map.
function combatLegendKeys(player) {
  const swing = itemMelee[heldItem(player)];
  return combatKeys().map(([cap, action, button]) =>
    [cap, swing && action === "PUNCH" ? swing : action, button]);
}

function combatLegend(player) {
  return combatLegendKeys(player)
    .map(([cap, action]) => (cap === "STICK_UP" ? "STICK" : cap) +
      " " + action).join("   ");
}

function selectionControlKeys() {
  const family = typeof capabilities === "function"
    ? capabilities().inputFamily : "xbox";
  if (family === "touch") return [];
  if (family === "keyboard") return [
    [["A", "D"], "SELECT", "ArrowLeft"], ["SPACE", "READY", "A"],
    ["SHIFT", "OPPONENT", "X"], ["ENTER", "BACK", "B"]];
  return [[["LEFT", "RIGHT"], "SELECT", "ArrowLeft"], ["A", "READY", "A"],
    ["X", "OPPONENT", "X"], ["B", "BACK", "B"]];
}

function replayControlKeys(paused) {
  const family = typeof capabilities === "function"
    ? capabilities().inputFamily : "xbox";
  if (family === "touch") return [];
  if (family === "keyboard") return [
    ["F", paused ? "PLAY" : "PAUSE", "A"],
    [["A", "D"], "SCRUB", "ArrowLeft"], ["G", "EXIT", "B"]];
  return [["A", paused ? "PLAY" : "PAUSE", "A"],
    [["LEFT", "RIGHT"], "SCRUB", "ArrowLeft"], ["B", "EXIT", "B"]];
}

function replayOfferKeys() {
  const family = typeof capabilities === "function"
    ? capabilities().inputFamily : "xbox";
  if (family === "touch") return [];
  return family === "keyboard" ? [["Q", "REPLAY", "Y"]]
    : [["Y", "REPLAY", "Y"]];
}

// A key drawn as a key: a gray cap with a line around it, sunk and brightened
// while it is actually held.
// `fade` is how present the cap is: 1 draws it, 0 dissolves it into the stage
// behind. The legend always passes 1; only the command stream ages its caps
// out, and it needs the whole cap to go — face, edge and letter together.
// A controller button drawn as a controller button: the letter in its Xbox
// color on a dark disc with a colored rim; pressed inverts to a filled disc.
// Directions are the same disc with an arrow glyph in neutral ink. Keyboard
// input keeps its keycaps; every other family gets these.
const padButtonInk = {
  A: [96, 200, 80], B: [235, 78, 78], X: [86, 148, 235], Y: [240, 198, 60],
};
const padGlyph = { UP: "↑", DOWN: "↓", LEFT: "←", RIGHT: "→" };
const padButtonDiameter = (size) => Math.round(size * .78) * 2;
function drawPadButton(label, x, y, size, pressed, fade = 1, display = null) {
  const radius = Math.round(size * .78);
  if (fade <= .01) return radius * 2;
  const ground = mixColor([7, 8, 28], [230, 239, 247], visualTheme.light);
  const veil = (color) => fade >= 1 ? color : mixColor(ground, color, fade);
  const ink = padButtonInk[label] || mixColor([148, 158, 178], [96, 104, 124],
    visualTheme.light);
  // Filled like the real thing: the color IS the button. Held brightens it
  // and rims it white; the glyph stays dark and dead-center either way.
  const face = pressed ? mixColor(ink, [255, 255, 255], .3) : ink;
  const cx = x + radius;
  const cy = y + Math.round(size * .75);
  filledDisc(cx, cy, radius, veil(face));
  if (pressed) filledRing(cx, cy, radius, radius - 3, veil([245, 248, 255]));
  const text = display ?? (padGlyph[label] || label).toUpperCase();
  const glyphSize = Math.round(size * .82);
  if (label === "STICK_UP" || label === "DOWN" ||
      label === "LEFT" || label === "RIGHT") {
    const arrowInk = veil([12, 14, 26]);
    const thickness = Math.max(2, Math.round(radius * .11));
    const dx = label === "LEFT" ? -1 : label === "RIGHT" ? 1 : 0;
    const dy = label === "STICK_UP" ? -1 : label === "DOWN" ? 1 : 0;
    const tipX = cx + dx * radius * .42;
    const tipY = cy + dy * radius * .42;
    const tailX = cx - dx * radius * .4;
    const tailY = cy - dy * radius * .4;
    const perpendicularX = -dy * radius * .32;
    const perpendicularY = dx * radius * .32;
    const wingX = tipX - dx * radius * .34;
    const wingY = tipY - dy * radius * .34;
    filledCapsule(tailX, tailY, tipX, tipY, thickness, arrowInk);
    filledCapsule(tipX, tipY, wingX + perpendicularX,
      wingY + perpendicularY, thickness, arrowInk);
    filledCapsule(tipX, tipY, wingX - perpendicularX,
      wingY - perpendicularY, thickness, arrowInk);
    return radius * 2;
  }
  const glyphX = Math.round(cx - handleWidth(text, glyphSize) / 2);
  // DirectWrite's cap shapes carry more visual weight below their nominal
  // midpoint. Lift the Xbox face letters together so A/B/X/Y read centered in
  // the colored hardware circles rather than sitting on their lower halves.
  const glyphY = Math.round(cy - glyphSize * (padButtonInk[label] ? .56 : .5));
  if (padButtonInk[label] || Object.hasOwn(padGlyph, label) ||
      /^[KPSI<>^v]$/.test(text))
    systemWrite(text, glyphX, glyphY, glyphSize, ...veil([12, 14, 26]));
  else typeWrite(text, glyphX, glyphY, glyphSize, ...veil([12, 14, 26]));
  return radius * 2;
}

function drawKeycap(label, x, y, size, pressed, fade = 1) {
  const padX = Math.round(size * .42);
  const height = Math.round(size * 1.5);
  const width = handleWidth(label, size) + padX * 2;
  if (fade <= .01) return width;
  const drop = pressed ? 2 : 0;
  const ground = mixColor([7, 8, 28], [230, 239, 247], visualTheme.light);
  const veil = (color) => fade >= 1 ? color : mixColor(ground, color, fade);
  const face = veil(pressed
    ? mixColor([96, 104, 126], [206, 214, 232], visualTheme.light)
    : mixColor([44, 50, 66], [176, 184, 202], visualTheme.light));
  const edge = veil(pressed
    ? mixColor([210, 220, 240], [40, 46, 62], visualTheme.light)
    : mixColor([112, 122, 146], [96, 104, 124], visualTheme.light));
  if (!pressed)
    box(x + 2, y + 4, width, height, ...veil(mixColor([6, 8, 18], [92, 99, 112],
      visualTheme.light * .7)));
  box(x, y + drop, width, height, ...face);
  strokeBox(x, y + drop, width, height, 2, edge);
  typeWrite(label, x + padX, y + drop + Math.round((height - size) / 2), size,
    ...veil(pressed ? [12, 14, 26] : [238, 242, 252]));
  return width;
}

// Lifted clear of the bottom edge, because the signed-in handle and its
// logout button now sit in that corner and the legend used to run straight
// through them on a narrow view.
function drawControlLegend(ink) {
  if (typeof capabilities === "function" &&
      capabilities().inputFamily === "touch") return;
  const safe = hudSafeRect();
  const pad = inputPads[0] || { down: [], leftX: 0, leftY: 0 };
  const held = pad.down || [];
  const directionActive = (button) => held.includes(button) ||
    (button === "ArrowLeft" && pad.leftX < -.12) ||
    (button === "ArrowRight" && pad.leftX > .12) ||
    (button === "ArrowUp" && pad.leftY > .12) ||
    (button === "ArrowDown" && pad.leftY < -.12);
  const size = compactLayout() ? 18 : 24;
  const x = safe.left + 8;
  const both = held.includes("A") && held.includes("B");
  const dash = players[0].lastButton === "DASH" &&
    runtime().monotonicUs - players[0].lastButtonAt < 700000;
  const controls = [
    ["LEFT", "ArrowLeft", directionActive("ArrowLeft") ? "MOVE" : ""],
    ["RIGHT", "ArrowRight", dash ? "DASH >>" :
      directionActive("ArrowRight") ? "MOVE" : ""],
    ["STICK_UP", "ArrowUp", directionActive("ArrowUp") ? "JUMP" : ""],
    ["DOWN", "ArrowDown", directionActive("ArrowDown") ? "CROUCH" : ""],
    ["A", "A", both ? "GRAB" : held.includes("A") ? "KICK" : ""],
    ["B", "B", !both && held.includes("B") ? "PUNCH" : ""],
    ["X", "X", held.includes("X") ? "SHIELD" : ""],
    ["Y", "Y", held.includes("Y") ? "USE ITEM" : ""],
  ];
  const keyboard = keycapFamily();
  const keyboardCap = { LEFT: "A", RIGHT: "D", STICK_UP: "W", DOWN: "S",
    A: "SPACE", B: "ENTER", X: "SHIFT", Y: "ALT" };
  const step = Math.round(size * 1.82);
  for (const [row, [cap, button, action]] of controls.entries()) {
    const y = safe.top + row * step;
    const width = keyboard
      ? drawKeycap(keyboardCap[cap] || cap,
        x, y, size,
        directionActive(button))
      : drawPadButton(cap, x, y, size,
        button.startsWith("Arrow") ? directionActive(button) : held.includes(button));
    if (action) typeWrite(action, x + width + 10,
      y + Math.round(size * .25), size, ...ink);
  }
}

const controlRailWidth = () => compactLayout() ? 138 : 188;

// Touch is part of the game surface, not a DOM overlay. One drawn d-pad and
// four action discs share the exact centers used by mac-test's canvas hit zones.
function drawTouchControls() {
  if (typeof capabilities !== "function" ||
      capabilities().inputFamily !== "touch" || capabilities().socialPreview)
    return;
  const held = inputPads[0]?.down || [];
  const spread = 64;
  const cy = viewHeight - 140;
  const dpadX = 130;
  const idle = mixColor([58, 66, 86], [170, 180, 196], visualTheme.light);
  const live = mixColor([110, 220, 150], [38, 128, 88], visualTheme.light);
  const arm = 38;
  const thick = 18;
  const directions = [[0, -1, "ArrowUp"], [0, 1, "ArrowDown"],
    [-1, 0, "ArrowLeft"], [1, 0, "ArrowRight"]];
  for (const [dx, dy, key] of directions)
    filledCapsule(dpadX + dx * 8, cy + dy * 8,
      dpadX + dx * arm, cy + dy * arm, thick,
      held.includes(key) ? live : idle);
  filledDisc(dpadX, cy, thick, held.some((key) => key.startsWith("Arrow"))
    ? live : idle);
  const actionX = viewWidth() - 130;
  const commandGlyph = { A: "/", B: "*", X: ")", Y: "+" };
  for (const [dx, dy, key] of [[0, -1, "Y"], [0, 1, "A"],
      [-1, 0, "X"], [1, 0, "B"]])
    drawPadButton(key, actionX + dx * spread - 27,
      cy + dy * spread - 25, 35, held.includes(key), 1, commandGlyph[key]);
}

function keycapRunWidth(entries, size) {
  const keyboard = keycapFamily();
  const padX = Math.round(size * .42);
  const capWidth = (label) => keyboard
    ? handleWidth(label, size) + padX * 2 : padButtonDiameter(size);
  return entries.reduce((total, [cap, action]) =>
    total + (Array.isArray(cap) ? cap : [cap]).reduce((width, label, index) =>
      width + capWidth(label) + (index ? 5 : 0), 0) + 8 +
      handleWidth(action, size) + 26, 0) - 26;
}

// Keyboard reads as keycaps; every other family reads as controller discs.
const keycapFamily = () => (typeof capabilities === "function"
  ? String(capabilities().inputFamily || "") : "") === "keyboard";

function drawKeycapRun(entries, x, y, size, held, ink, revealAction = null) {
  const keyboard = keycapFamily();
  let cursor = x;
  for (const [cap, action, button] of entries) {
    for (const [index, label] of (Array.isArray(cap) ? cap : [cap]).entries()) {
      if (index) cursor += 5;
      cursor += keyboard
        ? drawKeycap(label, cursor, y, size, held.includes(button))
        : drawPadButton(label, cursor, y, size, held.includes(button));
    }
    cursor += 8;
    if (!revealAction || revealAction([cap, action, button]))
      typeWrite(action, cursor, y + Math.round(size * .25), size, ...ink);
    cursor += handleWidth(action, size) + 26;
  }
  return cursor;
}

function drawCenteredKeycapRun(entries, y, size, held, ink) {
  if (!entries.length) return;
  drawKeycapRun(entries, viewCenterX() - keycapRunWidth(entries, size) / 2,
    y, size, held, ink);
}

function drawHandle(handle, x, y, size, colors, fallback) {
  let cursor = x;
  const characters = [...String(handle)];
  for (let index = 0; index < characters.length; index++) {
    typeWrite(characters[index], cursor, y, size,
      ...glyphColor(colors, index, fallback));
    cursor += comicGlyphAdvance(characters[index], size);
  }
}

function drawFace(player, head, color, t, now = runtime().monotonicUs) {
  if (head.radius < 5) return;
  const bodyDepth = triangleDepth;
  triangleDepth = bodyDepth - .012;
  const r = head.radius;
  const direction = player.facing || 1;
  const stroke = (x1, y1, x2, y2, width, ink = color) =>
    filledCapsule(x1, y1, x2, y2, width, ink);
  const celebrating = ["DANCE", "LAUGH", "POSE", "WIGGLE"]
    .includes(player.resultReaction);
  const grieving = ["CRY", "WOE", "SULK"].includes(player.resultReaction);
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
  const victoryAmount = deathCinematic?.winnerPad === player.pad
    ? clamp(deathCinematicAge(now) / 1.15, 0, 1) : 0;
  if (!player.alive || player.hit > .6 || inertDummy) {
    for (const offset of [-eyeGap, eyeGap]) {
      stroke(faceX + offset - eyeWidth, eyeY - eyeWidth,
        faceX + offset + eyeWidth, eyeY + eyeWidth, lineWidth);
      stroke(faceX + offset + eyeWidth, eyeY - eyeWidth,
        faceX + offset - eyeWidth, eyeY + eyeWidth, lineWidth);
    }
  } else if (blink) {
    stroke(faceX - eyeGap - eyeWidth, eyeY, faceX - eyeGap + eyeWidth,
      eyeY, lineWidth);
    stroke(faceX + eyeGap - eyeWidth, eyeY, faceX + eyeGap + eyeWidth,
      eyeY, lineWidth);
  } else {
    const pad = inputPads[player.pad] || {};
    const rawX = Number(pad.leftX) || 0;
    const rawY = Number(pad.leftY) || 0;
    const movementDeadzone = .48;
    const gazeNoiseFloor = .015;
    const digitalX = (pad.down || []).includes("ArrowRight") ? 1
      : (pad.down || []).includes("ArrowLeft") ? -1 : 0;
    const digitalY = (pad.down || []).includes("ArrowUp") ? 1
      : (pad.down || []).includes("ArrowDown") ? -1 : 0;
    const rawMagnitude = Math.hypot(rawX, rawY);
    const gazeAmount = rawMagnitude <= gazeNoiseFloor ? 0
      : clamp((rawMagnitude - gazeNoiseFloor) /
        (movementDeadzone - gazeNoiseFloor), 0, 1);
    const defeated = victoryAmount > 0
      ? players[deathCinematic.loserPad] : null;
    const lookX = defeated
      ? Math.sign(defeated.x - player.x) || direction
      : digitalX || lerp(direction * .2, rawX, gazeAmount);
    const lookY = defeated
      ? clamp((player.y - defeated.y) / 180, -1, 1)
      : digitalY || lerp(0, rawY, gazeAmount);
    const pupilX = clamp(lookX, -1, 1) * eyeWidth * 1.05;
    const pupilY = clamp(-lookY, -1, 1) * eyeWidth * 1.05;
    const socketRadius = eyeWidth * 2.35;
    const pupilRadius = eyeWidth * .9;
    const socketInk = [246, 248, 244];
    const pupilInk = [8, 12, 24];
    for (const offset of [-eyeGap, eyeGap]) {
      filledDisc(faceX + offset, eyeY, socketRadius, socketInk);
      triangleDepth = bodyDepth - .02;
      filledDisc(faceX + offset + pupilX, eyeY + pupilY,
        pupilRadius, pupilInk);
      triangleDepth = bodyDepth - .012;
    }
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
  if (grieving) {
    const tear = [112, 208, 255];
    const fall = (t * 34) % Math.max(3, r * .55);
    for (const offset of [-eyeGap, eyeGap])
      stroke(faceX + offset, eyeY + eyeWidth * 1.5,
        faceX + offset, eyeY + eyeWidth * 1.5 + r * .34 + fall,
        Math.max(1.5, lineWidth * .72), tear);
  }
  const mouthY = head.y + r * .3;
  if (player.resultReaction === "LAUGH") {
    const age = Math.max(0, (now - player.resultReactionAt) / 1000000);
    const open = .35 + Math.abs(Math.sin(age * 18)) * .65;
    filledRing(faceX + direction * r * .08, mouthY,
      r * (.12 + open * .1), r * .045, color);
    for (let index = 0; index < 3; index++) {
      const noteAge = (age * .72 + index * .27) % 1;
      const note = index % 2 ? "♫" : "♪";
      systemWrite(note,
        faceX + direction * r * (.34 + noteAge * 1.25) + index * direction * 3,
        mouthY - r * (.25 + noteAge * 1.75),
        Math.max(8, r * (.55 - noteAge * .16)), ...color);
    }
  } else if (celebrating || victoryAmount > 0) {
    const grin = r * (.09 + victoryAmount * .2);
    const width = r * (.23 + victoryAmount * .12);
    stroke(faceX - width, mouthY - grin * .18, faceX,
      mouthY + grin, lineWidth);
    stroke(faceX, mouthY + grin, faceX + width,
      mouthY - grin * .18, lineWidth);
  } else if (player.attackKind && meleePulse(player, now) > 0) {
    filledRing(faceX + direction * r * .12, mouthY,
      Math.max(1.8, r * .13), Math.max(.4, r * .05), color);
  } else if (player.blocking) {
    stroke(faceX - r * .26, mouthY, faceX + r * .26, mouthY, lineWidth);
  } else if (grieving || !player.alive || player.hit > .6) {
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
  triangleDepth = bodyDepth;
}

function drawInventory(player, now) {
  const scale = cameraScale();
  const gunColor = player.gunMode === "ROCKET LAUNCHER" ? [48, 61, 52]
    : player.gunMode === "RUBBER SMG" ? [38, 53, 72] : [63, 43, 76];
  const grenadeColor = [255, 105, 105];
  const firing = player.itemAction === "FIRE" && now < player.itemActionUntil;
  const throwing = player.itemAction === "THROW" && now < player.itemActionUntil;
  // A bash keeps the grenade visible in the fist; itemHandTarget already
  // parks both weapons on the swinging hand.
  const bashing = player.attackKind === "BASH" && itemSwinging(player, now);
  if ((player.gunAmmo > 0 || firing) && !throwing) {
    const pose = gunPose(player, now);
    const hand = projectPoint(pose.hand.x, pose.hand.y, pose.hand.z);
    const barrel = projectPoint(pose.muzzle.x, pose.muzzle.y, pose.muzzle.z);
    const barrelWidth = Math.max(3, 9 * scale);
    const gripWidth = Math.max(2, 6 * scale);
    // Xbox batches its native line layer underneath GPU fighter triangles.
    // Held items therefore use the same depth-aware capsule path as the hand.
    filledCapsule(hand.x, hand.y, barrel.x, barrel.y,
      barrelWidth, gunColor);
    const gripX = hand.x - player.facing * 8 * scale;
    const gripY = hand.y + 20 * scale;
    filledCapsule(hand.x, hand.y, gripX, gripY, gripWidth, gunColor);
    if (player.gunMode === "RUBBER SMG") {
      // One uninterrupted dark silhouette: longer receiver, rear stock, and a
      // forward magazine distinguish it from the short pistol in-hand.
      const stockX = hand.x - pose.dx * 32 * scale;
      const stockY = hand.y - pose.dy * 32 * scale;
      filledCapsule(hand.x, hand.y, stockX, stockY,
        barrelWidth * 1.12, gunColor);
      const magazineX = hand.x + pose.dx * 18 * scale - pose.dy * 17 * scale;
      const magazineY = hand.y + pose.dy * 18 * scale + pose.dx * 17 * scale;
      filledCapsule(hand.x + pose.dx * 18 * scale,
        hand.y + pose.dy * 18 * scale, magazineX, magazineY,
        gripWidth, gunColor);
    }
    if (player.gunMode === "ROCKET LAUNCHER") {
      const rearX = hand.x - pose.dx * 38 * scale;
      const rearY = hand.y - pose.dy * 38 * scale;
      filledCapsule(rearX, rearY, barrel.x, barrel.y,
        barrelWidth * 1.8, gunColor);
      filledDisc(barrel.x, barrel.y, barrelWidth * 1.45, [104, 62, 48]);
    }
    if (firing) {
      const normalX = -pose.dy;
      const normalY = pose.dx;
      const flashA = projectPoint(
        pose.muzzle.x + pose.dx * 28 + normalX * 18,
        pose.muzzle.y + pose.dy * 28 + normalY * 18, pose.muzzle.z);
      const flashB = projectPoint(
        pose.muzzle.x + pose.dx * 28 - normalX * 18,
        pose.muzzle.y + pose.dy * 28 - normalY * 18, pose.muzzle.z);
      filledCapsule(barrel.x, barrel.y, flashA.x, flashA.y,
        Math.max(2, 5 * scale), [255, 248, 190]);
      filledCapsule(barrel.x, barrel.y, flashB.x, flashB.y,
        Math.max(2, 5 * scale), [255, 248, 190]);
    }
  }
  if (throwing || bashing) {
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
  const directions = [-2.92, -2.61, -2.3, -2.01, -1.72, -1.43,
    -1.14, -.83, -.52, -.21, .1, .41, .72, 1.03];
  for (let index = 0; index < directions.length; index++) {
    const angle = directions[index] + (player.pad ? .14 : -.14);
    const speed = 48 + index * 4;
    const fall = 10 * burstAge + 72 * burstAge * burstAge;
    for (let speck = 0; speck < 3; speck++) {
      const spread = .38 + speck * .29;
      const distance = (5 + speed * burstAge) * spread;
      const depth = (index % 3 - 1) * (3 + burstAge * 9) * spread;
      const point = projectPoint(
        headWorld.x + Math.cos(angle) * distance,
        headWorld.y + Math.sin(angle) * distance + fall * spread,
        headWorld.z + depth);
      if (![point.x, point.y, point.z].every(Number.isFinite) ||
          Math.abs(point.x) > 30000 || Math.abs(point.y) > 30000) continue;
      triangleDepth = point.z;
      const pixel = clamp((2.6 - speck * .48) * cameraScale(), 1.5, 4);
      screenRect(point.x - pixel / 2, point.y - pixel / 2,
        pixel, pixel, palette[(index + speck) % palette.length]);
    }
  }
  // The face becomes a small broken core rather than remaining an intact disc.
  const core = Math.max(2, headWorld.radius * cameraScale() *
    Math.max(.08, 1 - burstAge * 6.2));
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
  // A fighter who is down is still on the stage: they read as the broken body
  // rather than as nothing. There is no state in which a fighter is skipped.
  const headBurstAge = player.headBustedAt
    ? Math.max(0, (runtime().monotonicUs - player.headBustedAt) / 1000000) : -1;
  if (!player.alive && headBurstAge >= .11) {
    drawBrokenRunner(player, headBurstAge);
    if (["CRY", "WOE", "SULK"].includes(player.resultReaction)) {
      const head = player.frozenGeometry?.head;
      if (head) {
        const point = projectPoint(head.x, head.y, head.z);
        const sway = Math.sin(t * 11) * 5;
        for (const side of [-1, 1])
          filledCapsule(point.x + side * 7 + sway, point.y,
            point.x + side * 7 - sway, point.y + 34 + (t * 31) % 18,
            3, [112, 208, 255]);
      }
    }
    return;
  }
  if (player.fallenBodyGeometry) {
    const fallen = projectRunnerWorldGeometry(player.fallenBodyGeometry);
    drawSkeletonSegments(fallen.segments, player.color, [8, 12, 24], null);
  }
  const geometry = player.replayGeometry
    ? projectRunnerWorldGeometry(player.replayGeometry)
    : player.frozenGeometry
      ? projectRunnerWorldGeometry(player.frozenGeometry)
    : runnerGeometry(player, t);
  if (roundResult && player.resultReaction) {
    const age = (runtime().monotonicUs - player.resultReactionAt) / 1000000;
    const amount = player.resultReaction === "POSE" ||
      player.resultReaction === "SULK" ? 3 : 8;
    const dx = Math.sin(age * (player.resultReaction === "DANCE" ? 15 : 9)) * amount;
    const dy = player.resultReaction === "DANCE"
      ? -Math.abs(Math.sin(age * 9)) * 15
      : Math.sin(age * 12) * amount * .35;
    geometry.head.x += dx;
    geometry.head.y += dy;
    for (const segment of geometry.segments) {
      segment.x1 += dx; segment.x2 += dx;
      segment.y1 += dy; segment.y2 += dy;
    }
  }
  // Preserve the fighter's identity color during hit flash. A pure white body
  // disappeared against the daylight arena, so impact now changes only its rim.
  const color = player.color;
  const outline = player.hit > 0
    ? mixColor([255, 232, 92], [28, 34, 52], visualTheme.light)
    : [8, 12, 24];
  const displayNow = player.frozenAt || runtime().monotonicUs;
  if (player.skateboard) {
    const board = projectPoint(player.x, player.y + 5, player.z);
    const leftEdge = projectPoint(player.x - 72,
      terrainFloorAt(player.x - 72) + 5, player.z);
    const boardEdge = projectPoint(player.x + 72,
      terrainFloorAt(player.x + 72) + 5, player.z);
    const reach = Math.max(28, Math.abs(boardEdge.x - board.x));
    const rotation = Math.atan2(boardEdge.y - leftEdge.y,
      boardEdge.x - leftEdge.x);
    drawSkateboardSymbol(board, reach, rotation);
  }
  drawFighterSilhouette(geometry, color, outline, player);
  const hitNow = runtime().monotonicUs;
  if (player.hitSegment >= 0 && hitNow < player.hitSegmentUntil &&
      Math.floor(hitNow / 45000) % 2 === 0) {
    const segment = geometry.segments[player.hitSegment];
    if (segment) {
      filledCapsule(segment.x1, segment.y1, segment.x2, segment.y2,
        segment.width + Math.max(3, 5 * cameraScale()), [255, 238, 102]);
    }
  }
  if (shellMode !== "MENU" || titleTransitionAt !== null ||
      titleAttractMode === "action" || (player.npc && !player.bot))
    drawFace(player, geometry.head, contrastShadow(color), t, displayNow);
  drawInventory(player, displayNow);
  if (player.blocking) {
    const worldShield = shieldGeometry(player);
    const shield = projectPoint(worldShield.x, worldShield.y, worldShield.z);
    const radius = Math.max(18, worldShield.radius * cameraScale());
    const shieldColor = player.blockFlash > 0 ? [255, 255, 255] : player.color;
    const outerWidth = Math.max(4, 11 * cameraScale());
    filledRing(shield.x, shield.y, radius,
      Math.max(0, radius - outerWidth), shieldColor);
  }

}

function drawDiveMotion(player, t) {
  if (player.grounded || (!player.pounding && !player.pogoDive)) return;
  const level = player.pounding ? Math.max(1, player.poundLevel || 1) : 1;
  const speed = clamp(Math.abs(player.vy) / poundMaxVelocity, .25, 1.8);
  const color = mixColor(player.color,
    level === 3 ? [255, 80, 92] : level === 2 ? [255, 190, 62] : [255, 245, 196],
    .46 + level * .12);
  const count = 3 + level * 3;
  for (let index = 0; index < count; index++) {
    const spread = (index - (count - 1) / 2) * 24;
    const wobble = Math.sin(t * 13 + index * 2.1) * 9;
    const x = player.x + spread + wobble;
    const startY = player.y - 125 - index % 2 * 36;
    const endY = startY - (95 + index * 17) * speed;
    worldCapsule(x, startY, player.z + 18, x, endY, player.z + 18,
      3 + speed * 3 + level, color, .028);
  }
}

function drawDoubleJumpMotion(player, t) {
  const now = runtime().monotonicUs;
  if (now >= (player.doubleJumpLinesUntil || 0)) return;
  const life = clamp((player.doubleJumpLinesUntil - now) / 280000, 0, 1);
  const color = mixColor(player.color, [244, 250, 255], .55);
  for (let index = 0; index < 7; index++) {
    const spread = (index - 3) * 24 + Math.sin(t * 18 + index) * 5;
    const startY = player.y + 28 + (index % 2) * 18;
    const endY = startY + (70 + index * 11) * life;
    worldCapsule(player.x + spread, startY, player.z + 16,
      player.x + spread, endY, player.z + 16,
      3 + life * 3, color, .029);
  }
}

// Hitboxes are an inspector, not a garnish. They used to flash on every
// impact for anyone watching, which put green boxes over ordinary play and
// over every recording of it. VIEW (tab) is now the only thing that shows
// them — `impactHitboxesUntil` still times the flash, but only for someone
// who has already asked to see the geometry.
function drawDebugHitboxes(player, t) {
  const now = runtime().monotonicUs;
  const impactDebug = debugHitboxes && !roundResult && now < impactHitboxesUntil;
  if ((!debugHitboxes && !impactDebug) || (!player.alive && !roundResult)) return;
  const cinematicAge = deathCinematicAge(now);
  if (deathCinematic?.loserPad === player.pad && cinematicAge >= .11) return;
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

function visibleHandle(player) {
  return player.name.toLowerCase();
}

function nationFlag(country) {
  const code = String(country || "").toUpperCase();
  if (!/^[A-Z]{2}$/.test(code)) return "";
  return [...code].map((letter) => String.fromCodePoint(
    0x1f1e6 + letter.charCodeAt(0) - 65)).join("");
}

function playerHandleLayout(player, side) {
  const safe = hudSafeRect();
  const touch = typeof capabilities === "function" &&
    capabilities().inputFamily === "touch";
  const size = touch ? 24 : hudTypeSize;
  const width = handleWidth(visibleHandle(player), size);
  const phraseReserve = size * 8 * .68;
  const x = side === 0 ? safe.left + 8
    : safe.right - 8 - width - phraseReserve;
  const y = safe.bottom - size - (touch ? 250 : 18);
  return { x, y, size, width };
}

function drawFloatingHandle(player, x, y, size) {
  const handle = visibleHandle(player);
  const shadows = player.handleColors?.map(runShadow);
  drawHandle(handle, x + 3, y + 4, size, shadows, runShadow(player.color));
  drawHandle(handle, x, y, size, player.handleColors, player.color);
}

function drawPlayerHandle(player, t, side) {
  const { x, y, size } = playerHandleLayout(player, side);
  const handle = visibleHandle(player);
  const drawGlyphs = (dx, dy, colors, fallback) => {
    let cursor = x + dx;
    for (let index = 0; index < handle.length; index++) {
      const character = handle[index];
      typeWrite(character, cursor, y + dy, size,
        ...glyphColor(colors, index, fallback));
      cursor += comicGlyphAdvance(character, size);
    }
  };
  drawGlyphs(3, 4, player.handleColors?.map(runShadow), runShadow(player.color));
  drawGlyphs(0, 0, player.handleColors, player.color);
  const flag = nationFlag(player.nation);
  if (flag) {
    const flagSize = Math.round(size * .92);
    const flagX = side === 0 ? x : x + Math.max(0, handleWidth(handle, size));
    systemWrite(flag, flagX, y - flagSize * 1.05, flagSize, 255, 255, 255);
  }
}

function playerStatLines(player) {
  const pad = inputPads[player.pad] ||
    { connected: false, down: [], leftX: 0, leftY: 0 };
  const input = quantizedInput(pad, player.suppressedDirections);
  const animation = fighterAnimationPhase(player,
    player.frozenAt || runtime().monotonicUs);
  const slot = (value, width) => String(value).padStart(width, " ");
  if (player.npc && !player.bot) {
    const parts = player.spiderDummy
      ? [...spiderLegParts, "torso"] : [...limbParts, "torso"];
    const remaining = parts.filter((part) => hasPart(player, part)).length;
    const damage = parts.reduce((total, part) =>
      total + Number(player.partDamage?.[part] || 0), 0);
    return [
      "p" + (player.pad + 1) + " :: " +
        (player.spiderDummy ? "spider dummy" : "training dummy"),
      "target::inert parts[" + slot(remaining, 2) + "/" +
        slot(parts.length, 2) + "] dmg[" + slot(damage, 2) + "]",
      "anim::" + animation.state.padEnd(7, " ") + " step[" +
        slot(animation.step, 2) + "/" + slot(animation.steps, 2) + "] t[" +
        slot(animation.tick, 7) + "]",
    ];
  }
  return [
    "p" + (player.pad + 1) + " :: " + player.stance +
      (player.attackKind ? " + " + player.attackKind : ""),
    "in[" + slot(input.horizontal, 2) + "," + slot(input.vertical, 2) +
      "] -> stk[" + slot(pad.leftX.toFixed(2), 5) + "] vx[" +
      slot(Math.round(player.vx), 5) + "]",
    "anim::" + animation.state.padEnd(7, " ") + " step[" +
      slot(animation.step, 2) + "/" + slot(animation.steps, 2) + "] t[" +
      slot(animation.tick, 7) + "]",
  ];
}

const playerStatSize = () => compactLayout() ? 16 : 22;
const playerStatPanelHeight = () => {
  const size = playerStatSize();
  return 16 + 3 * (size + 7) + 10;
};

// The state trace owns a fixed stack over each handle without a container.
const statStackHeight = () => debugHitboxes ? playerStatPanelHeight() + 8 : 0;

function drawPlayerStats(player, side, t) {
  if (!debugHitboxes) return;
  const lines = playerStatLines(player);
  const animation = fighterAnimationPhase(player,
    player.frozenAt || runtime().monotonicUs);
  const size = playerStatSize();
  const padding = 8;
  const lineHeight = size + 7;
  const safe = hudSafeRect();
  // A diagnostic instrument has fixed columns: changing signs and tick counts
  // must not make its chassis breathe every frame.
  // The tick and velocity columns are the longest legal row, so the chassis
  // reserves their full Xbox-width footprint instead of clipping their tail.
  const width = compactLayout() ? 360 : 520;
  const height = playerStatPanelHeight();
  const handle = playerHandleLayout(player, side);
  const x = side === 0 ? safe.left : safe.right - width;
  const y = handle.y - height - 12;
  const previousDepth = triangleDepth;
  triangleDepth = -1.445;
  for (let row = 0; row < lines.length; row++) {
    const rowY = y + padding + row * lineHeight;
    writeHudLine(lines[row], x + padding, rowY, size);
  }
  triangleDepth = previousDepth;
}

function drawHudInventory(player, side) {
  const items = [];
  if (player.gunAmmo > 0) items.push("gun " + player.gunAmmo);
  if (player.grenadeAmmo > 0) items.push("grenade " + player.grenadeAmmo);
  if (!items.length) return;
  const text = items.join("  ");
  const handle = playerHandleLayout(player, side);
  const size = Math.round(hudTypeSize * .62);
  const width = handleWidth(text, size);
  const x = side === 0 ? handle.x : handle.x + handle.width - width;
  const y = handle.y - statStackHeight() - size - 7;
  const shadow = contrastShadow(player.color);
  typeWrite(text, x + 2, y + 3, size, ...shadow);
  typeWrite(text, x, y, size, ...player.color);
}

function spatialHudPlayers() {
  const left = players[hudLeftPad] || players[0];
  const other = players[left.pad === 0 ? 1 : 0];
  // Preserve the lane through the instant of overlap, then swap once the
  // crossing is visually unambiguous. This prevents one-frame HUD chatter.
  if (other.x < left.x - 36) hudLeftPad = other.pad;
  return [players[hudLeftPad], players[hudLeftPad === 0 ? 1 : 0]];
}

// How present one glyph of the buffer is. `settle` is how far into the idle
// the dissolve has travelled: 0 for as long as the pad is still saying
// something, 1 once the whole stream has left. Doubling it and offsetting by
// each glyph's place in the stream spends the first half on the oldest glyph
// and the last half on the newest, so the buffer leaves in reading order
// rather than dimming as one sheet.
function commandFade(index, count, idle) {
  const settle = clamp((idle - commandHoldUs) / commandFadeUs, 0, 1);
  return clamp(index / Math.max(1, count - 1) + 1 - settle * 2, 0, 1);
}

function drawCommandStream(player, side) {
  const glyph = { LEFT: "<", RIGHT: ">", UP: "^", DOWN: "v" };
  const buttonFor = { LEFT: "ArrowLeft", RIGHT: "ArrowRight",
    UP: "ArrowUp", DOWN: "ArrowDown", "/": "A", "*": "B", ")": "X",
    "+": ["Y", "LeftShoulder", "RightShoulder"] };
  const now = runtime().monotonicUs;
  const idle = now - (player.commandStream.at(-1)?.at || now);
  const count = player.commandStream.length;
  const held = inputPads[player.pad]?.down || [];
  const entries = player.commandStream.map((entry, index) => ({ ...entry,
    text: glyph[entry.label] || entry.label,
    fade: commandFade(index, count, idle),
    held: Array.isArray(buttonFor[entry.label])
      ? buttonFor[entry.label].some((button) => held.includes(button))
      : held.includes(buttonFor[entry.label]),
  })).filter((entry) => entry.fade > .01);
  // A held button lights only its newest appearance — not every earlier
  // press of the same button still sitting in the buffer's history.
  const claimed = new Set();
  for (let index = entries.length - 1; index >= 0; index--) {
    const entry = entries[index];
    if (!entry.held) continue;
    if (claimed.has(entry.label)) entry.held = false;
    else claimed.add(entry.label);
  }
  if (!entries.length) return;
  // Commands emerge immediately to the right of the handle. Eight glyphs form
  // a phrase; completed phrases rise as a unit while a fresh phrase takes the
  // handle lane beneath it.
  entries.reverse();
  const handle = playerHandleLayout(player, side);
  const size = handle.size;
  const lineEntries = entries.slice(0, commandStreamDepth);
  const gap = Math.round(size * .34);
  const rows = [];
  for (let index = 0; index < lineEntries.length; index += 8)
    rows.push(lineEntries.slice(index, index + 8));
  const safe = hudSafeRect();
  for (let row = 0; row < rows.length; row++) {
    const rowEntries = rows[row];
    const width = rowEntries.reduce((sum, entry, index) => sum +
      handleWidth(entry.text, size) + (index ? gap : 0), 0);
    let cursor = handle.x + handle.width + 12;
    const newestAt = Math.max(...rowEntries.map((entry) => entry.at));
    const rise = clamp((now - newestAt) / 900000, 0, 1) * size * .8;
    const y = handle.y - statStackHeight() - row * (size + 7) - rise;
    for (const entry of rowEntries) {
      const quiet = mixColor([104, 114, 136], [82, 90, 108], visualTheme.light);
      const live = entry.held ? player.color : quiet;
      const glyphInk = entry.fade >= 1 ? live
        : mixColor(visualTheme.light > .5 ? [230, 239, 247] : [7, 8, 28],
          live, entry.fade);
      typeWrite(entry.text, cursor + 2, y + 3, size,
        ...contrastShadow(glyphInk));
      typeWrite(entry.text, cursor, y, size, ...glyphInk);
      cursor += handleWidth(entry.text, size) + gap;
    }
  }
}

function drawFightIntro(introSeconds, titleInk, statusShadow) {
  const centerX = viewCenterX();
  const centerY = (stageTop + stageBottom) / 2;
  const touch = typeof capabilities === "function" &&
    capabilities().inputFamily === "touch";
  const nameSize = touch ? 32 : compactLayout() ? 48 : 74;
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
  const pairGap = compactLayout() ? 320 - 30 : 320;
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

// A pole or a branch crossing the near plane has the same problem a face does,
// and the same cure — trim the segment at the plane rather than letting the
// far end pin itself to the near distance and rake across the screen. Null is
// a segment wholly behind the camera.
// Trimmed at the pin rather than the near plane: a line is handed to the host
// whole, with no band to catch it, so a pole crossing the plane would draw a
// streak the length of the guard band. Pulling the cut back to the pin costs
// nothing visible — nothing in the arena passes within eighty units of the
// lens — and keeps both ends of every line on the map.
function worldSegment(x1, y1, z1, x2, y2, z2) {
  let a = cameraDoll.toView({ x: x1, y: y1, z: z1 });
  let b = cameraDoll.toView({ x: x2, y: y2, z: z2 });
  if (a.z < cameraPin && b.z < cameraPin) return null;
  if (a.z < cameraPin)
    a = mixVertex(a, b, (cameraPin - a.z) / (b.z - a.z));
  else if (b.z < cameraPin)
    b = mixVertex(b, a, (cameraPin - b.z) / (a.z - b.z));
  const from = cameraDoll.projectView(a);
  const to = cameraDoll.projectView(b);
  return [from.x, from.y, from.z, to.x, to.y, to.z].every(Number.isFinite)
    ? { from, to } : null;
}

function worldLine(x1, y1, z1, x2, y2, z2, width, color) {
  const segment = worldSegment(x1, y1, z1, x2, y2, z2);
  if (!segment) return;
  line(segment.from.x, segment.from.y, segment.to.x, segment.to.y,
    width, ...color);
}

function worldCapsule(x1, y1, z1, x2, y2, z2, width, color,
    depthBias = -.004) {
  const segment = worldSegment(x1, y1, z1, x2, y2, z2);
  if (!segment) return;
  const { from, to } = segment;
  const previousDepth = triangleDepth;
  triangleDepth = Math.min(from.z, to.z) + depthBias;
  filledCapsule(from.x, from.y, to.x, to.y, width, color);
  triangleDepth = previousDepth;
}

function worldQuad(a, b, c, d, color) {
  // Lighting is decided in world space, off the surface the quad names, so it
  // is the same shade however the clipper ends up cutting the face up.
  const ab = { x: b.x - a.x, y: b.y - a.y, z: b.z - a.z };
  const ac = { x: c.x - a.x, y: c.y - a.y, z: c.z - a.z };
  const normal = normalize3(cross3(ab, ac));
  const light = { x: -globalLight.x, y: -globalLight.y, z: -globalLight.z };
  const illumination = .72 + Math.max(0, dot3(normal, light)) * .28;
  const lit = color.map((channel) => Math.round(channel * illumination));
  worldTriangle(a, b, c, lit);
  worldTriangle(a, c, d, lit);
}

function drawTerrainSurface(left, right, near, far, color) {
  const step = (right - left) / terrainSamples;
  for (let index = 0; index < terrainSamples; index++) {
    const x1 = left + index * step;
    const x2 = index === terrainSamples - 1 ? right : x1 + step;
    const y1 = terrainFloorAt(x1);
    const y2 = terrainFloorAt(x2);
    const slope = Math.abs(y2 - y1) / Math.max(1, step);
    const grain = .5 + .5 * Math.sin(index * 12.9898 + terrainPhase * 4.17);
    const earthy = mixColor([75, 91, 68], [205, 194, 156], grain * .34);
    const shade = mixColor(color, earthy, .08 + slope * .22 + grain * .16);
    worldQuad(
      { x: x1, y: y1, z: near }, { x: x2, y: y2, z: near },
      { x: x2, y: y2, z: far }, { x: x1, y: y1, z: far }, shade);
  }
}

function drawTerrainFrontWall(left, right, near, color) {
  // Close the near edge with a terrain-following skirt so camera pitch never
  // exposes the clear layer beneath the floor.
  const step = (right - left) / terrainSamples;
  const wallBottom = floorY + 720;
  for (let index = 0; index < terrainSamples; index++) {
    const x1 = left + index * step;
    const x2 = index === terrainSamples - 1 ? right : x1 + step;
    const y1 = terrainFloorAt(x1);
    const y2 = terrainFloorAt(x2);
    const grain = .5 + .5 * Math.sin(index * 9.71 + terrainPhase * 3.13);
    const wall = mixColor(color, [63, 54, 46], .3 + grain * .12);
    worldQuad(
      { x: x1, y: y1, z: near - 2 },
      { x: x2, y: y2, z: near - 2 },
      { x: x2, y: wallBottom, z: near - 2 },
      { x: x1, y: wallBottom, z: near - 2 }, wall);
  }
}

function drawRoomSurfaces(left, right, top, bottom, color) {
  const columns = 6;
  const rows = 2;
  for (let row = 0; row < rows; row++) {
    for (let column = 0; column < columns; column++) {
      const x1 = lerp(left, right, column / columns);
      const x2 = lerp(left, right, (column + 1) / columns);
      // Bury the wall slightly into the floor. Shared coplanar edges can expose
      // a one-pixel clear-color crescent after perspective rasterization.
      const floor1 = terrainFloorAt(x1) + 120;
      const floor2 = terrainFloorAt(x2) + 120;
      const y1Left = lerp(top, floor1, row / rows);
      const y1Right = lerp(top, floor2, row / rows);
      const y2Left = lerp(top, floor1, (row + 1) / rows);
      const y2Right = lerp(top, floor2, (row + 1) / rows);
      const grain = .5 + .5 * Math.sin(column * 17.7 + row * 43.1 +
        terrainPhase * 2.4);
      const plaster = mixColor([255, 198, 151], [198, 146, 184], grain);
      const checker = (row + column) % 2 ? .12 : .06;
      const shade = mixColor(color, plaster, checker + grain * .1);
      worldQuad({ x: x1, y: y1Left, z: worldFar },
        { x: x2, y: y1Right, z: worldFar },
        { x: x2, y: y2Right, z: worldFar },
        { x: x1, y: y2Left, z: worldFar }, shade);
    }
  }
  // The left side is a real wall, but only occupies the rear half of the room.
  // Segmenting it keeps the diorama corner visible without recreating the old
  // full-depth opaque slab that could pass in front of the camera and blackout
  // the fight during an orbit.
  const sideFloor = terrainFloorAt(left) + 120;
  for (let row = 0; row < 2; row++) {
    for (let depth = 0; depth < 2; depth++) {
      const y1 = lerp(top, sideFloor, row / 2);
      const y2 = lerp(top, sideFloor, (row + 1) / 2);
      const z1 = lerp(0, worldFar, depth / 2);
      const z2 = lerp(0, worldFar, (depth + 1) / 2);
      const pigment = (row + depth) % 2 ? [218, 151, 163] : [247, 188, 143];
      const shade = mixColor(color, pigment, .13);
      worldQuad({ x: left, y: y1, z: z1 },
        { x: left, y: y1, z: z2 },
        { x: left, y: y2, z: z2 },
        { x: left, y: y2, z: z1 }, shade);
    }
  }
  const edge = mixColor(color, [64, 78, 72], .24);
  worldQuad({ x: left, y: ceilingY, z: worldNear },
    { x: left, y: ceilingY, z: worldFar },
    { x: right, y: ceilingY, z: worldFar },
    { x: right, y: ceilingY, z: worldNear }, edge);
  worldLine(worldLeft, top, worldFar - 4, worldLeft, bottom, worldFar - 4,
    9, edge);
  worldLine(worldRight, top, worldFar - 4, worldRight, bottom, worldFar - 4,
    9, edge);
}

function drawTerrainGrass(left, right, color) {
  // Twenty deterministic tufts cost forty native lines total. Keeping them on
  // the line layer avoids turning background dressing into hundreds of faces.
  const count = 20;
  for (let index = 0; index < count; index++) {
    const seed = .5 + .5 * Math.sin(index * 91.73 + terrainPhase * 2.31);
    const x = lerp(left, right, (index + .5) / count);
    // Keep dressing in the near half of the floor. At the far plane the same
    // blades collapse into horizon ticks instead of reading as foreground grass.
    const z = lerp(worldNear * .88, worldNear * .22,
      .5 + .5 * Math.sin(index * 37.11 + terrainPhase));
    const y = terrainFloorAt(x) - 2;
    const height = 22 + seed * 22;
    const tuft = mixColor(color, [48, 92, 54], .42 + seed * .22);
    worldLine(x, y, z, x - 8 - seed * 5, y - height, z, 3, tuft);
    worldLine(x, y, z, x + 7 + seed * 6, y - height * .82, z, 3, tuft);
  }
}

function drawBoosterPad(t) {
  const pulse = .5 + .5 * Math.sin(t * 4);
  const color = mixColor([55, 174, 244], [255, 224, 74], pulse * .55);
  for (const boosterX of boosterXs) {
    const y = terrainFloorAt(boosterX) - 7;
    worldQuad(
      { x: boosterX - boosterRadius, y, z: -86 },
      { x: boosterX + boosterRadius, y, z: -86 },
      { x: boosterX + boosterRadius, y, z: 86 },
      { x: boosterX - boosterRadius, y, z: 86 }, color);
  }
}

function drawSkyAtmosphere(sky, arena) {
  // Broad D2D bands are effectively free compared with projected meshes and
  // make the sky feel spatial without rebuilding a textured skybox each frame.
  const bands = 6;
  const bottom = Math.round(viewHeight * .78);
  for (let band = 0; band < bands; band++) {
    const y1 = Math.round(bottom * band / bands);
    const y2 = Math.round(bottom * (band + 1) / bands);
    const amount = (band + .5) / bands;
    const color = mixColor(sky, arena, amount * .22);
    box(0, y1, viewWidth(), y2 - y1 + 1, ...color);
  }
  const haze = mixColor(sky, arena, .34);
  for (let streak = 0; streak < 11; streak++) {
    const seed = .5 + .5 * Math.sin(streak * 41.37 + terrainPhase * 1.9);
    const x = (streak + .18 + seed * .6) / 11 * viewWidth();
    const y = 88 + (.5 + .5 * Math.sin(streak * 19.17 - terrainPhase)) *
      viewHeight * .43;
    const reach = 32 + seed * 86;
    line(x - reach, y, x + reach, y + (seed - .5) * 7,
      2 + seed * 2, ...haze);
  }
}

function shadowSurfaceY(x, y) {
  return PLATFORM && x >= platformLeft && x <= platformRight && y <= platformY + 1
    ? platformY : terrainFloorAt(x);
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
  const radiusX = Math.max(4, Math.abs(edge.x - center.x) *
    (.5 + .18 * focus));
  const radiusY = Math.max(2, radiusX * (.2 + .08 * focus));
  // Bind the shadow to the owning object's depth, then bias it away from the
  // camera. It remains above the terrain pass but can never win against the
  // object that casts it. Restore the depth after — this used to leak, and
  // everything drawn until the next assignment inherited the last shadow
  // caster's depth: nondeterministic layering, frame to frame, on console.
  const previousDepth = triangleDepth;
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
  triangleDepth = previousDepth;
}

function projectedBallRadius(ball) {
  const center = projectPoint(ball.x, ball.y, ball.z);
  const edge = projectPoint(ball.x + ball.radius, ball.y, ball.z);
  return Math.max(8, Math.abs(edge.x - center.x));
}

function drawSkateboardSymbol(point, radius, rotation = 0) {
  const alongX = Math.cos(rotation) * radius;
  const alongY = Math.sin(rotation) * radius;
  const normalX = -Math.sin(rotation);
  const normalY = Math.cos(rotation);
  const deck = [236, 76, 118];
  const edge = [34, 25, 39];
  const truck = [174, 184, 202];
  filledCapsule(point.x - alongX * .86, point.y - alongY * .86,
    point.x + alongX * .86, point.y + alongY * .86,
    Math.max(8, radius * .22), edge);
  filledCapsule(point.x - alongX * .82, point.y - alongY * .82,
    point.x + alongX * .82, point.y + alongY * .82,
    Math.max(5, radius * .14), deck);
  for (const direction of [-1, 1]) {
    const truckX = point.x + alongX * direction * .56;
    const truckY = point.y + alongY * direction * .56;
    filledCapsule(truckX - normalX * radius * .22,
      truckY - normalY * radius * .22,
      truckX + normalX * radius * .22,
      truckY + normalY * radius * .22, Math.max(2, radius * .055), truck);
    for (const side of [-1, 1])
      filledDisc(truckX + normalX * radius * side * .26,
        truckY + normalY * radius * side * .26,
        Math.max(3, radius * .09), [22, 25, 34]);
  }
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
  const beach = ball.type === "beach";
  const skateboard = ball.type === "skateboard";
  const polygon = (x, y, polygonRadius, sides, rotation, color) => {
    for (let side = 0; side < sides; side++) {
      const a = rotation + side * Math.PI * 2 / sides;
      const b = rotation + (side + 1) * Math.PI * 2 / sides;
      screenTriangle(x, y,
        x + Math.cos(a) * polygonRadius,
        y + Math.sin(a) * polygonRadius,
        x + Math.cos(b) * polygonRadius,
        y + Math.sin(b) * polygonRadius, ...color);
    }
  };
  if (skateboard) {
    drawSkateboardSymbol(point, radius, ball.rotation);
    return;
  }
  if (beach) {
    const panels = [
      [244, 66, 96], [255, 146, 52], [255, 226, 66], [72, 210, 126],
      [54, 196, 224], [70, 112, 238], [184, 82, 224],
    ];
    const palettePhase = runtime().monotonicUs / 850000;
    const paletteStep = Math.floor(palettePhase);
    const paletteMix = palettePhase - paletteStep;
    const sides = 16;
    for (let side = 0; side < sides; side++) {
      const a = ball.rotation + side * Math.PI * 2 / sides;
      const b = ball.rotation + (side + 1) * Math.PI * 2 / sides;
      // Native monotonic time can read negative, and a negative remainder
      // would index off the front of the palette.
      const panel = ((Math.floor(side / 2) + paletteStep) % panels.length +
        panels.length) % panels.length;
      const panelColor = mixColor(panels[panel],
        panels[(panel + 1) % panels.length], paletteMix);
      screenTriangle(point.x, point.y,
        point.x + Math.cos(a) * radius * .92,
        point.y + Math.sin(a) * radius * .92,
        point.x + Math.cos(b) * radius * .92,
        point.y + Math.sin(b) * radius * .92,
        ...panelColor);
    }
    filledDisc(point.x, point.y, radius * .13, [246, 241, 220]);
    return;
  }
  filledDisc(point.x, point.y, radius * .92,
    soccer ? [226, 232, 224] : [232, 104, 28]);
  const seam = [42, 31, 29];
  if (soccer) {
    polygon(point.x, point.y, radius * .22, 5,
      ball.rotation - Math.PI / 2, seam);
    for (let patch = 0; patch < 5; patch++) {
      const angle = ball.rotation + patch * Math.PI * 2 / 5;
      const patchX = point.x + Math.cos(angle) * radius * .58;
      const patchY = point.y + Math.sin(angle) * radius * .58;
      filledCapsule(point.x + Math.cos(angle) * radius * .2,
        point.y + Math.sin(angle) * radius * .2,
        patchX, patchY, Math.max(1.5, radius * .035), seam);
      polygon(patchX, patchY, radius * .13, 5,
        angle - Math.PI / 2, seam);
    }
  } else {
    const width = Math.max(2, radius * .075);
    for (const angle of [ball.rotation, ball.rotation + Math.PI / 2]) {
      const dx = Math.cos(angle) * radius * .84;
      const dy = Math.sin(angle) * radius * .84;
      filledCapsule(point.x - dx, point.y - dy,
        point.x + dx, point.y + dy, width, seam);
    }
    for (const direction of [-1, 1]) {
      let previous = null;
      for (let step = -6; step <= 6; step++) {
        const y = step / 6 * radius * .82;
        const x = direction * Math.cos(step / 6 * Math.PI / 2) * radius * .42;
        const next = { x: point.x + x, y: point.y + y };
        if (previous) filledCapsule(previous.x, previous.y,
          next.x, next.y, width, seam);
        previous = next;
      }
    }
    filledDisc(point.x, point.y, Math.max(1.5, width * .55), seam);
  }
}

function drawBallHitboxes() {
  if (!debugHitboxes) return;
  for (const item of balls) {
    if (!item.active) continue;
    const point = projectPoint(item.x, item.y, item.z);
    const radius = projectedBallRadius(item);
    if (![point.x, point.y, radius].every(Number.isFinite) || radius > 5000 ||
        Math.abs(point.x) > 30000 || Math.abs(point.y) > 30000) continue;
    filledRing(point.x, point.y, radius + 5, radius + 2, [58, 222, 255]);
  }
}

function drawGunPickup(pickup, t) {
  if (!pickup.active) return;
  const bobY = pickup.y + Math.sin(t * 3 + pickup.x * .001) * 8;
  const scale = cameraScale();
  const metal = mixColor([202, 212, 228], [52, 59, 72], visualTheme.light);
  const grip = mixColor([126, 106, 88], [40, 43, 52], visualTheme.light);
  const barrelWidth = Math.max(2, 5 * scale);
  const gripWidth = Math.max(2, 4 * scale);
  const smg = pickup.kind === "RUBBER SMG";
  const rocket = pickup.kind === "ROCKET LAUNCHER";
  // A handgun-sized world object in gunmetal and grip material, not a glowing
  // pickup glyph with a second silhouette around it.
  // Capsules, not line() — the native renderer buries the whole line
  // stratum beneath the world's triangles, which is how the gun spent a day
  // drawn under the floor on console while the web shell showed it fine.
  worldCapsule(pickup.x - (rocket ? 58 : smg ? 42 : 16), bobY, pickup.z,
    pickup.x + (rocket ? 62 : smg ? 48 : 20), bobY, pickup.z,
    rocket ? barrelWidth * 2.2 : smg ? barrelWidth * 1.45 : barrelWidth,
    rocket ? [48, 61, 52] : metal, -.02);
  worldCapsule(pickup.x + (smg ? 8 : 2), bobY + 1, pickup.z,
    pickup.x + (smg ? 13 : 8), bobY + (smg ? 27 : 18), pickup.z,
    smg ? gripWidth * 1.5 : gripWidth, grip, -.02);
  if (smg) {
    worldCapsule(pickup.x - 42, bobY, pickup.z,
      pickup.x - 65, bobY + 18, pickup.z, barrelWidth, grip, -.02);
    worldCapsule(pickup.x - 6, bobY + 4, pickup.z,
      pickup.x - 1, bobY + 30, pickup.z, gripWidth * 1.4, metal, -.02);
  }
  if (rocket) {
    worldCapsule(pickup.x + 50, bobY, pickup.z,
      pickup.x + 68, bobY, pickup.z, barrelWidth * 2.8, [104, 62, 48], -.02);
    worldCapsule(pickup.x - 8, bobY + 3, pickup.z,
      pickup.x - 3, bobY + 30, pickup.z, gripWidth * 1.5, [38, 45, 42], -.02);
  }
}

function drawBullet(bullet) {
  const previous = projectPoint(bullet.previousX ?? bullet.x,
    bullet.previousY ?? bullet.y, bullet.z);
  const point = projectPoint(bullet.x, bullet.y, bullet.z);
  const blink = Math.floor(runtime().monotonicUs / 65000 + bullet.owner) % 2;
  const core = blink
    ? [255, 255, 248]
    : bullet.spit ? bullet.heavy ? [255, 92, 174] : [118, 255, 196]
    : bullet.rubber ? [255, 226, 58] : [255, 178, 76];
  const trail = blink
    ? [255, 244, 178]
    : bullet.spit ? bullet.heavy ? [182, 48, 116] : [58, 190, 132]
    : bullet.rubber ? [214, 178, 42] : [224, 116, 62];
  filledCapsule(previous.x, previous.y, point.x, point.y,
    Math.max(2, (bullet.rubber ? 4 : 3) * cameraScale()), trail);
  filledDisc(point.x, point.y, Math.max(4, 7 * cameraScale()), core);
}

function drawGrenadePickup(pickup, t) {
  if (!pickup.active) return;
  const bobY = pickup.y + Math.sin(t * 3.2 + pickup.x * .001) * 8;
  const point = projectPoint(pickup.x, bobY, pickup.z);
  const scale = cameraScale();
  const shell = mixColor([166, 194, 112], [72, 96, 58], visualTheme.light);
  const fuse = mixColor([210, 218, 232], [45, 50, 60], visualTheme.light);
  const radius = Math.max(3, 9 * scale);
  const previousDepth = triangleDepth;
  triangleDepth = point.z - .02;
  filledDisc(point.x, point.y, radius, shell);
  triangleDepth = previousDepth;
  worldCapsule(pickup.x + 1, bobY - 10, pickup.z,
    pickup.x + 11, bobY - 20, pickup.z, Math.max(2, 3 * scale), fuse, -.02);
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
  if (grenade.rocket) {
    const length = Math.hypot(grenade.vx, grenade.vy) || 1;
    const tail = projectPoint(grenade.x - grenade.vx / length * 54,
      grenade.y - grenade.vy / length * 54, grenade.z);
    filledCapsule(tail.x, tail.y, point.x, point.y,
      Math.max(5, 13 * cameraScale()), [52, 65, 56]);
    filledDisc(point.x, point.y, Math.max(5, 12 * cameraScale()), [190, 74, 54]);
    const flame = projectPoint(grenade.x - grenade.vx / length * 88,
      grenade.y - grenade.vy / length * 88, grenade.z);
    filledCapsule(flame.x, flame.y, tail.x, tail.y,
      Math.max(3, 7 * cameraScale()), [255, 214, 72]);
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
  const poleZ = 480;
  const poleBottom = platformY;
  const poleTop = platformY - 150;
  const calm = windMph < .35;
  const flagHeight = 68;
  const length = calm ? 58 : 78 + windMph * 7;
  const gust = calm ? 0
    : Math.sin(t * (4 + windMph * .16)) * (10 + windMph * .8);
  const tipX = poleX + windDirection * length;
  const tipY = poleTop + flagHeight * .5 + gust;
  const width = Math.max(3, 13 * cameraScale());
  const poleInk = mixColor([214, 222, 236], [50, 57, 70], visualTheme.light);
  const edgeInk = mixColor([242, 246, 252], [26, 31, 44], visualTheme.light);
  const fabric = calm
    ? mixColor([92, 205, 255], [35, 112, 190], visualTheme.light) : color;
  const flagPoints = [
    projectPoint(poleX, poleTop, poleZ),
    projectPoint(tipX, tipY, poleZ),
    projectPoint(poleX, poleTop + flagHeight, poleZ),
  ];
  if (flagPoints.every((point) => [point.x, point.y, point.z].every(Number.isFinite)))
    projectedTriangle(flagPoints[0], flagPoints[1], flagPoints[2], fabric);
  worldCapsule(poleX, poleBottom, poleZ,
    poleX, poleTop, poleZ, width, poleInk);
  const edgeWidth = Math.max(2, width * .34);
  worldCapsule(poleX, poleTop, poleZ,
    tipX, tipY, poleZ, edgeWidth, edgeInk);
  worldCapsule(tipX, tipY, poleZ,
    poleX, poleTop + flagHeight, poleZ, edgeWidth, edgeInk);
  worldCapsule(poleX, poleTop + flagHeight, poleZ,
    poleX, poleTop, poleZ, edgeWidth, edgeInk);
  worldCapsule(poleX - 35, poleBottom, poleZ,
    poleX + 35, poleBottom, poleZ, width, poleInk);
}

function hashUnit(text) {
  let hash = 2166136261;
  for (let cursor = 0; cursor < text.length; cursor++) {
    hash ^= text.charCodeAt(cursor);
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0) / 4294967295;
}

function airSeedValue(index, channel = 0) {
  return hashUnit("oskiewar-air:" + index + ":" + channel);
}

function resetAirParticles() {
  const count = 18;
  const spanX = worldRight - worldLeft;
  const spanY = floorY - ceilingY;
  const spanZ = worldFar - worldNear;
  airParticles.length = 0;
  for (let index = 0; index < count; index++)
    airParticles.push({
      id: "air:" + index,
      kind: "air",
      position: {
        x: worldLeft + airSeedValue(index, 1) * spanX,
        y: ceilingY + 180 + airSeedValue(index, 2) * (spanY - 360),
        z: worldNear + 100 + airSeedValue(index, 3) * (spanZ - 200),
      },
      velocity: { x: 0, y: 0, z: 0 },
      radius: 8 + airSeedValue(index, 4) * 14,
      phase: airSeedValue(index, 5) * Math.PI * 2,
    });
}

// A stream-function curl gives the air a divergence-free-looking circulation
// instead of independent sine wiggles. Entities relax toward this velocity,
// advect in fixed simulation time, and wrap at arena boundaries.
function airFlowAt(position, seconds, phase) {
  const nx = (position.x - worldLeft) / (worldRight - worldLeft) * Math.PI * 2;
  const ny = (position.y - ceilingY) / (floorY - ceilingY) * Math.PI * 2;
  const age = seconds * .32 + phase;
  const circulation = 96;
  return {
    x: windDirection * (42 + windMph * 12) +
      circulation * Math.sin(nx + age) * Math.cos(ny - age * .7),
    y: -circulation * Math.cos(nx + age) * Math.sin(ny - age * .7),
    z: 44 * Math.sin(nx + ny + age * .55),
  };
}

const wrapWorld = (value, low, high) => {
  const span = high - low;
  return low + ((value - low) % span + span) % span;
};

function simulateAirParticles(dt, now) {
  if (!airParticles.length) resetAirParticles();
  const seconds = now / 1000000;
  const response = 1 - Math.exp(-dt * 2.8);
  for (const entity of airParticles) {
    const flow = airFlowAt(entity.position, seconds, entity.phase);
    entity.velocity.x = lerp(entity.velocity.x, flow.x, response);
    entity.velocity.y = lerp(entity.velocity.y, flow.y, response);
    entity.velocity.z = lerp(entity.velocity.z, flow.z, response);
    entity.position.x = wrapWorld(entity.position.x + entity.velocity.x * dt,
      worldLeft, worldRight);
    entity.position.y = wrapWorld(entity.position.y + entity.velocity.y * dt,
      ceilingY + 120, floorY - 120);
    entity.position.z = wrapWorld(entity.position.z + entity.velocity.z * dt,
      worldNear + 80, worldFar - 80);
  }
}

function drawAmbientMotes(color) {
  const previousDepth = triangleDepth;
  for (const entity of airParticles) {
    const { x, y, z } = entity.position;
    const point = projectPoint(x, y, z);
    const edge = projectPoint(x + entity.radius, y, z);
    const radius = Math.max(.8, Math.hypot(edge.x - point.x, edge.y - point.y));
    const depthAmount = (z - worldNear) / (worldFar - worldNear);
    const ink = mixColor([18, 24, 48], color, .24 + (1 - depthAmount) * .34);
    if (![point.x, point.y, point.z, radius].every(Number.isFinite) ||
        point.x < stageLeft - radius || point.x > stageRight + radius ||
        point.y < -radius || point.y > viewHeight + radius) continue;
    triangleDepth = point.z;
    filledDisc(point.x, point.y, radius, ink);
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

function selectionPreview(option) {
  return { ...players[1], name: option.fighter.handle,
    color: option.fighter.color,
    npc: option.kind === "dummy" || option.kind === "bot",
    bot: option.kind === "bot" };
}

function drawSelectionScreen(t, ink, panel) {
  const layout = selectionTouchLayout();
  const hover = selectionHover(layout);
  if (globalThis.__oskiewarTouch) globalThis.__oskiewarTouch.hover = hover;
  const compact = compactLayout();
  const title = selectionStep === 0 ? "pick your pal" : "who are you fighting?";
  const titleSize = compact ? 36 : 58;
  const titleY = compact ? 58 : 86;
  const titleWidth = handleWidth(title, titleSize);
  typeWrite(title, viewCenterX() - titleWidth / 2, titleY, titleSize, ...ink);
  const stepText = (selectionStep + 1) + "/2";
  const stepSize = compact ? 24 : 32;
  typeWrite(stepText, layout.back.x + layout.back.width + (compact ? 18 : 28),
    layout.back.y + (compact ? 11 : 18), stepSize,
    ...mixColor([140, 150, 185], ink, .35));
  const backHovered = hover?.back;
  const options = selectionOptions();
  // Neighbours first so the focused card always lands on top of them.
  const wheel = [...layout.options].sort((a, b) =>
    Math.abs(b.slot) - Math.abs(a.slot));
  for (const rect of wheel) {
    const option = options[rect.index];
    const player = selectionPreview(option);
    const disabled = Boolean(option.disabled);
    const hovered = !disabled && hover?.option === rect.index;
    const focused = !disabled && rect.slot === 0;
    const disabledInk = mixColor([74, 80, 96], [164, 168, 178],
      visualTheme.light);
    const optionColor = disabled ? disabledInk : option.fighter.color;
    if (hovered || focused)
      box(rect.x, rect.y + rect.height - (hovered ? 8 : 5), rect.width,
        hovered ? 8 : 5, ...optionColor);
    const scale = rect.height / 520 * 1.05;
    const portraitY = rect.y + rect.height * (compact ? .55 : .53);
    drawSelectPortrait(player, rect.x + rect.width / 2, portraitY, scale, t);
    const label = option.fighter.handle.toLowerCase();
    const labelSize = Math.max(16, Math.round(rect.width * .085));
    const labelWidth = handleWidth(label, labelSize);
    drawHandle(label, rect.x + (rect.width - labelWidth) / 2,
      rect.y + rect.height - labelSize - (compact ? 10 : 18), labelSize,
      disabled ? [disabledInk] : option.fighter.colors, optionColor);
  }
  // Chevrons inside the focused card name the gesture that turns the wheel,
  // without crowding the neighbours they point at.
  if (layout.options.length > 1) {
    const focus = layout.options.find((rect) => rect.slot === 0);
    const arrowSize = compact ? 30 : 52;
    const arrowY = focus.y + focus.height / 2 - arrowSize / 2;
    const inset = (compact ? 8 : 14) + Math.sin(t * 3.4) * (compact ? 3 : 6);
    typeWrite("<", focus.x + inset, arrowY, arrowSize, ...ink);
    typeWrite(">", focus.x + focus.width - inset -
      comicGlyphAdvance(">", arrowSize), arrowY, arrowSize, ...ink);
  }
  // Keep navigation above every option on narrow/tall browser layouts.
  box(layout.back.x, layout.back.y, layout.back.width, layout.back.height,
    ...mixColor(panel, ink, backHovered ? .25 : .08));
  typeWrite("< back", layout.back.x + (compact ? 12 : 20),
    layout.back.y + (compact ? 10 : 17), compact ? 25 : 34, ...ink);
  const controls = selectionControlKeys();
  drawCenteredKeycapRun(controls, viewHeight - (compact ? 52 : 66),
    compact ? 15 : 20, inputPads[0]?.down || [], ink);
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

// The dummy count, as a sentence. The host hands over a window and a total;
// the window is already the shortest one that had anything in it, so all that
// is left here is to say it in English. Zero returns nothing at all — an
// arcade does not advertise that nobody played today.
function dummyPopLine(stat) {
  const pops = Math.floor(Number(stat?.pops) || 0);
  const hours = Math.floor(Number(stat?.hours) || 0);
  if (pops <= 0 || hours <= 0 || hours > 48) return "";
  return pops + " dummy head" + (pops === 1 ? "" : "s") + " popped in the last " +
    (hours === 1 ? "hour" : hours + " hours");
}

function drawDummyPopLine(button, transitionInk) {
  // Not on the poster: the social preview is burned to a file whose hash is
  // checked at deploy, and a number that moves would make it stale hourly.
  if (typeof capabilities !== "function" ||
      capabilities().socialPreview === true) return;
  const line = dummyPopLine(capabilities().pops);
  if (!line) return;
  const size = Math.round(hudTypeSize * (compactLayout() ? .46 : .56));
  const width = handleWidth(line, size);
  const x = viewCenterX() - width / 2;
  const y = button.y + button.height + (compactLayout() ? 14 : 22);
  if (y + size > viewHeight - 8) return;
  const ink = transitionInk ||
    mixColor([198, 206, 232], [58, 70, 104], visualTheme.light);
  typeWrite(line, x + 2, y + 2, size,
    ...mixColor([8, 10, 26], [226, 234, 246], visualTheme.light));
  typeWrite(line, x, y, size, ...ink);
}

function titleButtonRect() {
  const compact = compactLayout();
  const textSize = hudTypeSize;
  const textWidth = handleWidth("start", textSize);
  const width = textWidth + (compact ? 54 : 76);
  const height = textSize + (compact ? 24 : 34);
  return { x: viewCenterX() - width / 2,
    y: viewHeight * (compact ? .61 : .64) - (compact ? 10 : 15),
    width, height, textSize, textWidth };
}

// A letter you can push around. `grow` eases toward 1 while the pointer is on
// it and `kick` is a decaying shudder fired the frame it is first touched, so
// the word answers one letter at a time instead of inflating as a block.
function toyGlyph(toys, index, hot, dt) {
  const toy = toys[index] || (toys[index] = { grow: 0, kick: 0, hot: false });
  toy.grow += ((hot ? 1 : 0) - toy.grow) * (1 - Math.exp(-dt * (hot ? 16 : 7)));
  if (hot && !toy.hot) toy.kick = 1;
  toy.hot = hot;
  toy.kick *= Math.exp(-dt * 5.5);
  return toy;
}

const pointInCell = (point, x, y, width, height) => point && x <= point.x &&
  point.x <= x + width && y <= point.y && point.y <= y + height;

// Screen-space outline, four boxes, no projection — safe anywhere.
function strokeBox(x, y, width, height, thickness, color) {
  box(x, y, width, thickness, ...color);
  box(x, y + height - thickness, width, thickness, ...color);
  box(x, y, thickness, height, ...color);
  box(x + width - thickness, y, thickness, height, ...color);
}

function drawTitleScreen(t, ink, transitionAge = -1) {
  const compact = compactLayout();
  const socialPreview = typeof capabilities === "function" &&
    capabilities().socialPreview === true;
  // Keep the game identity visible on the opening frame. The live fighters
  // and START remain interactive beneath the wordmark.
  const title = "oskiewar";
  const breath = 1 + Math.sin(t * .9) * .018;
  const socialTitleSize = Math.min(220,
    (stageRight - stageLeft - 56) / handleWidth(title, 1));
  const titleSize = (socialPreview ? socialTitleSize : compact ? 88 : 154) * breath;
  const titleWidth = handleWidth(title, titleSize);
  const titleX = viewCenterX() - titleWidth / 2;
  const titleY = viewHeight * (compact ? .38 : .35);

  // Air fuzzies: sparse motes drifting the whole frame, no panels or stripes
  // behind the wordmark.
  if (transitionAge < 0)
    for (let index = 0; index < (compact ? 10 : 16); index++) {
      const phase = index * 2.39996;
      const x = stageLeft + (stageRight - stageLeft) *
        (.5 + .47 * Math.sin(t * (.055 + index % 5 * .01) + phase));
      const y = viewHeight *
        (.5 + .45 * Math.cos(t * (.043 + index % 4 * .012) + phase * 1.7));
      const radius = (compact ? 2 : 3) + (index % 3);
      circle(x, y, radius, Math.max(1.5, radius * .48),
        animatedTitleColor(index, t * .7));
    }

  let cursor = titleX;
  // Kerning scaffold: one cell per glyph, taken from the same advance the
  // layout walks, so drift and bob read against their own metrics.
  const glyphCells = debugHitboxes && transitionAge < 0 && !socialPreview
    ? [] : null;
  const flashPalette = [[255, 226, 48], [70, 224, 92], [181, 255, 48]];
  const flash = transitionAge >= 0
    ? flashPalette[Math.floor(transitionAge / .065) % flashPalette.length] : null;
  const fade = transitionAge >= 0 ? clamp((transitionAge - .46) / .24, 0, 1) : 0;
  const transitionInk = flash ? mixColor(flash, [7, 10, 26], fade) : null;
  // Toys ease in real time, and the burn renders headless with no pointer, so
  // the poster stays the same picture it always was.
  const dt = titleToyAt < 0 ? 0 : clamp(t - titleToyAt, 0, .1);
  titleToyAt = t;
  const raw = globalThis.__oskiewarTouch?.pointer;
  const pointer = transitionAge < 0 && !socialPreview && raw?.active &&
    Number.isFinite(raw.x) && Number.isFinite(raw.y) ? raw : null;
  let held = -1;
  for (let index = 0; index < title.length; index++) {
    const character = title[index];
    const bob = Math.sin(t * 2.05 + index * .72) * (compact ? 5 : 8);
    const drift = Math.cos(t * 1.12 + index * .91) * (compact ? 3 : 5) +
      Math.sin(t * .63 + index * 1.71) * (compact ? 7 : 12);
    const advance = comicGlyphAdvance(character, titleSize);
    // The cell is what the pointer touches, so a letter that has swollen out
    // of it cannot chase the cursor or shove its neighbours along the line.
    const hot = pointInCell(pointer, cursor + drift, titleY + bob,
      advance, titleSize);
    if (hot) held = index;
    const toy = toyGlyph(titleToys, index, hot, dt);
    const size = titleSize *
      (1 + toy.grow * .38 + toy.kick * Math.sin(t * 31 + index) * .17);
    const shudder = toy.kick * Math.sin(t * 27 + index * 1.3) *
      (compact ? 6 : 11);
    const glyphX = cursor + drift +
      (advance - comicGlyphAdvance(character, size)) / 2 + shudder;
    const glyphY = titleY + bob - (size - titleSize) * .5 -
      toy.grow * (compact ? 6 : 11);
    const shadowOffset = Math.max(4, Math.min(5, Math.round(size * .025)));
    typeWrite(character, glyphX + shadowOffset, glyphY + shadowOffset,
      size, ...mixColor([8, 10, 24], [73, 43, 55], visualTheme.light * .35));
    typeWrite(character, glyphX, glyphY,
      size, ...(transitionInk || animatedTitleColor(index, t)));
    if (glyphCells) glyphCells.push([cursor + drift, titleY + bob, advance]);
    cursor += advance;
  }
  if (glyphCells) {
    strokeBox(titleX, titleY, titleWidth, titleSize, 2, [92, 132, 255]);
    for (const [x, y, advance] of glyphCells) {
      strokeBox(x, y, advance, titleSize, 2, [255, 92, 116]);
      box(x, y, 2, titleSize, 116, 255, 184);
    }
    typeWrite("size " + Math.round(titleSize) + "  width " +
      Math.round(titleWidth), titleX, titleY + titleSize + 12, 24,
      116, 255, 184);
  }

  const prompt = "start";
  const button = titleButtonRect();
  const hovered = Boolean(pointer) && pointInRect(pointer, button);
  if (globalThis.__oskiewarTouch) {
    globalThis.__oskiewarTouch.titleButton = button;
    globalThis.__oskiewarTouch.titleHover = hovered;
    globalThis.__oskiewarTouch.titleGlyph = held;
  }
  promptBounce += ((hovered ? 1 : 0) - promptBounce) *
    (1 - Math.exp(-dt * (hovered ? 14 : 6)));
  const promptPulse = .68 + (Math.sin(t * 3.2) + 1) * .16;
  const promptInk = transitionInk ||
    mixColor([196, 142, 18], [255, 238, 82], promptPulse);
  // The word floats on its own; the rect survives only as the touch target.
  // Yellow type on a bright sky needs an edge, so a sharp offset shadow
  // deepens as the background lightens and lifts as it goes to night.
  if (!socialPreview) {
    let promptCursor = button.x + (button.width - button.textWidth) / 2;
    const promptY = button.y + (button.height - button.textSize) / 2 - 2;
    const offset = Math.max(3, Math.round(button.textSize * .1));
    const shadowInk = mixColor([10, 12, 30], [86, 26, 116], visualTheme.light);
    const litInk = hovered
      ? mixColor(promptInk, [255, 255, 255], .35) : promptInk;
    for (let index = 0; index < prompt.length; index++) {
      const character = prompt[index];
      const advance = comicGlyphAdvance(character, button.textSize);
      const toy = toyGlyph(promptToys, index,
        pointInCell(pointer, promptCursor, promptY, advance, button.textSize),
        dt);
      // The whole word takes the bounce, but each letter takes it a beat after
      // the one before, so start reads as a hop travelling along the word.
      const hop = promptBounce * button.textSize * .3 *
        Math.abs(Math.sin(t * 5.4 - index * .62));
      const size = button.textSize * (1 + toy.grow * .2 + toy.kick * .12);
      const x = promptCursor + (advance - comicGlyphAdvance(character, size)) / 2;
      const y = promptY - hop - (size - button.textSize) * .5;
      if (!transitionInk)
        typeWrite(character, x + offset, y + offset, size, ...shadowInk);
      typeWrite(character, x, y, size, ...litInk);
      promptCursor += advance;
    }
    drawDummyPopLine(button, transitionInk);
  }
  // Touch play keeps its thumbs in the bottom corners, and the fight is live
  // under this screen now, so the stamp yields the pad rather than sit on it.
  if (transitionAge >= 0 || socialPreview || (typeof capabilities ===
      "function" && capabilities().inputFamily === "touch")) return;
  const titleUnixMs = runtime().unixMs || Date.now();
  const version = "v" + buildVersion;
  // Build provenance belongs to the title itself. Center it directly beneath
  // the wordmark so the top-left remains the persistent input legend and live
  // command score.
  const safe = hudSafeRect();
  const versionSize = Math.round(hudTypeSize * .62);
  typeWrite(version, viewCenterX() - handleWidth(version, versionSize) / 2,
    titleY + titleSize + 18, versionSize, ...ink);
  if (debugHitboxes) {
    const fpsLabel = Math.round(displayFps || 0) + " fps";
    typeWrite(fpsLabel, viewCenterX() - handleWidth(fpsLabel, versionSize) / 2,
      titleY + titleSize + versionSize + 26,
      versionSize, ...ink);
  }
  const clock = hudClockBox(titleUnixMs);
  typeWrite(clock.label, clock.left + 3, safe.top + 5,
    clock.size, ...contrastShadow(ink));
  typeWrite(clock.label, clock.left, safe.top + 2, clock.size, ...ink);
  drawHudStatusTray(clock, ink, titleUnixMs);
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

// Status indicators share one lane rather than each finding its own corner, so
// they read as a set: same cell, same row, in the title-safe frame just left of
// the wall clock. The lane is right-aligned because the clock and the round QR
// grow leftward from the same edge.
const statusGap = 6;
const statusCellSize = () => shellMode === "GAME" && debugHitboxes ? 56 : 26;

function hudStatusIcons() {
  const icons = [];
  if (typeof capabilities === "function" && capabilities().midi) icons.push("midi");
  return icons;
}

// The clock's footprint is computed once and shared, so the lane cannot drift
// out of step with where the clock actually lands.
function hudClockBox(unixMs) {
  const safe = hudSafeRect();
  const label = pacificTimeLabel(unixMs);
  const touch = typeof capabilities === "function" &&
    capabilities().inputFamily === "touch";
  const size = touch ? 16 : hudTypeSize;
  const qrBox = spectatorQrBox();
  const right = qrBox ? qrBox.left - 14 : safe.right;
  return { label, size, right, left: right - handleWidth(label, size) };
}

function hudStatusTray(clock) {
  const icons = hudStatusIcons();
  if (!icons.length) return null;
  const statusCell = statusCellSize();
  const right = clock.left - 14;
  const width = icons.length * statusCell + (icons.length - 1) * statusGap;
  return { icons, right, left: right - width, top: hudSafeRect().top,
    height: statusCell };
}

// A one-octave keyboard: seven equal white keys with the black keys only on the
// boundaries that have them, so it reads as a piano at this size instead of an
// approximate smear. It brightens for a beat whenever a note leaves.
function drawStatusPiano(x, y, lit) {
  const width = 21, height = 14, key = width / 7;
  const left = Math.round(x - width / 2), top = Math.round(y - height / 2);
  box(left, top, width, height, ...(lit ? [108, 240, 168] : [176, 184, 202]));
  for (const step of [1, 2, 4, 5, 6])
    box(Math.round(left + step * key) - 1, top, 2, Math.round(height * .58),
      23, 27, 40);
}

function drawHudStatusTray(clock, ink, unixMs) {
  const tray = hudStatusTray(clock);
  const statusCell = statusCellSize();
  // Debug is global state, so its large indicator owns bottom-center rather than
  // masquerading as another peripheral in the clock-side status tray.
  if (debugHitboxes) {
    const safe = hudSafeRect();
    const top = safe.bottom - statusCell;
    drawDebugBug(viewCenterX(), top + statusCell / 2 + 2,
      statusCell / 26);
  }
  if (!tray) return;
  const lit = typeof capabilities === "function" &&
    unixMs - (capabilities().midiPulse || 0) < 140;
  tray.icons.forEach((name, index) => {
    const x = tray.left + index * (statusCell + statusGap) + statusCell / 2;
    const y = tray.top + tray.height / 2;
    if (name === "midi") drawStatusPiano(x, y, lit);
  });
}

function drawDebugBug(x, y, scale = 1) {
  const shell = [255, 86, 126];
  const detail = [22, 12, 34];
  const limb = (x1, y1, x2, y2, width, color) =>
    filledCapsule(x1, y1, x2, y2, width, color);
  filledDisc(x, y + 2 * scale, 8 * scale, shell);
  filledDisc(x, y - 6 * scale, 5 * scale, shell);
  limb(x, y - 7 * scale, x - 6 * scale, y - 13 * scale, 2 * scale, shell);
  limb(x, y - 7 * scale, x + 6 * scale, y - 13 * scale, 2 * scale, shell);
  limb(x - 5 * scale, y, x - 11 * scale, y - 4 * scale, 2 * scale, shell);
  limb(x + 5 * scale, y, x + 11 * scale, y - 4 * scale, 2 * scale, shell);
  limb(x - 5 * scale, y + 5 * scale, x - 11 * scale, y + 9 * scale,
    2 * scale, shell);
  limb(x + 5 * scale, y + 5 * scale, x + 11 * scale, y + 9 * scale,
    2 * scale, shell);
  limb(x, y - scale, x, y + 9 * scale, 2 * scale, detail);
  filledDisc(x - 2 * scale, y - 7 * scale, 1.2 * scale, detail);
  filledDisc(x + 2 * scale, y - 7 * scale, 1.2 * scale, detail);
}

// The round QR owns the top-right corner whenever it is up, so the HUD clock
// asks for its footprint before choosing a lane.
function spectatorQrBox() {
  if (typeof capabilities === "function" && capabilities().socialPreview)
    return null;
  if (shellMode === "GAME" && !roundIsTimed()) return null;
  if (!spectatorQr || typeof spectatorQr.getModuleCount !== "function")
    return null;
  const safe = hudSafeRect();
  const count = spectatorQr.getModuleCount();
  const quiet = 2;
  const cell = Math.max(2,
    Math.floor((compactLayout() ? 108 : 158) / (count + quiet * 2)));
  const size = (count + quiet * 2) * cell;
  return { left: safe.right - size, top: safe.top, size, cell, count, quiet };
}

function drawDebugPerformance(ink) {
  if (!debugHitboxes || shellMode !== "GAME" || roundResult) return;
  const safe = hudSafeRect();
  const compact = compactLayout();
  const metaSize = compact ? 19 : 24;
  const run = runtime();
  const measuredHz = Number(run.refreshHz) || displayFps || 0;
  const aa = Math.max(1, Math.round(Number(run.antialiasingSamples) || 1));
  const aaLabel = String(run.antialiasingMode || (aa + "x"));
  const surfaceLabel = Math.round(Number(run.renderWidth) || run.width || stageRight) +
    "x" + Math.round(Number(run.renderHeight) || run.height || viewHeight) + " @ " +
    measuredHz.toFixed(1) + " Hz  ·  " + aaLabel;
  const timingLabel = "frame " + (Number(run.frameMs) || 0).toFixed(2) +
    "ms  render " + (Number(run.renderCpuMs) || 0).toFixed(2) +
    "ms  present " + (Number(run.presentMs) || 0).toFixed(2) + "ms";
  const surfaceY = safe.bottom - metaSize - 4;
  typeWrite(surfaceLabel, safe.left, surfaceY, metaSize, ...ink);
  const timingSize = Math.max(17, metaSize * .76);
  typeWrite(timingLabel, safe.left, surfaceY - timingSize - 5,
    timingSize, ...ink);
}

function drawSpectatorQr(ink) {
  if (typeof capabilities === "function" && capabilities().socialPreview) return;
  const safe = hudSafeRect();
  const qr = spectatorQrBox();
  if (!qr) return;
  const { count, quiet, cell, size, left, top } = qr;
  const shadow = [24, 26, 34];
  const previousDepth = triangleDepth;
  triangleDepth = -1.43;
  screenRect(left + 3, top + 3, size, size, shadow);
  screenRect(left, top, size, size, [250, 250, 247]);
  // Dark modules coalesce into horizontal runs so a full code stays a few
  // dozen faces instead of a few hundred — drawErrorQr already draws this way.
  const dark = [7, 8, 14];
  for (let row = 0; row < count; row++) {
    let run = 0;
    for (let column = 0; column <= count; column++) {
      if (column < count && spectatorQr.isDark(row, column)) { run++; continue; }
      if (run) screenRect(left + (column - run + quiet) * cell,
        top + (row + quiet) * cell, run * cell, cell, dark);
      run = 0;
    }
  }
  triangleDepth = previousDepth;
}

// Framing marks rather than a full box: the corners state the bounds without
// drawing four lines through the fight.
function drawCornerCrops(rect, reach, width, color) {
  const arm = Math.min(reach, (rect.right - rect.left) / 3,
    (rect.bottom - rect.top) / 3);
  for (const [x, towardX] of [[rect.left, 1], [rect.right, -1]])
    for (const [y, towardY] of [[rect.top, 1], [rect.bottom, -1]]) {
      filledCapsule(x, y, x + towardX * arm, y, width, color);
      filledCapsule(x, y, x, y + towardY * arm, width, color);
    }
}

function drawImpacts() {
  for (const impact of impacts) {
    if (impact.explosion && impact.blastRadius) {
      const age = clamp(1 - impact.life / impact.duration, 0, 1);
      const center = projectPoint(impact.x, impact.y - 3, impact.z || 0);
      const edge = projectPoint(impact.x + impact.blastRadius,
        impact.y - 3, impact.z || 0);
      const radius = Math.abs(edge.x - center.x);
      if ([center.x, center.y, radius].every(Number.isFinite) && radius < 4000) {
        filledRing(center.x, center.y, radius,
          Math.max(0, radius - Math.max(7, 22 * (1 - age))),
          mixColor([255, 218, 86], [111, 74, 48], age));
        const shockRadius = radius * (.18 + age * .72);
        filledRing(center.x, center.y, shockRadius,
          Math.max(0, shockRadius - Math.max(4, 13 * (1 - age))),
          mixColor([255, 246, 184], [164, 92, 52], age));
      }
    }
    for (const mote of impact.debris || []) {
      const point = projectPoint(mote.x, mote.y, mote.z);
      const radius = Math.max(2, mote.radius * cameraScale());
      if (![point.x, point.y, radius].every(Number.isFinite) ||
          Math.abs(point.x) > 30000 || Math.abs(point.y) > 30000) continue;
      filledDisc(point.x, point.y, radius,
        impact.death ? [255, 105, 190] : [255, 232, 92]);
    }
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
  drawPaletteCapsule({ x1: first.x, y1: first.y, x2: second.x, y2: second.y,
    width, part: fragment.part }, fragment.colors,
    fragment.paletteCoordinate || 0, fragment.color);
}

function drawSafeZones() {
  if (!debugHitboxes) return;
  drawCornerCrops(hudSafeRect(), 46, 3, [255, 214, 84]);
  drawCornerCrops(actionSafeRect(), 34, 2, [105, 255, 118]);
}

function gamePaint() {
  syncGameView();
  const run = runtime();
  if (globalThis.__oskiewarTouch) {
    globalThis.__oskiewarTouch.screen = shellMode === "MENU"
      ? titleTransitionAt !== null ? "title-transition" : "title"
      : selecting ? "select" : "game";
  }
  if (lastPaintAt > 0 && run.monotonicUs > lastPaintAt) {
    const sample = clamp(1000000 / (run.monotonicUs - lastPaintAt), 1, 240);
    displayFps = displayFps ? lerp(displayFps, sample, .12) : sample;
  }
  lastPaintAt = run.monotonicUs;
  const t = (run.monotonicUs - startedAt) / 1000000;
  if (typeof ac === "function") acFeed = ac();
  syncSignedInFighter();
  const localNation = typeof capabilities === "function"
    ? String(capabilities().country || "").toUpperCase() : "";
  if (/^[A-Z]{2}$/.test(localNation)) {
    players[0].nation = localNation;
    if (!players[1].npc) players[1].nation = localNation;
  }
  for (const player of players) player.handleColors = fighterProfile(player.name).colors;
  visualTheme = displayTheme();
  const replayOven = typeof capabilities === "function" &&
    capabilities().replayOven === true;
  triangleDepth = -1.4;
  const skyDay = mixColor([176, 215, 245], [255, 160, 112],
    visualTheme.sunset * .7);
  const sky = mixColor([7, 8, 28], skyDay, visualTheme.light);
  // Match the clear color to the arena sky. Camera framing can reveal the
  // clear layer during a jump; a different clear color looked like a flash.
  const outside = sky;
  // Keep the room's planes materially distinct: blue atmosphere, warm plaster
  // behind the fighters, and green earth under them. Day/night can tint the
  // palette without washing every surface into the same gray.
  const arenaDay = mixColor([244, 211, 178], [235, 154, 150],
    visualTheme.sunset * .48);
  const arena = mixColor([24, 18, 42], arenaDay, visualTheme.light);
  const groundDay = mixColor([142, 184, 116], [190, 151, 103],
    visualTheme.sunset * .38);
  const ground = mixColor([13, 25, 29], groundDay, visualTheme.light);
  const platformColor = mixColor([24, 29, 46], [211, 198, 171],
    visualTheme.light);
  const titleInk = mixColor([245, 248, 255], [24, 35, 72], visualTheme.light);
  const statusShadow = contrastShadow(titleInk);
  const menuArena = mixColor([7, 10, 26], [235, 241, 248], visualTheme.light);
  const menuPanel = mixColor([20, 28, 56], [215, 225, 239], visualTheme.light);
  const menuInk = mixColor([245, 248, 255], [24, 35, 72], visualTheme.light);
  wipe(...outside);
  drawSkyAtmosphere(sky, arena);
  if (PAL_SELECT && selecting) {
    box(0, 0, viewWidth(), viewHeight, ...menuArena);
    drawSelectionScreen(t, menuInk, menuPanel);
    return;
  }
  const cinematicAge = deathCinematicAge(run.monotonicUs);
  if (cinematicAge < 0 || cinematicAge >= 1.45) containFighters(t);
  cameraDoll.prepare();
  const { left: spanLeft, right: spanRight,
    top: spanTop, bottom: spanBottom } = terrainSpan();
  drawRoomSurfaces(spanLeft, spanRight, spanTop, spanBottom, arena);
  drawTerrainSurface(spanLeft, spanRight, worldNear, worldFar, ground);
  drawTerrainFrontWall(spanLeft, spanRight, worldNear, ground);
  drawTerrainGrass(spanLeft, spanRight, ground);
  drawBoosterPad(t);
  const platformNear = -520;
  const platformFar = 520;
  const ledgeLeft = Math.max(platformLeft, spanLeft);
  const ledgeRight = Math.min(platformRight, spanRight);
  // The platform wants to be a slab, but the stage paints in order with no
  // depth buffer — the volume's faces landed over the fighters on console.
  // Back to the plane until the volume can be drawn behind them properly.
  if (PLATFORM && ledgeLeft < ledgeRight) worldQuad(
    { x: ledgeLeft, y: platformY, z: platformNear },
    { x: ledgeRight, y: platformY, z: platformNear },
    { x: ledgeRight, y: platformY, z: platformFar },
    { x: ledgeLeft, y: platformY, z: platformFar }, platformColor);
  const shadowInk = mixColor([3, 5, 14], [92, 99, 101],
    visualTheme.light * .72);
  for (const player of players)
    if (player.alive || roundResult)
      drawSpotShadow(player.x, player.y, player.z, player.ducking ? 52 : 64,
        shadowInk);
  for (const item of balls)
    if (item.active)
      drawSpotShadow(item.x, item.y, item.z, item.radius * 1.18, shadowInk);
  const windInk = windDirection < 0
    ? mixColor([72, 174, 255], [28, 88, 188], visualTheme.light)
    : mixColor([255, 92, 132], [184, 35, 62], visualTheme.light);
  if (WIND_FLAG && shellMode === "GAME") drawWindFlag(t, windInk);
  // The top row is the round's: a clock, and who is watching. The wordmark
  // screen carries its own clock, so this one waits for start.
  if (!replayOven && shellMode === "GAME" && gameplayStarted) {
    const timedRound = roundIsTimed();
    const remainingSeconds = roundResult || !timedRound ? 0 : Math.max(0,
      Math.ceil((roundDurationUs - roundElapsedUs) / 1000000));
    const timerText = roundResult
      ? roundResult === "TIE" ? "tie!" : ""
      : timedRound ? String(remainingSeconds).padStart(2, "0") : "";
    const hud = hudSafeRect();
    const timerSize = hudTypeSize;
    if (timerText) {
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
    }
    if (roundViewer) {
      const viewerLabel = roundViewerMode || roundViewerStatus;
      typeWrite(viewerLabel, hud.right - viewerLabel.length * 18, hud.top + 7,
        24, ...(roundViewerMode === "LIVE" ? [210, 42, 62] : titleInk));
    }
    // Wall clock in the top right, tucked left of the round QR and below the
    // spectator label so it never lands on either.
    const nowMs = run.unixMs || Date.now();
    const clock = hudClockBox(nowMs);
    typeWrite(clock.label, clock.left + 3,
      hud.top + (roundViewer ? clock.size + 13 : 5),
      clock.size, ...contrastShadow(titleInk));
    typeWrite(clock.label, clock.left,
      hud.top + (roundViewer ? clock.size + 10 : 2), clock.size, ...titleInk);
    drawHudStatusTray(clock, titleInk, nowMs);
  }
  for (const tree of bodyTrees) drawBodyTree(tree, t);
  for (const pickup of gunPickups) drawGunPickup(pickup, t);
  for (const pickup of grenadePickups) drawGrenadePickup(pickup, t);
  const introAge = run.monotonicUs - roundStartedAt;
  const showRunnerLabels = !replayOven &&
    (Boolean(roundResult) || introAge >= introDurationUs);
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
    // Both fighters are always in this list. A fighter is never removed from
    // the frame to make a camera move work — the shot is what moves.
    ...players.map((item) => ({
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
    else {
      drawDiveMotion(renderable.item, t);
      drawDoubleJumpMotion(renderable.item, t);
      drawRunner(renderable.item, t, showRunnerLabels);
    }
  }
  // Debug geometry sits above the world but below the -1.42 screen-UI lane.
  // It must remain visible through fighter bodies without cutting across the
  // title, controls, command notation, names, clock, or other HUD furniture.
  triangleDepth = -1.4;
  drawDebugHitboxes(players[0], t);
  drawDebugHitboxes(players[1], t);
  drawBallHitboxes();
  drawSafeZones();
  triangleDepth = -1.42;
  drawImpacts();
  const counting = !roundResult && introAge < introDurationUs;
  // The matchup card announces two names in the middle of the screen, which
  // is exactly where the wordmark sits. On the entry fight the word wins.
  if (!replayOven && counting && shellMode === "GAME")
    drawFightIntro(introAge / 1000000, titleInk, statusShadow);
  // The keys belong wherever a newcomer is looking: under the wordmark on the
  // way in, and again while a round counts itself off. Self-play has no
  // newcomer — two bots need no tutorial, and neither does a reel of them.
  // The legend is the stable key for the score written beneath it. Keep it
  // present throughout every player-controlled screen; only unattended bot
  // self-play and marketing reels have no learner to serve.
  if (!replayOven && !selfPlay) drawControlLegend(titleInk);
  const resultUiReady = cinematicAge < 0 || cinematicAge >= 1.1;
  if (!replayOven && roundResult && resultUiReady) {
    if (INSTANT_REPLAY && instantReplay) {
      const frame = Math.min(instantReplay.frames.length,
        Math.floor(instantReplay.cursor) + 1);
      const replayLabel = "REPLAY  " + frame + "/" + instantReplay.frames.length;
      typeWrite(replayLabel, viewCenterX() - replayLabel.length * 10,
        820, 30, ...titleInk);
      drawCenteredKeycapRun(replayControlKeys(instantReplay.paused),
        940, 19, inputPads[0]?.down || [], titleInk);
    } else {
      const result = resultCardText();
      // The result is a small ownership mark, not a second title screen.
      // Character reactions carry the emotional result in the arena.
      const winnerSize = compactLayout() ? 30 : 42;
      const winnerWidth = handleWidth(result.winner, winnerSize);
      const resultY = hudSafeRect().top + 62;
      typeWrite(result.winner, viewCenterX() - winnerWidth / 2 + 3, resultY + 4,
        winnerSize, ...statusShadow);
      typeWrite(result.winner, viewCenterX() - winnerWidth / 2, resultY,
        winnerSize, ...titleInk);
      if (result.action) {
        const actionSize = Math.min(44, winnerSize * .54);
        const actionWidth = handleWidth(result.action, actionSize);
        typeWrite(result.action, viewCenterX() - actionWidth / 2 + 3, 900,
          actionSize, ...statusShadow);
        typeWrite(result.action, viewCenterX() - actionWidth / 2, 896,
          actionSize, ...titleInk);
      }
      if (!roundViewer && INSTANT_REPLAY) {
        drawCenteredKeycapRun(replayOfferKeys(), 940, 19,
          inputPads[0]?.down || [], titleInk);
      }
    }
  }
  // Nameplates and stats wait for the wordmark to lift; the entry frame is
  // the word, the keys, and the two fighters, and nothing else.
  if (!replayOven && shellMode === "GAME" && ((roundResult && resultUiReady) ||
      (!roundResult && introAge >= introDurationUs))) {
    const hudPlayers = spatialHudPlayers();
    for (let side = 0; side < hudPlayers.length; side++) {
      const player = hudPlayers[side];
      drawPlayerHandle(player, t, side);
      drawPlayerStats(player, side, t);
      drawHudInventory(player, side);
      drawCommandStream(player, side);
    }
  }
  if (!replayOven) drawDebugPerformance(titleInk);
  drawDeathFlash();
  if (shellMode === "MENU") {
    const transitionAge = titleTransitionAt !== null
      ? (run.monotonicUs - titleTransitionAt) / 1000000 : -1;
    drawTitleScreen(t, menuInk, transitionAge);
    if (transitionAge >= 0) return;
  }
  if (!replayOven) {
    drawSpectatorQr(titleInk);
    drawTouchControls();
  }
}

function boot() {
  try {
    gameBoot();
  } catch (error) {
    captureClientError("boot", error);
  }
}

function sim() {
  if (clientError) { restartAfterClientError(); return; }
  try {
    gameSim();
  } catch (error) {
    captureClientError("sim", error);
  }
}

function paint() {
  if (clientError) {
    try { drawClientError(); }
    catch (_) { drawClientErrorFallback(); }
    return;
  }
  try {
    gamePaint();
  } catch (error) {
    captureClientError("paint", error);
    try { drawClientError(); }
    catch (_) { drawClientErrorFallback(); }
  }
}

function act() {}
function leave() {
  try {
    roundViewerStop?.();
    roundViewerStop = null;
    roundViewer = null;
  } catch (error) {
    captureClientError("leave", error);
  }
}
