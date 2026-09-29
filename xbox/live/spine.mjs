// spine.mjs — a body whose movement starts in its spine.
//
// Pelvis to head is one rope of beads. Stiff links keep its length; a bend
// spring at every joint holds a resting curve relative to the segment below
// it, so a kick anywhere travels along the rope instead of snapping the
// whole shape. Muscles don't place beads — they change each joint's resting
// curve (curl, arch, side bend, twist), and a rhythm sends those curves up
// the chain as a wave: hips one way, shoulders the other, head arriving last.
//
// Limbs are loose chains hung off the rope's own frames: arms from the chest
// bead, legs from the pelvis. Each has a goal for its hand or foot, set every
// frame by whoever is animating (actions.mjs). A hand that can't reach its
// goal makes the spine bend toward it (the reach reflex); a hand that grabs
// pins, and its arm drags the chest.
//
// Units are the game's (a leg is 95). y is up here; the game is y-down and
// converts at the boundary. Fixed 60 Hz steps and plain arithmetic, so the
// same inputs give the same body on every screen.

export const step = 1 / 60;

export const defaults = {
  beads: 9,               // pelvis .. base of the skull
  length: 84,             // pelvis to the base of the skull
  skull: 36,              // the rope's last link runs on through the head to its crown
  stiffNeck: .88,         // neck and skull joints: firm, so the head rides the spine
  hipHeight: 92,          // pelvis above the floor, standing (legs 48 + 47)
  stiffLow: .35,          // bend spring at the pelvis end (per substep, 0..1)
  stiffHigh: .14,         // … and at the head end: the loosest link
  muscle: 1,              // tone: scales every bend spring
  reaction: .35,          // share of a bend correction pushed back down the rope
  twistStiff: .28,        // how hard each bead follows the one below in twist
  twistRest: .04,         // how hard each bead holds its own resting twist
  damping: .2,            // velocity lost per frame
  gravity: 900,           // on the body's own beads (sag, swing)
  riseGravity: 3600,      // on the whole body in flight, going up …
  fallGravity: 5400,      // … and coming down: short, heavy jumps
  hold: .5,               // how hard the pelvis follows the root (the legs)
  carry: .9,              // a driven root's travel the legs carry the body through
                          // (the rest is felt as inertia: sway, lag, whip)
  tempo: 1.7,             // rhythm, cycles per second at full drive
  lag: .22,               // rhythm phase delay per bead: the whip
  sway: .012,             // rhythm side bend per joint (radians)
  wring: .18,             // rhythm twist at the ends (radians)
  react: 8,               // how fast muscles move toward what's asked (1/s)
  shoulder: 20,           // half the shoulders
  hipWidth: 11,           // half the hips
  arm: [34, 32],          // upper arm, forearm
  leg: [48, 47],          // thigh, shin
  limbDamping: .12,
  reflex: 1,              // how much an out-of-reach hand bends the spine
  reachHinge: .7,         // most a reach tips the pelvis (radians) …
  reachCurl: .6,          // … and curls the back on top of that
  substeps: 4,
  iterations: 4,
};

// A body of its own: every spawn rolls one from a seed, so the same seed is
// always the same body (replays and every screen agree). Ranges keep it a
// person — looser or stiffer, longer or shorter, quicker or slower — never
// a broken one.
export function randomBody(seed) {
  let a = (seed >>> 0) || 1;
  const random = () => {   // mulberry32
    a = (a + 0x6D2B79F5) >>> 0;
    let t = a; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const pick = (lo, hi) => lo + (hi - lo) * random();
  // Sized to the game's cast: hips near 86 (legs nearly straight), crown
  // near 175.
  const legScale = pick(.93, 1.06), armScale = pick(.92, 1.08);
  const leg = [Math.round(46 * legScale), Math.round(45 * legScale)];
  return {
    length: Math.round(pick(62, 78)), skull: Math.round(pick(30, 36)),
    stiffLow: pick(.22, .5), stiffHigh: pick(.1, .18), stiffNeck: pick(.84, .88),
    damping: pick(.16, .26), reaction: pick(.2, .45), twistStiff: pick(.18, .38),
    tempo: pick(1.45, 2), lag: pick(.16, .28), sway: pick(.008, .018), wring: pick(.12, .26),
    react: pick(6, 11), shoulder: Math.round(pick(16, 24)), hipWidth: Math.round(pick(9, 13)),
    // Hips set a touch above the legs' length: the rope's weight sags the
    // pelvis a few units, and this leaves the standing knees near straight.
    arm: [Math.round(32 * armScale), Math.round(30 * armScale)], leg, hipHeight: leg[0] + leg[1] + 3,
  };
}

// What the body is asked to do. Muscles ease toward it; nothing snaps.
// yaw turns the whole body off its direction of travel (a skater stands
// sideways); crouch lowers the hips.
// hinge tips the pelvis forward: bending at the hips, not the back.
const neutralIntent = () => ({ curl: 0, arch: 0, side: 0, twist: 0, yaw: 0, crouch: 0, hinge: 0 });

function makeLimb(kind, side, lengths) {
  const beads = () => new Float64Array(3);
  return { kind, side, lengths, x: beads(), y: beads(), z: beads(), px: beads(), py: beads(), pz: beads(),
    goal: null, stiff: 0, pin: false, reach: false };
}

export function createSpine(options = {}) {
  const o = { ...defaults, ...options };
  // The rope is the spine's beads plus one more at the crown: the head is
  // the rope's last link, not something hung on the end of it.
  const n = Math.max(3, Math.round(o.beads)) + 1, segment = o.length / (n - 2);
  const links = new Float64Array(n - 1).fill(segment);
  links[n - 2] = o.skull;
  const spine = {
    o, n, segment, links, floor: 0,
    x: new Float64Array(n), y: new Float64Array(n), z: new Float64Array(n),
    px: new Float64Array(n), py: new Float64Array(n), pz: new Float64Array(n),
    twist: new Float64Array(n), previousTwist: new Float64Array(n),
    // The root is what the legs give the spine: where the hips should be,
    // how the body is travelling, which way, and whether it's in the air.
    root: { x: 0, y: o.hipHeight, z: 0, vx: 0, vy: 0, vz: 0, yaw: 0, tilt: .06, air: false },
    intent: neutralIntent(), shape: neutralIntent(), reflex: { curl: 0, side: 0, twist: 0, crouch: 0, hinge: 0 },
    rhythm: { phase: 0, drive: 0, pace: 0, gliding: false }, time: 0, landed: 0, strain: 0, pull: { x: 0, y: 0, z: 0 },
    arms: [makeLimb("arm", -1, o.arm), makeLimb("arm", 1, o.arm)],
    legs: [makeLimb("leg", -1, o.leg), makeLimb("leg", 1, o.leg)],
  };
  settle(spine);
  return spine;
}

export const chestIndex = (spine) => Math.round((spine.n - 2) * .72);

// Stand the rope on its pelvis along its resting curve, limbs hanging, at rest.
export function settle(spine) {
  const { n, links, root } = spine;
  spine.carried = null;   // a re-stand isn't travel
  const bends = restBends(spine);
  let direction = pelvisUp(spine), x = root.x, y = root.y, z = root.z;
  for (let i = 0; i < n; i++) {
    spine.x[i] = spine.px[i] = x; spine.y[i] = spine.py[i] = y; spine.z[i] = spine.pz[i] = z;
    spine.twist[i] = spine.previousTwist[i] = 0;
    if (i === n - 1) break;
    direction = bendDirection(direction, frameAt(spine, i), bends.forward[i], bends.side[i]);
    x += direction.x * links[i]; y += direction.y * links[i]; z += direction.z * links[i];
  }
  for (const limb of [...spine.arms, ...spine.legs]) {
    const anchor = limbAnchor(spine, limb);
    for (let i = 0; i < 3; i++) {
      const down = i === 0 ? 0 : i === 1 ? limb.lengths[0] : limb.lengths[0] + limb.lengths[1];
      limb.x[i] = limb.px[i] = anchor.x; limb.y[i] = limb.py[i] = anchor.y - down; limb.z[i] = limb.pz[i] = anchor.z;
    }
  }
}

// ——— Vectors and frames ———

const cross = (a, b) => ({ x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x });
const dot = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z;
const scale = (a, k) => ({ x: a.x * k, y: a.y * k, z: a.z * k });
const add = (a, b) => ({ x: a.x + b.x, y: a.y + b.y, z: a.z + b.z });
const sub = (a, b) => ({ x: a.x - b.x, y: a.y - b.y, z: a.z - b.z });
const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));
function normal(a) {
  const length = Math.hypot(a.x, a.y, a.z) || 1;
  return { x: a.x / length, y: a.y / length, z: a.z / length };
}

// Forward and right at bead i: travel heading, the body's yaw off it, and
// that bead's twist.
function frameAt(spine, i) {
  const a = spine.root.yaw + spine.shape.yaw + spine.twist[i];
  return { forward: { x: Math.cos(a), y: 0, z: Math.sin(a) }, right: { x: -Math.sin(a), y: 0, z: Math.cos(a) } };
}

// The pelvis points up, tipped forward by its tilt.
function pelvisUp(spine) {
  const { forward } = frameAt(spine, 0), t = spine.root.tilt + spine.shape.hinge + spine.reflex.hinge;
  return normal(add(scale({ x: 0, y: 1, z: 0 }, Math.cos(t)), scale(forward, Math.sin(t))));
}

// Continue a rope segment from `below`, bent forward then sideways. The
// frame is built from `right`, which stays sideways however far the body
// folds; built from forward it went degenerate folded over (forward ≈ up)
// and the shoulders flipped side to side.
function bendDirection(below, frame, forwardBend, sideBend) {
  const right = normal(add(frame.right, scale(below, -dot(frame.right, below))));
  const ahead = cross(below, right);
  const pitched = add(scale(below, Math.cos(forwardBend)), scale(ahead, Math.sin(forwardBend)));
  return normal(add(scale(pitched, Math.cos(sideBend)), scale(right, Math.sin(sideBend))));
}

// An orthonormal frame read off the rope at bead i: its own up (along the
// rope), forward (the bead's facing, squared to that up) and right.
export function bodyFrame(spine, i) {
  const up = i === 0 ? pelvisUp(spine)
    : normal({ x: spine.x[i] - spine.x[i - 1], y: spine.y[i] - spine.y[i - 1], z: spine.z[i] - spine.z[i - 1] });
  const r = frameAt(spine, i).right;                     // see bendDirection
  const right = normal(add(r, scale(up, -dot(r, up))));
  return { origin: { x: spine.x[i], y: spine.y[i], z: spine.z[i] }, forward: cross(up, right), up, right };
}
// A point in a frame: forward, up, right.
export const place = (frame, f, u, r) =>
  add(frame.origin, add(add(scale(frame.forward, f), scale(frame.up, u)), scale(frame.right, r)));
export const chestFrame = (spine) => bodyFrame(spine, chestIndex(spine));
export const pelvisFrame = (spine) => bodyFrame(spine, 0);
// A point on the ground in the travel frame: along the heading, across it.
export function travelPoint(spine, along, across, height = 0) {
  const { root } = spine, c = Math.cos(root.yaw), s = Math.sin(root.yaw);
  return { x: root.x + c * along - s * across, y: spine.floor + height, z: root.z + s * along + c * across };
}

function limbAnchor(spine, limb) {
  return limb.kind === "arm"
    ? place(chestFrame(spine), 0, 4, limb.side * spine.o.shoulder)
    : place(pelvisFrame(spine), 0, -4, limb.side * spine.o.hipWidth);
}

// ——— The inner force ———

// Each joint's resting bend. A standing spine has a gentle S (lumbar in,
// chest out, neck in); curl, arch and the reach reflex spread along it; the
// rhythm adds a travelling side wave.
function restBends(spine) {
  const { n, shape, rhythm, o, reflex } = spine, joints = n - 1;
  const forward = new Float64Array(joints), side = new Float64Array(joints);
  const curl = shape.curl + reflex.curl, lean = shape.side + reflex.side;
  for (let j = 0; j < joints; j++) {
    const u = j / Math.max(1, joints - 1);                  // 0 lumbar … 1 neck
    const s = -.05 * Math.cos(u * Math.PI * 2);            // the standing S
    const arch = Math.sin(u * Math.PI) * .6 + .4;          // arching lives mid-back
    forward[j] = s + curl / joints - shape.arch * arch / joints;
    side[j] = lean / joints + o.sway * rhythm.drive * Math.sin(rhythm.phase - j * o.lag);
  }
  return { forward, side };
}

// Resting twist per bead: what's asked, plus the rhythm's wring travelling up
// the rope. With lag × beads near π the hips and shoulders counter-rotate.
function restTwist(spine, i) {
  const { o, rhythm, shape, n, reflex } = spine;
  return (shape.twist + reflex.twist) * i / (n - 1) +
    o.wring * rhythm.drive * Math.sin(rhythm.phase - i * o.lag);
}

// A hand reaching for something out of its arm's range asks the spine for
// the rest: curl down to what's low, bend toward what's to the side, turn
// toward what's behind. It's the spine doing the reaching.
// While the hand is still short, the effort builds (`strain`): the spine
// keeps curling and the legs keep bending until the hand gets there.
function reachReflex(spine, dt) {
  let curl = 0, side = 0, twist = 0, crouch = 0, hinge = 0, short = 0;
  // Measured from where the hips would be standing, facing the body's
  // heading — not from the pelvis, which this reflex itself moves (measuring
  // from it made the body bob: crouch, target seems higher, rise, repeat).
  const { o, root } = spine, armLength = o.arm[0] + o.arm[1], yaw = root.yaw + spine.shape.yaw;
  const pelvis = { origin: { x: root.x, y: spine.floor + o.hipHeight, z: root.z },
    forward: { x: Math.cos(yaw), y: 0, z: Math.sin(yaw) }, right: { x: -Math.sin(yaw), y: 0, z: Math.cos(yaw) } };
  for (const arm of spine.arms) {
    if (!arm.reach || !arm.goal) continue;
    const hand = { x: arm.x[2], y: arm.y[2], z: arm.z[2] };
    short = Math.max(short, Math.hypot(hand.x - arm.goal.x, hand.y - arm.goal.y, hand.z - arm.goal.z));
    // How far past the arm the goal is, from where this shoulder stands.
    const shoulder = add(add(pelvis.origin, { x: 0, y: o.length * .72, z: 0 }), scale(pelvis.right, arm.side * o.shoulder));
    const need = clamp((Math.hypot(...Object.values(sub(arm.goal, shoulder))) - armLength * .85) / 40, 0, 1) * (1 + spine.strain);
    if (!need) continue;
    const v = sub(arm.goal, pelvis.origin);
    const f = dot(v, pelvis.forward), u = dot(v, { x: 0, y: 1, z: 0 }), r = dot(v, pelvis.right);
    // Low things: the legs take the depth (crouch, below) and the hips hinge
    // for the distance out — something close by is squatted to with the
    // back fairly upright, something far is bent over to. A back curled past
    // ~1.2 rad hooks and lifts the hand.
    const low = clamp((o.length * .8 - u) / 90, 0, 1), out = clamp((Math.hypot(f, r) - 20) / 60, .3, 1);
    hinge += 1.15 * low * need * out;
    curl += clamp((o.length * .8 - u) / 110, -.4, 1) * need * out;
    side += clamp(r / 90, -.6, .6) * need;
    twist += clamp(Math.atan2(r, Math.max(1, f)) * .5, -.7, .7) * need;
    // Something near the floor needs the legs too.
    crouch += 48 * clamp((75 - (arm.goal.y - spine.floor)) / 70, 0, 1) * need;
  }
  // Effort holds once the hand arrives (letting it go is what made the body
  // rise, fall short and reach again); it only relaxes when nothing is reached for.
  const reaching = spine.arms.some((arm) => arm.reach && arm.goal);
  spine.strain = !reaching ? Math.max(0, spine.strain - dt * 2) : clamp(spine.strain + (short > 4 ? dt * 1.2 : 0), 0, 1);
  // A taller spine or longer arms needs less fold to reach the same spot:
  // the limits are for the default body and scale with its proportions.
  const fold = clamp((120 / (o.length + o.skull)) * (66 / armLength), .6, 1.2);
  return { curl: Math.min(o.reachCurl * fold, curl) * o.reflex, side: side * o.reflex, twist: twist * o.reflex,
    crouch: Math.min(60, crouch) * o.reflex, hinge: Math.min(o.reachHinge * fold, hinge) * o.reflex };
}

// ——— Stepping ———

export function stepSpine(spine, dt = step) {
  const { o, n, root, shape, intent, rhythm, reflex } = spine;
  // Muscles move toward what's asked, never jump there.
  // A strike fires the muscles faster than posture does (`snap`).
  const ease = 1 - Math.exp(-o.react * (spine.snap || 1) * dt);
  for (const key of Object.keys(shape)) shape[key] += (intent[key] - shape[key]) * ease;
  const want = reachReflex(spine, dt);
  for (const key of Object.keys(reflex)) reflex[key] += (want[key] - reflex[key]) * ease * .6;
  // The rhythm's tempo and depth come from how fast the body travels.
  // Gliding (a board, a kart) travels without stepping: no rhythm.
  const speed = Math.hypot(root.vx, root.vz);
  const stepping = rhythm.gliding || root.air ? 0 : Math.min(1, speed / 420);
  rhythm.drive += (stepping - rhythm.drive) * ease;
  rhythm.pace = rhythm.gliding || root.air ? 0 : speed / 420;
  rhythm.phase += Math.PI * 2 * frequency(spine) * dt;
  // The legs: they hold the hips at standing height less the crouch, or the
  // body flies until it comes down on them.
  spine.landed = 0;
  // However much is asked (a pose's crouch and a reach's together), the hips
  // stop at a deep squat — or, seated, wherever the seat puts them.
  const stand = spine.floor + o.hipHeight - Math.min(shape.crouch + reflex.crouch, Math.max(shape.crouch, o.hipHeight * .68));
  // A driven root belongs to someone else (the game): they say where it is,
  // how it moves and whether it's flying; the body only stands on it.
  if (root.driven) {
    root.y += (stand - root.y) * (1 - Math.exp(-14 * dt));
    carryAlong(spine);
  }
  else if (root.air) {
    root.vy -= (root.vy > 0 ? o.riseGravity : o.fallGravity) * dt; root.y += root.vy * dt;
    if (root.y <= stand && root.vy < 0) {
      spine.landed = -root.vy; root.air = false; root.vy = 0; root.y = stand;
      land(spine, spine.landed * .6);
    }
  } else root.y += (stand - root.y) * (1 - Math.exp(-14 * dt));
  if (!root.driven) { root.x += root.vx * dt; root.z += root.vz * dt; }

  const h = dt / o.substeps;
  for (let sub = 0; sub < o.substeps; sub++) {
    integrate(spine, h);
    stepTwist(spine);
    // Bend springs act once per substep, as springs; only the links (which
    // must never stretch) and the hips are solved to convergence.
    holdPelvis(spine);
    bendSprings(spine, restBends(spine));
    // The floor is part of the solve, not an afterthought: clamped after the
    // links, a body folded to the ground had its neck pulled out of length.
    const ground = () => { for (let i = 1; i < n; i++) if (spine.y[i] < spine.floor + 6) spine.y[i] = spine.floor + 6; };
    for (let k = 0; k < o.iterations; k++) { holdPelvis(spine); ground(); keepLinks(spine); keepLinks(spine); }
    for (const limb of spine.arms) stepLimb(spine, limb, h);
    for (const limb of spine.legs) stepLimb(spine, limb, h);
    takePull(spine);
    keepLinks(spine);   // the rope takes the pull without giving length
  }
  spine.time += dt;
}

// A driven root moves (and turns) however the game says, often faster than
// a body could follow. Most of that travel carries every bead with it —
// positions and their history alike, so it adds no velocity — leaving
// 1 - carry of it to be felt as inertia. Without this, game speeds folded
// the rope over backwards.
function carryAlong(spine) {
  const { root, o } = spine, last = spine.carried;
  spine.carried = { x: root.x, z: root.z, yaw: root.yaw };
  if (!last) return;
  const k = o.carry, dx = (root.x - last.x) * k, dz = (root.z - last.z) * k;
  let turn = root.yaw - last.yaw;
  turn = Math.atan2(Math.sin(turn), Math.cos(turn)) * k;
  const c = Math.cos(turn), sn = Math.sin(turn);
  const move = (xs, zs, i) => {
    const rx = xs[i] - last.x, rz = zs[i] - last.z;
    xs[i] = last.x + rx * c - rz * sn + dx; zs[i] = last.z + rx * sn + rz * c + dz;
  };
  for (let i = 0; i < spine.n; i++) { move(spine.x, spine.z, i); move(spine.px, spine.pz, i); }
  for (const limb of [...spine.arms, ...spine.legs]) for (let i = 0; i < 3; i++) { move(limb.x, limb.z, i); move(limb.px, limb.pz, i); }
}

// Cycles per second of the inner rhythm right now.
// Stepping cadence rises with speed but never stalls to a crawl; standing
// still, the rhythm still turns over but has no depth (drive 0).
// Past a run the cadence keeps climbing, so strides can keep pace.
export const frequency = (spine) => spine.o.tempo * (.6 + .5 * spine.rhythm.drive + .45 * Math.max(0, spine.rhythm.pace - 1));

// Verlet: each bead keeps the velocity it had, loses a little, and falls.
function integrate(spine, h) {
  const { o, n } = spine, keep = 1 - o.damping / o.substeps, fall = o.gravity * h * h;
  for (let i = 0; i < n; i++) {
    const vx = (spine.x[i] - spine.px[i]) * keep, vy = (spine.y[i] - spine.py[i]) * keep,
      vz = (spine.z[i] - spine.pz[i]) * keep;
    spine.px[i] = spine.x[i]; spine.py[i] = spine.y[i]; spine.pz[i] = spine.z[i];
    spine.x[i] += vx; spine.y[i] += vy - (i ? fall : 0); spine.z[i] += vz;
  }
}

// Twist is its own small rope: the pelvis takes it from the hips, each bead
// above follows the one below, late.
function stepTwist(spine) {
  const { o, n, twist, previousTwist } = spine, keep = 1 - o.damping / o.substeps;
  for (let i = 0; i < n; i++) {
    const velocity = (twist[i] - previousTwist[i]) * keep;
    previousTwist[i] = twist[i];
    twist[i] += velocity;
    const rest = restTwist(spine, i);
    if (i === 0) { twist[0] += (rest - twist[0]) * .6; continue; }
    const follow = twist[i - 1] + rest - restTwist(spine, i - 1);
    twist[i] += (follow - twist[i]) * o.twistStiff * o.muscle + (rest - twist[i]) * o.twistRest;
  }
}

function holdPelvis(spine) {
  const { root, o } = spine;
  spine.x[0] += (root.x - spine.x[0]) * o.hold;
  spine.y[0] += (root.y - spine.y[0]) * o.hold;
  spine.z[0] += (root.z - spine.z[0]) * o.hold;
}

// Each joint pulls the bead above toward its resting curve, relative to the
// segment below it — that relativity is what makes it a rope, not a statue.
function bendSprings(spine, bends) {
  const { o, n, links } = spine;
  let below = pelvisUp(spine);
  for (let j = 0; j < n - 1; j++) {
    // Firm at the pelvis, loosest through the upper back, firm again at the
    // neck and skull so the head stays connected to the spine's line.
    const u = j / Math.max(1, n - 3), segment = links[j];
    const k = Math.min(1, (j >= n - 3 ? o.stiffNeck : o.stiffLow + (o.stiffHigh - o.stiffLow) * u) * o.muscle);
    const want = bendDirection(below, frameAt(spine, j), bends.forward[j], bends.side[j]);
    const tx = spine.x[j] + want.x * segment, ty = spine.y[j] + want.y * segment,
      tz = spine.z[j] + want.z * segment;
    const dx = (tx - spine.x[j + 1]) * k, dy = (ty - spine.y[j + 1]) * k, dz = (tz - spine.z[j + 1]) * k;
    const push = j ? o.reaction : 0;
    spine.x[j + 1] += dx * (1 - push); spine.y[j + 1] += dy * (1 - push); spine.z[j + 1] += dz * (1 - push);
    spine.x[j] -= dx * push; spine.y[j] -= dy * push; spine.z[j] -= dz * push;
    below = normal({ x: spine.x[j + 1] - spine.x[j], y: spine.y[j + 1] - spine.y[j], z: spine.z[j + 1] - spine.z[j] });
  }
}

// Links never stretch or shrink. The pelvis is heavy (the legs hold it).
function keepLinks(spine) {
  const { n, links } = spine;
  for (let i = 0; i < n - 1; i++) {
    const dx = spine.x[i + 1] - spine.x[i], dy = spine.y[i + 1] - spine.y[i], dz = spine.z[i + 1] - spine.z[i];
    const d = Math.hypot(dx, dy, dz) || 1, error = (d - links[i]) / d;
    const low = i ? .5 : .3;   // the pelvis gives a little (all of a landing on one link tore it)
    spine.x[i] += dx * error * low; spine.y[i] += dy * error * low; spine.z[i] += dz * error * low;
    spine.x[i + 1] -= dx * error * (1 - low); spine.y[i + 1] -= dy * error * (1 - low); spine.z[i + 1] -= dz * error * (1 - low);
  }
}

// ——— Limbs ———

// A limb is three beads: the anchor (shoulder or hip, carried by the rope),
// the elbow or knee, and the hand or foot. The end chases its goal by its
// stiffness (a pinned end sits on it); the middle bows toward its pole
// (elbows back and out, knees forward); links keep both bones' lengths. A
// pinned hand whose arm runs out of length drags the chest bead with it.
function stepLimb(spine, limb, h) {
  const { o } = spine, keep = 1 - o.limbDamping / o.substeps, fall = o.gravity * h * h;
  const anchor = limbAnchor(spine, limb), [l1, l2] = limb.lengths;
  limb.x[0] = limb.px[0] = anchor.x; limb.y[0] = limb.py[0] = anchor.y; limb.z[0] = limb.pz[0] = anchor.z;
  for (let i = 1; i < 3; i++) {
    const vx = (limb.x[i] - limb.px[i]) * keep, vy = (limb.y[i] - limb.py[i]) * keep, vz = (limb.z[i] - limb.pz[i]) * keep;
    limb.px[i] = limb.x[i]; limb.py[i] = limb.y[i]; limb.pz[i] = limb.z[i];
    limb.x[i] += vx; limb.y[i] += vy - fall; limb.z[i] += vz;
  }
  if (limb.goal) {
    // A goal past the limb's length is aimed at, not stretched to (a pinned
    // hand is the exception: its arm pulls the body instead).
    let goal = limb.goal;
    const gx = goal.x - anchor.x, gy = goal.y - anchor.y, gz = goal.z - anchor.z, d = Math.hypot(gx, gy, gz), most = (l1 + l2) * .98;
    if (!limb.pin && d > most) goal = { x: anchor.x + gx * most / d, y: anchor.y + gy * most / d, z: anchor.z + gz * most / d };
    const k = limb.pin ? 1 : limb.stiff;
    limb.x[2] += (goal.x - limb.x[2]) * k; limb.y[2] += (goal.y - limb.y[2]) * k; limb.z[2] += (goal.z - limb.z[2]) * k;
  }
  // The joint bows toward its pole.
  const frame = limb.kind === "arm" ? chestFrame(spine) : pelvisFrame(spine);
  const pole = normal(limb.kind === "arm"
    ? add(add(scale(frame.forward, -.5), scale(frame.right, limb.side * .5)), scale(frame.up, -.3))
    : frame.forward);
  const end = { x: limb.x[2], y: limb.y[2], z: limb.z[2] }, mid = scale(add(anchor, end), .5);
  const half = Math.hypot(end.x - anchor.x, end.y - anchor.y, end.z - anchor.z) / 2;
  const bow = Math.sqrt(Math.max(0, ((l1 + l2) / 2) ** 2 - half * half));
  const knee = add(mid, scale(pole, bow));
  limb.x[1] += (knee.x - limb.x[1]) * .35; limb.y[1] += (knee.y - limb.y[1]) * .35; limb.z[1] += (knee.z - limb.z[1]) * .35;
  // Joints hinge one way: an elbow or knee on the wrong side is folded back.
  // The ground holds the knee and foot up. Both happen before the lengths
  // are solved, so neither can leave a bone short or long.
  const a = { x: limb.x[0], y: limb.y[0], z: limb.z[0] }, e = { x: limb.x[2], y: limb.y[2], z: limb.z[2] };
  const centre = scale(add(a, e), .5), off = dot(sub({ x: limb.x[1], y: limb.y[1], z: limb.z[1] }, centre), pole);
  if (off < 0) { limb.x[1] -= pole.x * off * 2; limb.y[1] -= pole.y * off * 2; limb.z[1] -= pole.z * off * 2; }
  const ground = () => { for (let i = 1; i < 3; i++) if (limb.y[i] < spine.floor + (i === 2 ? 3 : 6)) limb.y[i] = spine.floor + (i === 2 ? 3 : 6); };
  ground();
  // Bones keep their lengths. The anchor is the body's; a pinned end is
  // mostly the world's, so the arm's pull is handed to the chest (see pull).
  for (let k = 0; k < o.iterations * 2; k++) {
    const pull = linkLimb(limb, 0, 1, l1, limb.pin && limb.kind === "arm" ? .6 : 0, 1);
    if (pull) { spine.pull.x += pull.x; spine.pull.y += pull.y; spine.pull.z += pull.z; }
    linkLimb(limb, 1, 2, l2, 1, limb.pin ? .25 : 1);
  }
  // A foot held up by the ground keeps its place; the knee re-fits to it.
  ground();
  for (let k = 0; k < 2; k++) { linkLimb(limb, 1, 2, l2, 1, 0); linkLimb(limb, 0, 1, l1, 0, 1); }
}

// What a pinned arm asked of the chest this substep, handed over once and
// capped, so a hard pull bends the rope instead of tearing its links.
function takePull(spine) {
  const { pull } = spine, d = Math.hypot(pull.x, pull.y, pull.z), cap = 1.2, k = d > cap ? cap / d : 1;
  const chest = chestIndex(spine);
  for (const [i, w] of [[chest, 1], [chest - 1, .5]]) {
    spine.x[i] += pull.x * k * w; spine.y[i] += pull.y * k * w; spine.z[i] += pull.z * k * w;
  }
  pull.x = pull.y = pull.z = 0;
}

// Hold two limb beads at `length`, moving each by its weight (0 = fixed).
// Returns how far a movable anchor was asked to go, for the body to take.
function linkLimb(limb, a, b, length, weightA, weightB) {
  const dx = limb.x[b] - limb.x[a], dy = limb.y[b] - limb.y[a], dz = limb.z[b] - limb.z[a];
  const d = Math.hypot(dx, dy, dz) || 1, error = (d - length) / d, total = weightA + weightB;
  if (!total) return null;
  const wa = weightA / total, wb = weightB / total;
  limb.x[b] -= dx * error * wb; limb.y[b] -= dy * error * wb; limb.z[b] -= dz * error * wb;
  if (!wa) return null;
  const pull = { x: dx * error * wa, y: dy * error * wa, z: dz * error * wa };
  limb.x[a] += pull.x; limb.y[a] += pull.y; limb.z[a] += pull.z;
  return pull;
}

// ——— Pokes from outside ———

// Verlet keeps velocity as the distance moved in one substep, so a shove is
// scaled by the substep, not the frame.
export function impulse(spine, at, vx, vy, vz, spread = 2) {
  const h = step / spine.o.substeps;
  for (let i = 0; i < spine.n; i++) {
    const w = Math.exp(-((i - at) ** 2) / (2 * spread * spread)) * h;
    spine.px[i] -= vx * w; spine.py[i] -= vy * w; spine.pz[i] -= vz * w;
  }
  for (const arm of spine.arms) for (let i = 1; i < 3; i++) {
    arm.px[i] -= vx * h * .8; arm.py[i] -= vy * h * .8; arm.pz[i] -= vz * h * .8;
  }
}

// A landing: everything above the hips arrives still falling while the legs
// have already stopped. The rope takes the rest; the arms fall on.
export function land(spine, speed = 700) {
  const h = step / spine.o.substeps;
  for (let i = 1; i < spine.n; i++) spine.py[i] += speed * h;
  for (const arm of spine.arms) for (let i = 1; i < 3; i++) arm.py[i] += speed * h;
}

// ——— Reading it ———

// The head sits on the rope's skull link: its frame is that link's, its
// centre partway up it from the base of the skull.
export function frames(spine) {
  return { pelvis: pelvisFrame(spine), chest: chestFrame(spine), head: headFrame(spine) };
}
export function headFrame(spine) {
  const frame = bodyFrame(spine, spine.n - 1), base = spine.n - 2, k = .55;
  frame.origin = { x: spine.x[base] + (spine.x[base + 1] - spine.x[base]) * k,
    y: spine.y[base] + (spine.y[base + 1] - spine.y[base]) * k, z: spine.z[base] + (spine.z[base + 1] - spine.z[base]) * k };
  return frame;
}

// A bead in the travel frame (forward, up, right from the root), so
// travelling and turning don't read as motion.
export function local(spine, i) {
  const { root } = spine, c = Math.cos(root.yaw), s = Math.sin(root.yaw);
  const dx = spine.x[i] - root.x, dz = spine.z[i] - root.z;
  return { forward: dx * c + dz * s, up: spine.y[i] - root.y, right: -dx * s + dz * c };
}

export function linkError(spine) {
  let worst = 0;
  for (let i = 0; i < spine.n - 1; i++) {
    const d = Math.hypot(spine.x[i + 1] - spine.x[i], spine.y[i + 1] - spine.y[i], spine.z[i + 1] - spine.z[i]);
    worst = Math.max(worst, Math.abs(d - spine.links[i]) / spine.links[i]);
  }
  return worst;
}

// Worst stretch of any limb bone, as a share of its length.
export function limbError(spine) {
  let worst = 0;
  for (const limb of [...spine.arms, ...spine.legs]) for (let i = 0; i < 2; i++) {
    const d = Math.hypot(limb.x[i + 1] - limb.x[i], limb.y[i + 1] - limb.y[i], limb.z[i + 1] - limb.z[i]);
    worst = Math.max(worst, Math.abs(d - limb.lengths[i]) / limb.lengths[i]);
  }
  return worst;
}

export const limbEnd = (limb) => ({ x: limb.x[2], y: limb.y[2], z: limb.z[2] });
