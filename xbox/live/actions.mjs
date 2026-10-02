// actions.mjs — procedural animation on the spine body (spine.mjs).
//
// Nothing here is a keyframe. Each frame the actor turns what it's asked to
// do into two things: the spine's resting curves (curl, arch, side, twist,
// yaw, crouch — its inner force) and goals for the hands and feet. The rope
// and the loose limbs do the rest, late and springy.
//
// Movement starts in the core: walking feet follow the spine's own rhythm; a
// punch is the spine twisting first and the arm carried out after it; a jump
// is a crouch the legs spring out of; a grab is a hand that pins and drags.
//
// Modes: on foot, on a board (sideways stance, pushes, carves, ollies) and
// seated in a kart. One-shot actions: punch (left/right), kick, jump or
// ollie, hit (taking one). Held: crouch, reach, grab.

import { createSpine, stepSpine, step, chestFrame, pelvisFrame, place, travelPoint, impulse, limbEnd, frequency, chestIndex } from "./spine.mjs";

const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));
const smooth = (u) => { u = clamp(u, 0, 1); return u * u * (3 - 2 * u); };
const lerp = (a, b, u) => a + (b - a) * u;

export const walkSpeed = 200, runSpeed = 560, boardTop = 950, kartTop = 900;

export function createActor(options = {}) {
  return {
    body: createSpine(options), mode: "foot",
    // What the player is holding: forward/turn in -1..1, and held buttons.
    input: { forward: 0, turn: 0, run: false, crouch: false, reach: false, grab: false, hand: 1, spin: false },
    action: null,                  // the one-shot playing: { name, t, side }
    target: { x: 70, y: 70, z: 20 },  // what reach and grab go for
    grabbed: false, speed: 0, pushPhase: 0, carve: 0, time: 0,
  };
}

// One-shots. Each is a duration and a function from (actor, t in seconds)
// to the pose it asks for, laid over the mode's base pose.
export const actions = {
  punch: { duration: .36 },
  kick: { duration: .5 },
  jump: { duration: .18 },       // the crouch before the legs fire
  hit: { duration: .45 },
};

export function trigger(actor, name, side = 1) {
  const { body } = actor;
  if (name === "jump") {
    if (body.root.air || actor.action?.name === "jump") return;
    if (actor.mode === "kart") return;
  }
  if (name === "hit") {
    // The blow lands on the chest from `side`; the spine takes it first.
    const chest = chestFrame(body);
    impulse(body, chestIndex(body), chest.right.x * side * 560, 60, chest.right.z * side * 560);
  }
  actor.action = { name, t: 0, side };
}

export function setMode(actor, mode) {
  actor.mode = mode;
  actor.body.rhythm.gliding = mode !== "foot";
  actor.speed = Math.hypot(actor.body.root.vx, actor.body.root.vz);
}

// ——— the frame ———

export function stepActor(actor) {
  const { body, input } = actor, root = body.root;
  const pose = basePose(actor);
  if (actor.action) {
    const a = actor.action, spec = actions[a.name];
    layer(actor, pose, a);
    a.t += step;
    if (a.t >= spec.duration) {
      if (a.name === "jump") launch(actor);
      actor.action = null;
    }
  }
  // A spin flings the arms out level, like a helicopter, so whatever a
  // hand holds (the axe) is flung out sideways with it: straight out on
  // foot, wide on a board or in the air. A punch or kick in progress
  // keeps its arms.
  if (input.spin && !["punch", "kick"].includes(actor.action?.name)) {
    const chest = chestFrame(body), o = body.o, grounded = !root.air && actor.mode === "foot";
    for (const [name, side] of [["left", -1], ["right", 1]])
      pose.hands[name] = grounded
        ? { goal: place(chest, 2, 6, side * (o.shoulder + 48)), stiff: .22 }
        : { goal: place(chest, 0, 14, side * (o.shoulder + 46)), stiff: .22 };
  }
  // Holding reach or grab sends a hand (`input.hand`: 1 right, -1 left) to
  // the target; a grab that arrives pins, and then the target drags the body.
  if (input.reach || input.grab) {
    const which = input.hand < 0 ? "left" : "right", hand = body.arms[input.hand < 0 ? 0 : 1], end = limbEnd(hand);
    const close = Math.hypot(end.x - actor.target.x, end.y - actor.target.y, end.z - actor.target.z) < 9;
    if (input.grab && close) actor.grabbed = true;
    pose.hands[which] = { goal: { ...actor.target }, stiff: .3, pin: input.grab && actor.grabbed, reach: true };
  }
  if (!input.grab) actor.grabbed = false;
  body.snap = pose.snap || 1;
  apply(body, pose);
  travel(actor);
  stepSpine(body);
  // A grip lets go before an arm visibly stretches: past 6% over its length
  // the hand slips off.
  if (actor.grabbed) {
    const arm = body.arms[input.hand < 0 ? 0 : 1], reach = Math.hypot(arm.x[2] - arm.x[0], arm.y[2] - arm.y[0], arm.z[2] - arm.z[0]);
    if (reach > (arm.lengths[0] + arm.lengths[1]) * 1.06) { actor.grabbed = false; actor.slipped = actor.time; }
  }
  if (body.landed && actor.mode !== "kart") actor.landedAt = actor.time;
  actor.time += step;
}

// The mode's steady pose: what the spine and limbs do with no one-shot.
function basePose(actor) {
  const { body, input, mode } = actor, o = body.o;
  const pose = { curl: 0, arch: 0, side: 0, twist: 0, yaw: 0, crouch: 0, hands: {}, feet: {} };
  const since = actor.time - (actor.landedAt ?? -9);
  const absorb = since < .3 ? Math.sin(since / .3 * Math.PI) : 0;   // knees soak a landing
  if (mode === "foot") {
    const speed = Math.hypot(body.root.vx, body.root.vz), drive = body.rhythm.drive;
    pose.curl = (input.run ? .25 : .08) * drive + (input.crouch ? .45 : 0) + absorb * .35;
    pose.crouch = (input.crouch ? 34 : 4 * drive) + absorb * 22;
    // Feet follow the spine's rhythm: each foot's phase is the pelvis's,
    // half a cycle apart. Half the cycle a foot is planted and sweeps back
    // at exactly the body's speed (so it doesn't skate); the other half it
    // swings forward through the air.
    // A foot-forward walk: the stride is long for the cadence, the swing
    // foot plants a third of a stride ahead of the pelvis rather than
    // under it, and the feet track wide of the hips. (Measured: the lift
    // is what keeps a planted foot from sliding; the bias and the width
    // cut the slide, 22 -> 16 u/s at a walk.)
    const f = Math.max(.3, frequency(body)), stride = Math.min(110, speed / (4 * f)), lift = 18 + 14 * drive;
    for (const [name, s] of [["left", -1], ["right", 1]]) {
      const phase = body.rhythm.phase + (s > 0 ? Math.PI : 0), { along: sweep, up: rise } = footCycle(phase);
      const along = stride * sweep + stride * .3 * drive, up = rise * lift * drive;
      // A planted foot is pinned where it stands; a swinging one chases.
      pose.feet[name] = { goal: travelPoint(body, along, s * (o.hipWidth + (input.crouch ? 12 : 8)), 3 + up), stiff: .55, pin: !body.root.air && up === 0 && drive > .05 };
      // Arms swing against the legs, loose: a gentle goal, gravity does the rest.
      const chest = chestFrame(body);
      pose.hands[name] = { goal: place(chest, -stride * .7 * Math.sin(phase) * drive + 6, -58 + 10 * drive, s * (o.shoulder + 6)), stiff: .05 };
    }
    if (body.root.air) airPose(pose, body);
  } else if (mode === "board") {
    // Sideways on the deck, knees soft, arms out along the board for balance.
    // Carving leans the body over its toes or heels — the spine's curl/arch.
    pose.yaw = Math.PI / 2;
    // The deck lifts the feet ~13, so the hips stand taller than on the ground.
    pose.crouch = -8 + absorb * 22 + (input.crouch ? 26 : 0);
    pose.curl = Math.max(0, actor.carve) * .9 + absorb * .3 + (input.crouch ? .4 : 0);
    pose.arch = Math.max(0, -actor.carve) * .7;
    const deck = 10, chest = chestFrame(body);
    const pushing = actor.pushPhase > 0;
    for (const [name, s] of [["left", -1], ["right", 1]]) {
      // Front foot over the front truck; back foot over the back one unless
      // it's down pushing, when it strokes the ground beside the deck. Turned
      // sideways, the hip that leads is the one whose side faces the nose —
      // the left one for a stance turned right — so that foot goes forward
      // (the other way round crossed the legs).
      const front = s === (Math.sin(pose.yaw) > 0 ? -1 : 1);
      let goal = travelPoint(body, front ? 20 : -20, 0, deck + 3);
      if (!front && pushing) {
        const u = actor.pushPhase;              // 0..1 through a stroke
        goal = travelPoint(body, lerp(18, -42, u), -16, u < .8 ? 3 : 3 + (u - .8) * 80);
      }
      pose.feet[name] = { goal, stiff: .6 };
      // Arms along the board: the leading arm toward the nose.
      pose.hands[name] = { goal: place(chest, 8, -26, s * 46), stiff: .07 };
    }
    if (pushing) { pose.crouch += 8; pose.curl += .2; }
    if (body.root.air) { pose.crouch = 30; for (const name of ["left", "right"]) pose.feet[name].goal.y += 26; }
  } else if (mode === "kart") {
    // Seated: hips low, feet forward to the pedals, hands on the wheel, the
    // body leaning with the turn.
    pose.crouch = o.hipHeight - 46;
    pose.curl = .15; pose.side = -input.turn * .35 * clamp(actor.speed / kartTop, 0, 1);
    const chest = chestFrame(body);
    for (const [name, s] of [["left", -1], ["right", 1]]) {
      pose.feet[name] = { goal: travelPoint(body, 52, s * 12, 14), stiff: .5 };
      pose.hands[name] = { goal: place(chest, 40, -22, s * 12), stiff: .25 };
    }
  }
  return pose;
}

// Where a foot is in its step, from its phase: `along` -1..1 (back..front)
// and `up` 0..1. Planted for the half where cos < 0, sweeping front to back
// linearly; swinging back to the front for the other half.
function footCycle(phase) {
  const u = ((phase % (Math.PI * 2)) + Math.PI * 2) % (Math.PI * 2);
  if (u >= Math.PI / 2 && u < Math.PI * 1.5) return { along: 1 - 2 * (u - Math.PI / 2) / Math.PI, up: 0 };
  const v = (((u - Math.PI * 1.5) % (Math.PI * 2)) + Math.PI * 2) % (Math.PI * 2) / Math.PI;
  return { along: -1 + 2 * smooth(v), up: Math.sin(v * Math.PI) };
}

// In the air: knees up, arms out and up, the spine curled a little.
function airPose(pose, body) {
  const pelvis = pelvisFrame(body), chest = chestFrame(body), o = body.o;
  pose.curl += .25;
  for (const [name, s] of [["left", -1], ["right", 1]]) {
    pose.feet[name] = { goal: place(pelvis, 22, -62, s * (o.hipWidth + 4)), stiff: .3 };
    pose.hands[name] = { goal: place(chest, 10, 18, s * (o.shoulder + 30)), stiff: .12 };
  }
}

// A one-shot laid over the base pose.
function layer(actor, pose, a) {
  const { body } = actor, o = body.o, t = a.t, s = a.side, chest = chestFrame(body);
  const guard = (hand, side) => ({ goal: place(chest, 26, 8, side * 12), stiff: .2 });
  if (a.name === "punch") {
    // Wind: the spine turns the punching shoulder back. Throw: the spine
    // unwinds past square and the fist rides out on it, a beat behind.
    // Recover: back to guard.
    const name = s > 0 ? "right" : "left", other = s > 0 ? "left" : "right";
    pose.hands[other] = guard(other, -s);
    pose.snap = 4;
    if (t < .08) { pose.twist = -s * .4; pose.hands[name] = { goal: place(chest, 10, 2, s * 16), stiff: .25 }; }
    else if (t < .22) {
      // The unwind starts at .08; the fist is only thrown at .13, so it is
      // carried by a spine already turning.
      pose.twist = s * .6; pose.curl += .15;
      const u = smooth((t - .13) / .07);
      pose.hands[name] = t < .13 ? { goal: place(chest, 14, 4, s * 14), stiff: .25 } : { goal: place(chest, lerp(20, 64, u), 10, s * 2), stiff: .7 };
    } else { pose.twist = s * .6 * (1 - smooth((t - .22) / .14)); pose.hands[name] = guard(name, s); }
  } else if (a.name === "kick") {
    // Chamber the knee, snap the foot out, pull back; the spine leans away
    // from the kick and the arms go wide to balance it.
    const name = s > 0 ? "right" : "left";
    const pelvis = pelvisFrame(body);
    pose.snap = 3;
    if (t < .12) pose.feet[name] = { goal: place(pelvis, 30, -40, s * o.hipWidth), stiff: .4 };
    else if (t < .3) { pose.feet[name] = { goal: place(pelvis, 82, -8, s * o.hipWidth), stiff: .8 }; pose.arch = .45; }
    else pose.feet[name] = { goal: place(pelvis, 26, -50, s * o.hipWidth), stiff: .35 };
    pose.arch = (pose.arch || 0) + .15; pose.twist = -s * .2;
    for (const [hand, side] of [["left", -1], ["right", 1]]) pose.hands[hand] = { goal: place(chest, -12, -14, side * 52), stiff: .12 };
  } else if (a.name === "jump") {
    // The load: crouch and swing the arms back; the legs fire at the end.
    const u = smooth(t / actions.jump.duration);
    pose.crouch += 30 * u; pose.curl += .4 * u;
    for (const [hand, side] of [["left", -1], ["right", 1]]) pose.hands[hand] = { goal: place(chest, -34, -40, side * 24), stiff: .2 };
  } else if (a.name === "hit") {
    // Taking a hit: the spine was shoved in trigger(); here the arms fly up
    // and out and the legs give a little, then it all comes back.
    const u = 1 - smooth(t / actions.hit.duration);
    pose.crouch += 12 * u; pose.side += -s * .5 * u; pose.twist += -s * .3 * u;
    for (const [hand, side] of [["left", -1], ["right", 1]]) pose.hands[hand] = { goal: place(chest, 4, 16, side * 50), stiff: .1 * u };
  }
}

// The legs fire: into the air (a jump on foot, an ollie on a board).
function launch(actor) {
  const root = actor.body.root;
  root.air = true; root.vy = actor.mode === "board" ? 640 : 720;
}

// Send the pose into the body: the spine's intent and each limb's goal.
function apply(body, pose) {
  for (const key of ["curl", "arch", "side", "twist", "yaw", "crouch"]) body.intent[key] = pose[key] || 0;
  for (const [name, index] of [["left", 0], ["right", 1]]) {
    for (const [limbs, goals] of [[body.arms, pose.hands], [body.legs, pose.feet]]) {
      const limb = limbs[index], g = goals[name];
      limb.goal = g?.goal || null; limb.stiff = g?.stiff || 0; limb.pin = !!g?.pin; limb.reach = !!g?.reach;
    }
  }
}

// How the root travels in each mode. A driven actor (the game moves it)
// keeps only the board's bookkeeping — push strokes and carve lean — and
// leaves the root alone.
function travel(actor) {
  const { body, input, mode } = actor, root = body.root;
  if (actor.driven) {
    if (mode === "board") {
      if (input.forward > 0 && !root.air && actor.pushPhase === 0) actor.pushPhase = .001;
      if (actor.pushPhase > 0) { actor.pushPhase += step / .55; if (actor.pushPhase >= 1) actor.pushPhase = 0; }
      actor.carve = lerp(actor.carve, input.turn * clamp(actor.speed / 500, 0, 1), 1 - Math.exp(-6 * step));
    }
    return;
  }
  if (mode === "foot") {
    root.yaw += input.turn * 2.6 * step;
    let want = input.forward > 0 ? (input.run ? runSpeed : walkSpeed) * input.forward : input.forward * 160;
    if (input.crouch) want *= .4;
    // A grabbed hand at full stretch walks the body toward what it holds.
    if (actor.grabbed) {
      const pelvis = pelvisFrame(body), dx = actor.target.x - pelvis.origin.x, dz = actor.target.z - pelvis.origin.z;
      const d = Math.hypot(dx, dz);
      if (d > 50) { root.yaw = Math.atan2(dz, dx); want = Math.min(runSpeed, (d - 50) * 8); }
    }
    const speed = actor.speed = lerp(actor.speed, want, 1 - Math.exp(-(body.root.air ? 1 : 8) * step));
    root.vx = Math.cos(root.yaw) * speed; root.vz = Math.sin(root.yaw) * speed;
  } else if (mode === "board") {
    // Momentum: pushing adds speed in strokes, friction bleeds it, turning
    // carves (sharper when slow) and leans the rider into it.
    if (input.forward > 0 && !body.root.air && actor.pushPhase === 0 && actor.speed < boardTop) actor.pushPhase = .001;
    if (actor.pushPhase > 0) {
      const before = actor.pushPhase;
      actor.pushPhase += step / .55;
      if (before < .25 && actor.pushPhase >= .25) actor.speed = Math.min(boardTop, actor.speed + 190);
      if (actor.pushPhase >= 1) actor.pushPhase = 0;
    }
    if (input.forward < 0) actor.speed *= Math.exp(-2.5 * step);
    actor.speed *= Math.exp(-.12 * step);
    const grip = clamp(actor.speed / 300, .2, 1);
    root.yaw += input.turn * 1.9 * step * grip;
    actor.carve = lerp(actor.carve, input.turn * clamp(actor.speed / 500, 0, 1), 1 - Math.exp(-6 * step));
    root.vx = Math.cos(root.yaw) * actor.speed; root.vz = Math.sin(root.yaw) * actor.speed;
  } else if (mode === "kart") {
    const want = input.forward > 0 ? kartTop * input.forward : input.forward * 300;
    actor.speed = lerp(actor.speed, want, 1 - Math.exp(-(input.forward ? 1.6 : 1) * step));
    root.yaw += input.turn * 2.2 * step * clamp(Math.abs(actor.speed) / 250, 0, 1) * Math.sign(actor.speed || 1);
    root.vx = Math.cos(root.yaw) * actor.speed; root.vz = Math.sin(root.yaw) * actor.speed;
  }
}
