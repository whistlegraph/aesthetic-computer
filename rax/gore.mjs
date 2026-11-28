// 🩸 Gore: what a body does when a round or a blast ends it, drawn on each
// page (the server only says where it was struck: match.mjs hitZone).
// After Unreal Tournament 2004: a body a round ends falls as a ragdoll
// (Karma's PHYS_KarmaRagdoll), one a blast ends comes apart into gibs
// (xPawnGibGroup: head, torso, upper arms, calves, and meat), and a part
// that comes away spurts from where it was (BloodJet) and leaves splats
// (BloodSplatter) on what it lands on.
//
//   ragdoll  Jakobsen's verlet (Hitman, 2001): a point at each joint, a stick
//            for each bone, the torso braced so it stays a box, soft stops
//            so knees and elbows do not fold the wrong way; the floor, the
//            room's walls and what is built there push the points out.
//   gib      a rigid piece that tumbles, bounces off what it meets keeping
//            0.65 of its speed (Gib.DampenFactor) with a fresh spin, trails
//            blood for 1.8 s and splats where it lands hard.
//   spurt    a stump's jet, pulsing with a heartbeat that fades over 3 s.
//   splat    a flat stain on the floor or a wall, one of three shapes, that
//            shrinks away after 40 s; no more than MAX_SPLATS at once.
//
// The view draws what `draws()` returns with its own models (view.mjs):
// { part, seat, x, y, z, yaw, pitch, roll, scale }. `part` is one of torso,
// head, limb, hand (a ragdoll's, drawn from their joints), gib-torso, gib-head,
// gib-limb, gib-hand, gib-meat (flying, centred), drop, splat0, splat1, splat2.
import { surfaceHit } from './world.mjs';

const G = 18; // A little over the world's gravity: a body falls heavily.
const SUB = 3; // Physics steps a frame (verlet holds together better in short steps).
const RELAX = 6; // Constraint passes a step.
const CORPSE_SECONDS = 9;
const GIB_SECONDS = 8; // (Gib.LifeSpan)
const TRAIL_SECONDS = 1.8; // (Gib's trail: LifeSpan 1.8)
const SPURT_SECONDS = 3.2;
const SPLAT_SECONDS = 40;
export const MAX_SPLATS = 96;
const MAX_DROPS = 260;
const DAMPEN = 0.65;

// The body as view.mjs draws it, from the feet (base): joints and lengths.
const LIMB = 0.46, HIP = 0.68, SHOULDER = 1.27, NECK = 1.33, PELVIS = 0.71, CROWN = 1.88;
const HEAD_UP = 0.26, TORSO_UP = 0.31; // a part's middle above the joint it hangs from (head box 1.38–1.80, torso 0.71–1.33)
const JOINTS = ['pelvis', 'neck', 'crown', 'shoulderL', 'shoulderR', 'handL', 'handR', 'hipL', 'hipR', 'footL', 'footR'];
// Bones: [a, b] kept at their rest length. The torso's corners are braced.
const BONES = [
  ['pelvis', 'neck'], ['neck', 'crown'], ['shoulderL', 'shoulderR'], ['hipL', 'hipR'],
  ['shoulderL', 'pelvis'], ['shoulderR', 'pelvis'], ['hipL', 'neck'], ['hipR', 'neck'],
  ['shoulderL', 'hipL'], ['shoulderR', 'hipR'], ['shoulderL', 'neck'], ['shoulderR', 'neck'],
  ['hipL', 'pelvis'], ['hipR', 'pelvis'], ['crown', 'shoulderL'], ['crown', 'shoulderR'],
  ['shoulderL', 'handL'], ['shoulderR', 'handR'], ['hipL', 'footL'], ['hipR', 'footR'],
];
// Soft stops (at least this far apart): a knee cannot fold into the chest, a hand into the face.
const STOPS = [['footL', 'neck', 0.95], ['footR', 'neck', 0.95], ['handL', 'crown', 0.35], ['handR', 'crown', 0.35],
  ['footL', 'footR', 0.12], ['handL', 'hipL', 0.2], ['handR', 'hipR', 0.2]];
// What a zone takes away: the joints that leave with it, the part it becomes, and the stump it leaves.
const ZONES = {
  head: { leaves: ['crown'], part: 'head', stump: 'neck' },
  'arm-left': { leaves: ['handL'], part: 'limb', stump: 'shoulderL', hand: true },
  'arm-right': { leaves: ['handR'], part: 'limb', stump: 'shoulderR', hand: true },
  'leg-left': { leaves: ['footL'], part: 'limb', stump: 'hipL' },
  'leg-right': { leaves: ['footR'], part: 'limb', stump: 'hipR' },
};

const v = (x = 0, y = 0, z = 0) => ({ x, y, z });
const add = (a, b, k = 1) => v(a.x + b.x * k, a.y + b.y * k, a.z + b.z * k);
const sub = (a, b) => v(a.x - b.x, a.y - b.y, a.z - b.z);
const dot = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z;
const cross = (a, b) => v(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
const len = (a) => Math.hypot(a.x, a.y, a.z);
const unit = (a, or = v(0, 1, 0)) => { const l = len(a); return l > 1e-6 ? v(a.x / l, a.y / l, a.z / l) : or; };

// The three angles view.mjs's draw takes (project.mjs: yaw · pitch · roll)
// for a frame whose local x, y, z go along X, Y, Z.
export function anglesOf(X, Y, Z) {
  const pitch = Math.asin(Math.max(-1, Math.min(1, -Z.y)));
  return { yaw: Math.atan2(Z.x, Z.z), pitch, roll: Math.atan2(X.y, Y.y) };
}
// A frame from an up direction and a near-sideways one.
function frame(up, side) {
  const Y = unit(up), X = unit(sub(side, add(v(), Y, dot(side, Y))), v(1, 0, 0)), Z = cross(X, Y);
  return { X, Y, Z };
}

// The joints of a standing body (yaw in degrees, as players carry it).
export function pose({ x, base, z, yaw = 0, pitch = 0 }) {
  const y = (yaw * Math.PI) / 180;
  const L = v(Math.cos(y), 0, -Math.sin(y)); // direction(yaw + 90): the body's left
  const F = v(Math.sin(y), 0, Math.cos(y));
  const at = (side, up, ahead = 0) => v(x + L.x * side + F.x * ahead, base + up, z + L.z * side + F.z * ahead);
  const reach = -0.55 - (pitch * Math.PI) / 180; // the arms held as the view holds them
  const hand = (side) => v(x + L.x * side - F.x * Math.sin(reach) * LIMB, base + SHOULDER - Math.cos(reach) * LIMB, z + L.z * side - F.z * Math.sin(reach) * LIMB);
  return {
    pelvis: at(0, PELVIS), neck: at(0, NECK), crown: at(0, CROWN),
    shoulderL: at(0.3, SHOULDER), shoulderR: at(-0.3, SHOULDER), handL: hand(0.3), handR: hand(-0.3),
    hipL: at(0.15, HIP), hipR: at(-0.15, HIP), footL: at(0.15, HIP - LIMB), footR: at(-0.15, HIP - LIMB),
  };
}

export class Gore {
  constructor({ random = Math.random } = {}) {
    this.random = random;
    this.dolls = [];
    this.gibs = [];
    this.spurts = [];
    this.drops = [];
    this.splats = [];
    this.time = 0;
  }

  clear() {
    this.dolls = []; this.gibs = []; this.spurts = []; this.drops = []; this.splats = [];
  }

  // A body ended: `body` { x, base, z, yaw, pitch, vx, vy, vz }, `e` the match's gib event.
  gib(e, body, seat = e.seat) {
    const joints = pose(body);
    const dir = unit(v(e.dir?.x || 0, e.dir?.y || 0, e.dir?.z || 0), v(0, 0, 1));
    const carry = v(body.vx || 0, body.vy || 0, body.vz || 0);
    const hit = e.hit ? v(e.hit.x, e.hit.y, e.hit.z) : joints.pelvis;
    this.bloodBurst(hit, dir, e.kind === 'blast' ? 70 : 26);
    if (e.kind === 'blast') return this.explode(joints, hit, dir, carry, seat);
    const zone = ZONES[e.zone];
    const doll = this.ragdoll(joints, carry, seat);
    // The round's push, strongest at the point it struck.
    for (const name of JOINTS) {
      const p = doll.points[name], near = Math.max(0.15, 1 - len(sub(p.at, hit)) / 1.2);
      p.was = sub(p.was, v(dir.x * 0.09 * near, -0.03 * near, dir.z * 0.09 * near));
    }
    if (zone) {
      // The part comes away along the round and spins; the stump spurts.
      for (const j of zone.leaves) doll.gone.add(j);
      const from = joints[zone.stump], to = joints[zone.leaves[0]];
      const fling = add(add(carry, dir, 7 + this.random() * 4), v(0, 3 + this.random() * 3, 0));
      const side = sub(joints.shoulderL, joints.shoulderR);
      if (zone.part === 'head') {
        const f = frame(sub(to, from), side);
        this.piece('head', seat, add(from, f.Y, HEAD_UP), f, fling, 'head');
      } else {
        const f = frame(sub(from, to), side);
        this.piece('limb', seat, add(from, f.Y, -LIMB / 2), f, fling, 'limb');
        if (zone.hand) this.piece('hand', seat, to, f, add(fling, v(0, 1, 0)), 'hand');
      }
      this.spurts.push({ doll, joint: zone.stump, away: zone.leaves[0], born: this.time, next: 0 });
    }
    return doll;
  }

  ragdoll(joints, carry, seat) {
    const points = {};
    for (const name of JOINTS) {
      const at = { ...joints[name] };
      points[name] = { at, was: sub(at, v(carry.x / 60, carry.y / 60, carry.z / 60)) };
    }
    const rest = BONES.map(([a, b]) => [a, b, len(sub(joints[a], joints[b]))]);
    const doll = { seat, points, rest, gone: new Set(), born: this.time };
    this.dolls.push(doll);
    while (this.dolls.length > 8) this.dolls.shift();
    return doll;
  }

  // A blast: every part its own piece, and meat besides.
  explode(joints, hit, dir, carry, seat) {
    const out = (at, k = 1) => add(add(carry, unit(add(sub(at, hit), dir, 0.6)), (6 + this.random() * 7) * k), v(0, 4 + this.random() * 5, 0));
    const torsoUp = sub(joints.neck, joints.pelvis), side = sub(joints.shoulderL, joints.shoulderR), body = frame(torsoUp, side);
    this.piece('torso', seat, add(joints.pelvis, body.Y, TORSO_UP), body, out(joints.pelvis, 0.6), 'torso');
    this.piece('head', seat, add(joints.neck, body.Y, HEAD_UP), body, out(joints.crown, 1.2), 'head');
    for (const [a, b] of [['shoulderL', 'handL'], ['shoulderR', 'handR'], ['hipL', 'footL'], ['hipR', 'footR']]) {
      const f = frame(sub(joints[a], joints[b]), side);
      this.piece('limb', seat, add(joints[a], f.Y, -LIMB / 2), f, out(joints[b]), 'limb');
    }
    for (const h of ['handL', 'handR']) this.piece('hand', seat, joints[h], frame(torsoUp, side), out(joints[h], 1.3), 'hand');
    for (let k = 0; k < 9; k++) this.piece('meat', seat, add(joints.pelvis, v((this.random() - 0.5) * 0.4, this.random() * 0.6, (this.random() - 0.5) * 0.4)), frame(v(this.random() - 0.5, 1, this.random() - 0.5), v(1, 0, 0)), out(joints.neck, 1.4), 'meat');
    return null;
  }

  // A flying piece, placed by its middle (the view's gib- models are centred, so it spins about itself).
  piece(part, seat, at, f, velocity, kind) {
    const a = anglesOf(f.X, f.Y, f.Z);
    this.gibs.push({ part: `gib-${part}`, seat, at: { ...at }, vel: { ...velocity }, ...a,
      spin: v((this.random() - 0.5) * 16, (this.random() - 0.5) * 16, (this.random() - 0.5) * 16),
      born: this.time, trail: 0, resting: false, kind, scale: kind === 'meat' ? 0.6 + this.random() * 0.8 : 1 });
    while (this.gibs.length > 48) this.gibs.shift();
  }

  bloodBurst(at, dir, count) {
    for (let k = 0; k < count; k++)
      this.drop(at, add(v((this.random() - 0.5) * 5, this.random() * 4, (this.random() - 0.5) * 5), dir, 2 + this.random() * 4));
  }

  drop(at, velocity, size = 1) {
    if (this.drops.length >= MAX_DROPS) this.drops.shift();
    this.drops.push({ at: { ...at }, vel: { ...velocity }, size: (0.5 + this.random() * 0.8) * size, born: this.time });
  }

  splat(at, normal, size = 1) {
    if (this.splats.length >= MAX_SPLATS) this.splats.shift();
    this.splats.push({ at: add(at, normal, 0.012), normal: { ...normal }, size: (0.5 + this.random() * 0.9) * size,
      turn: this.random() * Math.PI * 2, shape: Math.floor(this.random() * 3), born: this.time });
  }

  // Move everything on `dt` seconds. `solids` and `half` as world.mjs surfaceHit takes them.
  // (`slab`: the room is a roof, world.mjs slabOf: nothing stands at its edge and there is no floor past it. Blood went on to the air there as on to a wall, and nothing fell off, until 2026-11-28.)
  step(dt, solids = [], half, slab = null) {
    this.slab = slab;
    dt = Math.min(0.05, Math.max(0, dt));
    this.time += dt;
    const h = dt / SUB;
    for (let s = 0; s < SUB; s++) for (const d of this.dolls) this.stepDoll(d, h, solids, half);
    for (const g of this.gibs) this.stepGib(g, dt, solids, half);
    for (const sp of this.spurts) this.stepSpurt(sp);
    for (const d of this.drops) this.stepDrop(d, dt, solids, half);
    this.drops = this.drops.filter((d) => !d.dead && this.time - d.born < 4);
    this.gibs = this.gibs.filter((g) => this.time - g.born < GIB_SECONDS);
    this.dolls = this.dolls.filter((d) => this.time - d.born < CORPSE_SECONDS);
    this.spurts = this.spurts.filter((sp) => this.time - sp.born < SPURT_SECONDS && this.dolls.includes(sp.doll));
    this.splats = this.splats.filter((p) => this.time - p.born < SPLAT_SECONDS);
  }

  stepDoll(d, h, solids, half) {
    const ps = d.points;
    for (const name of JOINTS) {
      const p = ps[name], vel = sub(p.at, p.was);
      p.was = { ...p.at };
      p.at = add(add(p.at, vel, 0.995), v(0, -G * h * h, 0));
    }
    for (let k = 0; k < RELAX; k++) {
      for (const [a, b, rest] of d.rest) {
        if (d.gone.has(a) || d.gone.has(b)) continue;
        const pa = ps[a], pb = ps[b], gap = sub(pb.at, pa.at), l = len(gap) || 1e-6, fix = (l - rest) / l / 2;
        pa.at = add(pa.at, gap, fix);
        pb.at = add(pb.at, gap, -fix);
      }
      for (const [a, b, least] of STOPS) {
        if (d.gone.has(a) || d.gone.has(b)) continue;
        const pa = ps[a], pb = ps[b], gap = sub(pb.at, pa.at), l = len(gap) || 1e-6;
        if (l >= least) continue;
        const fix = (l - least) / l / 2;
        pa.at = add(pa.at, gap, fix);
        pb.at = add(pb.at, gap, -fix);
      }
      for (const name of JOINTS) this.collide(ps[name], solids, half);
    }
  }

  // A point pushed out of the floor, the room's walls and what is built; the floor holds it back (friction).
  collide(p, solids, half) {
    const floor = 0.05;
    if (p.at.y < floor && (!this.slab || (Math.abs(p.at.x) <= half && Math.abs(p.at.z) <= half && p.at.y > -0.6))) {
      p.at.y = floor;
      p.was.x = p.at.x - (p.at.x - p.was.x) * 0.6;
      p.was.z = p.at.z - (p.at.z - p.was.z) * 0.6;
    }
    if (half && !this.slab) for (const k of ['x', 'z']) p.at[k] = Math.max(-half + 0.08, Math.min(half - 0.08, p.at[k]));
    const move = sub(p.at, p.was), l = len(move);
    if (l > 1e-4 && solids.length) {
      const hit = surfaceHit(p.was, unit(move), l, solids, 0, half, null, this.slab);
      if (hit && hit.normal.y <= 0.5) p.at = add(p.was, unit(move), Math.max(0, hit.t - 0.03));
    }
  }

  stepGib(g, dt, solids, half) {
    if (g.resting) return;
    g.vel.y -= G * dt;
    const speed = len(g.vel), dist = speed * dt;
    const hit = dist > 1e-5 ? surfaceHit(g.at, unit(g.vel), dist, solids, 0, half, null, this.slab) : null;
    if (hit) {
      g.at = add(g.at, unit(g.vel), Math.max(0, hit.t - 0.02));
      const n = hit.normal, d = dot(g.vel, n);
      g.vel = add(g.vel, n, -2 * d);
      g.vel = v(g.vel.x * DAMPEN, g.vel.y * DAMPEN, g.vel.z * DAMPEN);
      g.spin = v((this.random() - 0.5) * 14, (this.random() - 0.5) * 14, (this.random() - 0.5) * 14);
      if (speed > 2.5 && g.kind !== 'drop') this.splat(add(g.at, n, -0.02), n, g.kind === 'meat' ? 0.7 : 1.1);
      if (len(g.vel) < 0.6 && n.y > 0.5) { g.resting = true; g.vel = v(); }
    } else g.at = add(g.at, g.vel, dt);
    if (half && !this.slab) for (const k of ['x', 'z']) if (Math.abs(g.at[k]) > half - 0.1) { g.at[k] = Math.sign(g.at[k]) * (half - 0.1); g.vel[k] *= -DAMPEN; }
    g.yaw += g.spin.y * dt; g.pitch += g.spin.x * dt; g.roll += g.spin.z * dt;
    // The trail: drops shed as it flies, for its first 1.8 s.
    if (this.time - g.born < TRAIL_SECONDS && (g.trail += dt) > 0.035) {
      g.trail = 0;
      this.drop(g.at, add(g.vel, v((this.random() - 0.5) * 0.6, 0, (this.random() - 0.5) * 0.6), 0.2), 0.6);
    }
  }

  stepSpurt(sp) {
    const age = this.time - sp.born;
    // A heartbeat (about 100 a minute) that weakens: each beat throws a jet of drops.
    const beat = (age * 1.7) % 1, strength = Math.max(0, 1 - age / SPURT_SECONDS);
    if (beat > 0.35 || this.time < sp.next) return;
    sp.next = this.time + 0.02;
    const p = sp.doll.points[sp.joint], away = sp.doll.points[sp.away] || p;
    const out = unit(sub(p.at, sp.doll.points.pelvis.at), v(0, 1, 0)); // (out from the body's middle)
    const along = unit(add(out, unit(sub(away.at, p.at)), 0.3), out);
    for (let k = 0; k < 3; k++)
      this.drop(p.at, add(v((this.random() - 0.5) * 0.8, 1 + this.random(), (this.random() - 0.5) * 0.8), along, (3 + this.random() * 2.5) * strength), 0.8);
  }

  stepDrop(d, dt, solids, half) {
    d.vel.y -= 22 * dt;
    d.vel = v(d.vel.x * (1 - 0.6 * dt), d.vel.y, d.vel.z * (1 - 0.6 * dt)); // (air: BloodSpurt's mAirResistance)
    const speed = len(d.vel), dist = speed * dt;
    const hit = dist > 1e-5 ? surfaceHit(d.at, unit(d.vel), dist, solids, 0, half, null, this.slab) : null;
    if (hit) {
      d.dead = true;
      if (this.random() < 0.18) this.splat(add(d.at, unit(d.vel), hit.t), hit.normal, 0.35 * d.size);
      return;
    }
    d.at = add(d.at, d.vel, dt);
  }

  // What to draw this frame.
  draws() {
    const out = [];
    for (const d of this.dolls) this.drawDoll(d, out);
    for (const g of this.gibs) out.push({ part: g.part, seat: g.seat, x: g.at.x, y: g.at.y, z: g.at.z, yaw: g.yaw, pitch: g.pitch, roll: g.roll, scale: g.scale });
    for (const d of this.drops) out.push({ part: 'drop', x: d.at.x, y: d.at.y, z: d.at.z, yaw: 0, pitch: 0, roll: 0, scale: d.size });
    for (const p of this.splats) {
      const age = this.time - p.born, fade = age > SPLAT_SECONDS - 6 ? Math.max(0.05, (SPLAT_SECONDS - age) / 6) : 1;
      const f = frame(p.normal, Math.abs(p.normal.y) > 0.9 ? v(Math.cos(p.turn), 0, Math.sin(p.turn)) : v(0, 1, 0));
      const a = anglesOf(f.X, f.Y, f.Z);
      out.push({ part: `splat${p.shape}`, x: p.at.x, y: p.at.y, z: p.at.z, ...a, scale: p.size * fade });
    }
    return out;
  }

  drawDoll(d, out) {
    const P = (n) => d.points[n].at;
    const side = sub(P('shoulderL'), P('shoulderR'));
    const torso = frame(sub(P('neck'), P('pelvis')), side);
    const put = (part, origin, f) => out.push({ part, seat: d.seat, x: origin.x, y: origin.y, z: origin.z, ...anglesOf(f.X, f.Y, f.Z), scale: 1 });
    put('torso', add(P('pelvis'), torso.Y, -PELVIS), torso);
    if (!d.gone.has('crown')) {
      const head = frame(sub(P('crown'), P('neck')), side);
      put('head', add(P('neck'), head.Y, -NECK), head);
    }
    for (const [a, b] of [['shoulderL', 'handL'], ['shoulderR', 'handR'], ['hipL', 'footL'], ['hipR', 'footR']]) {
      if (d.gone.has(b)) continue;
      const f = frame(sub(P(a), P(b)), side);
      put('limb', P(a), f);
      if (b.startsWith('hand')) put('hand', P(b), f);
    }
  }
}
