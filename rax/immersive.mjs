// One tracking origin for looking, walking and both eyes. Rax faces +Z at
// yaw zero. WebXR faces -Z and its positive X is the wearer's right.
import { stick } from './controller.mjs';

const degrees = 180 / Math.PI;
// WebXR names each digit's joints in wrist-to-tip order.
const digits = ['thumb', 'index-finger', 'middle-finger', 'ring-finger', 'pinky-finger'];
const chains = digits.map(digit => ['wrist', ...['metacarpal', 'phalanx-proximal',
  ...(digit === 'thumb' ? [] : ['phalanx-intermediate']), 'phalanx-distal', 'tip'].map(part => digit + '-' + part)]);

export class Headset {
  update(pose, sources, entryYaw, frame, space, pads = []) {
    const m = pose.transform.matrix;
    const headYaw = Math.atan2(m[8], m[10]);
    if (!this.origin) {
      this.origin = [m[12], m[13], m[14]];
      this.turn = entryYaw / degrees - headYaw;
      this.snapped = false;
      this.jumpHeld = false;
    }
    let right = 0, fwd = 0, turnStick = 0, jumpHeld = false, viewHeld = false;
    for (const pad of pads || []) {
      if (!pad?.connected || pad.mapping !== 'standard') continue;
      const [x, y] = stick(pad.axes[0], pad.axes[1]);
      if (Math.hypot(x, y) > Math.hypot(right, fwd)) { right = x; fwd = -y; }
      if (Math.abs(pad.axes[2]) > Math.abs(turnStick)) turnStick = pad.axes[2];
      jumpHeld ||= !!pad.buttons[0]?.pressed;
    }
    for (const source of sources) {
      const pad = source.gamepad;
      if (source.hand || !pad) continue;
      const base = pad.axes.length >= 4 ? 2 : 0;
      if (source.handedness === 'left') {
        const [x, y] = stick(pad.axes[base], pad.axes[base + 1]);
        if (x || y) { right = x; fwd = -y; }
      } else if (source.handedness === 'right') {
        turnStick = pad.axes[base] || turnStick;
        jumpHeld ||= !!pad.buttons[4]?.pressed;
      }
      viewHeld ||= !!pad.buttons[5]?.pressed; // (B on the right hand, Y on the left: the next view)
    }
    if (!this.snapped && Math.abs(turnStick) > 0.7) {
      this.turn -= Math.sign(turnStick) * Math.PI / 6;
      this.snapped = true;
    }
    if (Math.abs(turnStick) < 0.35) this.snapped = false;
    const c = Math.cos(this.turn), s = Math.sin(this.turn);
    const rotate = (x, y, z) => [-c * x - s * z, y, s * x - c * z];
    const offset = rotate(m[12] - this.origin[0], m[13] - this.origin[1], m[14] - this.origin[2]);
    // Hand and controller positions are in metres relative to this frame's
    // head. The client adds the same world-space eye it gives the renderer.
    const point = ({ x, y, z }) => {
      const [dx, dy, dz] = rotate(x - m[12], y - m[13], z - m[14]);
      return { x: dx, y: dy, z: dz };
    };
    this.point = point; // (kept for roomPlanes: the same way from the headset's space to the head's)
    const hands = [], rays = [];
    let pinchDistance = Infinity;
    for (const source of sources) {
      const side = source.handedness;
      if (side !== 'left' && side !== 'right') continue;
      if (source.hand && frame?.getJointPose) {
        const joints = new Map();
        for (const [name, joint] of source.hand) {
          const tracked = frame.getJointPose(joint, space);
          if (tracked) joints.set(name, { ...point(tracked.transform.position), radius: tracked.radius || 0.008 });
        }
        const fingers = chains.map(chain => chain.map(name => joints.get(name)));
        if (joints.size) hands.push({ side, fingers });
        const thumb = joints.get('thumb-tip'), middle = joints.get('middle-finger-tip');
        if (side === 'left' && thumb && middle) pinchDistance = Math.hypot(thumb.x-middle.x, thumb.y-middle.y, thumb.z-middle.z);
      } else if (source.targetRaySpace && frame?.getPose) {
        const ray = frame.getPose(source.targetRaySpace, space);
        if (!ray) continue;
        const mat = ray.transform.matrix;
        const [x, y, z] = rotate(-mat[8], -mat[9], -mat[10]);
        rays.push({ side, origin: point(ray.transform.position), dir: {x, y, z}, down: !!source.gamepad?.buttons?.[0]?.pressed });
      }
    }
    this.walkPinch = pinchDistance < (this.walkPinch ? 0.035 : 0.018);
    // Cull against a cone enclosing both eye frusta, including eye separation
    // at Rax's near plane. The GPU clips to the individual display edges.
    let extent = 1;
    for (const view of pose.views) {
      const eye = view.transform.matrix, projection = view.projectionMatrix;
      const separation = Math.hypot(eye[12] - m[12], eye[13] - m[13], eye[14] - m[14]);
      for (const x of [-1, 1]) for (const y of [-1, 1]) {
        const tx = (x + projection[8]) / projection[0], ty = (y + projection[9]) / projection[5];
        const dx = eye[0] * tx + eye[4] * ty - eye[8];
        const dy = eye[1] * tx + eye[5] * ty - eye[9];
        const dz = eye[2] * tx + eye[6] * ty - eye[10];
        const depth = Math.max(0.001, -(m[8] * dx + m[9] * dy + m[10] * dz));
        for (const i of [0, 4]) {
          const slope = Math.abs((m[i] * dx + m[i + 1] * dy + m[i + 2] * dz) / depth);
          extent = Math.max(extent, slope + separation * (1 + slope) / 0.05);
        }
      }
    }
    const jump = jumpHeld && !this.jumpHeld;
    this.jumpHeld = jumpHeld;
    const viewPress = viewHeld && !this.viewHeld;
    this.viewHeld = viewHeld;
    return {
      cullScale: 1 / extent, offset, rightAxis: rotate(m[0], m[1], m[2]), up: rotate(m[4], m[5], m[6]),
      forward: rotate(-m[8], -m[9], -m[10]),
      yaw: ((this.turn + headYaw) * degrees % 360 + 360) % 360,
      pitch: Math.asin(Math.max(-1, Math.min(1, -m[9]))) * degrees,
      right, fwd, jump, viewPress, hands, rays, walkPinch: this.walkPinch,
    };
  }
}

// Upload Rax's visible triangles once. Both eyes draw that same geometry
// through their own tracked transform and asymmetric projection.
const vertex = `#version 300 es
layout(location=0) in vec3 position;
layout(location=1) in vec3 ink;
uniform mat4 head, eye, projection;
uniform vec2 tangent;
uniform float world;
out vec3 color;
void main() {
  bool sky = position.z < 0.;
  float z = sky ? 10000. : position.z;
  // (world: metres of the room to a metre of yours. 1 is life size; a twentieth is the room on a table. The sky stays where it is.)
  float k = sky ? 1. : world;
  gl_Position = projection * eye * head * vec4(position.xy * tangent * z * k, -z * k, 1.);
  if (sky) gl_Position.z = (1. + position.z * .000001) * gl_Position.w;
  color = ink;
}`;
const fragment = `#version 300 es
precision mediump float;
in vec3 color;
uniform float opacity;
out vec4 pixel;
void main() { pixel = vec4(color * opacity, opacity); }`;

export function stereoRenderer(gl) {
  const program = gl.createProgram();
  for (const [type, source] of [[gl.VERTEX_SHADER, vertex], [gl.FRAGMENT_SHADER, fragment]]) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    gl.attachShader(program, shader);
    gl.deleteShader(shader);
  }
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    const error = gl.getProgramInfoLog(program);
    gl.deleteProgram(program);
    throw new Error(error);
  }
  const uniforms = Object.fromEntries(['head', 'eye', 'projection', 'tangent', 'opacity', 'world'].map(k => [k, gl.getUniformLocation(program, k)]));
  const buffer = gl.createBuffer();
  gl.useProgram(program);
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  for (let i = 0; i < 2; i++) {
    gl.enableVertexAttribArray(i);
    gl.vertexAttribPointer(i, 3, gl.FLOAT, false, 24, i * 12);
  }
  gl.enable(gl.DEPTH_TEST);
  gl.depthFunc(gl.LEQUAL);
  let capacity = 0;
  return (scene, sky, pose, layer, projector, mixed, world = 1) => {
    gl.bindFramebuffer(gl.FRAMEBUFFER, layer.framebuffer);
    gl.uniform1f(uniforms.world, world);
    if (mixed) gl.clearColor(0, 0, 0, 0);
    else gl.clearColor(...sky);
    gl.uniform1f(uniforms.opacity, mixed ? 0.66 : 1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    if (scene.vertices.byteLength > capacity) {
      capacity = scene.vertices.byteLength;
      gl.bufferData(gl.ARRAY_BUFFER, capacity, gl.DYNAMIC_DRAW);
    }
    gl.bufferSubData(gl.ARRAY_BUFFER, 0, scene.frameVertices());
    gl.uniformMatrix4fv(uniforms.head, false, pose.transform.matrix);
    gl.uniform2f(uniforms.tangent, 1 / projector.sx, 1 / projector.sy);
    for (const view of pose.views) {
      const v = layer.getViewport(view);
      gl.viewport(v.x, v.y, v.width, v.height);
      gl.uniformMatrix4fv(uniforms.eye, false, view.transform.inverse.matrix);
      gl.uniformMatrix4fv(uniforms.projection, false, view.projectionMatrix);
      gl.drawArrays(gl.TRIANGLES, 0, scene.triangleCount * 3);
    }
  };
}

export function immersive({ frame, getYaw, transition, release }) {
  const xr = navigator.xr;
  let open = false, modes = [], busy = false, message = '', session = null;
  let gl, draw, desktopFrame, probeId = 0;
  function desktop(now) {
    desktopFrame = window.requestAnimationFrame(desktop);
    frame(now);
  }
  desktopFrame = window.requestAnimationFrame(desktop);
  async function probe() {
    const id = ++probeId;
    const supported = await Promise.all(['immersive-ar', 'immersive-vr'].map(async mode => {
      try { return await xr.isSessionSupported(mode) ? mode : null; } catch { return null; }
    }));
    if (id === probeId) modes = supported.filter(Boolean);
  }
  async function enter(mode) {
    if (busy || session || !open || !modes.includes(mode)) return;
    busy = true; message = '';
    const mixed = mode === 'immersive-ar';
    const boundaryHint = 'Set Roomscale in Quest. Retry MR.';
    let next, started = false;
    let requestingSpace = true;
    try {
      next = await xr.requestSession(mode, {
        requiredFeatures: mixed ? ['bounded-floor'] : [],
        // 🧱 (plane-detection: the real room's walls, floor, tables and screens, where the headset knows them. A probe for now: roomPlanes, below.)
        optionalFeatures: ['hand-tracking', 'plane-detection'],
      });
      session = next;
      next.addEventListener('end', () => {
        if (session !== next) return;
        session = null;
        if (started) {
          transition(false);
          desktopFrame = window.requestAnimationFrame(desktop);
        }
      }, { once: true });
      const space = await next.requestReferenceSpace(mixed ? 'bounded-floor' : 'local');
      requestingSpace = false;
      if (!draw || gl.isContextLost()) {
        draw = null;
        gl = document.createElement('canvas').getContext('webgl2', { xrCompatible: true, antialias: false });
        if (!gl) throw new Error('WebGL 2 unavailable');
        await gl.makeXRCompatible();
        draw = stereoRenderer(gl);
      }
      if (session !== next || !open) { await next.end(); return; }
      const layer = new XRWebGLLayer(next, gl, { alpha: true, antialias: false, framebufferScaleFactor: 0.8, ignoreDepthValues: true });
      if (typeof layer.fixedFoveation === 'number') layer.fixedFoveation = Math.max(0.5, layer.fixedFoveation);
      next.updateRenderState({ baseLayer: layer, depthNear: 0.05, depthFar: 150000 });
      if (mixed && next.environmentBlendMode === 'opaque') throw new Error('Room blending unavailable');
      let rig = new Headset();
      let missingSince = null;
      const reset = () => { rig = new Headset(); release(); };
      space.addEventListener('reset', reset);
      next.addEventListener('visibilitychange', () => { if (next.visibilityState !== 'visible') release(); });
      function tick(now, xrFrame) {
        if (session !== next) return;
        const visible = next.visibilityState === 'visible';
        // Bounds can arrive after session entry or disappear during room setup.
        // Let the compositor show the room while it resolves the play area.
        if (mixed && space.boundsGeometry.length < 3) {
          if (missingSince === null) release();
          missingSince = visible ? missingSince ?? now : null;
          gl.bindFramebuffer(gl.FRAMEBUFFER, layer.framebuffer);
          gl.clearColor(0, 0, 0, 0);
          gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
          if (visible && now - missingSince >= 4000) {
            message = boundaryHint;
            void next.end();
            return;
          }
          next.requestAnimationFrame(tick);
          return;
        }
        missingSince = null;
        next.requestAnimationFrame(tick);
        const pose = xrFrame.getViewerPose(space);
        if (!pose) { release(); return; }
        const input = rig.update(pose, visible ? next.inputSources : [], getYaw(), xrFrame, space, visible ? navigator.getGamepads?.() : []);
        frame(now, { ...input, mixed, pose, layer, draw, room: roomPlanes(xrFrame, space, rig.point, next) });
      }
      transition(true);
      window.cancelAnimationFrame(desktopFrame);
      started = true;
      next.requestAnimationFrame(tick);
    } catch (e) {
      if (next && session === next) await next.end().catch(() => {});
      if (open) {
        if (e.name === 'NotAllowedError') message = 'Permission declined. Try again.';
        else if (mixed && requestingSpace && e.name === 'NotSupportedError') message = boundaryHint;
        else message = 'Headset mode unavailable. Try again.';
      }
      console.warn('Rax VR', e);
    } finally { busy = false; }
  }
  if (xr) { void probe(); xr.addEventListener('devicechange', probe); }
  return {
    get supported() { return modes.length > 0; },
    get controls() { return open && !session ? { modes, message, busy } : null; },
    enter,
    show(value) {
      open = value;
      if (!open && session) void session.end().catch(() => {});
      if (!open) message = '';
    },
  };
}

// 🖐️ The left wrist and the right hand's pointing tip, in metres from the
// head (as `hands` and `rays` are): a tracked hand's own joints, or else the
// controller held in that hand. The wrist slate stands a little over the
// first and is poked with the second (client.mjs, view.mjs).
export const SLATE_LIFT = 0.09, SLATE_REACH = 0.055;
export function wristOf(xr) {
  const hand = xr?.hands?.find((h) => h.side === 'left');
  return hand?.fingers?.[0]?.[0] || xr?.rays?.find((r) => r.side === 'left')?.origin || null;
}
export function pokerOf(xr) {
  const hand = xr?.hands?.find((h) => h.side === 'right');
  return hand?.fingers?.[1]?.at(-1) || xr?.rays?.find((r) => r.side === 'right')?.origin || null;
}
// 🪑 The room on a table: where the eye is in the room when the room is
// `table.scale` of its size and its floor's middle (`pivot`) stands at
// `table.at` (metres from where the headset began, as `offset` is). A head
// that moves a hand's width moves that many times further over the room.
export function tableEye(table, offset) {
  const k = 1 / table.scale;
  return { x: table.pivot.x + (offset[0] - table.at[0]) * k, y: table.pivot.y + (offset[1] - table.at[1]) * k, z: table.pivot.z + (offset[2] - table.at[2]) * k };
}

// 📡 Hands for the room: each tracked hand as one flat list of its joints in
// the world's own metres (thumb to little finger, wrist to tip: 5 + 6 × 4
// joints, three numbers each) and one thickness, so everyone there sees them
// where they are. `eye` is where the head is in the room and `k` how many
// times life size the hands stand (the room on a table: twenty, and the
// hands are in the sky). handsUnpacked gives back what handOutline reads.
export const HAND_NUMBERS = 29 * 3;
export function handsPacked(xr, eye, k = 1) {
  const r3 = (v) => Math.round(v * 1000) / 1000;
  // (A joint the headset did not give this frame stands where the nearest one it did give is: a hand seen in part is still sent, whole. Only a hand with no wrist and no joint at all is left out.)
  return (xr?.hands || []).map((h) => {
    const any = h.fingers.flat().find(Boolean);
    return any && { ...h, fingers: h.fingers.map((chain) => { let last = chain.find(Boolean) || any; return chain.map((p) => (last = p || last)); }) };
  }).filter(Boolean).slice(0, 2).map((h) => ({
    side: h.side, r: r3((h.fingers[1][2].radius || 0.008) * k),
    j: h.fingers.flatMap((chain) => chain.flatMap((p) => [r3(eye.x + p.x * k), r3(eye.y + p.y * k), r3(eye.z + p.z * k)])),
  }));
}
export function handsValid(hands) {
  return Array.isArray(hands) && hands.length <= 2 && hands.every((h) => (h?.side === 'left' || h?.side === 'right') &&
    Number.isFinite(h.r) && h.r > 0 && h.r < 2 && Array.isArray(h.j) && h.j.length === HAND_NUMBERS && h.j.every((n) => Number.isFinite(n) && Math.abs(n) < 60000));
}
export function handsUnpacked(hands) {
  return hands.map((h) => {
    let at = 0;
    const fingers = [5, 6, 6, 6, 6].map((n) => Array.from({ length: n }, () => ({ x: h.j[at++], y: h.j[at++], z: h.j[at++], radius: h.r })));
    return { side: h.side, fingers };
  });
}

// ✋ A hand's outline from its joints (`fingers`: thumb to little finger, each
// wrist-to-tip): one closed line of points round the hand, or null when a
// joint it needs is not tracked. Each finger is as wide as its own joints'
// radius, measured across the palm (index knuckle to little knuckle); the
// thumb across its own way (wrist to index knuckle).
export function handOutline(fingers) {
  if (fingers?.length !== 5 || fingers.some((chain) => chain.length < 5 || chain.some((j) => !j))) return null;
  const wrist = fingers[0][0], unit = (a, b) => {
    const d = { x: b.x - a.x, y: b.y - a.y, z: b.z - a.z }, n = Math.hypot(d.x, d.y, d.z) || 1;
    return { x: d.x / n, y: d.y / n, z: d.z / n };
  };
  const across = unit(fingers[4][2], fingers[1][2]), along = unit(wrist, fingers[1][2]);
  const off = (j, dir, sign) => { const r = (j.radius || 0.008) * sign; return { x: j.x + dir.x * r, y: j.y + dir.y * r, z: j.z + dir.z * r }; };
  const out = [off(wrist, across, -2.6)];
  for (let f = 4; f >= 1; f--) {
    const joints = fingers[f].slice(2);
    for (const j of joints) out.push(off(j, across, -1));
    for (const j of joints.reverse()) out.push(off(j, across, 1));
  }
  const thumb = fingers[0].slice(1);
  for (const j of thumb) out.push(off(j, along, 1));
  for (const j of thumb.reverse()) out.push(off(j, along, -1));
  out.push(off(wrist, across, 2.6));
  return out;
}

// 🧱 The real room, as far as the headset tells a web page (WebXR plane
// detection): `asked` whether the feature was granted, `count` and `labels`
// (wall, floor, table, screen… as the headset's room setup named them: how
// many of each), and each plane's outline in metres from the head, as hands
// are. A probe (2026-11-27): the wrist card says what came back and the
// outlines are drawn, so one look in mixed reality shows what there is to
// build on. `point` is Headset's own way from the headset's space to the head's.
export function roomPlanes(frame, space, point, session) {
  const asked = !!session?.enabledFeatures?.includes?.('plane-detection');
  const planes = frame?.detectedPlanes;
  if (!planes || !point) return { asked, known: !!planes, count: 0, labels: {}, outlines: [] };
  const labels = {}, outlines = [];
  for (const plane of planes) {
    const label = plane.semanticLabel || 'unnamed';
    labels[label] = (labels[label] || 0) + 1;
    if (outlines.length >= 24) continue;
    const pose = frame.getPose(plane.planeSpace, space);
    if (!pose || !plane.polygon?.length) continue;
    const m = pose.transform.matrix;
    outlines.push({ label, points: plane.polygon.slice(0, 32).map((p) => point({
      x: m[0] * p.x + m[4] * p.y + m[8] * p.z + m[12], y: m[1] * p.x + m[5] * p.y + m[9] * p.z + m[13], z: m[2] * p.x + m[6] * p.y + m[10] * p.z + m[14] })) });
  }
  return { asked, known: true, count: planes.size ?? outlines.length, labels, outlines };
}

// ✊ Two fists take hold of the world. A hand is a fist when its four
// fingertips are curled in by the wrist; where it holds is its middle knuckle.
// With both closed, the two points of the room under them stay under them:
// move both and the room goes along (pan), part or close them and it grows or
// shrinks (scale), turn one round the other and it turns (rotate, about the
// upright). So the room is walked, flown and sized with two hands alone.
// A table (tableEye) is the room placed: { scale, pivot, at, rot }, where the
// room's point under a real point q (metres from where the headset began) is
//   pivot + (turn(q, rot) - at) / scale.
export const FIST_CURL = 0.095;
// (`k`: how many times life size the hand stands: someone else's, seen in the room.)
export function fistOf(hand, k = 1) {
  const wrist = hand?.fingers?.[0]?.[0], hold = hand?.fingers?.[2]?.[2];
  if (!wrist || !hold) return null;
  const tips = hand.fingers.slice(1).map((chain) => chain.at(-1));
  if (tips.some((t) => !t)) return null;
  const curl = tips.reduce((sum, t) => sum + Math.hypot(t.x - wrist.x, t.y - wrist.y, t.z - wrist.z), 0) / tips.length;
  return curl < FIST_CURL * k ? hold : null;
}
export const turn = (v, a) => {
  const c = Math.cos(a), s = Math.sin(a), [x, y, z] = Array.isArray(v) ? v : [v.x, v.y, v.z];
  return Array.isArray(v) ? [x * c + z * s, y, -x * s + z * c] : { ...v, x: x * c + z * s, z: -x * s + z * c };
};
const roomAt = (table, q) => {
  const t = turn(q, table.rot || 0);
  return [table.pivot.x + (t[0] - table.at[0]) / table.scale, table.pivot.y + (t[1] - table.at[1]) / table.scale, table.pivot.z + (t[2] - table.at[2]) / table.scale];
};
// The hold: the two points of the room under the fists as they close (q1, q2: real points, as arrays).
export const grabStart = (table, q1, q2) => ({ a1: roomAt(table, q1), a2: roomAt(table, q2) });
// The table that keeps them there now.
export function grabStep(table, hold, q1, q2, { least = 0.004, most = 2 } = {}) {
  const dq = [q2[0] - q1[0], q2[1] - q1[1], q2[2] - q1[2]], da = [hold.a2[0] - hold.a1[0], hold.a2[1] - hold.a1[1], hold.a2[2] - hold.a1[2]];
  const far = Math.hypot(...da);
  if (far < 1e-6 || Math.hypot(...dq) < 0.02) return table;
  const scale = Math.min(most, Math.max(least, Math.hypot(...dq) / far));
  // (Turned only when the fists are apart across the floor: one over the other says nothing of which way round.)
  const flat = Math.hypot(dq[0], dq[2]) > 0.04 && Math.hypot(da[0], da[2]) > 1e-6;
  const rot = flat ? Math.atan2(da[0], da[2]) - Math.atan2(dq[0], dq[2]) : table.rot || 0;
  const qm = turn([(q1[0] + q2[0]) / 2, (q1[1] + q2[1]) / 2, (q1[2] + q2[2]) / 2], rot);
  const am = [(hold.a1[0] + hold.a2[0]) / 2, (hold.a1[1] + hold.a2[1]) / 2, (hold.a1[2] + hold.a2[2]) / 2];
  return { ...table, scale, rot,
    at: [qm[0] - scale * (am[0] - table.pivot.x), qm[1] - scale * (am[1] - table.pivot.y), qm[2] - scale * (am[2] - table.pivot.z)] };
}
// 🔄 Everything the headset says of itself, turned with the room (the head's place and its three axes, the hands, the rays, the real room's planes), so what follows reads it as it always did.
export function turnHeadset(xr, a) {
  if (!a) return xr;
  for (const key of ['offset', 'forward', 'rightAxis', 'up']) xr[key] = turn(xr[key], a);
  xr.hands = xr.hands.map((h) => ({ ...h, fingers: h.fingers.map((chain) => chain.map((j) => j && turn(j, a))) }));
  xr.rays = xr.rays.map((r) => ({ ...r, origin: turn(r.origin, a), dir: turn(r.dir, a) }));
  if (xr.room?.outlines) xr.room = { ...xr.room, outlines: xr.room.outlines.map((o) => ({ ...o, points: o.points.map((p) => turn(p, a)) })) };
  xr.yaw = (((xr.yaw + (a * 180) / Math.PI) % 360) + 360) % 360;
  return xr;
}

// 🔫 In the body, the right hand is the body's hand: it aims the way the hand
// points (wrist to the index finger's knuckle, which holds still while the
// finger moves), and the index finger curled in is the trigger pulled.
// { yaw, pitch } in degrees as the room has them, and `pulled`; or null with no right hand.
export const TRIGGER_CURL = 0.105;
export function handAim(xr) {
  const hand = xr?.hands?.find((h) => h.side === 'right'), wrist = hand?.fingers?.[0]?.[0], knuckle = hand?.fingers?.[1]?.[2], tip = hand?.fingers?.[1]?.at(-1);
  if (!wrist || !knuckle || !tip) return null;
  const d = { x: knuckle.x - wrist.x, y: knuckle.y - wrist.y, z: knuckle.z - wrist.z }, n = Math.hypot(d.x, d.y, d.z);
  if (n < 0.02) return null;
  return {
    yaw: ((Math.atan2(d.x, d.z) * degrees) % 360 + 360) % 360, pitch: Math.asin(Math.max(-1, Math.min(1, d.y / n))) * degrees,
    pulled: Math.hypot(tip.x - wrist.x, tip.y - wrist.y, tip.z - wrist.z) < TRIGGER_CURL, hold: knuckle,
  };
}

// 👻 The ghost's head, told with its hands: where the head is in the room and
// the way it faces, [x, y, z, yaw, pitch, k] (degrees; k times life size), so
// whoever it looks at sees it looking.
export const headPacked = (eye, forward, k = 1) => {
  const n = Math.hypot(...forward) || 1, r = (v) => Math.round(v * 1000) / 1000;
  return [r(eye.x), r(eye.y), r(eye.z), r(Math.atan2(forward[0], forward[2]) * degrees), r(Math.asin(Math.max(-1, Math.min(1, forward[1] / n))) * degrees), r(k)];
};
export const headValid = (h) => h === undefined || (Array.isArray(h) && h.length === 6 && h.every(Number.isFinite) && h.slice(0, 3).every((n) => Math.abs(n) < 60000) && h[5] > 0 && h[5] <= 400);
