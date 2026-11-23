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
    let right = 0, fwd = 0, turnStick = 0, jumpHeld = false;
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
    return {
      cullScale: 1 / extent, offset, rightAxis: rotate(m[0], m[1], m[2]), up: rotate(m[4], m[5], m[6]),
      forward: rotate(-m[8], -m[9], -m[10]),
      yaw: ((this.turn + headYaw) * degrees % 360 + 360) % 360,
      pitch: Math.asin(Math.max(-1, Math.min(1, -m[9]))) * degrees,
      right, fwd, jump, hands, rays, walkPinch: this.walkPinch,
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
out vec3 color;
void main() {
  bool sky = position.z < 0.;
  float z = sky ? 10000. : position.z;
  gl_Position = projection * eye * head * vec4(position.xy * tangent * z, -z, 1.);
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
  const uniforms = Object.fromEntries(['head', 'eye', 'projection', 'tangent', 'opacity'].map(k => [k, gl.getUniformLocation(program, k)]));
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
  return (scene, sky, pose, layer, projector, mixed) => {
    gl.bindFramebuffer(gl.FRAMEBUFFER, layer.framebuffer);
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
    let next, started = false;
    try {
      next = await xr.requestSession(mode, { optionalFeatures: ['hand-tracking'] });
      session = next;
      next.addEventListener('end', () => {
        if (session !== next) return;
        session = null;
        if (started) {
          transition(false);
          desktopFrame = window.requestAnimationFrame(desktop);
        }
      }, { once: true });
      if (!draw || gl.isContextLost()) {
        draw = null;
        gl = document.createElement('canvas').getContext('webgl2', { xrCompatible: true, antialias: false });
        if (!gl) throw new Error('WebGL 2 unavailable');
        await gl.makeXRCompatible();
        draw = stereoRenderer(gl);
      }
      const space = await next.requestReferenceSpace('local');
      if (session !== next || !open) { await next.end(); return; }
      const layer = new XRWebGLLayer(next, gl, { alpha: true, antialias: false, framebufferScaleFactor: 0.8, ignoreDepthValues: true });
      if (typeof layer.fixedFoveation === 'number') layer.fixedFoveation = Math.max(0.5, layer.fixedFoveation);
      next.updateRenderState({ baseLayer: layer, depthNear: 0.05, depthFar: 150000 });
      const mixed = mode === 'immersive-ar';
      if (mixed && next.environmentBlendMode === 'opaque') throw new Error('Room blending unavailable');
      let rig = new Headset();
      const reset = () => { rig = new Headset(); release(); };
      space.addEventListener('reset', reset);
      next.addEventListener('visibilitychange', () => { if (next.visibilityState !== 'visible') release(); });
      function tick(now, xrFrame) {
        if (session !== next) return;
        next.requestAnimationFrame(tick);
        const pose = xrFrame.getViewerPose(space);
        if (!pose) { release(); return; }
        const visible = next.visibilityState === 'visible';
        const input = rig.update(pose, visible ? next.inputSources : [], getYaw(), xrFrame, space, visible ? navigator.getGamepads?.() : []);
        frame(now, { ...input, mixed, pose, layer, draw });
      }
      transition(true);
      window.cancelAnimationFrame(desktopFrame);
      started = true;
      next.requestAnimationFrame(tick);
    } catch (e) {
      if (next && session === next) await next.end().catch(() => {});
      if (open) message = e.name === 'NotAllowedError' ? 'Permission declined. Try again.' : 'Headset mode unavailable. Try again.';
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
