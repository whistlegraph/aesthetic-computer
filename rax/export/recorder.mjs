// 🎞️ The draw recorder: rax's own renderer, frame by frame, as data. Put in
// a page that runs rax (the film's camera page), it wraps the projector
// (project.mjs) and keeps, for every frame:
//   - each model drawn and where (its id, x, y, z, yaw, pitch, scale, roll),
//     the model itself kept once (by content: a model made again the same is
//     the same model), as its triangles with a colour at each corner;
//   - the camera (eye, forward, up, sx, sy), the small planet's radius as the
//     view bends the land this frame (`planet`, 0 for flat) and the ground;
//   - the sky: the day's phase, the sun, the sky's colours, and the other
//     planets (orbits.mjs) as seen.
// The sky's own dome and the shadows are not kept (Blender makes its own);
// the HUD never goes through draw(). A screen's pixels are models like any other, so a KidLisp
// or .mjs piece running on a panel comes out as its picture, frame by frame.
//
// `send(chunk)` is handed each frame as it ends (JSON-safe: a new model's
// floats are base64). Blender reads the lot with rax/blender/rax_import.py.
import { Projector } from '../project.mjs';
import { skyAt, phase } from '../sky.mjs';
import { skyOf, sunFrom, HOME } from '../orbits.mjs';

const b64 = (floats) => {
  const bytes = new Uint8Array(floats.buffer, floats.byteOffset, floats.byteLength);
  let s = '';
  for (let i = 0; i < bytes.length; i += 0x8000) s += String.fromCharCode.apply(null, bytes.subarray(i, i + 0x8000));
  return btoa(s);
};
// FNV-1a over the model's floats, as 32-bit words: a model made again the same is one model.
const hashOf = (floats) => {
  const w = new Uint32Array(floats.buffer, floats.byteOffset, floats.length);
  let h = 2166136261 ^ w.length;
  for (let i = 0; i < w.length; i++) h = Math.imul(h ^ w[i], 16777619);
  return `${h >>> 0}:${w.length}`;
};

export function installRecorder(send, { every = 1 } = {}) {
  const P = Projector.prototype, draw = P.draw, frame = P.frame;
  const ids = new WeakMap(), byHash = new Map();
  let main = null, on = false, n = 0, current = null, models = 0, fresh = [];
  const close = () => {
    if (current && (n - 1) % every === 0) send({ ...current, fresh });
    current = null; fresh = [];
  };
  P.frame = function (...a) {
    main ||= this;
    const out = frame.apply(this, a);
    if (this === main && on) {
      close();
      const now = Date.now(), day = phase(now), sky = skyAt(day);
      current = {
        n: n++, t: performance.now(),
        camera: { eye: { ...this.eye }, forward: [...this.forward], up: [...this.up], sx: this.sx, sy: this.sy, lens: this.lens || 0 },
        planet: this.planet || 0, ground: this.ground || 0,
        sky: { phase: day, sun: sunFrom(HOME, now, day), zenith: sky.zenith, mid: sky.mid, horizon: sky.horizon, stars: sky.stars,
          planets: skyOf(HOME, now, day).map((p) => ({ id: p.id, dir: p.dir, size: p.size, ink: p.ink, dark: p.dark, light: p.light })) },
        draws: [],
      };
    }
    return out;
  };
  P.draw = function (v, x = 0, y = 0, z = 0, yaw = 0, pitch = 0, scale = 1, roll = 0) {
    // (Shadows are drawn from one scratch buffer, a view of part of it each time: not models, and Blender casts its own.)
    const scratch = v.buffer && v.byteLength !== v.buffer.byteLength;
    if (on && current && this === main && this.bends !== false && !scratch) {
      let id = ids.get(v);
      if (id === undefined) {
        const h = hashOf(v);
        id = byHash.get(h);
        if (id === undefined) {
          id = models++;
          byHash.set(h, id);
          fresh.push({ id, floats: b64(v instanceof Float32Array ? v : new Float32Array(v)) });
        }
        ids.set(v, id);
      }
      current.draws.push([id, x, y, z, yaw, pitch, scale, roll]);
    }
    return draw.call(this, v, x, y, z, yaw, pitch, scale, roll);
  };
  return {
    start() { on = true; },
    stop() { close(); on = false; P.draw = draw; P.frame = frame; return { frames: n, models }; },
  };
}
