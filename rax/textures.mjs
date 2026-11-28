// 🖼️ Where textures come from. A texture is named, and the name says what
// makes it:
//
//   #code          a painting, by its short code (fetched once, kept)
//   $code          a KidLisp piece, running: stepped and sent up as it goes
//   $code:still    a KidLisp piece run a moment and then held (cached)
//   $code:64       either, at a fixed size (pixels a side) and not by distance
//   url:https://…  any picture
//   fn:name        a picture drawn by a function `register`ed under that name
//
// Models carry a texture's key (`keyOf(name)`, a small number, the same in
// every model). The projector asks `resolve(key)` for its layer each time it
// draws a triangle wearing it: -1 until the picture is there, and the asking
// is what starts it. Once a frame `tick` steps what is running and sends
// what changed, inside a budget of milliseconds and bytes, nearest first.
//
// 📏 Level of detail (as screen-lod.mjs does for screens): how far off a
// texture is seen (`hint`) picks its size and how often it steps. A texture
// out of sight is not stepped; one not drawn for a while gives up its layer.
//
// The GPU side is the host's (lens-host.mjs `texture(layer, picture)`), and
// the loaders are passed in, so this runs under node for its tests.

const names = [], keys = new Map();
export const keyOf = (name) => {
  if (!keys.has(name)) { keys.set(name, names.length); names.push(name); }
  return keys.get(name);
};
export const nameOf = (key) => names[key] ?? null;

// Size (pixels a side) and steps a second, by how far off it is seen (metres).
export const TEXTURE_TIERS = [
  { within: 3, size: 256, hz: 30 },
  { within: 8, size: 128, hz: 20 },
  { within: 20, size: 64, hz: 12 },
  { within: 45, size: 32, hz: 6 },
  { within: Infinity, size: 16, hz: 2 },
];
export const tierOf = (distance) => TEXTURE_TIERS.find((t) => distance < t.within) || TEXTURE_TIERS.at(-1);
const UNHINTED = 5; // (a texture nobody says the distance of is taken to be this near: 128 a side, 20 a second)
const STILL_FRAMES = 30; // (a held piece runs this long first, so what it builds up is there)

// What a name asks for: { kind, ref, still, size }.
export function parseName(name) {
  const [head, ...options] = String(name).split(':');
  if (head === 'url') return { kind: 'url', ref: String(name).slice(4), still: true, size: 0 };
  if (head === 'fn') return { kind: 'fn', ref: options[0] || '', still: false, size: Number(options[1]) || 0 };
  const still = options.includes('still'), size = Number(options.find((o) => /^\d+$/.test(o))) || 0;
  if (head.startsWith('#')) return { kind: 'painting', ref: head.slice(1), still: true, size };
  if (head.startsWith('$')) return { kind: 'kidlisp', ref: head.slice(1), still, size };
  return { kind: 'unknown', ref: head, still: true, size };
}

// host: { texture(layer, picture) → bytes } · layers, maxSize: the array's
// loaders: { kidlisp(code, { width, height }) → { step() → pixels, width, height },
//            painting(code, size) → picture, image(url, size) → picture }
export function createTextures({ host, layers = 64, maxSize = 256, loaders = {}, now = () => performance.now() } = {}) {
  const sources = new Map(); // key → source
  const resident = new Array(layers).fill(null); // layer → key
  const drawn = {}; // functions by name, for fn: textures
  let frame = 0;
  const totals = { uploads: 0, bytes: 0, steps: 0, stepMs: 0, evictions: 0 };
  // 🎛️ Overrides for the bench: a size or a rate for all, still for all, or no budget.
  const config = { size: 0, hz: 0, still: false, budgetMs: 4, budgetBytes: 2 << 20, keepFrames: 120 };

  function source(key) {
    let s = sources.get(key);
    if (s) return s;
    const name = nameOf(key), want = parseName(name);
    s = { key, name, ...want, layer: -1, picture: null, runner: null, runnerSize: 0, state: 'new', usedAt: -1, dueAt: 0,
      distance: Infinity, hinted: -1, frames: 0, error: null };
    sources.set(key, s);
    return s;
  }
  const sizeOf = (s) => Math.min(maxSize, config.size || s.size || tierOf(s.hinted >= 0 && s.hinted >= frame - 1 ? s.distance : UNHINTED).size);
  const hzOf = (s) => config.hz || tierOf(s.hinted >= 0 && s.hinted >= frame - 1 ? s.distance : UNHINTED).hz;
  const stillOf = (s) => s.still || config.still;

  // A free layer, or the one used longest ago (not this frame's).
  function claim(s) {
    let at = resident.indexOf(null);
    if (at < 0) {
      let oldest = Infinity;
      for (let i = 0; i < layers; i++) {
        const o = sources.get(resident[i]);
        if (o && o.usedAt < frame && o.usedAt < oldest) { oldest = o.usedAt; at = i; }
      }
      if (at < 0) return -1; // every layer is on screen this frame
      const old = sources.get(resident[at]);
      old.layer = -1; totals.evictions++;
    }
    resident[at] = s.key;
    s.layer = at;
    return at;
  }
  function send(s, picture) {
    if (s.layer < 0 && claim(s) < 0) return 0;
    const bytes = host?.texture?.(s.layer, picture) || 0;
    totals.uploads++; totals.bytes += bytes;
    return bytes;
  }

  async function load(s) {
    s.state = 'loading';
    try {
      const size = sizeOf(s);
      if (s.kind === 'painting') s.picture = await loaders.painting(s.ref, size);
      else if (s.kind === 'url') s.picture = await loaders.image(s.ref, size);
      else if (s.kind === 'kidlisp') { s.runner = await loaders.kidlisp(s.ref, { width: size, height: size }); s.runnerSize = size; }
      else if (s.kind === 'fn') {
        if (!drawn[s.ref]) throw Error(`nothing registered as fn:${s.ref}`);
        s.picture = { width: size, height: size, pixels: new Uint8ClampedArray(size * size * 4) };
      } else throw Error(`no texture source for ${s.name}`);
      s.state = 'ready';
      s.dirty = true;
    } catch (error) {
      s.state = 'failed'; s.error = String(error?.message || error);
    }
  }

  return {
    config,
    keyOf,
    register(name, fn) { drawn[name] = fn; },
    // The projector's question: this texture's layer, or -1 (and it is asked for).
    resolve(key) {
      const s = source(key);
      s.usedAt = frame;
      if (s.state === 'new') load(s);
      return s.layer;
    },
    // How far off a texture is seen this frame (the nearest asking wins).
    hint(key, distance) {
      const s = source(key);
      if (s.hinted !== frame || distance < s.distance) { s.distance = distance; s.hinted = frame; }
    },
    // Once a frame, before drawing: step what is due, send what changed.
    tick(t = now()) {
      const started = now();
      let bytes = 0;
      const due = [...sources.values()].filter((s) => s.state === 'ready' && s.usedAt >= frame - 2)
        .sort((a, b) => a.distance - b.distance);
      for (const s of due) {
        if (now() - started > config.budgetMs || bytes > config.budgetBytes) break;
        if (s.kind === 'kidlisp') {
          // 🧊 Held (:still): run STILL_FRAMES frames, keep the last, send only that from then on.
          if (stillOf(s) && s.picture) { if (s.layer < 0) bytes += send(s, s.picture); continue; }
          // A new tier: a new runner at its size (the old picture shows until it is ready).
          if (!stillOf(s) && !(config.size || s.size) && sizeOf(s) !== s.runnerSize) { s.state = 'new'; s.runner = null; continue; }
          if (!stillOf(s) && t < s.dueAt) continue;
          const begun = now();
          const pixels = s.runner.step();
          totals.steps++; totals.stepMs += now() - begun;
          s.frames++;
          s.dueAt = t + 1000 / hzOf(s);
          if (stillOf(s) && s.frames >= STILL_FRAMES) { s.picture = { width: s.runner.width, height: s.runner.height, pixels: pixels.slice() }; s.runner = null; }
          bytes += send(s, { width: s.runnerSize, height: s.runnerSize, pixels });
        } else if (s.kind === 'fn') {
          // (A function marked `still` draws once: its picture is kept and only sent again when its layer was given up.)
          if (s.held) { if (s.layer < 0) bytes += send(s, s.picture); continue; }
          if (drawn[s.ref].still) s.held = true;
          if (t < s.dueAt) continue;
          const p = s.picture, begun = now();
          drawn[s.ref](p.pixels, p.width, p.height, t / 1000);
          totals.steps++; totals.stepMs += now() - begun;
          s.dueAt = t + 1000 / hzOf(s);
          bytes += send(s, p);
        } else if (s.picture && (s.dirty || s.layer < 0)) {
          bytes += send(s, s.picture);
          s.dirty = false;
        }
      }
      // Long unseen: the layer goes back (the picture is kept, so it comes back cheaply).
      for (const s of sources.values())
        if (s.layer >= 0 && s.usedAt < frame - config.keepFrames) { resident[s.layer] = null; s.layer = -1; }
      frame++; // (what is asked and hinted from here is the next frame's)
      return bytes;
    },
    stats() {
      const all = [...sources.values()];
      return { ...totals, frame, layers: resident.filter((k) => k !== null).length, of: layers,
        live: all.filter((s) => s.layer >= 0 && s.usedAt >= frame - 2).length,
        failed: all.filter((s) => s.state === 'failed').map((s) => `${s.name}: ${s.error}`) };
    },
    sources: () => [...sources.values()].map(({ name, kind, state, layer, frames, distance }) => ({ name, kind, state, layer, frames, distance })),
  };
}
