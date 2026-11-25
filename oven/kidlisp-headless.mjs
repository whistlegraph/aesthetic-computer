// KidLisp, headless: AC's own evaluator (lib/kidlisp.mjs) drawing through AC's
// own renderer (lib/graph.mjs) into a plain pixel buffer, with no browser.
// 3D objects (slab, orb, capsule, or `; #3d`) go through the object viewer
// (lib/object/viewer.mjs), the same software rasterizer the browser runs.
//
//   const webp = await renderKidlisp("$hq5", { width: 128, frames: 30 });
//
// Each render runs in its own worker thread: graph.mjs and KidLisp keep
// module-level state, and the worker's clock (Date, performance.now) and
// Math.random are replaced by a simulated clock (1/60 s per paint) and a
// seeded generator, so a render is repeatable and never blocks the oven.
//
// It fails loudly — throws HeadlessKidlispError — when the output is blank or
// one flat colour, when boot throws, when a pasted painting cannot load, or
// when the piece reaches for something this host does not have (an embedded
// $code, a !tape). Callers fall back to the Chrome grab on any throw.

import { Worker, isMainThread, parentPort, workerData } from "node:worker_threads";
import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

// The deployed oven keeps public runtime modules in ac-source/ (as
// object-preview.mjs does); a checkout reads them from system/.
const deployedRoot = new URL("./ac-source/", import.meta.url);
const AC_ROOT = existsSync(new URL("lib/kidlisp.mjs", deployedRoot))
  ? deployedRoot
  : new URL("../system/public/aesthetic.computer/", import.meta.url);

export const HEADLESS_RENDER_VERSION = "h1";

export class HeadlessKidlispError extends Error {
  constructor(reason, message, details = {}) {
    super(message);
    this.name = "HeadlessKidlispError";
    this.reason = reason; // 'blank' | 'uniform' | 'boot' | 'painting' | 'unsupported' | 'source' | 'timeout' | 'worker'
    Object.assign(this, details);
  }
}

const SHAPES_3D = /\((?:slab|orb|capsule)\s/;
export function isObjectSource(source) {
  if (typeof source !== "string") return false;
  if (/^\s*;\s*#3d\b/m.test(source)) return true;
  return SHAPES_3D.test(source.replace(/;[^\n]*/g, ""));
}

const usesText = (code) => /\(\s*write\b/.test(code);

// What this host cannot draw yet. A piece that uses one goes to Chrome.
function unsupportedFeatures(source) {
  const code = source.replace(/;[^\n]*/g, "");
  const found = [];
  if (/(^|[\s(])\$[0-9A-Za-z]{2,}/.test(code)) found.push("embedded $code");
  if (/(^|[\s(])![0-9A-Za-z]+/.test(code)) found.push("!tape");
  if (/\((?:mesh|camera|place|light)\s/.test(code)) found.push("mesh/camera layer");
  if (/\b(?:cam(?:rot|spin)[xyz]?|form|cube)\b/.test(code) && /\(\s*(?:form|cube)\b/.test(code)) found.push("legacy 3D form");
  // font_1's hand-drawn glyphs are ASCII; anything else the browser fetches
  // from the BDF fallback mid-paint, which never lands in a synchronous render.
  if (usesText(code) && /[^\x00-\x7f]/.test(code)) found.push("non-ASCII text");
  return found;
}

export async function fetchSource(code) {
  const response = await fetch(
    `https://aesthetic.computer/api/store-kidlisp?code=${encodeURIComponent(code)}`,
    { signal: AbortSignal.timeout(15000) },
  );
  if (!response.ok) throw new HeadlessKidlispError("source", `KidLisp source HTTP ${response.status} for $${code}`, { status: response.status });
  const { source } = await response.json();
  if (typeof source !== "string" || !source.trim()) throw new HeadlessKidlispError("source", `No KidLisp source for $${code}`);
  return source;
}

// The image references a source pastes or stamps, as KidLisp passes them to
// the host: `#abc`, `@handle/2025.01.02.03.04.05.678`, or an http(s) URL.
function imageRefs(source) {
  const code = source.replace(/;[^\n]*/g, "");
  const refs = new Set();
  for (const m of code.matchAll(/(?:^|[\s(])(#[0-9A-Za-z]{1,8})(?=[\s)]|$)/gm)) refs.add(m[1]);
  for (const m of code.matchAll(/(?:^|[\s(])"?(@?[\w.-]+\/\d{4}\.\d{1,2}\.\d{1,2}\.\d{1,2}\.\d{1,2}\.\d{1,2}\.\d{1,3})"?/gm)) refs.add(m[1]);
  for (const m of code.matchAll(/(?:paste|stamp)\s+"?(https?:\/\/[^\s")]+)/g)) refs.add(m[1]);
  return [...refs];
}

function imageUrl(ref) {
  if (/^https?:\/\//.test(ref)) return ref;
  if (ref.includes("/")) {
    const [author, slug] = ref.replace(/^#/, "").split("/");
    const handle = author.startsWith("@") ? author : `@${author}`;
    return `https://aesthetic.computer/media/${handle}/painting/${slug}.png`;
  }
  return `https://aesthetic.computer/media/paintings/${ref.replace(/^#/, "")}.png`;
}

let sharpModule = null;
async function getSharp() {
  sharpModule ||= (await import("sharp")).default;
  return sharpModule;
}

// Fetch + decode every referenced image to { width, height, pixels }.
async function loadImages(refs) {
  const sharp = await getSharp();
  const images = {}, missing = [];
  await Promise.all(refs.map(async (ref) => {
    try {
      const response = await fetch(imageUrl(ref), { signal: AbortSignal.timeout(15000) });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const input = Buffer.from(await response.arrayBuffer());
      const { data, info } = await sharp(input).ensureAlpha().raw().toBuffer({ resolveWithObject: true });
      images[ref] = { width: info.width, height: info.height, pixels: new Uint8ClampedArray(data) }; // own buffer: transferable
    } catch (error) {
      // A six-digit hex colour (`#ff00aa`) reads like a code; it is not missing.
      if (!/^#[0-9a-f]{6}$/i.test(ref)) missing.push(`${ref} (${error.message})`);
    }
  }));
  return { images, missing };
}

function runWorker(data, timeoutMs) {
  return new Promise((resolve, reject) => {
    const transfer = Object.values(data.images || {}).map((img) => img.pixels.buffer);
    const worker = new Worker(fileURLToPath(import.meta.url), { workerData: data, transferList: transfer, resourceLimits: { maxOldGenerationSizeMb: 1024 } });
    const timer = setTimeout(() => {
      worker.terminate();
      reject(new HeadlessKidlispError("timeout", `Headless render exceeded ${timeoutMs} ms`));
    }, timeoutMs);
    worker.once("message", (msg) => {
      clearTimeout(timer);
      worker.terminate();
      if (msg.error) reject(new HeadlessKidlispError(msg.error.reason || "worker", msg.error.message, msg.error.details || {}));
      else resolve(msg.result);
    });
    worker.once("error", (error) => {
      clearTimeout(timer);
      reject(new HeadlessKidlispError("worker", error.message));
    });
  });
}

// How alike the frames are: unique colours seen, whether every pixel of every
// frame is one colour, and whether all frames are identical.
function inspectFrames(frames, width, height) {
  const n = width * height;
  const first = frames[0];
  const r0 = first[0], g0 = first[1], b0 = first[2];
  let uniform = true, identical = true;
  const colours = new Set();
  for (let f = 0; f < frames.length; f++) {
    const px = frames[f];
    for (let i = 0; i < n * 4; i += 4) {
      if (uniform && (px[i] !== r0 || px[i + 1] !== g0 || px[i + 2] !== b0)) uniform = false;
      if (identical && f > 0 && (px[i] !== first[i] || px[i + 1] !== first[i + 1] || px[i + 2] !== first[i + 2])) identical = false;
      if (colours.size < 4096 && (i & 63) === 0) colours.add((px[i] << 16) | (px[i + 1] << 8) | px[i + 2]);
    }
    if (!uniform && !identical) break;
  }
  return { uniform, identical: frames.length > 1 && identical, colours: colours.size };
}

/**
 * Render a KidLisp piece without a browser.
 * @param {string|{code?:string, source?:string}} input  "$abc" (fetched) or KidLisp source
 * @returns {Promise<{ webp: Buffer, frames: Uint8ClampedArray[], width, height, stats }>}
 */
export async function renderKidlispDetailed(input, options = {}) {
  const {
    width = 128,
    height = width,
    frames: frameCount = 30,
    fps = 7.5, // samples per second of piece time
    playbackFps = fps, // webp playback rate (the Chrome grab plays 7.5 fps captures at 15)
    quality = 80,
    lossless = true, // exact colours; pixel art compresses 4–15× smaller lossless than lossy
    scale = 1, // nearest-neighbour upscale of the encoded frames (chunky pixels)
    epochMs = Date.now(), // the simulated clock's start
    seed = null, // Math.random seed; default derives from the source
    collapseStill = true, // identical frames → a one-frame webp
    objectPreview = false, // 3D: the viewer's fitted turntable without name label/speaker (the live piece shows both)
    allowUnsupported = false,
    allowMissingImages = false,
    timeoutMs = 20000,
  } = options;
  if (![width, height, frameCount].every(Number.isInteger) || width < 8 || height < 8 || width > 2048 || height > 2048 || frameCount < 1 || frameCount > 600)
    throw new HeadlessKidlispError("source", "Invalid headless dimensions or frame count");

  const t0 = performance.now();
  let code = null, source;
  if (typeof input === "object" && input) ({ code = null, source } = input);
  else if (typeof input === "string" && /^\$[0-9A-Za-z]{2,64}$/.test(input.trim())) code = input.trim().slice(1);
  else source = input;
  if (!source && code) source = await fetchSource(code.replace(/^\$/, ""));
  if (typeof source !== "string" || !source.trim()) throw new HeadlessKidlispError("source", "Empty KidLisp source");
  const tSource = performance.now();

  const kind = isObjectSource(source) ? "3d" : "2d";
  const unsupported = kind === "2d" ? unsupportedFeatures(source) : [];
  if (unsupported.length && !allowUnsupported)
    throw new HeadlessKidlispError("unsupported", `Headless KidLisp cannot draw: ${unsupported.join(", ")}`, { unsupported });

  const refs = kind === "2d" ? imageRefs(source) : [];
  const { images, missing } = refs.length ? await loadImages(refs) : { images: {}, missing: [] };
  if (missing.length && !allowMissingImages)
    throw new HeadlessKidlispError("painting", `Could not load: ${missing.join(", ")}`, { missing });
  const tImages = performance.now();

  let seedValue = seed;
  if (seedValue === null) {
    seedValue = 2166136261;
    for (let i = 0; i < source.length; i++) seedValue = Math.imul(seedValue ^ source.charCodeAt(i), 16777619) >>> 0;
  }

  const result = await runWorker({
    kind, source, label: code ? `$${code}` : "", width, height, frameCount, fps,
    epochMs, seed: seedValue >>> 0, images, acRoot: AC_ROOT.href, objectPreview,
  }, timeoutMs);
  const tRender = performance.now();

  const frames = result.frames.map((b) => new Uint8ClampedArray(b));
  const check = inspectFrames(frames, width, height);
  const stats = {
    code, kind, width, height, frames: frames.length, paints: result.paints,
    diagnostics: result.diagnostics, unknownWords: result.unknownWords,
    images: Object.keys(images), missing, unsupported,
    colours: check.colours, identical: check.identical, glyphs: result.glyphCount,
  };
  // Text that could not draw is a wrong poster, not a blank one: fail loudly.
  if (kind === "2d" && usesText(source.replace(/;[^\n]*/g, "")) && result.glyphCount < 9)
    throw new HeadlessKidlispError("unsupported", `Text glyphs did not load (${result.glyphCount}/9 probe glyphs)`, { stats });
  if (result.bootError) throw new HeadlessKidlispError("boot", `Boot threw: ${result.bootError}`, { stats });
  if (check.uniform) {
    const [r, g, b] = frames[0];
    const reason = r + g + b === 0 ? "blank" : "uniform";
    throw new HeadlessKidlispError(reason, `Headless output is one colour (${r},${g},${b}) across ${frames.length} frames${result.paintError ? `; paint threw: ${result.paintError}` : ""}`, { stats, colour: [r, g, b] });
  }
  if (result.paintError) stats.paintError = result.paintError;

  // A piece that never moves is a still (one frame), as the Chrome grab returns.
  const encoded = check.identical && collapseStill ? frames.slice(0, 1) : frames;
  const webp = await encodeAnimatedWebp(encoded, width, height, { fps: playbackFps, quality, lossless, scale });
  const tEncode = performance.now();
  stats.timings = {
    source: Math.round(tSource - t0), images: Math.round(tImages - tSource),
    render: Math.round(tRender - tImages), workerRender: result.renderMs,
    encode: Math.round(tEncode - tRender), total: Math.round(tEncode - t0),
  };
  stats.bytes = webp.length;
  return { webp, frames, width, height, stats };
}

/** renderKidlisp(source|code, opts) → animated webp Buffer (throws HeadlessKidlispError). */
export async function renderKidlisp(input, options = {}) {
  return (await renderKidlispDetailed(input, options)).webp;
}


// ─── Animated WebP ─────────────────────────────────────────────────────────
// sharp 0.33 cannot mark a raw strip as pages, so frames are encoded one by
// one as still WebPs and muxed here into an ANIM/ANMF container. Each frame
// after the first carries only the rectangle that changed since the last.

const u24 = (b, o, v) => { b[o] = v & 255; b[o + 1] = (v >> 8) & 255; b[o + 2] = (v >> 16) & 255; };
function chunk(fourcc, payload) {
  const pad = payload.length & 1;
  const out = Buffer.alloc(8 + payload.length + pad);
  out.write(fourcc, 0, "ascii");
  out.writeUInt32LE(payload.length, 4);
  payload.copy(out, 8);
  return out;
}
// The image chunks (ALPH, VP8, VP8L) inside a still WebP file.
function imageChunks(file) {
  const parts = [];
  for (let o = 12; o + 8 <= file.length;) {
    const id = file.toString("ascii", o, o + 4), size = file.readUInt32LE(o + 4);
    if (id === "ALPH" || id === "VP8 " || id === "VP8L") parts.push(file.subarray(o, o + 8 + size + (size & 1)));
    o += 8 + size + (size & 1);
  }
  return Buffer.concat(parts);
}
function changedRect(prev, next, w, h) {
  let x0 = w, y0 = h, x1 = -1, y1 = -1;
  for (let y = 0; y < h; y++) {
    const row = y * w * 4;
    for (let x = 0; x < w; x++) {
      const i = row + x * 4;
      if (prev[i] !== next[i] || prev[i + 1] !== next[i + 1] || prev[i + 2] !== next[i + 2]) {
        if (x < x0) x0 = x; if (x > x1) x1 = x; if (y < y0) y0 = y; if (y > y1) y1 = y;
      }
    }
  }
  if (x1 < 0) return { x: 0, y: 0, w: 2, h: 2 }; // nothing changed: a tiny repeat
  x0 &= ~1; // ANMF offsets are stored halved
  return { x: x0, y: y0, w: x1 - x0 + 1, h: y1 - y0 + 1 };
}

export async function encodeAnimatedWebp(frames, width, height, { fps = 7.5, quality = 80, lossless = false, scale = 1 } = {}) {
  const sharp = await getSharp();
  const W = width * scale, H = height * scale;
  // Opaque, upscaled (nearest) frames.
  const full = frames.map((f) => {
    const px = new Uint8ClampedArray(W * H * 4);
    for (let y = 0; y < H; y++) {
      const sy = ((y / scale) | 0) * width;
      for (let x = 0; x < W; x++) {
        const si = (sy + ((x / scale) | 0)) * 4, di = (y * W + x) * 4;
        px[di] = f[si]; px[di + 1] = f[si + 1]; px[di + 2] = f[si + 2]; px[di + 3] = 255;
      }
    }
    return px;
  });
  const rects = full.map((px, i) => (i ? changedRect(full[i - 1], px, W, H) : { x: 0, y: 0, w: W, h: H }));
  const stills = await Promise.all(full.map((px, i) => {
    const r = rects[i];
    return sharp(Buffer.from(px.buffer), { raw: { width: W, height: H, channels: 4 } })
      .extract({ left: r.x, top: r.y, width: r.w, height: r.h })
      .removeAlpha()
      .webp({ quality, lossless, effort: 4, smartSubsample: true })
      .toBuffer();
  }));
  const duration = Math.round(1000 / fps);
  const anmf = stills.map((still, i) => {
    const r = rects[i], head = Buffer.alloc(16);
    u24(head, 0, r.x / 2); u24(head, 3, r.y / 2); u24(head, 6, r.w - 1); u24(head, 9, r.h - 1); u24(head, 12, duration);
    head[15] = 0b10; // no blending (opaque rect replaces), no disposal
    return chunk("ANMF", Buffer.concat([head, imageChunks(still)]));
  });
  const vp8x = Buffer.alloc(10);
  vp8x[0] = 0x02; // animation
  u24(vp8x, 4, W - 1); u24(vp8x, 7, H - 1);
  const anim = Buffer.alloc(6); // background 0,0,0,0; loop 0 = forever
  const body = Buffer.concat([Buffer.from("WEBP"), chunk("VP8X", vp8x), chunk("ANIM", anim), ...anmf]);
  const riff = Buffer.alloc(8);
  riff.write("RIFF", 0, "ascii");
  riff.writeUInt32LE(body.length, 4);
  return Buffer.concat([riff, body]);
}

// ─── Worker ────────────────────────────────────────────────────────────────

async function workerMain({ kind, source, label, width, height, frameCount, fps, epochMs, seed, images, acRoot, objectPreview }) {
  // Simulated clock: every Date and performance.now in this isolate reads it.
  let simMs = epochMs;
  const RealDate = Date;
  class SimDate extends RealDate {
    constructor(...args) { if (args.length) super(...args); else super(Math.floor(simMs)); }
    static now() { return Math.floor(simMs); }
  }
  globalThis.Date = SimDate;
  const perfBase = 1000;
  Object.defineProperty(performance, "now", { value: () => perfBase + (simMs - epochMs), configurable: true });

  // Seeded Math.random (mulberry32): the same piece paints the same poster.
  let s = seed >>> 0;
  Math.random = () => {
    s = (s + 0x6d2b79f5) >>> 0;
    let t = s;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };

  const diagnostics = [];
  const unknownWords = new Set();
  const note = (...args) => {
    const line = args.map((a) => (a instanceof Error ? a.message : typeof a === "string" ? a : (() => { try { return JSON.stringify(a); } catch { return String(a); } })())).join(" ").slice(0, 300);
    const unknown = line.match(/Unknown KidLisp word:?\s*"?([^\s"]+)/i);
    if (unknown) unknownWords.add(unknown[1]);
    else if (/Glyph cache|indexedDB|MODULE_TYPELESS|Reparsing as ES module|trace-warnings|add "type"/.test(line)) return; // no IndexedDB here; glyphs come from disk
    else if (diagnostics.length < 20) diagnostics.push(line);
  };
  console.warn = console.error = note;
  console.log = console.info = console.debug = () => {};

  const lib = (p) => new URL(`lib/${p}`, acRoot).href;
  const graph = await import(lib("graph.mjs"));

  const display = { width, height, pixels: new Uint8ClampedArray(width * height * 4) };
  const trueScreen = { pixels: display.pixels, width, height };
  const restoreScreen = () => Object.assign(display, trueScreen);

  // Text: AC's font_1, loaded from the local glyph files.
  let typeface = null;
  try {
    const { Typeface } = await import(lib("type.mjs"));
    typeface = new Typeface("font_1");
    const drawings = new URL("disks/drawings/", acRoot);
    await typeface.load(async (path) => {
      const rel = path.replace(/^.*?disks\/drawings\//, "");
      return JSON.parse(await readFile(new URL(rel, drawings), "utf8"));
    });
  } catch (error) {
    note("typeface:", error.message);
    typeface = null;
  }
  // Count hand-drawn glyphs without touching the BDF proxy's lazy fetches.
  let glyphCount = 0;
  if (typeface) for (const c of "ABCabc012") if (typeface.glyphs?.[c]?.commands) glyphCount++;

  let paintCount = 0;
  const hud = { hidden: () => true, label() {}, tinyLabel() {}, qr() {}, supportsInlineColor: false, disablePieceNameColoring() {}, sourceless() {}, frameHash() {} };
  const voice = { update() {}, kill() {} };
  const sound = { synth: () => voice, speaker: { amplitudes: { left: 0, right: 0 }, waveforms: { left: [], right: [] }, poll() {} }, play: () => voice, bpm: () => 120, time: 0 };
  const lookupImage = (ref) => images[ref] || images[`#${ref}`] || images[String(ref).replace(/^#/, "")] || null;

  const base = {
    screen: display,
    params: [], colon: [], query: {},
    clock: { time: () => new SimDate(Math.floor(simMs)), resync() {} },
    get paintCount() { return paintCount; },
    system: { fps: 60, painting: null, nopaint: {} },
    sound, hud,
    net: { iframe: false, preload: async () => null },
    store: {},
    paintings: images,
    cachedKidlispOwner: null,
    inkrn: () => graph.c.slice(),
    fps() {}, needsPaint() {}, toggleHUD() {}, jump() {}, goto() {}, send() {},
    pens: () => [],
    page(buffer) {
      if (!buffer || buffer === display) restoreScreen();
      else Object.assign(display, { width: buffer.width, height: buffer.height, pixels: buffer.pixels });
      graph.setBuffer(buffer && buffer !== display ? buffer : trueScreen);
      return api;
    },
    painting(w, h, fill) {
      const saved = graph.getBuffer?.();
      const pix = graph.makeBuffer(w, h, fill, undefined, api);
      if (saved) graph.setBuffer(saved);
      return pix;
    },
    ink(...args) { graph.color(...graph.findColor(...args)); return api; },
    wipe(...args) {
      if (args.length) graph.color(...graph.findColor(...args));
      graph.clear();
      return api;
    },
    paste(from, ...rest) {
      if (typeof from === "string") { const img = lookupImage(from); if (img) graph.paste(img, ...rest); }
      else graph.paste(from, ...rest);
      return api;
    },
    stamp(from, ...rest) {
      if (typeof from === "string") { const img = lookupImage(from); if (img) graph.stamp(img, ...rest); }
      else graph.stamp(from, ...(rest.length ? rest : [0, 0]));
      return api;
    },
    write(text, pos, bg) {
      if (!typeface || text === undefined || text === null || text === "") return api;
      if (typeof pos === "number") pos = { x: arguments[1], y: arguments[2] };
      const options = arguments[2] && typeof arguments[2] === "object" && !Array.isArray(arguments[2]) ? arguments[2] : null;
      try {
        typeface.print(api, pos || {}, 0, String(text), options ? options.bg : bg);
      } catch (error) { note("write:", error.message); }
      return api;
    },
    typeface: null,
  };
  // Anything else is graph.mjs's function of that name (line, box, circle,
  // spin, zoom, scroll, blur, ...), or a chainable no-op.
  const api = new Proxy(base, {
    get(target, key) {
      if (key in target) return target[key];
      const f = graph[key];
      if (typeof f === "function")
        return (...args) => {
          const out = f(...args);
          return out === undefined ? api : out;
        };
      if (typeof key === "symbol") return undefined;
      return () => api;
    },
  });
  base.typeface = typeface;

  graph.setBuffer(trueScreen);
  let piece, bootError = null, paintError = null;
  const t0 = RealDate.now();

  if (kind === "3d") {
    const { objectPiece } = await import(lib("object/viewer.mjs"));
    piece = objectPiece(source, label, objectPreview ? { preview: true } : {});
  } else {
    const { KidLisp } = await import(lib("kidlisp.mjs"));
    const lisp = new KidLisp();
    lisp.setAPI?.(api);
    piece = lisp.module(source, true);
    lisp.cacheInitiated = true; // no publishing, no source lookups
    if (!piece) bootError = "KidLisp did not compile";
  }

  try { piece?.boot?.(api); } catch (error) { bootError = error.message; }

  const out = [];
  // Paint at 60 Hz as the browser does (scrolls, `frame`, the turntable's
  // integrated yaw all count paints) and keep every per-th. A 3D preview's
  // pose is a function of time alone, so it paints only the kept frames.
  const hz = kind === "3d" && objectPreview ? fps : 60;
  const per = Math.max(1, Math.round(hz / fps));
  const total = frameCount * per;
  if (!bootError) {
    for (let i = 0; i < total; i++) {
      simMs = epochMs + (i * 1000) / hz;
      paintCount = i;
      restoreScreen();
      graph.setBuffer(trueScreen);
      try {
        piece.sim?.(api);
        piece.paint?.(api);
      } catch (error) {
        paintError ||= `frame ${i}: ${error.message}`;
      }
      restoreScreen();
      if (i % per === 0) out.push(trueScreen.pixels.slice().buffer);
    }
  }
  try { piece?.leave?.(api); } catch {}

  return {
    frames: out, paints: total, renderMs: RealDate.now() - t0,
    diagnostics, unknownWords: [...unknownWords], bootError, paintError, glyphCount,
  };
}

if (!isMainThread && workerData?.frameCount && workerData?.acRoot) {
  workerMain(workerData)
    .then((result) => parentPort.postMessage({ result }, result.frames))
    .catch((error) => parentPort.postMessage({ error: { reason: error.reason || "worker", message: error.stack || error.message } }));
}
