import { WebGPUOskiewarScene3D } from './engine/scene3d-webgpu.mjs';
import { model, INK, PAPER, rgb, tint } from './paint.mjs';
import { poseObject, objectBoxes } from './objects.mjs';
import { OBJECT_FORMAT, sourceFrame, sourceTree, sourceHair } from './object-language.mjs';
import { Projector } from './project.mjs';
import { joinCode, hudLayout } from './hud.mjs';

// What each kind of input does, one line per hand, as Oskiewar lists its
// controls: the same world plays from a keyboard alone, a pad or a touchscreen.
// A key is [label, code]; it lights while held. Plain strings are the verbs.
export const LEGEND = {
  // Lines stay under thirty columns so a phone or a narrow window shows them whole.
  keys: [
    [['w', 'KeyW'], ['a', 'KeyA'], ['s', 'KeyS'], ['d', 'KeyD'], ' move / ', ['←', 'ArrowLeft'], ['→', 'ArrowRight'], ['↑', 'ArrowUp'], ['↓', 'ArrowDown'], ' turn'],
    [['space', 'Space'], ' jump / ', ['v', 'KeyV'], ' camera / ', ['h', 'KeyH'], ' hide'],
    [['enter', 'Enter'], ' build / ', ['f', 'KeyF'], ' use / ', ['q', 'KeyQ'], ' drop'],
    [['x', 'KeyX'], ' paint / ', ['c', 'KeyC'], ' chalk / ', ['tab', 'Tab'], ' rooms'],
  ],
  pad: ['[LS] move / [RS] look', '[LT] jump / [Y] camera', '[RB] build / [X] paint', '[LB] lock on / [B] cancel'],
  touch: [
    [['left side', 'left'], ': drag to move'],
    [['right side', 'right'], ': drag to look'],
    [['tap right', 'right'], ' jumps / ', ['hold right', 'right'], ' builds'],
  ],
};
// held: { keys: [codes], pad: [badges], touch: ['left' | 'right'] } from the client.
function legendRows(input, held = {}) {
  const down = new Set(held[input === 'keys' ? 'keys' : input] || []);
  const lit = (code) => (Array.isArray(code) ? code.some((c) => down.has(c)) : down.has(code));
  return (LEGEND[input] || LEGEND.keys).map((line) =>
    typeof line === 'string'
      ? hintSpans(line, THEME.dim).map((s) => (s.badge && down.has(s.badge) ? { ...s, lit: true } : s))
      : line.map((part) =>
          typeof part === 'string'
            ? { text: part, color: THEME.dim }
            : { cap: part[0], lit: lit(part[1]) },
        ),
  );
}
import { radialSlots, ringGeometry } from './radial.mjs';
import {
  THEME,
  codeSpans,
  animateSpans,
  hintSpans,
  devSpans,
  phraseSpans,
  clipSpans,
  drawSpans,
  smallSpanWidth,
} from './highlight.mjs';
import { smallWidth } from './paint.mjs';
import { spanWidth as spanWidthOf } from './highlight.mjs';

// A dark room: night floor, slate walls; the two seat colours stay bright.
// Floor paint, cycled with X: red, yellow, green, blue, purple, pink.
const TILE_COLORS = [0xd9534f, 0xe0b23a, 0x3fa55b, 0x3f86c9, 0x8e5bd6, 0xd66fae];
// Braincells are magical purple.
const BRAIN = 0xb46bff;
// West, east, south, north.
const WALL_HUES = [0x2b3a5c, 0x4a2f5c, 0x1f4a4a, 0x3a3f4d];
const WALL = 0x3a3f4d,
  WALL_SIDE = 0x2c303b,
  LINE = 0x5a5f70;
// Two rooms by the viewer's clock: a light morning and afternoon, a dark
// evening and night. The sky bands run from the horizon glow up to overhead.
export const PALETTES = {
  day: {
    sky: 0xcfe3f5,
    far: 0xd3d8e2,
    // A pastel room: every surface its own tint, so the corners read.
    floor: 0xe2d2bb,
    dot: 0xb8a48c,
    walls: { north: 0xb7d2ec, south: 0xefc6d0, east: 0xc3e0b8, west: 0xefdca8 },
    wallSide: 0xa9acb8,
    ceiling: 0xf2eae2,
    bands: [[-0.02, 0.04, 0xfff3dc], [0.04, 0.12, 0xeaf2fb], [0.12, 0.3, 0xcfe3f5], [0.3, 0.6, 0xb4d3f0], [0.6, 1.2, 0x9ec4e9]],
  },
  night: {
    sky: 0x0f1118, // Solid: no bands (2026-11-03).
    far: 0x0f1118, // The ground beyond the walls is the sky's colour: no horizon line.
    floor: 0x2a2632,
    dot: 0x4e4360,
    walls: { north: 0x26304a, south: 0x3c2742, east: 0x233b35, west: 0x3d3527 }, // Darker pastels.
    wallSide: 0x16181f,
    ceiling: 0x222433,
    bands: [[-0.02, 0.04, 0x3a3150], [0.04, 0.12, 0x2c2a48], [0.12, 0.3, 0x1f2240], [0.3, 0.6, 0x171a30], [0.6, 1.2, 0x11131f]],
  },
};
// The room follows the viewer's colour scheme (dark mode is the night room);
// with no such signal it follows the clock.
const darkScheme = typeof matchMedia === 'function' ? matchMedia('(prefers-color-scheme: dark)') : null;
// The room is the night room always (2026-11-03): dark pastel walls, a solid dark sky.
export const paletteFor = () => 'night';
import {
  WALLS,
  COLORS,
  seatColor,
  WORLD,
  SCREEN,
  CFG,
  direction,
  wallDistance,
  along,
  rayBox,
  bodyBox,
} from './world.mjs';

// Handle glyphs take one colour per character, '@' included.
const shadowInk = (label) =>
  Array.from({ length: label.length + 1 }, () => ({ r: 24, g: 24, b: 24 }));

export async function createView(canvas) {
  let host, backend;
  let lost = false;
  try {
    host = await WebGPUOskiewarScene3D.create(canvas, {
      maxTriangles: 49152,
      onDeviceLost: () => {
        lost = true;
        canvas.dataset.engine = 'lost';
      },
    });
    backend = 'webgpu';
  } catch (error) {
    console.info(
      'WebGPU unavailable; using the shared WebGL 2 host.',
      error.message,
    );
    // A failed WebGPU initialization can already own the canvas context.
    const replacement = canvas.cloneNode(false);
    canvas.replaceWith(replacement);
    canvas = replacement;
    const { WebGLOskiewarScene3D } = await import('./engine/scene3d-webgl.mjs');
    host = new WebGLOskiewarScene3D(canvas, {
      maxTriangles: 49152,
      antialias: false,
    });
    backend = 'webgl2';
  }
  canvas.dataset.engine = backend;
  const projector = new Projector(host.scene);
  const facecam = new Projector(host.scene);
  // A small studio: a floor marked only by faint dots every two metres, four
  // plain walls and a low ceiling, all at the world's edges.
  const ROOM_HEIGHT = 4;
  const buildArena = (p) =>
    model(({ box, quad }) => {
      // (No ground beyond the walls: it was the sky's colour anyway, and a
      // centimetre under the floor it z-fought from any distance.)
      quad([-WORLD, 0, -WORLD], [-WORLD, 0, WORLD], [WORLD, 0, WORLD], [WORLD, 0, -WORLD], p.floor);
      for (let x = -WORLD + 2; x < WORLD; x += 2)
        for (let z = -WORLD + 2; z < WORLD; z += 2)
          box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot); // clear of the floor and the paint
      // Four slabs that never share volume (2026-11-03: overlapping corners
      // z-fought): north and south run the full outside width and own the
      // corners; east and west fit exactly between their inner faces.
      const h = ROOM_HEIGHT, t = 0.3, outside = 2 * WORLD + 2 * t, inside = 2 * WORLD;
      box(0, h / 2, -WORLD - t / 2, outside, h, t, p.walls.south, p.wallSide, p.walls.south);
      box(0, h / 2, WORLD + t / 2, outside, h, t, p.walls.north, p.wallSide, p.walls.north);
      // A box colours its x faces second and its z faces third: the long
      // walls show their z face, the end walls their x face.
      box(-WORLD - t / 2, h / 2, 0, t, h, inside, p.walls.west, p.walls.west, p.wallSide);
      box(WORLD + t / 2, h / 2, 0, t, h, inside, p.walls.east, p.walls.east, p.wallSide);
      // No ceiling (2026-11-03): the sky shows over the walls.
    });
  // Sky: rings of colour around the viewer, deep overhead to a glow at the
  // horizon, drawn at the eye so it never comes closer.
  const buildSky = (p) =>
    model(({ quad }) => {
      const r = 600, n = 32;
      for (const [a, b, c] of p.bands)
        for (let k = 0; k < n; k++) {
          const t0 = (k / n) * Math.PI * 2, t1 = ((k + 1) / n) * Math.PI * 2;
          const q = (t, e) => [Math.cos(t) * r * Math.cos(e), r * Math.sin(e), Math.sin(t) * r * Math.cos(e)];
          quad(q(t0, a), q(t1, a), q(t1, b), q(t0, b), c);
        }
    });
  // The room follows the clock: rebuilt when the hour crosses into day or night.
  let paletteName = null, arena, skyModel, sky;
  function dress(name) {
    if (name === paletteName) return;
    const p = PALETTES[name];
    paletteName = name;
    arena = buildArena(p);
    skyModel = buildSky(p);
    sky = [...rgb(p.sky), 1];
    document.body.style.background = '#' + p.sky.toString(16).padStart(6, '0');
  }
  dress(paletteFor());
  const fighters = COLORS.map((color) => ({
    torso: model(({ box }) =>
      box(0, 1.02, 0, 0.42, 0.62, 0.26, color, INK, PAPER),
    ),
    head: model(({ box }) => {
      box(0, 1.59, 0, 0.44, 0.42, 0.36, PAPER, 0xc0c0c0, PAPER);
      // The brain: a magical purple cap of lobes on top of the head.
      box(0, 1.835, 0, 0.36, 0.09, 0.3, BRAIN, tint(BRAIN, 0.75), tint(BRAIN, 1.15));
      box(-0.09, 1.89, 0, 0.15, 0.06, 0.26, tint(BRAIN, 1.1), tint(BRAIN, 0.8), tint(BRAIN, 1.25));
      box(0.09, 1.89, 0, 0.15, 0.06, 0.26, tint(BRAIN, 1.1), tint(BRAIN, 0.8), tint(BRAIN, 1.25));
      box(0, 1.88, 0, 0.012, 0.07, 0.27, tint(BRAIN, 0.55), tint(BRAIN, 0.55), tint(BRAIN, 0.55));
    }),
    limb: model(({ box }) =>
      box(0, -0.23, 0, 0.13, 0.46, 0.16, color, INK, PAPER),
    ),
    hand: model(({ box }) => {
      box(0, 0, 0, 0.16, 0.17, 0.2, PAPER, 0xc0c0c0, PAPER);
      box(0, -0.12, -0.06, 0.17, 0.12, 0.18, color, INK, color);
      // Five digits: four fingers along the top edge and a thumb to the side.
      for (let k = 0; k < 4; k++) box(-0.06 + k * 0.04, 0.12, 0.03, 0.03, 0.08, 0.04, PAPER, 0xc0c0c0, PAPER);
      box(0.1, 0.03, 0.05, 0.04, 0.07, 0.04, PAPER, 0xc0c0c0, PAPER);
    }),
  }));
  const rocket = model(({ box }) =>
    box(0, 0, 0, 0.16, 0.16, 0.32, 0xffff00, INK, PAPER),
  );
  const tracer = model(({ box }) => box(0, 0, 0, 0.09, 0.09, 0.09, 0xffffff, 0xffe066, 0xffffff));
  // Trail dots from bright to dim.
  const trailModels = [1, 0.75, 0.5, 0.32].map((k) => {
    const c = tint(0xffe066, k);
    return model(({ box }) => box(0, 0, 0, 0.05, 0.05, 0.05, c, c, c));
  });
  const trails = new Map();
  // Fur: each strand a thin crossed ribbon from root to tip in the surface colour.
  function drawHair(shape, x, y, z) {
    let strands;
    try {
      strands = sourceHair(shape, lastWorldTime);
    } catch {
      return;
    }
    if (!strands.length) return;
    const m = model(({ quad }) => {
      const w = 0.008;
      for (let i = 0; i < strands.length; i += 9) {
        const a = [strands[i], strands[i + 1], strands[i + 2]],
          b = [strands[i + 3], strands[i + 4], strands[i + 5]];
        const c = (Math.round(strands[i + 6] * 255) << 16) | (Math.round(strands[i + 7] * 255) << 8) | Math.round(strands[i + 8] * 255);
        quad([a[0] - w, a[1], a[2]], [b[0] - w, b[1], b[2]], [b[0] + w, b[1], b[2]], [a[0] + w, a[1], a[2]], c);
        quad([a[0], a[1], a[2] - w], [b[0], b[1], b[2] - w], [b[0], b[1], b[2] + w], [a[0], a[1], a[2] + w], c);
      }
    });
    draw(m, x, y, z);
  }
  let lastWorldTime = 0;
  const blast = model(({ box }) => box(0, 0, 0, 1, 1, 1, 0xffff00, INK, PAPER));
  const dot = model(({ box }) => box(0, 0, 0, 0.06, 0.06, 0.06, INK, INK, INK));
  // Magic: sparkles burst from each part as it lands and spiral upward.
  const SPARK = [0xffe066, 0xffffff, 0x7cc4ff, 0xd7a6ff, 0xff9ad5];
  const BRAIN_SPARK = [0xb46bff, 0xd7a6ff, 0x8e44ff, 0xf0d8ff].map((c) => model(({ box }) => box(0, 0, 0, 0.08, 0.08, 0.08, c, c, c)));
  const sparkModels = SPARK.map((c) => model(({ box }) => box(0, 0, 0, 0.09, 0.09, 0.09, c, c, c)));
  const sparkles = [],
    partCounts = new Map();
  function burstColor(x, y, z, count, models) {
    const born = performance.now();
    for (let i = 0; i < count && sparkles.length < 400; i++) {
      const a = Math.random() * Math.PI * 2, speed = 0.4 + Math.random() * 0.8;
      sparkles.push({ x, y, z, vx: Math.cos(a) * speed, vy: 2 + Math.random() * 2, vz: Math.sin(a) * speed,
        spin: (Math.random() - 0.5) * 6, born, life: 700 + Math.random() * 700, model: models[i % models.length] });
    }
  }
  function burst(x, y, z, count = 14, spread = 1) {
    const born = performance.now();
    for (let i = 0; i < count && sparkles.length < 400; i++) {
      const a = Math.random() * Math.PI * 2,
        speed = (0.8 + Math.random() * 1.6) * spread;
      sparkles.push({
        x, y, z,
        vx: Math.cos(a) * speed,
        vy: 1.5 + Math.random() * 2.5 * spread,
        vz: Math.sin(a) * speed,
        spin: (Math.random() - 0.5) * 8,
        born,
        life: 600 + Math.random() * 600,
        model: sparkModels[i % sparkModels.length],
      });
    }
  }
  const effects = [];
  // Eyes: two dark patches on the face, shifted by where the player looks
  // (dx, dy in metres) and squeezed shut to h. Cached by quantized state.
  const eyeModels = new Map();
  function eyes(dx, dy, h) {
    const key = `${Math.round(dx * 100)}|${Math.round(dy * 100)}|${Math.round(h * 100)}`;
    let m = eyeModels.get(key);
    if (!m) {
      if (eyeModels.size > 200) eyeModels.clear();
      m = model(({ rect }) => {
        rect(-0.14 + dx, 1.59 + dy + (0.07 - h) / 2, 0.19, 0.09, h, INK);
        rect(0.05 + dx, 1.59 + dy + (0.07 - h) / 2, 0.19, 0.09, h, INK);
      });
      eyeModels.set(key, m);
    }
    return m;
  }
  const blinking = (now, seat) => (now + seat * 1700) % 4200 < 130;
  // Where a player's eyes point: up/down with their aim; your own also glance
  // with your turning. Squint when just hurt, shut on a blink.
  function eyeFor(p, i, now, faceState) {
    const dy = Math.max(-1, Math.min(1, (p.pitch || 0) / 60)) * 0.045;
    const dx = faceState && i === faceState.seat ? Math.max(-1, Math.min(1, (faceState.turn || 0) * 1.4)) * 0.035 : 0;
    const hurt = faceState && i === faceState.seat && (now - faceState.hurtAt) < 600;
    const h = hurt ? 0.02 : blinking(now, i) ? 0.012 : 0.07;
    return eyes(-dx, dy, h);
  }
  const objectModels = new Map(),
    previewModels = new Map();
  let promptKey, promptModel;
  let presenceKey, presenceModel;
  let ringKey, ringModel, devKey, devModel, fpsKey, fpsModel, netKey, netModel;
  // The frame-rate readout follows AC's shared fps switch (localStorage
  // "ac:fps-hud", toggled by backtick on the prompt; client.mjs sets it).
  let SHOW_FPS = false;
  let panelTop = null;
  let stride = 0, strideAt = performance.now(); // The walk's step phase, for the camera's bob.
  let objectHintSpans = null; // What the highlighted object offers, drawn at the object.
  let cornerRect = null; // The corner block, in buffer pixels from the top left.
  let lastEye = null; // Where the camera stood last frame, for the cursor's aim ray.
  let snow = null, snowAt = 0; // The room's falling snow.
  let orbitZoom = 1; // The curtain view's picture scale (below 1 shrinks it).
  const SNOW = false; // Resting for now (2026-11-03).
  let qrRect = null, qrURL = null; // The aimed object's code tag, a button.
  let qrModel = null; // Retain the tag until its code or pixel dimensions change.
  const screenFrames = new Map(); // Standing screens' frames, by screen id.
  const chalkModels = new Map();
  let boom = 4;
  let cubeModel = null, cubeAt = 0;
  const cellBits = [];
  let tileCacheKey = '', tileModel = null;
  // The Lisp tree in space: labels and the lines between them, per source.
  const trees = new Map(),
    labels = new Map(),
    lingering = new Map();
  function treeOf(shape) {
    let t = trees.get(shape.source);
    if (t) return t;
    let nodes;
    try {
      nodes = sourceTree(shape).filter((n) => n.bounds);
    } catch {
      return null;
    }
    // One label per repeat: the first iteration speaks for the rest, and the
    // fitting wrapper's no-op move is not part of what Aesel wrote.
    const firstChild = new Map();
    for (const n of nodes) if (!firstChild.has(n.parent)) firstChild.set(n.parent, n.id);
    const hidden = new Set();
    for (const n of nodes) {
      const parent = nodes.find((m) => m.id === n.parent);
      if (hidden.has(n.parent) || (parent?.text.startsWith('(repeat') && firstChild.get(parent.id) !== n.id))
        hidden.add(n.id);
    }
    nodes = nodes.filter((n) => !hidden.has(n.id) && n.text !== '(move 0 0 0 ...)');
    const byId = new Map(nodes.map((n) => [n.id, n]));
    const depth = (n) => (n.parent < 0 ? 0 : 1 + depth(byId.get(n.parent) || { parent: -1 }));
    const leafy = (n) => !nodes.some((m) => m.parent === n.id);
    // Each label hangs on what it makes: a leaf just above its own part, a
    // branch just above its children's labels, so the tree sits on the object.
    const yOf = new Map();
    const place = (n) => {
      if (yOf.has(n.id)) return yOf.get(n.id);
      const kids = nodes.filter((m) => m.parent === n.id);
      const y = kids.length
        ? Math.max(...kids.map(place)) + 0.4
        : n.bounds.yMax + 0.2;
      yOf.set(n.id, y);
      return y;
    };
    nodes.forEach(place);
    const placed = nodes.slice(0, 24).map((n) => {
      const b = n.bounds;
      return {
        ...n,
        text: n.id === 0 ? `; ${shape.name}` : n.text,
        leaf: leafy(n) && n.id > 0,
        x: (b.xMin + b.xMax) / 2,
        y: yOf.get(n.id),
        z: (b.zMin + b.zMax) / 2,
      };
    });
    const at = new Map(placed.map((n) => [n.id, n]));
    const lines = model(({ quad, box }) => {
      const seg = (a, b, color) => {
        const w = 0.008;
        quad([a[0] - w, a[1], a[2]], [b[0] - w, b[1], b[2]], [b[0] + w, b[1], b[2]], [a[0] + w, a[1], a[2]], color);
        quad([a[0], a[1], a[2] - w], [b[0], b[1], b[2] - w], [b[0], b[1], b[2] + w], [a[0], a[1], a[2] + w], color);
      };
      for (const n of placed) {
        const parent = at.get(n.parent);
        if (parent) seg([n.x, n.y + 0.1, n.z], [parent.x, parent.y - 0.05, parent.z], LINE);
        if (n.leaf) {
          // The points the form names: its two extreme corners, marked and tied to the text.
          const b = n.bounds;
          for (const corner of [
            [b.xMin, b.yMin, b.zMin],
            [b.xMax, b.yMax, b.zMax],
          ]) {
            box(corner[0], corner[1], corner[2], 0.07, 0.07, 0.07, THEME.number, THEME.number, THEME.number);
            seg([n.x, n.y - 0.05, n.z], corner, THEME.head);
          }
        }
      }
    });
    t = { placed, lines };
    if (trees.size > 24) trees.clear();
    trees.set(shape.source, t);
    return t;
  }
  function labelModel(text) {
    let m = labels.get(text);
    if (!m) {
      const clipped = text.length > 36 ? text.slice(0, 33) + '...' : text;
      m = model(({ rect: worldRect, text: glyphs }) =>
        drawSpans(
          { rect: (x, y, z, w, h, c) => worldRect(-x - w, y, z, w, h, c), text: glyphs },
          text.startsWith(';') ? [{ text: clipped, color: THEME.dim }] : codeSpans(clipped),
          0,
          0.1,
          0.02,
          { step: 0.002, shadow: null },
        ),
      );
      if (labels.size > 256) labels.clear();
      labels.set(text, m);
    }
    return m;
  }
  function drawTree(shape, ox, oz, eye) {
    // The tree is decoration: skip it when the scene is already busy, so the
    // world and the HUD always keep their share of the triangle budget.
    if (host.scene.triangleCount > host.scene.maxTriangles * 0.45) return;
    const t = treeOf(shape);
    if (!t) return;
    draw(t.lines, ox, 0, oz);
    for (const n of t.placed) {
      const x = ox + n.x,
        z = oz + n.z;
      draw(
        labelModel(n.text),
        x,
        n.y,
        z,
        Math.atan2(eye.x - x, eye.z - z) + Math.PI,
        0,
        // A real size in the world, so labels shrink as you walk away.
        1.4,
      );
    }
  }
  let aspect = 1,
    pixelScale = 1,
    grid = null; // The host's pixel grid, when the prompt is the paper over us.
  function resize() {
    if (grid) {
      // Pixel-matched to the prompt (2026-11-03): one buffer pixel is one of
      // the prompt's buffer pixels, on the same grid. The prompt's canvas sits
      // at (left, top) in CSS pixels; our canvas starts a fraction of a pixel
      // before the viewport's edge so the two grids coincide, and fills past
      // the far edge. The HUD unit is then 1, the prompt's own font size.
      const p = Math.max(0.5, grid.pixel);
      const shift = (v) => {
        const r = ((v % p) + p) % p;
        return r ? r - p : 0;
      };
      const left = shift(grid.left),
        top = shift(grid.top);
      const cols = Math.ceil((innerWidth - left) / p),
        rows = Math.ceil((innerHeight - top) / p);
      Object.assign(canvas.style, {
        position: 'absolute',
        left: `${left}px`,
        top: `${top}px`,
        width: `${cols * p}px`,
        height: `${rows * p}px`,
      });
      host.resize(cols, rows);
      aspect = (cols * p) / (rows * p);
      pixelScale = p;
      return;
    }
    const width = Math.max(1, canvas.clientWidth),
      height = Math.max(1, canvas.clientHeight);
    // Whole steps keep the buffer at least 640x360; on a HiDPI screen each
    // step is three quarters of a CSS pixel, so the HUD's letters read at
    // 10.5 CSS pixels (the size 75% browser zoom gave) and the world gains
    // detail. A 1x screen keeps whole CSS pixels.
    const step = Math.max(1, Math.floor(Math.min(width / 640, height / 360)));
    const scale = step * (devicePixelRatio >= 1.5 ? 0.75 : 1);
    host.resize(Math.ceil(width / scale), Math.ceil(height / scale));
    aspect = width / height;
    pixelScale = scale;
  }
  const observer = new ResizeObserver(resize);
  observer.observe(canvas);
  addEventListener('resize', () => grid && resize());
  resize();
  function effect(e) {
    // Spending braincells: a purple puff from the brain.
    if (e.type === 'think' && e.at) burstColor(e.at.x, e.at.y, e.at.z, 18, BRAIN_SPARK);
    // Ballistic contact: sparks where a round ricochets, cancels or lands.
    if (e.at && (e.type === 'ricochet' || e.type === 'spark')) burst(e.at.x, e.at.y, e.at.z, 6, 0.5);
    if (e.at && e.type === 'cancel') burst(e.at.x, e.at.y, e.at.z, 16, 0.9);
    if (e.at && e.type === 'hit') burst(e.at.x, e.at.y + 0.6, e.at.z, 10, 0.7);
    if (e.at && e.type === 'removed') burst(e.at.x, e.at.y, e.at.z, 24, 1.3);
    if (e.at && e.type === 'dust') burst(e.at.x, e.at.y, e.at.z, 3, 0.25);
    if (e.type === 'blast') {
      if (effects.length >= 24) effects.shift();
      effects.push({ at: e.at, born: performance.now() });
    }
  }
  const draw = (...args) => projector.draw(...args);
  function render({
    players = [],
    me,
    camera,
    spectator = false,
    controller,
    seat = 0,
    third = false,
    rockets = [],
    status = '',
    members = [],
    self = null,
    connection,
    joinURL,
    objects = [],
    prompt,
    radial = null,
    items = [],
    bullets = [],
    since = 0,
    holding = null,
    touch = null,
    input = 'keys',
    devLog = [],
    fps = 0,
    walls = [],
    cube = [],
    menu = null,
    removeArmed = null,
    lockedOn = false,
    notices = [],
    braincells = null,
    spending = false,
    tiles = {},
    floorAim = null,
    chalk = [],
    chalkLocal = [],
    chalkMode = null,
    face = null,
    owner,
    focusObject,
    builder = false,
    builderNames = [],
    cursor = null, // The mouse, in CSS pixels, when it (not the lock) is the aim.
    reticle = false, // Draw the reticle even while bare: the paper is moving.
    reticleShift = null, // On the way home, where the reticle has flown to (buffer px, y up).
    orbit = 0, // 1: the far camera circling the room (behind the curtain); 0: the player's own.
    flyTo = null, // Where the flight in lands while the seat is still being granted.
    cornerScale = 1, // The curtain crosshair's breadth (1 at rest); the room scales with it.
    netLog = [], // The connection's own story, newest last, shown while it is down.
    connected = true,
    offline = false, // The sheet is open and the connection is down: the log panel, nothing else.
    jobs = [],
    worldTime = 0,
    locked = false,
    legend = false,
    held = {},
    corner = false,
    screens = [],
    screenPrograms = new Map(),
    bare = false, // Behind the prompt's curtain: no panel, hint or legend.
  } = {}) {
    if (lost) {
      location.reload();
      return;
    }
    const start = performance.now(),
      now = start;
    const solidObjects = objects.filter((o) => !o.item);
    lastWorldTime = worldTime;
    // The corner join code is retired for now: dool is joining the AC homepage.
    const cells = null;
    const hud = hudLayout(canvas.width, canvas.height, cells, pixelScale, grid ? 1 : 0);
    dress(paletteFor());
    host.beginFrame();
    let eye = { x: 15, y: 19, z: -22 },
      aim = { x: 0, y: 0, z: 0 };
    if (me) {
      const dir = direction(me.yaw, me.pitch),
        origin = { x: me.x, y: me.y, z: me.z };
      eye = origin;
      if (third) {
        const back = { x: -dir.x, y: -dir.y + 0.22, z: -dir.z },
          len = Math.hypot(back.x, back.y, back.z);
        for (const k of ['x', 'y', 'z']) back[k] /= len;
        // Ease the boom in and out rather than snapping as things pass.
        const want = Math.max(0, wallDistance(origin, back, 4, solidObjects, worldTime) - 0.2);
        boom = want < boom ? want : boom + (want - boom) * 0.12;
        eye = along(origin, back, boom);
      }
      let distance = wallDistance(origin, dir, 100, solidObjects, worldTime);
      players.forEach((p, i) => {
        if (i !== seat && p?.alive)
          distance = Math.min(distance, rayBox(origin, dir, bodyBox(p), 100));
      });
      aim = along(origin, dir, Math.max(0.1, distance - 0.1));
    }
    if (camera) {
      eye = camera;
      aim = along(camera, direction(camera.yaw, camera.pitch), 100);
    }
    // Behind the curtain the room turns slowly, seen from far off and above;
    // the drop flies the camera down a curve that passes behind the player's
    // head and into their eyes (2026-11-03). `orbit` eases 1 → 0 on the drop.
    // The room turns small in the bottom-right corner, right under the
    // curtain's crosshair (a lens shift puts the room's centre there), so the
    // curtain stays mostly 2D and the corner is the way in (2026-11-03).
    projector.shiftX = projector.shiftY = 0;
    if (orbit > 0) {
      const a = now / 3200; // a slow spin, about one turn in twenty seconds
      // Straight down from high above, far enough that the room fits inside
      // the crosshair's 16 px box (half 7 px) when square to it; a slight
      // tilt sets which way is up, and as it circles the room turns
      // (2026-11-03). sy = 1 on a landscape screen (Projector.camera).
      // The camera hovers 25 m up (depth stays precise; from hundreds of
      // metres the floor and its marks z-fought) and a zoom factor shrinks
      // the picture instead of distance, eased back to 1 as it flies in.
      const tan = aspect < 1 ? Math.min(Math.sqrt(3), 1 / aspect) : 1;
      const height = 25;
      const natural = ((WORLD + 0.3) / height) * (1 / tan) * (canvas.height / 2); // room half-size in px
      orbitZoom = Math.pow((7 * cornerScale) / natural, 1 - (1 - orbit) * (1 - orbit) * (3 - 2 * (1 - orbit)));
      const far = { x: Math.sin(a) * height * 0.002, y: height, z: -Math.cos(a) * height * 0.002 };
      const px = canvas.width / Math.max(1, canvas.clientWidth);
      // The crosshair's centre sits 22 prompt pixels in from the corner; on
      // the host's grid a buffer pixel is a prompt pixel.
      const inset = grid ? 22 : 22 * px;
      const cornerX = (canvas.width - inset) / canvas.width * 2 - 1, cornerY = 1 - (canvas.height - inset) / canvas.height * 2;
      const k0 = 1 - orbit, e0 = k0 * k0 * (3 - 2 * k0);
      projector.shiftX = cornerX * (1 - e0);
      projector.shiftY = cornerY * (1 - e0);
      const centre = { x: 0, y: 0, z: 0 };
      const me0 = me || flyTo; // Before the seat is granted, fly toward the spawn.
      if (!me0 || orbit >= 1) {
        eye = far;
        aim = centre;
      } else {
        const k = 1 - orbit, e = k * k * (3 - 2 * k); // smoothstep along the curve
        const f = direction(me0.yaw, 0);
        const behind = { x: me0.x - f.x * 3.2, y: me0.y + 1.4, z: me0.z - f.z * 3.2 };
        if (!me) {
          // No seat yet: the eye and aim the seat will have.
          eye = { x: me0.x, y: me0.y, z: me0.z };
          aim = along(eye, direction(me0.yaw, 0), 10);
        }
        const bez = (p, q, r) => (1 - e) * (1 - e) * p + 2 * (1 - e) * e * q + e * e * r;
        eye = { x: bez(far.x, behind.x, eye.x), y: bez(far.y, behind.y, eye.y), z: bez(far.z, behind.z, eye.z) };
        aim = {
          x: centre.x + (aim.x - centre.x) * e,
          y: centre.y + (aim.y - centre.y) * e,
          z: centre.z + (aim.z - centre.z) * e,
        };
      }
    }
    // Breathing: a slow rise and fall with a slighter sway, so the room is
    // never quite still, from the first frame behind the curtain on. Walking
    // adds a step-paced bob with a small roll and a nod (2026-11-03): the
    // head tilts side to side with each stride and dips as the foot lands.
    let roll = 0;
    {
      const breath = Math.sin(now / 1900) * 0.03, sway = Math.sin(now / 3100) * 0.015;
      const pace = me ? Math.min(1, Math.hypot(me.vx || 0, me.vz || 0) / 6) : 0;
      stride += (pace * (now - strideAt)) / 1000 * 9; // about 1.4 steps a second at full pace
      strideAt = now;
      const bob = Math.sin(stride) * 0.045 * pace, lean = Math.sin(stride / 2) * 0.02 * pace;
      roll = Math.sin(stride / 2) * 0.012 * pace + Math.sin(now / 2600) * 0.0025;
      const r = projector.right || [1, 0, 0];
      eye = { ...eye, x: eye.x + sway + r[0] * lean, y: eye.y + breath + bob, z: eye.z + r[2] * lean };
      aim = { ...aim, x: aim.x + sway + r[0] * lean, y: aim.y + breath + bob * 0.6, z: aim.z + r[2] * lean };
    }
    projector.camera(eye, aim, aspect, roll);
    if (orbit > 0 && orbitZoom !== 1) {
      projector.sx *= orbitZoom;
      projector.sy *= orbitZoom;
    }
    orbitZoom = 1;
    // The sky is the clear colour alone: no banded dome (2026-11-03).
    lastEye = eye;
    const tags = [];
    draw(arena);
    const aliveObjects = new Set(objects.map((o) => o.id));
    for (const id of objectModels.keys())
      if (!aliveObjects.has(id)) objectModels.delete(id);
    const poses = new Map(items.map((it) => [it.id, it]));
    for (const object of objects) {
      if (object.item) {
        const it = poses.get(object.id);
        if (!it || object.geometry.format !== OBJECT_FORMAT) continue;
        // In hand: follow the holder's aim; loose: tumble with its spin.
        let at = it, yaw = (now / 1000) * (it.spin || 0), pitch = 0;
        const holder = it.holder >= 0 ? (it.holder === seat && me ? me : players[it.holder]) : null;
        if (holder) {
          const f = direction(holder.yaw, holder.pitch),
            r = direction(holder.yaw - 90);
          at = {
            x: holder.x + r.x * 0.3 + f.x * 0.45,
            y: holder.y - 0.55 + f.y * 0.45,
            z: holder.z + r.z * 0.3 + f.z * 0.45,
          };
          yaw = (holder.yaw * Math.PI) / 180;
          pitch = (-holder.pitch * Math.PI) / 180;
          // First person: keep your own item out of the middle of the view.
          if (it.holder === seat && !third) at = { x: at.x, y: at.y - 0.1, z: at.z };
        }
        draw(sourceFrame(object.geometry, worldTime).vertices, at.x, at.y, at.z, yaw, pitch);
        continue;
      }
      if (object.geometry.format === OBJECT_FORMAT) {
        draw(
          sourceFrame(object.geometry, worldTime).vertices,
          object.x,
          0,
          object.z,
        );
        drawHair(object.geometry, object.x, 0, object.z);
        continue;
      }
      let cached = objectModels.get(object.id);
      if (!cached || cached.version !== object.version) {
        cached = {
          version: object.version,
          parts: object.geometry.boxes.map((b) =>
            model(({ box }) =>
              box(0, 0, 0, b.w, b.h, b.d, b.color, INK, PAPER),
            ),
          ),
          model: model(({ box }) => {
            for (const b of object.geometry.boxes)
              box(b.x, b.y, b.z, b.w, b.h, b.d, b.color, INK, PAPER);
          }),
        };
        objectModels.set(object.id, cached);
      }
      if (object.geometry.motion) {
        const pose = poseObject(object, worldTime);
        pose.geometry.boxes.forEach((b, i) =>
          draw(
            cached.parts[i],
            pose.x + b.x,
            (pose.y || 0) + b.y,
            pose.z + b.z,
            0,
            0,
            1,
            b.roll || 0,
          ),
        );
      } else draw(cached.model, object.x, 0, object.z);
    }
    for (const id of previewModels.keys())
      if (!jobs.some((j) => j.id === id)) previewModels.delete(id);
    for (const job of jobs) {
      if (job.kind === 'kidlisp') continue; // a wall rewrite has no footprint
      const code = JSON.stringify(job.preview);
      let preview = previewModels.get(job.id);
      if (!preview || preview.code !== code) {
        preview = {
          code,
          model: model(({ quad }) => {
            // Before any geometry, a footprint; once the object streams in
            // solid, only the newest part wears a wire edge (2026-11-03), so
            // the outline is the object's own shape, not a cage.
            const parts = job.preview?.boxes || [];
            const boxes = job.preview?.format === OBJECT_FORMAT && parts.length
              ? parts.slice(-1)
              : parts.length
                ? parts
                : [{ x: 0, y: 0.1, z: 0, w: 2, h: 0.2, d: 3 }];
            const ink = job.owner === owner ? 0x4f67cc : 0xc17824;
            const line = 0.06;
            const edge = (a, b) => {
              const axis = a[0] !== b[0] ? 2 : 0;
              const c = [...b],
                d = [...a];
              c[axis] += line;
              d[axis] += line;
              quad(a, b, c, d, ink);
            };
            for (const b of boxes) {
              const x = b.x - b.w / 2,
                X = b.x + b.w / 2,
                z = b.z - b.d / 2,
                Z = b.z + b.d / 2;
              const y = Math.max(0.025, b.y - b.h / 2),
                Y = b.y + b.h / 2;
              for (const h of [y, Y]) {
                edge([x, h, z], [X, h, z]);
                edge([X, h, z], [X, h, Z]);
                edge([X, h, Z], [x, h, Z]);
                edge([x, h, Z], [x, h, z]);
              }
              if (job.preview)
                for (const [a, c] of [
                  [x, z],
                  [X, z],
                  [X, Z],
                  [x, Z],
                ])
                  edge([a, y, c], [a, Y, c]);
            }
          }),
        };
        previewModels.set(job.id, preview);
      }
      draw(preview.model, job.x, 0, job.z);
      const parts = job.preview?.boxes || [];
      const seen = partCounts.get(job.id) ?? 0;
      for (const b of parts.slice(seen)) burst(job.x + b.x, b.y + b.h / 2, job.z + b.z);
      partCounts.set(job.id, Math.max(seen, parts.length));
      // The object itself, solid, growing as Aesel writes it, with its tree.
      if (job.preview?.format === OBJECT_FORMAT) {
        try {
          draw(sourceFrame(job.preview, worldTime).vertices, job.x, 0, job.z);
          // The code tree stays out of the room (2026-11-03): the $code tag has it.
        } catch {}
        lingering.set(job.objectId, now + 10000);
      }
    }
    // Your build's status floats over it while it is highlighted (the aim
    // on its square or its object), with the way to cancel (2026-11-03).
    for (const job of jobs) {
      if (job.owner !== owner || job.kind === 'kidlisp') continue;
      const near = (focusObject && focusObject.id === job.objectId) ||
        (floorAim && Math.hypot(-18 + floorAim.i * 2 + 1 - job.x, -18 + floorAim.j * 2 + 1 - job.z) < 2.5);
      if (!near) continue;
      const s = projector.project(job.x, 1.8, job.z);
      if (!s || Math.abs(s.x) > 1.1 || Math.abs(s.y) > 1.1) continue;
      const { unit } = hud;
      const seconds = Math.max(0, Math.floor((Date.now() - job.startedAt) / 1000));
      const rows = [
        [{ text: (job.prompt || 'building').slice(0, 40), color: THEME.text }],
        [{ text: `v${job.version} ${job.phase} ${seconds}s`, color: THEME.dim }],
        hintSpans(input === 'pad' ? '[B] cancel' : '{esc} cancel', THEME.dim),
      ];
      const sx = Math.round((s.x * W) / 2), sy = Math.round((s.y * H) / 2);
      const m = model(({ rect: worldRect, text }) =>
        rows.forEach((spans, i) => drawSpans({ rect: flip(worldRect), text }, spans, sx, sy + (rows.length - i) * 11 * unit, unit, { shadow: INK })),
      );
      projector.overlay(m, W, H);
    }
    // A job that ends with its object present has landed: one big burst.
    for (const [id] of partCounts)
      if (!jobs.some((j) => j.id === id)) partCounts.delete(id);
    for (const [objectId, until] of lingering)
      if (until - now > 9900 && !jobs.some((j) => j.objectId === objectId)) {
        const o = objects.find((x) => x.id === objectId);
        if (o && !o.landed) {
          o.landed = true;
          for (const b of o.geometry.boxes || []) burst(o.x + b.x, b.y, o.z + b.z, 8, 1.4);
        }
      }
    // Keep a finished object's tree up for a few seconds after it lands.
    for (const [id, until] of lingering) {
      if (until < now) lingering.delete(id);
      // (The object's code tree is no longer drawn in the room; the lingering
      // window still times the landing burst above.)
    }
    for (let i = 0; i < Math.max(players.length, seat + 1); i++) {
      const p = i === seat && me ? me : players[i];
      if (!p?.alive) continue;
      const f = fighters[i % fighters.length],
        yaw = (p.yaw * Math.PI) / 180,
        base = p.y - CFG.eyeHeight;
      if (i !== seat || third || !me) {
        draw(f.torso, p.x, base, p.z, yaw);
        draw(f.head, p.x, base, p.z, yaw);
        draw(eyeFor(p, i, now, face), p.x, base, p.z, yaw);
        // Name tags are screen text drawn after the world (see below), so
        // they stay crisp and never fight the scene for depth.
        if (i !== seat) tags.push({ seat: i, x: p.x, y: base + 2.15, z: p.z });
        const stride = Math.min(1, Math.hypot(p.vx, p.vz) / 10),
          right = direction(p.yaw - 90);
        for (const side of [-1, 1]) {
          const step =
            Math.sin(now / 85 + (side === 1 ? Math.PI : 0)) * stride * 0.65;
          const armPitch = -0.55 - step * 0.2 - (p.pitch * Math.PI) / 180;
          draw(
            f.limb,
            p.x + right.x * 0.15 * side,
            base + 0.68,
            p.z + right.z * 0.15 * side,
            yaw,
            step,
          );
          draw(
            f.limb,
            p.x + right.x * 0.3 * side,
            base + 1.27,
            p.z + right.z * 0.3 * side,
            yaw,
            armPitch,
          );
          draw(
            f.hand,
            p.x +
              right.x * 0.3 * side -
              Math.sin(yaw) * Math.sin(armPitch) * 0.46,
            base + 1.27 - Math.cos(armPitch) * 0.46,
            p.z +
              right.z * 0.3 * side -
              Math.cos(yaw) * Math.sin(armPitch) * 0.46,
            yaw,
            armPitch + 0.5,
          );
        }
      }
    }
    for (let i = sparkles.length - 1; i >= 0; i--) {
      const s = sparkles[i],
        t = (now - s.born) / 1000,
        k = t / (s.life / 1000);
      if (k >= 1) {
        sparkles.splice(i, 1);
        continue;
      }
      // Up and outward, curling as they rise, shrinking to nothing.
      const curl = s.spin * t;
      draw(
        s.model,
        s.x + (s.vx * Math.cos(curl) - s.vz * Math.sin(curl)) * t,
        s.y + s.vy * t - 1.2 * t * t,
        s.z + (s.vx * Math.sin(curl) + s.vz * Math.cos(curl)) * t,
        curl,
        curl,
        1 - k * k,
      );
    }
    // Rounds: carried forward from the last update by their velocity, with a
    // trail of where they have been (bends at every ricochet) fading behind.
    const live = new Set();
    for (const b of bullets) {
      live.add(b.id);
      const t = Math.min(0.1, since);
      const at = { x: b.x + b.vx * t, y: b.y + b.vy * t, z: b.z + b.vz * t };
      let trail = trails.get(b.id);
      if (!trail) trails.set(b.id, (trail = []));
      const last = trail[trail.length - 1];
      if (!last || Math.hypot(last.x - at.x, last.y - at.y, last.z - at.z) > 0.35) trail.push(at);
      if (trail.length > 14) trail.shift();
      draw(tracer, at.x, at.y, at.z);
      trail.forEach((p, i) => {
        const k = (i + 1) / trail.length;
        draw(trailModels[Math.min(trailModels.length - 1, Math.floor((1 - k) * trailModels.length))], p.x, p.y, p.z, 0, 0, 0.4 + 0.6 * k);
      });
    }
    for (const id of trails.keys()) if (!live.has(id)) trails.delete(id);
    for (const r of rockets)
      draw(rocket, r.x, r.y, r.z, Math.atan2(r.dir.x, r.dir.z));
    // (The aim reticle is drawn on screen after the world; see below.)
    for (let i = effects.length - 1; i >= 0; i--) {
      const e = effects[i],
        age = (now - e.born) / 180;
      if (age > 1) effects.splice(i, 1);
      else draw(blast, e.at.x, e.at.y, e.at.z, 0, 0, 0.2 + age * 1.5);
    }
    if (prompt) {
      const width = canvas.width,
        height = canvas.height;
      const { unit, safeX, safeY } = hud;
      const columns = Math.max(12, Math.min(48, Math.floor((width - 2 * safeX - 16) / (6 * unit))));
      const wrap = (line) => line.match(new RegExp(`.{1,${columns}}`, 'g')) || [''];
      const cursor = now % 1000 < 500 ? '_' : ' ';
      const pad = input === 'pad';
      const title = prompt.target
        ? `${prompt.target.code ? '$' + prompt.target.code : prompt.target.geometry.name} v${prompt.target.version} > v${prompt.target.version + 1}`
        : 'aesel';
      const writing =
        prompt.text.slice(0, prompt.cursor) +
        cursor +
        prompt.text.slice(prompt.cursor);
      const named = (o) => (o.code ? `$${o.code}` : o.geometry.name);
      const verb = holding ? { shoot: 'shoot', poke: 'poke', throw: 'throw' }[holding.item] : null;
      const carry = holding
        ? pad
          ? `[RB] ${verb} / [X] drop`
          : input === 'touch'
            ? `tap ${verb}`
            : `f or click ${verb} / q drop`
        : null;
      const drawing = chalkMode
        ? pad
          ? `chalk ${chalkMode.text}: hold [RT] draw / [X] build / [B] cancel`
          : `chalk ${chalkMode.text}: hold c or right click / enter build / esc cancel`
        : null;
      const whose = (o) =>
        o.wall
          ? pad ? '[RB] to change' : 'kidlisp wall'
          : o.owner === owner
            ? pad ? '[RB] change / [LB] lock on' : 'yours - {delete} removes'
            : pad ? '[LB] lock on' : 'player ' + String(o.owner || '').slice(0, 4);
      const paintHint = !focusObject && floorAim && !holding ? (pad ? '[X] paint the floor' : '{x} paints the floor') : null;
      const confirming = focusObject && removeArmed === focusObject.id ? '{delete} again to remove' : null;
      const lockHint = lockedOn && focusObject
        ? focusObject.wall
          ? `${named(focusObject)} locked: [RB] change / [LB] unlock`
          : `${named(focusObject)} locked: [B] remove / [RB] change / dpad next / [LB] unlock`
        : null;
      // What the highlighted object offers ([B] remove, [LB] unlock, its
      // name and owner) is drawn at the object itself (2026-11-03); the
      // corner keeps only the general hint.
      const objectHint = lockHint
        ? lockHint
        : confirming
          ? confirming
          : focusObject
            ? `${named(focusObject)}${focusObject.wall ? '' : ` v${focusObject.version}`} - ${whose(focusObject)}`
            : null;
      objectHintSpans = null; // The lock-on shows only its bracket; its offers read in the corner.
      const hint = objectHint ? objectHint : drawing ? drawing : carry ? carry : paintHint ? paintHint
        : input === 'touch'
          ? 'hold the right side to build'
          : pad
            ? '[RB] to build'
            : '{enter} to build';
      const building = jobs.find((j) => j.owner === owner) || jobs[0];
      const elapsed = building
        ? Math.max(0, Math.floor((Date.now() - building.startedAt) / 1000))
        : 0;
      const progressCode =
        building?.code
          ?.replace(/,/g, ',\n')
          .split('\n')
          .filter(Boolean)
          .slice(-3)
          .flatMap(wrap)
          .slice(-3) || [];
      const cancel = pad ? '[B] cancel' : locked ? '{esc} cancel' : 'click to move / {esc} cancel';
      // A KidLisp wall in the crosshair: its source, highlighted, in the panel.
      const wallCode =
        !prompt.open && !building && focusObject?.wall && focusObject.source
          ? focusObject.source
              .split('\n')
              .filter((l) => l.trim())
              .slice(0, 6)
              .flatMap(wrap)
              .slice(0, 7)
          : null;
      // The legend only joins the plain hint, never a panel.
      const showLegend = legend && !spectator && !wallCode && !prompt.open && !building;
      const rows = bare
        ? []
        : wallCode
        ? [
            [
              { text: `$${focusObject.code}`, color: THEME.verb },
              { text: ` kidlisp on the wall - ${focusObject.size || ''}`, color: THEME.dim },
            ],
            ...(focusObject.colored?.()?.slice(0, 7).map((l) => clipSpans(l, columns)) ||
              wallCode.map((l) => animateSpans(codeSpans(l), now))),
          ]
        : prompt.open
        ? [
            // The typed line first, on the 2D prompt's own grid, so the text
            // stays put across the trip in and out of the paper.
            ...wrap(writing).map((l) => (radial ? phraseSpans(l) : [{ text: l, color: THEME.text }])),
            [
              // Whether a builder is listening, told where it matters: in the
              // prompt; after the revision's title when there is one.
              ...(prompt.target ? [{ text: `${title} `, color: THEME.verb }] : []),
              builder
                ? { text: `aesel online${builderNames.length ? ': ' + builderNames.join(' + ') : ''}`, color: 0x3fb950 }
                : { text: 'aesel offline', color: 0xff6b6b },
            ],
            ...(prompt.message ? wrap(prompt.message).map((l) => [{ text: l, color: THEME.number }]) : []),
            hintSpans(
              radial
                ? input === 'touch'
                  ? 'tap a choice / centre builds / outside undoes'
                  : '[A] choose / [X] build / [B] undo'
                : prompt.busy
                  ? cancel
                  : '{enter} build / {esc} close',
              THEME.dim,
            ),
          ]
        : [
              // The controls for the hand in use, until the player has played
              // a while (H, on the keys, brings them back).
              ...(showLegend ? legendRows(input, held) : []),
              spectator
                ? hintSpans(
                    controller ? 'freecam / [LB] down / [RB] up' : 'freecam / {wasd} / {space} up / {shift} down',
                    THEME.text,
                  )
                : hintSpans(hint, confirming ? 0xff6b6b : THEME.text),
            ];
      const visible = rows.slice(
        -Math.max(3, Math.floor((height - 2 * safeY) / (11 * unit)) - 3),
      ).map((r) => clipSpans(r, columns));
      const key = JSON.stringify([visible, width, height, prompt.open, !!radial, !!wallCode]);
      if (key !== promptKey) {
        promptKey = key;
        promptModel = model(({ rect: worldRect, text }) => {
          const rect = (x, y, z, w, h, color) => worldRect(-x - w, y, z, w, h, color);
          const panel = prompt.open; // A build's status floats over the build itself.
          // Nothing here lifts the corners (2026-11-03). The prompt and a
          // build in progress sit top-left on the 2D prompt's own text grid
          // (6, 6; ten-pixel lines) so the typed line is pixel for pixel
          // where the paper shows it; the legend and the hint line are
          // right-aligned at the top-right.
          panelTop = null;
          const widths = visible.map((spans) => spanWidthOf(spans, unit));
          if (panel) {
            const ox = grid ? 6 : safeX, oy = grid ? 6 : safeY;
            visible.forEach((spans, i) =>
              drawSpans({ rect, text }, spans, -width / 2 + ox + widths[i] / 2, height / 2 - oy - 2 * unit - i * 10 * unit, unit, { shadow: INK }),
            );
          } else {
            const right = width / 2 - safeX;
            visible.forEach((spans, i) =>
              drawSpans({ rect, text }, spans, right - widths[i] / 2, height / 2 - safeY - 2 * unit - i * 11 * unit, unit, { shadow: INK }),
            );
          }
        });
      }
      projector.overlay(promptModel, width, height);
    }
    if (members.length || connection || cells || fps) {
      const width = canvas.width,
        height = canvas.height,
        { unit, safeX, safeY, qrSize } = hud;
      const labelColumns = Math.max(4, Math.floor((width - 2 * safeX - Math.max(qrSize, 90) - 18 * unit) / (6 * unit)));
      const labels = (self ? [self] : members.filter((m) => seat >= 0 ? m.seat === seat : owner && m.owner === owner).slice(0, 1)).map((m) => ({
        seat: m.seat,
        label: (m.handle ? `@${m.handle}` : m.name || `GUEST ${m.seat + 1}`).slice(0, labelColumns),
        handle: m.handle,
        colors: m.colors,
      }));
      const meter = (connection?.label || '').toLowerCase();
      const jitter =
        connection?.samples && connection.jitter >= 10
          ? `j ${Math.round(connection.jitter)} ms`
          : '';
      const key = JSON.stringify([
        labels,
        meter,
        jitter,
        connection?.bars,
        width,
        height,
        joinURL,
        panelTop,
        fps,
        !!face,
        bare,
      ]);
      if (key !== presenceKey) {
        presenceKey = key;
        presenceModel = model(({ rect: worldRect, text, handleText, small }) => {
          // Text already uses screen-right coordinates; ordinary faces need the same flip.
          const rect = (x, y, z, w, h, color) =>
            worldRect(-x - w, y, z, w, h, color);
          // Bottom left is the local identity; other names stay on their heads.
          const floor = Math.max(-height / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit);
          labels.forEach(({ label, seat, handle, colors }, i) => {
            const x = -width / 2 + safeX + 2,
              y = floor + (labels.length - 1 - i) * 12 * unit;
            // No seat chip: the handle's own colours identify the player.
            const cx = x + ((label.length * 6 - 1) * unit) / 2;
            (handle ? handleText : text)(
              label,
              cx + unit,
              y - unit,
              0.000002,
              unit,
              handle ? shadowInk(label) : INK,
            );
            (handle ? handleText : text)(
              label,
              cx,
              y,
              0,
              unit,
              handle ? colors : PAPER,
            );
          });
          if ((meter || (SHOW_FPS && fps)) && !face && !bare) {
            // No face cam to sit under: shadowed text right-aligned under the QR, coloured by health.
            const ping = (connection?.label || '').toLowerCase();
            const bars = connection?.bars || 0;
            const good = 0x3fb950, fair = 0xffe066, poor = 0xff6b6b;
            const pingColor = !ping ? THEME.dim : bars >= 3 ? good : bars >= 2 ? fair : poor;
            const fpsColor = fps >= 55 ? good : fps >= 30 ? fair : poor;
            const right = width / 2 - safeX;
            let y = height / 2 - safeY - (cells ? qrSize + 16 * unit : 2 * unit);
            const line = (spans) => {
              const w = smallSpanWidth(spans, unit);
              drawSpans({ rect, text, small }, spans, right - w / 2, y, unit, { tiny: true });
              y -= 9 * unit;
            };
            // One number; its colour carries the connection's health.
            if (ping) {
              // Signal as a staircase of four bars rising left to right,
              // lit bars in the health colour, the rest dark.
              const wx = right - (smallWidth(ping) + 1) * unit - 13 * unit;
              const base = y - 7 * unit;
              for (let i = 0; i < 4; i++) {
                const h = (2 + i * 2) * unit,
                  x = wx + i * 3 * unit;
                const lit = i < bars;
                rect(x + unit, base - unit, 0.000002, 2 * unit, h, INK);
                rect(x, base, 0.000001, 2 * unit, h, lit ? pingColor : 0x2c3040);
                if (lit) rect(x, base + h - unit, 0, 2 * unit, unit, tint(pingColor, 1.25));
              }
              line([{ text: ping, color: pingColor }]);
            }
            if (SHOW_FPS && fps) line([{ text: `${fps} fps`, color: fpsColor }]);
          }
        });
      }
      projector.overlay(presenceModel, width, height);
    }
    const W = canvas.width,
      H = canvas.height,
      pixel = W / Math.max(1, canvas.clientWidth);
    if (tags.length && !menu) {
      const { unit } = hud;
      const m = model(({ rect: worldRect, text, handleText }) => {
        for (const t of tags) {
          const s = projector.project(t.x, t.y, t.z);
          if (!s || Math.abs(s.x) > 1.1 || Math.abs(s.y) > 1.1) continue;
          // Hidden behind walls and built objects, as a real tag would be.
          const dx = t.x - eye.x, dy = t.y - eye.y, dz = t.z - eye.z,
            d = Math.hypot(dx, dy, dz);
          if (wallDistance(eye, { x: dx / d, y: dy / d, z: dz / d }, d, solidObjects, worldTime) < d - 0.3) continue;
          const member = members.find((x) => x.seat === t.seat);
          const label = member?.handle ? `@${member.handle}` : member?.name || `guest ${t.seat + 1}`;
          const x = Math.round((s.x * W) / 2),
            y = Math.round((s.y * H) / 2) + 10 * unit;
          if (member?.handle) {
            handleText(label, x + unit, y - unit, 0.000002, unit, shadowInk(label));
            handleText(label, x, y, 0, unit, member.colors);
          } else {
            text(label, x + unit, y - unit, 0.000002, unit, INK);
            text(label, x, y, 0, unit, PAPER);
          }
        }
      });
      projector.overlay(m, W, H);
    }
    // Overlay space: origin at the centre, y up; text x is screen-right,
    // ordinary faces are mirrored, as in the presence layer.
    const flip = (worldRect) => (x, y, z, w, h, color) =>
      worldRect(-x - w, y, z, w, h, color);
    const shadowed = (text, line, x, y, unit, color) => {
      text(line, x + unit, y - unit, 0.000002, unit, INK);
      text(line, x, y, 0, unit, color);
    };
    if (radial) {
      const { unit } = hud;
      const key = JSON.stringify([radial, W, H]);
      if (key !== ringKey) {
        ringKey = key;
        const { r, cy } = ringGeometry(W, H);
        ringModel = model(({ rect: worldRect, text }) => {
          const rect = flip(worldRect);
          radialSlots(radial.items.length).forEach((slot, i) => {
            const label = radial.items[i],
              x = slot.x * r,
              y = cy + slot.y * r + 4 * unit,
              w = (label.length * 6 + 5) * unit;
            if (i === radial.selected) {
              rect(x - w / 2 + unit, y - 10 * unit, 0.000004, w, 12 * unit, INK);
              rect(x - w / 2, y - 9 * unit, 0.000003, w, 12 * unit, 0xffe066);
              text(label, x, y, 0, unit, INK);
            } else drawSpans({ rect, text }, phraseSpans(label), x, y, unit);
          });
          // The centre names what X (or a tap) does: build once there is a prompt.
          const centre = radial.ready
            ? hintSpans(input === 'touch' ? 'build' : '[X] build', INK)
            : [{ text: radial.title.toLowerCase(), color: INK }];
          const w = (centre.reduce((n, c) => n + (c.badge ? 10 : c.text.length * 6), 0) + 8) * unit;
          rect(-w / 2 + unit, cy - 6 * unit, 0.000005, w, 13 * unit, INK);
          rect(-w / 2, cy - 5 * unit, 0.000004, w, 13 * unit, radial.ready ? 0xffe066 : PAPER);
          drawSpans({ rect, text }, centre, 0, cy + 5 * unit, unit, { shadow: null });
        });
      }
      projector.overlay(ringModel, W, H);
    }
    if (touch?.stick || touch?.look?.holding) {
      // Pixel squares, like everything else: a ring for the anchor, a knob for the thumb.
      const { unit } = hud;
      const m = model(({ rect: worldRect }) => {
        const rect = flip(worldRect);
        const at = (x, y) => [x * pixel - W / 2, H / 2 - y * pixel];
        const frame = (cx, cy, size, thick, color, z) => {
          rect(cx - size / 2, cy + size / 2 - thick, z, size, thick, color);
          rect(cx - size / 2, cy - size / 2, z, size, thick, color);
          rect(cx - size / 2, cy - size / 2, z, thick, size, color);
          rect(cx + size / 2 - thick, cy - size / 2, z, thick, size, color);
        };
        if (touch.stick) {
          const radius = Math.max(36, Math.min(canvas.clientWidth, canvas.clientHeight) * 0.12) * pixel;
          const [ox, oy] = at(touch.stick.ox, touch.stick.oy),
            [kx, ky] = at(touch.stick.x, touch.stick.y);
          frame(ox + unit, oy - unit, radius * 2, 2 * unit, INK, 0.000003);
          frame(ox, oy, radius * 2, 2 * unit, PAPER, 0.000002);
          const k = radius * 0.6;
          rect(kx - k / 2 + unit, ky - k / 2 - unit, 0.000003, k, k, INK);
          rect(kx - k / 2, ky - k / 2, 0.000002, k, k, PAPER);
        }
        if (touch.look?.holding) {
          // The hold fills a bar under the thumb until the ring opens.
          const t = Math.min(1, (performance.now() - touch.look.at) / 450);
          const [x, y] = at(touch.look.x, touch.look.y);
          const w = 30 * unit;
          rect(x - w / 2, y - 24 * unit, 0.000003, w, 3 * unit, INK);
          rect(x - w / 2, y - 24 * unit, 0.000002, w * t, 3 * unit, 0xffff00);
        }
      });
      projector.overlay(m, W, H);
    }
    // The corner block: the prompt's own pink cursor block at the top left,
    // where the system label sits on every AC piece (SCREEN.md). A tap on it
    // goes back to the prompt.
    cornerRect = null;
    if (corner && !prompt?.open && !jobs.some((j) => j.owner === owner)) {
      const { unit } = hud;
      const safeX = grid ? 6 : hud.safeX, safeY = grid ? 6 : hud.safeY;
      const w = 6 * unit,
        h = 10 * unit;
      const left = -W / 2 + safeX,
        bottom = H / 2 - safeY - h;
      cornerRect = { x: safeX, y: safeY, w, h };
      const m = model(({ rect: worldRect }) => {
        const rect = flip(worldRect);
        // An outline like the curtain's caret (2026-11-03): its hue drifting
        // around the edge, each pixel shadowed.
        const ring = [];
        const cw = w / unit, ch = h / unit;
        for (let i = 0; i < cw; i++) ring.push([i, ch - 1]);
        for (let j = ch - 2; j >= 0; j--) ring.push([cw - 1, j]);
        for (let i = cw - 2; i >= 0; i--) ring.push([i, 0]);
        for (let j = 1; j < ch - 1; j++) ring.push([0, j]);
        ring.forEach(([i, j], k) => {
          const hue = (k / ring.length + now / 2400) % 1, a = hue * 6, f = a - Math.floor(a), q = 1 - f;
          const [r, g, b] = [[1, f, 0], [q, 1, 0], [0, 1, f], [0, q, 1], [f, 0, 1], [1, 0, q]][Math.floor(a) % 6];
          const col = (Math.round(40 + 215 * r) << 16) | (Math.round(40 + 215 * g) << 8) | Math.round(40 + 215 * b);
          rect(left + i * unit + unit, bottom + j * unit - unit, 0.000003, unit, unit, INK);
          rect(left + i * unit, bottom + j * unit, 0.000002, unit, unit, col);
        });
      });
      projector.overlay(m, W, H);
    }
    if (devLog.length) {
      // A live feed: each step types in a character at a time with a block
      // cursor, its code and commands as sublines beneath it; the stack eases
      // upward as lines arrive and older rows fade into the room.
      const { unit, safeX, safeY } = hud;
      const columns = Math.max(16, Math.floor((W * 0.45 - safeX) / (4 * unit)));
      const rows = [];
      for (const d of devLog) {
        rows.push({ d, spans: devSpans(d, THEME.text, true), at: d.at, sub: false });
        for (const line of d.detail || [])
          rows.push({
            d,
            spans: [{ text: line, color: line.startsWith('+') ? 0x8be28b : line.startsWith('-') ? 0xff8a8a : 0x7cc4ff }],
            at: d.at,
            sub: true,
          });
      }
      const LINE = 8 * unit,
        SHOWN = 9;
      const newest = devLog[devLog.length - 1].at;
      // Ease the whole stack up by one line over a quarter second per arrival.
      const ease = Math.min(1, (now - newest) / 250);
      const lift = (1 - (1 - ease) * (1 - ease)) * LINE;
      const visible = rows.slice(-SHOWN);
      const left = -W / 2 + safeX + 2;
      // Anchored at the top, under the corner block: lines start there,
      // newest lowest; once the stack is full it eases upward as each line arrives.
      const top = H / 2 - safeY - (cornerRect ? 13 * unit : 0);
      devModel = model(({ rect: worldRect, text, small }) => {
        const rect = flip(worldRect);
        // Characters revealed so far, across an entry and its sublines.
        const typed = new Map();
        visible.forEach((r, i) => {
          const age = (now - r.at) / 1000;
          const budget = typed.has(r.d) ? typed.get(r.d) : Math.floor(age * 140);
          let spans = clipSpans(r.spans, columns);
          const length = spans.reduce((n, x) => n + x.text.length, 0);
          const shown = Math.max(0, Math.min(length, budget));
          typed.set(r.d, budget - shown);
          if (shown < length) {
            let left2 = shown;
            spans = spans
              .map((x) => {
                const t = x.text.slice(0, Math.max(0, left2));
                left2 -= x.text.length;
                return { ...x, text: t };
              })
              .filter((x) => x.text);
          }
          if (!spans.length && shown === 0) return;
          // Fade with age and with distance from the newest row.
          const fromNewest = visible.length - 1 - i;
          const fade = Math.max(0.18, Math.min(1, 1 - age / 90) * (1 - fromNewest * 0.09));
          spans = spans.map((x) => ({ ...x, color: tint(x.color, fade) }));
          const full = rows.length >= SHOWN;
          const y = top - i * LINE + (full ? lift - LINE : 0);
          const x0 = left + (r.sub ? 8 * unit : 0);
          const w = smallSpanWidth(spans, unit);
          drawSpans({ rect, text, small }, spans, x0 + w / 2, y, unit, { tiny: true, shadow: null });
          if (shown < length && now % 300 < 200)
            rect(x0 + w + unit, y - 7 * unit, 0, 3 * unit, 7 * unit, THEME.verb);
        });
      });
      projector.overlay(devModel, W, H);
    }
    // Offline: the connection's log in a panel at the centre, so a restart
    // or a dropped socket reads as what it is, not an empty room (2026-11-03).
    if (offline && !menu) {
      const { unit } = hud;
      const lines = netLog.slice(-8);
      const key = JSON.stringify([lines, W, H]);
      if (key !== netKey) {
        netKey = key;
        netModel = model(({ rect: worldRect, text, small }) => {
          const rect = flip(worldRect);
          const cols = Math.min(60, Math.floor((W - 20 * unit) / (4 * unit)));
          const rows = [
            [{ text: 'dool is offline', color: 0xff6b6b }],
            [{ text: 'reconnecting; the room comes back on its own', color: THEME.dim }],
            ...lines.map((l) => [
              { text: `${l.when} `, color: THEME.dim },
              { text: l.text.slice(0, cols - 9), color: l.bad ? 0xff6b6b : l.good ? 0x3fb950 : THEME.text },
            ]),
          ];
          const ph = (rows.length + 1) * 9 * unit + 8 * unit, pw = Math.min(W - 8 * unit, (cols + 2) * 4 * unit);
          rect(-pw / 2, -ph / 2, 0.000004, pw, ph, LINE);
          rect(-pw / 2 + unit, -ph / 2 + unit, 0.000003, pw - 2 * unit, ph - 2 * unit, 0x15171e);
          rows.forEach((spans, i) =>
            drawSpans({ rect, text, small }, spans, -pw / 2 + 4 * unit + smallSpanWidth(spans, unit) / 2, ph / 2 - 6 * unit - i * 9 * unit, unit, { tiny: true, shadow: null }),
          );
        });
      }
      projector.overlay(netModel, W, H);
    }
    // A square reticle: four L-shaped corners on whole pixels, symmetric
    // about the centre, each with a one-pixel shadow. It opens with speed,
    // closes and colours on an object (green when yours), in pixel steps.
    // The reticle holds through the paper's drop and return (2026-11-03): the
    // prompt's crosshair fades with the paper, so this one must already be
    // there, solid, under it, and stay until the paper is whole again.
    // On the floor the cursor is laid on the aimed square in 3D (below);
    // facing a wall, an object or the sky it is this 2D reticle.
    const onFloor = floorAim && !focusObject && !lockedOn && !radial && !menu && !bare && orbit <= 0 && !cursor;
    if ((me?.alive || reticle) && !radial && !menu && (!bare || reticle) && !onFloor) {
      const { unit } = hud;
      const speed = me ? Math.min(1, Math.hypot(me.vx || 0, me.vz || 0) / 10) : 0;
      // At rest the reticle is the cyan of AC's own cursor (cursors/precise.svg).
      const color = lockedOn ? 0xffe066 : focusObject ? (focusObject.owner === owner ? 0x3fb950 : THEME.verb) : 0x00ffff;
      let half = (lockedOn ? 4 : focusObject ? 5 : 7 + Math.round(speed * 5)) * unit,
        arm = 4 * unit,
        t = unit;
      // Locked on: the corners surround the object on screen instead of
      // closing in on the centre. Its world box projects to a screen box,
      // padded a little and kept at least as big as the resting reticle.
      let box = null;
      if (lockedOn && focusObject) {
        let world = null;
        if (focusObject.wall) {
          const w = walls.find((x) => `wall:${x.code}` === focusObject.id);
          if (w) {
            const d = SCREEN - 0.03, top = 1.5 + 18;
            world = w.face === 'north'
              ? { xMin: -16, xMax: 16, yMin: 1.5, yMax: top, zMin: d, zMax: d }
              : { xMin: -d, xMax: -d, yMin: 1.5, yMax: top, zMin: -16, zMax: 16 };
          }
        } else if (focusObject.item) {
          const it = items.find((x) => x.id === focusObject.id);
          if (it) world = { xMin: it.x - 0.6, xMax: it.x + 0.6, yMin: it.y, yMax: it.y + 1.2, zMin: it.z - 0.6, zMax: it.z + 0.6 };
        } else {
          for (const b of objectBoxes([focusObject], worldTime)) {
            if (!world) world = { ...b };
            else {
              world.xMin = Math.min(world.xMin, b.xMin); world.xMax = Math.max(world.xMax, b.xMax);
              world.yMin = Math.min(world.yMin, b.yMin); world.yMax = Math.max(world.yMax, b.yMax);
              world.zMin = Math.min(world.zMin, b.zMin); world.zMax = Math.max(world.zMax, b.zMax);
            }
          }
        }
        if (world) {
          let l = Infinity, r = -Infinity, b = Infinity, u = -Infinity;
          for (const x of [world.xMin, world.xMax])
            for (const y of [world.yMin, world.yMax])
              for (const z of [world.zMin, world.zMax]) {
                const s = projector.project(x, y, z);
                if (!s) { l = Infinity; break; }
                l = Math.min(l, s.x); r = Math.max(r, s.x);
                b = Math.min(b, s.y); u = Math.max(u, s.y);
              }
          if (Number.isFinite(l) && Number.isFinite(r)) {
            const pad = 3 * unit;
            const clamp = (v, lim) => Math.max(-lim, Math.min(lim, v));
            const left = clamp(Math.round((l * W) / 2) - pad, W / 2 - 2 * unit),
              right = clamp(Math.round((r * W) / 2) + pad, W / 2 - 2 * unit),
              bottom = clamp(Math.round((b * H) / 2) - pad, H / 2 - 2 * unit),
              top = clamp(Math.round((u * H) / 2) + pad, H / 2 - 2 * unit);
            const min = 2 * half;
            box = {
              left: Math.min(left, -min / 2),
              right: Math.max(right, min / 2),
              bottom: Math.min(bottom, -min / 2),
              top: Math.max(top, min / 2),
            };
            // Arms grow with the box, up to a quarter of its shorter side.
            arm = Math.max(4 * unit, Math.min(12 * unit, Math.floor(Math.min(box.right - box.left, box.top - box.bottom) / 4 / unit) * unit));
          }
        }
      }
      // With a free mouse the reticle sits on the cursor (2026-11-03).
      const pxc = canvas.width / Math.max(1, canvas.clientWidth);
      // In flight the reticle rides the lens shift: it sits on the room's
      // centre as the camera carries that from the corner to the middle.
      const shifted = projector.shiftX || projector.shiftY;
      const cx0 = shifted ? Math.round((projector.shiftX * W) / 2) : reticleShift ? reticleShift.x : cursor && !box ? Math.round(cursor.x * pxc - W / 2) : 0,
        cy0 = shifted ? Math.round((projector.shiftY * H) / 2) : reticleShift ? reticleShift.y : cursor && !box ? Math.round(H / 2 - cursor.y * pxc) : 0;
      const m = model(({ rect: worldRect, text }) => {
        // rect(left, bottom, w, h): screen x right, y up, whole pixels.
        const rect0 = flip(worldRect);
        const rect = (x, y, z, w, h, color) => rect0(x + cx0, y + cy0, z, w, h, color);
        // The highlighted object's offer, under its bracket (or the reticle).
        if (objectHintSpans) {
          const cx = box ? (box.left + box.right) / 2 : 0, under = (box ? box.bottom : -half) - 4 * unit;
          drawSpans({ rect, text }, objectHintSpans, cx, under, unit, { shadow: INK });
        }
        const L = (x, y, w, h) => {
          rect(x + t, y - t, 0.000002, w, h, INK);
          rect(x, y, 0, w, h, color);
        };
        for (const sx of [-1, 1])
          for (const sy of [-1, 1]) {
            // The corner's outer pixel, then arms running inward.
            const x = box ? (sx < 0 ? box.left - t : box.right) : sx < 0 ? -half - t : half,
              y = box ? (sy < 0 ? box.bottom - t : box.top) : sy < 0 ? -half - t : half;
            L(sx < 0 ? x : x - arm + t, y, arm, t);
            L(x, sy < 0 ? y : y - arm + t, t, arm);
          }
        if (lockedOn) {
          // A small diamond instead of a dot.
          L(-t / 2, t, t, t);
          L(-t / 2 - t, 0, t, t);
          L(-t / 2 + t, 0, t, t);
          L(-t / 2, -t, t, t);
        } else L(-t, -t, 2 * t, 2 * t);
      });
      projector.overlay(m, W, H);
    }
    // Match the native KidLisp corner tag: prompt.ac keeps the same QR
    // matrix; modules and margins use buffer pixels, independent of HUD size.
    qrRect = null;
    qrURL = null;
    if (focusObject?.code && !radial && !menu && !bare) {
      const url = `https://prompt.ac/$${focusObject.code}`;
      qrURL = url;
      const cells = joinCode(url);
      if (cells) {
        const n = cells.length, margin = 4, qrOffsetY = 9;
        const width = n + 1, height = qrOffsetY + n + 2;
        const left = W - width - margin, top = H - height - margin;
        qrRect = { x: left, y: top, w: width, h: height };
        const key = `${url}:${W}:${H}`;
        if (qrModel?.key !== key) {
          const m = model(({ rect: worldRect, small }) => {
            const rect = flip(worldRect);
            // Coordinates share the 2D runtime's top-left pixel convention.
            const pixel = (x, y, w, h, color, z = 0.000001) =>
              rect(-W / 2 + left + x, H / 2 - top - y - h, z, w, h, color);
            pixel(0, qrOffsetY, n, n, 0xffffff, 0.000003);
            cells.forEach((row, y) => {
              for (let c = 0; c < row.length;) {
                if (!row[c]) { c++; continue; }
                const start = c++;
                while (c < row.length && row[c]) c++;
                pixel(start, qrOffsetY + y, c - start, 1, 0x000000);
              }
            });
            const label = `$${focusObject.code}`;
            // smallWidth omits the trailing advance; the runtime includes it.
            const textWidth = smallWidth(label) + 1, labelWidth = textWidth + 2;
            const labelX = n - labelWidth;
            // Keep the label clear of the first QR row.
            pixel(labelX, 0, labelWidth, qrOffsetY, 0x000000, 0);
            small(label, -W / 2 + left + labelX + 1 + smallWidth(label) / 2,
              H / 2 - top - 2, -0.000001, 1, 0xffffff);
            pixel(n, 1, 1, qrOffsetY + n, 0x808080);
            pixel(1, qrOffsetY + n, n, 1, 0x808080);
            // Native corner tags have a subtle white top/left highlight.
            for (let i = 0; i < n; i++) {
              pixel(i, qrOffsetY, 1, 1,
                cells[0][i] ? 0x404040 : 0xffffff, -0.000002);
              if (i) pixel(0, qrOffsetY + i, 1, 1,
                cells[i][0] ? 0x404040 : 0xffffff, -0.000002);
            }
          });
          qrModel = { key, mesh: m };
        }
        projector.overlay(qrModel.mesh, W, H);
      }
    }
    if (notices.length && !menu) {
      // Build results, briefly, above the panel.
      const { unit, safeY } = hud;
      const base = (panelTop ?? -H / 2 + safeY) + 6 * unit;
      const m = model(({ rect: worldRect, text, small }) =>
        notices.forEach((n, i) => {
          const spans = [{ text: n.ok ? 'ready ' : 'failed ', color: n.ok ? 0x3fb950 : 0xff6b6b }, { text: n.text.slice(0, 80), color: THEME.text }];
          drawSpans({ rect: flip(worldRect), text, small }, spans, 0, base + (notices.length - i) * 10 * unit, unit, { tiny: true });
        }),
      );
      projector.overlay(m, W, H);
    }
    if (face && me && !bare) {
      const { unit, safeX, safeY } = hud;
      const floor = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit);
      const listTop = floor + 4 * unit;
      const S = 24 * unit, x0 = -W / 2 + safeX + 2, y0 = listTop + 4 * unit;
      const base = me.y - CFG.eyeHeight,
        yawR = (me.yaw * Math.PI) / 180;
      const f2 = direction(me.yaw, 0);
      const head = { x: me.x, y: base + 1.5, z: me.z };
      const camAt = { x: head.x + f2.x * 1.3, y: head.y + 0.1, z: head.z + f2.z * 1.3 };
      const start = host.scene.triangleCount;
      facecam.camera(camAt, head, 1);
      // A narrow lens: head and shoulders fill the frame.
      facecam.sx *= 2.2;
      facecam.sy *= 2.2;
      const fd = (...a) => facecam.draw(...a);
      fd(arena);
      for (const w of walls) if (w.model) fd(w.model, 0, 0, 0);
      for (const o of objects)
        if (!o.item && o.geometry.format === OBJECT_FORMAT) fd(sourceFrame(o.geometry, worldTime).vertices, o.x, 0, o.z);
      const fs = fighters[Math.max(0, face.seat) % fighters.length];
      fd(fs.torso, me.x, base, me.z, yawR);
      fd(fs.head, me.x, base, me.z, yawR);
      fd(eyeFor(me, face.seat, now, face), me.x, base, me.z, yawR);
      // Into the frame: clip each triangle to the cam's square (so big ones
      // keep correct depth and nothing spills), scale it into the portrait,
      // and pull it in front of the world.
      const v = host.scene.vertices,
        sx = S / W,
        sy = S / H,
        cx = (2 * (W / 2 + x0) + S) / W - 1,
        cy = (2 * y0 + S) / H;
      const tris = v.slice(start * 18, host.scene.triangleCount * 18);
      host.scene.triangleCount = start;
      const edges = [[0, -1], [0, 1], [1, -1], [1, 1]];
      for (let k = 0; k < tris.length; k += 18) {
        let poly = [0, 6, 12].map((o) => Array.from(tris.subarray(k + o, k + o + 6)));
        for (const [axis, side] of edges) {
          const out = [];
          for (let i = 0; i < poly.length; i++) {
            const a = poly[i], b = poly[(i + 1) % poly.length];
            const ina = a[axis] * side <= 1, inb = b[axis] * side <= 1;
            if (ina) out.push(a);
            if (ina !== inb) {
              const t = (side - a[axis]) / (b[axis] - a[axis]);
              out.push(a.map((x, n) => x + (b[n] - x) * t));
            }
          }
          poly = out;
          if (poly.length < 3) break;
        }
        for (let i = 1; i + 1 < poly.length; i++) {
          if (host.scene.triangleCount >= host.scene.maxTriangles) break;
          let at = host.scene.triangleCount++ * 18;
          for (const p of [poly[0], poly[i], poly[i + 1]]) {
            v[at++] = cx + p[0] * sx;
            v[at++] = cy + p[1] * sy;
            v[at++] = p[2] * 0.001;
            v[at++] = p[3];
            v[at++] = p[4];
            v[at++] = p[5];
          }
        }
      }
    }
    if (face && !bare) {
      // A face cam, bottom left above your handle: an original pixel portrait
      // in the player's own look that reacts to what happens to them.
      const { unit, safeX, safeY } = hud;
      const floor = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit);
      const listTop = floor + 4 * unit;
      const S = 24 * unit,
        x0 = -W / 2 + safeX + 2,
        y0 = listTop + 4 * unit; // bottom edge of the frame
      const hurt = (now - face.hurtAt) / 1000,
        grin = (now - face.grinAt) / 1000;
      const blink = now % 4200 < 130;
      const hp = face.health ?? 100;
      const look = hurt < 0.6 ? face.hurtSide : Math.max(-1, Math.min(1, Math.round((face.turn || 0) * 1.4)));
      // Looking up or down lifts or lowers the face by a pixel (pitch < 0 is down).
      const tilt = Math.max(-1, Math.min(1, Math.round((face.pitch || 0) / 35)));
      const bob = Math.round(Math.sin(now / 700) * 0.6) * unit;
      const m = model(({ rect: worldRect, small }) => {
        const rect = flip(worldRect);
        const px = (x, y, w, h, c, z = 0) => rect(x0 + x * unit, y0 + y * unit, z, w * unit, h * unit, c);
        // Frame around the cam: slate, flashing red when hurt.
        const edge = hurt < 0.35 ? 0xff6b6b : 0x5a5f70;
        px(-1, -1, 26, 1, edge);
        px(-1, 24, 26, 1, edge);
        px(-1, 0, 1, 24, edge);
        px(24, 0, 1, 24, edge);
        // Braincells beside the cam: a pixel brain and the count, in purple.
        if (Number.isFinite(braincells)) {
          const BR = ['.##.##.', '#######', '###.###', '#######', '.##.##.'];
          BR.forEach((row, r) =>
            [...row].forEach((ch, c) => ch === '#' && px(27 + c + 1, 20 - r - 1, 1, 1, INK, 0.000002)),
          );
          BR.forEach((row, r) =>
            [...row].forEach((ch, c) => ch === '#' && px(27 + c, 20 - r, 1, 1, r === 2 && c === 3 ? 0x8e44ff : 0xb46bff)),
          );
          // Exact, with thousands separators, shadowed for contrast.
          const n = Math.round(braincells).toLocaleString('en-US');
          const nx = x0 + (36 + smallWidth(n) / 2) * unit, ny = y0 + 21 * unit;
          small(n, nx + unit, ny - unit, 0.000002, unit, INK);
          small(n, nx, ny, 0, unit, spending ? 0xf0d8ff : 0xd7a6ff);
          // Spending: purple pixels fly up and away from the count.
          if (spending && Math.random() < 0.7)
            cellBits.push({ x: 36 + Math.random() * n.length * 4, y: 22, vx: (Math.random() - 0.3) * 30, vy: 20 + Math.random() * 30, born: now });
          for (let k = cellBits.length - 1; k >= 0; k--) {
            const b = cellBits[k], t = (now - b.born) / 1000;
            if (t > 0.9) { cellBits.splice(k, 1); continue; }
            px(Math.round(b.x + b.vx * t), Math.round(b.y + b.vy * t - 20 * t * t), 1, 1, t < 0.3 ? 0xf0d8ff : t < 0.6 ? 0xb46bff : 0x8e44ff);
          }
        }
        // Under the braincells: the connection as four rising bars and its
        // ping, then the frame rate, each coloured by health.
        {
          const ping = (connection?.label || '').toLowerCase();
          const bars = connection?.bars || 0;
          const good = 0x3fb950, fair = 0xffe066, poor = 0xff6b6b;
          const pingColor = !ping ? 0x8b90a0 : bars >= 3 ? good : bars >= 2 ? fair : poor;
          const fpsColor = fps >= 55 ? good : fps >= 30 ? fair : poor;
          // small() takes the top of its 8-row glyph box; the count sits at 21.
          let top = Number.isFinite(braincells) ? 10 : 21;
          if (ping) {
            const base = top - 8;
            for (let i = 0; i < 4; i++) {
              const h = 2 + i * 2, lit = i < bars;
              px(27 + i * 3 + 1, base - 1, 2, h, INK, 0.000002);
              px(27 + i * 3, base, 2, h, lit ? pingColor : 0x2c3040, 0.000001);
              if (lit) px(27 + i * 3, base + h - 1, 2, 1, tint(pingColor, 1.25));
            }
            // The glyphs' foot on the bars' foot (small() takes the top of an 8-row box).
            const tx = x0 + (40 + smallWidth(ping) / 2) * unit, ty = y0 + (base + 7) * unit;
            small(ping, tx + unit, ty - unit, 0.000002, unit, INK);
            small(ping, tx, ty, 0, unit, pingColor);
            top -= 9;
          }
          if (SHOW_FPS && fps) {
            const f = `${fps} fps`;
            small(f, x0 + (27 + smallWidth(f) / 2) * unit, y0 + top * unit, 0, unit, fpsColor);
          }
        }
        // The health bar under it, in health colours.
        const hpc = hp > 60 ? 0x3fb950 : hp > 30 ? 0xffe066 : 0xff6b6b;
        px(0, -3, 24, 2, 0x2c3040, 0.000001);
        px(0, -3, Math.max(0, Math.round((24 * hp) / 100)), 2, hpc);
        if (me) return;
        px(0, 0, 24, 24, 0x15171e, 0.000003);
        // Shoulders in the seat colour.
        px(3, 0, 18, 5, face.seat >= 0 ? seatColor(face.seat) : 0xc0c0c0, 0.000002);
        // Head: the player's white block with a shaded side.
        const hx = 6, hy = 5 + bob / unit;
        px(hx, hy, 12, 13, 0xf2f2f2, 0.000002);
        px(hx + 10, hy, 2, 13, 0xc0c0c0, 0.000001);
        if (!face.alive) {
          // X eyes.
          for (const ex of [hx + 2, hx + 7])
            for (let k = 0; k < 3; k++) {
              px(ex + k, hy + 9 - k, 1, 1, INK);
              px(ex + 2 - k, hy + 9 - k, 1, 1, INK);
            }
          px(hx + 4, hy + 3, 4, 1, INK);
          return;
        }
        // Bruises as health drops.
        if (hp < 60) px(hx + 1, hy + 4, 2, 2, 0x8a6aa8, 0.000001);
        if (hp < 35) px(hx + 8, hy + 10, 2, 2, 0x8a6aa8, 0.000001);
        if (hp < 20) px(hx + 5, hy + 11, 1, 2, 0xb33a3a, 0.000001);
        // Eyes: squint when hurt, closed on a blink, else looking left/right.
        const ex = [hx + 2 + look, hx + 7 + look], ey = hy + tilt;
        for (const x of ex) {
          if (hurt < 0.6) px(x, ey + 8, 3, 1, INK);
          else if (blink) px(x, ey + 8, 3, 1, INK);
          else px(x, ey + 7, 2, 3, INK);
        }
        // Mouth: grin, grimace, frown when low, else a line.
        if (grin < 1.2) {
          px(hx + 3, hy + 3, 6, 1, INK);
          px(hx + 2, hy + 4, 1, 1, INK);
          px(hx + 9, hy + 4, 1, 1, INK);
        } else if (hurt < 0.6) {
          px(hx + 3, hy + 3, 6, 2, INK);
          px(hx + 4, hy + 3, 4, 1, 0xf2f2f2);
        } else if (hp < 35) {
          px(hx + 4, hy + 4, 4, 1, INK);
          px(hx + 3, hy + 3, 1, 1, INK);
          px(hx + 8, hy + 3, 1, 1, INK);
        } else px(hx + 4, hy + 3, 4, 1, INK);
        // A health bar under the portrait, in health colours.
        const hc = hp > 60 ? 0x3fb950 : hp > 30 ? 0xffe066 : 0xff6b6b;
        px(0, -3, 24, 2, 0x2c3040, 0.000001);
        px(0, -3, Math.max(0, Math.round((24 * hp) / 100)), 2, hc);
      });
      projector.overlay(m, W, H);
    }
    // ❄️ Snow falling into the open room (2026-11-03): small camera-facing
    // flakes in a volume over and around the room, depth-tested against the
    // walls, drifting as they fall, nearer ones a little larger.
    if (SNOW) {
      if (!snow) {
        snow = [];
        for (let i = 0; i < 260; i++)
          snow.push({ x: (Math.random() - 0.5) * 30, y: Math.random() * 14, z: (Math.random() - 0.5) * 30, v: 0.5 + Math.random() * 0.7, p: Math.random() * 6.3, s: 0.025 + Math.random() * 0.035 });
      }
      const dt = Math.min(0.1, (now - (snowAt || now)) / 1000);
      snowAt = now;
      const r = projector.right, u = projector.up;
      const m = model(({ quad }) => {
        for (const f of snow) {
          f.y -= f.v * dt;
          f.x += Math.sin(now / 1700 + f.p) * 0.25 * dt;
          f.z += Math.cos(now / 2100 + f.p) * 0.2 * dt;
          if (f.y < 0.02) {
            f.y = 13 + Math.random();
            f.x = (Math.random() - 0.5) * 30;
            f.z = (Math.random() - 0.5) * 30;
          }
          const s = f.s;
          const c = (sx, sy) => [f.x + (r[0] * sx + u[0] * sy) * s, f.y + (r[1] * sx + u[1] * sy) * s, f.z + (r[2] * sx + u[2] * sy) * s];
          quad(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1), 0xe6eeff);
        }
      });
      draw(m, 0, 0, 0);
    }
    // Painted floor squares, rebuilt only when they change.
    const tilesKey = JSON.stringify(tiles);
    if (tilesKey !== tileCacheKey) {
      tileCacheKey = tilesKey;
      tileModel = model(({ quad }) => {
        for (const [key, c] of Object.entries(tiles)) {
          const [i, j] = key.split(',').map(Number);
          const x = -18 + i * 2, z = -18 + j * 2, col = TILE_COLORS[(c - 1) % TILE_COLORS.length];
          quad([x + 0.06, 0.005, z + 0.06], [x + 1.94, 0.005, z + 0.06], [x + 1.94, 0.005, z + 1.94], [x + 0.06, 0.005, z + 1.94], col);
        }
      });
    }
    if (tileModel) draw(tileModel, 0, 0, 0);
    if (floorAim && !menu && !radial && !bare && orbit <= 0) {
      // The cursor projected onto the aimed square, drawn in screen space
      // (2026-11-03): the square's four corners go through the camera and
      // the reticle's L corners and centre dot are drawn between them as
      // whole-pixel HUD lines with their shadow, not as quads on the floor.
      const x = -18 + floorAim.i * 2, z = -18 + floorAim.j * 2, y = 0.02, k = 0.275;
      const P = [[x, z], [x + 2, z], [x + 2, z + 2], [x, z + 2]].map(([px, pz]) => projector.project(px, y, pz));
      const mid = projector.project(x + 1, y, z + 1);
      if (P.every(Boolean) && mid) {
        const S = P.map((p) => [Math.round((p.x * W) / 2), Math.round((p.y * H) / 2)]);
        const C = [Math.round((mid.x * W) / 2), Math.round((mid.y * H) / 2)];
        const { unit } = hud;
        const m = model(({ rect: worldRect }) => {
          const rect = flip(worldRect);
          const dot = (px, py, color, z0) => rect(px, py, z0, unit, unit, color);
          const line = (a, b, color, z0, off) => {
            const steps = Math.max(Math.abs(b[0] - a[0]), Math.abs(b[1] - a[1])) / unit;
            for (let i = 0; i <= steps; i++) {
              const t = steps ? i / steps : 0;
              dot(Math.round((a[0] + (b[0] - a[0]) * t) / unit) * unit + off, Math.round((a[1] + (b[1] - a[1]) * t) / unit) * unit - off, color, z0);
            }
          };
          for (const [color, z0, off] of [[INK, 0.000002, unit], [0x00ffff, 0, 0]]) {
            S.forEach((p, i) => {
              const nx = S[(i + 1) % 4], pv = S[(i + 3) % 4];
              line(p, [p[0] + (nx[0] - p[0]) * k, p[1] + (nx[1] - p[1]) * k], color, z0, off);
              line(p, [p[0] + (pv[0] - p[0]) * k, p[1] + (pv[1] - p[1]) * k], color, z0, off);
            });
            rect(C[0] - unit + off, C[1] - unit - off, z0, 2 * unit, 2 * unit, color);
          }
        });
        projector.overlay(m, W, H);
      }
    }
    // Chalk: a cached model per stroke, rebuilt only while it grows; your
    // own newest points draw at once, before the server echoes them.
    // Chalk as a rounded tube (2026-11-03): a six-sided prism per segment,
    // each side shaded by how it faces the light, every joint overlapped a
    // radius each way so corners read round. Points closer than a few
    // centimetres are skipped, so a long stroke stays a few hundred quads.
    const ribbon = (points, ink) =>
      model(({ quad }) => {
        const r = 0.045, N = 6;
        const kept = [];
        for (const p of points) {
          const l = kept[kept.length - 1];
          if (!l || Math.hypot(p.x - l.x, p.y - l.y, p.z - l.z) > 0.035) kept.push(p);
        }
        const last = points[points.length - 1];
        if (kept.length && kept[kept.length - 1] !== last && Math.hypot(last.x - kept[kept.length - 1].x, last.y - kept[kept.length - 1].y, last.z - kept[kept.length - 1].z) > 0.005) kept.push(last);
        if (kept.length < 2) return;
        for (let k = 1; k < kept.length; k++) {
          const a = kept[k - 1], b = kept[k];
          let tx = b.x - a.x, ty = b.y - a.y, tz = b.z - a.z;
          const len = Math.hypot(tx, ty, tz) || 1;
          tx /= len; ty /= len; tz /= len;
          // Two perpendiculars to the segment.
          const hx = Math.abs(ty) < 0.9 ? 0 : 1, hy = Math.abs(ty) < 0.9 ? 1 : 0;
          let n1x = ty * 0 - tz * hy, n1y = tz * hx - tx * 0, n1z = tx * hy - ty * hx;
          const n1l = Math.hypot(n1x, n1y, n1z) || 1;
          n1x /= n1l; n1y /= n1l; n1z /= n1l;
          const n2x = ty * n1z - tz * n1y, n2y = tz * n1x - tx * n1z, n2z = tx * n1y - ty * n1x;
          // The ends reach a radius past the points so neighbours overlap.
          const ax = a.x - tx * r, ay = a.y - ty * r, az = a.z - tz * r;
          const bx = b.x + tx * r, by = b.y + ty * r, bz = b.z + tz * r;
          const ring = (cx, cy, cz) => {
            const out = [];
            for (let i = 0; i < N; i++) {
              const th = (i / N) * Math.PI * 2, c = Math.cos(th), s = Math.sin(th);
              out.push([cx + r * (c * n1x + s * n2x), cy + r * (c * n1y + s * n2y), cz + r * (c * n1z + s * n2z)]);
            }
            return out;
          };
          const A = ring(ax, ay, az), B = ring(bx, by, bz);
          for (let i = 0; i < N; i++) {
            const j = (i + 1) % N, th = ((i + 0.5) / N) * Math.PI * 2;
            const ny = Math.cos(th) * n1y + Math.sin(th) * n2y; // the side's upward facing
            quad(A[i], B[i], B[j], A[j], tint(ink, 0.72 + 0.28 * (ny * 0.5 + 0.5)));
          }
        }
      });
    const liveChalk = new Set();
    chalk.forEach((strokes, seatIndex) => {
      const ink = tint(seatColor(seatIndex), 1.6);
      (strokes || []).forEach((points, k) => {
        if (!points || points.length < 2) return;
        const key = `${seatIndex}:${k}`;
        liveChalk.add(key);
        let c = chalkModels.get(key);
        const first = points[0];
        if (!c || c.length !== points.length || c.x !== first.x || c.z !== first.z) {
          c = { length: points.length, x: first.x, z: first.z, model: ribbon(points, ink) };
          chalkModels.set(key, c);
        }
        draw(c.model, 0, 0, 0);
      });
    });
    for (const key of chalkModels.keys()) if (!liveChalk.has(key)) chalkModels.delete(key);
    if (chalkLocal.length >= 1 && seat >= 0) {
      const mine = chalk[seat]?.at(-1)?.at(-1);
      const tail = mine ? [mine, ...chalkLocal] : chalkLocal;
      if (tail.length >= 2) draw(ribbon(tail, seat === 0 ? 0xffd9cf : 0xd6ecff), 0, 0, 0);
    }
    // The KidLisp cube: six programs on six faces, turning slowly above the
    // middle of the room. Rebuilt ten times a second; turned every frame.
    if (cube.some(Boolean)) {
      if (!cubeModel || now - cubeAt > 100) {
        cubeAt = now;
        const S = 2; // half size in metres
        const faces = [
          (u, v) => [u, v, S], (u, v) => [-u, v, -S], (u, v) => [S, v, -u],
          (u, v) => [-S, v, u], (u, v) => [u, S, -v], (u, v) => [u, -S, v],
        ];
        cubeModel = model(({ quad }) => {
          cube.forEach((face, f) => {
            if (!face) return;
            const px = face.step(), n = face.width, cell = (2 * S) / n, at = faces[f];
            for (let j = 0; j < n; j++)
              for (let i = 0; i < n; ) {
                const k = (j * n + i) * 4;
                let run = 1;
                while (i + run < n) {
                  const q = (j * n + i + run) * 4;
                  if (px[q] !== px[k] || px[q + 1] !== px[k + 1] || px[q + 2] !== px[k + 2]) break;
                  run++;
                }
                const u0 = S - i * cell, u1 = S - (i + run) * cell, v1 = S - j * cell, v0 = v1 - cell;
                quad(at(u0, v0), at(u1, v0), at(u1, v1), at(u0, v1), (px[k] << 16) | (px[k + 1] << 8) | px[k + 2]);
                i += run;
              }
          });
        });
      }
      draw(cubeModel, 0, 7.5, 0, now / 4000, Math.sin(now / 5000) * 0.4);
    }
    // KidLisp walls: step each program about 20 times a second and lay its
    // pixels on the wall as a grid of cells, unmirrored from inside.
    for (const w of walls) {
      const remixJob = jobs.find((j) => j.kind === 'kidlisp' && j.face === w.face);
      const remix = !!remixJob;
      if (!w.model || now - (w.at || 0) > 50 || w.wasRemix !== remix) {
        w.at = now;
        w.wasRemix = remix;
        const px = w.step();
        const cols = w.width, rows = w.height;
        const span = w.face === 'north' ? 32 : 32, tall = (span * rows) / cols;
        const cell = span / cols, base = 1.5;
        w.model = model(({ quad }) => {
          for (let j = 0; j < rows; j++)
            for (let i = 0; i < cols; ) {
              const k = (j * cols + i) * 4;
              const c = (px[k] << 16) | (px[k + 1] << 8) | px[k + 2];
              // Merge runs of one colour into one quad.
              let n = 1;
              while (i + n < cols) {
                const q = (j * cols + i + n) * 4;
                if (px[q] !== px[k] || px[q + 1] !== px[k + 1] || px[q + 2] !== px[k + 2]) break;
                n++;
              }
              const u0 = span / 2 - i * cell, u1 = span / 2 - (i + n) * cell;
              const y1 = base + tall - j * cell, y0 = y1 - cell;
              const d = SCREEN - 0.03;
              // While Aesel remixes it, the screen splits in two in space:
              // the program keeps the left half, the new source the right.
              const sq = remix ? 0.5 : 1, shift = remix ? span / 4 : 0;
              const a = u0 * sq + shift, b = u1 * sq + shift;
              if (w.face === 'north') quad([a, y0, d], [b, y0, d], [b, y1, d], [a, y1, d], c);
              else quad([-d, y0, -a], [-d, y0, -b], [-d, y1, -b], [-d, y1, -a], c);
              i += n;
            }
        });
      }
      draw(w.model, 0, 0, 0);
      const tall = (32 * w.height) / w.width;
      if (remixJob) {
        // The new source streams onto the right half of the screen itself.
        const lines = (remixJob.code || '...').split('\n').filter((l) => l.trim()).slice(-14);
        const key = JSON.stringify(lines);
        if (w.remixKey !== key) {
          w.remixKey = key;
          w.remixModel = model(({ rect: worldRect, text }) => {
            const rect = (x, y, z, ww, hh, c) => worldRect(-x - ww, y, z, ww, hh, c);
            rect(-8, -tall, 0.01, 16, tall, 0x0b0c10);
            lines.forEach((l, k) => drawSpans({ rect, text }, codeSpans(l.slice(0, 40)), 0, -0.6 - k * 1.1, 0.06, { shadow: null, step: 0.002 }));
          });
        }
        const d = SCREEN - 0.05, top = 1.5 + tall;
        if (w.face === 'north') draw(w.remixModel, -8, top, d, Math.PI);
        else draw(w.remixModel, -d, top, -8, -Math.PI / 2);
      }
      // A frame and two legs, so each screen stands in the open.
      if (!w.frame || w.frameTall !== tall) {
        w.frameTall = tall;
        const d = SCREEN, span = 32, c = 0x2a2d3a, side = 0x22252f;
        w.frame = model(({ box }) => {
          if (w.face === 'north') {
            box(0, 1.5 + tall / 2, d + 0.25, span + 0.6, tall + 0.6, 0.4, c, side, c);
            box(-span / 2 + 1, 0.75, d + 0.25, 0.4, 1.5, 0.4, c, side, c);
            box(span / 2 - 1, 0.75, d + 0.25, 0.4, 1.5, 0.4, c, side, c);
          } else {
            box(-d - 0.25, 1.5 + tall / 2, 0, 0.4, tall + 0.6, span + 0.6, c, side, c);
            box(-d - 0.25, 0.75, -span / 2 + 1, 0.4, 1.5, 0.4, c, side, c);
            box(-d - 0.25, 0.75, span / 2 - 1, 0.4, 1.5, 0.4, c, side, c);
          }
        });
      }
      draw(w.frame, 0, 0, 0);
    }
    // Standing KidLisp screens: each $code prompt stands its piece on the
    // field, facing whoever asked, stepping about 20 times a second.
    for (const s of screens) {
      const w = screenPrograms.get(s.code);
      if (!w) continue;
      const span = 4, cols = w.width, rows = w.height, cell = span / cols, tall = (span * rows) / cols, base = 0.4;
      if (!s.model || now - (s.at || 0) > 50) {
        s.at = now;
        const px = w.step();
        s.model = model(({ quad }) => {
          for (let j = 0; j < rows; j++)
            for (let i = 0; i < cols; ) {
              const k = (j * cols + i) * 4;
              const c = (px[k] << 16) | (px[k + 1] << 8) | px[k + 2];
              let n = 1;
              while (i + n < cols) {
                const q = (j * cols + i + n) * 4;
                if (px[q] !== px[k] || px[q + 1] !== px[k + 1] || px[q + 2] !== px[k + 2]) break;
                n++;
              }
              const u0 = span / 2 - i * cell, u1 = span / 2 - (i + n) * cell;
              const y1 = base + tall - j * cell, y0 = y1 - cell;
              quad([u0, y0, 0], [u1, y0, 0], [u1, y1, 0], [u0, y1, 0], c);
              i += n;
            }
        });
      }
      let frame = screenFrames.get(s.id);
      if (!frame) {
        const c = 0x2a2d3a, side = 0x22252f;
        frame = model(({ box }) => {
          box(0, base + tall / 2, 0.14, span + 0.3, tall + 0.3, 0.16, c, side, c);
          box(-span / 2 + 0.4, base / 2, 0.14, 0.16, base, 0.16, c, side, c);
          box(span / 2 - 0.4, base / 2, 0.14, 0.16, base, 0.16, c, side, c);
        });
        screenFrames.set(s.id, frame);
      }
      const yaw = ((s.yaw || 0) * Math.PI) / 180;
      draw(s.model, s.x, 0, s.z, yaw);
      draw(frame, s.x, 0, s.z, yaw);
    }
    if (menu) {
      // A modal: the scene dimmed behind it, a panel with a title and a
      // left-aligned list, the chosen row on yellow. Drawn nearest of all.
      const { unit } = hud;
      const rows = menu.items.slice(0, 12);
      const w = Math.min(W - 40 * unit, Math.max(130, ...rows.map((r) => r.label.length * 6 + 28)) * unit);
      const lineH = 12 * unit,
        h = (rows.length + 2) * lineH + 4 * unit;
      const z = -0.000009;
      const near = z - 0.000004; // text clearly in front of the row highlight
      const m = model(({ rect: worldRect, text }) => {
        const rect = flip(worldRect);
        rect(-W / 2, -H / 2, z + 0.000004, W, H, 0x0b0c10);
        rect(-w / 2 - unit, -h / 2 - unit, z + 0.000003, w + 2 * unit, h + 2 * unit, LINE);
        rect(-w / 2, -h / 2, z + 0.000002, w, h, 0x15171e);
        const left = -w / 2 + 8 * unit;
        const title = [{ text: menu.title, color: THEME.verb }];
        drawSpans({ rect, text }, title, left + spanWidthOf(title, unit) / 2, h / 2 - 5 * unit, unit, { shadow: null, z: near });
        rows.forEach((r, i) => {
          const y = h / 2 - (i + 2) * lineH;
          const on = i === menu.selected;
          if (on) rect(-w / 2 + 4 * unit, y - 9 * unit, z + 0.000001, w - 8 * unit, 11 * unit, 0xffe066);
          const spans = r.label.startsWith('$')
            ? [{ text: r.label.split(' ')[0], color: on ? INK : THEME.verb }, { text: ' ' + r.label.split(' ').slice(1).join(' '), color: on ? INK : THEME.text }]
            : [{ text: r.label, color: on ? INK : THEME.text }];
          drawSpans({ rect, text }, spans, left + spanWidthOf(spans, unit) / 2, y, unit, { shadow: null, z: near });
        });
      });
      projector.overlay(m, W, H);
    }
    host.present({ clear: sky });
    // Local inspection only; no telemetry, labels or DOM overlays.
    canvas.doolStats = {
      backend,
      bare,
      tool: 'hands',
      role: spectator ? 'spectator' : 'player',
      camera: camera || null,
      controller,
      members,
      connection,
      joinURL,
      hud,
      objects: objects.map((o) => ({
        id: o.id,
        owner: o.owner,
        version: o.version,
        name: o.geometry.name,
      })),
      promptOpen: !!prompt?.open,
      radial,
      input,
      devLog: devLog.map((d) => d.text),
      promptText: prompt?.text || '',
      promptMessage: prompt?.message || '',
      builder,
      owner,
      jobs: jobs.map((j) => ({
        id: j.id,
        phase: j.phase,
        parts: j.preview?.boxes.length || 0,
        code: j.code || '',
      })),
      animated: objects
        .filter((o) => o.geometry.motion || o.geometry.animated)
        .map((o) => {
          const p = poseObject(o, worldTime);
          return { id: o.id, x: p.x, z: p.z, boxes: p.geometry.boxes };
        }),
      triangles: host.scene.triangleCount,
      drawCalls: 1,
      cpuMs: performance.now() - start,
    };
  }
  return {
    canvas,
    render,
    effect,
    // The host's pixel grid: { pixel, left, top } (CSS pixels per buffer
    // pixel, and where the prompt's canvas starts); null to stand alone.
    showFps(on) {
      SHOW_FPS = !!on;
    },
    grid(g) {
      const next = g && g.pixel > 0 ? { pixel: g.pixel, left: g.left || 0, top: g.top || 0 } : null;
      if (JSON.stringify(next) === JSON.stringify(grid)) return;
      grid = next;
      if (!grid) canvas.style.cssText = '';
      resize();
    },
    // Is this point (CSS pixels on the canvas) on the corner block?
    get qrURL() {
      return qrURL;
    },
    // The camera's eye and the unit ray through a CSS point on the canvas,
    // so a free mouse aims where it points instead of at the view's centre.
    aimOrigin() {
      return lastEye;
    },
    // The camera's centre ray: exactly the crosshair's normal, bob, nod and
    // third-person offset included (2026-11-03).
    aimForward() {
      const f = projector.forward;
      return f && lastEye ? { x: f[0], y: f[1], z: f[2] } : null;
    },
    aimRay(cssX, cssY) {
      if (!projector.forward || !lastEye) return null;
      const pxr = canvas.width / Math.max(1, canvas.clientWidth);
      const nx = (cssX * pxr - canvas.width / 2) / (canvas.width / 2), ny = (canvas.height / 2 - cssY * pxr) / (canvas.height / 2);
      const [f, r, u] = [projector.forward, projector.right, projector.up];
      const kx = nx / projector.sx, ky = ny / projector.sy;
      let x = f[0] + r[0] * kx + u[0] * ky, y = f[1] + r[1] * kx + u[1] * ky, z = f[2] + r[2] * kx + u[2] * ky;
      const l = Math.hypot(x, y, z) || 1;
      return { x: x / l, y: y / l, z: z / l };
    },
    qrHit(cssX, cssY) {
      if (!qrRect) return false;
      const px = canvas.width / Math.max(1, canvas.clientWidth);
      const x = cssX * px, y = cssY * px;
      return x >= qrRect.x && x <= qrRect.x + qrRect.w && y >= qrRect.y && y <= qrRect.y + qrRect.h;
    },
    cornerHit(cssX, cssY) {
      if (!cornerRect) return false;
      const px = canvas.width / Math.max(1, canvas.clientWidth);
      const x = cssX * px,
        y = cssY * px,
        slack = 6 * px;
      return x >= 0 && y >= 0 && x <= cornerRect.x + cornerRect.w + slack && y <= cornerRect.y + cornerRect.h + slack;
    },
    destroy() {
      observer.disconnect();
      host.destroy();
    },
  };
}
