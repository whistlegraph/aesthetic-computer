import { poolFloor, poolView } from './pool-view.mjs';
import { STAIR, DEPTH, POOL, treads, cellarWalls, ceiling, berths } from './basement.mjs';
import { createFrameChart } from './ac/lib/frame-chart.mjs';
import { LensScene3D, lensWide, TEXTURE_SIZE, TEXTURE_LAYERS } from './lens-host.mjs';
import { createTextures } from './textures.mjs';
import { loaders as textureLoaders } from './texture-loaders.mjs';
import { BOP, DRONE, ducked } from './bop.mjs';
import { groundsOf, promenade, GROUNDS, SPAN } from './park.mjs';
import { CAR, CAR_HEIGHT, CAR_WALL, CAR_Z, carSolids, carScreenSolids, CAR_SCREEN_Z, STOREY, ATRIUM, SLAB, LEG, storeyOf, storeyAt, heightOf, LOBBY_FRONT, DOOR, DOOR_HEIGHT, PLANTERS, PLANTER, roofSolids, standsOn } from './elevator.mjs';
import { OBJECT_PROXY_DISTANCE, SCREEN_SPAN, screenDetail, screenSeen, carryPixels } from './screen-lod.mjs';
import { model, INK, PAPER, rgb, tint, audit, boxesOf } from './paint.mjs';
import { shadowVolume, groundShadow, VOLUME_TRIANGLES, SLAB_TRIANGLES, reach, shadeFor } from './shadow.mjs';
import { describeCrossing } from './solids.mjs';
import { phase, stepOf, skyAt, skyColor, stars as fixedStars, STEPS, blend as blendInk } from './sky.mjs';
import { decorFor, COLS as DECOR_COLS, ROWS as DECOR_ROWS } from './bike-decor.mjs';
import { BIKE } from './bike.mjs';
// 🧱 `localStorage["rax:solids"] = "on"` audits every model as it is built:
// solids that share volume are named once in the console, and the ones that
// would flicker are drawn magenta.
try {
  if (globalThis.localStorage?.getItem('rax:solids') === 'on') {
    const said = new Set();
    audit.on = true;
    audit.report = (found) => {
      for (const crossing of found) {
        const line = describeCrossing(crossing);
        if (said.has(line) || said.size > 200) continue;
        said.add(line);
        console.warn('🧱', line);
      }
    };
  }
} catch {}
import { poseObject, objectBoxes, distantBoxes } from './objects.mjs';
import { OBJECT_FORMAT, sourceFrame, sourceTree, sourceHair } from './object-language.mjs';
import { BALL, DISPENSER } from './ball.mjs';
import { airAt, sun, heading } from './air.mjs';
import { Projector, screen } from './project.mjs';
import { PLANET, FAR, landTiles, viewCurvature, bend } from './planet.mjs';
import { joinCode, hudLayout, createPack, fitHudScreen } from './hud.mjs';
import { pairLayout, pairModel } from './pair-view.mjs';
import { glide } from './dock.mjs';
import { cursorOver, cursorCells, INKS as cursorInks } from './ac/lib/cursor-shape.mjs';

// The HUD cannot sample the world behind it. Draw the shared cursor's solid
// strokes there; interactive panels blend the full cursor into their pixels.
const reticleStrokes = cursorCells().filter(([, , ink]) => ink !== cursorInks.glow);

// What each kind of input does, one line per hand, as Oskiewar lists its
// controls: the same world plays from a keyboard alone, a pad or a touchscreen.
// A key is [label, code]; it lights while held. Plain strings are the verbs.
export const LEGEND = {
  // Lines stay under thirty columns so a phone or a narrow window shows them whole.
  // Keys and pad list the same actions, row for row (2026-11-08).
  keys: [
    [['w', 'KeyW'], ['a', 'KeyA'], ['s', 'KeyS'], ['d', 'KeyD'], ' move / ', ['←', 'ArrowLeft'], ['→', 'ArrowRight'], ['↑', 'ArrowUp'], ['↓', 'ArrowDown'], ' look'],
    [['space', 'Space'], ' jump / ', ['v', 'KeyV'], ' camera / ', ['p', 'KeyP'], ' drone'],
    [['f', 'KeyF'], ' focus or use / ', ['q', 'KeyQ'], ' drop'],
    [['x', 'KeyX'], ' paint / ', ['enter', 'Enter'], ' build'],
    [['z', 'KeyZ'], ' lock / ', ['del', ['Delete', 'Backspace']], ' remove / ', ['b', 'KeyB'], ' bike'],
    [['$', 'Digit4'], ' menu / ', ['t', 'KeyT'], ' chat / ', ['tab', 'Tab'], ' prompt'],
  ],
  pad: [
    '[LS] move / [RS] look',
    '[A] jump / [LT] zoom',
    '[Y] camera / [L3] distance, held + [RS]',
    'hold [Y] drone',
    '[RT] use / [X] drop',
    '[X] paint / hold [LB] build',
    '[RB] lock or go in / [B] remove',
    '[START] prompt / [D>] $ menu / [DV] density',
    '[D<] bike: [A] up / hold [B] down',
  ],
  touch: [],
};
// 🚁 A drone's controls (looking on), in the same shape.
const DRONE_LEGEND = {
  keys: [
    [['w', 'KeyW'], ['a', 'KeyA'], ['s', 'KeyS'], ['d', 'KeyD'], ' move'],
    [['q', 'KeyQ'], ['r', 'KeyR'], ' roll / ', ['e', 'KeyE'], ' fast / ', ['v', 'KeyV'], ' drone'],
    [['f', 'KeyF'], ' focus, full screen / ', ['b', 'KeyB'], ' warp'],
  ],
  pad: [
    '[LS] move / [RS] look / [LB][RB] roll',
    '[LT] zoom / [A] fast / [Y] drone',
    '[X] lock on / [B] warp / [BACK] full screen',
  ],
  touch: [],
};
// Each action has its own colour, shared by its words and its key caps, so
// the eye finds a control by colour (2026-11-08). Separators stay dim.
const ACTION_COLORS = {
  move: 0x6bb6ff, turn: 0x8fd3ff, look: 0x8fd3ff, jump: 0x3fb950, jumps: 0x3fb950,
  camera: 0xb46bff, build: 0xffe066, builds: 0xffe066, use: 0xff9f43, drop: 0xff6b6b,
  cursor: 0x00ffff, paint: 0xff7ac8, chalk: 0xe8e8e8, rooms: 0x2ec4b6, 'lock on': 0x00ffff, lock: 0x00ffff, remove: 0xff6b6b, cancel: 0xff6b6b, prompt: 0xb0b0ff, '$ list': 0xffd28a, list: 0xffd28a, chat: 0x9be7a0,
  change: 0xffe066, next: 0x00ffff, unlock: 0x00ffff,
  up: 0x3fb950, down: 0xff9f43, roll: 0xff7ac8, zoom: 0x8fd3ff, density: 0xb0b0ff, fast: 0xffe066, drone: 0xb46bff, warp: 0x5ef2c8, bike: 0x5ef2c8, 'full screen': 0xb0b0ff,
};
const actionColor = (text) => {
  const word = text.replace(/^[\s:]*(drag to )?/, '').trim();
  return ACTION_COLORS[word] ?? null;
};
// Split a text run on its separators: the verbs take their colours, the
// slashes and colons stay dim.
const colourWords = (s) =>
  s.text.split(/(\s*\/\s*)/).filter(Boolean).map((t) => {
    const c = /^\s*\/\s*$/.test(t) ? null : actionColor(t);
    return { ...s, text: t, color: c ?? s.color };
  });
// held: { keys: [codes], pad: [badges], touch: ['left' | 'right'] } from the client.
function legendRows(input, held = {}, table = LEGEND) {
  const down = new Set(held[input === 'keys' ? 'keys' : input] || []);
  const lit = (code) => (Array.isArray(code) ? code.some((c) => down.has(c)) : down.has(code));
  return (table[input] || table.keys).map((line) => {
    const spans =
      typeof line === 'string'
        ? hintSpans(line, THEME.dim).map((s) => (s.badge && down.has(s.badge) ? { ...s, lit: true } : s))
        : line.map((part) =>
            typeof part === 'string'
              ? { text: part, color: THEME.dim }
              : { cap: part[0], lit: lit(part[1]) },
          );
    return colourRow(spans);
  });
}
// Verbs take their colours and each key cap the colour of the action it
// leads to (the next verb); separators stay dim.
function colourRow(spans) {
  const out = spans.flatMap((s) => (s.text && !s.badge ? colourWords(s) : [s]));
  for (let i = out.length - 1, next = null; i >= 0; i--) {
    const s = out[i];
    if (s.cap) { if (next) out[i] = { ...s, color: next }; }
    else if (s.text && ACTION_COLORS[s.text.replace(/^[\s:]*(drag to )?/, '').trim()]) next = s.color;
    else if (s.text && /\//.test(s.text)) next = null;
  }
  return out;
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
import { Gore } from './gore.mjs';
import { spanWidth as spanWidthOf } from './highlight.mjs';

// Build status shares the input prompt's font and wraps on its six-pixel grid.
function buildTextLines(builds, columns, now) {
  const WORDS = { asked: ['asking', THEME.verb], accepted: ['aesel has it', THEME.head], building: ['building', THEME.string], done: ['ready', 0x3fb950], failed: ['failed', 0xff6b6b], refused: ['not sent', 0xff6b6b], cancelled: ['cancelled', THEME.dim] };
  const dots = '.'.repeat(1 + (Math.floor(now / 330) % 3)).padEnd(3, ' ');
  const lines = [];
  for (const b of builds) {
    const [word, color] = WORDS[b.state] || [b.state, THEME.text], live = ['asked', 'accepted', 'building'].includes(b.state);
    const what = b.state === 'done' ? b.name || b.text : b.text;
    const more = b.state === 'asked' ? `${b.tries > 1 ? 'asked again ' : ''}${b.seconds}s`
      : b.state === 'accepted' ? `${b.seconds}s`
      : b.state === 'building' ? `${b.phase}${b.parts ? ` ${b.parts} part${b.parts === 1 ? '' : 's'}` : ''} ${b.seconds}s`
      : b.state === 'done' ? [b.kidlisp ? `$${b.kidlisp} is on the wall` : b.version !== undefined ? `v${b.version}` : '', b.where].filter(Boolean).join(' ')
      : b.note || '';
    const limit = Math.min(28, columns);
    const subject = what?.length > limit ? what.slice(0, Math.max(0, limit - 2)).trimEnd() + '..'.slice(0, limit) : what;
    const fields = [
      [live ? word + dots : word, color],
      [subject, THEME.text],
      [more, live || b.state === 'done' ? THEME.dim : THEME.number],
    ];
    let row = [], used = 0;
    for (const [value, ink] of fields) {
      if (!value) continue;
      const field = value.toLowerCase();
      if (used && used + 2 + field.length > columns) {
        lines.push(row);
        row = [];
        used = 0;
      }
      let rest = field;
      while (rest.length > columns) {
        const space = rest.lastIndexOf(' ', columns);
        const split = space > 0 ? space : columns;
        lines.push([{ text: rest.slice(0, split), color: b.fade < 0.5 ? THEME.dim : ink }]);
        rest = rest.slice(split).trimStart();
      }
      const text = (used ? '  ' : '') + rest;
      row.push({ text, color: b.fade < 0.5 ? THEME.dim : ink });
      used += text.length;
    }
    lines.push(row);
  }
  return lines;
}

// A dark room: night floor, slate walls; the two seat colours stay bright.
// Floor paint, cycled with X: red, yellow, green, blue, purple, pink.
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
    sky: 0xcfe3f5, // Solid, like the night's: the clear colour is the whole sky.
    far: 0xcfe3f5,
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
// Under the prompt's paper the host says which theme the prompt is in (it can
// be set by hand there, apart from the system's), and that wins.
let hostDark = null;
export const paletteFor = () => ((hostDark ?? darkScheme?.matches ?? true) ? 'night' : 'day');
import {
  WALLS,
  COLORS,
  seatColor,
  WORLD,
  TOWER_HALF,
  SCREEN,
  TILE_COLORS,
  CFG,
  direction,
  wallDistance,
  along,
  clamp,
} from './world.mjs';

// The $ history's thumbnails: TINY-square base64 RGB from the server, decoded
// once into 0xRRGGBB per pixel.
const TINY = 20;
const thumbCache = new Map();
function thumbPixels(b64) {
  if (thumbCache.has(b64)) return thumbCache.get(b64);
  let out = null;
  try {
    const bytes = Uint8Array.from(atob(b64), (c) => c.charCodeAt(0));
    if (bytes.length === TINY * TINY * 3) {
      out = new Uint32Array(TINY * TINY);
      for (let i = 0; i < out.length; i++) out[i] = (bytes[i * 3] << 16) | (bytes[i * 3 + 1] << 8) | bytes[i * 3 + 2];
    }
  } catch {}
  if (thumbCache.size > 512) thumbCache.delete(thumbCache.keys().next().value);
  thumbCache.set(b64, out);
  return out;
}

// Handle glyphs take one colour per character, '@' included.
const shadowInk = (label) =>
  Array.from({ length: label.length + 1 }, () => ({ r: 24, g: 24, b: 24 }));

// 🚫 Neither WebGPU nor WebGL 2: the page says so in its own hand, sized to
// the screen it is on (a television across a room reads it), with what each
// one answered underneath, and what to do.
function needsCard(canvas, why = []) {
  const board = canvas.cloneNode(false); // (a failed start can already own the canvas)
  canvas.replaceWith(board);
  board.dataset.engine = 'none';
  const g = board.getContext('2d');
  if (!g) return;
  const ratio = Math.min(2, (typeof devicePixelRatio === 'number' && devicePixelRatio) || 1);
  const w = board.width = Math.round((board.clientWidth || 640) * ratio), h = board.height = Math.round((board.clientHeight || 360) * ratio);
  const unit = Math.max(12, Math.min(w / 34, h / 16));
  g.fillStyle = '#0b0d1a'; g.fillRect(0, 0, w, h);
  // A faint floor of dots, as the studio's is.
  g.fillStyle = '#1b2040';
  for (let y = unit; y < h; y += unit * 2) for (let x = unit; x < w; x += unit * 2) g.fillRect(Math.round(x), Math.round(y), Math.max(2, unit / 8), Math.max(2, unit / 8));
  g.textAlign = 'center'; g.textBaseline = 'middle';
  const line = (text, y, size, ink) => {
    g.font = `${Math.round(size)}px ui-monospace, Menlo, Consolas, monospace`;
    // (A line too long for the screen is drawn smaller, never cut.)
    const wide = g.measureText(text).width, most = w - unit * 2;
    if (wide > most) g.font = `${Math.max(9, Math.floor(size * most / wide))}px ui-monospace, Menlo, Consolas, monospace`;
    g.fillStyle = ink; g.fillText(text, w / 2, y);
  };
  const secure = typeof isSecureContext === 'undefined' || isSecureContext;
  line('rax', h / 2 - unit * 3.2, unit * 2.4, '#ff5fa2');
  line('this browser cannot draw the world', h / 2 - unit * 0.6, unit * 1.15, '#f2f2f2');
  line(secure ? 'it needs WebGPU or WebGL 2: try a current Chrome, Edge, Safari or Firefox' : 'open it over https (or on localhost): WebGPU is only given to a secure page',
    h / 2 + unit * 1.1, unit * 0.7, '#9aa3d8');
  why.slice(0, 2).forEach((text, n) => line(text.slice(0, 96), h / 2 + unit * (2.6 + n * 0.9), unit * 0.5, '#5b6296'));
}

export async function createView(canvas) {
  let host, backend;
  let lost = false;
  // 🧊 The card's side of a frame (stalls.mjs): hand-off to finished, the
  // slowest since the client last read it, and how many are still out.
  const gpu = { max: 0, queued: 0 };
  const options = {
    maxTriangles: 98304, // (the lens halves long edges: a frame takes more triangles than its shapes have)
    shadows: true, // 🌑 the sun's shadows, as stencil volumes (shadow.mjs)
    onDeviceLost: () => {
      lost = true;
      canvas.dataset.engine = 'lost';
    },
  };
  // 🎛️ WebGPU where the browser has it, WebGL 2 where it has not (Xbox Edge,
  // older phones): the same lens, shadows, textures and glaze from the same
  // triangles (lens-host.mjs, lens-host-gl.mjs). ?gl asks for WebGL 2 anyway.
  const wantGL = typeof location !== 'undefined' && new URLSearchParams(location.search).has('gl');
  const why = []; // what each one said, for the page that has neither
  if (!wantGL) {
    try {
      host = await LensScene3D.create(canvas, options);
      backend = 'webgpu';
    } catch (error) {
      why.push(`WebGPU: ${error?.message || error}`);
    }
  }
  if (!host) {
    try {
      const { LensScene3DGL } = await import('./lens-host-gl.mjs');
      try {
        host = await LensScene3DGL.create(canvas, options);
      } catch (error) {
        // (A canvas that WebGPU took and then failed on gives no other kind: a fresh one, in its place.)
        if (wantGL || !why.length) throw error;
        const fresh = canvas.cloneNode(false);
        host = await LensScene3DGL.create(fresh, options);
        canvas.replaceWith(fresh);
        canvas = fresh;
      }
      backend = 'webgl2';
      if (why.length) console.warn(`rax: ${why[0]}; drawing with WebGL 2.`);
    } catch (error) {
      why.push(`WebGL 2: ${error?.message || error}`);
    }
  }
  if (!host) {
    console.error('rax needs WebGPU or WebGL 2.', ...why);
    needsCard(canvas, why);
    throw new Error('rax needs WebGPU or WebGL 2');
  }
  canvas.dataset.engine = backend;
  const projector = new Projector(host.scene);
  // 🖼️ Textures (textures.mjs): paintings, pictures and KidLisp, on the card's layers.
  const textures = createTextures({ host, layers: TEXTURE_LAYERS, maxSize: TEXTURE_SIZE, loaders: textureLoaders });
  projector.textures = (key) => textures.resolve(key);
  let staged = []; // 🎭 (stage)
  projector.marks = true; // 📺 screens tagged for the lens's glaze (lens-host.mjs)
  const facecam = new Projector(host.scene);
  // A small studio: a floor marked only by faint dots every two metres, four
  // plain walls and a low ceiling, all at the world's edges.
  const ROOM_HEIGHT = 4;
  // (walled false: a floating platform — the same floor on a slab, nothing
  // around it but the sky. A room made by name is one, 2026-11-27; a game's
  // arena keeps its walls. The edge still holds a body: world.mjs clamps.)
  const buildArena = (p, W = WORLD, walled = true) =>
    model(({ box, quad }) => {
      // (No ground beyond the walls: it was the sky's colour anyway, and a
      // centimetre under the floor it z-fought from any distance.)
      // (A platform's floor is one piece, edge to edge; a walled room's is the cellar's ceiling, with its stair hole.)
      if (!walled) quad([-W, 0, -W], [-W, 0, W], [W, 0, W], [W, 0, -W], p.floor);
      else for (const b of ceiling)
          quad([b.xMin, 0, b.zMin], [b.xMin, 0, b.zMax], [b.xMax, 0, b.zMax], [b.xMax, 0, b.zMin], p.floor);
      for (let x = -W + 2; x < W; x += 2)
        for (let z = -W + 2; z < W; z += 2)
          box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot); // clear of the floor and the paint
      // Four slabs that never share volume (2026-11-03: overlapping corners
      // z-fought): north and south run the full outside width and own the
      // corners; east and west fit exactly between their inner faces.
      const h = ROOM_HEIGHT, t = 0.3, outside = 2 * W + 2 * t, inside = 2 * W;
      if (!walled) {
        // The slab under the floor (its top a hair below, so the two never fight), each side in its wall's colour.
        box(0, -0.36, 0, 2 * W, 0.6, 2 * W, p.floor, p.walls.east, p.walls.north);
        return;
      }
      box(0, h / 2, -W - t / 2, outside, h, t, p.walls.south, p.wallSide, p.walls.south);
      box(0, h / 2, W + t / 2, outside, h, t, p.walls.north, p.wallSide, p.walls.north);
      // A box colours its x faces second and its z faces third: the long
      // walls show their z face, the end walls their x face.
      box(-W - t / 2, h / 2, 0, t, h, inside, p.walls.west, p.walls.west, p.wallSide);
      box(W + t / 2, h / 2, 0, t, h, inside, p.walls.east, p.walls.east, p.wallSide);
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
  // 🌞 The sky over the world (sky.mjs): a dome of bands from the horizon's
  // colour up to overhead, the sun and the moon as squares on it, and the
  // stars at night. Drawn at the eye and far behind everything, so it never
  // comes closer; rebuilt only when the world's clock steps on.
  const starField = fixedStars();
  let domeKept = null;
  function skyDome(step) {
    if (domeKept?.step === step) return domeKept.model;
    const s = skyAt(step / STEPS), R = 2350, n = 24;
    const bands = model(({ quad }) => {
      const edges = [-0.12, 0, 0.06, 0.16, 0.34, 0.65, 1.1, Math.PI / 2];
      const at = (turn, e) => [Math.cos(turn) * R * Math.cos(e), R * Math.sin(e), Math.sin(turn) * R * Math.cos(e)];
      for (let b = 0; b < edges.length - 1; b++) {
        const lo = edges[b], hi = edges[b + 1], color = skyColor(s, lo < 0 ? 0 : (lo + hi) / 2);
        for (let k = 0; k < n; k++) {
          const t0 = (k / n) * Math.PI * 2, t1 = ((k + 1) / n) * Math.PI * 2;
          quad(at(t0, lo), at(t1, lo), at(t1, hi), at(t0, hi), color);
        }
      }
    });
    const square = (quad, d, size, color) => {
      const flat = Math.hypot(d.x, d.z);
      const right = flat ? [d.z / flat, 0, -d.x / flat] : [1, 0, 0];
      const up = [d.y * right[2], d.z * right[0] - d.x * right[2], -d.y * right[0]];
      const corner = (a, b) => [d.x, d.y, d.z].map((v, i) => v * R + (right[i] * a + up[i] * b) * size / 2);
      quad(corner(-1, -1), corner(1, -1), corner(1, 1), corner(-1, 1), color);
    };
    const lights = model(({ quad }) => {
      if (s.stars > 0.3) for (const star of starField)
        square(quad, star, star.size, star.bright ? 0xffffff : 0xa9b8e6);
      if (s.sun.y > -0.12) {
        const around = skyColor(s, Math.max(0, Math.asin(s.sun.y)));
        square(quad, s.sun, 250, blendInk(around, s.sunColor, 0.35));
      }
    });
    const discs = model(({ quad }) => {
      if (s.sun.y > -0.12) square(quad, s.sun, 150, s.sunColor);
      if (s.moon.y > -0.12) square(quad, s.moon, 110, 0xe9edf6);
    });
    domeKept = { step, model: [bands, lights, discs] };
    return domeKept.model;
  }
  // 🪐 The planets hung over home (orbits.mjs HUNG): real balls where they
  // are, lit from the side the day's light is on. One is made again only
  // when that light has turned a couple of degrees.
  const hungBalls = new Map();
  function hungBall(b, L) {
    const key = L ? [L.x, L.y, L.z].map((n) => Math.round(n * 30)).join() : 'unlit', kept = hungBalls.get(b.id);
    if (kept?.key === key) return kept.model;
    const RINGS = 16, SEGMENTS = 28;
    const made = model(({ quad }) => {
      const at = (i, j) => { const e = (i / RINGS - 0.5) * Math.PI, t = (j / SEGMENTS) * 2 * Math.PI; return [Math.cos(e) * Math.cos(t), Math.sin(e), Math.cos(e) * Math.sin(t)]; };
      const ink = (n) => { if (!L) return b.ink; const lit = n[0] * L.x + n[1] * L.y + n[2] * L.z; return lit > 0 ? blendInk(b.dark, b.ink, Math.min(1, 0.35 + 0.65 * lit)) : blendInk(b.dark, b.ink, 0.12); };
      for (let i = 0; i < RINGS; i++)
        for (let j = 0; j < SEGMENTS; j++) {
          const p = [at(i, j), at(i + 1, j), at(i + 1, j + 1), at(i, j + 1)];
          quad(...p, p.map(ink));
        }
    });
    hungBalls.set(b.id, { key, model: made });
    return made;
  }
  // Where each is drawn this frame, and how far that is from where it is.
  // The land is wrapped round a ball under the eye (project.mjs), which
  // spreads whatever is high over it; a planet is drawn whole instead, at
  // the place the wrapping gives its middle. Come near one, it eases to
  // where it truly is, so that standing on it the feet are on it; and what
  // stands on it (within `hold` of its middle) is drawn with it.
  let hungNow = [];
  // The room follows the clock: rebuilt when the hour crosses into day or night.
  let paletteName = null, arena, skyModel, sky;
  const arenas = new Map(); // a plain room's walls and floor, by palette and half-width
  // 🌍 Per-block models, kept by block id ('here' is the one underfoot) and
  // rebuilt only when what they show changes.
  const grounds = new Map(), tileSets = new Map();
  // A block's ground: the plaza's floor or a residential block's grass and
  // lanes. A side with an open neighbour is left open; the world's edge gets
  // the plaza's wall or the park's hedge.
  const drawPool = poolView();
  const cellar = model(({ box, text }) => {
    poolFloor({box});
    for (const b of [...treads, ...cellarWalls, ...ceiling]) {
      box((b.xMin + b.xMax) / 2, (b.yMin + b.yMax) / 2, (b.zMin + b.zMax) / 2,
        b.xMax - b.xMin, b.yMax - b.yMin - (b.ceiling ? 0.03 : 0), b.zMax - b.zMin, b.ink, tint(b.ink, 0.74), b.ink);
    }
    for (const x of [7.76]) for (const z of [-5, 0, 5])
      box(x, 2.4, z, 0.08, 0.14, 2.4, 0xffce81, 0xffb56a, 0xffce81);
    text('B1', 0, 2.4, -7.74, 0.14, 0xffce81);
    const signX = STAIR.left - 0.12;
    box(signX, DEPTH + 0.64, STAIR.lip, 0.66, 0.48, 0.09, 0x223448, 0x223448, 0x536476);
    text('B1', signX, DEPTH + 0.78, STAIR.lip - 0.055, 0.044, 0xffce81);
    for (const step of treads.slice(0, STAIR.steps))
      box((STAIR.left + STAIR.right) / 2, step.yMax - 0.025, step.zMax, STAIR.right - STAIR.left, 0.05, 0.025, 0xd8ae7c, 0xd8ae7c, 0xd8ae7c);
  });
  function groundFor(id, neighbours, plaza, field = null) {
    const W = TOWER_HALF; // the plaza is the tower's ground floor
    const key = JSON.stringify([neighbours, !!plaza, paletteName, field, W]);
    const kept = grounds.get(id);
    if (kept?.key === key) return kept.model;
    const p = PALETTES[paletteName], open = neighbours || {};
    const made = model(({ quad, box }) => {
      if (plaza) {
        quad([-W, 0, -W], [-W, 0, W], [W, 0, W], [W, 0, -W], p.floor);
        for (let x = -W + 2; x < W; x += 2)
          for (let z = -W + 2; z < W; z += 2)
            if (!(x >= STAIR.left && x <= STAIR.right && z >= STAIR.landing && z <= STAIR.end)) box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot);
        const h = ROOM_HEIGHT, t = 0.3, outside = 2 * W + 2 * t, inside = 2 * W;
        if (!open.south) box(0, h / 2, -W - t / 2, outside, h, t, p.walls.south, p.wallSide, p.walls.south);
        if (!open.north) box(0, h / 2, W + t / 2, outside, h, t, p.walls.north, p.wallSide, p.walls.north);
        if (!open.west) box(-W - t / 2, h / 2, 0, t, h, inside, p.walls.west, p.walls.west, p.wallSide);
        if (!open.east) box(W + t / 2, h / 2, 0, t, h, inside, p.walls.east, p.walls.east, p.wallSide);
        return;
      }
      const day = paletteName === 'day';
      const grass = day ? 0xb9d7ac : 0x243d36;
      const path = day ? 0xe5decb : 0x5a6664;
      quad([-8, 0, -8], [-8, 0, 8], [8, 0, 8], [8, 0, -8], grass);
      if (field) {
        // 🌳 The tower's grounds: open grass, and south of the door the
        // promenade (park.mjs): a reflecting pool between two rose beds.
        const inks = day
          ? { paving: 0xefe7da, stone: 0xdcd6c8, water: 0x8fc7e0, soil: 0x6b4a3a, leaf: 0x4f9a5b, post: 0x8f98ac, lamp: 0xffe9a8, cushion: 0xff7ac8,
              rose0: 0xff5c8a, rose1: 0xe63462, rose2: 0xff86b5, rose3: 0xffd1dc }
          : { paving: 0x3a3644, stone: 0x4a4e5c, water: 0x1b2c4a, soil: 0x2e2422, leaf: 0x244a38, post: 0x59607a, lamp: 0xffe9a8, cushion: 0x8a3a6c,
              rose0: 0xb8405f, rose1: 0x9c2444, rose2: 0xb86080, rose3: 0xc09aa4 };
        const laid = promenade(field);
        if (laid) {
          for (const f of laid.flats) quad([f.x0, f.y, f.z0], [f.x0, f.y, f.z1], [f.x1, f.y, f.z1], [f.x1, f.y, f.z0], inks[f.ink]);
          for (const s of laid.solids) box(s.x, s.y, s.z, s.w, s.h, s.d, inks[s.ink], tint(inks[s.ink], 0.8), inks[s.ink]);
        } else {
          // A field beside the tower: two trees, set by where the field lies.
          const wood = day ? 0xc9a37a : 0x6b5440, leaf2 = day ? 0x8cc97f : 0x3b6e52;
          for (const [x, z] of [[3.5 * Math.sign(field.i || 1), -3 + field.j], [-4 + field.i, 4.5 * Math.sign(field.j || -1)]]) {
            box(x, 1.15, z, 0.3, 2.3, 0.3, wood, tint(wood, 0.8), wood); // (its top a little way into the leaves: level with their underside, the two fought for the pixel)
            box(x, 3, z, 2.2, 1.6, 2.2, inks.leaf, tint(inks.leaf, 0.82), leaf2);
          }
        }
      } else {
        quad([-1, 0.006, -8], [-1, 0.006, 8], [1, 0.006, 8], [1, 0.006, -8], path);
        quad([-8, 0.007, -1], [-8, 0.007, 1], [8, 0.007, 1], [8, 0.007, -1], path);
      }
      for (const edge of ['east', 'west', 'north', 'south']) {
        if (open[edge]) continue;
        const across = edge === 'north' || edge === 'south';
        const side = edge === 'east' || edge === 'north' ? 1 : -1;
        box(across ? 0 : side * 8, 0.3, across ? side * 8 : 0, across ? 16 : 0.2, 0.6, across ? 0.2 : 16, 0x47765d);
      }
    });
    grounds.set(id, { key, model: made });
    return made;
  }
  // 🛗 The elevator car against the north edge of the plaza and of every
  // floor: a glass box (posts, a sill, a roof) with its door to the south and
  // the world behind its back. It wears no sign: the pad inside tells the floor.
  let carSet = null;
  function carFor(tower) {
    const key = paletteName;
    if (carSet?.key === key) return carSet.model;
    const day = paletteName === 'day';
    const frame = day ? 0x8f98ac : 0x59607a, sill = day ? 0xdfe3ee : 0x3d4357, pad = day ? 0xffd9ec : 0x5a2244;
    const made = model(({ box }) => {
      // The car's solids (elevator.mjs): a floor, a roof, glass on three
      // sides, a post at each corner and a rail over the door, none of
      // them sharing volume with another.
      const inks = {
        pad: [pad, frame, pad], roof: [sill, frame, sill], sill: [sill, frame, sill],
        frame: [frame, frame, frame],
      };
      for (const s of carSolids()) box(s.x, s.y, s.z, s.w, s.h, s.d, ...inks[s.ink]);
      // Reflections mark the three glazed panes of the car.
      const glint = day ? 0xacdce5 : 0x608b9b;
      for (const side of [-1, 1]) for (let i = 0; i < 4; i++) {
        box(side * (CAR + CAR_WALL / 2), 1.85 + i * 0.09, CAR_Z + 0.55 - i * 0.12, 0.03, 0.07, 0.16, glint);
        box(side * 0.8 + i * 0.08, 1.85 + i * 0.09, CAR_Z + CAR + CAR_WALL / 2, 0.13, 0.07, 0.03, glint);
      }
    });
    carSet = { key, model: made };
    return made;
  }
  // 📟 The screen inside the car, over the door: the floor passing now, whose
  // it is, and where the car is going. Built facing -z; drawn turned round.
  let carScreen = null;
  function carScreenFor(screen) {
    const key = JSON.stringify(screen);
    if (carScreen?.key === key) return carScreen.model;
    const made = model(({ box, text }) => {
      // A bezel plate with the dark screen proud of it: no two faces share a plane.
      const inks = { bezel: 0x59607a, screen: 0x1d2536 };
      for (const s of carScreenSolids()) box(s.x, s.y, s.z, s.w, s.h, s.d, inks[s.ink], inks[s.ink], inks[s.ink]);
      const fit = (label, width, most) => Math.min(most, width / Math.max(1, label.length * 6));
      const line = (label, x, top, width, most, color) => {
        const unit = fit(label, width, most);
        text(label, x + unit, top - 2 * unit, -0.003, unit, 0x000000);
        text(label, x, top - unit, -0.006, unit, color);
      };
      // The number large on the left; the name and the trip beside it.
      line(screen.number, -0.85, 2.93, 0.9, 0.06, screen.moving ? 0xf4f1ea : 0xff3296);
      if (screen.handle) line(screen.handle, 0.45, 2.9, 1.6, 0.03, 0x9be7a0);
      if (screen.note) line(screen.note, 0.45, 2.55, 1.6, 0.022, 0x9aa3b5);
    });
    carScreen = { key, model: made };
    return made;
  }
  // 🏢 The tower itself, in pieces drawn at their real heights: a storey's
  // glass edge (a sill and mullions, open where the car runs), a storey's
  // slab, its four corner columns, and the legs that carry it over the plaza.
  let towerSet = null;
  function towerParts() {
    if (towerSet?.key === paletteName) return towerSet;
    const day = paletteName === 'day', p = PALETTES[paletteName];
    const slab = day ? 0xd9dde8 : 0x30354a, edge = day ? 0xb9c0d2 : 0x232738, under = day ? 0xc9cedb : 0x262a3c;
    const post = day ? 0x9aa3b5 : 0x4b526b, sill = day ? 0xe9edf5 : 0x3a4056, column = day ? 0xaab2c6 : 0x3f455c;
    const W = TOWER_HALF, E = LEG.at;
    // The shaft: the bay of every floor the car passes through, left open.
    const S = SLAB, bayX = CAR + CAR_WALL + 0.25, bayZ = CAR_Z - CAR - CAR_WALL - 0.05;
    // A floor-sized sheet at height y with the shaft cut out of it, as three pieces.
    const sheet = (piece) => {
      piece(-S, S, -S, bayZ);
      piece(-S, -bayX, bayZ, S);
      piece(bayX, S, bayZ, S);
    };
    const slabModel = model(({ box }) =>
      sheet((x0, x1, z0, z1) => box((x0 + x1) / 2, -0.21, (z0 + z1) / 2, x1 - x0, 0.4, z1 - z0, edge, edge, slab)));
    // Columns for one storey and, to save drawing, for ten at once.
    const columns = (storeys) => model(({ box }) => {
      for (const x of [-E, E]) for (const z of [-E, E])
        box(x, (storeys * STOREY) / 2 - 0.4, z, 0.5, storeys * STOREY, 0.5, column, column, column);
    });
    const glass = model(({ box }) => {
      const run = (along, at, from, to) => {
        const mid = (from + to) / 2, len = to - from;
        const b = (y, h, w, color) => along === 'x'
          ? box(mid, y, at, len, h, w, color, color, color)
          : box(at, y, mid, w, h, len, color, color, color);
        b(0.5, 1, 0.12, sill); // the sill, to the hip
        b(1.04, 0.08, 0.2, post); // its rail
        b(STOREY - 0.5, 0.2, 0.12, post); // the head, under the next slab
        for (let s = from + 4; s < to - 0.5; s += 4)
          along === 'x' ? box(s, STOREY / 2 - 0.2, at, 0.1, STOREY - 0.4, 0.1, post, post, post)
            : box(at, STOREY / 2 - 0.2, s, 0.1, STOREY - 0.4, 0.1, post, post, post);
      };
      run('x', -E, -W, W);
      run('z', -E, -W, W);
      run('z', E, -W, W);
      // North, either side of the shaft: the car's glass back is the window there.
      run('x', E, -W, -CAR - 0.3);
      run('x', E, CAR + 0.3, W);
    });
    // Two rails the car runs on, a storey tall.
    const rails = model(({ box }) => {
      for (const x of [-CAR - 0.3, CAR + 0.3]) box(x, STOREY / 2 - 0.2, E, 0.14, STOREY, 0.14, post, post, post);
    });
    const legs = model(({ box }) => {
      for (const x of [-E, E]) for (const z of [-E, E]) box(x, ATRIUM / 2 - 0.2, z, LEG.wide, ATRIUM - 0.4, LEG.wide, column, column, column);
      for (const x of [-CAR - 0.3, CAR + 0.3]) box(x, ATRIUM / 2, E, 0.14, ATRIUM, 0.14, post, post, post);
    });
    // The floor you stand on: the room's own floor and dots, no walls.
    // (`roofed`: under the floor above. The roof's deck is the same floor under the sky.)
    const floorOf = (roofed) => model(({ quad, box }) => {
      const cut = (x0, x1, z0, z1) => [Math.max(-W, x0), Math.min(W, x1), Math.max(-W, z0), Math.min(W, z1)];
      sheet((...r) => { const [a, b, c, d] = cut(...r); quad([a, 0, c], [a, 0, d], [b, 0, d], [b, 0, c], p.floor); });
      for (let x = -W + 2; x < W; x += 2)
        for (let z = -W + 2; z < W; z += 2)
          if (!(Math.abs(x) < bayX && z > bayZ)) box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot);
      // The underside of the floor above is this floor's ceiling.
      const top = STOREY - 0.42;
      if (roofed) sheet((...r) => { const [a, b, c, d] = cut(...r); quad([a, top, c], [b, top, c], [b, top, d], [a, top, d], under); });
    });
    const floor = floorOf(true), deck = floorOf(false);
    // 🏙️ What stands on the roof (elevator.mjs roofSolids): the parapet round its edge and the curbs beside the car.
    const parapet = model(({ box }) => {
      const inks = { sill: [sill, edge, sill], frame: [post, post, post] };
      for (const s of roofSolids()) box(s.x, s.y, s.z, s.w, s.h, s.d, ...inks[s.ink]);
    });
    // The land around the plaza and its blocks, out to the horizon: four
    // fields around the middle 48 m, which the blocks themselves fill.
    const grass = day ? 0xb9d7ac : 0x243d36;
    // (planet.mjs landTiles: rings of squares, each three times the last, far
    // past where a drone may fly; the hole a little under the blocks' edge.
    // Each tile is a fan about its middle, to every point round its edge.)
    const LAND_REACH = 9000;
    const tiled = (tri, tiles, y, ink) => {
      for (const t of tiles) {
        const mid = [(t.x0 + t.x1) / 2, y, (t.z0 + t.z1) / 2], n = t.points.length;
        for (let k = 0; k < n; k++) tri(mid, [t.points[k][0], y, t.points[k][1]], [t.points[(k + 1) % n][0], y, t.points[(k + 1) % n][1]], ink(t));
      }
    };
    const land = model(({ tri }) => tiled(tri, landTiles(47, LAND_REACH), -0.02, () => grass));
    // 🫧 The land from underneath, for an eye inside the hollow planet: a
    // bubble's skin in two pale tints, each ring's squares cut in nine, seen
    // through (drawn below) so the tower and the sky show beyond it.
    const earth = day ? [0xcfeaff, 0xb4dcf8] : [0x4a6a9c, 0x3c5a8a];
    const dome = model(({ tri }) => tiled(tri, landTiles(47, LAND_REACH, 3), -0.5, (t) => earth[t.odd]));
    const lawn = model(({ quad }) => quad([-8, -0.02, -8], [-8, -0.02, 8], [8, -0.02, 8], [8, -0.02, -8], grass));
    towerSet = { key: paletteName, slabModel, columns1: columns(1), columns10: columns(10), glass, rails, legs, floor, deck, parapet, land, dome, lawn };
    return towerSet;
  }
  // 🚪 Automatic doors: two glass leaves that slide apart when someone comes
  // near. `width` is one leaf's; the entrance's are DOOR wide, the car's CAR.
  const leaves = new Map();
  function leafFor(width, height) {
    const key = `${width}:${height}:${paletteName}`;
    if (leaves.has(key)) return leaves.get(key);
    const post = paletteName === 'day' ? 0x8f98ac : 0x59607a, pane = paletteName === 'day' ? 0xdfeaf6 : 0x2c3550;
    const made = model(({ box }) => {
      const t = 0.07;
      box(0, t / 2, 0, width, t, t, post, post, post);
      box(0, height - t / 2, 0, width, t, t, post, post, post);
      for (const x of [-width / 2 + t / 2, width / 2 - t / 2]) box(x, height / 2, 0, t, height, t, post, post, post);
      box(0, height * 0.42, 0, width, 0.05, 0.05, post, post, post); // the push rail
      box(0, 0.22, 0, width - 2 * t, 0.3, 0.03, pane, pane, pane); // a kick plate, so the glass reads
    });
    leaves.set(key, made);
    return made;
  }
  // How open each door is, 0 shut … 1 open, eased frame to frame.
  const doorAt = { entrance: 0, car: 0 };
  let doorTime = performance.now();
  // 🚪 The lobby: a glass hall the height of the tower's legs, across the
  // north half of the plaza. A canopy and the tower's name over the entrance;
  // inside, a pale floor and benches.
  let lobbySet = null;
  function lobbyFor() {
    const key = paletteName;
    if (lobbySet?.key === key) return lobbySet.model;
    const day = paletteName === 'day';
    const post = day ? 0x8f98ac : 0x59607a, sill = day ? 0xe9edf5 : 0x3a4056, stone = day ? 0xf3eee6 : 0x34313f;
    const dark = 0x171d26, wood = day ? 0xc9a37a : 0x6b5440, cushion = 0xff7ac8;
    const made = model(({ quad, box, text, handleText }) => {
      const f = LOBBY_FRONT, H = ATRIUM - 0.4, W = TOWER_HALF;
      // The hall's own floor, clear of the car's bay.
      quad([-W, 0.011, f], [-W, 0.011, W], [W, 0.011, W], [W, 0.011, f], stone);
      // The front: a sill and glass to the first floor, open at the entrance.
      for (const [a, b] of [[-W, -DOOR], [DOOR, W]]) {
        box((a + b) / 2, 0.25, f, b - a, 0.5, 0.2, sill, post, sill);
        for (const y of [3.2, 6.2, 9.2]) box((a + b) / 2, y, f, b - a, 0.12, 0.12, post, post, post);
        for (let x = a; x <= b + 0.01; x += (b - a) / 3) box(x, (H + 0.5) / 2, f, 0.12, H - 0.5, 0.12, post, post, post); // standing on the sill
      }
      box(0, H - 0.1, f, 2 * W, 0.2, 0.14, post, post, post);
      // The entrance: a frame, glass over it, and a canopy out over the plaza.
      box(0, DOOR_HEIGHT + 0.08, f, 2 * DOOR + 0.24, 0.16, 0.22, post, post, post);
      for (const y of [6.2, 9.2]) box(0, y, f, 2 * DOOR, 0.12, 0.12, post, post, post);
      box(0, 3.25, f - 0.9, 6.4, 0.16, 2, sill, post, sill);
      const fit = (label, width, most) => Math.min(most, width / Math.max(1, label.length * 6));
      // Sign text with a hard shadow one pixel down and across, just behind it.
      const inked = (label, x, y, z, unit, color) => {
        text(label, x + unit, y - unit, z + 0.004, unit, 0x000000);
        text(label, x, y, z, unit, color);
      };
      const line = (label, x, top, z, width, most, color) => {
        const unit = fit(label, width, most);
        inked(label, x, top - unit, z, unit, color);
      };
      // (No name over the door: the tower is its own sign.)
      // The two ends, glass like the front.
      for (const x of [-W + 0.05, W - 0.05]) {
        box(x, 0.25, (f + W) / 2, 0.2, 0.5, W - f, sill, sill, post);
        for (const y of [3.2, 6.2, 9.2]) box(x, y, (f + W) / 2, 0.12, 0.12, W - f, post, post, post);
        for (let z = f; z <= W + 0.01; z += (W - f) / 3) box(x, (H + 0.5) / 2, z, 0.12, H - 0.5, 0.12, post, post, post); // standing on the sill
      }
      // Inside: two benches and a runner from the door to the car. (No desk and no board: the walls are for screens.)
      for (const z of [3, 6]) {
        box(5.6, 0.22, z, 0.7, 0.44, 1.9, post, post, post);
        box(5.6, 0.48, z, 0.74, 0.1, 1.94, cushion, cushion, cushion);
      }
      quad([-1.3, 0.016, f + 0.1], [-1.3, 0.016, CAR_Z - CAR - 0.3], [1.3, 0.016, CAR_Z - CAR - 0.3], [1.3, 0.016, f + 0.1], day ? 0xffd9ec : 0x5a2244);
      // 🌳 The courtyard, on the plaza's side of the glass: a paved walk to
      // the door, a cross walk, two planters with trees, lamps, benches.
      const paving = day ? 0xefe7da : 0x3a3644, leaf = day ? 0x6fb26a : 0x2f5a44, leaf2 = day ? 0x8cc97f : 0x3b6e52;
      quad([-DOOR, 0.008, -W], [-DOOR, 0.008, f], [DOOR, 0.008, f], [DOOR, 0.008, -W], paving);
      quad([-W, 0.008, -1.2], [-W, 0.008, 0.2], [-DOOR, 0.008, 0.2], [-DOOR, 0.008, -1.2], paving);
      quad([DOOR, 0.008, -1.2], [DOOR, 0.008, 0.2], [W, 0.008, 0.2], [W, 0.008, -1.2], paving);
      for (const p of PLANTERS) {
        box(p.x, 0.275, p.z, PLANTER, 0.55, PLANTER, sill, post, leaf);
        box(p.x, 1.5, p.z, 0.28, 1.9, 0.28, wood, tint(wood, 0.8), wood);
        box(p.x, 3.1, p.z, 1.9, 1.5, 1.9, leaf, tint(leaf, 0.82), leaf2);
        box(p.x + 0.25, 4.1, p.z - 0.2, 1.2, 0.9, 1.2, leaf2, leaf, leaf2);
      }
      for (const x of [-2.3, 2.3])
        for (const z of [-2.2, -6.2]) {
          box(x, 1.4, z, 0.1, 2.8, 0.1, post, post, post);
          box(x, 2.9, z, 0.34, 0.34, 0.34, 0xffe9a8, 0xffd27a, 0xffe9a8);
        }
      for (const x of [-3.6, 3.6]) {
        box(x, 0.22, -6.4, 1.9, 0.44, 0.7, post, post, post);
        box(x, 0.48, -6.4, 1.94, 0.1, 0.74, cushion, cushion, cushion);
      }
    });
    lobbySet = { key, model: made };
    return made;
  }
  // Draw the tower standing at (ox, oy, oz), `top` floors tall, as seen from
  // a height: full storeys near the eye, slabs further off, columns to the
  // limit of sight. `stood` is what must be drawn under whoever is seen on
  // it (elevator.mjs standsOn), and `own` the height of this room's own car
  // (null where the room has none).
  function drawTower(top, ox, oy, oz, eyeY, zoom, stood = null, own = null) {
    const t = towerParts();
    if (!top) return;
    draw(t.legs, ox, oy, oz);
    const near = 9 * zoom, mid = 70 * zoom, sight = 2300;
    // (Counted in storeys, n: one for every floor in the full tower, a few
    // while it is short. elevator.mjs SHORT.)
    top = storeyOf(top);
    const at = Math.max(1, Math.min(top, storeyAt(eyeY - oy) || 1));
    const lo = Math.max(1, storeyAt(eyeY - oy - sight) || 1), hi = Math.min(top, Math.max(1, storeyAt(eyeY - oy + sight)));
    for (let n = lo; n <= hi; ) {
      const y = oy + heightOf(n);
      if (n + 9 <= hi && (n > at + mid || n + 9 < at - mid)) { draw(t.columns10, ox, y, oz); n += 10; continue; }
      draw(t.columns1, ox, y, oz);
      if (Math.abs(n - at) <= mid) draw(t.slabModel, ox, y, oz);
      if (Math.abs(n - at) <= near) { draw(t.glass, ox, y, oz); draw(t.rails, ox, y, oz); }
      n++;
    }
    if (hi === top) { // the roof, and its parapet
      draw(t.slabModel, ox, oy + heightOf(top + 1), oz);
      draw(t.parapet, ox, oy + heightOf(top + 1), oz);
    }
    if (!stood) return;
    // 🧍 Nobody stands on air: a storey someone is seen on has its slab,
    // however far off it is, and whoever stands in the shaft (where every
    // slab is open) has a car under them. This room's own car is drawn with
    // the room, so not again where it stands.
    for (const n of stood.slabs)
      if (n <= top + 1 && !(n >= lo && n <= hi && Math.abs(n - at) <= mid) && !(n === top + 1 && hi === top)) draw(t.slabModel, ox, oy + heightOf(n), oz);
    for (const n of stood.cars) {
      const y = oy + heightOf(n);
      if (n <= top + 1 && !(own !== null && !ox && !oz && Math.abs(y - own) < CAR_HEIGHT)) draw(carFor(), ox, y, oz);
    }
  }
  // 🍄 botbop's ledges: slabs lit on top, rebuilt with the palette.
  // 🤖 A drone: a squat grey box on a tread, one cyan eye, a pink aerial.
  let scoreShown = 0, scoreAt = 0; // the headline score and when it last rose
  let chainShown = [0, 0], chainAt = 0, flyPoints = 0, flyAt = 0; // 🔗 the chain under it, and its flight up on landing
  let droneMesh = null;
  const droneModel = () =>
    (droneMesh ||= model(({ box }) => {
      box(0, 0.09, 0, 0.62, 0.18, 0.52, 0x2c3038, 0x23262d, 0x23262d);
      box(0, 0.48, 0, 0.7, 0.6, 0.6, 0xa9b2c4, 0x7d869a, 0x8f98ac);
      box(0, 0.93, 0, 0.5, 0.3, 0.5, 0xc4ccdb, 0x8f98ac, 0xa9b2c4);
      box(0, 0.94, 0.26, 0.3, 0.1, 0.04, 0x3fe0d0, 0x3fe0d0, 0x3fe0d0);
      box(0, 1.17, 0, 0.05, 0.18, 0.05, 0xff3296, 0xff3296, 0xff3296);
    }));
  // The stage is the studio with its near wall away, so the camera looks in.
  let ledgeSet = null;
  function ledgesFor(ledges) {
    if (ledgeSet?.key === paletteName && ledgeSet.ledges === ledges) return ledgeSet.model;
    const p = PALETTES[paletteName];
    const made = model(({ box, quad }) => {
      quad([-WORLD, 0, -WORLD], [-WORLD, 0, WORLD], [WORLD, 0, WORLD], [WORLD, 0, -WORLD], p.floor);
      const h = 7, t = 0.3;
      box(0, h / 2, -WORLD - t / 2, 2 * WORLD + 2 * t, h, t, p.walls.south, p.wallSide, p.walls.south);
      box(-WORLD - t / 2, h / 2, 0, t, h, 2 * WORLD, p.walls.west, p.walls.west, p.wallSide);
      box(WORLD + t / 2, h / 2, 0, t, h, 2 * WORLD, p.walls.east, p.walls.east, p.wallSide);
      for (const b of ledges)
        box((b.xMin + b.xMax) / 2, (b.yMin + b.yMax) / 2, (b.zMin + b.zMax) / 2,
          b.xMax - b.xMin, b.yMax - b.yMin, b.zMax - b.zMin, 0xff3296, p.wallSide, p.walls.north);
    });
    ledgeSet = { key: paletteName, ledges, model: made };
    return made;
  }
  // A neighbouring block's painted floor squares.
  function tilesFor(id, tiles) {
    const key = JSON.stringify(tiles || {});
    const kept = tileSets.get(id);
    if (kept?.key === key) return kept.model;
    const made = key === '{}' ? null : model(({ quad }) => {
      for (const [at, c] of Object.entries(tiles)) {
        const [i, j] = at.split(',').map(Number);
        const x = -18 + i * 2, z = -18 + j * 2, col = TILE_COLORS[(c - 1) % TILE_COLORS.length];
        quad([x, 0.005, z], [x + 2, 0.005, z], [x + 2, 0.005, z + 2], [x, 0.005, z + 2], col);
      }
    });
    tileSets.set(id, { key, model: made });
    return made;
  }
  function dress(name) {
    if (name === paletteName) return;
    const p = PALETTES[name];
    paletteName = name;
    arena = buildArena(p);
    arenas.clear();
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
  // 🩸 Gore (gore.mjs): the same parts, centred so a flying one spins about
  // itself, with a red cut where it came away; drops; three splats.
  const RED = 0xa8101c, DARK = 0x5c0610;
  const gibFighters = COLORS.map((color) => ({
    'gib-torso': model(({ box }) => { box(0, 0, 0, 0.42, 0.62, 0.26, color, INK, PAPER); box(0, 0.31, 0, 0.3, 0.02, 0.18, RED, DARK, RED); }),
    'gib-head': model(({ box }) => {
      box(0, 0, 0, 0.44, 0.42, 0.36, PAPER, 0xc0c0c0, PAPER);
      box(0, 0.245, 0, 0.36, 0.09, 0.3, BRAIN, tint(BRAIN, 0.75), tint(BRAIN, 1.15));
      box(0, -0.215, 0, 0.2, 0.02, 0.18, RED, DARK, RED); // (the neck)
    }),
    'gib-limb': model(({ box }) => { box(0, 0, 0, 0.13, 0.46, 0.16, color, INK, PAPER); box(0, 0.235, 0, 0.1, 0.02, 0.12, RED, DARK, RED); }),
    'gib-hand': model(({ box }) => box(0, 0, 0, 0.16, 0.17, 0.2, PAPER, 0xc0c0c0, PAPER)),
  }));
  const goreModels = {
    'gib-meat': model(({ box }) => { box(0, 0, 0, 0.14, 0.11, 0.12, RED, DARK, 0xd4303c); box(0.04, 0.05, 0, 0.06, 0.05, 0.06, 0xf0e6d8, 0xc8b8a0, 0xf0e6d8); }),
    drop: model(({ box }) => box(0, 0, 0, 0.045, 0.045, 0.045, RED, DARK, 0xd02030)),
    // Splats: a pool and its droplets, flat on y = 0 (each a different throw).
    ...Object.fromEntries([0, 1, 2].map((k) => [`splat${k}`, model(({ box }) => {
      let seed = 7 + k * 31;
      const r = () => ((seed = (seed * 16807) % 2147483647) / 2147483647);
      box(0, 0, 0, 0.42 + k * 0.08, 0.006, 0.34 + k * 0.05, DARK, DARK, DARK);
      box(0.05, 0.002, -0.03, 0.3, 0.006, 0.26, RED, DARK, RED);
      for (let n = 0; n < 9 + k * 3; n++) {
        const a = r() * Math.PI * 2, d = 0.22 + r() * 0.4, w = 0.03 + r() * 0.07;
        box(Math.cos(a) * d, 0.001, Math.sin(a) * d, w * (1 + r()), 0.006, w, r() < 0.5 ? RED : DARK, DARK, RED);
      }
    })])),
  };
  const gore = new Gore();
  // (A target's box is bigger than its straw man: blood would hang in the air beside it.)
  let goreFrom = null, goreList = [];
  const goreSolids = (list) => (list === goreFrom ? goreList : (goreFrom = list, goreList = list.filter((o) => !o.target)));
  const lastPose = new Map(); // seat → how its body stood when last drawn (for its ragdoll)
  let goreAt = 0;
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
  // 🏙️ What a body comes apart into (instagib): chunks of red and bone.
  const GIB = [0xc0202a, 0x8a1018, 0xe84a4a, 0xf0e6d8].map((c) => model(({ box }) => box(0, 0, 0, 0.16, 0.16, 0.16, c, c, c)));
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
  // 🎯 Damage numbers off a struck target: they rise and fade (match.mjs strike).
  const pops = [];
  const POP_MS = 900;
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
  const objectModels = new WeakMap(),
    previewModels = new Map();
  let promptKey, promptModel;
  let presenceKey, presenceModel;
  let ringKey, ringModel, devKey, devModel, fpsKey, fpsModel, netKey, netModel;
  let netSince = null; // When the connection went down, for the log's typing.
  // 🔭 The lens: how much of it (1: straight lines near the edge bow a good deal), and how much larger the middle is drawn under all of it. It is done to the finished picture on the card (lens-host.mjs): corner by corner it could not keep two things that lie on one another in order, and holes opened in them.
  const LENS = 0.65, LENS_ZOOM = 1.3;
  let lensNow = null; // what the card is told of the lens this frame
  // 🪶 Anti-aliasing: the world drawn twice as fine each way as it is shown, and averaged (lens-host.mjs). ?aa=1 turns it off, ?aa=3 is finer still.
  const AA = (() => { const q = typeof location !== 'undefined' ? new URLSearchParams(location.search) : null; return q?.has('aa') ? Math.max(1, Math.min(3, Number(q.get('aa')) || 1)) : 1; })();
  // 👁️ How much of the picture, from its middle out, is drawn at full sharpness (lens-host.mjs); ?fovea=0 draws all of it so, ?fovea=0.3 a smaller middle.
  const FOVEA = (() => { const q = typeof location !== 'undefined' ? new URLSearchParams(location.search) : null; return q?.has('fovea') ? Math.max(0, Math.min(0.9, Number(q.get('fovea')) || 0)) : 0.5; })();
  const SKY = (0.97 * FAR) / 2350; // the sky's dome, drawn this many times its own size: behind everything, however high or far the eye
  let cost = 0; // milliseconds a frame's drawing takes on this machine, eased
  let plain = 0;
  let lensFull = 0; // how full of triangles the last frame was, 0 to 1: the lens halves edges less finely when it runs high
  let density = 0;
  try { density = Math.max(-5, Math.min(5, Math.round(Number(localStorage.getItem('rax-density')) || 0))); } catch {}
  const frameChart = createFrameChart();
  let chartVisible = false;
  // 📐 The axes' colours, as in any 3D tool: x red, y green, z blue.
  const AXIS = { x: 0xff6b6b, y: 0x6fdc6f, z: 0x6aa8ff };
  let panelTop = null;
  let objectHintSpans = null; // What the highlighted object offers, drawn at the object.
  let previousRegions = [], screenFrame = null, frameLayoutKey = '', handleRect = null;
  const overlayBounds = new WeakMap();
  let headsetRects = [];
  let cornerRect = null; // The corner block, in buffer pixels from the top left.
  let menuBox = null; // The open menu's panel and rows, in buffer pixels about the centre.
  let lastEye = null; // Where the camera stood last frame, for the cursor's aim ray.
  let natural = null; // 🖥️ The picture's scale last frame before any zoom a camera asked for: what going in to a screen works its own zoom from (dock.mjs).
  let snow = null, snowAt = 0; // The room's falling snow.
  let orbitAngle = 0, spinAt = null, flightTurn = null;
  let orbitZoom = 1; // The curtain view's picture scale (below 1 shrinks it).
  let motes = null, motesAt = 0; // the air's own motes
  let rain = null, rainAt = 0; // The rain that falls when it rains in Los Angeles.
  let qrRect = null, qrURL = null; // The aimed object's code tag, a button.
  let pairAt = null; // 📱 The sign-in code bottom left (pair-view.mjs), a button too.
  let qrModel = null; // Retain the tag until its code or pixel dimensions change.
  // One frame fits every four-metre standing screen, including those whose
  // KidLisp program is still loading.
  const fingerBone = model(({ box }) => box(0, 0, 0.5, 0.28, 0.28, 1, 0x51e3ee, 0x269baa));
  const fingertip = model(({ box }) => box(0, 0, 0, 1, 1, 1, 0xffffff));
  const controllerStem = model(({ box }) => box(0, 0, 0.5, 0.18, 0.18, 1, 0x51e3ee));
  function drawHands(xr, eye) {
    const segment = (a, b, mesh) => {
      if (!a || !b) return;
      const dx = b.x-a.x, dy = b.y-a.y, dz = b.z-a.z, length = Math.hypot(dx, dy, dz);
      if (length < 0.001) return;
      projector.draw(mesh, eye.x+a.x, eye.y+a.y, eye.z+a.z, Math.atan2(dx, dz), -Math.atan2(dy, Math.hypot(dx, dz)), length);
    };
    const point = (p, size) => projector.draw(fingertip, eye.x+p.x, eye.y+p.y, eye.z+p.z, 0, 0, size);
    for (const hand of xr.hands) for (const chain of hand.fingers) {
      for (let i = 1; i < chain.length; i++) segment(chain[i-1], chain[i], fingerBone);
      const tip = chain.at(-1);
      if (tip) point(tip, 2 * tip.radius);
    }
    for (const ray of xr.rays) {
      const { origin: p, dir: d } = ray;
      const at = t => ({x: p.x+d.x*t, y: p.y+d.y*t, z: p.z+d.z*t});
      segment(p, at(0.13), controllerStem);
      if (ray.distance != null) point(at(Math.max(0, ray.distance-0.008)), 0.014);
    }
  }
  const standingFrame = model(({ box }) => {
    box(0, 1.525, 0.14, 4.3, 2.55, 0.16, 0x2a2d3a, 0x22252f, 0x2a2d3a);
    for (const x of [-1.6, 1.6]) box(x, 0.2, 0.14, 0.16, 0.4, 0.16, 0x22252f, 0x22252f, 0x22252f);
  });
  const distantModels = new WeakMap();
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
    paperLeft = 0, paperRight = 0, paperBottom = 0,
    grid = null; // The host's pixel grid, when the prompt is the paper over us.
  const orbitInset = 22; // Prompt pixels from the paper edge to the reticle center.
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
      paperLeft = Math.round((grid.left - left) / p);
      paperRight = Number.isFinite(grid.width) ? cols - paperLeft - grid.width : 0;
      paperBottom = Number.isFinite(grid.height)
        ? rows - Math.round((grid.top - top) / p) - grid.height : 0;
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
    // (Standing alone, a controller's L4 and R4 make it chunkier or sharper: density below.)
    const scale = Math.max(0.5, Math.min(8, step * (devicePixelRatio >= 1.5 ? 0.75 : 1) * 0.8 ** density));
    host.resize(Math.ceil(width / scale), Math.ceil(height / scale));
    aspect = width / height;
    pixelScale = scale;
  }
  const observer = new ResizeObserver(resize);
  // 🔬 The picture's own density when no prompt lays the grid: each step up is
  // a quarter more pixels across, each step down a fifth fewer; 0 is as it came.
  function setDensity(by) {
    density = by === 0 ? 0 : Math.max(-5, Math.min(5, density + Math.sign(by)));
    try { localStorage.setItem('rax-density', String(density)); } catch {}
    resize();
    return density;
  }
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
    if (e.at && e.type === 'gib') {
      // 🩸 The body comes down as a ragdoll or comes apart (gore.mjs), from how it last stood.
      const was = e.zombie ? null : lastPose.get(e.seat), fresh = was && performance.now() - was.at < 1500; // (a zombie shares a seat's colour, not its pose)
      gore.gib(e, fresh ? was : { x: e.at.x, base: e.at.y - 0.9, z: e.at.z, yaw: 0, pitch: 0 });
      burstColor(e.at.x, e.at.y + 0.6, e.at.z, e.kind === 'blast' ? 24 : 8, GIB);
    }
    if (e.at && e.type === 'damage') {
      burst(e.at.x, e.at.y, e.at.z, e.crit ? 14 : 8, e.crit ? 0.9 : 0.6);
      if (pops.length >= 32) pops.shift();
      // (Side by side when they land together, so a burst of hits still reads.)
      const near = pops.filter((p) => performance.now() - p.born < 200 && Math.hypot(p.x - e.at.x, p.z - e.at.z) < 1).length;
      pops.push({ x: e.at.x + (near % 3 - 1) * 0.35, y: e.at.y, z: e.at.z, label: String(e.amount) + (e.crit ? '!' : ''), crit: e.crit, attacker: e.attacker, born: performance.now() });
    }
    if (e.at && e.type === 'bop') burst(e.at.x, e.at.y, e.at.z, 14, 0.8);
    if (e.at && e.type === 'removed') burst(e.at.x, e.at.y, e.at.z, 24, 1.3);
    if (e.at && e.type === 'dust') burst(e.at.x, e.at.y, e.at.z, 3, 0.25);
    if (e.type === 'blast') {
      if (effects.length >= 24) effects.shift();
      effects.push({ at: e.at, born: performance.now() });
    }
  }
  // 🌑 While the sun is up, every model drawn standing in the world is also
  // noted with where it stands, so its boxes can cast shadows at the end of
  // the frame: the model, then x, y, z, yaw and scale, flat in one list.
  const casters = [];
  let casting = false;
  const SHADOW_BUDGET = 14000; // triangles of shadow volume in a frame, at most
  const SHADOW_SIGHT = 45; // metres from the eye a box still casts
  const SHADOW_MOST = 12000; // triangles in the frame the shadows may come to, once their long faces are halved for the lens
  const volumeFloats = new Float32Array(Math.max(VOLUME_TRIANGLES, SLAB_TRIANGLES) * 18); // one box's solid at a time
  const SOLIDS = typeof location !== 'undefined' && new URLSearchParams(location.search).has('volumes');
  // (?lens=0.3, ?planet=0: for looking at what each bend does to the picture.)
  const TRY = (() => { const q = typeof location !== 'undefined' ? new URLSearchParams(location.search) : new Map(); const n = (k) => (q.has(k) && Number.isFinite(Number(q.get(k))) ? Number(q.get(k)) : null); return { lens: n('lens'), planet: n('planet') }; })();
  const MOON_SHADE = [0.9, 0.91, 0.95]; // how dark the moon's shadows are, beside the sun's (shadow.mjs shadeFor)
  const SUN_AT = typeof location !== 'undefined' && new URLSearchParams(location.search).has('sun') ? Number(new URLSearchParams(location.search).get('sun')) : null; // (?sun=0.42: a fixed time of day to light by)
  let shadowReach = 45, reachCut = 0; // metres from the eye a box still casts now (SHADOW_SIGHT at the most), and the frame it was last drawn in
  const castLate = new WeakMap(); let castFrame = 0; // boxes whose solid did not fit the frame, and the frame each may be tried again
  // The boxes that cast this frame: how far each is, which thing it is part of, the box, its top; and the order they are cast in.
  const castFar = new Float32Array(Math.floor(SHADOW_BUDGET / VOLUME_TRIANGLES)), castFrom = new Int32Array(castFar.length), castTop = new Float32Array(castFar.length), castBox = [], castOrder = [];
  // 🫧 A seam: the line where one room of the world hands a walker to the
  // next. A net of threads
  // you can see through: posts that stand and rungs that climb. Cyan at
  // rest, amber while your body is between the two rooms, dim on the blocks
  // around.
  const SEAM = { rest: 0x4ff0ff, crossing: 0xffb000, far: 0x2f8f9c }, SEAM_STEP = 0.5, SEAM_HEIGHT = 3;
  const seams = new Map();
  const seamFor = (ink, part) => {
    const key = `${ink} ${part}`;
    if (!seams.has(key))
      seams.set(key, model(({ quad }) => {
        const both = (a, b, c, d) => { quad(a, b, c, d, ink); quad(d, c, b, a, ink); };
        const t = 0.02, W = SPAN / 2;
        if (part === 'posts')
          for (let x = -W; x <= W + 0.001; x += SEAM_STEP) both([x - t, 0, 0], [x + t, 0, 0], [x + t, SEAM_HEIGHT, 0], [x - t, SEAM_HEIGHT, 0]);
        else
          for (let y = 0; y < SEAM_HEIGHT - 0.001; y += SEAM_STEP) both([-W, y, 0], [W, y, 0], [W, y + 2 * t, 0], [-W, y + 2 * t, 0]);
      }));
    return seams.get(key);
  };
  const drawSeams = (neighbours, dx, dy, dz, ink, now, skip = null) => {
    const climb = ((now / 2400) % 1) * SEAM_STEP;
    for (const [side, sx, sz] of [['east', 1, 0], ['west', -1, 0], ['north', 0, 1], ['south', 0, -1]]) {
      if (!neighbours?.[side] || skip?.(sx, sz)) continue;
      const x = dx + sx * SPAN / 2, z = dz + sz * SPAN / 2, turn = sx ? Math.PI / 2 : 0;
      projector.draw(seamFor(ink, 'posts'), x, dy, z, turn);
      projector.draw(seamFor(ink, 'rungs'), x, dy + climb, z, turn);
    }
  };
  // ⚽ The ball: a small sphere in black and white patches, turned by how
  // far it has rolled.
  let ballModel = null, litBall = null, ballSeen = null;
  const ballNormals = [];
  const ballCasters = [];
  const ballVolume = model(({ tri, quad }) => {
    const r = BALL.r * 0.94, end = -70;
    for (let i = 0; i < 12; i++) {
      const a = i * Math.PI / 6, b = (i + 1) * Math.PI / 6;
      const at = (t, y) => [r * Math.cos(t), y, r * Math.sin(t)];
      const A = at(a, 0), B = at(b, 0), C = at(b, end), D = at(a, end);
      quad(A, B, C, D, 0);
      tri([0, 0, 0], B, A, 0);
      tri([0, end, 0], D, C, 0);
    }
  });
  const ballRoll = new Map();
  const dispenserModel = model(({ box }) => {
    const { width, height, depth } = DISPENSER;
    box(0, height / 2, 0, width, height, depth, 0x26adc2, 0x185a6b, 0xc7f5ed);
    box(-width / 2, 0.55, 0, 0.06, 0.78, 0.72, 0x142329, 0x142329, 0x27404b);
  });
  // 🛵 A cabin bike (bike.mjs): a long low body on two hover wheels, a glass
  // cabin of four posts and a roof over the seat, and down each side under
  // the windows a panel wearing one of the rider's #paintings
  // (bike-decor.mjs). Its origin is the rider's feet; it faces +z. Made once
  // a rider and look, and kept.
  const bikeModels = new Map();
  function bikeFor(key, body, sides) {
    if (bikeModels.has(key)) return bikeModels.get(key);
    const made = model(({ box, quad }) => {
      const trim = tint(body, 0.55), glass = 0x9fd8ff, dark = 0x1d2230, keel = -BIKE.seat;
      box(0, keel + 0.22, 0, 0.62, 0.3, 1.9, body, tint(body, 0.8), tint(body, 1.1)); // the body
      box(0, keel + 0.42, 0.98, 0.5, 0.5, 0.26, body, tint(body, 0.8)); // the nose
      box(0, keel + 0.4, -0.98, 0.56, 0.42, 0.3, trim, trim); // the tail
      box(0, keel + 0.5, 1.12, 0.3, 0.12, 0.04, 0xfff1b0, 0xfff1b0, 0xfff1b0); // the lamp
      for (const z of [-0.78, 0.78]) { // the wheels, hovering, a glow under each
        box(0, keel + 0.08, z, 0.16, 0.52, 0.52, dark, dark, 0x3a4256);
        box(0, keel - 0.2, z, 0.14, 0.02, 0.4, 0x7ff6ff, 0x7ff6ff, 0x7ff6ff);
      }
      box(0, 0.02, -0.2, 0.42, 0.1, 0.5, 0x2b2f3d, 0x2b2f3d); // the seat
      for (const x of [-0.34, 0.34]) for (const z of [-0.62, 0.62])
        box(x, 0.92, z, 0.05, 1.82, 0.05, trim, trim); // the cabin's posts
      box(0, 1.85, 0, 0.74, 0.06, 1.3, body, tint(body, 0.8)); // its roof
      box(0, 1.05, 0.64, 0.66, 0.03, 0.02, glass, glass, glass); // the windscreen's rail
      // The side panels, under the windows: a painting each, or the body's own colour.
      const y0 = keel + 0.4, y1 = 0.62, z0 = -0.6, z1 = 0.6;
      for (const sx of [1, -1]) {
        const x = sx * 0.33, cells = sides[sx > 0 ? 0 : 1] || sides[0];
        box(x - sx * 0.02, (y0 + y1) / 2, 0, 0.03, y1 - y0, z1 - z0, trim, trim);
        if (!cells) continue;
        const X = x + sx * 0.003, dz = (z1 - z0) / DECOR_COLS, dy = (y1 - y0) / DECOR_ROWS;
        for (let r = 0; r < DECOR_ROWS; r++)
          for (let c = 0; c < DECOR_COLS; c++) {
            const ink = cells[r][c];
            if (ink === null) continue;
            // Read from outside: left to right is -z on the +x side and +z on the -x side.
            const za = sx > 0 ? z1 - (c + 1) * dz : z0 + c * dz, zb = za + dz;
            const ya = y1 - (r + 1) * dy, yb = ya + dy;
            if (sx > 0) quad([X, ya, za], [X, ya, zb], [X, yb, zb], [X, yb, za], ink);
            else quad([X, ya, zb], [X, ya, za], [X, yb, za], [X, yb, zb], ink);
          }
      }
    });
    bikeModels.set(key, made);
    return made;
  }
  function drawBike(p, i, base, now, handle) {
    const decor = decorFor(handle);
    const body = decor?.body ?? seatColor(i);
    const key = `${handle || 'guest'}:${body}:${decor?.version || 0}`;
    const yawR = (p.yaw * Math.PI) / 180, ahead = direction(p.yaw), side = direction(p.yaw - 90);
    const along = (p.vx || 0) * ahead.x + (p.vz || 0) * ahead.z, across = (p.vx || 0) * side.x + (p.vz || 0) * side.z;
    const lean = Math.max(-0.35, Math.min(0.35, -(p.vy || 0) * 0.03 + along * 0.004));
    const bank = Math.max(-0.4, Math.min(0.4, across * 0.03));
    const bob = Math.sin(now / 420 + i) * 0.03;
    draw(bikeFor(key, body, decor?.sides || []), p.x, base + bob, p.z, yawR, lean, 1, -bank);
  }
  // 🚁 A looker-on's drone: a small body, four arms to four rotors (two
  // frames of them, turned, for the blur), a light at the nose.
  const droneModels = [];
  const droneFor = (frame) => (droneModels[frame] ||= model(({ box, quad }) => {
    const shell = 0x2a2d3a, arm = 0x4a4f63, blade = frame ? 0xcfd6ea : 0x9aa3bd;
    box(0, 0, 0, 0.26, 0.09, 0.34, shell, tint(shell, 0.8), tint(shell, 1.2));
    box(0, 0.02, 0.18, 0.08, 0.05, 0.04, 0x00ffff, 0x00ffff, 0x00ffff); // the eye, at the nose
    for (const [sx, sz] of [[-1, -1], [1, -1], [1, 1], [-1, 1]]) {
      const x = sx * 0.24, z = sz * 0.24;
      box(x / 2, 0.01, z / 2, 0.05, 0.03, 0.05, arm, arm, arm);
      box(x, 0.03, z, 0.06, 0.07, 0.06, arm, tint(arm, 0.8), arm);
      // The rotor: a thin cross, turned an eighth between the two frames.
      const a = frame ? Math.PI / 4 : 0, R = 0.15, w = 0.025, top = 0.075;
      for (const t of [a, a + Math.PI / 2]) {
        const c = Math.cos(t), s = Math.sin(t);
        const p = (u, v) => [x + c * u - s * v, top, z + s * u + c * v];
        quad(p(-R, -w), p(R, -w), p(R, w), p(-R, w), blade);
        quad(p(-R, w), p(R, w), p(R, -w), p(-R, -w), blade);
      }
    }
  }));
  // (A football proper: twelve black pentagons and twenty white hexagons, an
  // icosahedron with its corners cut, each corner pushed out to the sphere.)
  const ballFor = () => (ballModel ||= model(({ quad }) => {
    const R = BALL.r, F = (1 + Math.sqrt(5)) / 2, corners = [];
    for (const a of [-1, 1]) for (const b of [-F, F]) corners.push([0, a, b], [a, b, 0], [b, 0, a]);
    const sub = (p, q) => [p[0] - q[0], p[1] - q[1], p[2] - q[2]], dot = (p, q) => p[0] * q[0] + p[1] * q[1] + p[2] * q[2];
    const cross = (p, q) => [p[1] * q[2] - p[2] * q[1], p[2] * q[0] - p[0] * q[2], p[0] * q[1] - p[1] * q[0]];
    const out = (p) => { const d = Math.hypot(p[0], p[1], p[2]); return [(p[0] / d) * R, (p[1] / d) * R, (p[2] / d) * R]; };
    const third = (p, q) => [p[0] + (q[0] - p[0]) / 3, p[1] + (q[1] - p[1]) / 3, p[2] + (q[2] - p[2]) / 3];
    const near = (i) => corners.map((_, j) => j).filter((j) => j !== i && Math.abs(Math.hypot(...sub(corners[i], corners[j])) - 2) < 0.01);
    // A face's corners in order round its middle, then as a fan of quads.
    const face = (points, ink) => {
      const mid = points.reduce((m, p) => [m[0] + p[0], m[1] + p[1], m[2] + p[2]], [0, 0, 0]);
      const u = sub(points[0], mid.map((v) => v / points.length)), n = out(mid), v = cross(n, u);
      const ring = points.map((p) => { const d = sub(p, mid.map((m) => m / points.length)); return [Math.atan2(dot(d, v), dot(d, u)), out(p)]; }).sort((p, q) => p[0] - q[0]).map(([, p]) => p);
      for (let k = 1; k + 1 < ring.length; k += 2) {
        const c = ring[k + 2] || ring[k + 1];
        quad(ring[0], ring[k], ring[k + 1], c, ink);
        quad(c, ring[k + 1], ring[k], ring[0], ink);
        for (let i = 0; i < 4; i++) ballNormals.push(n[0] / R, n[1] / R, n[2] / R);
      }
    };
    corners.forEach((c, i) => face(near(i).map((j) => third(c, corners[j])), 0x16181d));
    for (let i = 0; i < 12; i++) for (const j of near(i)) for (const k of near(i))
      if (i < j && j < k && near(j).includes(k))
        face([[i, j], [j, i], [j, k], [k, j], [k, i], [i, k]].map(([p, q]) => third(corners[p], corners[q])), 0xf4f4f0);
  }));
  // 🟦 A grid of pixels (RGBA) as a model: `wide` by `tall` metres about x = 0,
  // its foot at `base`, facing -z; a row's run of one colour is one quad. The
  // standing KidLisp screens and the world panels are both drawn with it.
  // The glass picks up the sky at grazing angles. Quantize its reflection
  // so a still piece can keep its mesh as the camera drifts slightly.
  function glassAt(eye, x, y, z, yaw, wide, sky, id, zoom) {
    const dx = eye.x - x, dz = eye.z - z, dy = eye.y - y;
    const distance = Math.hypot(dx, dy, dz);
    if (distance > wide * 8 * zoom || distance < 0.001) return null;
    const side = Math.round(12 * (dx * Math.cos(yaw) - dz * Math.sin(yaw)) / distance);
    const height = Math.round(10 * dy / distance);
    const ink = blendInk(sky, 0xc5d7e4, 0.45);
    let seed = 0;
    for (const ch of id) seed = (31 * seed + ch.charCodeAt(0)) >>> 0;
    return { key: `${side}/${height}/${ink}`, side, height, ink, seed };
  }
  function gridRuns(px, cols, rows, glass, drawRun) {
    for (let j = 0; j < rows; j++) {
      const v = (j + 0.5) / rows;
      const band = glass ? Math.round(cols * (0.42 + glass.side / 28 + 0.28 * (v - 0.5))) : cols;
      const bandEnd = band + Math.max(1, Math.round(cols * 0.085));
      const horizon = glass ? 0.5 - glass.height / 12 : 0;
      const reflected = glass ? (v < horizon ? 0.035 : 0.01) + Math.abs(glass.side) * 0.003 : 0;
      // One forked hairline at a fixed edge of each pane. Its position
      // follows the glass when resolution or viewing angle changes.
      let crack = cols, fork = cols;
      if (glass && v < 0.21 && cols >= 32) {
        const edge = (glass.seed % 13 + 5) / 100;
        const zig = v < 0.08 ? v * 0.48 : 0.038 - (v - 0.08) * 0.23;
        const col = u => Math.floor(cols * (glass.seed % 2 ? u : 1 - u));
        crack = col(edge + zig);
        if (v > 0.08 && v < 0.14) fork = col(edge + 0.038 + (v - 0.08) * 0.74);
      }
      const cuts = glass ? [band, bandEnd, crack, crack + 1, fork, fork + 1]
        .filter(x => x > 0 && x < cols).sort((a, b) => a - b) : [];
      let cut = 0;
      for (let i = 0; i < cols;) {
        while (cut < cuts.length && cuts[cut] <= i) cut++;
        const end = cuts[cut] ?? cols;
        const k = (j * cols + i) * 4;
        let color = (px[k] << 16) | (px[k + 1] << 8) | px[k + 2];
        if (glass) {
          const chipped = i === crack || i === fork;
          color = blendInk(color, chipped ? 0xccdae2 : glass.ink,
            chipped ? 0.34 : reflected + (i >= band && i < bandEnd ? 0.085 : 0));
        }
        let next = i + 1;
        for (; next < end; next++) {
          const q = (j * cols + next) * 4;
          if (px[q] !== px[k] || px[q + 1] !== px[k + 1] || px[q + 2] !== px[k + 2]) break;
        }
        drawRun(i, next, j, color);
        i = next;
      }
    }
  }
  const gridModel = (px, cols, rows, wide, tall, base, glass) => screen(model(({ quad }) => {
    const cw = wide / cols, ch = tall / rows;
    // A vertical stretch of identical horizontal runs is one face. Preserve
    // every pixel edge while avoiding two triangles per row of a piano key.
    const active = new Array(cols);
    const emit = (i, run) => {
      const u0 = wide / 2 - i * cw, u1 = wide / 2 - run.end * cw;
      const top = base + tall - run.first * ch, bottom = base + tall - run.after * ch;
      quad([u0, bottom, 0], [u1, bottom, 0], [u1, top, 0], [u0, top, 0], run.color);
    };
    gridRuns(px, cols, rows, glass, (i, end, j, color) => {
      const prev = active[i];
      if (prev && prev.end === end && prev.color === color && prev.after === j) prev.after++;
      else {
        if (prev) emit(i, prev);
        active[i] = { end, color, first: j, after: j + 1 };
      }
    });
    active.forEach((run, i) => emit(i, run));
  }));
  const panelModels = new Map(); // a world panel's face and slab, by its id
  const draw = (made, x = 0, y = 0, z = 0, yaw = 0, pitch = 0, scale = 1, roll = 0) => {
    // 🪐 What stands on a hung planet is drawn where the planet is drawn, as it is (not spread round the land's ball).
    if (hungNow.length && projector.bends && projector.planet)
      for (const m of hungNow)
        if ((x - m.x) ** 2 + (y - m.y) ** 2 + (z - m.z) ** 2 < m.hold * m.hold) {
          projector.bends = false;
          projector.draw(made, x + m.sx, y + m.sy, z + m.sz, yaw, pitch, scale, roll);
          projector.bends = true;
          return;
        }
    projector.draw(made, x, y, z, yaw, pitch, scale, roll);
    if (casting && !pitch && !roll && boxesOf(made)) casters.push(made, x, y, z, yaw, scale);
  };
  function render({
    xr = null, headset = false, headsetControls = null,
    players = [],
    me,
    camera,
    spectator = false,
    idleCamera = null, idleEffect = 0,
    gate = null, // 'account' | 'handle': what a visitor needs before a seat.
    pair = null, // 📱 { url, number }: a code a phone signs this tab in with, while looking on signed out
    controller,
    seat = 0,
    third = false,
    drones = [], // 🤖 the game's dumb robots: { id, x, y, dir }
    game = null, // 🎮 the room's gametype (games.mjs): its ledges, legend and what it hides
    bops = [], // bops landed, by seat
    combos = [], // 🔗 a chain in the air, by seat: [how many, worth] or 0
    debug = false, // 🔲 a game's hitboxes and numbers
    boomLength = 4, // how far back the third-person camera sits
    shoulder = 1, // which shoulder it looks over: 1 right, -1 left, 0 straight behind
    rockets = [],
    status = '',
    members = [],
    self = null,
    connection,
    joinURL,
    objects = [],
    prompt,
    radial = null,
    items = null,
    bullets = [],
    since = 0,
    holding = null,
    touch = null,
    touchButtons = [],
    input = 'keys',
    pointerSource = 'mouse', handlePoint = null,
    devLog = [],
    stallNote = '',
    chat = null,
    tower = null, // 🏢 { count, top, here: { number, handle, colors }, lift } where a car stands
    skyline = null, // 🏢 the tower seen from this room: { top, dx, dy, dz } (dy is the plaza's height from here)
    car = false, // standing in the elevator
    plainTags = false, // 🎬 a film: names over heads, no positions after them
    ride = null, // { from, to, handle, age } while the elevator runs
    walls = [],
    cube = [],
    menu = null,
    removeArmed = null,
    lockedOn = false,
    lockCursor = null,
    screenCursor = null,
    nearZoom = 1, // 🔎 the focus zoom (client.mjs): the lens closed on what is looked at
    docked = false,
    notices = [],
    braincells = null,
    spending = false,
    served = null,
    roomName = null,
    battery = null, // 🔋 { level 0…1, charging } where the browser says (client.mjs), else null
    tiles = {},
    park = null, basement = false,
    field = null, // 🌳 this room's cell on the tower's grounds ({ i, j }), or null
    inspectScenery = false,
    around = [], // the open blocks beside this one: { id, dx, dz, neighbours, plaza, objects, tiles }
    seam = null, // 🫧 { crossing, shown }: the body is between two rooms; shown: draw the rooms' lines as walls (rax?seams)
    sunAt = null, shadows = true, // 🌑 a fixed sun and a switch, for measuring shadows
    gauge = null, // 🚁 { mph, rpm, feet }: a drone's dials, bottom centre
    agents = 0, // 🤖 how many agents are linked to your handle (lib/agent-presence's room): a small mark after your name
    stats = false, // Shared frame chart and Rax diagnostics.
    sticks = null, // 🎮 { move: [x, y], look: [x, y], buttons: [bool] } from -1 to 1: a controller's two sticks and what is pressed, shown while looking on
    place = null, // 📍 { x, z }: this room's place in the world; your position is shown from the plaza's middle
    eyes = [], // 🚁 whoever looks on, each a small drone where their eye is: { name, x, y, z, yaw, vx, vz }
    beyond = [], // 👀 the people in the rooms around this one, in this room's metres: { name, seat, x, y, z, yaw, pitch, vx, vz }
    hung = [], // 🪐 the planets hung over home, in this room's metres (orbits.mjs hungFor): { id, x, y, z, r, ink, dark }
    weather = null, // 🌧️ { rain, snow, cloud, wind }: what falls over Los Angeles (weather.mjs)
    balls = [], courtyard = null,
    home = null,
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
    decay = 0, // 📉 0 to 1: the server is gone and the picture is coming apart into blocks and bits
    chunk = 0, // 🧱 0 to 1: the room not yet spoken (a drop, a rejoin): coarse blocks, pieces still missing
    fuzz = 0, // 📶 0 to 1: the line is shoddy: bands of the picture tear sideways
    wire = null, // 📟 { text, color }: one line of the connection's story, top left, while the room is still up (the server quiet, or just back on new code)
    offline = false, // The sheet is open and the connection is down: the log panel, nothing else.
    jobs = [],
    builds = [], // 🧱 this page's own builds, each with its state (client.mjs buildRows): { state, text, note, phase, parts, seconds, tries, name, where, version }
    worldTime = 0,
    locked = false,
    flash = null, // ⎋ { color, at }: a step of Escape just taken, shown around the screen
    legend = false,
    held = {},
    corner = false,
    screens = [],
    screenPrograms = new Map(),
    panels = [], // 🖼️ world panels (panel.mjs): { id, x, y, z, yaw, w, h, deep, cols, rows, pixels, frame, cursor: { x, y, down } | null }
    bare = false, // Behind the prompt's curtain: no panel, hint or legend.
    half = WORLD, // 📐 This room's half-width (world.mjs halfOf).
  } = {}) {
    if (lost) {
      location.reload();
      return;
    }
    projector.immersive = !!xr;
    projector.marks = !xr;
    const ledges = game?.ledges || null;
    const start = performance.now(),
      now = start;
    const parked = !!grid && bare && orbit === 1 && !reticle;
    const solidObjects = objects.filter((o) => !o.item);
    lastWorldTime = worldTime;
    // 👀 Looking on, the connection's meter stays away: it sat over the controls.
    if (spectator) connection = null;
    // The corner join code is retired for now: rax is joining the AC homepage.
    const cells = null;
    const hud = hudLayout(canvas.width, canvas.height, cells, pixelScale, grid ? 1 : 0);
    const flashAge = flash ? (now - flash.at) / 360 : 1;
    const bump = flashAge >= 0 && flashAge < 1 ? Math.round(7 * (1 - flashAge) ** 2) * hud.unit : 0;
    hud.safeX += bump;
    hud.safeY += bump;
    const modeIcon = idleCamera ? 'cinematic' : spectator && !bare ? 'spectator' : null;
    const hudPack = createPack(canvas.width, canvas.height, { safeX: 4 + bump, safeY: 4 + bump, gap: 2 * hud.unit });
    const modeBox = modeIcon ? hudPack.reserve(modeIcon, 'tr', 11 * hud.unit, 9 * hud.unit) : null;
    function hudOverlay(id, mesh, width, height) {
      if ((camera?.face || stats) && mesh.length) {
        let bounds = overlayBounds.get(mesh);
        if (!bounds) {
          let l = Infinity, t = Infinity, r = -Infinity, b = -Infinity;
          for (let i = 0; i < mesh.length; i += 6) {
            const x = -mesh[i], y = -mesh[i + 1];
            l = Math.min(l, x); r = Math.max(r, x); t = Math.min(t, y); b = Math.max(b, y);
          }
          bounds = { x: l, y: t, w: r-l, h: b-t };
          overlayBounds.set(mesh, bounds);
        }
        hudPack.claim(id, { ...bounds, x: width / 2 + bounds.x, y: height / 2 + bounds.y });
      }
      projector.overlay(mesh, width, height);
    }
    let chartTop = 21 * hud.unit + bump;
    const promptX = grid ? 6 + bump : hud.safeX, promptY = grid ? 6 + bump : hud.safeY;
    const columns = Math.max(1, Math.floor((canvas.width - 2 * promptX) / (6 * hud.unit)));
    const buildLines = bare || !builds.length ? [] : buildTextLines(builds, columns, now);
    const buildWidth = Math.max(0, ...buildLines.map((row) => spanWidthOf(row, hud.unit)));
    let promptRows = 0;
    // 🌞 In the world the room follows the world's own day (sky.mjs), the
    // same for everyone; behind the prompt's curtain it keeps the page's
    // theme, and a game arena its own.
    const skyPhase = Number.isFinite(sunAt) ? sunAt : SUN_AT ?? phase();
    const skyStep = !ledges ? stepOf(skyPhase) : -1;
    const today = skyStep >= 0 ? skyAt(skyStep / STEPS) : null;
    dress(today ? (today.day ? 'day' : 'night') : paletteFor());
    sky = [...rgb(orbit === 1 ? PALETTES[paletteFor()].sky : today ? today.horizon : PALETTES[paletteName].sky), 1];
    textures.tick(performance.now()); // 🖼️ step what runs, send what changed (before anything asks for a layer)
    host.beginFrame();
    // 🌑 Shadows follow the sun itself, not the sky's three-second pictures.
    // (`sunAt`: a time of day to light by, 0 to 1, for looking at shadows; `shadows: false` casts none.)
    const sunNow = today ? skyAt(skyPhase).sun : null;
    // 🌙 With the sun down the moon casts (it stands across the sky from the sun: sky.mjs), and its shadows are faint: the night is lit a little all over.
    const lamp = !sunNow ? null : sunNow.y >= 0 ? sunNow : { x: -sunNow.x, y: -sunNow.y, z: -sunNow.z };
    const shade = lamp && !headset && !xr && host.shadows && shadows && orbit < 0.6 ? shadeFor(lamp.y, lamp === sunNow ? undefined : MOON_SHADE) : null;
    casters.length = 0;
    casting = !!shade;
    let eye = { x: 15, y: 19, z: -22 },
      aim = { x: 0, y: 0, z: 0 };
    let through = false; // The third-person camera, looking through what hides you.
    if (me) {
      const dir = direction(me.yaw, me.pitch),
        origin = { x: me.x, y: me.y, z: me.z };
      eye = origin;
      if (third) {
        const back = { x: -dir.x, y: -dir.y + 0.22, z: -dir.z },
          len = Math.hypot(back.x, back.y, back.z);
        for (const k of ['x', 'y', 'z']) back[k] /= len;
        // 👻 The camera keeps its distance and goes through whatever is
        // behind you: a wall or a ceiling there is looked through (see
        // `projector.look` below), never a reason to rush the camera in
        // (2026-11-18). Only the ground you stand on holds it up, by sliding.
        boom += (boomLength - boom) * 0.2;
        eye = along(origin, back, boom);
        // 🎥 Over a shoulder, not straight behind: the camera stands to one
        // side, so the body is off the middle and what is aimed at is not
        // behind its head. It still looks where the body looks (`aim`
        // below), so the two sightlines meet out in front. Nearer in, less
        // to the side, so a close camera does not leave the body behind.
        const level = Math.hypot(dir.x, dir.z);
        if (shoulder && level > 1e-4) {
          const side = shoulder * Math.min(1.1, 0.26 * boom);
          eye.x += (-dir.z / level) * side;
          eye.z += (dir.x / level) * side;
        }
        eye.y = Math.max(eye.y, me.y - CFG.eyeHeight + 0.3);
        through = true;
      }
      // Keep the camera on the player's sightline through open block edges.
      // Picking a surface is a separate ray; its distance must not steer the camera.
      aim = along(origin, dir, 20);
    }
    if (camera) {
      eye = camera;
      aim = along(camera, direction(camera.yaw, camera.pitch), 100);
    }
    if (idleCamera) {
      const dx = aim.x - eye.x, dy = aim.y - eye.y, dz = aim.z - eye.z;
      const play = { ...eye, yaw: Math.atan2(dx, dz) * 180 / Math.PI, pitch: Math.atan2(dy, Math.hypot(dx, dz)) * 180 / Math.PI };
      camera = { ...glide(play, idleCamera, idleCamera.weight), cinematic: true, weight: idleCamera.weight };
      eye = camera;
      aim = along(eye, direction(camera.yaw, camera.pitch), 100);
    }
    // Behind the curtain the room turns slowly, seen from far off and above;
    // the drop flies the camera down a curve that passes behind the player's
    // head and into their eyes (2026-11-03). `orbit` eases 1 → 0 on the drop.
    // The room turns small in the bottom-right corner, right under the
    // curtain's crosshair (a lens shift puts the room's centre there), so the
    // curtain stays mostly 2D and the corner is the way in (2026-11-03).
    projector.shiftX = projector.shiftY = 0;
    const ground = skyline?.dy ?? (basement ? DEPTH : 0);
    const landing = 1 - orbit;
    const landed = landing * landing * (3 - 2 * landing);
    if (orbit === 0 || orbit === 1) flightTurn = null;
    if (orbit > 0) {
      // A late frame or a resumed tab should advance at most one visible step.
      if (spinAt !== null) orbitAngle += Math.min(50, Math.max(0, now - spinAt)) * (1 - landed) / 3200;
      const a = orbitAngle;
      // Fit the entire planetary limb under the cursor, including a margin
      // at maximum hover. Use its apparent radius from this camera height.
      const tan = aspect < 1 ? Math.min(Math.sqrt(3), 1 / aspect) : 1;
      const height = PLANET * 10;
      const natural = PLANET / Math.sqrt(height * (height + 2 * PLANET)) * canvas.height / (2 * tan);
      orbitZoom = Math.pow((15 * cornerScale) / natural, (1 - landed));
      const far = { x: Math.sin(a) * PLANET, y: ground + height, z: -Math.cos(a) * PLANET };
      const px = canvas.width / Math.max(1, canvas.clientWidth);
      // The crosshair's centre sits 22 prompt pixels in from the corner; on
      // the host's grid a buffer pixel is a prompt pixel.
      const inset = grid ? orbitInset : orbitInset * px;
      const cornerX = 1 - 2 * (inset + (grid ? paperRight : 0)) / canvas.width;
      const cornerY = 2 * (inset + (grid ? paperBottom : 0)) / canvas.height - 1;
      projector.shiftX = cornerX * (1 - landed);
      projector.shiftY = cornerY * (1 - landed);
      const centre = { x: far.x * 0.98, y: ground - PLANET, z: far.z * 0.98 };
      const me0 = me || flyTo || camera; // Before the seat is granted, fly toward the spawn.
      if (!me0 || orbit >= 1) {
        eye = far;
        aim = centre;
      } else {
        const e = landed;
        const f = direction(me0.yaw, 0);
        const behind = { x: me0.x - f.x * 3.2, y: me0.y + 1.4, z: me0.z - f.z * 3.2 };
        if (!me && flyTo && (!camera || !spectator)) {
          // No seat yet: the eye and aim the seat will have.
          eye = { x: me0.x, y: me0.y, z: me0.z };
          aim = along(eye, direction(me0.yaw, me0.pitch || 0), 10);
        }
        const nearYaw = Math.atan2(aim.x - eye.x, aim.z - eye.z);
        // The overhead eye's heading is -a. Keep the chosen arc across the
        // +/-pi seam, including when the paper reverses mid-flight.
        const rawTurn = nearYaw + a;
        const tau = 2 * Math.PI;
        flightTurn = rawTurn - tau * Math.round((rawTurn - (flightTurn ?? 0)) / tau);
        const heading = -a + e * flightTurn;
        const bez = (p, q, r) => (1 - e) * (1 - e) * p + 2 * (1 - e) * e * q + e * e * r;
        eye = { x: bez(far.x, behind.x, eye.x), y: bez(far.y, behind.y, eye.y), z: bez(far.z, behind.z, eye.z) };
        // Follow the landing point in elevation. Heading stays defined even
        // when the sightline passes directly beneath the camera.
        const targetX = centre.x + (aim.x - centre.x) * e;
        const targetZ = centre.z + (aim.z - centre.z) * e;
        const reach = Math.max(0.0001, Math.hypot(targetX - eye.x, targetZ - eye.z));
        aim = { x: eye.x + Math.sin(heading) * reach, y: centre.y + (aim.y - centre.y) * e, z: eye.z + Math.cos(heading) * reach };
      }
    }
    spinAt = orbit > 0 ? now : null;
    const roll = (camera?.roll || 0) * landed;
    // Keep the idle orbital motion out of the player's walk and the drone's controls.
    if (orbit > 0) {
      const breath = Math.sin(now / 1900) * 0.03 * orbit;
      eye = { ...eye, y: eye.y + breath };
      aim = { ...aim, y: aim.y + breath };
    }
    projector.camera(eye, aim, aspect, roll);
    // 👻 Head, chest, feet and both shoulders: what the eye must still see.
    if (((through && !camera) || camera?.cinematic) && orbit <= 0) {
      const r = projector.right, feet = me.y - CFG.eyeHeight;
      projector.look([
        { x: me.x, y: me.y, z: me.z }, { x: me.x, y: feet + 1.1, z: me.z }, { x: me.x, y: feet + 0.15, z: me.z },
        { x: me.x + r[0] * 0.35, y: feet + 1.2, z: me.z + r[2] * 0.35 }, { x: me.x - r[0] * 0.35, y: feet + 1.2, z: me.z - r[2] * 0.35 },
      ]);
    } else projector.look();
    if (camera?.zoom) {
      projector.sx *= camera.zoom;
      projector.sy *= camera.zoom;
    }
    if (orbit > 0 && orbitZoom !== 1) {
      projector.sx *= orbitZoom;
      projector.sy *= orbitZoom;
    }
    orbitZoom = 1;
    // At walking height the source coordinates are already a local map in metres.
    // Ease in both the planet and the wide-angle lens as the camera leaves it.
    const curvature = eye.y < ground || basement ? 1 : viewCurvature(eye.y - ground, orbit);
    projector.decay = decay;
    projector.chunk = chunk;
    projector.fuzz = fuzz;
    projector.decayAt = now;
    projector.card = !xr;
    projector.under = projector.card ? 1 : 0;
    // 🪐 How much each hung planet is the place the eye is at: 1 on it and close by, 0 from 30 m off.
    const hungNear = hung.map((b) => { const t = Math.min(1, Math.max(0, (Math.hypot(b.x - eye.x, b.y - eye.y, b.z - eye.z) - b.r - 6) / 24)); return 1 - t * t * (3 - 2 * t); });
    const onHung = Math.max(0, ...hungNear);
    projector.lens = game?.camera || !projector.card || orbit > 0 ? 0 : TRY.lens ?? LENS * curvature * (1 - onHung); // (on a planet the lens is a walker's)
    if (camera?.flat) projector.lens *= 1 - camera.flat;
    if (projector.lens) {
      projector.sx *= 1 + (LENS_ZOOM - 1) * projector.lens;
      projector.sy *= 1 + (LENS_ZOOM - 1) * projector.lens;
    }
    natural = { sx: projector.sx / (camera?.zoom || 1), sy: projector.sy / (camera?.zoom || 1) };
    if (camera?.face && orbit <= 0) {
      // (A standing screen has a casing round its picture: the casing is what is fitted, so no edge of it runs under the HUD.)
      const W = canvas.width, H = canvas.height, w = camera.face.fw || camera.face.w, h = camera.face.fh || camera.face.h;
      const layoutKey = JSON.stringify([W, H, w / h, hud.unit, previousRegions]);
      if (layoutKey !== frameLayoutKey) {
        frameLayoutKey = layoutKey;
        screenFrame = fitHudScreen(W, H, w / h, previousRegions, 3 * hud.unit);
      }
      if (screenFrame) {
        const f = camera.flat, box = screenFrame;
        const scale = box.w * 2 * camera.back / (W * natural.sx * camera.dockZoom * w);
        projector.sx *= 1 + f * (scale - 1);
        projector.sy *= 1 + f * (scale - 1);
        projector.shiftX = f * ((2 * box.x + box.w) / W - 1);
        projector.shiftY = f * (1 - (2 * box.y + box.h) / H);
      }
    } else { screenFrame = null; frameLayoutKey = ''; }
    // Zoom changes the apparent distance used for detail as well as the projection.
    const zoom = orbit <= 0 ? nearZoom : 1;
    projector.sx *= zoom;
    projector.sy *= zoom;
    if (xr) {
      eye = { x: eye.x + xr.offset[0], y: eye.y + xr.offset[1], z: eye.z + xr.offset[2] };
      Object.assign(projector, { eye, forward: xr.forward, right: xr.rightAxis, up: xr.up,
        sx: xr.cullScale, sy: xr.cullScale, shiftX: 0, shiftY: 0 });
    }
    // (The flat picture the card bends must reach as far out as the screen's corners look: so much wider, and so many more pixels that its middle is as sharp.)
    projector.stretch = projector.card ? lensWide(projector.sx, projector.sy, projector.lens) : 1;
    // Resolve the far globe into paper pixels before the canvas is enlarged.
    // Sample fine world edges at higher resolution as the lens closes. Keep
    // the HUD on its own grid and cap the existing GPU targets at 2×.
    const over = Math.min(2, Math.max(1 / projector.stretch, Math.round(zoom * 2) / 2));
    lensNow = projector.card ? { idle: idleEffect, sx: projector.sx, sy: projector.sy, wide: projector.stretch, lens: projector.lens, over, fovea: parked || headset ? 0 : FOVEA, aa: orbit > 0.6 ? 2 : AA, dither: Math.max(0, (orbit - 0.6) / 0.4) } : null;
    if (!projector.card) {
      const grade = idleEffect ? `contrast(${1 + 0.08 * idleEffect}) saturate(${1 - 0.06 * idleEffect})` : '';
      if (canvas.style.filter !== grade) canvas.style.filter = grade;
    }
    projector.planet = game?.camera && orbit <= 0 ? 0 :
      TRY.planet ?? (curvature > 0 ? PLANET / curvature : 0);
    // 🖥️ Gone in to a screen (`camera.flat`, 0 to 1) the ball is let out to nothing as the lens closes: a face filling the picture is flat and square, not sagging round the planet.
    if (camera?.flat && projector.planet) projector.planet /= Math.max(1e-4, (1 - camera.flat) ** 2);
    // (How plain the picture is, 0 to 3, goes up when the drawing runs long and comes down only when it is well under: so it settles and does not hunt.)
    if (plain < 3 && cost > [7, 10, 14][plain]) plain++;
    else if (plain > 0 && cost < [4.5, 6.5, 9][plain - 1]) plain--;
    projector.fine = Math.max(lensFull < 0.3 ? 0.16 : lensFull < 0.55 ? 0.22 : 0.3, [0, 0.24, 0.34, 0.5][plain]);
    // The land lies at the plaza's height (far below, seen from a floor of the tower).
    projector.ground = ground;
    projector.frame();
    hungNow = hung.map((b, i) => {
      if (!projector.planet) return { ...b, sx: 0, sy: 0, sz: 0, hold: b.r + 3.2 };
      const at = bend(b.x - eye.x, b.y - ground, b.z - eye.z, projector.planet, eye.y - ground), w = hungNear[i];
      const sx = (eye.x + at[0] - b.x) * (1 - w), sy = (eye.y + at[1] - b.y) * (1 - w), sz = (eye.z + at[2] - b.z) * (1 - w);
      return { ...b, sx, sy, sz, hold: b.r + 3.2 };
    });
    // Daylight follows the normals of the wrapped terrain as the camera climbs.
    projector.orbitLight = lamp && orbit > 0 ? [projector.right, projector.up, projector.forward]
      .map(axis => axis[0]*lamp.x + axis[1]*lamp.y + axis[2]*lamp.z) : null;
    projector.orbitShade = 0.48 * orbit * orbit;

    // 🕳️ An eye under the land is inside the hollow planet: the land is a dome over it, and no sun comes in.
    const under = !!projector.planet && eye.y < projector.ground;
    if (under) casting = false;
    // The sky is the clear colour alone: no banded dome (2026-11-03).
    lastEye = eye;
    const tags = [];
    // 🌍 The block underfoot, then every open block around it where it
    // stands: one continuous ground.
    if (today && orbit < 1 && !xr?.mixed) {
      projector.bends = false; // (the sky is not wrapped round the planet)
      // Keep sky layers at successive far-depth values. At this distance the
      // flat dome cuts through the lights and depth precision can erase the glow.
      const layers = skyDome(skyStep);
      for (let i = 0; i < layers.length; i++) {
        const first = host.scene.triangleCount;
        draw(layers[i], eye.x, eye.y, eye.z, 0, 0, SKY);
        const depth = xr ? -(i + 1) : 1 - (i + 1) / 0x400000;
        for (let k = first * 18 + 2; k < host.scene.triangleCount * 18; k += 6) host.scene.vertices[k] = depth;
      }
      projector.bends = true;
    }
    // 🪐 The planets hung over home, whole.
    if (hungNow.length && orbit < 1 && !ledges) {
      const wrapped = projector.bends;
      projector.bends = false;
      for (const m of hungNow) projector.draw(hungBall(m, lamp), m.x + (m.sx || 0), m.y + (m.sy || 0), m.z + (m.sz || 0), 0, 0, m.r);
      projector.bends = wrapped;
    }
    if (xr) {
      projector.bends = false;
      drawHands(xr, eye);
      projector.bends = true;
    }
    if (under && !ledges) {
      const riders = basement ? players.slice() : beyond.filter(p => p.boat).map(p => ({ ...p, x: p.x - (skyline?.dx || 0), z: p.z - (skyline?.dz || 0) }));
      if (basement && me && seat >= 0) riders[seat] = me;
      const plaza = skyline || { dx: 0, dy: ground, dz: 0 };
      drawPool(draw, eye, { ...plaza, dy: plaza.dy - DEPTH }, riders, now);
    }
    if (basement) draw(cellar);
    else if (ledges) draw(ledgesFor(ledges));
    else if (tower?.here?.number > 0) draw(tower.here.roof ? towerParts().deck : towerParts().floor); // a floor of the tower: no walls, the world outside (on the roof no ceiling either)
    else if (park?.neighbours) {
      draw(groundFor('here', park.neighbours, park.plaza, field));
      if (park.plaza && !park.one) draw(cellar, 0, -DEPTH, 0); // (🏠 one room: no stair, nothing under the floor)
      // Both rooms draw the fence on their common boundary.
      if (orbit < 1 && seam?.shown) drawSeams(park.neighbours, 0, 0, 0, seam?.crossing ? SEAM.crossing : SEAM.rest, now);
    }
    else if (half === WORLD && game) draw(arena);
    else {
      // A room made by name stands in the open, on a platform; a game's arena has its walls.
      const kind = `${half}:${game ? 'walled' : 'open'}`;
      if (!arenas.has(kind)) arenas.set(kind, buildArena(PALETTES[paletteName], half, !!game));
      draw(arenas.get(kind));
    }
    for (const m of staged) {
      const far = Math.hypot(m.x - eye.x, (m.y || 0) - eye.y, m.z - eye.z);
      for (const key of m.textures || []) textures.hint(key, far);
      draw(m.model, m.x, m.y || 0, m.z, m.yaw || 0, m.pitch || 0, m.scale || 1, m.roll || 0);
    }
    ballCasters.length = 0;
    function drawBall(x, y, z, yaw = 0, roll = 0) {
      const base = ballFor();
      litBall ||= base.slice();
      // Turn the lamp into the rolling ball's local coordinates. Each leather
      // panel keeps one normal, including across its internal triangles.
      const skyLight = lamp || { x: 0, y: 1, z: 0 };
      const c = Math.cos(yaw), s = Math.sin(yaw), a = Math.cos(roll), b = Math.sin(roll);
      const lx = c * skyLight.x - s * skyLight.z, forward = s * skyLight.x + c * skyLight.z;
      const ly = a * skyLight.y + b * forward, lz = -b * skyLight.y + a * forward;
      const day = Math.min(1, Math.max(0, (sunNow?.y ?? 1) * 2 + 0.15));
      for (let i = 0, j = 0; i < base.length; i += 18, j += 3) {
        const diffuse = Math.max(0, lx * ballNormals[j] + ly * ballNormals[j+1] + lz * ballNormals[j+2]);
        const light = 0.19 + day * 0.25 + diffuse * (0.17 + day * 0.32);
        litBall[i+3] = base[i+3] * light * (0.82 + 0.18 * day);
        litBall[i+4] = base[i+4] * light * (0.9 + 0.1 * day);
        litBall[i+5] = base[i+5] * light;
      }
      draw(litBall, x, y, z, yaw, roll);
      if (casting && Math.hypot(x-eye.x, z-eye.z) < shadowReach * zoom) ballCasters.push(x, y, z);
    }
    if (courtyard) {
      const x = courtyard.dx + DISPENSER.x, z = courtyard.dz + DISPENSER.z;
      draw(dispenserModel, x, courtyard.dy, z);
      drawBall(x, courtyard.dy + DISPENSER.height + BALL.r, z);
    }
    const ballDt = ballSeen === null ? 0 : Math.min(0.1, (now - ballSeen) / 1000);
    ballSeen = now;
    for (const ball of balls) {
      const pace = Math.hypot(ball.vx, ball.vz);
      const angle = ((ballRoll.get(ball.id) || 0) + pace * ballDt / BALL.r) % (2 * Math.PI);
      ballRoll.set(ball.id, angle);
      drawBall(ball.x, ball.y, ball.z, pace > 0.05 ? Math.atan2(ball.vx, ball.vz) : 0, angle);
    }
    if (tower && !ledges) {
      draw(carFor(tower), 0, tower.lift || 0, 0);
      // (The screen that hung over the door inside is gone: the pad on the back wall is the one thing in the car that tells and is told.)
      // 🚪 The doors answer whoever is near: the car's stay shut while it
      // runs, the lobby's entrance opens from either side.
      const bodies = [me, ...players].filter(Boolean), dtDoor = Math.min(0.1, (now - doorTime) / 1000);
      doorTime = now;
      const near = (x, z, reach) => bodies.some((b) => Math.hypot(b.x - x, b.z - z) < reach);
      const ease = (name, open) => (doorAt[name] += ((open ? 1 : 0) - doorAt[name]) * (1 - Math.exp(-dtDoor * 7)));
      const carDoor = CAR_Z - CAR - CAR_WALL / 2 + 0.16;
      const c = ease('car', !ride?.moving && !tower.lift && near(0, carDoor, 3.2));
      const carLeaf = leafFor(CAR, 2.18);
      for (const side of [-1, 1]) draw(carLeaf, side * (CAR / 2 + c * (CAR - 0.1)), tower.lift || 0, carDoor);
      if (tower.here && !tower.here.number) {
        const e = ease('entrance', near(0, LOBBY_FRONT, 3.4));
        const leaf = leafFor(DOOR, DOOR_HEIGHT);
        for (const side of [-1, 1]) draw(leaf, side * (DOOR / 2 + e * (DOOR - 0.12)), 0, LOBBY_FRONT + 0.14);
      }
    }
    // Bring in building detail as the flight comes down from the globe.
    if (skyline && !ledges) {
      // (Who is seen on the tower from here, and what is built on its floors, as feet in the tower's own metres.)
      const feet = [];
      for (const b of beyond) feet.push({ x: b.x - skyline.dx, y: b.y - CFG.eyeHeight - skyline.dy, z: b.z - skyline.dz }); // (a body's y is its eye)
      for (const b of around) if (b.floor) feet.push({ x: 0, y: (b.dy || 0) - skyline.dy, z: 0 });
      drawTower(skyline.top, skyline.dx, skyline.dy, skyline.dz, eye.y, zoom, standsOn(feet), tower && !ledges ? tower.lift || 0 : null);
      draw(lobbyFor(), skyline.dx, skyline.dy, skyline.dz);
      // The land the tower stands on, seen from the plaza and from every floor:
      // fields to the horizon, and a lawn wherever a block around the plaza is
      // not open yet.
      // The grounds too stand wherever they are seen from, so the pool and
      // its beds do not appear only on stepping into the block before them.
      {
        const t = towerParts();
        const shown = (dx, dz) => (dx === 0 && dz === 0 && !skyline.dy) || around.some((b) => !b.sceneryOnly && b.dx === dx && b.dz === dz);
        projector.halvings = 13; // (its far squares are kilometres across: halved until they lie round the ball)
        // (From inside, the grass is not drawn: the bubble's skin is the land there. On the card it is seen through, and so drawn last: at the frame's end.)
        if (!under) draw(t.land, skyline.dx, skyline.dy, skyline.dz);
        else if (!projector.card) draw(t.dome, skyline.dx, skyline.dy, skyline.dz);
        projector.halvings = 0;
        for (let i = -1; i <= 1; i++)
          for (let j = -1; j <= 1; j++)
            if (!shown(skyline.dx + i * 16, skyline.dz + j * 16))
              draw(t.lawn, skyline.dx + i * 16, skyline.dy, skyline.dz + j * 16);
        for (const [i, j] of GROUNDS) {
          const dx = skyline.dx + i * 16, dz = skyline.dz + j * 16;
          if (!shown(dx, dz)) draw(groundFor(`grounds ${i} ${j}`, { east: 1, west: 1, north: 1, south: 1 }, false, { i, j }), dx, skyline.dy, dz);
        }
      }
    }
    for (const b of around) {
      if (b.sceneryOnly) continue;
      const by = b.dy || 0; // a block seen from up the tower lies below
      if (b.plaza && !basement) draw(cellar, b.dx, by - DEPTH, b.dz);
      if (!b.floor) draw(groundFor(b.id, b.neighbours, b.plaza, groundsOf(b.id)), b.dx, by, b.dz); // (a floor of the tower has its own slab)
      const painted = tilesFor(b.id, b.tiles);
      if (painted) draw(painted, b.dx, by, b.dz);
      // The seams between the blocks around, on their lines; the ones that
      // face this block are its own, drawn above.
      if (!by && orbit < 1 && seam?.shown) drawSeams(b.neighbours, b.dx, 0, b.dz, SEAM.far, now, (sx, sz) => b.dx + sx * SPAN === 0 && b.dz + sz * SPAN === 0);
    }
    for (const id of grounds.keys())
      if (id !== 'here' && !id.startsWith('grounds ') && !around.some((b) => b.id === id)) {
        grounds.delete(id); tileSets.delete(id);
      }
    // A thing standing on the ground, drawn where its block stands.
    const scenery = inspectScenery ? { objects: [], screens: [], panels: [] } : null;
    const stand = (object, ox, oz, oy = 0) => {
      const distance = Math.hypot(ox + object.x - eye.x, oy - eye.y, oz + object.z - eye.z) / zoom;
      // Grow the distant silhouette within the detailed model before retiring
      // the details. Both tiers are available well before a block handoff.
      const proxyScale = object.coarse ? 1 : clamp((distance - 48) / (OBJECT_PROXY_DISTANCE - 48), 0, 1);
      if (scenery) scenery.objects.push({ id: object.id, x: ox + object.x, y: oy, z: oz + object.z, proxyScale });
      if (proxyScale > 0) {
        const shape = object.geometry;
        let proxy = distantModels.get(shape);
        if (!proxy) {
          proxy = model(({ box }) => {
            for (const b of distantBoxes(shape)) box(b.x, b.y, b.z, b.w, b.h, b.d, b.color, INK, PAPER);
          });
          distantModels.set(shape, proxy);
        }
        draw(proxy, ox + object.x, oy, oz + object.z, 0, 0, proxyScale);
        if (proxyScale === 1) return;
      }
      if (object.geometry.format === OBJECT_FORMAT) {
        draw(
          sourceFrame(object.geometry, distance < 22 ? worldTime : Math.floor(worldTime * 12) / 12).vertices,
          ox + object.x,
          oy,
          oz + object.z,
        );
        if (distance < 22) drawHair(object.geometry, ox + object.x, oy, oz + object.z);
        return;
      }
      let cached = objectModels.get(object.geometry);
      if (!cached) {
        cached = {
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
        objectModels.set(object.geometry, cached);
      }
      if (object.geometry.motion) {
        const pose = poseObject(object, worldTime);
        pose.geometry.boxes.forEach((b, i) =>
          draw(
            cached.parts[i],
            ox + pose.x + b.x,
            oy + (pose.y || 0) + b.y,
            oz + pose.z + b.z,
            0,
            0,
            1,
            b.roll || 0,
          ),
        );
      } else draw(cached.model, ox + object.x, oy, oz + object.z);
    };
    const poses = new Map(items?.map(it => [it.id, it]));
    for (const object of objects) {
      if (object.item) {
        const it = poses.get(object.id) || (!items && { x: object.x, y: object.y || 0, z: object.z, holder: -1 });
        if (!it) continue;
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
        if ((holder || it.spin) && object.geometry.format === OBJECT_FORMAT)
          draw(sourceFrame(object.geometry, worldTime).vertices, at.x, at.y, at.z, yaw, pitch);
        else stand(object, at.x - object.x, at.z - object.z, at.y);
        continue;
      }
      stand(object, 0, 0);
    }
    for (const b of around)
      for (const object of b.objects || [])
        if (object.geometry) stand(object, b.dx, b.dz, (b.dy || 0) + (object.y || 0));
    for (const id of previewModels.keys())
      if (!jobs.some((j) => j.id === id)) previewModels.delete(id);
    for (const job of jobs) {
      if (job.kind === 'kidlisp') continue; // a wall rewrite has no footprint
      // (A new preview is a new object off the wire: compared as such, not
      // written out to text and compared every frame.)
      const code = job.preview;
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
    for (const d of drones)
      draw(droneModel(), d.x, d.y + Math.abs(Math.sin(now / 90 + d.id)) * 0.03, 0, (d.dir > 0 ? Math.PI : -Math.PI) / 2);
    // 🔲 Hitboxes (`): every body in green, the band a stomper's feet must
    // fall through to bop it in pink, drones in cyan, ledge tops in white,
    // your own feet as a yellow mark. Drawn in front of the plane.
    if (debug && game) {
      const all = Array.from({ length: Math.max(players.length, seat + 1) }, (_, i) => (i === seat && me ? me : players[i]));
      draw(model(({ box }) => {
        const z = 0.75, t = 0.035;
        const bar = (x0, y0, x1, y1, c) => box((x0 + x1) / 2, (y0 + y1) / 2, z, Math.max(t, x1 - x0), Math.max(t, y1 - y0), 0.02, c, c, c);
        const frame = (x0, y0, x1, y1, c) => { bar(x0, y0, x1, y0, c); bar(x0, y1, x1, y1, c); bar(x0, y0, x0, y1, c); bar(x1, y0, x1, y1, c); };
        for (const p of all) {
          if (!p?.alive) continue;
          const feet = p.y - CFG.eyeHeight, low = ducked(p);
          frame(p.x - 0.4, feet, p.x + 0.4, feet + 1.9 - low, 0x3fb950);
          frame(p.x - BOP.reach, feet + BOP.low - low, p.x + BOP.reach, feet + BOP.high - low, 0xff3296);
        }
        for (const d of drones) {
          frame(d.x - 0.35, d.y, d.x + 0.35, d.y + DRONE.tall, 0x3fe0d0);
          frame(d.x - BOP.reach, d.y + DRONE.tall - 0.5, d.x + BOP.reach, d.y + DRONE.tall + 0.25, 0xff3296);
        }
        for (const b of ledges || []) bar(b.xMin, b.yMax, b.xMax, b.yMax, 0xffffff);
        if (me?.alive) { const feet = me.y - CFG.eyeHeight; bar(me.x - 0.12, feet - 0.02, me.x + 0.12, feet + 0.02, 0xffe066); }
      }));
    }
    const cut = projector.cut; // People are never looked through: a body stays whole.
    projector.cut = null;
    // 🩸 Ragdolls, gibs, blood and splats (gore.mjs), stepped to now.
    {
      const t = performance.now(), dt = goreAt ? (t - goreAt) / 1000 : 0;
      goreAt = t;
      gore.step(dt, goreSolids(solidObjects), half);
      for (const g of gore.draws()) {
        const fig = g.seat === undefined ? null : g.part.startsWith('gib-') ? gibFighters[g.seat % gibFighters.length] : fighters[g.seat % fighters.length];
        const m = fig?.[g.part] || goreModels[g.part];
        if (m) draw(m, g.x, g.y, g.z, g.yaw, g.pitch, g.scale, g.roll);
      }
    }
    const inRoom = Math.max(players.length, seat + 1);
    for (let n = 0; n < inRoom + beyond.length; n++) {
      // (After the room's own: the people beyond it, each drawn as the seat they hold there.)
      const far = n >= inRoom ? beyond[n - inRoom] : null, i = far ? far.seat : n;
      const p = far || (i === seat && me ? me : players[i]);
      if (!p?.alive) continue;
      const f = fighters[i % fighters.length],
        yaw = (p.yaw * Math.PI) / 180,
        base = p.y - CFG.eyeHeight - (game ? ducked(p) : 0); // 🦆 a ducking body sits lower
      if (!far) lastPose.set(i, { x: p.x, base, z: p.z, yaw: p.yaw, pitch: p.pitch || 0, vx: p.vx || 0, vy: p.vy || 0, vz: p.vz || 0, at: performance.now() });
      if (p.bike) drawBike(p, i, base, now, far ? (far.name?.startsWith('@') ? far.name.slice(1) : null) : members.find((m) => m.seat === i)?.handle); // 🛵 (in first person too: the cabin is round you)
      if (far || i !== seat || third || !me || (idleCamera && Math.hypot(eye.x - me.x, eye.y - me.y, eye.z - me.z) > 0.7)) {
        draw(f.torso, p.x, base, p.z, yaw);
        draw(f.head, p.x, base, p.z, yaw);
        draw(eyeFor(p, i, now, face), p.x, base, p.z, yaw);
        // Name tags are screen text drawn after the world (see below), so
        // they stay crisp and never fight the scene for depth.
        if (far) { if (far.name) tags.push({ label: far.name, seat: i, x: p.x, y: base + 2.15, z: p.z, at: p }); }
        else if (i !== seat) tags.push({ seat: i, x: p.x, y: base + 2.15, z: p.z, at: p });
        const stride = p.bike ? 0 : Math.min(1, Math.hypot(p.vx, p.vz) / 10),
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
    // 🚁 Those looking on: a quadcopter each, hovering with a small bob,
    // leaning into the way it goes (nose down ahead, banked to the side),
    // its rotors a blur, a light at the front, its name over it.
    for (const d of eyes) {
      const yawR = (d.yaw * Math.PI) / 180, ahead = direction(d.yaw), side = direction(d.yaw - 90);
      const along = (d.vx || 0) * ahead.x + (d.vz || 0) * ahead.z, across = (d.vx || 0) * side.x + (d.vz || 0) * side.z;
      // It is never quite still: a hover's own small hunting, and the wind over
      // Los Angeles pushing it about (weather.mjs), each drone out of step
      // with the next. A fast one is steadier than one hanging in the air.
      // The air it flies in (air.mjs), where it is in the world: it leans
      // against what pushes it and rides what lifts it, as a small boat does.
      const seed = (d.name || '').length * 1.7 + (d.name || 'x').charCodeAt(0);
      const air = airAt(weather, d.x + (place?.x || 0), d.y, d.z + (place?.z || 0), Date.now() / 1000, { day: sun(phase()) });
      const pushAlong = air.x * ahead.x + air.z * ahead.z, pushAcross = air.x * side.x + air.z * side.z;
      // (Held level by its gyro: it floats, slowly, and leans only a little against the air.)
      const hunt = (rate, k) => Math.sin(now / rate + seed * k) * 0.008;
      const lean = Math.max(-0.6, Math.min(0.6, along * 0.05 - pushAlong * 0.012 + hunt(1300, 1)));
      const bank = Math.max(-0.6, Math.min(0.6, across * 0.05 - pushAcross * 0.012 + hunt(1100, 2)));
      const y = d.y + air.y * 0.03 + hunt(900, 3) * 3 - 0.05;
      draw(droneFor(Math.floor(now / 40) % 2), d.x, y, d.z, yawR, lean, 1, -bank + (d.roll || 0));
      if (d.name) tags.push({ label: d.name, x: d.x, y: y + 0.45, z: d.z, at: d });
    }
    // 🎯 Damage numbers: up and away from where the hit landed, then gone.
    const popNow = performance.now();
    for (let i = pops.length - 1; i >= 0; i--) if (popNow - pops[i].born > POP_MS) pops.splice(i, 1);
    for (const p of pops) {
      const k = (popNow - p.born) / POP_MS;
      tags.push({ label: p.label, x: p.x, y: p.y + 0.3 + k * 1.1, z: p.z, pop: true,
        color: p.crit ? 0xff4422 : p.attacker === seat ? 0xffe066 : 0xffffff });
    }
    projector.cut = cut;
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
      const t = b.flown ? 0 : Math.min(0.1, since); // (rounds.mjs has already flown it to now)
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
    if (prompt && !xr) {
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
      // The cursor is a wireframe block (CARET marks its column); the
      // ring's phrase keeps the blinking underscore.
      const CARET = '\u0000';
      const writing =
        prompt.text.slice(0, prompt.cursor) +
        (radial ? cursor : CARET) +
        prompt.text.slice(prompt.cursor);
      // 💬 Chat types on the same top line as a build, with a message bubble
      // where the build has its cube (2026-11-15).
      const saying = !bare && !!chat?.open;
      const said = saying ? chat.text.slice(0, chat.cursor) + CARET + chat.text.slice(chat.cursor) : '';
      const bubbleSpans = (l) => {
        const at = l.indexOf(CARET);
        if (at < 0) return [{ text: l, color: THEME.text }];
        return [
          { text: l.slice(0, at), color: THEME.text },
          { block: true, bubble: 0x9be7a0, beat: Math.floor(now / 300) % 4 },
          { text: l.slice(at + 1), color: THEME.text },
        ].filter((x) => x.block || x.text);
      };
      const caretSpans = (l) => {
        const at = l.indexOf(CARET);
        if (at < 0) return [{ text: l, color: THEME.text }];
        return [
          { text: l.slice(0, at), color: THEME.text },
          { block: true, wire: builder ? 0x3fb950 : 0xff6b6b, turn: Math.round(now / 33) * 0.033 * 1.6 },
          { text: l.slice(at + 1), color: THEME.text },
        ].filter((x) => x.block || x.text);
      };
      const named = (o) => (o.code ? `$${o.code}` : o.geometry.name);
      const verb = holding ? { shoot: 'shoot', poke: 'poke', throw: 'throw' }[holding.item] : null;
      const carry = holding
        ? pad
          ? `[RT] ${verb} / [X] drop`
          : input === 'touch'
            ? null
            : `f or click ${verb} / q drop`
        : null;
      const drawing = chalkMode
        ? pad
          ? `chalk ${chalkMode.text}: hold [RT] draw / [X] build / [B] cancel`
          : `chalk ${chalkMode.text}: hold c or right click / enter build / esc cancel`
        : null;
      const whose = (o) =>
        o.panel || o.screen
          ? input === 'touch' ? 'go in' : pad ? `${spectator ? '[X]' : '[RB]'} go in` : o.panel || spectator ? '{f} go in' : '{z} go in'
        : o.wall // (only a KidLisp wall, which has its `source`, is changed; at a standing screen or a thing in the room beside this one the ring is for a new build: client.mjs wallOf)
          ? pad ? `${spectator ? '[X]' : '[RB]'} ${o.far ? 'lock on' : 'go in'}` : `{f} ${o.source ? 'change' : 'build'}`
          : spectator ? pad ? '[X] lock on' : '{f} lock on' : o.owner === owner
            ? pad ? '[RB] lock on' : '{enter} change / {z} lock on'
            : pad ? '[RB] lock on' : '{z} lock on';
      const paintHint = !focusObject && floorAim && !holding && input !== 'touch' ? (pad ? '[X] paint the floor' : '{x} paints the floor') : null;
      const confirming = focusObject && removeArmed === focusObject.id ? '{delete} again to remove' : null;
      // Locked on, the top-right controls become the lock's own, row for
      // row in the hand in use (2026-11-08).
      const mine = focusObject && focusObject.owner === owner;
      const screenHint = docked && focusObject
        ? [named(focusObject),
            ...(input === 'touch' ? [focusObject.panel ? 'tap: press' : 'leave'] : [
              ...(pad ? ['[LS] look / [L3] 2x zoom'] : []),
              pad ? '[RS] cursor / [RT] press' : focusObject.panel ? '{enter} or click: press' : '',
              pad ? '[DH] next / [B] leave' : '{[} {]} next / {esc} leave',
            ])].filter(Boolean).join('\n')
        : null;
      const lockHint = lockedOn && focusObject
        ? [
            `${named(focusObject)} locked`,
            ...(pad
              ? spectator ? ['[RS] cursor / [DH] next', '[X] unlock'] : focusObject.wall
                ? ['[RS] cursor / [RB] unlock']
                : ['[B] remove', '[RS] cursor / [DH] next', '[RB] unlock']
              : focusObject.wall
                ? [`{f} ${focusObject.source ? 'change' : 'build'} / {z} unlock`]
                : [`${mine ? '{enter} change / ' : ''}{del} remove`, '{[} {]} next / {z} unlock']),
          ].join('\n')
        : null;
      // What the highlighted object offers ([B] remove, [RB] unlock, its
      // name and owner) is drawn at the object itself (2026-11-03); the
      // corner keeps only the general hint.
      const objectHint = screenHint || (lockHint
        ? lockHint
        : confirming
          ? confirming
          : focusObject
            ? `${named(focusObject)}${focusObject.wall ? '' : ` v${focusObject.version}`}\n${whose(focusObject)}`
            : null);
      objectHintSpans = null; // The lock-on shows only its bracket; its offers read in the corner.
      // In the car the floor comes first: before paint, and on touch too.
      const waterHint = basement && me && (me.boat ? 'move to row / jump to swim'
        : me.y < POOL.level + 2 && berths.some(b => Math.hypot(me.x-b.x,me.z-b.z)<0.86) ? 'jump to board'
        : me.swimming ? 'look down and swim to dive' : null);
      const hint = waterHint || objectHint || drawing || carry || (panels.some((p) => p.cursor) // 🖼️ the aim is a world panel's mouse
          ? input === 'touch' ? 'tap the screen' : pad ? '[RT] press' : '{enter} or click: press'
        : car
          ? pad ? '[DV] tap a floor / hold for roof or B1' : '{enter} pick a floor'
        : paintHint ? paintHint
        : pad ? null : '{enter} to build');
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
      // A mode (locked on, aiming at a build, chalk) shows its own controls
      // in place of the general legend. Carrying is not one, nor is aiming at
      // something to pick up: with a gun in hand or in the crosshair you still
      // walk, jump and build, so the legend stays and the item's line joins it.
      const moded = !!(objectHint || drawing || carry);
      const itemOnly = !drawing && (!objectHint || (focusObject?.item && !lockedOn && !docked));
      const showLegend = legend && (!moded || itemOnly) && !spectator && !wallCode && !prompt.open && !building;
      const rows = bare
        ? []
        : saying
        ? wrap(said).map(bubbleSpans)
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
            ...wrap(writing).map((l) => (radial ? phraseSpans(l) : caretSpans(l))),
            // Whether a builder listens is the cursor's colour; a revision
            // still names what it revises.
            ...(prompt.target ? [[{ text: title, color: THEME.verb }]] : []),
            ...(prompt.message ? wrap(prompt.message).map((l) => [{ text: l, color: THEME.number }]) : []),
            // Typing, {enter} follows the cube on its line and {esc} sits
            // top-right (both small, drawn below); the ring and a running
            // build keep their own row.
            ...(radial || prompt.busy
              ? [
                  hintSpans(
                    radial?.bumper ? '[RS] aim / [A] add / release [LB] at centre to build' : radial
                      ? input === 'touch'
                        ? 'tap a choice / centre builds / outside undoes'
                        : '[A] choose / [X] build / [B] undo'
                      : cancel,
                    THEME.dim,
                  ),
                ]
              : []),
          ]
        : input === 'touch'
        ? (focusObject ? [[{ text: named(focusObject), color: THEME.verb }]] : [])
        : [
              // The controls for the hand in use, until the player has played
              // a while (H, on the keys, brings them back).
              ...(showLegend ? legendRows(input, held, game?.legend || LEGEND) : []),
              ...(spectator
                ? [
                    // Looking on. (No way in is offered here: signing up
                    // and taking a handle belong to the paper, 2026-11-18.)
                    // (Three short rows, as a walker's are: each key lights while it is down, each action in its colour.)
                    ...legendRows(input === 'touch' ? 'touch' : controller ? 'pad' : 'keys', held, DRONE_LEGEND),
                    ...(self?.canTakeBody && !docked ? [colourRow(hintSpans(pad ? '[L3] take body' : '{p} take body', THEME.text))] : []),
                    // Target controls for the drone.
                    ...(objectHint && (focusObject?.panel || focusObject?.screen || lockedOn) ? objectHint.split('\n').map((l) => colourRow(hintSpans(l, THEME.text))) : []),
                  ]
                : !me ? [] // Still arriving: no seat yet, so no controls to offer.
                : game && !moded ? [] // a game's legend is its whole hint
                : !hint ? [] : hint.split('\n').map((l, i) =>
                    i === 0 && moded && hint.includes('\n')
                      ? [{ text: l, color: THEME.verb }]
                      : colourRow(hintSpans(l, confirming ? 0xff6b6b : THEME.text)),
                  )),
            ];
      const visible = rows.slice(
        -Math.max(3, Math.floor((height - 2 * safeY) / (11 * unit)) - 3),
      ).map((r) => clipSpans(r, columns));
      promptRows = prompt.open || saying ? visible.length : 0;
      // A phone has send and cancel as caps; the key hints are for keys.
      const typing = (saying || (prompt.open && !radial && !prompt.busy)) && input !== 'touch';
      const enterHint = typing ? hintSpans(chat?.lift ? '{enter} ride / {up} {down} a floor' : saying ? '{enter} say' : '{enter} build', THEME.dim) : null;
      const escHint = typing ? hintSpans('{esc} cancel', THEME.dim) : null;
      // Stack the chart below the controls while their help is visible.
      let hintOffset = modeBox ? modeBox.h + 2 * unit : 0;
      if (buildLines.length && !prompt.open && !saying) {
        const rightWidth = Math.max(0, ...visible.map((row) => smallSpanWidth(row, unit)));
        const wireWidth = wire && !menu ? wire.text.length * 6 * unit : 0;
        if (promptX + Math.max(buildWidth, wireWidth) + 2 * unit > width - safeX - rightWidth)
          hintOffset += (buildLines.length + (wireWidth ? 1 : 0)) * 10 * unit;
      }
      if (!prompt.open && !saying && visible.length) chartTop = Math.max(chartTop, safeY + hintOffset + (14 * visible.length + 2) * unit);
      const key = JSON.stringify([visible, width, height, bump, hintOffset, prompt.open, saying, !!chat?.lift, !!radial, !!wallCode, !!enterHint]);
      if (key !== promptKey) {
        promptKey = key;
        promptModel = model(({ rect: worldRect, text, small }) => {
          const rect = (x, y, z, w, h, color) => worldRect(-x - w, y, z, w, h, color);
          const panel = prompt.open || saying; // A build's status floats over the build itself.
          // Nothing here lifts the corners (2026-11-03). The prompt and a
          // build in progress sit top-left on the 2D prompt's own text grid
          // (6, 6; ten-pixel lines) so the typed line is pixel for pixel
          // where the paper shows it; the legend and the hint line are
          // right-aligned at the top-right.
          panelTop = null;
          const widths = visible.map((spans) => spanWidthOf(spans, unit));
          if (panel) {
            const ox = promptX, oy = promptY;
            visible.forEach((spans, i) => {
              const y = height / 2 - oy - 2 * unit - i * 10 * unit;
              drawSpans({ rect, text }, spans, -width / 2 + ox + widths[i] / 2, y, unit, { shadow: INK });
              if (enterHint && spans.some((x) => x.wire !== undefined || x.bubble !== undefined)) {
                const hw = smallSpanWidth(enterHint, unit);
                drawSpans({ rect, text, small }, enterHint, -width / 2 + ox + widths[i] + 4 * unit + hw / 2, y, unit, { shadow: INK, tiny: true });
              }
            });
            // The way out sits top-right, where the legend rests while walking.
            if (escHint) {
              const ew = smallSpanWidth(escHint, unit);
              drawSpans({ rect, text, small }, escHint, width / 2 - safeX - ew / 2, height / 2 - safeY - hintOffset - 2 * unit, unit, { shadow: INK, tiny: true });
            }
          } else {
            // The controls and the hint, top-right, compact: MatrixChunky8
            // with small key caps and badges (2026-11-08).
            const right = width / 2 - safeX;
            visible.forEach((spans) => {
              const w = smallSpanWidth(spans, unit);
              drawSpans({ rect, text, small }, spans, right - w / 2, height / 2 - safeY - hintOffset - 2 * unit - visible.indexOf(spans) * 14 * unit, unit, { shadow: INK, tiny: true }); // (fourteen-pixel rows: a cap and its shadow clear the row under it)
            });
          }
        });
      }
      hudOverlay('controls', promptModel, width, height);
    }
    // Prompt's profile button has a six-pixel margin, four pixels of padding,
    // and a ten-row font. handleText places row zero one unit above its origin.
    // Match the paper only during its transition. In the room, use the visible viewport.
    const handleX = -canvas.width / 2 + (grid ? paperLeft * orbit : 0) + 10 * hud.unit + bump;
    const handleY = -canvas.height / 2 + (grid ? paperBottom * orbit : 0) + 19 * hud.unit + bump;
    handleRect = null;
    let ownTag = null; // Where your own handle ends, for your world position beside it.
    if (!xr && (members.length || connection || cells)) {
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
      if (labels.length && !bare) {
        handleRect = { x: width / 2 + handleX - 4 * unit, y: height / 2 - handleY - 4 * unit,
          w: (labels[0].label.length * 6 + 22) * unit, h: 20 * unit };
      }
      const hoverX = handlePoint && handlePoint.x * width / canvas.clientWidth;
      const hoverY = handlePoint && handlePoint.y * height / canvas.clientHeight;
      const handleHovered = !!(handlePoint && handleRect && hoverX >= handleRect.x && hoverX < handleRect.x + handleRect.w &&
        hoverY >= handleRect.y && hoverY < handleRect.y + handleRect.h);
      if (labels.length && !bare)
        ownTag = {
          x: handleX + (labels[0].label.length * 6 + 3 + (agents ? 10 : 0)) * unit, // (past the linked-agent mark, when one stands there)
          y: handleY,
        };
      const meter = (connection?.label || '').toLowerCase();
      const jitter =
        connection?.samples && connection.jitter >= 10
          ? `j ${Math.round(connection.jitter)} ms`
          : '';
      const key = JSON.stringify([
        labels,
        agents, handleX, handleY, handleHovered,
        meter,
        jitter,
        connection?.bars,
        width,
        height,
        joinURL,
        panelTop,
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
          const floor = handleY;
          labels.forEach(({ label, handle, colors }, i) => {
            const y = floor + (labels.length - 1 - i) * 12 * unit;
            const x = handleX;
            if (handleHovered) rect(x, y - 10 * unit, 0, (label.length * 6 - 1) * unit, unit, PAPER);
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
            // 🤖 An agent linked to your handle stands after it: an antenna and a head, drawn on this grid as the name is, in the space the position beside it leaves.
            if (agents && i === labels.length - 1) {
              const gx = x + (label.length * 6 + 2) * unit;
              for (const [dx, dy, w, h, color] of [[2, 7, 3, 1, 0x46c85a], [3, 6, 1, 1, 0x46c85a], [1, 5, 5, 1, 0xb44887], [0, 2, 7, 3, 0xb44887], [1, 1, 5, 1, 0x782c5c], [1, 3, 1, 1, 0xffffff], [5, 3, 1, 1, 0xffffff]]) {
                rect(gx + (dx + 1) * unit, y - 7 * unit + (dy - 1) * unit, 0.000002, w * unit, h * unit, INK);
                rect(gx + dx * unit, y - 7 * unit + dy * unit, 0, w * unit, h * unit, color);
              }
            }
          });
          if (meter && !face && !bare && !game) {
            // No face cam to sit under: shadowed text right-aligned under the QR, coloured by health.
            const ping = (connection?.label || '').toLowerCase();
            const bars = connection?.bars || 0;
            const good = 0x3fb950, fair = 0xffe066, poor = 0xff6b6b;
            const pingColor = !ping ? THEME.dim : bars >= 3 ? good : bars >= 2 ? fair : poor;
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
          }
        });
      }
      hudOverlay('handle', presenceModel, width, height);
    }
    const W = canvas.width,
      H = canvas.height,
      pixel = W / Math.max(1, canvas.clientWidth);
    const chatting = !bare && chat && (chat.open || chat.lines.length);
    if (!xr && (tags.length || ownTag || chatting || ride) && !menu) {
      const { unit } = hud;
      // 📍 World position in metres (x y z), small, after a handle: yours
      // bottom left, everyone else's over their head.
      const where = (p) => [['x ' + p.x.toFixed(1), AXIS.x], ['y ' + (p.y || 0).toFixed(1), AXIS.y], ['z ' + p.z.toFixed(1), AXIS.z]];
      const m = model(({ rect: worldRect, text, handleText, small }) => {
        const place = (p, left, y, n = 0, beside = false, more = []) => {
          // 🍄 In the arena the count is the point; elsewhere it follows the position.
          const count = `${n} bop${n === 1 ? '' : 's'}`;
          const parts = [...(ledges || beside || !stats ? [] : where(p)), ...(ledges || n ? [[count, THEME.dim]] : []), ...more]; // (metres are the debugging layer's)
          for (const [t, c] of parts) {
            const cx = left + (smallWidth(t) / 2) * unit;
            small(t, cx + unit, y - unit, 0.000002, unit, INK);
            small(t, cx, y, 0, unit, c);
            left += (smallWidth(t) + 6) * unit;
          }
        };
        // (Your own position is in the column beside the face cam.)
        // 🕰️🔋 On your own line, after your handle: the time of day here, and the machine's battery where the browser tells it.
        const clock = new Date().toLocaleTimeString([], { hour: 'numeric', minute: '2-digit' }).toLowerCase().replace(/\s+/g, ' ');
        const charge = battery && Number.isFinite(battery.level) ? `${battery.charging ? '+' : ''}${Math.round(battery.level * 100)}%` : null;
        const system = [[clock, 0xd0d4e0], ...(charge ? [[charge, battery.charging ? 0x69bfff : battery.level < 0.2 ? 0xff6b6b : battery.level < 0.5 ? 0xffe066 : 0x3fb950]] : [])];
        if (ownTag && !game) place(me || { x: 0, y: 0, z: 0 }, ownTag.x, ownTag.y, me ? bops[seat] || 0 : 0, !!face || !me, system);
        // 🎮 In a game the score is the headline: top centre, large. A
        // chain in the air reads under it as +points (and xN from the
        // second bop); on landing that flies up into the score, which
        // counts it in and jumps.
        if (game && seat >= 0) {
          const n = bops[seat] || 0, chain = combos[seat] || null;
          if (chain) {
            if (chain[0] !== chainShown[0]) chainAt = now;
            chainShown = chain;
          } else if (chainShown[0]) {
            // Landed: if the score rose, the chain is on its way up to it.
            if (n > scoreShown) { flyAt = now; flyPoints = n - scoreShown; }
            chainShown = [0, 0];
          }
          const flying = flyPoints ? Math.min(1, (now - flyAt) / 320) : 1;
          if (flyPoints && flying >= 1) { flyPoints = 0; scoreAt = now; }
          if (!flyPoints && n !== scoreShown) { if (n > scoreShown) scoreAt = now; scoreShown = n; }
          const pop = Math.max(0, 1 - (now - scoreAt) / 260);
          const big = unit * (5 + Math.round(pop * 3)), top = H / 2 - hud.safeY;
          const digits = `${scoreShown}`, y = top - 8 * unit - unit * 20;
          text(digits, unit * 2, y - unit * 2, 0.000002, big, INK);
          text(digits, 0, y, 0, big, pop > 0 ? 0xff3296 : PAPER);
          const word = scoreShown === 1 ? 'bop' : 'bops', wy = y - unit * 32;
          small(word, unit, wy - unit, 0.000002, unit * 2, INK);
          small(word, 0, wy, 0, unit * 2, THEME.dim);
          if (debug && me) {
            const info = `feet ${(me.y - CFG.eyeHeight).toFixed(2)}  vy ${(me.vy || 0).toFixed(1)}  ${me.onGround ? 'ground' : 'air'}${me.duck ? '  duck' : ''}  x ${me.x.toFixed(2)}`;
            small(info, unit, -H / 2 + hud.safeY + 22 * unit - unit, 0.000002, unit, INK);
            small(info, 0, -H / 2 + hud.safeY + 22 * unit, 0, unit, 0xffe066);
          }
          const cy = wy - unit * 26;
          if (chainShown[0] || flyPoints) {
            const kick = Math.max(0, 1 - (now - chainAt) / 200);
            const size = unit * (3 + (flyPoints ? 0 : Math.round(kick * 2)));
            const label = flyPoints ? `+${flyPoints}` : `+${chainShown[1]}${chainShown[0] > 1 ? ` x${chainShown[0]}` : ''}`;
            const ease = flying * flying, at = flyPoints ? cy + (y - cy) * ease : cy;
            text(label, unit, at - unit, 0.000002, size, INK);
            text(label, 0, at, 0, size, 0xff3296);
          }
        }
        // 🛗 The ride is told on the car's own screen (2026-11-19): nothing
        // of it is laid over the picture.
        // 💬 Chat: the room's last lines up the left side, over the face cam,
        // newest lowest. The line being typed is the top line (above).
        if (chatting) {
          const { safeX, safeY } = hud;
          const rect = (x, y, z, w, h, color) => worldRect(-x - w, y, z, w, h, color);
          const left = -W / 2 + safeX + 2, step = 11 * unit;
          const columns = Math.max(12, Math.floor((W * 0.55) / (6 * unit)));
          const run = (s, x, y, color, handle) => {
            const cx = x + ((s.length * 6 - 1) * unit) / 2;
            (handle ? handleText : text)(s, cx + unit, y - unit, 0.000002, unit, handle ? shadowInk(s) : INK);
            (handle ? handleText : text)(s, cx, y, 0, unit, color);
            return x + s.length * 6 * unit;
          };
          // Bottom up, newest first.
          let y = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit) + 64 * unit;
          for (const line of [...chat.lines].reverse()) {
            const who = line.handle ? `@${line.handle}` : line.name || `guest ${line.seat + 1}`;
            const room = Math.max(8, columns - who.length - 1);
            const words = line.text.split(' '), rows = [''];
            for (const w of words) {
              if ((rows.at(-1) + ' ' + w).trim().length > room) rows.push('');
              rows[rows.length - 1] = (rows.at(-1) + ' ' + w).trim().slice(0, room);
            }
            for (let k = rows.length - 1; k >= 0; k--) {
              let x = left;
              if (k === 0) x = run(who + ' ', left, y, line.handle ? line.colors : PAPER, !!line.handle);
              else x = left + (who.length + 1) * 6 * unit;
              run(rows[k], x, y, THEME.text);
              y += step;
            }
          }
        }
        const placedTags = [];
        tags.sort((p, q) => Math.hypot(p.x - eye.x, p.z - eye.z) - Math.hypot(q.x - eye.x, q.z - eye.z));
        for (const t of orbit > 0.5 ? [] : tags) {
          const s = projector.project(t.x, t.y, t.z);
          if (!s || Math.abs(s.x) > 1.1 || Math.abs(s.y) > 1.1) continue;
          // Hidden behind walls and built objects, as a real tag would be.
          const dx = t.x - eye.x, dy = t.y - eye.y, dz = t.z - eye.z,
            d = Math.hypot(dx, dy, dz);
          if (wallDistance(eye, { x: dx / d, y: dy / d, z: dz / d }, d, solidObjects, worldTime, half) < d - 0.3) continue;
          const member = t.label ? null : members.find((x) => x.seat === t.seat);
          const label = t.label || (member?.handle ? `@${member.handle}` : member?.name || `guest ${t.seat + 1}`);
          const x = Math.round((s.x * W) / 2);
          let y = Math.round((s.y * H) / 2) + 10 * unit;
          // 🏷️ A crowd's tags never sit on one another: a tag that would
          // land on one already placed this frame rises a line at a time
          // (nearest first below, so the closest player keeps the spot over
          // their own head). Its width allows for the position after it.
          const reach = ((label.length * 6) / 2 + 3) * unit, wide = reach + 15 * 4 * unit;
          if (t.pop) {
            // (A damage number keeps to its own spot: it never shoulders a name tag aside.)
            text(label, x + unit, y - unit, 0.000002, unit, INK);
            text(label, x, y, 0, unit, t.color);
            continue;
          }
          for (let tries = 0; tries < 12; tries++) {
            const hit = placedTags.find((p) => x - reach < p.right && x + wide > p.left && Math.abs(y - p.y) < 9 * unit);
            if (!hit) break;
            y = hit.y + 9 * unit;
          }
          placedTags.push({ left: x - reach, right: x + wide, y });
          if (member?.handle) {
            handleText(label, x + unit, y - unit, 0.000002, unit, shadowInk(label));
            handleText(label, x, y, 0, unit, member.colors);
          } else {
            text(label, x + unit, y - unit, 0.000002, unit, INK);
            text(label, x, y, 0, unit, t.label ? (t.mine ? 0xff3296 : 0xb56bff) : PAPER);
          }
          if (t.at && !plainTags) place(t.at, x + ((label.length * 6) / 2 + 3) * unit, y, bops[t.seat] || 0);
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
            ? hintSpans(radial.bumper || input === 'touch' ? 'build' : '[X] build', INK)
            : [{ text: radial.title.toLowerCase(), color: INK }];
          const w = (centre.reduce((n, c) => n + (c.badge ? 10 : c.text.length * 6), 0) + 8) * unit;
          rect(-w / 2 + unit, cy - 6 * unit, 0.000005, w, 13 * unit, INK);
          rect(-w / 2, cy - 5 * unit, 0.000004, w, 13 * unit, radial.ready && (!radial.bumper || radial.centered) ? 0xffe066 : PAPER);
          drawSpans({ rect, text }, centre, 0, cy + 5 * unit, unit, { shadow: null });
        });
      }
      projector.overlay(ringModel, W, H);
    }
    if (touch?.stick || touchButtons.length) {
      // Pixel squares, like everything else: a ring for the anchor, a knob for the thumb.
      const { unit } = hud;
      const m = model(({ rect: worldRect, text }) => {
        const rect = flip(worldRect);
        const at = (x, y) => [x * pixel - W / 2, H / 2 - y * pixel];
        // The phone's buttons: a cap in the action's colour, pressed in while held.
        for (const b of touchButtons) {
          const [x, top] = at(b.x, b.y),
            w = b.w * pixel,
            h = b.h * pixel,
            sink = b.held ? unit : 0;
          rect(x + unit, top - h - unit, 0.000004, w, h, INK);
          rect(x + sink, top - h - sink, 0.000003, w, h, b.held ? 0xffffff : b.color);
          text(b.name, x + w / 2 + sink, top - h / 2 + 3 * unit - sink, 0.000001, unit, INK);
        }
        const frame = (cx, cy, size, thick, color, z) => {
          rect(cx - size / 2, cy + size / 2 - thick, z, size, thick, color);
          rect(cx - size / 2, cy - size / 2, z, size, thick, color);
          rect(cx - size / 2, cy - size / 2, z, thick, size, color);
          rect(cx + size / 2 - thick, cy - size / 2, z, thick, size, color);
        };
        if (touch?.stick) {
          const radius = Math.max(36, Math.min(canvas.clientWidth, canvas.clientHeight) * 0.12) * pixel;
          const [ox, oy] = at(touch.stick.ox, touch.stick.oy),
            [kx, ky] = at(touch.stick.x, touch.stick.y);
          frame(ox + unit, oy - unit, radius * 2, 2 * unit, INK, 0.000003);
          frame(ox, oy, radius * 2, 2 * unit, PAPER, 0.000002);
          const k = radius * 0.6;
          rect(kx - k / 2 + unit, ky - k / 2 - unit, 0.000003, k, k, INK);
          rect(kx - k / 2, ky - k / 2, 0.000002, k, k, PAPER);
        }
      });
      projector.overlay(m, W, H);
    }
    // 🎮 A controller while looking on, top right under the controls: its two
    // sticks as rings with the knob where the thumb has it (left moves the
    // eye, right, in cyan, turns it), and everything on it that can be
    // pressed as a dark square that lights while held.
    if (sticks) {
      const { unit, safeX, safeY } = hud;
      const m = model(({ rect: worldRect }) => {
        const rect = flip(worldRect);
        const radius = Math.max(16, Math.min(canvas.clientWidth, canvas.clientHeight) * 0.04) * pixel;
        const b = Math.max(3 * unit, Math.round(radius * 0.34)), gapX = radius * 1.4, step = b * 1.25, wing = gapX + radius + b * 2.4;
        // Its middle: the whole pad's width in from the right edge, below the legend's row.
        const cx = W / 2 - safeX - (wing + step + b), oy = H / 2 - safeY - 18 * unit - (radius + b * 3);
        const frame = (x, y, size, thick, color, z) => {
          rect(x - size / 2, y + size / 2 - thick, z, size, thick, color);
          rect(x - size / 2, y - size / 2, z, size, thick, color);
          rect(x - size / 2, y - size / 2, z, thick, size, color);
          rect(x + size / 2 - thick, y - size / 2, z, thick, size, color);
        };
        for (const [side, [ax, ay], ink] of [[-1, sticks.move, PAPER], [1, sticks.look, 0x00ffff]]) {
          const ox = cx + side * gapX, moved = Math.hypot(ax, ay) > 0.05;
          const kx = ox + Math.max(-1, Math.min(1, ax)) * radius * 0.7, ky = oy - Math.max(-1, Math.min(1, ay)) * radius * 0.7;
          frame(ox + unit, oy - unit, radius * 2, 2 * unit, INK, 0.000003);
          frame(ox, oy, radius * 2, 2 * unit, moved ? ink : tint(ink, 0.6), 0.000002);
          const k = radius * 0.6;
          rect(kx - k / 2 + unit, ky - k / 2 - unit, 0.000003, k, k, INK);
          rect(kx - k / 2, ky - k / 2, 0.000002, k, k, moved ? ink : tint(ink, 0.6));
        }
        const held = sticks.buttons || [];
        const key = (x, y, on, ink, w = b, h = b) => {
          rect(x - w / 2 + unit, y - h / 2 - unit, 0.000003, w, h, INK);
          rect(x - w / 2, y - h / 2, 0.000002, w, h, on ? ink : 0x2c3040);
        };
        // D-pad: up 12, down 13, left 14, right 15.
        for (const [i, dx, dy] of [[12, 0, 1], [13, 0, -1], [14, -1, 0], [15, 1, 0]]) key(cx - wing + dx * step, oy + dy * step, held[i], PAPER);
        // Faces as they sit: bottom 0 (A), right 1 (B), left 2 (X), top 3 (Y), in the legend's colours.
        for (const [i, dx, dy, ink] of [[0, 0, -1, 0x3fb950], [1, 1, 0, 0xff6b6b], [2, -1, 0, 0x6aa8ff], [3, 0, 1, 0xffe066]]) key(cx + wing + dx * step, oy + dy * step, held[i], ink);
        // Bumpers (4, 5) and triggers (6, 7) over the sticks; back and start (8, 9) between them.
        for (const [side, bumper, trigger] of [[-1, 4, 6], [1, 5, 7]]) {
          key(cx + side * gapX, oy + radius + b * 1.1, held[bumper], PAPER, radius * 1.3, b * 0.8);
          key(cx + side * gapX, oy + radius + b * 2.3, held[trigger], 0x00ffff, radius * 0.9, b * 0.8);
        }
        key(cx - b * 0.7, oy + radius + b * 1.1, held[8], PAPER, b * 0.8, b * 0.6);
        key(cx + b * 0.7, oy + radius + b * 1.1, held[9], PAPER, b * 0.8, b * 0.6);
        // Anything past the standard seventeen (L4, R4, paddles): a row beneath, as they come.
        held.forEach((on, i) => { if (i > 16 && on) key(cx + (i - 18.5) * step, oy - radius - b, true, 0xd7a6ff); });
      });
      hudOverlay('controller', m, W, H);
    }
    // 🚁 A drone's dials along the bottom's middle: speed, the rotors, height.
    if (gauge && !bare && !menu) {
      const { unit, safeY } = hud;
      const m = model(({ small }) => {
        const parts = [[`${Math.round(gauge.mph)}`, PAPER], [' mph   ', THEME.dim], [`${(Math.round(gauge.rpm / 10) * 10).toLocaleString('en-US')}`, 0x00ffff], [' rpm   ', THEME.dim],
          [`${Math.round(gauge.feet).toLocaleString('en-US')}`, AXIS.y], [' ft', THEME.dim]];
        let left = -(parts.reduce((w, [t]) => w + smallWidth(t) + 1, 0) * unit) / 2;
        const y = -H / 2 + safeY + 12 * unit;
        for (const [t, ink] of parts) {
          const x = left + (smallWidth(t) / 2) * unit;
          if (t.trim()) { small(t, x + unit, y - unit, 0.000002, unit, INK); small(t, x, y, 0, unit, ink); }
          left += (smallWidth(t) + 1) * unit;
        }
      });
      hudOverlay('drone-gauges', m, W, H);
    }
    lensFull = (projector.scene?.triangleCount || 0) / (projector.scene?.maxTriangles || 1);
    if (stats && !chartVisible) frameChart.reset();
    if (stats) frameChart.sample(now);
    chartVisible = stats;
    // 📦 This frame's places at the edges (hud.mjs createPack). The aimed object's code tag is the corner's first tenant; what else stands bottom right takes its room after it.

    headsetRects = [];
    if (headsetControls?.modes.length && !menu && !prompt?.open && !chat?.open) {
      const { unit } = hud, { modes, busy, message } = headsetControls;
      const size = Math.max(20 * unit, Math.ceil(44 * pixel));
      const row = hudPack.reserve('headset', 'br', size * modes.length, size);
      const buttons = model(({ rect, small }) => {
        modes.forEach((mode, i) => {
          const x = row.x + i * size, y = row.y;
          const cx = Math.round(x + size / 2), cy = Math.round(y + size / 2);
          const ink = busy ? THEME.dim : mode === 'immersive-vr' ? 0x00ffff : 0xff9ee0;
          headsetRects.push({ mode, x, y, w: size, h: size });
          const tile = (dx, dy, w, h, color, z) => rect(W/2 - cx - (dx+w)*unit, H/2 - cy - (dy+h)*unit, z, w*unit, h*unit, color);
          tile(-9, -8, 18, 17, INK, 0.000004);
          tile(-6, -6, 12, 1, ink, 0);
          tile(-7, -5, 1, 5, ink, 0); tile(6, -5, 1, 5, ink, 0);
          tile(-6, 0, 4, 1, ink, 0); tile(2, 0, 4, 1, ink, 0);
          tile(-2, -1, 1, 1, ink, 0); tile(1, -1, 1, 1, ink, 0);
          tile(-1, -2, 2, 1, ink, 0);
          small(mode === 'immersive-vr' ? 'VR' : 'MR', cx-W/2, H/2-cy-2*unit, 0, unit, PAPER);
        });
        if (message) small(message, row.x+row.w-W/2-smallWidth(message)*unit/2, H/2-row.y+9*unit, 0, unit, PAPER);
      });
      projector.overlay(buttons, W, H);
    }
    const codeTag = focusObject?.code && !radial && !menu && !bare ? joinCode(`https://prompt.ac/$${focusObject.code}`) : null;
    const codeAt = codeTag ? hudPack.reserve('code', 'br', (codeTag.length + 1) * hud.unit, (9 + codeTag.length + 2) * hud.unit) : null;
    if (buildLines.length) {
      const { unit } = hud;
      const top = promptY + promptRows * 10 * unit;
      hudPack.claim('builds', { x: promptX, y: top, w: buildWidth, h: buildLines.length * 10 * unit });
      const m = model(({ rect: worldRect, text }) => {
        const rect = flip(worldRect);
        buildLines.forEach((spans, i) => {
          const y = H / 2 - top - (2 + i * 10) * unit;
          drawSpans({ rect, text }, spans, -W / 2 + promptX + spanWidthOf(spans, unit) / 2, y, unit, { shadow: INK });
        });
      });
      projector.overlay(m, W, H);
    }
    if (stats) {
      const { unit } = hud, at = camera || me;
      const rows = [{ label: 'tri', value: lensFull, text: `${Math.round(lensFull * 100)}%` }];
      if (weather && at) {
        const air = airAt(weather, at.x + (place?.x || 0), at.y, at.z + (place?.z || 0), Date.now() / 1000, { day: sun(phase()) });
        const bearing = (Math.round(Math.atan2(air.x, air.z) / (Math.PI / 4)) + 8) % 8;
        const compass = ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw'][bearing];
        rows.push(`wind ${compass} ${Math.hypot(air.x, air.z).toFixed(1)}`, `lift ${air.y.toFixed(1)}`);
      }
      // The same widget as every AC piece's (frame-chart.mjs), in hud units.
      let fit = frameChart.layout(rows, { screenW: W / unit, screenH: H / unit, top: chartTop / unit });
      if (stallNote) {
        let line = '';
        for (const word of stallNote.split(' ')) {
          if (line && smallWidth(line + ' ' + word) > fit.width - 6) {
            rows.push(line);
            line = '';
          }
          line += (line ? ' ' : '') + word;
        }
        rows.push(line);
        fit = frameChart.layout(rows, { screenW: W / unit, screenH: H / unit, top: chartTop / unit });
      }
      const left = fit.left * unit - W / 2, top = H / 2 - fit.top * unit;
      const m = model(({ rect: worldRect, small }) => {
        const rect = flip(worldRect);
        frameChart.draw({
          box: (x, y, w, h, color, layer) => rect(left + x * unit, top - (y + h) * unit, (4 - layer) * 0.000001, w * unit, h * unit, color),
          text: (line, x, y, color) => small(line, left + (x + smallWidth(line) / 2) * unit, top - (y + 1) * unit, 0, unit, color),
        }, rows, fit);
      });
      hudOverlay('debug-chart', m, W, H);
    }
    // The corner block: the prompt's own pink cursor block at the top left,
    // where the system label sits on every AC piece (SCREEN.md). A tap on it
    // goes back to the prompt.
    cornerRect = null;
    if (corner && !prompt?.open && !chat?.open && !jobs.some((j) => j.owner === owner) && !buildLines.length) {
      const { unit } = hud;
      const safeX = grid ? 6 + bump : hud.safeX, safeY = grid ? 6 + bump : hud.safeY;
      const w = 6 * unit,
        h = 10 * unit;
      const left = -W / 2 + safeX,
        bottom = H / 2 - safeY - h;
      // Beside it, what Escape does next: with the mouse held it frees the mouse, and with the mouse free it goes to the prompt.
      const label = input === 'touch' ? [] : hintSpans(input === 'pad' ? '[START] to prompt' : locked ? '{esc} to use mouse' : '{esc} to prompt', THEME.dim); // (a pad's way back is START)
      const labelW = smallSpanWidth(label, unit);
      cornerRect = input === 'touch'
        ? { x: safeX, y: safeY, w: Math.max(w, 44 * pixel - safeX), h: Math.max(h, 44 * pixel - safeY) }
        : { x: safeX, y: safeY, w: w + 4 * unit + labelW, h };
      const m = model(({ rect: worldRect, text, small }) => {
        const rect = flip(worldRect);
        drawSpans({ rect, text, small }, label, left + w + 4 * unit + labelW / 2, H / 2 - safeY - 2 * unit, unit, { shadow: INK, tiny: true });
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
      hudOverlay('prompt-button', m, W, H);
    }
    // ⎋ A step of Escape: the full border narrows over a third of a second.
    if (flash && now - flash.at < 360) {
      const wide = Math.max(1, Math.round(8 * (1 - (now - flash.at) / 360))) * hud.unit;
      const m = model(({ rect: worldRect }) => {
        const rect = flip(worldRect);
        rect(-W / 2, -H / 2, 0.000004, wide, H, flash.color);
        rect(W / 2 - wide, -H / 2, 0.000004, wide, H, flash.color);
        rect(-W / 2, H / 2 - wide, 0.000004, W, wide, flash.color);
        rect(-W / 2, -H / 2, 0.000004, W, wide, flash.color);
      });
      projector.overlay(m, W, H);
    }
    const occupiedRows = promptRows + buildLines.length + (cornerRect ? 1 : 0);
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
      const top = H / 2 - safeY - (occupiedRows ? (10 * occupiedRows + 3) * unit : 0);
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
      hudOverlay('dev-log', devModel, W, H);
    }
    // Offline: no panel over the room. The top-left text spits the
    // connection's log a letter at a time under the corner block, as a
    // machine going down and coming back up would (2026-11-15).
    if (!offline) netSince = null;
    if (offline && !menu) {
      const { unit } = hud;
      netSince ??= now;
      const safeX = grid ? 6 + bump : hud.safeX, safeY = grid ? 6 + bump : hud.safeY;
      const cols = Math.max(12, Math.min(48, Math.floor((W - 2 * safeX) / (6 * unit))));
      // Only this outage: from the first bad word after the last good one
      // that came before it.
      let from = 0;
      netLog.forEach((l, i) => { if (l.good && netLog.slice(i + 1).some((x) => x.bad)) from = i + 1; });
      const outage = netLog.slice(from).slice(-8);
      const back = !!outage.at(-1) && !outage.at(-1).bad;
      const lines = [
        { text: 'rax is shutting down', color: 0xff6b6b },
        ...outage.map((l) => ({ text: `${l.when} ${l.text}`.slice(0, cols), color: l.bad ? 0xff6b6b : l.good ? 0x3fb950 : THEME.text })),
        { text: back ? 'rebooting' : 'waiting for the server', color: THEME.dim },
      ];
      // Sixty letters a second, counted from the first line: a line that
      // arrives late starts typing when the ones above it are done.
      let budget = Math.floor((now - netSince) / 16);
      const shown = [];
      for (const l of lines) {
        if (budget <= 0) break;
        shown.push({ ...l, text: l.text.slice(0, budget), done: budget >= l.text.length });
        budget -= l.text.length + 6;
      }
      const blink = now % 600 < 400;
      const key = JSON.stringify([shown, blink, W, H, safeX, safeY, occupiedRows, unit]);
      if (key !== netKey) {
        netKey = key;
        netModel = model(({ rect: worldRect, text }) => {
          const rect = flip(worldRect);
          shown.forEach((l, i) => {
            const last = i === shown.length - 1;
            const spans = [{ text: l.text, color: l.color }, ...(last && (blink || !l.done) ? [{ block: true }] : [])];
            const y = H / 2 - safeY - 2 * unit - (i + occupiedRows) * 10 * unit;
            drawSpans({ rect, text }, spans, -W / 2 + safeX + spanWidthOf(spans, unit) / 2, y, unit, { shadow: INK });
          });
        });
      }
      hudOverlay('network', netModel, W, H);
    }
    // 📟 Not down, but worth a word: one line where the outage's log goes.
    if (wire && !offline && !menu && !prompt.open) {
      const { unit } = hud;
      const safeX = grid ? 6 + bump : hud.safeX, safeY = grid ? 6 + bump : hud.safeY;
      const m = model(({ rect: worldRect, text }) => {
        const rect = flip(worldRect);
        const spans = [{ text: wire.text, color: wire.color ?? THEME.text }];
        drawSpans({ rect, text }, spans, -W / 2 + safeX + spanWidthOf(spans, unit) / 2, H / 2 - safeY - 2 * unit - occupiedRows * 10 * unit, unit, { shadow: INK });
      });
      hudOverlay('connection', m, W, H);
    }
    // A square reticle: four L-shaped corners on whole pixels, symmetric
    // about the centre, each with a one-pixel shadow. It opens with speed,
    // closes and colours on an object (green when yours), in pixel steps.
    // The reticle holds through the paper's drop and return (2026-11-03): the
    // prompt's crosshair fades with the paper, so this one must already be
    // there, solid, under it, and stay until the paper is whole again.
    const onPanel = !lockedOn && panels.some((p) => p.cursor); // 🖼️ on a world panel it is the panel's own mouse (below)
    if ((me?.alive || reticle || (spectator && (lockCursor || screenCursor))) && !radial && !menu && (!bare || reticle) && !onPanel && game?.hud?.reticle !== false) {
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
      let box = null, cursorRegion = null;
      if (lockedOn && focusObject) {
        let world = null;
        if (focusObject.box) world = focusObject.box; // (a standing screen, or a thing in another room: its box is told)
        else if (focusObject.wall) {
          const w = walls.find((x) => `wall:${x.code}` === focusObject.id);
          if (w) {
            const d = SCREEN - 0.03, top = 1.5 + 18;
            world = w.face === 'north'
              ? { xMin: -16, xMax: 16, yMin: 1.5, yMax: top, zMin: d, zMax: d }
              : { xMin: -d, xMax: -d, yMin: 1.5, yMax: top, zMin: -16, zMax: 16 };
          }
        } else if (focusObject.item) {
          const it = poses.get(focusObject.id);
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
            const limitX = W / 2 - 2 * unit, limitY = H / 2 - 2 * unit;
            cursorRegion = {
              left: clamp(Math.round(l * W / 2), limitX), right: clamp(Math.round(r * W / 2), limitX),
              bottom: clamp(Math.round(b * H / 2), limitY), top: clamp(Math.round(u * H / 2), limitY),
            };
            box = {
              left: clamp(cursorRegion.left - pad, limitX), right: clamp(cursorRegion.right + pad, limitX),
              bottom: clamp(cursorRegion.bottom - pad, limitY), top: clamp(cursorRegion.top + pad, limitY),
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
      let cursorX, cursorY;
      if (screenCursor) {
        const { face, cursor } = screenCursor;
        const across = (0.5 - cursor.x / face.cols) * face.w, angle = face.yaw * Math.PI / 180;
        const point = projector.project(face.x + across * Math.cos(angle), face.y + (0.5 - cursor.y / face.rows) * face.h, face.z - across * Math.sin(angle));
        if (point) { cursorX = Math.round(point.x * W / 2); cursorY = Math.round(point.y * H / 2); }
      } else if (lockCursor) {
        const region = cursorRegion || { left: -half, right: half, top: half, bottom: -half };
        cursorX = Math.round(region.left + (region.right - region.left) * lockCursor.x);
        cursorY = Math.round(region.top + (region.bottom - region.top) * lockCursor.y);
      }
      const m = model(({ rect: worldRect, text }) => {
        // rect(left, bottom, w, h): screen x right, y up, whole pixels.
        const rect0 = flip(worldRect);
        const rect = lockCursor || screenCursor ? rect0 : (x, y, z, w, h, color) => rect0(x + cx0, y + cy0, z, w, h, color);
        // The highlighted object's offer, under its bracket (or the reticle).
        if (objectHintSpans) {
          const cx = box ? (box.left + box.right) / 2 : 0, under = (box ? box.bottom : -half) - 4 * unit;
          drawSpans({ rect, text }, objectHintSpans, cx, under, unit, { shadow: INK });
        }
        const L = (x, y, w, h) => {
          rect(x + t, y - t, 0.000002, w, h, INK);
          rect(x, y, 0, w, h, color);
        };
        if (!screenCursor && (box || pointerSource !== 'touch')) for (const sx of [-1, 1]) // (a pad has the corners too: they are what goes green on a thing of yours)
          for (const sy of [-1, 1]) {
            // The corner's outer pixel, then arms running inward.
            const x = box ? (sx < 0 ? box.left - t : box.right) : sx < 0 ? -half - t : half,
              y = box ? (sy < 0 ? box.bottom - t : box.top) : sy < 0 ? -half - t : half;
            L(sx < 0 ? x : x - arm + t, y, arm, t);
            L(x, sy < 0 ? y : y - arm + t, t, arm);
          }
        // The brackets straddle a pixel boundary. An even-sized dot can sit
        // exactly on it at every HUD scale, including over a locked target.
        const dot = (x, y) => {
          rect(x - 2 * t, y - 2 * t, 0.000002, 4 * t, 4 * t, INK);
          rect(x - t, y - t, 0, 2 * t, 2 * t, 0xffffff);
        };
        if (Number.isFinite(cursorX)) {
          if (pointerSource === 'pad') {
            dot(cursorX, cursorY);
          } else if (pointerSource === 'mouse') for (const [dx, dy, ink] of reticleStrokes) {
            const shadow = ink === cursorInks.shadow;
            rect(cursorX + dx * t, cursorY - dy * t, shadow ? 0.000002 : 0, t, t, shadow ? INK : 0x00ffff);
          }
        } else if (!screenCursor && pointerSource === 'mouse') L(-t, -t, 2 * t, 2 * t);
        else if (!screenCursor && pointerSource === 'pad') {
          dot(0, 0);
        }
      });
      projector.overlay(m, W, H);
    }
    // 🎯 Looking on, everyone's aim is on the screen too: where each body
    // looks (down its yaw and pitch to the floor, or forty metres out) is
    // projected into this view and marked with that seat's own small reticle,
    // drawn on whole pixels like your own (2026-11-27).
    if (spectator && !bare && !radial && !menu) {
      const { unit } = hud, t = unit, half = 4 * unit, arm = 2 * unit;
      const marks = [];
      players.forEach((p, i) => {
        if (!p || p.alive === false || !Number.isFinite(p.yaw)) return;
        const dir = direction(p.yaw, p.pitch || 0);
        const reach = dir.y < -0.02 ? Math.min(40, p.y / -dir.y) : 40;
        const at = along(p, dir, reach), s = projector.project(at.x, at.y, at.z);
        if (s && Math.abs(s.x) < 1 && Math.abs(s.y) < 1) marks.push({ x: Math.round((s.x * W) / 2), y: Math.round((s.y * H) / 2), color: seatColor(i) });
      });
      if (marks.length) {
        const m = model(({ rect: worldRect }) => {
          const rect = flip(worldRect);
          for (const { x: cx, y: cy, color } of marks) {
            const L = (x, y, w, h) => {
              rect(cx + x + t, cy + y - t, 0.000002, w, h, INK);
              rect(cx + x, cy + y, 0, w, h, color);
            };
            for (const sx of [-1, 1])
              for (const sy of [-1, 1]) {
                const x = sx < 0 ? -half - t : half, y = sy < 0 ? -half - t : half;
                L(sx < 0 ? x : x - arm + t, y, arm, t);
                L(x, sy < 0 ? y : y - arm + t, t, arm);
              }
            L(-t, -t, 2 * t, 2 * t); // (the point itself)
          }
        });
        projector.overlay(m, W, H);
      }
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
        // (A module is one HUD unit, as every code on the screen is drawn: 2026-11-20. It was one buffer pixel, finer than the pairing code beside it.)
        const n = cells.length, qrOffsetY = 9, s = hud.unit;
        const { x: left, y: top, w: width, h: height } = codeAt || { x: W - (n + 1) * s - 4, y: H - (qrOffsetY + n + 2) * s - 4, w: (n + 1) * s, h: (qrOffsetY + n + 2) * s };
        qrRect = { x: left, y: top, w: width, h: height };
        const key = `${url}:${W}:${H}:${left}:${top}:${s}`;
        if (qrModel?.key !== key) {
          const m = model(({ rect: worldRect, small }) => {
            const rect = flip(worldRect);
            // Coordinates share the 2D runtime's top-left pixel convention.
            const pixel = (x, y, w, h, color, z = 0.000001) =>
              rect(-W / 2 + left + x * s, H / 2 - top - (y + h) * s, z, w * s, h * s, color);
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
            small(label, -W / 2 + left + (labelX + 1 + smallWidth(label) / 2) * s,
              H / 2 - top - 2 * s, -0.000001, s, 0xffffff);
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
      const floor = handleY;
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
            v[at++] = p[2] * 0.001 - (projector.under || 0);
            v[at++] = p[3];
            v[at++] = p[4];
            v[at++] = p[5];
          }
        }
      }
    }
    // 👁️ Looking on there is no face: the connection and where the eye is, in
    // the world's metres, stand where the face's column would, bottom left.
    // 📱 Signed out, the sign-in code stands in the face's place and that column beside it.
    pairAt = pair && !face && !bare && !menu && !game ? pairLayout(pair, hud, W, H, panelTop, pixelScale) : null;
    if (pairAt) hudOverlay('pairing', pairModel(pairAt, flip), W, H);
    if (!face && !bare && !game && camera && !me) {
      const { unit, safeX, safeY } = hud;
      const x0 = -W / 2 + safeX + 2 + (pairAt ? pairAt.size + 4 * unit : 0), y0 = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit) + 8 * unit;
      const m = model(({ rect: worldRect, small }) => {
        const rect = flip(worldRect);
        const ping = (connection?.label || '').toLowerCase(), bars = connection?.bars || 0;
        const pingColor = !ping ? 0x8b90a0 : bars >= 3 ? 0x3fb950 : bars >= 2 ? 0xffe066 : 0xff6b6b;
        let row = 20;
        if (ping) {
          for (let i = 0; i < 4; i++) {
            const h = 2 + i * 2, lit = i < bars;
            rect(x0 + (i * 3 + 1) * unit, y0 + (row - 8) * unit, 0.000002, 2 * unit, h * unit, INK);
            rect(x0 + i * 3 * unit, y0 + (row - 7) * unit, 0.000001, 2 * unit, h * unit, lit ? pingColor : 0x2c3040);
          }
          const tx = x0 + (14 + smallWidth(ping) / 2) * unit, ty = y0 + row * unit;
          small(ping, tx + unit, ty - unit, 0.000002, unit, INK);
          small(ping, tx, ty, 0, unit, pingColor);
          row -= 10;
        }
        let left = x0;
        for (const [t, c] of !stats ? [] : [['x ' + (camera.x + (place?.x || 0)).toFixed(1), AXIS.x], ['y ' + (camera.y || 0).toFixed(1), AXIS.y], ['z ' + (camera.z + (place?.z || 0)).toFixed(1), AXIS.z]]) {
          const cx = left + (smallWidth(t) / 2) * unit, cy = y0 + row * unit;
          small(t, cx + unit, cy - unit, 0.000002, unit, INK);
          small(t, cx, cy, 0, unit, c);
          left += (smallWidth(t) + 6) * unit;
        }
      });
      hudOverlay('drone-status', m, W, H);
    }
    if (face && !bare) {
      // A face cam, bottom left above your handle: an original pixel portrait
      // in the player's own look that reacts to what happens to them.
      const { unit, safeX, safeY } = hud;
      const floor = handleY;
      const listTop = floor + 4 * unit;
      // (The cam's frame stands on the same left edge as the handle under it, 2026-11-27.)
      const S = 24 * unit,
        x0 = handleX + unit,
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
        // Braincells and connection details sit in even 10-row slots, so no
        // two overlap (2026-11-08). A slot's number is the top of small()'s
        // 8-row glyph box; the glyphs themselves start a row lower.
        let slot = 26;
        const nextSlot = () => ((slot -= 10), slot + 10); // (a clear row between each line)
        // Three rows, the cam's own height: the connection (four rising bars
        // and its ping), where you stand, your braincells.
        {
          const ping = (connection?.label || '').toLowerCase();
          const bars = connection?.bars || 0;
          const good = 0x3fb950, fair = 0xffe066, poor = 0xff6b6b;
          const pingColor = !ping ? 0x8b90a0 : bars >= 3 ? good : bars >= 2 ? fair : poor;
          if (ping) {
            const pt = nextSlot();
            const base = pt - 7; // The bars' foot on the glyphs' foot.
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
            // 🏙️ Which server answers, to the right of the ping: its city, or its host.
            if (served) {
              const sx = x0 + (40 + smallWidth(ping) + 5 + smallWidth(served) / 2) * unit;
              small(served, sx + unit, ty - unit, 0.000002, unit, INK);
              small(served, sx, ty, 0, unit, 0x8b90a0);
            }
          }
        // Braincells beside the cam: a pixel brain and the count, in purple.
        if (Number.isFinite(braincells)) {
          const bt = nextSlot();
          const BR = ['.##.##.', '#######', '###.###', '#######', '.##.##.'];
          BR.forEach((row, r) =>
            [...row].forEach((ch, c) => ch === '#' && px(27 + c + 1, bt - 1 - r - 1, 1, 1, INK, 0.000002)),
          );
          BR.forEach((row, r) =>
            [...row].forEach((ch, c) => ch === '#' && px(27 + c, bt - 1 - r, 1, 1, r === 2 && c === 3 ? 0x8e44ff : 0xb46bff)),
          );
          // Exact, with thousands separators, shadowed for contrast.
          const n = Math.round(braincells).toLocaleString('en-US');
          const nx = x0 + (36 + smallWidth(n) / 2) * unit, ny = y0 + bt * unit;
          small(n, nx + unit, ny - unit, 0.000002, unit, INK);
          small(n, nx, ny, 0, unit, spending ? 0xf0d8ff : 0xd7a6ff);
          // Spending: purple pixels fly up and away from the count.
          if (spending && Math.random() < 0.7)
            cellBits.push({ x: 36 + Math.random() * n.length * 4, y: bt + 1, vx: (Math.random() - 0.3) * 30, vy: 20 + Math.random() * 30, born: now });
          for (let k = cellBits.length - 1; k >= 0; k--) {
            const b = cellBits[k], t = (now - b.born) / 1000;
            if (t > 0.9) { cellBits.splice(k, 1); continue; }
            px(Math.round(b.x + b.vx * t), Math.round(b.y + b.vy * t - 20 * t * t), 1, 1, t < 0.3 ? 0xf0d8ff : t < 0.6 ? 0xb46bff : 0x8e44ff);
          }
        }
          // 📍 Under the braincells: the space you are in and where you stand
          // in it, in metres, each axis in its colour. Always shown (it was
          // the debugging layer's until 2026-11-27).
          if (me) {
            const pt = nextSlot();
            let left = x0 + 27 * unit;
            for (const [t, c] of [...(roomName ? [[roomName, 0xd0d4e0]] : []), ['x ' + (me.x + (place?.x || 0)).toFixed(1), AXIS.x], ['y ' + (me.y || 0).toFixed(1), AXIS.y], ['z ' + (me.z + (place?.z || 0)).toFixed(1), AXIS.z]]) {
              const cx = left + (smallWidth(t) / 2) * unit, cy = y0 + pt * unit;
              small(t, cx + unit, cy - unit, 0.000002, unit, INK);
              small(t, cx, cy, 0, unit, c);
              left += (smallWidth(t) + 6) * unit;
            }
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
      hudOverlay('avatar', m, W, H);
    }
    // ❄️ Snow falling into the open room (2026-11-03): small camera-facing
    // flakes in a volume over and around the room, depth-tested against the
    // walls, drifting as they fall, nearer ones a little larger.
    // 🌧️ Rain, when it rains over Los Angeles: thin streaks that fall fast and
    // lean with the wind, more of them the harder it comes down.
    if (weather?.rain > 0) {
      if (!rain) {
        rain = [];
        for (let i = 0; i < 520; i++)
          rain.push({ x: (Math.random() - 0.5) * 34, y: Math.random() * 14, z: (Math.random() - 0.5) * 34, v: 9 + Math.random() * 5 });
      }
      const dt = Math.min(0.1, (now - (rainAt || now)) / 1000);
      rainAt = now;
      const r = projector.right, lean = Math.min(0.35, (weather.wind || 0) * 0.03), blown = heading(weather), count = Math.round(rain.length * Math.min(1, 0.15 + weather.rain));
      const cx = eye?.x || 0, cz = eye?.z || 0;
      const m = model(({ quad }) => {
        for (let i = 0; i < count; i++) {
          const f = rain[i];
          f.y -= f.v * dt;
          f.x += f.v * lean * blown.x * dt;
          f.z += f.v * lean * blown.z * dt;
          if (f.y < 0.02) { f.y = 13 + Math.random(); f.x = (Math.random() - 0.5) * 34; f.z = (Math.random() - 0.5) * 34; }
          const x = cx + f.x, z = cz + f.z, w = 0.02, long = 0.5;
          quad([x - r[0] * w, f.y, z - r[2] * w], [x + r[0] * w, f.y, z + r[2] * w],
            [x + r[0] * w - blown.x * lean * long, f.y + long, z + r[2] * w - blown.z * lean * long], [x - r[0] * w - blown.x * lean * long, f.y + long, z - r[2] * w - blown.z * lean * long], 0x5f7fb0);
        }
      });
      draw(m, 0, 0, 0);
    }
    // 🌬️ The air seen: pale motes that ride it (air.mjs), each drawn as a
    // short streak along the way it is going, so the wind's set, its gusts
    // and what rises show. More of them the harder it blows.
    if (weather && orbit <= 0) {
      if (!motes) {
        motes = [];
        for (let i = 0; i < 150; i++) motes.push({ x: (Math.random() - 0.5) * 36, y: Math.random() * 16 - 4, z: (Math.random() - 0.5) * 36, vx: 0, vy: 0, vz: 0 });
      }
      const dt = Math.min(0.1, (now - (motesAt || now)) / 1000);
      motesAt = now;
      const cx = eye.x, cy = eye.y, cz = eye.z, when = Date.now() / 1000, day = sun(phase());
      const count = Math.round(motes.length * Math.min(1, 0.35 + (weather.wind || 0) / 8 + (weather.gust || 0) / 24));
      const r = projector.right, ink = phase() > 0.27 && phase() < 0.73 ? 0xf2f6ff : 0x9fb0d0;
      const m = model(({ quad }) => {
        for (let i = 0; i < count; i++) {
          const f = motes[i];
          const a = airAt(weather, cx + f.x + (place?.x || 0), Math.max(0.3, cy + f.y), cz + f.z + (place?.z || 0), when, { day });
          f.vx = a.x; f.vy = a.y; f.vz = a.z;
          f.x += a.x * dt; f.y += a.y * dt; f.z += a.z * dt;
          // Out of the box round the eye: back in on the far side.
          if (f.x > 18) f.x -= 36; else if (f.x < -18) f.x += 36;
          if (f.z > 18) f.z -= 36; else if (f.z < -18) f.z += 36;
          if (f.y > 12) f.y -= 16; else if (f.y < -4 || cy + f.y < 0.2) f.y += 16;
          const x = cx + f.x, y = cy + f.y, z = cz + f.z, w = 0.012, tail = 0.16;
          quad([x - r[0] * w, y - w, z - r[2] * w], [x + r[0] * w, y - w, z + r[2] * w],
            [x + r[0] * w - a.x * tail, y + w - a.y * tail, z + r[2] * w - a.z * tail], [x - r[0] * w - a.x * tail, y + w - a.y * tail, z - r[2] * w - a.z * tail], ink);
        }
      });
      draw(m, 0, 0, 0);
    }
    if (weather?.snow > 0) {
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
          // Edge to edge: neighbours of one colour read as one field (2026-11-10).
          // 🧩 Laid as sixteen half-metre pieces, not one two-metre square: on a planet this small (and a floor of the tower bends with it) a straight two-metre edge cuts more than a centimetre under the round, and the floor beneath, cut finer, came up through the paint in dark shards (2026-11-20). Half a metre cuts under by less than a millimetre.
          for (let a = 0; a < 4; a++)
            for (let b = 0; b < 4; b++) {
              const x0 = x + a * 0.5, z0 = z + b * 0.5;
              quad([x0, 0.005, z0], [x0 + 0.5, 0.005, z0], [x0 + 0.5, 0.005, z0 + 0.5], [x0, 0.005, z0 + 0.5], col);
            }
        }
      });
    }
    if (tileModel) draw(tileModel, 0, 0, 0);
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
        w.model = screen(model(({ quad }) => {
          const north = w.face === 'north';
          const glass = glassAt(eye, north ? 0 : -SCREEN, base + tall / 2, north ? SCREEN : 0,
            north ? 0 : Math.PI / 2, span, today?.horizon ?? PALETTES[paletteName].sky, w.code, zoom);
          gridRuns(px, cols, rows, glass, (i, end, j, c) => {
            const u0 = span / 2 - i * cell, u1 = span / 2 - end * cell;
            const y1 = base + tall - j * cell, y0 = y1 - cell;
            const d = SCREEN - 0.03;
            // While Aesel remixes it, the screen splits in two in space:
            // the program keeps the left half, the new source the right.
            const sq = remix ? 0.5 : 1, shift = remix ? span / 4 : 0;
            const a = u0 * sq + shift, b = u1 * sq + shift;
            if (w.face === 'north') quad([a, y0, d], [b, y0, d], [b, y1, d], [a, y1, d], c);
            else quad([-d, y0, -a], [-d, y0, -b], [-d, y1, -b], [-d, y1, -a], c);
          });
        }));
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
    // field, facing whoever asked. How fine its grid is and how often its
    // program steps follow the eye (screen-lod.mjs): the screen says what
    // it wants, the page loads that grid, and it draws from the one it has
    // until then.
    const gaze = Math.hypot(aim.x - eye.x, aim.z - eye.z) || 1;
    const look = { x: (aim.x - eye.x) / gaze, z: (aim.z - eye.z) / gaze };
    for (const s of screens) {
      const span = SCREEN_SPAN, base = 0.4, y = s.y || 0;
      const far = Math.hypot(s.x - eye.x, y + 1.525 - eye.y, s.z - eye.z) / zoom;
      const detail = screenDetail(far, { cols: s.cols || 0, seen: screenSeen(eye, look, s) });
      const yaw = ((s.yaw || 0) * Math.PI) / 180;
      draw(standingFrame, s.x, y, s.z, yaw);
      if (scenery) scenery.screens.push({ id: s.id, x: s.x, y, z: s.z, cols: s.cols || 0 });
      s.want = detail.hz || far < 32 ? detail.cols : 0;
      const ready = screenPrograms.get(`${s.programKey}@${detail.cols}`);
      const w = s.pressed || ready || (s.cols && screenPrograms.get(`${s.programKey}@${s.cols}`));
      s.program = w;
      if (!w) {
        if (s.model) draw(s.model, s.x, y, s.z, yaw);
        continue;
      }
      w.usedAt = now;
      const cols = w.width, rows = w.height, cell = span / cols, tall = (span * rows) / cols;
      // Keep the current grid on screen until the next tier has pixels.
      if (w.steppedAt == null || (detail.hz && now - w.steppedAt >= 1000 / detail.hz)) {
        w.steppedAt = now;
        w.step();
        w.frames = (w.frames || 0) + 1;
      }
      const glass = detail.hz ? glassAt(eye, s.x, y + base + tall / 2, s.z, yaw, span, today?.horizon ?? PALETTES[paletteName].sky, s.id, zoom) : null;
      if (!s.model || s.cols !== cols || s.drawn !== w.frames || s.glassKey !== glass?.key) {
        s.glassKey = glass?.key;
        s.cols = cols;
        s.drawn = w.frames;
        const px = w.pixels;
        s.model = gridModel(px, cols, rows, span, tall, base, glass);
      }
      draw(s.model, s.x, y, s.z, yaw);
    }
    // 🖼️ World panels (panel.mjs): a piece's pixels on a rectangle in the
    // room, drawn as a standing screen's are, on a slab. Where the aim meets
    // one its virtual mouse is laid on the face: the crosshair, come to rest
    // on the panel (and so not drawn in the middle of the picture, above).
    for (const p of panels) {
      let kept = panelModels.get(p.id);
      if (!kept) panelModels.set(p.id, (kept = { frame: -1 }));
      const yaw = ((p.yaw || 0) * Math.PI) / 180;
      const glass = p.label || p.id === 'car-pad'
        ? glassAt(eye, p.x, p.y, p.z, yaw, p.w, today?.horizon ?? PALETTES[paletteName].sky, p.id, zoom) : null;
      const distance = Math.hypot(p.x - eye.x, p.y - eye.y, p.z - eye.z) / zoom;
      const tier = screenDetail(distance * SCREEN_SPAN / p.w, { cols: kept.tier });
      kept.tier = tier.cols;
      const cols = p.cursor ? p.cols : Math.min(p.cols, tier.cols);
      if (kept.frame !== p.frame || kept.glassKey !== glass?.key || kept.cols !== cols) {
        kept.glassKey = glass?.key;
        kept.frame = p.frame;
        const rows = Math.max(1, Math.round(cols * p.rows / p.cols));
        if (cols !== p.cols) {
          if (kept.cols !== cols) kept.pixels = new Uint8ClampedArray(cols * rows * 4);
          carryPixels(p.pixels, p.cols, p.rows, kept.pixels, cols, rows);
        }
        kept.cols = cols;
        kept.face = gridModel(cols === p.cols ? p.pixels : kept.pixels, cols, rows, p.w, p.h, -p.h / 2, glass);
      }
      kept.slab ||= model(({ box }) => box(0, 0, 0.004 + p.deep / 2, p.w + 0.06, p.h + 0.06, p.deep, 0x2a2d3a, 0x22252f, 0x2a2d3a));
      draw(kept.slab, p.x, p.y, p.z, yaw);
      draw(kept.face, p.x, p.y, p.z, yaw);
      if (scenery) scenery.panels.push({ id: p.id, x: p.x, y: p.y, z: p.z, cols: kept.cols });
      if (!p.cursor || pointerSource === 'touch') continue;
      // The mouse: AC's one software cursor (lib/cursor-shape.mjs, the same mark a pad's stick moves on the prompt's paper), laid on the panel's own pixels a hair proud of the face: a soft dab of light, and over it a dot and four arms on their shadow; closed up and yellow while pressed.
      const cell = p.w / p.cols, row = p.h / p.rows, mx = Math.floor(p.cursor.x), my = Math.floor(p.cursor.y);
      // (Nothing here is see-through, so the glow is mixed into the panel's own pixels under it: lib/cursor-shape.mjs cursorOver. One face a pixel, a hair proud of the glass.)
      const lit = cursorOver((dx, dy) => {
        const px = mx + dx, py = my + dy;
        if (px < 0 || py < 0 || px >= p.cols || py >= p.rows) return null;
        const k = (py * p.cols + px) * 4;
        return [p.pixels[k], p.pixels[k + 1], p.pixels[k + 2]];
      }, !!p.cursor.down, pointerSource === 'mouse');
      projector.draw(model(({ quad }) => {
        for (const [key, [r, g, b]] of lit) {
          const [dx, dy] = key.split(',').map(Number), z = -0.003;
          const x = p.w / 2 - (mx + dx + 0.5) * cell, y = p.h / 2 - (my + dy + 0.5) * row;
          quad([x + cell / 2, y - row / 2, z], [x - cell / 2, y - row / 2, z], [x - cell / 2, y + row / 2, z], [x + cell / 2, y + row / 2, z], (Math.round(r) << 16) | (Math.round(g) << 8) | Math.round(b));
        }
      }), p.x, p.y, p.z, yaw);
    }
    menuBox = null;
    if (menu) {
      // A modal: the scene dimmed behind it, a panel with a title and a
      // left-aligned list, the chosen row on yellow. Drawn nearest of all.
      const { unit } = hud;
      // A list with thumbnails (the $ history) gets taller rows, each led by
      // its object; a long list scrolls a window around the chosen row.
      const pictured = menu.items.some((r) => r.thumb);
      const lineH = (pictured ? TINY + 3 : 12) * unit;
      const fits = Math.max(3, Math.min(12, Math.floor((H - 40 * unit) / lineH) - 1));
      const first = Math.max(0, Math.min(menu.selected - Math.floor(fits / 2), menu.items.length - fits));
      const rows = menu.items.slice(first, first + fits);
      const indent = pictured ? (TINY + 6) * unit : 0;
      const w = Math.min(W - 40 * unit, Math.max(170, ...rows.map((r) => r.label.length * 6 + 28)) * unit + indent);
      const titleH = 12 * unit,
        h = rows.length * lineH + titleH + 8 * unit;
      const z = -0.000009;
      const near = z - 0.000004; // text clearly in front of the row highlight
      menuBox = { w, h, titleH, lineH, first, count: rows.length, unit, W, H, tabs: [] };
      const m = model(({ rect: worldRect, text }) => {
        const rect = flip(worldRect);
        rect(-W / 2, -H / 2, z + 0.000004, W, H, 0x0b0c10);
        rect(-w / 2 - unit, -h / 2 - unit, z + 0.000003, w + 2 * unit, h + 2 * unit, LINE);
        rect(-w / 2, -h / 2, z + 0.000002, w, h, 0x15171e);
        const left = -w / 2 + 8 * unit;
        const more = menu.items.length > rows.length ? `  ${menu.selected + 1}/${menu.items.length}` : '';
        // The tabs along the top, the open one on the row's yellow; then
        // the place in a long list.
        let tx = left;
        (menu.tabs || []).forEach((name, i) => {
          const tw = name.length * 6 * unit, on = i === menu.tab;
          if (on) rect(tx - 3 * unit, h / 2 - 11 * unit, z + 0.000001, tw + 5 * unit, 10 * unit, 0xffe066);
          const span = [{ text: name, color: on ? INK : THEME.text }];
          drawSpans({ rect, text }, span, tx + tw / 2, h / 2 - 5 * unit, unit, { shadow: null, z: near });
          menuBox.tabs.push([tx - 3 * unit, tx + tw + 3 * unit]);
          tx += tw + 10 * unit;
        });
        if (more) {
          const count = [{ text: more.trim(), color: THEME.text }];
          drawSpans({ rect, text }, count, w / 2 - 8 * unit - spanWidthOf(count, unit) / 2, h / 2 - 5 * unit, unit, { shadow: null, z: near });
        }
        rows.forEach((r, k) => {
          const i = first + k;
          const top = h / 2 - titleH - 2 * unit - k * lineH;
          const y = top - (2 + Math.floor((lineH / unit - 12) / 2)) * unit; // centred in the row
          const on = i === menu.selected;
          if (on) rect(-w / 2 + 4 * unit, top - lineH + unit, z + 0.000001, w - 8 * unit, lineH - unit, 0xffe066);
          const pixels = r.thumb && thumbPixels(r.thumb);
          if (pixels) {
            // One rect per run of a colour along each pixel row.
            const x0 = left - 2 * unit,
              y0 = top - (TINY + 1) * unit;
            for (let py = 0; py < TINY; py++)
              for (let px = 0; px < TINY; ) {
                const c = pixels[py * TINY + px];
                let run = 1;
                while (px + run < TINY && pixels[py * TINY + px + run] === c) run++;
                rect(x0 + px * unit, y0 + (TINY - 1 - py) * unit, near, run * unit, unit, c);
                px += run;
              }
          }
          const spans = r.label.startsWith('$')
            ? [{ text: r.label.split(' ')[0], color: on ? INK : THEME.verb }, { text: ' ' + r.label.split(' ').slice(1).join(' '), color: on ? INK : THEME.text }]
            : [{ text: r.label, color: on ? INK : THEME.text }];
          drawSpans({ rect, text }, spans, left + indent + spanWidthOf(spans, unit) / 2, y, unit, { shadow: null, z: near });
        });
      });
      projector.overlay(m, W, H);
    }
    if (modeIcon) {
      const u = hud.unit;
      const { x, y } = modeBox;
      const icon = model(({ rect }) => {
        const pixel = (dx, dy, w, h, color, z = 0) => rect(W / 2 - x - (dx + w) * u, H / 2 - y - (dy + h) * u, z, w * u, h * u, color);
        pixel(0, 0, 11, 9, INK, 0.000003);
        if (modeIcon === 'cinematic') {
          pixel(1, 3, 6, 5, PAPER);
          pixel(2, 1, 2, 2, PAPER);
          pixel(5, 1, 2, 2, PAPER);
          pixel(7, 4, 1, 2, PAPER);
          pixel(8, 3, 2, 4, PAPER);
        } else {
          pixel(1, 1, 3, 1, 0x7fd4ff);
          pixel(7, 1, 3, 1, 0x7fd4ff);
          pixel(2, 2, 1, 2, PAPER);
          pixel(8, 2, 1, 2, PAPER);
          pixel(2, 4, 7, 2, PAPER);
          pixel(4, 5, 3, 1, 0xff6b9d, -0.000001);
          pixel(2, 6, 1, 2, PAPER);
          pixel(8, 6, 1, 2, PAPER);
        }
      });
      projector.overlay(icon, W, H);
    }
    previousRegions = hudPack.taken;
    if (stats) {
      const guides = model(({ rect }) => {
        for (const region of [...hudPack.taken, ...(screenFrame ? [{id:'screen', ...screenFrame}] : [])]) {
          const {x,y,w,h} = region, ink = region.id === 'screen' ? 0xffdb67 : 0x41c6ce;
          for (const [l,t,a,b] of [[x,y,w,1],[x,y+h-1,w,1],[x,y,1,h],[x+w-1,y,1,h]]) {
            if (a > 0 && b > 0) rect(W / 2 - l - a, H / 2 - t - b, -0.000005, a, b, ink);
          }
        }
      });
      projector.overlay(guides, W, H);
    }
    // 🫧 The bubble's skin, from inside the planet: after all the world, so what lies beyond shows through it (lens-host.mjs).
    if (under && projector.card && skyline && !ledges && orbit < 0.6) {
      const from = host.scene.triangleCount;
      projector.halvings = 13;
      projector.draw(towerParts().dome, skyline.dx, skyline.dy, skyline.dz);
      projector.halvings = 0;
      for (let v = host.scene.vertices, i = from * 18 + 3, end = host.scene.triangleCount * 18; i < end; i += 6) v[i] += 2;
    }
    // 🌑 The shadow volumes go last in the stream; the host counts them in
    // its stencil and shades what lies inside (scene3d-webgpu.mjs).
    let shadow = null;
    if (casting && (casters.length || ballCasters.length)) {
      const first = host.scene.triangleCount;
      // What the shadows may take of the frame: a share of it, so they cost no more than a few milliseconds to work out.
      const most = Math.min(first + SHADOW_MOST, host.scene.maxTriangles - 64);
      const light = { x: -lamp.x, y: -lamp.y, z: -lamp.z };
      let n = 0;
      for (let i = 0; i < casters.length && n < castFar.length; i += 6) {
        const x = casters[i + 1], y = casters[i + 2], z = casters[i + 3], yaw = casters[i + 4], scale = casters[i + 5];
        const cy = Math.cos(yaw), sy = Math.sin(yaw);
        for (const b of boxesOf(casters[i])) {
          if (n >= castFar.length) break;
          // Not the floor's dots and marks, nothing underground, nothing far overhead or far off.
          if (b.h * scale < 0.06 || Math.max(b.w, b.d) * scale < 0.06) continue;
          const top = (b.y + b.h / 2) * scale + y, foot = (b.y - b.h / 2) * scale + y;
          if (top < 0.05 || foot > eye.y + 40) continue;
          const wx = (b.x * cy + b.z * sy) * scale + x, wz = (-b.x * sy + b.z * cy) * scale + z, far = Math.hypot(wx - eye.x, wz - eye.z);
          if (far > shadowReach * zoom + Math.max(b.w, b.d) * scale) continue;
          castFar[n] = far / zoom; castFrom[n] = i; castBox[n] = b; castTop[n] = top; castOrder[n] = n; n++;
        }
      }
      // 🌑 Each box's solid is drawn by itself, the nearest the eye first
      // (one out of the picture costs nothing: project.mjs `unseen`). When
      // the frame can hold no more the far ones go without: a solid cut
      // short would shade wrongly, so each is there whole or not at all.
      // (Its edges are halved by the same rule as what it falls on: a
      // coarser one left lit patches inside a shadow.)
      castOrder.length = n;
      castOrder.sort((p, q) => castFar[p] - castFar[q]);
      projector.overflow = false;
      projector.horizon = Infinity; // (a solid is counted where it lies behind what is seen: its faces over the horizon are part of the count)
      const lightYaw = Math.atan2(lamp.x, lamp.z), lightPitch = Math.acos(Math.min(1, lamp.y));
      for (let i = 0; i < ballCasters.length; i += 3) {
        const before = host.scene.triangleCount;
        projector.draw(ballVolume, ballCasters[i], ballCasters[i+1], ballCasters[i+2], lightYaw, lightPitch);
        if (projector.overflow || host.scene.triangleCount > most) {
          host.scene.triangleCount = before;
          break;
        }
      }
      let spilt = 0;
      castFrame++;
      for (const k of castOrder) {
        const i = castFrom[k], before = host.scene.triangleCount;
        // (Each box's shadow is a low slab on the ground it stands on: shadow.mjs groundShadow. `?volumes` draws the long solids instead, to compare.)
        const end = SOLIDS
          ? shadowVolume(volumeFloats, 0, castBox[k], casters[i + 1], casters[i + 2], casters[i + 3], casters[i + 4], casters[i + 5], light, 0.02, reach(castTop[k], light))
          : groundShadow(volumeFloats, 0, castBox[k], casters[i + 1], casters[i + 2], casters[i + 3], casters[i + 4], casters[i + 5], light, casters[i + 2], reach(castTop[k], light));
        if (!end) continue;
        projector.draw(volumeFloats.subarray(0, end), 0, 0, 0);
        if (projector.overflow || host.scene.triangleCount > most) {
          // (One that does not fit is taken back, and with it everything
          // farther: the shadows' reach is drawn in to just short of it.)
          host.scene.triangleCount = before;
          shadowReach = Math.max(6, castFar[k] * 0.9);
          spilt++;
          projector.overflow = false;
          break;
        }
      }
      castBox.length = 0;
      const count = host.scene.triangleCount - first;
      // 🌑 How far from the eye a thing still casts is one distance, moved
      // with care so no shadow blinks: drawn in at once when they do not fit
      // or the drawing runs long (at most twice a second), let out again a
      // couple of metres a second and only while there is plenty to spare.
      if (cost > 12 && castFrame - reachCut > 30) { shadowReach = Math.max(6, shadowReach * 0.85); reachCut = castFrame; }
      else if (!spilt && cost < 8 && count < SHADOW_MOST * 0.55 && castFrame - reachCut > 120) shadowReach = Math.min(SHADOW_SIGHT, shadowReach + 0.03);
      if (spilt) reachCut = castFrame;
      if (count > 0) shadow = { first, count, shade };
    }
    casting = false;
    if (xr) xr.draw(host.scene, sky, xr.pose, xr.layer, projector, xr.mixed);
    else host.present({ clear: sky, shadow, lens: lensNow });
    if (!xr && gpu.queued < 64) {
      const handed = performance.now();
      gpu.queued++;
      const done = () => {
        gpu.queued--;
        gpu.max = Math.max(gpu.max, performance.now() - handed);
      };
      host.device.queue.onSubmittedWorkDone().then(done, done);
    }
    // Local inspection only; no telemetry, labels or DOM overlays.
    canvas.raxStats = {
      ...(scenery ? { scenery } : {}),
      backend: xr ? 'webxr' : backend,
      headsetRects, headsetMessage: headsetControls?.message || '',
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
      screenFrame, hudRegions: stats ? hudPack.taken : undefined,
      input,
      devLog: devLog.map((d) => d.text),
      promptText: prompt?.text || '',
      promptMessage: prompt?.message || '',
      builder,
      owner,
      builds: builds.map((b) => ({ state: b.state, text: b.text, note: b.note, phase: b.phase, tries: b.tries, where: b.where || '' })),
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
      gpu,
    };
  }
  return {
    canvas,
    // ⏱️ Each frame's drawing is timed (eased): when it runs long the lens
    // halves edges less finely and, longer still, the shadows are left out,
    // so a heavy view is a plainer picture and never a stopped one.
    render(params) {
      const began = performance.now();
      try { return render(params); } finally {
        const took = performance.now() - began;
        cost += (took - cost) * (took > cost ? 0.5 : 0.08); // (quick to take fright, slow to trust again)
      }
    },
    effect,
    clearGore: () => gore.clear(), // 🩸 (a room left: what fell there stays there)
    // 🖼️ The textures (textures.mjs): `stats()`, `config` for the bench, `register` for fn: sources.
    textures,
    // 🎭 Models put on the stage by hand (the texture bench, demos): each
    // { model, x, y, z, yaw, pitch, scale, roll, textures: [key] }, drawn
    // every frame after the room, its textures told how far off it is.
    stage(list) { staged = Array.isArray(list) ? list : []; },
    // The host's pixel grid: { pixel, left, top } (CSS pixels per buffer
    // pixel, and where the prompt's canvas starts); null to stand alone.
    density: setDensity,
    // The host's theme: true for dark, false for light, null to follow the system.
    theme(dark) {
      hostDark = typeof dark === 'boolean' ? dark : null;
    },
    grid(g) {
      const next = g && g.pixel > 0 ? { pixel: g.pixel, left: g.left || 0, top: g.top || 0, width: g.width, height: g.height } : null;
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
    // 🖥️ How large the picture draws a thing, zoom apart: one metre off the middle at one metre away is `sx` of the half-width across, `sy` of the half-height up.
    scale() {
      return natural;
    },
    // The camera's centre ray: exactly the crosshair's normal, bob, nod and
    // third-person offset included (2026-11-03).
    aimForward() {
      const f = projector.forward;
      return f && lastEye ? { x: f[0], y: f[1], z: f[2] } : null;
    },
    headsetHit(cssX, cssY) {
      const x = cssX * canvas.width / canvas.clientWidth, y = cssY * canvas.height / canvas.clientHeight;
      return headsetRects.find(r => x >= r.x && x < r.x+r.w && y >= r.y && y < r.y+r.h)?.mode;
    },
    handleHit(cssX, cssY) {
      const x = cssX * canvas.width / canvas.clientWidth, y = cssY * canvas.height / canvas.clientHeight;
      return handleRect && x >= handleRect.x && y >= handleRect.y && x < handleRect.x + handleRect.w && y < handleRect.y + handleRect.h;
    },
    overHud(cssX, cssY) {
      const x = cssX * canvas.width / canvas.clientWidth, y = cssY * canvas.height / canvas.clientHeight;
      return previousRegions.some(r => x >= r.x && y >= r.y && x < r.x + r.w && y < r.y + r.h);
    },
    aimRay(cssX, cssY) {
      if (!projector.forward || !lastEye) return null;
      const pxr = canvas.width / Math.max(1, canvas.clientWidth);
      const nx = (cssX * pxr - canvas.width / 2) / (canvas.width / 2), ny = (canvas.height / 2 - cssY * pxr) / (canvas.height / 2);
      const [f, r, u] = [projector.forward, projector.right, projector.up];
      // (Under the lens a place on the screen is 2 tan(a/2) from the middle: back to tan(a).)
      let kx = (nx - projector.shiftX) / projector.sx, ky = (ny - projector.shiftY) / projector.sy;
      if (projector.lens) { const r = Math.hypot(kx, ky), back = r > 1e-6 ? Math.tan(2 * Math.atan(Math.min(1.9, r) / 2)) / r : 1; kx *= back; ky *= back; }
      let x = f[0] + r[0] * kx + u[0] * ky, y = f[1] + r[1] * kx + u[1] * ky, z = f[2] + r[2] * kx + u[2] * ky;
      const l = Math.hypot(x, y, z) || 1;
      return { x: x / l, y: y / l, z: z / l };
    },
    pairHit(cssX, cssY) {
      if (!pairAt) return false;
      const px = canvas.width / Math.max(1, canvas.clientWidth), x = cssX * px, y = cssY * px, r = pairAt.rect;
      return x >= r.x && x <= r.x + r.w && y >= r.y && y <= r.y + r.h;
    },
    qrHit(cssX, cssY) {
      if (!qrRect) return false;
      const px = canvas.width / Math.max(1, canvas.clientWidth);
      const x = cssX * px, y = cssY * px;
      return x >= qrRect.x && x <= qrRect.x + qrRect.w && y >= qrRect.y && y <= qrRect.y + qrRect.h;
    },
    // A tap on the open menu: the row under it, a tab (-10 and down), -1 on
    // the panel but neither, or null outside the panel.
    menuHit(cssX, cssY) {
      if (!menuBox) return null;
      const { w, h, titleH, lineH, first, count, unit, W, H, tabs } = menuBox;
      const x = (cssX * canvas.width) / (canvas.clientWidth || 1) - W / 2,
        y = H / 2 - (cssY * canvas.height) / (canvas.clientHeight || 1);
      if (Math.abs(x) > w / 2 || Math.abs(y) > h / 2) return null;
      // The tab strip: -10 for the first tab, -11 the next, and so on.
      if (y > h / 2 - titleH) {
        const t = tabs.findIndex(([a, b]) => x >= a && x <= b);
        return t >= 0 ? -10 - t : -1;
      }
      const k = Math.floor((h / 2 - titleH - 2 * unit - y) / lineH);
      return k >= 0 && k < count ? first + k : -1;
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
