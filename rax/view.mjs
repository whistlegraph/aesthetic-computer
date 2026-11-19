import { WebGPUOskiewarScene3D } from './engine/scene3d-webgpu.mjs';
import { BOP, DRONE, ducked } from './bop.mjs';
import { groundsOf, promenade, GROUNDS, EDGE, SPAN } from './park.mjs';
import { CAR, CAR_HEIGHT, CAR_WALL, CAR_Z, carSolids, carScreenSolids, CAR_SCREEN_Z, STOREY, ATRIUM, SLAB, LEG, altitude, floorAt, LOBBY_FRONT, DOOR, DOOR_HEIGHT, DESK, PLANTERS, PLANTER } from './elevator.mjs';
import { SCREEN_SPAN, screenDetail, screenSeen } from './screen-lod.mjs';
import { model, INK, PAPER, rgb, tint, audit, boxesOf } from './paint.mjs';
import { shadowVolume, VOLUME_TRIANGLES, reach, shadeFor } from './shadow.mjs';
import { describeCrossing } from './solids.mjs';
import { phase, stepOf, skyAt, skyColor, stars as fixedStars, STEPS, blend as blendInk } from './sky.mjs';
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
import { poseObject, objectBoxes } from './objects.mjs';
import { OBJECT_FORMAT, sourceFrame, sourceTree, sourceHair } from './object-language.mjs';
import { BALL } from './ball.mjs';
import { airAt, sun, heading } from './air.mjs';
import { Projector } from './project.mjs';
import { joinCode, hudLayout } from './hud.mjs';

// What each kind of input does, one line per hand, as Oskiewar lists its
// controls: the same world plays from a keyboard alone, a pad or a touchscreen.
// A key is [label, code]; it lights while held. Plain strings are the verbs.
export const LEGEND = {
  // Lines stay under thirty columns so a phone or a narrow window shows them whole.
  // Keys and pad list the same actions, row for row (2026-11-08).
  keys: [
    [['w', 'KeyW'], ['a', 'KeyA'], ['s', 'KeyS'], ['d', 'KeyD'], ' move / ', ['←', 'ArrowLeft'], ['→', 'ArrowRight'], ['↑', 'ArrowUp'], ['↓', 'ArrowDown'], ' look'],
    [['space', 'Space'], ' jump / ', ['v', 'KeyV'], ' camera'],
    [['enter', 'Enter'], ' build / ', ['f', 'KeyF'], ' use / ', ['q', 'KeyQ'], ' drop'],
    [['x', 'KeyX'], ' paint / ', ['c', 'KeyC'], ' chalk'],
    [['z', 'KeyZ'], ' lock on / ', ['del', ['Delete', 'Backspace']], ' remove'],
    [['$', 'Digit4'], ' menu / ', ['t', 'KeyT'], ' chat / ', ['tab', 'Tab'], ' prompt'],
  ],
  pad: [
    '[LS] move / [RS] look',
    '[LT] jump / [Y] camera',
    '[RB] build / [RB] use / [X] drop',
    '[X] paint / [RT] chalk',
    '[LB] lock on / [B] remove',
    '[START] $ menu',
  ],
  touch: [], // The thumb areas and the icons name themselves.
};
// Each action has its own colour, shared by its words and its key caps, so
// the eye finds a control by colour (2026-11-08). Separators stay dim.
const ACTION_COLORS = {
  move: 0x6bb6ff, turn: 0x8fd3ff, look: 0x8fd3ff, jump: 0x3fb950, jumps: 0x3fb950,
  camera: 0xb46bff, build: 0xffe066, builds: 0xffe066, use: 0xff9f43, drop: 0xff6b6b,
  paint: 0xff7ac8, chalk: 0xe8e8e8, rooms: 0x2ec4b6, 'lock on': 0x00ffff, remove: 0xff6b6b, cancel: 0xff6b6b, prompt: 0xb0b0ff, '$ list': 0xffd28a, list: 0xffd28a, chat: 0x9be7a0,
  change: 0xffe066, next: 0x00ffff, unlock: 0x00ffff,
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
import { ICONS } from './touch.mjs';
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
  rayBox,
  bodyBox,
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

export async function createView(canvas) {
  let host, backend;
  let lost = false;
  // 🧊 The card's side of a frame (stalls.mjs): hand-off to finished, the
  // slowest since the client last read it, and how many are still out.
  const gpu = { max: 0, queued: 0 };
  try {
    host = await WebGPUOskiewarScene3D.create(canvas, {
      maxTriangles: 98304, // (the lens halves long edges: a frame takes more triangles than its shapes have)
      shadows: true, // 🌑 the sun's shadows, as stencil volumes (shadow.mjs)
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
      maxTriangles: 98304, // (the lens halves long edges: a frame takes more triangles than its shapes have)
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
  const buildArena = (p, W = WORLD) =>
    model(({ box, quad }) => {
      // (No ground beyond the walls: it was the sky's colour anyway, and a
      // centimetre under the floor it z-fought from any distance.)
      quad([-W, 0, -W], [-W, 0, W], [W, 0, W], [W, 0, -W], p.floor);
      for (let x = -W + 2; x < W; x += 2)
        for (let z = -W + 2; z < W; z += 2)
          box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot); // clear of the floor and the paint
      // Four slabs that never share volume (2026-11-03: overlapping corners
      // z-fought): north and south run the full outside width and own the
      // corners; east and west fit exactly between their inner faces.
      const h = ROOM_HEIGHT, t = 0.3, outside = 2 * W + 2 * t, inside = 2 * W;
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
    const made = model(({ quad }) => {
      const edges = [-0.12, 0, 0.06, 0.16, 0.34, 0.65, 1.1, Math.PI / 2];
      const at = (turn, e, r = R) => [Math.cos(turn) * r * Math.cos(e), r * Math.sin(e), Math.sin(turn) * r * Math.cos(e)];
      for (let b = 0; b < edges.length - 1; b++) {
        const lo = edges[b], hi = edges[b + 1], color = skyColor(s, lo < 0 ? 0 : (lo + hi) / 2);
        for (let k = 0; k < n; k++) {
          const t0 = (k / n) * Math.PI * 2, t1 = ((k + 1) / n) * Math.PI * 2;
          quad(at(t0, lo), at(t1, lo), at(t1, hi), at(t0, hi), color);
        }
      }
      // A square on the sky, facing the middle: `d` is where it stands.
      const square = (d, r, size, color) => {
        const flat = Math.hypot(d.x, d.z) || 1;
        const right = [d.z / flat, 0, -d.x / flat];
        const up = [d.y * right[2], d.z * right[0] - d.x * right[2], -d.y * right[0]];
        const corner = (a, b) => [0, 1, 2].map((i) => [d.x, d.y, d.z][i] * r + (right[i] * a + up[i] * b) * size / 2);
        quad(corner(-1, -1), corner(1, -1), corner(1, 1), corner(-1, 1), color);
      };
      if (s.stars > 0.3)
        for (const star of starField) square(star, R - 10, star.size, star.bright ? 0xffffff : 0xa9b8e6);
      if (s.sun.y > -0.12) {
        const around = skyColor(s, Math.max(0, Math.asin(s.sun.y)));
        square(s.sun, R - 30, 250, blendInk(around, s.sunColor, 0.35)); // its glow
        square(s.sun, R - 50, 150, s.sunColor);
      }
      if (s.moon.y > -0.12) square(s.moon, R - 50, 110, 0xe9edf6);
    });
    domeKept = { step, model: made };
    return made;
  }
  // The room follows the clock: rebuilt when the hour crosses into day or night.
  let paletteName = null, arena, skyModel, sky;
  const arenas = new Map(); // a plain room's walls and floor, by palette and half-width
  // 🌍 Per-block models, kept by block id ('here' is the one underfoot) and
  // rebuilt only when what they show changes.
  const grounds = new Map(), tileSets = new Map();
  // A block's ground: the plaza's floor or a residential block's grass and
  // lanes. A side with an open neighbour is left open; the world's edge gets
  // the plaza's wall or the park's hedge.
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
            box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot);
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
  // the world behind its back. The floor's number and handle are over the
  // door, outside and in.
  let carSet = null;
  function carFor(tower) {
    const here = tower?.here, key = JSON.stringify([here, tower?.count, paletteName]);
    if (carSet?.key === key) return carSet.model;
    const day = paletteName === 'day';
    const frame = day ? 0x8f98ac : 0x59607a, sill = day ? 0xdfe3ee : 0x3d4357, pad = day ? 0xffd9ec : 0x5a2244;
    const number = here ? (here.number ? `${here.number}` : 'G') : '';
    const who = here?.handle ? `@${here.handle}` : here ? 'AC TOWER' : '';
    const under = here?.handle ? '' : tower?.count ? `${tower.count} floors` : '';
    const made = model(({ quad, box, text, handleText }) => {
      const z0 = CAR_Z, t = CAR_WALL;
      // The car's solids (elevator.mjs): a floor, a roof, glass on three
      // sides, a post at each corner and the sign over the door, none of
      // them sharing volume with another.
      const inks = {
        pad: [pad, frame, pad], roof: [sill, frame, sill], sill: [sill, frame, sill],
        frame: [frame, frame, frame], sign: [0x27324a, 0x171d26, 0x171d26],
      };
      for (const s of carSolids()) box(s.x, s.y, s.z, s.w, s.h, s.d, ...inks[s.ink]);
      const door = z0 - CAR - t / 2;
      const fit = (label, width, most) => Math.min(most, width / Math.max(1, label.length * 6));
      const line = (label, top, z, width, most, color) => {
        const unit = fit(label, width, most);
        text(label, unit, top - 2 * unit, z + 0.004, unit, 0x000000); // a hard shadow, one pixel down and across
        if (color === 'handle') handleText(label, 0, top - unit, z, unit, here.colors);
        else text(label, 0, top - unit, z, unit, color);
      };
      if (number) line(number, 2.95, door - t / 2 - 0.012, 1.6, 0.035, 0xff3296);
      if (who) line(who, 2.55, door - t / 2 - 0.012, 2.6, 0.028, here?.handle ? 'handle' : PAPER);
      if (under) line(under, 2.3, door - t / 2 - 0.012, 2.6, 0.012, 0x9aa3b5);
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
    const floor = model(({ quad, box }) => {
      const cut = (x0, x1, z0, z1) => [Math.max(-W, x0), Math.min(W, x1), Math.max(-W, z0), Math.min(W, z1)];
      sheet((...r) => { const [a, b, c, d] = cut(...r); quad([a, 0, c], [a, 0, d], [b, 0, d], [b, 0, c], p.floor); });
      for (let x = -W + 2; x < W; x += 2)
        for (let z = -W + 2; z < W; z += 2)
          if (!(Math.abs(x) < bayX && z > bayZ)) box(x, 0.014, z, 0.08, 0.008, 0.08, p.dot, p.dot, p.dot);
      // The underside of the floor above is this floor's ceiling.
      const top = STOREY - 0.42;
      sheet((...r) => { const [a, b, c, d] = cut(...r); quad([a, top, c], [b, top, c], [b, top, d], [a, top, d], under); });
    });
    // The land around the plaza and its blocks, out to the horizon: four
    // fields around the middle 48 m, which the blocks themselves fill.
    const grass = day ? 0xb9d7ac : 0x243d36;
    const land = model(({ quad }) => {
      const far = 2200, n = 24, y = -0.02;
      quad([-far, y, n], [-far, y, far], [far, y, far], [far, y, n], grass);
      quad([-far, y, -far], [-far, y, -n], [far, y, -n], [far, y, -far], grass);
      quad([-far, y, -n], [-far, y, n], [-n, y, n], [-n, y, -n], grass);
      quad([n, y, -n], [n, y, n], [far, y, n], [far, y, -n], grass);
    });
    const lawn = model(({ quad }) => quad([-8, -0.02, -8], [-8, -0.02, 8], [8, -0.02, 8], [8, -0.02, -8], grass));
    towerSet = { key: paletteName, slabModel, columns1: columns(1), columns10: columns(10), glass, rails, legs, floor, land, lawn };
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
  // inside, a pale floor, a desk, benches and the directory board.
  let lobbySet = null;
  function lobbyFor(count, board) {
    const key = JSON.stringify([paletteName, count, board]);
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
      // The name over the door, facing the plaza.
      box(0, 4.15, f - 0.12, 6, 1.3, 0.12, 0x27324a, dark, dark);
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
      line('AC TOWER', 0, 4.72, f - 0.19, 5, 0.085, 0xff3296);
      if (count) line(`${count} floors`, 0, 3.86, f - 0.19, 4, 0.026, 0xc9cedb);
      // The two ends, glass like the front.
      for (const x of [-W + 0.05, W - 0.05]) {
        box(x, 0.25, (f + W) / 2, 0.2, 0.5, W - f, sill, sill, post);
        for (const y of [3.2, 6.2, 9.2]) box(x, y, (f + W) / 2, 0.12, 0.12, W - f, post, post, post);
        for (let z = f; z <= W + 0.01; z += (W - f) / 3) box(x, (H + 0.5) / 2, z, 0.12, H - 0.5, 0.12, post, post, post); // standing on the sill
      }
      // Inside: the desk, two benches, a runner from the door to the car.
      box(DESK.x, DESK.h / 2, DESK.z, DESK.w, DESK.h, DESK.d, wood, tint(wood, 0.8), tint(wood, 1.08));
      box(DESK.x, DESK.h + 0.02, DESK.z, DESK.w + 0.16, 0.05, DESK.d + 0.16, stone, stone, stone);
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
      // The board by the desk, read from the door: your floor, how tall the
      // tower is, and the floors with someone on them right now.
      if (board) {
        const bx = DESK.x, bz = DESK.z + 1.5, z = bz - 0.012;
        box(bx, 1.75, bz + 0.05, 3, 2.9, 0.1, 0x27324a, dark, dark); // a lighter face, so the shadows read
        box(bx, 0.15, bz + 0.05, 3.2, 0.3, 0.3, post, post, post);
        // (Text is laid out with x running the other way from boxes: a
        // viewer facing north has +x on the left. `tx` is the board's middle.)
        const tx = -bx;
        line('ac tower', tx, 3.08, z, 2.4, 0.034, 0xff3296);
        line(`${count} floors`, tx, 2.7, z, 2.4, 0.022, 0xc9cedb);
        line(board.yours ? `your floor: ${board.yours}` : 'sign in for a floor', tx, 2.36, z, 2.6, 0.026, board.yours ? 0xff3296 : 0x9aa3b5);
        line(board.in.length ? 'in now' : 'nobody is upstairs', tx, 1.92, z, 2.4, 0.02, 0x9aa3b5);
        board.in.slice(0, 4).forEach(([n, handle], k) => {
          const top = 1.66 - k * 0.27, unit = 0.02, label = `${n}  @${handle}`.slice(0, 22);
          inked(label, tx - 1.2 + ((label.length * 6 - 1) * unit) / 2, top - unit, z, unit, 0x9be7a0);
        });
        line('ride: a number or @handle', tx, 0.56, z, 2.6, 0.016, 0x9aa3b5);
      }
    });
    lobbySet = { key, model: made };
    return made;
  }
  // Draw the tower standing at (ox, oy, oz), `top` floors tall, as seen from
  // a height: full storeys near the eye, slabs further off, columns to the
  // limit of sight.
  function drawTower(top, ox, oy, oz, eyeY) {
    const t = towerParts();
    if (!top) return;
    draw(t.legs, ox, oy, oz);
    const near = 9, mid = 70, sight = 2300;
    const at = Math.max(1, Math.min(top, floorAt(eyeY - oy) || 1));
    const lo = Math.max(1, floorAt(eyeY - oy - sight) || 1), hi = Math.min(top, Math.max(1, floorAt(eyeY - oy + sight)));
    for (let n = lo; n <= hi; ) {
      const y = oy + altitude(n);
      if (n + 9 <= hi && (n > at + mid || n + 9 < at - mid)) { draw(t.columns10, ox, y, oz); n += 10; continue; }
      draw(t.columns1, ox, y, oz);
      if (Math.abs(n - at) <= mid) draw(t.slabModel, ox, y, oz);
      if (Math.abs(n - at) <= near) { draw(t.glass, ox, y, oz); draw(t.rails, ox, y, oz); }
      n++;
    }
    if (hi === top) draw(t.slabModel, ox, oy + altitude(top + 1), oz); // the roof
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
  let netSince = null; // When the connection went down, for the log's typing.
  // The frame-rate readout follows AC's shared fps switch (localStorage
  // "ac:fps-hud", toggled by backtick on the prompt; client.mjs sets it).
  let SHOW_FPS = false;
  const LENS = 0.65, LENS_ZOOM = 1.3; // how much of the lens (1: straight lines near the edge bow a good deal), and how much larger the middle is drawn under all of it
  let lensFull = 0; // how full of triangles the last frame was, 0 to 1: the lens halves edges less finely when it runs high
  let density = 0;
  try { density = Math.max(-5, Math.min(5, Math.round(Number(localStorage.getItem('rax-density')) || 0))); } catch {}
  const statsRun = new Float32Array(120); let statsHead = 0; // the last 120 frames' gaps, a ring
  let statsAt = 0, statsGap = 16.7, statsWorst = 0, statsWorstAt = 0; // 🧰 the last frame's start, the time between frames (eased), the worst lately
  // 📐 The axes' colours, as in any 3D tool: x red, y green, z blue.
  const AXIS = { x: 0xff6b6b, y: 0x6fdc6f, z: 0x6aa8ff };
  let panelTop = null;
  let stride = 0, strideAt = performance.now(); // The walk's step phase, for the camera's bob.
  let objectHintSpans = null; // What the highlighted object offers, drawn at the object.
  let cornerRect = null; // The corner block, in buffer pixels from the top left.
  let menuBox = null; // The open menu's panel and rows, in buffer pixels about the centre.
  let lastEye = null; // Where the camera stood last frame, for the cursor's aim ray.
  let snow = null, snowAt = 0; // The room's falling snow.
  let orbitZoom = 1; // The curtain view's picture scale (below 1 shrinks it).
  let rain = null, rainAt = 0; // The rain that falls when it rains in Los Angeles.
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
  const SHADOW_BUDGET = 9000; // triangles of shadow volume in a frame, at most
  const SHADOW_SIGHT = 45; // metres from the eye a box still casts
  const volumeFloats = new Float32Array(SHADOW_BUDGET * 18);
  // 🫧 A seam: the line where one room of the world hands a walker to the
  // next (each block is its own room, on its own socket). A net of threads
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
  const drawSeams = (neighbours, dx, dy, dz, line, ink, now, skip = null) => {
    const climb = ((now / 2400) % 1) * SEAM_STEP;
    for (const [side, sx, sz] of [['east', 1, 0], ['west', -1, 0], ['north', 0, 1], ['south', 0, -1]]) {
      if (!neighbours?.[side] || skip?.(sx, sz)) continue;
      const x = dx + sx * line, z = dz + sz * line, turn = sx ? Math.PI / 2 : 0;
      projector.draw(seamFor(ink, 'posts'), x, dy, z, turn);
      projector.draw(seamFor(ink, 'rungs'), x, dy + climb, z, turn);
    }
  };
  // ⚽ The ball: a small sphere in black and white patches, turned by how
  // far it has rolled.
  let ballModel = null, ballRoll = 0, ballSeen = null;
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
      }
    };
    corners.forEach((c, i) => face(near(i).map((j) => third(c, corners[j])), 0x16181d));
    for (let i = 0; i < 12; i++) for (const j of near(i)) for (const k of near(i))
      if (i < j && j < k && near(j).includes(k))
        face([[i, j], [j, i], [j, k], [k, j], [k, i], [i, k]].map(([p, q]) => third(corners[p], corners[q])), 0xf4f4f0);
  }));
  const draw = (made, x = 0, y = 0, z = 0, yaw = 0, pitch = 0, scale = 1, roll = 0) => {
    projector.draw(made, x, y, z, yaw, pitch, scale, roll);
    if (casting && !pitch && !roll && boxesOf(made)) casters.push(made, x, y, z, yaw, scale);
  };
  function render({
    players = [],
    me,
    camera,
    spectator = false,
    gate = null, // 'account' | 'handle': what a visitor needs before a seat.
    controller,
    seat = 0,
    third = false,
    drones = [], // 🤖 the game's dumb robots: { id, x, y, dir }
    game = null, // 🎮 the room's gametype (games.mjs): its ledges, legend and what it hides
    bops = [], // bops landed, by seat
    combos = [], // 🔗 a chain in the air, by seat: [how many, worth] or 0
    debug = false, // 🔲 a game's hitboxes and numbers
    boomLength = 4, // how far back the third-person camera sits
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
    touchButtons = [],
    touchZones = [],
    input = 'keys',
    devLog = [],
    fps = 0,
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
    notices = [],
    braincells = null,
    spending = false,
    tiles = {},
    park = null,
    field = null, // 🌳 this room's cell on the tower's grounds ({ i, j }), or null
    around = [], // the open blocks beside this one: { id, dx, dz, neighbours, plaza, objects, tiles }
    seam = null, // 🫧 { crossing, shown }: the body is between two rooms; shown: draw the rooms' lines as walls (rax?seams)
    gauge = null, // 🚁 { mph, rpm, feet }: a drone's dials, bottom centre
    stats = false, // 🧰 the debugging panel, bottom right (the ` key): frames, the picture's size, triangles, the backend
    sticks = null, // 🎮 { move: [x, y], look: [x, y], buttons: [bool] } from -1 to 1: a controller's two sticks and what is pressed, shown while looking on
    place = null, // 📍 { x, z }: this room's place in the world; your position is shown from the plaza's middle
    eyes = [], // 🚁 whoever looks on, each a small drone where their eye is: { name, x, y, z, yaw, vx, vz }
    beyond = [], // 👀 the people in the rooms around this one, in this room's metres: { name, seat, x, y, z, yaw, pitch, vx, vz }
    weather = null, // 🌧️ { rain, snow, cloud, wind }: what falls over Los Angeles (weather.mjs)
    ball = null, // ⚽ { x, y, z, vx, vz } in this room's metres
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
    wire = null, // 📟 { text, color }: one line of the connection's story, top left, while the room is still up (the server quiet, or just back on new code)
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
    half = WORLD, // 📐 This room's half-width (world.mjs halfOf).
  } = {}) {
    if (lost) {
      location.reload();
      return;
    }
    const ledges = game?.ledges || null;
    const start = performance.now(),
      now = start;
    const solidObjects = objects.filter((o) => !o.item);
    lastWorldTime = worldTime;
    // 👀 Looking on, the connection's meter stays away: it sat over the controls.
    if (spectator) connection = null;
    // The corner join code is retired for now: rax is joining the AC homepage.
    const cells = null;
    const hud = hudLayout(canvas.width, canvas.height, cells, pixelScale, grid ? 1 : 0);
    // 🌞 In the world the room follows the world's own day (sky.mjs), the
    // same for everyone; behind the prompt's curtain it keeps the page's
    // theme, and a game arena its own.
    const skyStep = !ledges && orbit < 1 ? stepOf(phase()) : -1;
    const today = skyStep >= 0 ? skyAt(skyStep / STEPS) : null;
    dress(today ? (today.day ? 'day' : 'night') : paletteFor());
    sky = [...rgb(today ? today.horizon : PALETTES[paletteName].sky), 1];
    host.beginFrame();
    // 🌑 Shadows follow the sun itself, not the sky's three-second pictures.
    const sunNow = today ? skyAt(phase()).sun : null;
    const shade = sunNow && host.shadows ? shadeFor(sunNow.y) : null;
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
        eye.y = Math.max(eye.y, me.y - CFG.eyeHeight + 0.3);
        through = true;
      }
      let distance = wallDistance(origin, dir, 100, solidObjects, worldTime, half);
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
      const natural = ((half + 0.3) / height) * (1 / tan) * (canvas.height / 2); // room half-size in px
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
        // 🧭 Straight down, which way is up hangs on a hair: the first step
        // of the flight used to snap the room round to the curve's own
        // heading. Now the curve leaves the overhead view on that heading
        // (`out`), and the whole flight is turned about the room's middle by
        // what is left between it and the slow spin, eased away to nothing
        // by the time the eye is the player's (2026-11-19).
        const ox = aim.x - centre.x - 2 * behind.x, oz = aim.z - centre.z - 2 * behind.z, on = Math.hypot(ox, oz) || 1;
        const lean = height * 0.002;
        const out = { x: (-ox / on) * lean, y: height, z: (-oz / on) * lean };
        eye = { x: bez(out.x, behind.x, eye.x), y: bez(out.y, behind.y, eye.y), z: bez(out.z, behind.z, eye.z) };
        aim = {
          x: centre.x + (aim.x - centre.x) * e,
          y: centre.y + (aim.y - centre.y) * e,
          z: centre.z + (aim.z - centre.z) * e,
        };
        let turn = Math.atan2(-Math.sin(a), Math.cos(a)) - Math.atan2(ox, oz);
        turn = (((turn + Math.PI) % (2 * Math.PI)) + 2 * Math.PI) % (2 * Math.PI) - Math.PI; // the short way round
        turn *= 1 - e;
        const c = Math.cos(turn), n = Math.sin(turn);
        const about = (p) => ({ x: p.x * c + p.z * n, y: p.y, z: -p.x * n + p.z * c });
        eye = about(eye);
        aim = about(aim);
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
    // 👻 Head, chest, feet and both shoulders: what the eye must still see.
    if (through && !camera && orbit <= 0) {
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
    // 🔭 The lens (project.mjs): walking or flying, the middle of the picture
    // is drawn a little larger than a flat picture of this width would have
    // it, and the edges are drawn in, so more is seen to either side and
    // nothing there is stretched. Not from overhead, nor side-on in a game.
    projector.decay = decay;
    projector.decayAt = now;
    // It is how rax is seen, always; flying in from overhead it comes on as the eye comes down.
    projector.lens = game?.camera ? 0 : LENS * Math.max(0, Math.min(1, 1 - orbit));
    projector.fine = lensFull < 0.3 ? 0.16 : lensFull < 0.55 ? 0.22 : 0.3;
    if (projector.lens) {
      projector.sx *= 1 + (LENS_ZOOM - 1) * projector.lens;
      projector.sy *= 1 + (LENS_ZOOM - 1) * projector.lens;
    }
    // The sky is the clear colour alone: no banded dome (2026-11-03).
    lastEye = eye;
    const tags = [];
    // 🌍 The block underfoot, then every open block around it where it
    // stands: one continuous ground.
    if (today) draw(skyDome(skyStep), eye.x, eye.y, eye.z);
    if (ledges) draw(ledgesFor(ledges));
    else if (tower?.here?.number > 0) draw(towerParts().floor); // a floor of the tower: no walls, the world outside
    else if (park?.neighbours) {
      draw(groundFor('here', park.neighbours, park.plaza, field));
      // This room's seams stand where it lets go of a walker (EDGE, a step
      // past the block's line), so the net is seen to hop on the way over.
      // (Not behind the curtain: the small room in the prompt's corner is the
      // block alone, fitted to the crosshair's square.)
      if (orbit < 1 && seam?.shown) drawSeams(park.neighbours, 0, 0, 0, EDGE, seam?.crossing ? SEAM.crossing : SEAM.rest, now);
    }
    else if (half === WORLD) draw(arena);
    else {
      if (!arenas.has(half)) arenas.set(half, buildArena(PALETTES[paletteName], half));
      draw(arenas.get(half));
    }
    if (ball) {
      const pace = Math.hypot(ball.vx || 0, ball.vz || 0);
      if (ballSeen !== null) ballRoll += (pace * Math.min(0.1, (now - ballSeen) / 1000)) / BALL.r;
      ballSeen = now;
      draw(ballFor(), ball.x, ball.y, ball.z, pace > 0.05 ? Math.atan2(ball.vx, ball.vz) : 0, ballRoll);
    }
    if (tower && !ledges) {
      draw(carFor(tower), 0, tower.lift || 0, 0);
      if (tower.screen) draw(carScreenFor(tower.screen), 0, tower.lift || 0, CAR_SCREEN_Z, Math.PI); // hung clear of the door's sign
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
    // Behind the curtain the room is a small square under the crosshair, seen
    // from 25 m straight up: the tower's storeys, the lobby's glass and the
    // land around would stand between that camera and the floor and fill the
    // screen. They come in once the drop is most of the way down.
    if (skyline && !ledges && orbit < 0.6) {
      drawTower(skyline.top, skyline.dx, skyline.dy, skyline.dz, eye.y);
      draw(lobbyFor(skyline.count, skyline.board), skyline.dx, skyline.dy, skyline.dz);
      // The land the tower stands on, seen from the plaza and from every floor:
      // fields to the horizon, and a lawn wherever a block around the plaza is
      // not open yet.
      // The grounds too stand wherever they are seen from, so the pool and
      // its beds do not appear only on stepping into the block before them.
      {
        const t = towerParts();
        const shown = (dx, dz) => (dx === 0 && dz === 0 && !skyline.dy) || around.some((b) => b.dx === dx && b.dz === dz);
        draw(t.land, skyline.dx, skyline.dy, skyline.dz);
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
      const by = b.dy || 0; // a block seen from up the tower lies below
      if (!b.floor) draw(groundFor(b.id, b.neighbours, b.plaza, groundsOf(b.id)), b.dx, by, b.dz); // (a floor of the tower has its own slab)
      const painted = tilesFor(b.id, b.tiles);
      if (painted) draw(painted, b.dx, by, b.dz);
      // The seams between the blocks around, on their lines; the ones that
      // face this block are its own, drawn above.
      if (!by && orbit < 1 && seam?.shown) drawSeams(b.neighbours, b.dx, 0, b.dz, SPAN / 2, SEAM.far, now, (sx, sz) => b.dx + sx * SPAN === 0 && b.dz + sz * SPAN === 0);
    }
    for (const id of grounds.keys())
      if (id !== 'here' && !id.startsWith('grounds ') && !around.some((b) => b.id === id)) {
        grounds.delete(id); tileSets.delete(id);
      }
    // A thing standing on the ground, drawn where its block stands.
    const stand = (object, ox, oz, oy = 0) => {
      if (object.geometry.format === OBJECT_FORMAT) {
        draw(
          sourceFrame(object.geometry, worldTime).vertices,
          ox + object.x,
          oy,
          oz + object.z,
        );
        drawHair(object.geometry, ox + object.x, oy, oz + object.z);
        return;
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
    const aliveObjects = new Set(objects.map((o) => o.id));
    for (const b of around) for (const o of b.objects || []) aliveObjects.add(o.id);
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
      stand(object, 0, 0);
    }
    for (const b of around)
      for (const object of b.objects || [])
        if (!object.item && object.geometry) stand(object, b.dx, b.dz, b.dy || 0);
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
    const inRoom = Math.max(players.length, seat + 1);
    for (let n = 0; n < inRoom + beyond.length; n++) {
      // (After the room's own: the people beyond it, each drawn as the seat they hold there.)
      const far = n >= inRoom ? beyond[n - inRoom] : null, i = far ? far.seat : n;
      const p = far || (i === seat && me ? me : players[i]);
      if (!p?.alive) continue;
      const f = fighters[i % fighters.length],
        yaw = (p.yaw * Math.PI) / 180,
        base = p.y - CFG.eyeHeight - (game ? ducked(p) : 0); // 🦆 a ducking body sits lower
      if (far || i !== seat || third || !me) {
        draw(f.torso, p.x, base, p.z, yaw);
        draw(f.head, p.x, base, p.z, yaw);
        draw(eyeFor(p, i, now, face), p.x, base, p.z, yaw);
        // Name tags are screen text drawn after the world (see below), so
        // they stay crisp and never fight the scene for depth.
        if (far) { if (far.name) tags.push({ label: far.name, seat: i, x: p.x, y: base + 2.15, z: p.z, at: p }); }
        else if (i !== seat) tags.push({ seat: i, x: p.x, y: base + 2.15, z: p.z, at: p });
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
      draw(droneFor(Math.floor(now / 40) % 2), d.x, y, d.z, yawR, lean, 1, -bank);
      if (d.name) tags.push({ label: d.name, x: d.x, y: y + 0.45, z: d.z, at: d });
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
          ? `[RB] ${verb} / [X] drop`
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
        o.wall
          ? pad ? '[RB] change' : '{f} change'
          : o.owner === owner
            ? pad ? '[RB] change / [LB] lock on' : '{enter} change / {z} lock on'
            : pad ? '[LB] lock on' : '{z} lock on';
      const paintHint = !focusObject && floorAim && !holding && input !== 'touch' ? (pad ? '[X] paint the floor' : '{x} paints the floor') : null;
      const confirming = focusObject && removeArmed === focusObject.id ? '{delete} again to remove' : null;
      // Locked on, the top-right controls become the lock's own, row for
      // row in the hand in use (2026-11-08).
      const mine = focusObject && focusObject.owner === owner;
      const lockHint = lockedOn && focusObject
        ? [
            `${named(focusObject)} locked`,
            ...(pad
              ? focusObject.wall
                ? ['[RB] change / [LB] unlock']
                : [`${mine ? '[RB] change / ' : ''}[B] remove`, 'dpad next / [LB] unlock']
              : focusObject.wall
                ? ['{f} change / {z} unlock']
                : [`${mine ? '{enter} change / ' : ''}{del} remove`, '{[} {]} next / {z} unlock']),
          ].join('\n')
        : null;
      // What the highlighted object offers ([B] remove, [LB] unlock, its
      // name and owner) is drawn at the object itself (2026-11-03); the
      // corner keeps only the general hint.
      const objectHint = lockHint
        ? lockHint
        : confirming
          ? confirming
          : focusObject
            ? `${named(focusObject)}${focusObject.wall ? '' : ` v${focusObject.version}`}\n${whose(focusObject)}`
            : null;
      objectHintSpans = null; // The lock-on shows only its bracket; its offers read in the corner.
      // In the car the floor comes first: before paint, and on touch too.
      const hint = objectHint ? objectHint : drawing ? drawing : carry ? carry
        : car
          ? input === 'touch' ? 'tap the cube: pick a floor' : pad ? '[D-PAD] up / down a floor' : '{enter} pick a floor'
        : paintHint ? paintHint
        : input === 'touch'
          ? 'tap the cube to build'
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
      // A mode (locked on, aiming at a build, chalk, carrying) shows its own
      // controls in place of the general legend.
      const moded = !!(objectHint || drawing || carry);
      const showLegend = legend && !moded && !spectator && !wallCode && !prompt.open && !building;
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
                    radial
                      ? input === 'touch'
                        ? 'tap a choice / centre builds / outside undoes'
                        : '[A] choose / [X] build / [B] undo'
                      : cancel,
                    THEME.dim,
                  ),
                ]
              : []),
          ]
        : [
              // The controls for the hand in use, until the player has played
              // a while (H, on the keys, brings them back).
              ...(showLegend ? legendRows(input, held, game?.legend || LEGEND) : []),
              ...(spectator
                ? [
                    // Looking on. (No way in is offered here: signing up
                    // and taking a handle belong to the paper, 2026-11-18.)
                    hintSpans(
                      input === 'touch' ? 'look' : controller ? 'look / [LB] down / [RB] up / [Y] drone / [X] lock on' : 'look / {wasd} / {space} up / {shift} down / {v} drone / {z} lock on',
                      THEME.text,
                    ),
                  ]
                : !me ? [] // Still arriving: no seat yet, so no controls to offer.
                : game && !moded ? [] // a game's legend is its whole hint
                : hint.split('\n').map((l, i) =>
                    i === 0 && moded && hint.includes('\n')
                      ? [{ text: l, color: THEME.verb }]
                      : colourRow(hintSpans(l, confirming ? 0xff6b6b : THEME.text)),
                  )),
            ];
      const visible = rows.slice(
        -Math.max(3, Math.floor((height - 2 * safeY) / (11 * unit)) - 3),
      ).map((r) => clipSpans(r, columns));
      // A phone has send and cancel as caps; the key hints are for keys.
      const typing = (saying || (prompt.open && !radial && !prompt.busy)) && input !== 'touch';
      const enterHint = typing ? hintSpans(chat?.lift ? '{enter} ride / {up} {down} a floor' : saying ? '{enter} say' : '{enter} build', THEME.dim) : null;
      const escHint = typing ? hintSpans('{esc} cancel', THEME.dim) : null;
      const key = JSON.stringify([visible, width, height, prompt.open, saying, !!chat?.lift, !!radial, !!wallCode, !!enterHint]);
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
            const ox = grid ? 6 : safeX, oy = grid ? 6 : safeY;
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
              drawSpans({ rect, text, small }, escHint, width / 2 - safeX - ew / 2, height / 2 - safeY - 2 * unit, unit, { shadow: INK, tiny: true });
            }
          } else {
            // The controls and the hint, top-right, compact: MatrixChunky8
            // with small key caps and badges (2026-11-08).
            const right = width / 2 - safeX;
            visible.forEach((spans) => {
              const w = smallSpanWidth(spans, unit);
              drawSpans({ rect, text, small }, spans, right - w / 2, height / 2 - safeY - 2 * unit - visible.indexOf(spans) * 14 * unit, unit, { shadow: INK, tiny: true }); // (fourteen-pixel rows: a cap and its shadow clear the row under it)
            });
          }
        });
      }
      projector.overlay(promptModel, width, height);
    }
    let ownTag = null; // Where your own handle ends, for your world position beside it.
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
      if (labels.length && me && !bare)
        ownTag = {
          x: -width / 2 + safeX + 2 + (labels[0].label.length * 6 + 3) * unit,
          y: Math.max(-height / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit),
        };
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
        stallNote,
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
          if ((meter || (SHOW_FPS && fps)) && !face && !bare && !game) {
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
            if (SHOW_FPS && fps) line([{ text: `${fps} fps`, color: fpsColor }, ...(stallNote ? [{ text: `  ${stallNote}`, color: fair }] : [])]);
          }
        });
      }
      projector.overlay(presenceModel, width, height);
    }
    const W = canvas.width,
      H = canvas.height,
      pixel = W / Math.max(1, canvas.clientWidth);
    const chatting = !bare && chat && (chat.open || chat.lines.length);
    if ((tags.length || ownTag || chatting || ride) && !menu) {
      const { unit } = hud;
      // 📍 World position in metres (x y z), small, after a handle: yours
      // bottom left, everyone else's over their head.
      const where = (p) => [[p.x.toFixed(1), AXIS.x], [(p.y || 0).toFixed(1), AXIS.y], [p.z.toFixed(1), AXIS.z]];
      const m = model(({ rect: worldRect, text, handleText, small }) => {
        const place = (p, left, y, n = 0, beside = false) => {
          // 🍄 In the arena the count is the point; elsewhere it follows the position.
          const count = `${n} bop${n === 1 ? '' : 's'}`;
          const parts = [...(ledges || beside ? [] : where(p)), ...(ledges || n ? [[count, THEME.dim]] : [])];
          for (const [t, c] of parts) {
            const cx = left + (smallWidth(t) / 2) * unit;
            small(t, cx + unit, y - unit, 0.000002, unit, INK);
            small(t, cx, y, 0, unit, c);
            left += (smallWidth(t) + 3) * unit;
          }
        };
        // (Your own position is in the column beside the face cam.)
        if (ownTag && !game) place(me, ownTag.x, ownTag.y, bops[seat] || 0, !!face);
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
        for (const t of tags) {
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
    if (touch?.stick || touchButtons.length || touchZones.length) {
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
          const art = ICONS[b.icon];
          if (!art) {
            text(b.name, x + w / 2 + sink, top - h / 2 + 3 * unit - sink, 0.000001, unit, INK);
            continue;
          }
          // An icon: whole units, as large as leaves a margin, centred.
          const k = Math.max(unit, Math.floor((h * 0.6) / art.length / unit) * unit);
          const ix = Math.round((x + sink + (w - art[0].length * k) / 2) / unit) * unit,
            iy = Math.round((top - sink - (h - art.length * k) / 2) / unit) * unit;
          art.forEach((row, r) => {
            for (let c = 0; c < row.length;) {
              if (row[c] !== '#') { c++; continue; }
              const from = c++;
              while (row[c] === '#') c++;
              rect(ix + from * k, iy - (r + 1) * k, 0.000001, (c - from) * k, k, INK);
            }
          });
        }
        // The two thumb areas: corner brackets and a name, dim until a thumb
        // is down in one.
        for (const zone of touchZones) {
          const [x, top] = at(zone.x, zone.y), w = zone.w * pixel, h = zone.h * pixel;
          const on = zone.side === 'left' ? !!touch?.stick : !!touch?.look;
          const color = on ? PAPER : 0x5a6072, arm = 7 * unit, t = unit;
          for (const [cx, sx] of [[x, 1], [x + w, -1]])
            for (const [cy, sy] of [[top, -1], [top - h, 1]]) {
              rect(Math.min(cx, cx + sx * arm), sy < 0 ? cy - t : cy, 0.000004, arm, t, color);
              rect(sx > 0 ? cx : cx - t, Math.min(cy, cy + sy * arm), 0.000004, t, arm, color);
            }
          const tx = x + w / 2, ty = top - 5 * unit;
          text(zone.name, tx + unit, ty - unit, 0.000004, unit, INK);
          text(zone.name, tx, ty, 0.000003, unit, color);
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
      projector.overlay(m, W, H);
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
      projector.overlay(m, W, H);
    }
    // 🧰 The debugging panel (the ` key), bottom right: how fast the picture
    // comes, how big it is, how many triangles it took and what draws them.
    {
      const t = performance.now(), gap = statsAt ? t - statsAt : 16.7;
      statsAt = t;
      statsGap += (gap - statsGap) * 0.1;
      statsRun[statsHead] = gap; statsHead = (statsHead + 1) % statsRun.length;
      lensFull = (projector.scene?.triangleCount || 0) / (projector.scene?.maxTriangles || 1);
      if (gap > statsWorst || t - statsWorstAt > 3000) { statsWorst = gap; statsWorstAt = t; }
    }
    if (stats) {
      // A meter, not a table: the last two seconds of frames as bars (each as
      // tall as the frame was long, green under a sixtieth, yellow under a
      // thirtieth, red over), a line where sixty a second lies; under it how
      // full the triangle budget is; beside it the wind as an arrow that
      // points where the air goes, longer the harder it blows, with a tick
      // up or down for lift. One number: frames a second.
      const { unit, safeX, safeY } = hud;
      const tris = projector.scene?.triangleCount || 0, most = projector.scene?.maxTriangles || 0;
      const rate = Math.round(1000 / Math.max(1, statsGap));
      const good = 0x3fb950, fair = 0xffe066, poor = 0xff6b6b;
      const at = camera || me;
      const air = weather && at ? airAt(weather, at.x + (place?.x || 0), at.y, at.z + (place?.z || 0), Date.now() / 1000, { day: sun(phase()) }) : null;
      const m = model(({ rect: worldRect, small }) => {
        const rect = flip(worldRect);
        const BARS = statsRun.length, tall = 24 * unit, wide = BARS * unit;
        const right = W / 2 - safeX - 2 * unit, foot = -H / 2 + safeY + 14 * unit, left = right - wide;
        rect(left - unit, foot - unit, 0.000004, wide + 2 * unit, tall + 2 * unit, INK);
        rect(left, foot, 0.000003, wide, tall, 0x15171e);
        for (let i = 0; i < BARS; i++) {
          const gap = statsRun[(statsHead + i) % BARS];
          if (!gap) continue;
          const h = Math.max(unit, Math.min(tall, Math.round((gap / 50) * 24) * unit)); // the box is 50 ms tall
          rect(left + i * unit, foot, 0.000002, unit, h, gap <= 18 ? good : gap <= 34 ? fair : poor);
        }
        rect(left, foot + Math.round((16.7 / 50) * 24) * unit, 0.000001, wide, unit, 0x5a5f70); // sixty a second
        // The triangle budget: a bar under the frames, red when nearly full.
        const full = most ? Math.min(1, tris / most) : 0;
        rect(left - unit, foot - 6 * unit, 0.000004, wide + 2 * unit, 4 * unit, INK);
        rect(left, foot - 5 * unit, 0.000003, wide, 2 * unit, 0x15171e);
        rect(left, foot - 5 * unit, 0.000002, Math.max(unit, Math.round((wide * full) / unit) * unit), 2 * unit, full > 0.85 ? poor : 0x6aa8ff);
        // Frames a second, over the box, in its health's colour; the backend's initial beside it.
        const words = `${rate}`, ink = rate >= 55 ? good : rate >= 30 ? fair : poor, wx = right - (smallWidth(words) / 2) * unit, wy = foot + tall + 10 * unit;
        small(words, wx + unit, wy - unit, 0.000002, unit, INK);
        small(words, wx, wy, 0, unit, ink);
        const kind = backend === 'webgpu' ? 'gpu' : 'gl', kx = left + (smallWidth(kind) / 2) * unit;
        small(kind, kx + unit, wy - unit, 0.000002, unit, INK);
        small(kind, kx, wy, 0, unit, backend === 'webgpu' ? good : fair);
        // The wind: a dial left of the box, north up on the screen as on a map.
        if (air) {
          const cx = left - 18 * unit, cy = foot + tall / 2, R = 12 * unit;
          const speed = Math.hypot(air.x, air.z), reach = Math.min(R, (2 + speed * 1.2) * unit), a = Math.atan2(air.x, air.z);
          rect(cx - R - unit, cy - R - unit, 0.000004, 2 * R + 2 * unit, 2 * R + 2 * unit, INK);
          rect(cx - R, cy - R, 0.000003, 2 * R, 2 * R, 0x15171e);
          for (let k = 0; k <= reach; k += unit) rect(Math.round((cx + Math.sin(a) * k) / unit) * unit, Math.round((cy + Math.cos(a) * k) / unit) * unit, 0.000002, unit, unit, k > reach - 3 * unit ? 0xffffff : 0x9db4d6);
          rect(cx - unit / 2, cy - unit / 2, 0.000001, unit, unit, 0x5a5f70);
          // Lift: a bar on the dial's right edge, up from the middle when the air rises.
          const lift = Math.max(-R, Math.min(R, Math.round(air.y * 6) * unit));
          rect(cx + R - 2 * unit, lift >= 0 ? cy : cy + lift, 0.000002, 2 * unit, Math.max(unit, Math.abs(lift)), lift >= 0 ? good : poor);
        }
      });
      projector.overlay(m, W, H);
    }
    // The corner block: the prompt's own pink cursor block at the top left,
    // where the system label sits on every AC piece (SCREEN.md). A tap on it
    // goes back to the prompt.
    cornerRect = null;
    if (corner && !prompt?.open && !chat?.open && !jobs.some((j) => j.owner === owner)) {
      const { unit } = hud;
      const safeX = grid ? 6 : hud.safeX, safeY = grid ? 6 : hud.safeY;
      const w = 6 * unit,
        h = 10 * unit;
      const left = -W / 2 + safeX,
        bottom = H / 2 - safeY - h;
      // Beside it, what it and Escape do (2026-11-08).
      const label = hintSpans('{esc} back to AC', THEME.dim);
      const labelW = smallSpanWidth(label, unit);
      cornerRect = { x: safeX, y: safeY, w: w + 4 * unit + labelW, h };
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
    // Offline: no panel over the room. The top-left text spits the
    // connection's log a letter at a time under the corner block, as a
    // machine going down and coming back up would (2026-11-15).
    if (!offline) netSince = null;
    if (offline && !menu) {
      const { unit } = hud;
      netSince ??= now;
      const safeX = grid ? 6 : hud.safeX, safeY = grid ? 6 : hud.safeY;
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
      const key = JSON.stringify([shown, blink, W, H]);
      if (key !== netKey) {
        netKey = key;
        netModel = model(({ rect: worldRect, text }) => {
          const rect = flip(worldRect);
          shown.forEach((l, i) => {
            const last = i === shown.length - 1;
            const spans = [{ text: l.text, color: l.color }, ...(last && (blink || !l.done) ? [{ block: true }] : [])];
            const y = H / 2 - safeY - 2 * unit - (i + (cornerRect ? 1 : 0)) * 10 * unit;
            drawSpans({ rect, text }, spans, -W / 2 + safeX + spanWidthOf(spans, unit) / 2, y, unit, { shadow: INK });
          });
        });
      }
      projector.overlay(netModel, W, H);
    }
    // 📟 Not down, but worth a word: one line where the outage's log goes.
    if (wire && !offline && !menu && !prompt.open) {
      const { unit } = hud;
      const safeX = grid ? 6 : hud.safeX, safeY = grid ? 6 : hud.safeY;
      const m = model(({ rect: worldRect, text }) => {
        const rect = flip(worldRect);
        const spans = [{ text: wire.text, color: wire.color ?? THEME.text }];
        drawSpans({ rect, text }, spans, -W / 2 + safeX + spanWidthOf(spans, unit) / 2, H / 2 - safeY - 2 * unit - (cornerRect ? 1 : 0) * 10 * unit, unit, { shadow: INK });
      });
      projector.overlay(m, W, H);
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
    if ((me?.alive || reticle) && !radial && !menu && (!bare || reticle) && !onFloor && game?.hud?.reticle !== false) {
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
    // 👁️ Looking on there is no face: the connection and where the eye is, in
    // the world's metres, stand where the face's column would, bottom left.
    if (!face && !bare && !game && camera && !me) {
      const { unit, safeX, safeY } = hud;
      const x0 = -W / 2 + safeX + 2, y0 = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit) + 8 * unit;
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
        for (const [t, c] of [[(camera.x + (place?.x || 0)).toFixed(1), AXIS.x], [(camera.y || 0).toFixed(1), AXIS.y], [(camera.z + (place?.z || 0)).toFixed(1), AXIS.z]]) {
          const cx = left + (smallWidth(t) / 2) * unit, cy = y0 + row * unit;
          small(t, cx + unit, cy - unit, 0.000002, unit, INK);
          small(t, cx, cy, 0, unit, c);
          left += (smallWidth(t) + 3) * unit;
        }
      });
      projector.overlay(m, W, H);
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
        // The column beside the cam — braincells, the connection, the frame
        // rate — in even 10-row slots from the frame's top edge down, so no
        // two overlap (2026-11-08). A slot's number is the top of small()'s
        // 8-row glyph box; the glyphs themselves start a row lower.
        let slot = 26;
        const nextSlot = () => ((slot -= 10), slot + 10); // (a clear row between each line)
        // Three rows, the cam's own height: the connection (four rising bars
        // and its ping), where you stand, your braincells. The frame rate,
        // when shown, goes under them.
        {
          const ping = (connection?.label || '').toLowerCase();
          const bars = connection?.bars || 0;
          const good = 0x3fb950, fair = 0xffe066, poor = 0xff6b6b;
          const pingColor = !ping ? 0x8b90a0 : bars >= 3 ? good : bars >= 2 ? fair : poor;
          const fpsColor = fps >= 55 ? good : fps >= 30 ? fair : poor;
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
          }
          // 📍 Where you stand, in metres, each axis in its colour.
          if (me) {
            const pt = nextSlot();
            let left = x0 + 27 * unit;
            for (const [t, c] of [[(me.x + (place?.x || 0)).toFixed(1), AXIS.x], [(me.y || 0).toFixed(1), AXIS.y], [(me.z + (place?.z || 0)).toFixed(1), AXIS.z]]) {
              const cx = left + (smallWidth(t) / 2) * unit, cy = y0 + pt * unit;
              small(t, cx + unit, cy - unit, 0.000002, unit, INK);
              small(t, cx, cy, 0, unit, c);
              left += (smallWidth(t) + 3) * unit;
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
          if (SHOW_FPS && fps) {
            const f = `${fps} fps`;
            const fx = x0 + (27 + smallWidth(f) / 2) * unit, fy = y0 + nextSlot() * unit;
            small(f, fx + unit, fy - unit, 0.000002, unit, INK); // Shadowed like the rest.
            small(f, fx, fy, 0, unit, fpsColor);
            // 🧊 After it: how many times the picture stopped, and why last (stalls.mjs).
            if (stallNote) {
              const sx = fx + (smallWidth(f) / 2 + 4 + smallWidth(stallNote) / 2) * unit;
              small(stallNote, sx + unit, fy - unit, 0.000002, unit, INK);
              small(stallNote, sx, fy, 0, unit, fair);
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
      projector.overlay(m, W, H);
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
          quad([x, 0.005, z], [x + 2, 0.005, z], [x + 2, 0.005, z + 2], [x, 0.005, z + 2], col);
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
        // The corners stay on the aimed square; the dot is the exact point
        // the aim meets the floor, so it glides inside the square instead of
        // jumping square to square (2026-11-08).
        let C = [Math.round((mid.x * W) / 2), Math.round((mid.y * H) / 2)];
        const f = projector.forward, e = lastEye;
        if (f && e && f[1] < -1e-4) {
          const t = (y - e.y) / f[1];
          const hit = projector.project(e.x + f[0] * t, y, e.z + f[2] * t);
          if (hit) C = [Math.round((hit.x * W) / 2), Math.round((hit.y * H) / 2)];
        }
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
    // field, facing whoever asked. How fine its grid is and how often its
    // program steps follow the eye (screen-lod.mjs): the screen says what
    // it wants, the page loads that grid, and it draws from the one it has
    // until then.
    const gaze = Math.hypot(aim.x - eye.x, aim.z - eye.z) || 1;
    const look = { x: (aim.x - eye.x) / gaze, z: (aim.z - eye.z) / gaze };
    for (const s of screens) {
      const span = SCREEN_SPAN, base = 0.4;
      const far = Math.hypot(s.x - eye.x, s.z - eye.z);
      const detail = screenDetail(far, { cols: s.cols || 0, seen: screenSeen(eye, look, s) });
      s.want = detail.cols;
      const ready = screenPrograms.get(`${s.code}@${detail.cols}`);
      const w = ready || (s.cols && screenPrograms.get(`${s.code}@${s.cols}`));
      if (!w) continue;
      w.usedAt = now;
      const cols = w.width, rows = w.height, cell = span / cols, tall = (span * rows) / cols;
      // The program steps at its rate once, whichever of its screens asks;
      // a screen redraws when its program has a newer frame or a new grid.
      if (w.steppedAt == null || (detail.hz && now - w.steppedAt >= 1000 / detail.hz)) {
        w.steppedAt = now;
        w.step();
        w.frames = (w.frames || 0) + 1;
      }
      if (!s.model || s.cols !== cols || s.drawn !== w.frames) {
        s.cols = cols;
        s.drawn = w.frames;
        const px = w.pixels;
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
    // 🌑 The shadow volumes go last in the stream; the host counts them in
    // its stencil and shades what lies inside (scene3d-webgpu.mjs).
    let shadow = null;
    if (casting && casters.length) {
      const first = host.scene.triangleCount;
      const room = Math.min(SHADOW_BUDGET, host.scene.maxTriangles - first - 64);
      const light = { x: -sunNow.x, y: -sunNow.y, z: -sunNow.z };
      let at = 0;
      cast: for (let i = 0; i < casters.length; i += 6) {
        const x = casters[i + 1], y = casters[i + 2], z = casters[i + 3], yaw = casters[i + 4], scale = casters[i + 5];
        const cy = Math.cos(yaw), sy = Math.sin(yaw);
        for (const b of boxesOf(casters[i])) {
          if (at / 18 + VOLUME_TRIANGLES > room) break cast;
          // Not the floor's dots and marks, nothing underground, nothing far overhead or far off.
          if (b.h * scale < 0.06 || Math.max(b.w, b.d) * scale < 0.06) continue;
          const top = (b.y + b.h / 2) * scale + y, foot = (b.y - b.h / 2) * scale + y;
          if (top < 0.05 || foot > eye.y + 40) continue;
          const wx = (b.x * cy + b.z * sy) * scale + x, wz = (-b.x * sy + b.z * cy) * scale + z;
          if (Math.hypot(wx - eye.x, wz - eye.z) > SHADOW_SIGHT + Math.max(b.w, b.d) * scale) continue;
          at = shadowVolume(volumeFloats, at, b, x, y, z, yaw, scale, light, 0.02, reach(top, light));
        }
      }
      if (at) {
        projector.overflow = false;
        projector.draw(volumeFloats.subarray(0, at), 0, 0, 0);
        const count = host.scene.triangleCount - first;
        // A volume cut short by the triangle budget would shade wrongly: cast none.
        if (count > 0 && !projector.overflow) shadow = { first, count, shade };
        else host.scene.triangleCount = first;
      }
    }
    casting = false;
    host.present({ clear: sky, shadow });
    if (backend === 'webgpu' && gpu.queued < 64) {
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
      gpu,
    };
  }
  return {
    canvas,
    render,
    effect,
    // The host's pixel grid: { pixel, left, top } (CSS pixels per buffer
    // pixel, and where the prompt's canvas starts); null to stand alone.
    density: setDensity,
    showFps(on) {
      SHOW_FPS = !!on;
    },
    // The host's theme: true for dark, false for light, null to follow the system.
    theme(dark) {
      hostDark = typeof dark === 'boolean' ? dark : null;
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
      // (Under the lens a place on the screen is 2 tan(a/2) from the middle: back to tan(a).)
      let kx = nx / projector.sx, ky = ny / projector.sy;
      if (projector.lens) { const r = Math.hypot(kx, ky), back = r > 1e-6 ? Math.tan(2 * Math.atan(Math.min(1.9, r) / 2)) / r : 1; kx *= back; ky *= back; }
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
