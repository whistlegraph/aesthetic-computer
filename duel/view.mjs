import { WebGPUOskiewarScene3D } from './engine/scene3d-webgpu.mjs';
import { model, INK, PAPER, rgb, tint } from './paint.mjs';
import { poseObject } from './objects.mjs';
import { OBJECT_FORMAT, sourceFrame, sourceTree, sourceHair } from './object-language.mjs';
import { Projector } from './project.mjs';
import { joinCode, hudLayout, drawJoinCode } from './hud.mjs';
import { radialSlots, ringGeometry } from './radial.mjs';
import {
  THEME,
  codeSpans,
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
// Braincells are magical purple.
const BRAIN = 0xb46bff;
// West, east, south, north.
const WALL_HUES = [0x2b3a5c, 0x4a2f5c, 0x1f4a4a, 0x3a3f4d];
const FLOOR = 0x23262f,
  WALL = 0x3a3f4d,
  WALL_SIDE = 0x2c303b,
  LINE = 0x5a5f70,
  SKY = 0x141720;
import {
  WALLS,
  COLORS,
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
  const sky = [...rgb(SKY), 1];
  const arena = model(({ box, quad }) => {
    quad([-19, 0, -19], [-19, 0, 19], [19, 0, 19], [19, 0, -19], FLOOR);
    // A big grid: the room starts as a blank sheet to build on.
    for (let v = -18; v <= 18; v += 2) {
      const c = v === 0 ? LINE : 0x2c3040;
      box(v, 0.006, 0, 0.04, 0.01, 36, c, c, c);
      box(0, 0.006, v, 36, 0.01, 0.04, c, c, c);
    }
    WALLS.forEach((w, i) =>
      box(
        (w.xMin + w.xMax) / 2,
        (w.yMin + w.yMax) / 2,
        (w.zMin + w.zMax) / 2,
        w.xMax - w.xMin,
        w.yMax - w.yMin,
        w.zMax - w.zMin,
        i < 4 ? WALL_HUES[i] : COLORS[i - 4],
        i < 4 ? tint(WALL_HUES[i], 0.8) : INK,
        PAPER,
      ),
    );
  });
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
  let ringKey, ringModel, devKey, devModel, fpsKey, fpsModel;
  let panelTop = null;
  const chalkModels = new Map();
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
  let aspect = 1;
  function resize() {
    const width = Math.max(1, canvas.clientWidth),
      height = Math.max(1, canvas.clientHeight);
    const scale = Math.max(1, Math.floor(Math.min(width / 640, height / 360)));
    host.resize(Math.ceil(width / scale), Math.ceil(height / scale));
    aspect = width / height;
  }
  const observer = new ResizeObserver(resize);
  observer.observe(canvas);
  resize();
  function effect(e) {
    // Spending braincells: a purple puff from the brain.
    if (e.type === 'think' && e.at) burstColor(e.at.x, e.at.y, e.at.z, 18, BRAIN_SPARK);
    // Ballistic contact: sparks where a round ricochets, cancels or lands.
    if (e.at && (e.type === 'ricochet' || e.type === 'spark')) burst(e.at.x, e.at.y, e.at.z, 6, 0.5);
    if (e.at && e.type === 'cancel') burst(e.at.x, e.at.y, e.at.z, 16, 0.9);
    if (e.at && e.type === 'hit') burst(e.at.x, e.at.y + 0.6, e.at.z, 10, 0.7);
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
    menu = null,
    braincells = null,
    chalk = [[], []],
    chalkLocal = [],
    chalkMode = null,
    face = null,
    owner,
    focusObject,
    builder = false,
    jobs = [],
    worldTime = 0,
    locked = false,
  } = {}) {
    if (lost) {
      location.reload();
      return;
    }
    const start = performance.now(),
      now = start;
    const solidObjects = objects.filter((o) => !o.item);
    lastWorldTime = worldTime;
    const cells = joinCode(joinURL);
    const hud = hudLayout(canvas.width, canvas.height, cells);
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
        eye = along(
          origin,
          back,
          Math.max(0, wallDistance(origin, back, 4, solidObjects, worldTime) - 0.2),
        );
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
    projector.camera(eye, aim, aspect);
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
      const code = JSON.stringify(job.preview);
      let preview = previewModels.get(job.id);
      if (!preview || preview.code !== code) {
        preview = {
          code,
          model: model(({ quad }) => {
            const boxes = job.preview?.boxes || [
              { x: 0, y: 0.1, z: 0, w: 2, h: 0.2, d: 3 },
            ];
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
          drawTree(job.preview, job.x, job.z, eye);
        } catch {}
        lingering.set(job.objectId, now + 10000);
      }
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
      // One tree at a time: an active build owns the air above it.
      else if (!jobs.length) {
        const object = objects.find((o) => o.id === id);
        if (object?.geometry.format === OBJECT_FORMAT)
          drawTree(object.geometry, object.x, object.z, eye);
      }
    }
    for (let i = 0; i < 2; i++) {
      const p = i === seat && me ? me : players[i];
      if (!p?.alive) continue;
      const f = fighters[i],
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
      } else {
        const forward = direction(p.yaw, p.pitch),
          right = direction(p.yaw - 90);
        // Arms: from each shoulder to its hand, so you see your own limbs.
        const armColor = COLORS[i] || 0xc0c0c0;
        const arms = model(({ quad }) => {
          for (const side of [-1, 1]) {
            const a = [p.x + right.x * 0.32 * side, p.y - 0.38, p.z + right.z * 0.32 * side];
            const b = [p.x + forward.x * 0.68 + right.x * 0.34 * side, p.y + forward.y * 0.68 - 0.42, p.z + forward.z * 0.68 + right.z * 0.34 * side];
            const t = 0.06;
            const up = [0, t, 0], across = [right.x * t, 0, right.z * t];
            const corner = (s, u, v) => [s[0] + up[0] * u + across[0] * v, s[1] + up[1] * u + across[1] * v, s[2] + up[2] * u + across[2] * v];
            for (const [u1, v1, u2, v2, c] of [[1, -1, 1, 1, armColor], [-1, -1, -1, 1, tint(armColor, 0.7)], [-1, 1, 1, 1, tint(armColor, 0.85)], [-1, -1, 1, -1, tint(armColor, 0.85)]])
              quad(corner(a, u1, v1), corner(a, u2, v2), corner(b, u2, v2), corner(b, u1, v1), c);
          }
        });
        draw(arms, 0, 0, 0);
        for (const side of [-1, 1])
          draw(
            f.hand,
            p.x + forward.x * 0.75 + right.x * 0.34 * side,
            p.y + forward.y * 0.75 - 0.36,
            p.z + forward.z * 0.75 + right.z * 0.34 * side,
            yaw,
            (-p.pitch * Math.PI) / 180,
          );
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
      const columns = Math.max(12, Math.floor((width - 2 * safeX - 16) / (6 * unit)));
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
        o.wall ? 'kidlisp wall' : o.owner === owner ? (pad ? '[LB] to change' : 'yours') : 'player ' + String(o.owner || '').slice(0, 4);
      const hint = drawing ? drawing : carry ? carry : focusObject
        ? `${named(focusObject)}${focusObject.wall ? '' : ` v${focusObject.version}`} - ${whose(focusObject)}`
        : input === 'touch'
          ? 'hold the right side to build'
          : pad
            ? '[LB] to build'
            : 'enter to build';
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
      const cancel = pad ? '[B] cancel' : locked ? 'esc cancel' : 'click to move / esc cancel';
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
      const rows = wallCode
        ? [
            [
              { text: `$${focusObject.code}`, color: THEME.verb },
              { text: ` kidlisp on the wall - ${focusObject.size || ''}`, color: THEME.dim },
            ],
            ...wallCode.map((l) => codeSpans(l)),
          ]
        : prompt.open
        ? [
            [{ text: title, color: THEME.verb }],
            ...wrap(writing).map((l) => (radial ? phraseSpans(l) : [{ text: l, color: THEME.text }])),
            ...(prompt.message ? wrap(prompt.message).map((l) => [{ text: l, color: THEME.number }]) : []),
            hintSpans(
              radial
                ? input === 'touch'
                  ? 'tap a choice / centre builds / outside undoes'
                  : '[A] choose / [X] build / [B] undo'
                : prompt.busy
                  ? cancel
                  : 'enter build / esc close',
              THEME.dim,
            ),
          ]
        : building
          ? [
              [
                { text: `aesel v${building.version} `, color: THEME.verb },
                { text: `${building.phase} ${elapsed}s`, color: THEME.dim },
              ],
              ...progressCode.map((l) => codeSpans(l)),
              building.owner === owner
                ? hintSpans(cancel, THEME.dim)
                : [{ text: 'player ' + building.owner.slice(0, 4), color: THEME.dim }],
            ]
          : [
              spectator
                ? hintSpans(
                    controller ? 'freecam / lb down / rb up' : 'freecam / wasd / space up / shift down',
                    THEME.text,
                  )
                : hintSpans(hint, THEME.text),
            ];
      const visible = rows.slice(
        -Math.max(3, Math.floor((height - 2 * safeY) / (11 * unit)) - 3),
      ).map((r) => clipSpans(r, columns));
      const key = JSON.stringify([visible, width, height, prompt.open, !!radial, !!wallCode]);
      if (key !== promptKey) {
        promptKey = key;
        promptModel = model(({ rect: worldRect, text }) => {
          const rect = (x, y, z, w, h, color) => worldRect(-x - w, y, z, w, h, color);
          const panelHeight = visible.length * 11 * unit + 10 * unit;
          const panel = prompt.open || building || wallCode;
          panelTop = panel ? -height / 2 + safeY + panelHeight : null;
          if (panel) {
            // A dark slab with a thin slate rule; text carries the colour.
            rect(-width / 2 + safeX, -height / 2 + safeY, 0.000004, width - 2 * safeX, panelHeight, LINE);
            rect(
              -width / 2 + safeX + unit,
              -height / 2 + safeY + unit,
              0.000003,
              width - 2 * safeX - 2 * unit,
              panelHeight - 2 * unit,
              0x15171e,
            );
          }
          visible.forEach((spans, i) =>
            drawSpans(
              { rect, text },
              spans,
              0,
              -height / 2 + safeY + panelHeight - 5 * unit - i * 11 * unit,
              unit,
              { shadow: panel ? null : INK },
            ),
          );
        });
      }
      projector.overlay(promptModel, width, height);
    }
    if (members.length || connection || cells || fps) {
      const width = canvas.width,
        height = canvas.height,
        { unit, safeX, safeY, qrSize } = hud;
      const labelColumns = Math.max(4, Math.floor((width - 2 * safeX - Math.max(qrSize, 90) - 18 * unit) / (6 * unit)));
      const labels = members.map((m) => ({
        seat: m.seat,
        label: (m.handle ? `@${m.handle}` : `GUEST ${m.seat + 1}`).slice(0, labelColumns),
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
      ]);
      if (key !== presenceKey) {
        presenceKey = key;
        presenceModel = model(({ rect: worldRect, text, handleText, small }) => {
          // Text already uses screen-right coordinates; ordinary faces need the same flip.
          const rect = (x, y, z, w, h, color) =>
            worldRect(-x - w, y, z, w, h, color);
          // Bottom left, newest at the bottom, lifted above an open panel.
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
          drawJoinCode(rect, text, cells, width, height, hud);
          if (meter || fps) {
            // No box: shadowed text right-aligned under the QR, coloured by health.
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
            if (fps) line([{ text: `${fps} fps`, color: fpsColor }]);
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
          const label = member?.handle ? `@${member.handle}` : `guest ${t.seat + 1}`;
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
      // Anchored at the top: lines start at the corner, newest lowest; once
      // the stack is full it eases upward as each line arrives.
      const top = H / 2 - safeY;
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
    // A square reticle: four L-shaped corners on whole pixels, symmetric
    // about the centre, each with a one-pixel shadow. It opens with speed,
    // closes and colours on an object (green when yours), in pixel steps.
    if (me?.alive && !radial && !menu) {
      const { unit } = hud;
      const speed = Math.min(1, Math.hypot(me.vx || 0, me.vz || 0) / 10);
      const color = focusObject ? (focusObject.owner === owner ? 0x3fb950 : THEME.verb) : PAPER;
      const half = (focusObject ? 5 : 7 + Math.round(speed * 5)) * unit,
        arm = 4 * unit,
        t = unit;
      const m = model(({ rect: worldRect }) => {
        // rect(left, bottom, w, h): screen x right, y up, whole pixels.
        const rect = flip(worldRect);
        const L = (x, y, w, h) => {
          rect(x + t, y - t, 0.000002, w, h, INK);
          rect(x, y, 0, w, h, color);
        };
        for (const sx of [-1, 1])
          for (const sy of [-1, 1]) {
            // The corner's outer pixel, then arms running inward.
            const x = sx < 0 ? -half - t : half,
              y = sy < 0 ? -half - t : half;
            L(sx < 0 ? x : x - arm + t, y, arm, t);
            L(x, sy < 0 ? y : y - arm + t, t, arm);
          }
        L(-t, -t, 2 * t, 2 * t);
      });
      projector.overlay(m, W, H);
    }
    // Aiming at an object with a $code: a small QR at the crosshair opens it
    // on aesthetic.computer from a phone.
    if (focusObject?.code && !radial && !menu) {
      const url = `https://aesthetic.computer/$${focusObject.code}`;
      const cells = joinCode(url);
      if (cells) {
        const { unit } = hud;
        const n = cells.length + 4,
          size = n * unit;
        const m = model(({ rect: worldRect, small }) => {
          const rect = flip(worldRect);
          const { safeX, safeY } = hud;
          const x0 = W / 2 - safeX - size,
            top = Math.max(-H / 2 + safeY, (panelTop ?? -Infinity) + 4 * unit) + size;
          rect(x0, top - size, 0.000002, size, size, PAPER);
          cells.forEach((row, y) => {
            for (let c = 0; c < row.length; ) {
              if (!row[c]) { c++; continue; }
              const start = c++;
              while (c < row.length && row[c]) c++;
              rect(x0 + (start + 2) * unit, top - (y + 3) * unit, 0, (c - start) * unit, unit, INK);
            }
          });
          small(`$${focusObject.code}`, x0 + size / 2, top + 9 * unit, 0, unit, THEME.verb);
        });
        projector.overlay(m, W, H);
      }
    }
    if (face && me) {
      const { unit, safeX, safeY } = hud;
      const floor = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit);
      const listTop = floor + Math.max(0, members.length - 1) * 12 * unit + 4 * unit;
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
      const fs = fighters[face.seat] || fighters[0];
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
    if (face) {
      // A face cam, bottom left above the handles: an original pixel portrait
      // in the player's own look that reacts to what happens to them.
      const { unit, safeX, safeY } = hud;
      const floor = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit);
      const listTop = floor + Math.max(0, members.length - 1) * 12 * unit + 4 * unit;
      const S = 24 * unit,
        x0 = -W / 2 + safeX + 2,
        y0 = listTop + 4 * unit; // bottom edge of the frame
      const hurt = (now - face.hurtAt) / 1000,
        grin = (now - face.grinAt) / 1000;
      const blink = now % 4200 < 130;
      const hp = face.health ?? 100;
      const look = hurt < 0.6 ? face.hurtSide : Math.max(-1, Math.min(1, Math.round((face.turn || 0) * 1.4)));
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
            [...row].forEach((ch, c) => ch === '#' && px(27 + c, 20 - r, 1, 1, r === 2 && c === 3 ? 0x8e44ff : 0xb46bff)),
          );
          const n = braincells >= 1e6 ? `${(braincells / 1e6).toFixed(1)}m` : braincells >= 1e4 ? `${Math.round(braincells / 1e3)}k` : `${Math.round(braincells)}`;
          small(n, x0 + (36 + (n.length * 4) / 2) * unit, y0 + 21 * unit, 0, unit, 0xd7a6ff);
        }
        // The health bar under it, in health colours.
        const hpc = hp > 60 ? 0x3fb950 : hp > 30 ? 0xffe066 : 0xff6b6b;
        px(0, -3, 24, 2, 0x2c3040, 0.000001);
        px(0, -3, Math.max(0, Math.round((24 * hp) / 100)), 2, hpc);
        if (me) return;
        px(0, 0, 24, 24, 0x15171e, 0.000003);
        // Shoulders in the seat colour.
        px(3, 0, 18, 5, COLORS[face.seat] || 0xc0c0c0, 0.000002);
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
        const ex = [hx + 2 + look, hx + 7 + look];
        for (const x of ex) {
          if (hurt < 0.6) px(x, hy + 8, 3, 1, INK);
          else if (blink) px(x, hy + 8, 3, 1, INK);
          else px(x, hy + 7, 2, 3, INK);
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
    // Chalk: a cached model per stroke, rebuilt only while it grows; your
    // own newest points draw at once, before the server echoes them.
    const ribbon = (points, ink) =>
      model(({ quad }) => {
        const w = 0.02;
        for (let k = 1; k < points.length; k++) {
          const a = points[k - 1], b = points[k];
          quad([a.x - w, a.y, a.z], [b.x - w, b.y, b.z], [b.x + w, b.y, b.z], [a.x + w, a.y, a.z], ink);
          quad([a.x, a.y - w, a.z], [b.x, b.y - w, b.z], [b.x, b.y + w, b.z], [a.x, a.y + w, a.z], ink);
        }
      });
    const liveChalk = new Set();
    chalk.forEach((strokes, seatIndex) => {
      const ink = seatIndex === 0 ? 0xffd9cf : 0xd6ecff;
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
    // KidLisp walls: step each program about 20 times a second and lay its
    // pixels on the wall as a grid of cells, unmirrored from inside.
    for (const w of walls) {
      if (!w.model || now - (w.at || 0) > 50) {
        w.at = now;
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
              if (w.face === 'north') quad([u0, y0, 17.97], [u1, y0, 17.97], [u1, y1, 17.97], [u0, y1, 17.97], c);
              else quad([-17.97, y0, -u0], [-17.97, y0, -u1], [-17.97, y1, -u1], [-17.97, y1, -u0], c);
              i += n;
            }
        });
      }
      draw(w.model, 0, 0, 0);
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
    destroy() {
      observer.disconnect();
      host.destroy();
    },
  };
}
