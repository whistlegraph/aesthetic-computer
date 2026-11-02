import { WebGPUOskiewarScene3D } from './engine/scene3d-webgpu.mjs';
import { model, INK, PAPER, rgb } from './paint.mjs';
import { Projector } from './project.mjs';
import { WALLS, COLORS, CFG, direction, wallDistance, along, rayBox, bodyBox } from './world.mjs';

export async function createView(canvas) {
  let host, backend;
  let lost = false;
  try {
    host = await WebGPUOskiewarScene3D.create(canvas, {
      maxTriangles: 16384,
      onDeviceLost: () => {
        lost = true;
        canvas.dataset.engine = 'lost';
      },
    });
    backend = 'webgpu';
  } catch (error) {
    console.info('WebGPU unavailable; using the shared WebGL 2 host.', error.message);
    // A failed WebGPU initialization can already own the canvas context.
    const replacement = canvas.cloneNode(false);
    canvas.replaceWith(replacement);
    canvas = replacement;
    const { WebGLOskiewarScene3D } = await import('./engine/scene3d-webgl.mjs');
    host = new WebGLOskiewarScene3D(canvas, { maxTriangles: 16384, antialias: false });
    backend = 'webgl2';
  }
  canvas.dataset.engine = backend;
  const projector = new Projector(host.scene);
  const sky = [...rgb(0xc0c0c0), 1];
  const arena = model(({ box, quad }) => {
    quad([-19, 0, -19], [-19, 0, 19], [19, 0, 19], [19, 0, -19], 0xeeeeee);
    WALLS.forEach((w, i) =>
      box(
        (w.xMin + w.xMax) / 2,
        (w.yMin + w.yMax) / 2,
        (w.zMin + w.zMax) / 2,
        w.xMax - w.xMin,
        w.yMax - w.yMin,
        w.zMax - w.zMin,
        i < 4 ? PAPER : COLORS[i - 4],
        i < 4 ? 0xc0c0c0 : INK,
        PAPER,
      ),
    );
    // Court markings describe the playable space, with no scenery beyond it.
    for (const s of [0, 1])
      box(0, 0.012, s ? 12 : -12, 2, 0.02, 0.1, COLORS[s], COLORS[s], COLORS[s]);
    box(0, 0.012, 0, 10, 0.02, 0.06, INK, INK, INK);
    box(0, 0.012, 0, 0.06, 0.02, 4, INK, INK, INK);
  });
  const boardBase = model(({ rect }) => {
    rect(-12, 0, 0, 24, 6, INK);
    rect(-11.9, 0.1, -0.012, 23.8, 5.8, PAPER);
  });
  let boardKey, boardLetters;
  const fighters = COLORS.map((color) => ({
    torso: model(({ box }) => box(0, 1.02, 0, 0.42, 0.62, 0.26, color, INK, PAPER)),
    head: model(({ box, rect }) => {
      box(0, 1.59, 0, 0.44, 0.42, 0.36, PAPER, 0xc0c0c0, PAPER);
      rect(-0.14, 1.59, 0.181, 0.09, 0.07, INK);
      rect(0.05, 1.59, 0.181, 0.09, 0.07, INK);
    }),
    limb: model(({ box }) => box(0, -0.23, 0, 0.13, 0.46, 0.16, color, INK, PAPER)),
    gun: model(({ box, rect }) => {
      box(0, 0, 0, 0.32, 0.3, 0.8, 0xc0c0c0, INK, PAPER);
      box(0, -0.23, -0.13, 0.1, 0.2, 0.16, color, INK, color);
      rect(-0.13, -0.12, 0.401, 0.26, 0.24, INK);
      rect(-0.145, -0.13, -0.401, 0.29, 0.26, INK);
    }),
    hp: -1,
    display: null,
    recoil: 0,
  }));
  const rocket = model(({ box }) => box(0, 0, 0, 0.16, 0.16, 0.32, 0xffff00, INK, PAPER));
  const blast = model(({ box }) => box(0, 0, 0, 1, 1, 1, 0xffff00, INK, PAPER));
  const dot = model(({ box }) => box(0, 0, 0, 0.06, 0.06, 0.06, INK, INK, INK));
  const effects = [];
  let aspect = 1;
  function resize() {
    const width = Math.max(1, canvas.clientWidth),
      height = Math.max(1, canvas.clientHeight);
    const scale = Math.max(1, Math.floor(Math.min(width / 480, height / 270)));
    host.resize(Math.ceil(width / scale), Math.ceil(height / scale));
    aspect = width / height;
  }
  const observer = new ResizeObserver(resize);
  observer.observe(canvas);
  resize();
  function effect(e) {
    if (e.type === 'launch') fighters[e.seat].recoil = 1;
    if (e.type === 'blast') {
      if (effects.length >= 24) effects.shift();
      effects.push({ at: e.at, born: performance.now() });
    }
  }
  const draw = (...args) => projector.draw(...args);
  function render({
    players = [],
    me,
    seat = 0,
    third = false,
    rockets = [],
    scores = [0, 0],
    status = '',
  } = {}) {
    if (lost) {
      location.reload();
      return;
    }
    const start = performance.now(),
      now = start;
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
        eye = along(origin, back, Math.max(0, wallDistance(origin, back, 4) - 0.2));
      }
      let distance = wallDistance(origin, dir, 100);
      players.forEach((p, i) => {
        if (i !== seat && p?.alive)
          distance = Math.min(distance, rayBox(origin, dir, bodyBox(p), 100));
      });
      aim = along(origin, dir, Math.max(0.1, distance - 0.1));
    }
    projector.camera(eye, aim, aspect);
    draw(arena);
    const key = JSON.stringify([scores, status]);
    if (key !== boardKey) {
      boardKey = key;
      boardLetters = model(({ text }) => {
        text(scores[0], -3, 4.8, -0.025, 0.32, COLORS[0]);
        text(':', 0, 4.8, -0.025, 0.32, INK);
        text(scores[1], 3, 4.8, -0.025, 0.32, COLORS[1]);
        const label = status || 'FIRST TO 10';
        text(label, 0, 1.6, -0.025, Math.min(0.2, 22 / (label.length * 6)), INK);
      });
    }
    // Panels face into the arena. Letter geometry is drawn from the front (-Z).
    for (const end of [-1, 1]) {
      draw(boardBase, 0, 4, end * 17.95, end === 1 ? 0 : Math.PI);
      draw(boardLetters, 0, 4, end * 17.95, end === 1 ? 0 : Math.PI);
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
        const stride = Math.min(1, Math.hypot(p.vx, p.vz) / 10),
          right = direction(p.yaw - 90);
        for (const side of [-1, 1]) {
          const step = Math.sin(now / 85 + (side === 1 ? Math.PI : 0)) * stride * 0.65;
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
            -0.8 - step * 0.2,
          );
        }
      }
      const forward = direction(p.yaw, p.pitch),
        right = direction(p.yaw - 90),
        recoil = f.recoil;
      f.recoil *= 0.78;
      const x = p.x + forward.x * (1.05 - recoil * 0.1) + right.x * 0.34,
        y = p.y + forward.y * 1.05 - 0.3,
        z = p.z + forward.z * (1.05 - recoil * 0.1) + right.z * 0.34;
      draw(f.gun, x, y, z, yaw, (-p.pitch * Math.PI) / 180);
      if (f.hp !== p.health) {
        f.hp = p.health;
        f.display = model(({ text }) => text(p.health, 0, 0.065, -0.403, 0.014, PAPER));
      }
      draw(f.display, x, y, z, yaw, (-p.pitch * Math.PI) / 180);
    }
    for (const r of rockets) draw(rocket, r.x, r.y, r.z, Math.atan2(r.dir.x, r.dir.z));
    if (me?.alive) draw(dot, aim.x, aim.y, aim.z);
    for (let i = effects.length - 1; i >= 0; i--) {
      const e = effects[i],
        age = (now - e.born) / 180;
      if (age > 1) effects.splice(i, 1);
      else draw(blast, e.at.x, e.at.y, e.at.z, 0, 0, 0.2 + age * 1.5);
    }
    host.present({ clear: sky });
    // Local inspection only; no telemetry, labels or DOM overlays.
    canvas.doolStats = {
      backend,
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
