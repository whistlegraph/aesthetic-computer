// arena, 2025.4.7
// Quake-style arena with large tessellated ground, player shadow, and speedometer.

/* #region 🏁 TODO
  - [] Add more arena geometry (walls, pillars)
  - [] Strafe-jumping / bunny-hop acceleration
  + Done
  - [x] Fork from fps.mjs
  - [x] Large pre-tessellated ground plane
  - [x] Speed meter HUD + FPS counter
#endregion */

let groundPlane;
let penLocked = false;

// Speed tracking
let prevX = 0, prevY = 0, prevZ = 0;
let speedSmoothed = 0;
const SPEED_SMOOTH = 0.15;

// FPS tracking
let frameTimes = [];
let lastFrameTime = 0;

// Ground config
const GROUND_SIZE = 14;
const GRID = 14;
const GROUND_Y = -1.5;
const FOG_START_SQ = 7 * 7;
const FOG_END_SQ = 14 * 14;

const BG = [45 / 255, 48 / 255, 55 / 255];
const COLOR_A = [0.38, 0.35, 0.30, 1.0];
const COLOR_B = [0.22, 0.20, 0.19, 1.0];

function fogColor(base, distSq) {
  if (distSq <= FOG_START_SQ) return base;
  if (distSq >= FOG_END_SQ) return [BG[0], BG[1], BG[2], 0.0];
  const t = (distSq - FOG_START_SQ) / (FOG_END_SQ - FOG_START_SQ);
  return [
    base[0] + (BG[0] - base[0]) * t,
    base[1] + (BG[1] - base[1]) * t,
    base[2] + (BG[2] - base[2]) * t,
    base[3] * (1 - t),
  ];
}

function boot({ Form, penLock, system }) {
  penLock();

  const cam = system?.fps?.doll?.cam;
  if (cam) { prevX = cam.x; prevY = cam.y; prevZ = cam.z; }
  lastFrameTime = performance.now();

  // --- Vertex-colored checkerboard ground with fog ---
  const positions = [];
  const colors = [];
  const step = (GROUND_SIZE * 2) / GRID;

  for (let row = 0; row < GRID; row++) {
    for (let col = 0; col < GRID; col++) {
      const x0 = -GROUND_SIZE + col * step;
      const z0 = -GROUND_SIZE + row * step;
      const x1 = x0 + step;
      const z1 = z0 + step;

      const cx = (x0 + x1) / 2;
      const cz = (z0 + z1) / 2;
      const dSq = cx * cx + cz * cz;

      const base = (row + col) % 2 === 0 ? COLOR_A : COLOR_B;
      const c = fogColor(base, dSq);

      positions.push(
        [x0, GROUND_Y, z0, 1], [x0, GROUND_Y, z1, 1], [x1, GROUND_Y, z1, 1],
        [x0, GROUND_Y, z0, 1], [x1, GROUND_Y, z1, 1], [x1, GROUND_Y, z0, 1],
      );
      colors.push(c, c, c, c, c, c);
    }
  }

  groundPlane = new Form(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  groundPlane.noFade = true;
}

function sim({ system }) {
  const cam = system?.fps?.doll?.cam;
  if (!cam) return;

  const dx = cam.x - prevX;
  const dy = cam.y - prevY;
  const dz = cam.z - prevZ;
  const speed = Math.sqrt(dx * dx + dy * dy + dz * dz);
  speedSmoothed += (speed - speedSmoothed) * SPEED_SMOOTH;
  prevX = cam.x; prevY = cam.y; prevZ = cam.z;
}

function paint({ wipe, ink, screen, write, box }) {
  // FPS calc
  const now = performance.now();
  const dt = now - lastFrameTime;
  lastFrameTime = now;
  frameTimes.push(dt);
  if (frameTimes.length > 60) frameTimes.shift();
  const avgDt = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const fps = Math.round(1000 / avgDt);

  // Render scene
  wipe(45, 48, 55).form(groundPlane);

  // --- HUD (top-right) ---
  const font = "MatrixChunky8";
  const margin = 4;
  const lineH = 10;
  const rX = screen.width - margin; // right edge

  // FPS
  ink(fps >= 30 ? "lime" : fps >= 15 ? "yellow" : "red");
  write(`${fps} FPS`, { x: rX - 7 * 4, y: margin }, undefined, undefined, false, font);

  // Frame time
  ink(180, 180, 180);
  write(`${avgDt.toFixed(1)}ms`, { x: rX - 7 * 4, y: margin + lineH }, undefined, undefined, false, font);

  // Speed meter (bottom-center)
  const ups = speedSmoothed * 60;
  const barMaxUPS = 15;
  const barFill = Math.min(1, ups / barMaxUPS);

  const barW = Math.min(160, Math.floor(screen.width * 0.4));
  const barH = 6;
  const barX = Math.floor((screen.width - barW) / 2);
  const barY = screen.height - 16;

  ink(0, 0, 0, 140);
  box(barX - 1, barY - 1, barW + 2, barH + 2);

  const cr = barFill > 0.5 ? Math.floor(255 * ((barFill - 0.5) * 2)) : 0;
  const cg = barFill < 0.5 ? 255 : Math.floor(255 * (1 - (barFill - 0.5) * 2));
  ink(cr, cg, 50, 220);
  box(barX, barY, Math.floor(barW * barFill), barH);

  ink("white");
  write(`${ups.toFixed(1)} u/s`, { x: barX + barW + 4, y: barY - 1 }, undefined, undefined, false, font);
}

function act({ event: e, penLock }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;
  if (!penLocked && e.is("touch")) penLock();
}

export const system = "fps";
export { boot, sim, paint, act };
