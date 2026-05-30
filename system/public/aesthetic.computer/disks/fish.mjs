// Fish, 2026.05.30
// A realtime raycast (SDF raymarched) 3D fish that swims in place.

/* 📝 Engineering Notes
  Pure software renderer — no WebGL, no Form/Camera. Every frame `paint`
  walks `screen.pixels` (a Uint8ClampedArray) and raymarches a signed
  distance field per pixel.

  The fish is a smooth-union of an ellipsoid body + flattened-ellipsoid
  fins (caudal tail, dorsal, pectoral). A lateral wiggle bends the
  distance field along the body axis so it looks like it's swimming.

  Performance: we march at a 2px STEP and fill 2x2 blocks. Inner loops
  use scalar math (no array allocation) to avoid GC churn. Drag left/right
  to spin the fish.
*/

const { sin, cos, sqrt, abs, min, max, pow, hypot } = Math;

let time = 0;
let yaw = 0; // current heading
let targetYaw = 0; // drag target
let dragging = false;
let lastX = 0;
let zoom = 4.0; // camera distance from the glass sphere
let targetZoom = 4.0; // scroll-wheel target

// Fish swim path — set once per frame in paint(). The fish orbits inside the
// glass sphere on a horizontal circle (+ vertical bob) and faces its heading.
let fcx = 0, fcy = 0, fcz = 0; // fish center (world)
let ffx = 1, ffz = 0; // fish forward (unit, in the XZ plane)

// Performance instrumentation + adaptive-resolution controller.
let stepOverride = null; // offline renderer forces a fixed STEP (full-res stills)
let showStats = true; // FPS/stats overlay (toggle with 'f')
let dynStep = 0; // current adaptive pixel step (0 = pick from resolution on frame 1)
let frameMs = 16; // smoothed raymarch work time per frame (ms)
let interMs = 16; // smoothed wall time between frames (ms) -> displayed fps
let lastT = 0; // performance.now() of the previous frame

// Pixel block size is chosen per-frame from the window size so the raymarch
// cost stays roughly constant (~this many samples) regardless of resolution.
// Small windows stay crisp (STEP 2); large windows coarsen gracefully.
const SAMPLE_BUDGET = 42000;
const MAX_STEPS = 64;
const SURF = 0.0015;

function pickStep(width, height) {
  const s = Math.ceil(Math.sqrt((width * height) / SAMPLE_BUDGET));
  return s < 2 ? 2 : s > 5 ? 5 : s;
}

const nowMs = () =>
  typeof performance !== "undefined" ? performance.now() : Date.now();

const ZOOM_MIN = 2.0; // closest the camera can pull in (stays outside the bowl)
const ZOOM_MAX = 10.0; // farthest it can pull out

// Glass sphere ("bowl") the fish swims inside, centered at the origin.
const R_BOWL = 1.5;
const R_BOWL2 = R_BOWL * R_BOWL;

// The fish is modeled at unit scale, then shrunk to fit and sent on an orbit.
const FISH_SCALE = 0.46;
const INV_SCALE = 1 / FISH_SCALE;
const R_ORBIT = 0.58; // radius of the swim circle inside the bowl
// World-space bound around the fish center (unit bound ~1.9 + offset, scaled).
// Lets us march only the small moving region the fish occupies, not the bowl.
const R_FISH_BOUND = 0.97;
const R_FISH_BOUND2 = R_FISH_BOUND * R_FISH_BOUND;

function boot() {
  time = 0;
}

// --- Signed distance field -------------------------------------------------

// Ellipsoid (bound, not exact, but stable for raymarching).
function sdEllipsoid(px, py, pz, rx, ry, rz) {
  const k0 = sqrt((px * px) / (rx * rx) + (py * py) / (ry * ry) + (pz * pz) / (rz * rz));
  const k1 = sqrt(
    (px * px) / (rx * rx * rx * rx) +
      (py * py) / (ry * ry * ry * ry) +
      (pz * pz) / (rz * rz * rz * rz),
  );
  if (k1 === 0) return 0;
  return (k0 * (k0 - 1.0)) / k1;
}

// Polynomial smooth minimum (blends shapes seamlessly).
function smin(a, b, k) {
  const h = max(0, min(1, 0.5 + (0.5 * (b - a)) / k));
  return b * (1 - h) + a * h - k * h * (1 - h);
}

// World point -> fish-local (unit-scale) coords: translate to the fish
// center, rotate so the heading aligns with local +x, then unscale. Forward
// basis F=(ffx,0,ffz), right=(ffz,0,-ffx), up=(0,1,0). Writes [lx,ly,lz].
function worldToFishLocal(px, py, pz, out) {
  const qx = px - fcx,
    qy = py - fcy,
    qz = pz - fcz;
  out[0] = (qx * ffx + qz * ffz) * INV_SCALE;
  out[1] = qy * INV_SCALE;
  out[2] = (qx * ffz - qz * ffx) * INV_SCALE;
}

const L = [0, 0, 0]; // scratch for local coords

// The fish, as a world-space SDF (orbiting + oriented + scaled). Body axis is
// local +x (head at +x, tail at -x); the swim wiggle is lateral in local z.
function fishSDF(px, py, pz) {
  worldToFishLocal(px, py, pz, L);
  px = L[0];
  py = L[1];
  pz = L[2];

  // Swim wiggle: increases toward the tail.
  const tailward = max(0, (0.9 - px) * 0.55); // 0 at head -> larger at tail
  const wiggle = sin(px * 2.6 - time * 6.0) * 0.16 * tailward;
  let qz = pz - wiggle;

  // Body — elongated ellipsoid, slightly fuller near the head.
  const taper = 1.0 - max(0, -px) * 0.12;
  let d = sdEllipsoid(px, py, qz, 1.0, 0.46 * taper, 0.30 * taper);

  // Caudal (tail) fin — flattened ellipsoid that fans vertically.
  const tx = px + 1.18;
  const tfin = sdEllipsoid(tx, py * 0.62, qz, 0.42, 0.5, 0.035);
  d = smin(d, tfin, 0.12);

  // Dorsal fin (top) — flattened, thin in z, sitting on the back.
  const dfin = sdEllipsoid(px * 0.9 + 0.05, py - 0.5, qz, 0.5, 0.32, 0.035);
  d = smin(d, dfin, 0.1);

  // Re-apply the scale we divided out in worldToFishLocal so this stays a
  // valid world-space distance (sdf(p/s) * s).
  return d * FISH_SCALE;
}

// Surface normal via the tetrahedron trick — 4 SDF taps instead of 6.
function fishNormal(px, py, pz, out) {
  const e = 0.0015;
  // Tetrahedron offset directions: (+--), (-+-), (--+), (+++).
  const a = fishSDF(px + e, py - e, pz - e);
  const b = fishSDF(px - e, py + e, pz - e);
  const c = fishSDF(px - e, py - e, pz + e);
  const d = fishSDF(px + e, py + e, pz + e);
  let nx = a - b - c + d;
  let ny = -a + b - c + d;
  let nz = -a - b + c + d;
  const len = hypot(nx, ny, nz) || 1;
  out[0] = nx / len;
  out[1] = ny / len;
  out[2] = nz / len;
}

// --- Render ----------------------------------------------------------------

const N = [0, 0, 0];

function paint({ screen, ink, write }) {
  const { width, height, pixels } = screen;
  const offline = stepOverride !== null;
  const t0 = nowMs();

  // Track wall time between frames (the perceived fps) and pick a pixel step.
  if (!offline) {
    if (lastT) interMs += (t0 - lastT - interMs) * 0.15;
    lastT = t0;
    if (!dynStep) dynStep = pickStep(width, height);
  }
  const STEP = offline ? stepOverride : dynStep;

  if (!offline) time += 0.016;

  // Ease yaw toward drag target.
  yaw += (targetYaw - yaw) * 0.12;
  // Idle drift when not dragging.
  if (!dragging) targetYaw += 0.004;
  // Ease zoom toward scroll-wheel target.
  zoom += (targetZoom - zoom) * 0.15;

  // Camera basis (rotating fish == rotating world about y by -yaw).
  const cy = cos(yaw),
    sy = sin(yaw);
  const camZ = zoom;
  const focal = 1.5;
  const invH = 1 / height;
  const halfW = width * 0.5;
  const halfH = height * 0.5;

  // Swim path: orbit a circle inside the bowl, bob vertically, and face the
  // direction of travel (tangent to the circle).
  const orbit = time * 0.5;
  const oc = cos(orbit),
    os = sin(orbit);
  fcx = R_ORBIT * oc;
  fcz = R_ORBIT * os;
  fcy = sin(time * 0.7) * 0.22;
  ffx = -os; // d/dt of (cos, sin) — unit tangent
  ffz = oc;

  // Light direction (normalized).
  const lx = 0.42,
    ly = 0.78,
    lz = 0.46;

  for (let py = 0; py < height; py += STEP) {
    for (let px = 0; px < width; px += STEP) {
      // Screen -> ray (square aspect via height).
      const u = (px + 0.5 - halfW) * invH;
      const v = -(py + 0.5 - halfH) * invH;

      // Ray dir in camera space, then yaw-rotate into world.
      let rdx = u,
        rdy = v,
        rdz = -focal;
      const rl = hypot(rdx, rdy, rdz);
      rdx /= rl;
      rdy /= rl;
      rdz /= rl;
      // rotate about y
      let wrdx = rdx * cy + rdz * sy;
      let wrdz = -rdx * sy + rdz * cy;
      const wrdy = rdy;

      // Ray origin (camera) rotated the same way. Camera always sits outside
      // the bowl, looking at the origin.
      const rox = camZ * sy;
      const roz = camZ * cy;
      const roy = 0;

      let r, g, b;

      // --- Glass bowl intersection (center = origin). Outer cull + glass shell.
      const bdotB = rox * wrdx + roy * wrdy + roz * wrdz;
      const ccB = rox * rox + roy * roy + roz * roz - R_BOWL2;
      const discB = bdotB * bdotB - ccB;

      if (discB <= 0) {
        // Misses the bowl entirely — dim room behind it.
        const grad = py * invH;
        r = 12 + grad * 10;
        g = 16 + grad * 16;
        b = 26 + grad * 26;
      } else {
        const sqB = sqrt(discB);
        const tFront = -bdotB - sqB; // near (front) glass surface
        const tBack = -bdotB + sqB; // far (back) glass surface

        // March the fish only within its small moving bound sphere (center =
        // the orbiting fish center), not the whole bowl.
        const ocx = rox - fcx,
          ocy = roy - fcy,
          ocz = roz - fcz;
        const bdotF = ocx * wrdx + ocy * wrdy + ocz * wrdz;
        const ccF = ocx * ocx + ocy * ocy + ocz * ocz - R_FISH_BOUND2;
        const discF = bdotF * bdotF - ccF;

        let hit = false;
        let hx = 0,
          hy = 0,
          hz = 0;
        if (discF > 0) {
          const sqF = sqrt(discF);
          const tfExit = -bdotF + sqF;
          if (tfExit > 0) {
            const tfEnter = -bdotF - sqF;
            let t = tfEnter > 0 ? tfEnter : 0;
            for (let i = 0; i < MAX_STEPS; i++) {
              hx = rox + wrdx * t;
              hy = roy + wrdy * t;
              hz = roz + wrdz * t;
              const d = fishSDF(hx, hy, hz);
              if (d < SURF) {
                hit = true;
                break;
              }
              t += d;
              if (t > tfExit) break;
            }
          }
        }

        if (hit) {
          fishNormal(hx, hy, hz, N);
          const nx = N[0],
            ny = N[1],
            nz = N[2];

          // View dir (toward camera) = -ray dir.
          const vx = -wrdx,
            vy = -wrdy,
            vz = -wrdz;

          const diff = max(0, nx * lx + ny * ly + nz * lz);
          const ndl = nx * lx + ny * ly + nz * lz;
          const refx = 2 * ndl * nx - lx;
          const refy = 2 * ndl * ny - ly;
          const refz = 2 * ndl * nz - lz;
          const spec = pow(max(0, refx * vx + refy * vy + refz * vz), 24) * 0.8;
          const fres = pow(1 - max(0, nx * vx + ny * vy + nz * vz), 3);

          // Hit point in fish-local (unit) coords drives the palette + eye.
          const lqx = hx - fcx,
            lqy = hy - fcy,
            lqz = hz - fcz;
          const lhx = (lqx * ffx + lqz * ffz) * INV_SCALE;
          const lhy = lqy * INV_SCALE;
          const lhz = (lqx * ffz - lqz * ffx) * INV_SCALE;

          // Base koi palette: dark warm back, orange flank, pale belly.
          const belly = max(0, min(1, 0.5 - lhy));
          const back = max(0, min(1, 0.5 + lhy));
          const patch =
            0.5 + 0.5 * sin(lhx * 5.0 + 1.3) * sin(lhz * 6.0 + lhy * 4.0);
          let baseR = 250,
            baseG = 120,
            baseB = 40;
          baseR = baseR * (1 - belly * 0.1) + 255 * belly * 0.6;
          baseG = baseG * (1 - belly) + 235 * belly;
          baseB = baseB * (1 - belly) + 215 * belly;
          baseR = baseR * (1 - back * 0.25) + 180 * back * 0.25;
          baseG = baseG * (1 - back * 0.45);
          baseB = baseB * (1 - back * 0.55);
          baseR -= patch * 30;
          baseG -= patch * 20;

          const li = 0.28 + diff * 0.95;
          r = baseR * li + spec * 255 + fres * 70;
          g = baseG * li + spec * 235 + fres * 90;
          b = baseB * li + spec * 215 + fres * 150;

          // Eye — dark sphere with a glint, on each flank near the head.
          const edx = lhx - 0.66;
          const edy = lhy - 0.1;
          const edz = abs(lhz) - 0.2;
          if (hypot(edx, edy, edz) < 0.1) {
            r = 22;
            g = 18;
            b = 20;
            if (hypot(lhx - 0.69, lhy - 0.13) < 0.03) r = g = b = 245;
          }
        } else {
          // Water inside the bowl: lit aqua with caustic shimmer.
          const grad = py * invH;
          r = 18 + grad * 14;
          g = 90 + grad * 40;
          b = 140 + grad * 70;
          const caust =
            sin(px * 0.05 + time * 1.3) * sin(py * 0.06 - time * 0.9) +
            sin((px + py) * 0.03 + time * 2.1);
          const c = max(0, caust) * 16;
          r += c * 0.4;
          g += c * 0.8;
          b += c;
        }

        // --- Glass shell overlay: fresnel rim + a sharp specular glint on the
        // front surface. Brightens the silhouette so it reads as a glass orb.
        const ft = tFront > 0 ? tFront : tBack;
        const gx = (rox + wrdx * ft) / R_BOWL;
        const gy = (roy + wrdy * ft) / R_BOWL;
        const gz = (roz + wrdz * ft) / R_BOWL;
        const ndv = -(gx * wrdx + gy * wrdy + gz * wrdz); // dot(N, view)
        const rim = pow(1 - max(0, ndv), 3) * 0.6;
        const gndl = gx * lx + gy * ly + gz * lz;
        const grx = 2 * gndl * gx - lx,
          gry = 2 * gndl * gy - ly,
          grz = 2 * gndl * gz - lz;
        const glint =
          pow(max(0, -(grx * wrdx + gry * wrdy + grz * wrdz)), 48) * 255;
        r = r * (1 - rim) + 205 * rim + glint;
        g = g * (1 - rim) + 230 * rim + glint;
        b = b * (1 - rim) + 255 * rim + glint;
      }

      // Clamp + fill the STEPxSTEP block.
      const R = r < 0 ? 0 : r > 255 ? 255 : r;
      const G = g < 0 ? 0 : g > 255 ? 255 : g;
      const B = b < 0 ? 0 : b > 255 ? 255 : b;

      for (let by = 0; by < STEP && py + by < height; by++) {
        let idx = ((py + by) * width + px) * 4;
        for (let bx = 0; bx < STEP && px + bx < width; bx++) {
          pixels[idx] = R;
          pixels[idx + 1] = G;
          pixels[idx + 2] = B;
          pixels[idx + 3] = 255;
          idx += 4;
        }
      }
    }
  }

  if (offline) return; // stills: no controller, no overlay

  // Measure the raymarch work cost and steer STEP toward a 60fps budget.
  // Keep work in a band well under the 16.6ms frame; coarsen if we exceed it,
  // sharpen (toward STEP 1) when there's headroom, so quality fills the budget.
  frameMs += (nowMs() - t0 - frameMs) * 0.15;
  if (frameMs > 14 && dynStep < 6) dynStep++;
  else if (frameMs < 8 && dynStep > 1) dynStep--;

  // Little stats overlay (top-left): fps / work-ms / current step.
  if (showStats && ink && write) {
    const fps = Math.min(999, Math.round(1000 / Math.max(interMs, 0.001)));
    const label = `${fps}fps ${frameMs.toFixed(1)}ms s${STEP}`;
    ink(0, 0, 0, 150).box(2, 2, label.length * 6 + 6, 13);
    const col =
      fps >= 58 ? [120, 255, 150] : fps >= 30 ? [255, 220, 120] : [255, 120, 120];
    ink(col).write(label, { x: 5, y: 4 });
  }
}

function act({ event: e }) {
  if (e.is("touch")) {
    dragging = true;
    lastX = e.x;
  }
  if (e.is("draw") && dragging) {
    targetYaw += (e.x - lastX) * 0.01;
    lastX = e.x;
  }
  if (e.is("lift")) dragging = false;

  // Scroll wheel zooms the camera in/out of the fish's environment.
  if (e.is("scroll") || e.is("wheel")) {
    const dy = e.delta?.y ?? e.y ?? 0;
    targetZoom = max(ZOOM_MIN, min(ZOOM_MAX, targetZoom + dy * 0.12));
  }

  // 'f' toggles the FPS/stats overlay.
  if (e.is("keyboard:down:f")) showStats = !showStats;
}

export { boot, paint, act };
