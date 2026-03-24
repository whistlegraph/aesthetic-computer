// blank, 26.03.20
// AC Blank — AC Native Laptop product page & checkout

const { floor, sin, cos, abs, min, max, PI, sqrt } = Math;

// Module state
let amount = 12800;
let checkoutUrl = null;
let checkoutReady = false;
let checkoutError = null;
let checkoutLoading = false;
let buyPending = false;
let thanks = false;

// UI elements
let buyBtn = null;
let manualBtn = null;
let paperBtn = null;
let userHandle = null;

// Handle cycling (when not logged in)
let allHandles = [];
let cycleIndex = 0;
let cycleTimer = 0;
const CYCLE_INTERVAL = 90; // frames between handle switches

// Whitelist handles to highlight
const HIGHLIGHT_HANDLES = ["sat", "fifi", "prutti"];

// Handle colors cache: Map<handle, Array<{r, g, b}> | null>
const handleColors = new Map();
let currentDisplayColors = null; // colors for the currently displayed handle

async function fetchHandleColor(handle) {
  const clean = handle.startsWith("@") ? handle.slice(1) : handle;
  if (handleColors.has(clean)) return handleColors.get(clean);
  try {
    const res = await fetch(
      `/.netlify/functions/handle-colors?handle=${encodeURIComponent(clean)}`,
    );
    if (res.ok) {
      const data = await res.json();
      handleColors.set(clean, data.colors || null);
      return data.colors || null;
    }
  } catch {}
  handleColors.set(clean, null);
  return null;
}

const MANUAL_URL =
  "https://download.lenovo.com/pccbbs/mobiles_pdf/tp_11e-yoga_gen6_ug_en.pdf";
const PAPER_URL =
  "https://papers.aesthetic.computer/plorking-the-planet-26-arxiv-cards.pdf";
const DESCRIPTION =
  "A @jeffrey approved, refurbished Thinkpad 11e Yoga Gen 6 pre-flashed with AC Native OS and Live USB recovery stick.";
const AUTH_TIMEOUT_MS = 1200;

async function getOptionalToken(api) {
  if (!api?.authorize) return null;

  try {
    return await Promise.race([
      api.authorize().catch(() => null),
      new Promise((resolve) => setTimeout(() => resolve(null), AUTH_TIMEOUT_MS)),
    ]);
  } catch {
    return null;
  }
}

// Animation
let frame = 0;

const charH = 16;

function displayAmount(amt) {
  return `$${(amt / 100).toFixed(0)}`;
}

function getBuyText() {
  if (buyPending) return "CHECKING OUT...";
  return `BUY ${displayAmount(amount)}`;
}

async function boot({ params, ui, screen, cursor, hud, api, handle }) {
  cursor("native");
  hud.labelBack();

  if (params[0] === "thanks") {
    thanks = true;
    return;
  }

  userHandle = handle();
  setupButtons(ui, screen);
  fetchCheckout(api);
  if (!userHandle) fetchHandles(screen);
  // Prefetch colors for logged-in user
  if (userHandle) fetchHandleColor(userHandle);
  // Prefetch colors for whitelisted handles
  HIGHLIGHT_HANDLES.forEach((h) => fetchHandleColor(h));
}

// Max chars that fit on the laptop screen text line: "hi @handle"
// MatrixChunky8 is ~4px per char, screen plane is ~56px wide → ~14 chars
const MAX_SCREEN_CHARS = 14;

async function fetchHandles(screen) {
  try {
    const res = await fetch("/api/handles?tenant=aesthetic");
    if (!res.ok) return;
    const data = await res.json();
    allHandles = (data.handles || [])
      .map((h) => h.handle)
      .filter(Boolean)
      .filter((h) => `hi @${h}`.length <= MAX_SCREEN_CHARS);
    // Put highlighted handles first, then shuffle the rest
    const highlighted = [];
    const rest = [];
    for (const h of allHandles) {
      if (HIGHLIGHT_HANDLES.includes(h.toLowerCase())) highlighted.push(h);
      else rest.push(h);
    }
    for (let i = rest.length - 1; i > 0; i--) {
      const j = floor(Math.random() * (i + 1));
      [rest[i], rest[j]] = [rest[j], rest[i]];
    }
    allHandles = [...highlighted, ...rest];
    // Prefetch colors for all displayed handles
    allHandles.forEach((h) => fetchHandleColor(h));
  } catch {}
}

function setupButtons(ui, screen) {
  buyBtn = new ui.TextButton(getBuyText(), { center: "x", bottom: 20, screen });
  manualBtn = new ui.TextButton("MANUAL", { x: 6, bottom: 20, screen });
  paperBtn = new ui.TextButton("PAPER", { x: 6 + manualBtn.width + 4, bottom: 20, screen });
}

async function fetchCheckout(api) {
  if (checkoutLoading) return;

  checkoutLoading = true;
  checkoutReady = false;
  checkoutError = null;
  checkoutUrl = null;

  try {
    const headers = { "Content-Type": "application/json" };
    const token = await getOptionalToken(api);
    if (token) headers.Authorization = `Bearer ${token}`;

    const res = await fetch("/api/blank?new=true", {
      method: "POST",
      headers,
      body: JSON.stringify({ amount, currency: "usd" }),
    });

    if (!res.ok) {
      checkoutError = `Checkout failed: ${res.status}`;
      return;
    }

    const data = await res.json();
    if (data?.location) {
      checkoutUrl = data.location;
      checkoutReady = true;
    } else {
      checkoutError = data?.error || "Checkout failed";
    }
  } catch (e) {
    checkoutError = e?.message || "Checkout error";
  } finally {
    checkoutLoading = false;
  }
}

function paint($) {
  const { wipe, ink, line, screen, dark: isDark, tri } = $;
  frame += 1;
  const w = screen.width;
  const h = screen.height;

  // Theme colors
  const bg = isDark ? [14, 12, 20] : [232, 228, 238];
  const fg = isDark ? 255 : 20;
  const fgDim = isDark ? 120 : 100;


  wipe(...bg);

  // Animated backdrop — drifting primary/secondary color bands
  {
    const t = frame * 0.01;
    const alpha = isDark ? 35 : 20;
    // Red, blue, yellow — primary colors that pop against the plain laptop
    const palette = [
      [200, 30, 30],   // red
      [30, 50, 200],   // blue
      [220, 200, 20],  // yellow
      [20, 180, 60],   // green
      [180, 40, 180],  // magenta
    ];
    for (let i = 0; i < palette.length; i++) {
      const phase = i * (PI * 2) / palette.length;
      const bw = floor(w * 0.5);
      const bh = floor(h * 0.35);
      const bx = max(0, min(w - bw, floor(w * 0.5 + sin(t + phase) * w * 0.4 - bw / 2)));
      const by = max(0, min(h - bh, floor(h * 0.5 + cos(t * 0.7 + phase) * h * 0.35 - bh / 2)));
      const [cr, cg, cb] = palette[i];
      ink(cr, cg, cb, alpha).box(bx, by, bw, bh, "fill");
    }
  }

  // Product description (top of page)
  ink(fgDim).write(DESCRIPTION, { x: 6, y: 6, width: w - 12, screen });

  // Thanks page
  if (thanks) {
    const cy = floor(h / 2);
    ink(fg).write("your blank is coming.", { center: "x", y: cy - 30, screen });
    ink(fgDim).write("we'll be in touch.", { center: "x", y: cy, screen });
    return;
  }

  // Compute button zone height
  const btnH = buyBtn ? buyBtn.height + 6 : 26;
  const uiZoneH = btnH * 2 + 20;
  const contentBottom = h - uiZoneH - 20;

  // 💻 Wireframe laptop (turntable swivel)
  {
    const cx = floor(w / 2);
    const cy = floor(h * 0.54); // slightly below center for optical balance
    // Keep laptop fully within viewport (account for rotation + lid overshoot)
    const availW = w * 0.42;
    const availH = (contentBottom - 20) * 0.32;
    const size = min(availW, availH);
    const fov = size * 5.5;

    // Slow turntable + gentle tilt variation (see it from all angles)
    const ay = frame * 0.006;
    const ax = 0.3 + sin(frame * 0.003) * 0.25 + sin(frame * 0.0017) * 0.15;

    // Animated hinge: cycle through closed → laptop → flat → tablet
    const closedAngle = 0.02;
    const laptopAngle = PI * 0.67;
    const flatAngle = PI;
    const tabletAngle = PI * 2 - 0.02;
    const keyframes = [closedAngle, laptopAngle, flatAngle, tabletAngle, laptopAngle];
    const totalPhases = keyframes.length;
    const phaseLen = 180;
    const t = (frame % (totalPhases * phaseLen)) / phaseLen;
    const phase = floor(t);
    const frac = t - phase;
    const ease = frac < 0.5 ? 2 * frac * frac : 1 - 2 * (1 - frac) * (1 - frac);
    const from = keyframes[phase % totalPhases];
    const to = keyframes[(phase + 1) % totalPhases];
    const hingeAngle = from + ease * (to - from);

    // Dimensions from spec: 293mm × 207mm × 19.9mm
    const hw = 1.44, hh = 0.07, hd = 1.0;
    const lidThick = hh * 0.9; // lid thinner than base (display panel)

    // Barrel hinge: attachment point travels a semicircular arc
    // from base top-back (closed) to base bottom-back (tablet).
    // Barrel radius = half total stack height (base + lid clearance)
    const R = hh + lidThick / 2;

    // Attachment point on the barrel circle
    // hingeAngle 0 → top-back (0, 0), hingeAngle 2π → bottom-back (2*hh, 0)
    const attachY = R * (1 - cos(hingeAngle / 2));
    const attachZ = R * sin(hingeAngle / 2);

    // Base slab: y 0 (top) to 2*hh (bottom), z 0 (back) to 2*hd (front)
    const base = [
      [-hw, 0, 0], [hw, 0, 0], [hw, 2 * hh, 0], [-hw, 2 * hh, 0],
      [-hw, 0, 2 * hd], [hw, 0, 2 * hd], [hw, 2 * hh, 2 * hd], [-hw, 2 * hh, 2 * hd],
    ];

    // Lid local: screen face at y=0, outer face at y=-lidThick
    // z from 0 (hinge edge) to 2*hd (front edge)
    const cosH = cos(hingeAngle), sinH = sin(hingeAngle);
    const lidLocal = [
      [-hw, 0, 0], [hw, 0, 0],
      [hw, -lidThick, 0], [-hw, -lidThick, 0],
      [-hw, 0, 2 * hd], [hw, 0, 2 * hd],
      [hw, -lidThick, 2 * hd], [-hw, -lidThick, 2 * hd],
    ];

    // Lid transform: rotate by hingeAngle (2:1 vs barrel sweep), translate to attachment
    const lid = lidLocal.map(([lx, ly, lz]) => {
      const ry = ly * cosH - lz * sinH;
      const rz = ly * sinH + lz * cosH;
      return [lx, attachY + ry, attachZ + rz];
    });

    // Hinge barrels — centered at barrel center, rotate at barrel sweep rate
    const barrelCY = R; // = hh + lidThick/2
    const barrelCZ = 0;
    const barrelW = 0.28;
    const barrelH = hh * 1.6;
    const barrelD = hh * 1.4;
    const barrelPositions = [-hw * 0.65, hw * 0.65];
    const barrelSweep = hingeAngle / 2;
    const cB = cos(barrelSweep), sB = sin(barrelSweep);
    const hingeVerts = [];
    for (const bx of barrelPositions) {
      const bw = barrelW / 2, bh = barrelH / 2, bd = barrelD / 2;
      const barrelLocal = [
        [bx - bw, -bh, -bd], [bx + bw, -bh, -bd],
        [bx + bw, bh, -bd], [bx - bw, bh, -bd],
        [bx - bw, -bh, bd], [bx + bw, -bh, bd],
        [bx + bw, bh, bd], [bx - bw, bh, bd],
      ];
      hingeVerts.push(barrelLocal.map(([vx, vy, vz]) => {
        const ry = vy * cB - vz * sB;
        const rz = vy * sB + vz * cB;
        return [vx, barrelCY + ry, barrelCZ + rz];
      }));
    }

    const halfEdges = [
      [0, 1], [1, 2], [2, 3], [3, 0],
      [4, 5], [5, 6], [6, 7], [7, 4],
      [0, 4], [1, 5], [2, 6], [3, 7],
    ];

    const project = ([x, y, z]) => {
      let rx = x * cos(ay) - z * sin(ay);
      let rz = x * sin(ay) + z * cos(ay);
      let ry = y * cos(ax) - rz * sin(ax);
      rz = y * sin(ax) + rz * cos(ax);
      const scale = fov / (fov + rz * size);
      return [cx + rx * size * scale, cy + ry * size * scale, rz];
    };

    const projBase = base.map(project);
    const projLid = lid.map(project);

    // 🎨 Z-sorted rendering: collect all faces, sort back-to-front, draw
    const faceQuads = [
      [0, 1, 5, 4], [3, 2, 6, 7], [0, 3, 7, 4],
      [1, 2, 6, 5], [0, 1, 2, 3], [4, 5, 6, 7],
    ];
    // ThinkPad black/dark gray palette
    const baseColor = isDark ? [28, 28, 30] : [42, 42, 45];
    const lidColor = isDark ? [24, 24, 26] : [38, 38, 40];
    const keyColor = isDark ? [38, 38, 42] : [52, 52, 56];

    // Collect all drawable quads with z-depth
    const drawList = [];

    // 3D face normal backface culling — works regardless of vertex winding
    // Compute view direction (camera looks toward +z in world, before rotations)
    // After ay (Y-rot) and ax (X-rot), the view direction is:
    const viewDirX = sin(ay) * cos(ax);
    const viewDirY = sin(ax);
    const viewDirZ = cos(ay) * cos(ax);

    const addFaces = (verts3d, proj, color, tag) => {
      const frontFaces = new Set();
      // Compute box center for outward normal correction
      let cx3 = 0, cy3 = 0, cz3 = 0;
      for (const v of verts3d) { cx3 += v[0]; cy3 += v[1]; cz3 += v[2]; }
      cx3 /= verts3d.length; cy3 /= verts3d.length; cz3 /= verts3d.length;

      for (let fi = 0; fi < faceQuads.length; fi++) {
        const [a, b, c, d] = faceQuads[fi];
        // Face center
        const fcx = (verts3d[a][0] + verts3d[b][0] + verts3d[c][0] + verts3d[d][0]) / 4;
        const fcy = (verts3d[a][1] + verts3d[b][1] + verts3d[c][1] + verts3d[d][1]) / 4;
        const fcz = (verts3d[a][2] + verts3d[b][2] + verts3d[c][2] + verts3d[d][2]) / 4;
        // 3D face normal via cross product of two edges
        const e1x = verts3d[b][0] - verts3d[a][0], e1y = verts3d[b][1] - verts3d[a][1], e1z = verts3d[b][2] - verts3d[a][2];
        const e2x = verts3d[d][0] - verts3d[a][0], e2y = verts3d[d][1] - verts3d[a][1], e2z = verts3d[d][2] - verts3d[a][2];
        let nx = e1y * e2z - e1z * e2y;
        let ny = e1z * e2x - e1x * e2z;
        let nz = e1x * e2y - e1y * e2x;
        // Ensure normal points OUTWARD (away from box center)
        const outDot = nx * (fcx - cx3) + ny * (fcy - cy3) + nz * (fcz - cz3);
        if (outDot < 0) { nx = -nx; ny = -ny; nz = -nz; }
        // Normalize
        const nLen = sqrt(nx * nx + ny * ny + nz * nz) || 1;
        nx /= nLen; ny /= nLen; nz /= nLen;
        // Dot with view direction — negative = faces camera (opposes view dir)
        const dot = nx * viewDirX + ny * viewDirY + nz * viewDirZ;
        if (dot >= 0) continue; // faces away from camera → skip
        frontFaces.add(fi);
        const z = (proj[a][2] + proj[b][2] + proj[c][2] + proj[d][2]) / 4;
        drawList.push({ z, type: "face", proj, verts: [a, b, c, d], color, tag });
      }
      return frontFaces;
    };

    const baseFrontFaces = addFaces(base, projBase, baseColor, "base");
    const lidFrontFaces = addFaces(lid, projLid, lidColor, "lid");

    // Keyboard keys (on base top face, y = -0.001 just above y=0)
    const kbInset = 0.18;
    const kbTL = project([-hw + kbInset, -0.001, kbInset]);
    const kbTR = project([hw - kbInset, -0.001, kbInset]);
    const kbBL = project([-hw + kbInset, -0.001, 2 * hd - kbInset * 3]);
    const kbBR = project([hw - kbInset, -0.001, 2 * hd - kbInset * 3]);
    // Screen-space winding check (same approach as lid screen content)
    const kbE1x = kbTR[0] - kbTL[0], kbE1y = kbTR[1] - kbTL[1];
    const kbE2x = kbBL[0] - kbTL[0], kbE2y = kbBL[1] - kbTL[1];
    const kbCross = kbE1x * kbE2y - kbE1y * kbE2x;
    // Smooth alpha from cross product magnitude (same as screen content)
    const kbMaxCross = size * size * 0.5;
    const kbFacingAmount = kbCross < 0 ? min(1, abs(kbCross) / kbMaxCross) : 0;
    // Fade with hinge angle (smooth open/close) and lid occlusion
    const hingeFade = min(1, max(0, (hingeAngle - 0.1) / 0.5));
    const lidOccludes = hingeAngle < PI && lidFrontFaces.has(1);
    const kbAlpha = floor(kbFacingAmount * hingeFade * (lidOccludes ? 0 : 1) * 255);
    const kbFacing = kbAlpha > 2;
    const kbKeys = [];
    if (kbFacing) {
      // 6-row ThinkPad keyboard layout (key counts per row)
      // Row 0: Fn keys (14 keys: Esc + F1-F12 + Del)
      // Row 1: Number row (14 keys: ` 1-0 - = Bksp)
      // Row 2: QWERTY (14 keys: Tab + Q-P + [ ] \)
      // Row 3: Home row (13 keys: Caps + A-L + ; ' Enter)
      // Row 4: Bottom alpha (12 keys: Shift + Z-M + , . / Shift)
      // Row 5: Modifier row (8 keys: Ctrl Fn Win Alt Space Alt PrtSc Ctrl)
      const rows = [14, 14, 14, 13, 12, 8];
      const rowIndent = [0, 0, 0.02, 0.04, 0.06, 0];
      // Keyboard occupies top 55% of base, trackpad extends to near edge
      const kbFrac = 0.55;
      const lerp = (a, b, t) => a + (b - a) * t;
      for (let r = 0; r < rows.length; r++) {
        const nKeys = rows[r];
        const t0 = (r + 0.1) / rows.length * kbFrac;
        const t1 = (r + 0.9) / rows.length * kbFrac;
        const indent = rowIndent[r];
        for (let k = 0; k < nKeys; k++) {
          // Row 5 has variable-width keys (space bar is wider)
          let u0, u1;
          if (r === 5) {
            // Space bar is key index 4, takes ~40% width
            const widths = [1, 1, 1, 1, 4.5, 1, 1, 1];
            const total = widths.reduce((s, w) => s + w, 0);
            let start = 0;
            for (let j = 0; j < k; j++) start += widths[j];
            u0 = (start + 0.1) / total;
            u1 = (start + widths[k] - 0.1) / total;
          } else {
            u0 = indent + (k + 0.1) / nKeys * (1 - indent * 2);
            u1 = indent + (k + 0.9) / nKeys * (1 - indent * 2);
          }
          kbKeys.push({
            pts: [
              [lerp(lerp(kbTL[0], kbTR[0], u0), lerp(kbBL[0], kbBR[0], u0), t0),
               lerp(lerp(kbTL[1], kbTR[1], u0), lerp(kbBL[1], kbBR[1], u0), t0)],
              [lerp(lerp(kbTL[0], kbTR[0], u1), lerp(kbBL[0], kbBR[0], u1), t0),
               lerp(lerp(kbTL[1], kbTR[1], u1), lerp(kbBL[1], kbBR[1], u1), t0)],
              [lerp(lerp(kbTL[0], kbTR[0], u1), lerp(kbBL[0], kbBR[0], u1), t1),
               lerp(lerp(kbTL[1], kbTR[1], u1), lerp(kbBL[1], kbBR[1], u1), t1)],
              [lerp(lerp(kbTL[0], kbTR[0], u0), lerp(kbBL[0], kbBR[0], u0), t1),
               lerp(lerp(kbTL[1], kbTR[1], u0), lerp(kbBL[1], kbBR[1], u0), t1)],
            ],
          });
        }
      }

      // Trackpad (centered, below keyboard, ~35% width of base)
      const tpU0 = 0.3, tpU1 = 0.7;
      const tpT0 = kbFrac + 0.03, tpT1 = 0.98;
      const trackpadColor = isDark ? [32, 32, 36] : [48, 48, 52];
      kbKeys.push({
        pts: [
          [lerp(lerp(kbTL[0], kbTR[0], tpU0), lerp(kbBL[0], kbBR[0], tpU0), tpT0),
           lerp(lerp(kbTL[1], kbTR[1], tpU0), lerp(kbBL[1], kbBR[1], tpU0), tpT0)],
          [lerp(lerp(kbTL[0], kbTR[0], tpU1), lerp(kbBL[0], kbBR[0], tpU1), tpT0),
           lerp(lerp(kbTL[1], kbTR[1], tpU1), lerp(kbBL[1], kbBR[1], tpU1), tpT0)],
          [lerp(lerp(kbTL[0], kbTR[0], tpU1), lerp(kbBL[0], kbBR[0], tpU1), tpT1),
           lerp(lerp(kbTL[1], kbTR[1], tpU1), lerp(kbBL[1], kbBR[1], tpU1), tpT1)],
          [lerp(lerp(kbTL[0], kbTR[0], tpU0), lerp(kbBL[0], kbBR[0], tpU0), tpT1),
           lerp(lerp(kbTL[1], kbTR[1], tpU0), lerp(kbBL[1], kbBR[1], tpU0), tpT1)],
        ],
        color: trackpadColor,
      });
    }

    // Wireframe edges — only for edges that touch at least one front-facing face
    // Map each edge to which faces it belongs to
    const edgeFaceMap = {}; // "a-b" → [faceIdx, ...]
    faceQuads.forEach(([a, b, c, d], fi) => {
      const edges = [[a,b],[b,c],[c,d],[d,a]];
      // For our quads: edges are [a,b],[b,c=next],[c,d],[d,a]
      // But faceQuads use 4 verts, edges are between consecutive + wrap
      [[a,b],[b,c],[c,d],[d,a]].forEach(([ea, eb]) => {
        const key = min(ea,eb) + "-" + max(ea,eb);
        (edgeFaceMap[key] = edgeFaceMap[key] || []).push(fi);
      });
    });

    const addEdges = (proj, frontFaces) => {
      halfEdges.forEach(([a, b]) => {
        const key = min(a,b) + "-" + max(a,b);
        const faces = edgeFaceMap[key] || [];
        // Only draw if at least one adjacent face is front-facing
        if (!faces.some(fi => frontFaces.has(fi))) return;
        const z = (proj[a][2] + proj[b][2]) / 2;
        drawList.push({ z, type: "edge", proj, a, b });
      });
    };
    addEdges(projBase, baseFrontFaces);
    addEdges(projLid, lidFrontFaces);

    // Sort back-to-front (highest z = farthest = draw first)
    drawList.sort((a, b) => b.z - a.z);

    // Draw everything in sorted order
    for (const item of drawList) {
      if (item.type === "face") {
        const { proj, verts: [a, b, c, d], color } = item;
        const shade = max(0.6, min(1.2, 1.0 - item.z * 0.12));
        ink(floor(color[0] * shade), floor(color[1] * shade), floor(color[2] * shade));
        tri(proj[a][0], proj[a][1], proj[b][0], proj[b][1], proj[c][0], proj[c][1]);
        tri(proj[a][0], proj[a][1], proj[c][0], proj[c][1], proj[d][0], proj[d][1]);
      } else if (item.type === "edge") {
        const { proj, a, b } = item;
        const blink = sin(frame * 0.08 + a * 1.7 + b * 2.3) * 0.5 + 0.5;
        const alpha = floor(30 + blink * 50);
        ink(255, 255, 255, alpha).line(proj[a][0], proj[a][1], proj[b][0], proj[b][1]);
      }
    }

    // ⌨️ Keyboard keys — smooth fade based on facing + hinge + occlusion
    if (kbFacing) {
      for (const key of kbKeys) {
        const [[x0, y0], [x1, y1], [x2, y2], [x3, y3]] = key.pts;
        const c = key.color || keyColor;
        ink(c[0], c[1], c[2], kbAlpha);
        tri(x0, y0, x1, y1, x2, y2);
        tri(x0, y0, x2, y2, x3, y3);
        // White blinky wireframe outline
        const blink = sin(frame * 0.08 + x0 * 0.3 + y0 * 0.7) * 0.5 + 0.5;
        const wireAlpha = floor((30 + blink * 50) * kbAlpha / 255);
        ink(255, 255, 255, wireAlpha);
        line(x0, y0, x1, y1);
        line(x1, y1, x2, y2);
        line(x2, y2, x3, y3);
        line(x3, y3, x0, y0);
      }
    }

    // 🖥️ Screen on the lid (with bezel, solid fill, and text)
    const inset = 0.15;
    const bezelInset = 0.08;
    const hingeInset = 0.35; // larger inset at hinge end (away from base)
    // Screen is on the INNER face of the lid (y=0 in lid local).
    const screenY = 0.002;
    const screenTL = [-hw + inset, screenY, 2 * hd - inset];
    const screenTR = [hw - inset, screenY, 2 * hd - inset];
    const screenBL = [-hw + inset, screenY, hingeInset];
    const screenBR = [hw - inset, screenY, hingeInset];

    // Bezel corners (slightly larger than screen)
    const bezelY = 0.001;
    const bezelTL = [-hw + bezelInset, bezelY, 2 * hd - bezelInset];
    const bezelTR = [hw - bezelInset, bezelY, 2 * hd - bezelInset];
    const bezelBL = [-hw + bezelInset, bezelY, hingeInset - 0.07];
    const bezelBR = [hw - bezelInset, bezelY, hingeInset - 0.07];

    // Same transform as lid vertices (barrel hinge)
    const hingeXform = ([lx, ly, lz]) => {
      const ry = ly * cosH - lz * sinH;
      const rz = ly * sinH + lz * cosH;
      return [lx, attachY + ry, attachZ + rz];
    };
    const sTL = hingeXform(screenTL), sTR = hingeXform(screenTR);
    const sBL = hingeXform(screenBL), sBR = hingeXform(screenBR);
    const bTL = hingeXform(bezelTL), bTR = hingeXform(bezelTR);
    const bBL = hingeXform(bezelBL), bBR = hingeXform(bezelBR);

    // Negate right vector — inner face mirrors x when viewed from front
    const planeRight = [sTL[0] - sTR[0], sTL[1] - sTR[1], sTL[2] - sTR[2]];
    const planeDown = [sBL[0] - sTL[0], sBL[1] - sTL[1], sBL[2] - sTL[2]];

    // Screen-space normal check (is it facing the camera?)
    const projTL = project(sTL);
    const projTR = project(sTR);
    const projBL = project(sBL);
    const projBR = project(sBR);
    const ex1 = projTR[0] - projTL[0], ey1 = projTR[1] - projTL[1];
    const ex2 = projBL[0] - projTL[0], ey2 = projBL[1] - projTL[1];
    const cross = ex1 * ey2 - ey1 * ex2;

    if (cross < 0) {
      const maxCross = size * size * 0.5;
      const facing = min(1, abs(cross) / maxCross);
      const screenAlpha = floor(facing * 220);

      // Bezel (dark border around screen) — two filled triangles
      const pBTL = project(bTL), pBTR = project(bTR);
      const pBBL = project(bBL), pBBR = project(bBR);
      const bezelColor = [20, 20, 22];
      ink(bezelColor[0], bezelColor[1], bezelColor[2], screenAlpha);
      tri(pBTL[0], pBTL[1], pBTR[0], pBTR[1], pBBR[0], pBBR[1]);
      tri(pBTL[0], pBTL[1], pBBR[0], pBBR[1], pBBL[0], pBBL[1]);

      // Screen fill (dark background) — two filled triangles
      const screenColor = [6, 6, 10];
      ink(screenColor[0], screenColor[1], screenColor[2], screenAlpha);
      tri(projTL[0], projTL[1], projTR[0], projTR[1], projBR[0], projBR[1]);
      tri(projTL[0], projTL[1], projBR[0], projBR[1], projBL[0], projBL[1]);

      // Subtle scanline / noise animation on screen
      {
        const st = frame * 0.02;
        const scanA = floor(facing * 18);
        // Horizontal scanline that scrolls down
        const scanFrac = (st * 0.5) % 1;
        const scanY0 = projTL[1] + (projBL[1] - projTL[1]) * scanFrac;
        const scanY1 = scanY0 + (projBL[1] - projTL[1]) * 0.06;
        const sxL = projTL[0] + (projBL[0] - projTL[0]) * scanFrac;
        const sxR = projTR[0] + (projBR[0] - projTR[0]) * scanFrac;
        const sxL1 = projTL[0] + (projBL[0] - projTL[0]) * min(1, scanFrac + 0.06);
        const sxR1 = projTR[0] + (projBR[0] - projTR[0]) * min(1, scanFrac + 0.06);
        ink(40, 60, 80, scanA);
        tri(sxL, scanY0, sxR, scanY0, sxR1, scanY1);
        tri(sxL, scanY0, sxR1, scanY1, sxL1, scanY1);
      }

      // Screen text
      const textAlpha = floor(facing * 255);
      const planeW = sqrt(planeRight[0] ** 2 + planeRight[1] ** 2 + planeRight[2] ** 2);
      const planeH = sqrt(planeDown[0] ** 2 + planeDown[1] ** 2 + planeDown[2] ** 2);

      const glyphScale = planeW / (4 * 14);
      const rn = [planeRight[0] / planeW * glyphScale,
                  planeRight[1] / planeW * glyphScale,
                  planeRight[2] / planeW * glyphScale];
      const dn = [planeDown[0] / planeH * glyphScale,
                  planeDown[1] / planeH * glyphScale,
                  planeDown[2] / planeH * glyphScale];

      // Dynamic text: "hi @handle" if logged in, cycle real handles otherwise
      let displayHandle = userHandle;
      let handleClean = null;
      if (!displayHandle && allHandles.length > 0) {
        cycleTimer += 1;
        if (cycleTimer >= CYCLE_INTERVAL) {
          cycleTimer = 0;
          cycleIndex = (cycleIndex + 1) % allHandles.length;
        }
        handleClean = allHandles[cycleIndex];
        displayHandle = `@${handleClean}`;
      } else if (displayHandle) {
        handleClean = displayHandle.startsWith("@")
          ? displayHandle.slice(1)
          : displayHandle;
      }
      // Look up colors every frame (async fetch may have completed)
      currentDisplayColors = handleClean
        ? handleColors.get(handleClean) || null
        : null;
      const screenText = displayHandle ? `hi ${displayHandle}` : "hi";
      // "hi " prefix length for color offset — "@" is at index 3
      const handleStart = 3;
      // Estimate width: MatrixChunky8 avg ~4px per char
      const textW = screenText.length * 4;
      const textH = 8;
      const offsetR = (planeW / glyphScale - textW) / 2;
      const offsetD = (planeH / glyphScale - textH) / 2;

      // Origin from sTR (visual top-left when viewed from front)
      const textOrigin = [
        sTR[0] + offsetR * rn[0] + offsetD * dn[0],
        sTR[1] + offsetR * rn[1] + offsetD * dn[1],
        sTR[2] + offsetR * rn[2] + offsetD * dn[2],
      ];

      // Screen text with handle colors when available
      ink(0).write3D(screenText, {
        origin: textOrigin,
        right: rn,
        down: dn,
        project,
        typeface: "MatrixChunky8",
        pixelCallback: (sx, sy, gx, gy, ci) => {
          const seed = (gx * 7 + gy * 13 + ci * 31 + frame * 3) & 0xFF;
          const flicker = sin(frame * 0.15 + seed * 0.1) * 0.5 + 0.5;
          // More varied opacity — some pixels dim, some bright, some blink off
          const opVar = sin(seed * 0.37 + frame * 0.08) * 0.5 + 0.5;
          const dropout = ((seed * 17 + frame) % 61) < 4 ? 0.15 : 1;
          const a = floor(textAlpha * (0.3 + opVar * 0.5 + flicker * 0.2) * dropout);

          // Use handle colors for the @handle portion
          const charIdx = ci - handleStart;
          if (currentDisplayColors && charIdx >= 0 && charIdx < currentDisplayColors.length) {
            const c = currentDisplayColors[charIdx];
            const shimmer = 0.6 + flicker * 0.4;
            ink(
              floor(c.r * shimmer),
              floor(c.g * shimmer),
              floor(c.b * shimmer),
              a,
            );
          } else {
            // Default blue/cyan for "hi " prefix or no colors
            const flash = (seed + frame) % 47 === 0;
            const r = flash ? 220 : floor(20 + flicker * 40);
            const g = flash ? 240 : floor(80 + flicker * 100 + seed * 0.2);
            const b = flash ? 255 : floor(180 + flicker * 75);
            ink(r, g, b, a);
          }
        },
      });
    }
  }

  // Buy button
  const $btn = { ink };
  if (buyBtn) {
    buyBtn.reposition({ center: "x", bottom: 20, screen }, getBuyText());

    let scheme, hover;
    if (buyPending) {
      const pulse = sin(performance.now() / 150) * 0.5 + 0.5;
      scheme = isDark
        ? [[floor(30 + pulse * 30), floor(40 + pulse * 20), 30],
           [floor(150 + pulse * 105), floor(200 + pulse * 55), 100],
           [floor(200 + pulse * 55), floor(220 + pulse * 35), 180]]
        : [[floor(200 + pulse * 30), floor(220 + pulse * 20), 200],
           [floor(60 + pulse * 40), floor(120 + pulse * 40), 60],
           [floor(30 + pulse * 20), floor(80 + pulse * 30), 30]];
      hover = scheme;
    } else {
      const blink = sin(performance.now() / 500) * 0.3 + 0.7;
      scheme = isDark
        ? [[25, 35, 25],
           [floor(80 + blink * 70), floor(160 + blink * 95), floor(80 + blink * 70)],
           [180, 230, 180]]
        : [[220, 235, 220],
           [floor(40 + blink * 40), floor(100 + blink * 55), floor(40 + blink * 40)],
           [30, 80, 30]];
      hover = isDark
        ? [[40, 55, 40], [150, 255, 150], [200, 255, 200]]
        : [[210, 230, 210], [40, 180, 40], [20, 60, 20]];
    }
    buyBtn.paint($btn, scheme, hover);
  }

  // Manual PDF link (bottom left)
  const linkScheme = isDark
    ? [[20, 20, 24], fgDim, [180, 180, 190]]
    : [[228, 228, 232], fgDim, [60, 60, 70]];
  const linkHover = isDark
    ? [[30, 30, 38], [180, 180, 200], [220, 220, 230]]
    : [[215, 215, 225], [60, 60, 80], [30, 30, 40]];
  if (manualBtn) {
    manualBtn.reposition({ x: 6, bottom: 20, screen }, "MANUAL");
    manualBtn.paint($btn, linkScheme, linkHover);
  }
  if (paperBtn) {
    const paperX = manualBtn ? 6 + manualBtn.width + 4 : 6;
    paperBtn.reposition({ x: paperX, bottom: 20, screen }, "PAPER");
    paperBtn.paint($btn, linkScheme, linkHover);
  }
}

function act({ event: e, screen, jump, sound, ui, api }) {
  if (thanks) return;

  if (e.is("reframed")) {
    setupButtons(ui, screen);
  }

  manualBtn?.btn?.act(e, {
    push: () => jump(`out:${MANUAL_URL}`),
  });

  paperBtn?.btn?.act(e, {
    push: () => jump(`out:${PAPER_URL}`),
  });

  buyBtn?.btn?.act(e, {
    down: () => {
      sound?.synth({ type: "sine", tone: 440, duration: 0.05, volume: 0.3 });
    },
    push: () => {
      if (buyPending) return;

      if (checkoutReady && checkoutUrl) {
        sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
        jump(checkoutUrl);
      } else if (checkoutError) {
        checkoutError = null;
        fetchCheckout(api);
        sound?.synth({ type: "sine", tone: 550, duration: 0.06, volume: 0.3 });
        buyPending = true;
        waitForCheckout(jump, sound, api);
      } else {
        buyPending = true;
        if (!checkoutLoading) fetchCheckout(api);
        sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.3 });
        waitForCheckout(jump, sound, api);
      }
    },
  });
}

async function waitForCheckout(jump, sound, api) {
  const maxWait = 10000;
  const startTime = Date.now();

  if (!checkoutReady && !checkoutError && !checkoutLoading) {
    fetchCheckout(api);
  }

  while (!checkoutReady && !checkoutError && Date.now() - startTime < maxWait) {
    await new Promise((r) => setTimeout(r, 100));
  }

  buyPending = false;

  if (checkoutReady && checkoutUrl) {
    sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
    jump(checkoutUrl);
  } else if (checkoutError) {
    sound?.synth({ type: "square", tone: 200, duration: 0.15, volume: 0.3 });
  }
}

function meta() {
  return {
    title: "AC Blank",
    desc: "AC Native Laptop — a surplus laptop running AC Native OS.",
  };
}

export { boot, paint, act, meta };
