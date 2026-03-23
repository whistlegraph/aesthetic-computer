// blank, 26.03.20
// AC Blank — AC Native Laptop product page & checkout

const { floor, sin, cos, abs, min, max, PI, sqrt } = Math;

// Module state
let amount = 12800;
let checkoutUrl = null;
let checkoutReady = false;
let checkoutError = null;
let buyPending = false;
let thanks = false;

// UI elements
let buyBtn = null;
let userHandle = null;

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
}

function setupButtons(ui, screen) {
  buyBtn = new ui.TextButton(getBuyText(), { center: "x", bottom: 20, screen });
}

async function fetchCheckout(api) {
  checkoutReady = false;
  checkoutError = null;
  checkoutUrl = null;

  try {
    const headers = { "Content-Type": "application/json" };
    const token = await api?.authorize?.();
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
  }
}

function paint($) {
  const { wipe, ink, screen, dark: isDark, tri } = $;
  frame += 1;
  const w = screen.width;
  const h = screen.height;

  // Theme colors
  const bg = isDark ? [12, 12, 14] : [245, 243, 240];
  const fg = isDark ? 255 : 20;
  const fgDim = isDark ? 120 : 100;


  wipe(...bg);

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
    const fov = 260;

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

    // Base + lid faces
    let faceIdx = 0;
    const addFaces = (proj, color, tag) => {
      for (const [a, b, c, d] of faceQuads) {
        const fi = faceIdx++;
        const e1x = proj[b][0] - proj[a][0], e1y = proj[b][1] - proj[a][1];
        const e2x = proj[d][0] - proj[a][0], e2y = proj[d][1] - proj[a][1];
        const cross = e1x * e2y - e1y * e2x;
        if (cross < 0) {
          const z = (proj[a][2] + proj[b][2] + proj[c][2] + proj[d][2]) / 4;
          drawList.push({ z, type: "face", proj, verts: [a, b, c, d], color, tag, fi });
        } else if (cross > 0) {
          const z = (proj[a][2] + proj[b][2] + proj[c][2] + proj[d][2]) / 4;
          drawList.push({ z, type: "face", proj, verts: [d, c, b, a], color, tag, fi });
        }
      }
    };
    addFaces(projBase, baseColor, "base");
    addFaces(projLid, lidColor, "lid");

    // Hinge barrels
    const hingeColor = isDark ? [32, 32, 35] : [45, 45, 48];
    for (const hv of hingeVerts) {
      const projH = hv.map(project);
      addFaces(projH, hingeColor, "hinge", false);
    }

    // Keyboard keys (on base top face, y = -0.001 just above y=0)
    const kbInset = 0.18;
    const kbTL = project([-hw + kbInset, -0.001, kbInset]);
    const kbTR = project([hw - kbInset, -0.001, kbInset]);
    const kbBL = project([-hw + kbInset, -0.001, 2 * hd - kbInset * 3]);
    const kbBR = project([hw - kbInset, -0.001, 2 * hd - kbInset * 3]);
    const ke1x = kbTR[0] - kbTL[0], ke1y = kbTR[1] - kbTL[1];
    const ke2x = kbBL[0] - kbTL[0], ke2y = kbBL[1] - kbTL[1];
    // Store keyboard data to draw after sorted loop (always on top of base)
    let kbVisible = ke1x * ke2y - ke1y * ke2x < 0;
    const kbKeys = [];
    if (kbVisible) {
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
      }
    }

    // ⌨️ Keyboard keys — drawn after z-sorted loop, always on top of base
    if (kbVisible) {
      for (const key of kbKeys) {
        const [[x0, y0], [x1, y1], [x2, y2], [x3, y3]] = key.pts;
        const c = key.color || keyColor;
        ink(c[0], c[1], c[2]);
        tri(x0, y0, x1, y1, x2, y2);
        tri(x0, y0, x2, y2, x3, y3);
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

      // Dynamic text: "hi @handle" if logged in, "AC Blank" otherwise
      const screenText = userHandle ? `hi ${userHandle}` : "hi";
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

      // Glitchy digital blue — vary per-pixel via pixelCallback
      ink(0).write3D(screenText, {
        origin: textOrigin,
        right: rn,
        down: dn,
        project,
        typeface: "MatrixChunky8",
        pixelCallback: (sx, sy, gx, gy, ci) => {
          // Hash-ish seed from position + frame for shimmer
          const seed = (gx * 7 + gy * 13 + ci * 31 + frame * 3) & 0xFF;
          const flicker = sin(frame * 0.15 + seed * 0.1) * 0.5 + 0.5;
          // Blue/cyan palette with occasional white flash
          const flash = (seed + frame) % 47 === 0;
          const r = flash ? 220 : floor(20 + flicker * 40);
          const g = flash ? 240 : floor(80 + flicker * 100 + seed * 0.2);
          const b = flash ? 255 : floor(180 + flicker * 75);
          const a = floor(textAlpha * (0.6 + flicker * 0.4));
          ink(r, g, b, a);
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
}

function act({ event: e, screen, jump, sound, ui, api }) {
  if (thanks) return;

  if (e.is("reframed")) {
    setupButtons(ui, screen);
  }

  buyBtn?.act(e, {
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
      } else {
        buyPending = true;
        sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.3 });
        waitForCheckout(jump, sound);
      }
    },
  });
}

async function waitForCheckout(jump, sound) {
  const maxWait = 10000;
  const startTime = Date.now();

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
