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

async function boot({ params, ui, screen, cursor, hud, api }) {
  cursor("native");
  hud.labelBack();

  if (params[0] === "thanks") {
    thanks = true;
    return;
  }

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
  const { wipe, ink, screen, dark: isDark } = $;
  frame += 1;
  const w = screen.width;
  const h = screen.height;

  // Theme colors
  const bg = isDark ? [12, 12, 14] : [245, 243, 240];
  const fg = isDark ? 255 : 20;
  const fgDim = isDark ? 120 : 100;
  const wireAlpha = isDark ? 140 : 120;

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
    const laptopTop = 20;
    const cy = floor((laptopTop + contentBottom) / 2);
    // Keep laptop fully within viewport (account for rotation + lid overshoot)
    const availW = w * 0.32;
    const availH = (contentBottom - laptopTop) * 0.25;
    const size = min(availW, availH);
    const fov = 260;

    // Turntable rotation (slow steady swivel)
    const ay = frame * 0.008;
    const ax = 0.3; // fixed downward tilt

    // Fixed open angle (~120 degrees)
    const hingeAngle = PI * 0.67;

    // Half-box dimensions (proportional to ThinkPad Yoga 11e: ~290mm × 202mm × 22mm)
    // Width:Depth ratio ≈ 1.44:1, thickness is thin
    const hw = 1.44, hh = 0.07, hd = 1.0;

    // Base (keyboard half)
    const base = [
      [-hw, -hh, -hd], [hw, -hh, -hd], [hw, hh, -hd], [-hw, hh, -hd],
      [-hw, -hh, hd], [hw, -hh, hd], [hw, hh, hd], [-hw, hh, hd],
    ];

    // Lid — hinged at back edge
    const lidLocal = [
      [-hw, -hh, 0], [hw, -hh, 0], [hw, hh, 0], [-hw, hh, 0],
      [-hw, -hh, 2 * hd], [hw, -hh, 2 * hd], [hw, hh, 2 * hd], [-hw, hh, 2 * hd],
    ];
    const cosH = cos(hingeAngle), sinH = sin(hingeAngle);
    const lid = lidLocal.map(([lx, ly, lz]) => {
      const ry = ly * cosH - lz * sinH;
      const rz = ly * sinH + lz * cosH;
      return [lx, ry + hh, rz - hd];
    });

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

    const waveOffset = frame * 0.05;
    const drawEdges = (proj, edgeOffset) => {
      halfEdges.forEach(([a, b], i) => {
        const depth = (proj[a][2] + proj[b][2]) / 2;
        const brightness = 0.55 + depth * 0.15;
        const hue = (((i + edgeOffset) * 0.37 + sin(frame * 0.02 + i + edgeOffset) * 0.3) + waveOffset) % 1;
        const sector = abs(hue) * 6;
        const f = sector - floor(sector);
        let r, g, bl;
        const s = floor(sector) % 6;
        if (s === 0) { r = 1; g = f; bl = 0; }
        else if (s === 1) { r = 1 - f; g = 1; bl = 0; }
        else if (s === 2) { r = 0; g = 1; bl = f; }
        else if (s === 3) { r = 0; g = 1 - f; bl = 1; }
        else if (s === 4) { r = f; g = 0; bl = 1; }
        else { r = 1; g = 0; bl = 1 - f; }
        ink(
          floor(r * 255 * brightness),
          floor(g * 255 * brightness),
          floor(bl * 255 * brightness),
          wireAlpha,
        ).line(proj[a][0], proj[a][1], proj[b][0], proj[b][1]);
      });
    };

    drawEdges(projBase, 0);
    drawEdges(projLid, 12);

    // Vertex particles
    const particleColors = [
      [255, 80, 200], [80, 255, 220], [255, 255, 80],
      [80, 200, 255], [255, 120, 80], [180, 80, 255],
    ];
    [...projBase, ...projLid].forEach(([px, py], i) => {
      const pColor = particleColors[(i + floor(frame * 0.04)) % particleColors.length];
      const flicker = 0.6 + sin(frame * 0.1 + i * 1.3) * 0.4;
      ink(...pColor, floor(flicker * 150)).box(px - 1, py - 1, 2, 2);
    });

    // 🖥️ "AC Blank" projected in 3D on the lid screen
    const inset = 0.15;
    const screenTL = [-hw + inset, -hh - 0.002, inset];
    const screenTR = [hw - inset, -hh - 0.002, inset];
    const screenBL = [-hw + inset, -hh - 0.002, 2 * hd - inset];

    const hingeXform = ([lx, ly, lz]) => {
      const ry = ly * cosH - lz * sinH;
      const rz = ly * sinH + lz * cosH;
      return [lx, ry + hh, rz - hd];
    };
    const sTL = hingeXform(screenTL);
    const sTR = hingeXform(screenTR);
    const sBL = hingeXform(screenBL);

    const planeRight = [sTR[0] - sTL[0], sTR[1] - sTL[1], sTR[2] - sTL[2]];
    const planeDown = [sBL[0] - sTL[0], sBL[1] - sTL[1], sBL[2] - sTL[2]];

    // Screen-space normal check (is it facing the camera?)
    const projTL = project(sTL);
    const projTR = project(sTR);
    const projBL = project(sBL);
    const ex1 = projTR[0] - projTL[0], ey1 = projTR[1] - projTL[1];
    const ex2 = projBL[0] - projTL[0], ey2 = projBL[1] - projTL[1];
    const cross = ex1 * ey2 - ey1 * ex2;

    if (cross > 0) {
      const maxCross = size * size * 0.5;
      const facing = min(1, abs(cross) / maxCross);
      const textAlpha = floor(facing * 255);

      const planeW = sqrt(planeRight[0] ** 2 + planeRight[1] ** 2 + planeRight[2] ** 2);
      const planeH = sqrt(planeDown[0] ** 2 + planeDown[1] ** 2 + planeDown[2] ** 2);

      // MatrixChunky8: "AC Blank" = 30px wide, 8px tall
      const glyphScale = planeW / (4 * 14); // fit ~14 MatrixChunky8 avg-width chars
      const rn = [planeRight[0] / planeW * glyphScale,
                  planeRight[1] / planeW * glyphScale,
                  planeRight[2] / planeW * glyphScale];
      const dn = [planeDown[0] / planeH * glyphScale,
                  planeDown[1] / planeH * glyphScale,
                  planeDown[2] / planeH * glyphScale];

      const textW = 30; // "AC Blank" in MatrixChunky8
      const textH = 8;
      const offsetR = (planeW / glyphScale - textW) / 2;
      const offsetD = (planeH / glyphScale - textH) / 2;

      const textOrigin = [
        sTL[0] + offsetR * rn[0] + offsetD * dn[0],
        sTL[1] + offsetR * rn[1] + offsetD * dn[1],
        sTL[2] + offsetR * rn[2] + offsetD * dn[2],
      ];

      const titleColor = isDark ? [255, 255, 255] : [20, 20, 20];
      ink(titleColor[0], titleColor[1], titleColor[2], textAlpha)
        .write3D("AC Blank", {
          origin: textOrigin,
          right: rn,
          down: dn,
          project,
          typeface: "MatrixChunky8",
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
