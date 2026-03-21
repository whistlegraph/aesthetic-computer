// blank, 26.03.20
// AC Blank — AC Native Laptop product page & checkout

const { floor, sin, cos, abs, min, max, PI } = Math;

// Pricing (cents)
const pricing = {
  usd: { min: 9600, suggested: 12800, max: 51200, symbol: "$" },
  dkk: { min: 67200, suggested: 89600, max: 358400, symbol: "kr" },
};

const tiers = [
  { amount: 9600, dkk: 67200, label: "AC Blank", desc: "laptop + AC OS" },
  { amount: 12800, dkk: 89600, label: "AC Blank", desc: "+ support AC" },
  { amount: 51200, dkk: 358400, label: "AC Blank", desc: "+ tutorial in LA" },
];

// Module state
let amount = 12800;
let currency = "usd";
let checkoutUrl = null;
let checkoutReady = false;
let checkoutError = null;
let buyPending = false;
let thanks = false;

// UI elements (TextButtons)
let buyBtn = null;
let tierBtns = [];
let currencyBtn = null;

// Animation
let frame = 0;

const charH = 16;

function displayAmount(amt, cur) {
  if (cur === "dkk") return `${(amt / 100).toFixed(0)} kr`;
  return `$${(amt / 100).toFixed(0)}`;
}

function tierLabel(amt) {
  if (amt >= 51200) return "tutorial";
  if (amt > 9600) return "support";
  return "blank";
}

function getBuyText() {
  if (buyPending) return "CHECKING OUT...";
  if (checkoutError) return "RETRY";
  return `BUY ${displayAmount(amount, currency)}`;
}

async function boot({ params, ui, screen, cursor, hud, api }) {
  cursor("native");
  hud.labelBack();

  // Check for thanks state
  if (params[0] === "thanks") {
    thanks = true;
    return;
  }

  setupButtons(ui, screen);
  fetchCheckout(api);
}

function tierText(i) {
  const t = tiers[i];
  const tierAmt = currency === "dkk" ? t.dkk : t.amount;
  return `${displayAmount(tierAmt, currency)}  ${t.desc}`;
}

function setupButtons(ui, screen) {
  const h = screen.height;

  // Buy button (bottom center)
  buyBtn = new ui.TextButton(getBuyText(), { center: "x", bottom: 12, screen });

  // Tier buttons (stacked, above buy button)
  const tierStartY = buyBtn.btn.box.y - (buyBtn.height + 6) * 3 - 8;
  tierBtns = tiers.map((t, i) => {
    return new ui.TextButton(tierText(i), {
      center: "x",
      y: tierStartY + i * (buyBtn.height + 6),
      screen,
    });
  });

  // Currency toggle (top right)
  currencyBtn = new ui.TextButton(currency.toUpperCase(), {
    top: 8,
    right: 8,
    screen,
  });
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
      body: JSON.stringify({ amount, currency }),
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

function paint({ wipe, ink, write, box, line, screen }) {
  frame += 1;
  const w = screen.width;
  const h = screen.height;

  // Dark background
  wipe(12, 12, 14);

  // Thanks page
  if (thanks) {
    const cy = floor(h / 2);
    ink(255).write("your blank is coming.", {
      center: "x",
      y: cy - 30,
      screen,
    });
    ink(140).write("we'll be in touch.", { center: "x", y: cy, screen });
    return;
  }

  // Title
  const titleY = floor(h * 0.08);
  ink(255).write("AC Blank", {
    center: "x",
    y: titleY,
    screen,
  });

  // Subtitle
  ink(120).write("AC Native Laptop", {
    center: "x",
    y: titleY + charH + 4,
    screen,
  });

  // Description block
  const descY = titleY + charH * 2 + 16;
  const descLines = [
    "A surplus laptop running AC Native OS.",
    "Stable commands. Nothing extra.",
    "Like a blank tape waiting to be filled.",
  ];
  descLines.forEach((ln, i) => {
    ink(90).write(ln, { center: "x", y: descY + i * (charH + 2), screen });
  });

  // Specs
  const specY = descY + descLines.length * (charH + 2) + 12;
  ink(70).write("Lenovo ThinkPad Yoga 11e", {
    center: "x",
    y: specY,
    screen,
  });
  ink(50).write("11.6\" touchscreen · flip design", {
    center: "x",
    y: specY + charH + 2,
    screen,
  });

  // 💻 Wireframe laptop (two hinged halves)
  {
    const cx = floor(w / 2);
    const laptopTop = specY + charH * 2 + 16;
    const laptopBottom = tierBtns.length > 0 ? tierBtns[0].btn.box.y - 12 : h * 0.55;
    const cy = floor((laptopTop + laptopBottom) / 2);
    const size = min(w, laptopBottom - laptopTop) * 0.28;
    const fov = 260;
    const ay = frame * 0.008;
    const ax = frame * 0.005;

    // Hinge angle: smoothly open and close
    const hingeAngle = (sin(frame * 0.012) * 0.5 + 0.5) * PI * 0.85 + PI * 0.1;

    // Half-box dimensions: wide, thin, moderate depth
    const hw = 1.4, hh = 0.08, hd = 0.9;

    // Bottom half (base)
    const base = [
      [-hw, -hh, -hd], [ hw, -hh, -hd], [ hw,  hh, -hd], [-hw,  hh, -hd],
      [-hw, -hh,  hd], [ hw, -hh,  hd], [ hw,  hh,  hd], [-hw,  hh,  hd],
    ];

    // Top half (lid) — hinged at back edge
    const lidLocal = [
      [-hw, -hh, 0], [ hw, -hh, 0], [ hw, hh, 0], [-hw, hh, 0],
      [-hw, -hh, 2 * hd], [ hw, -hh, 2 * hd], [ hw, hh, 2 * hd], [-hw, hh, 2 * hd],
    ];
    const cosH = cos(hingeAngle), sinH = sin(hingeAngle);
    const lid = lidLocal.map(([lx, ly, lz]) => {
      const ry = ly * cosH - lz * sinH;
      const rz = ly * sinH + lz * cosH;
      return [lx, ry + hh, rz - hd];
    });

    const halfEdges = [
      [0,1],[1,2],[2,3],[3,0],
      [4,5],[5,6],[6,7],[7,4],
      [0,4],[1,5],[2,6],[3,7],
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
          140,
        ).line(
          proj[a][0], proj[a][1],
          proj[b][0], proj[b][1],
        );
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
  }

  // Tier buttons
  const $ = { ink };
  tierBtns.forEach((btn, i) => {
    const t = tiers[i];
    const tierAmt = currency === "dkk" ? t.dkk : t.amount;
    const isSelected = amount === tierAmt;

    const selectedScheme = [[40, 50, 40], [120, 200, 120], [220, 255, 220]];
    const defaultScheme = [[22, 22, 25], [60, 60, 65], [140, 140, 145]];
    const hoverScheme = [[35, 35, 40], [150, 150, 180], [200, 200, 220]];

    btn.paint($, isSelected ? selectedScheme : defaultScheme, hoverScheme);
  });

  // Buy button
  if (buyBtn) {
    buyBtn.reposition({ center: "x", bottom: 12, screen }, getBuyText());

    let scheme, hover;
    if (buyPending) {
      const pulse = sin(performance.now() / 150) * 0.5 + 0.5;
      scheme = [
        [floor(30 + pulse * 30), floor(40 + pulse * 20), 30],
        [floor(150 + pulse * 105), floor(200 + pulse * 55), 100],
        [floor(200 + pulse * 55), floor(220 + pulse * 35), 180],
      ];
      hover = scheme;
    } else if (checkoutError) {
      scheme = [[50, 25, 25], [200, 80, 80], [255, 120, 120]];
      hover = scheme;
    } else {
      const blink = sin(performance.now() / 500) * 0.3 + 0.7;
      scheme = [
        [25, 35, 25],
        [floor(80 + blink * 70), floor(160 + blink * 95), floor(80 + blink * 70)],
        [180, 230, 180],
      ];
      hover = [[40, 55, 40], [150, 255, 150], [200, 255, 200]];
    }
    buyBtn.paint($, scheme, hover);
  }

  // Currency toggle (top right)
  if (currencyBtn) {
    currencyBtn.paint($, [[12, 12, 14], [50, 50, 55], [80, 80, 85]], [[12, 12, 14], [120, 120, 130], [200, 200, 210]]);
  }

  // Subtle bottom line
  ink(30).line(0, h - 1, w, h - 1);
}

function act({ event: e, screen, jump, sound, ui, api }) {
  if (thanks) return;

  if (e.is("reframed")) {
    setupButtons(ui, screen);
  }

  // Tier selection
  tierBtns.forEach((btn, i) => {
    btn.act(e, {
      down: () => {
        sound?.synth({ type: "sine", tone: 500 + i * 100, duration: 0.03, volume: 0.2 });
      },
      push: () => {
        const t = tiers[i];
        const newAmount = currency === "dkk" ? t.dkk : t.amount;
        if (newAmount !== amount) {
          amount = newAmount;
          sound?.synth({ type: "sine", tone: 600 + i * 150, duration: 0.06, volume: 0.3 });
          // Update tier labels to reflect selection
          tierBtns.forEach((tb, j) => tb.replaceLabel(tierText(j)));
          fetchCheckout(api);
        }
      },
    });
  });

  // Currency toggle
  currencyBtn?.act(e, {
    push: () => {
      currency = currency === "usd" ? "dkk" : "usd";
      const tierIdx = tiers.findIndex(
        (t) => (currency === "usd" ? t.dkk : t.amount) === amount,
      );
      if (tierIdx >= 0) {
        amount =
          currency === "dkk" ? tiers[tierIdx].dkk : tiers[tierIdx].amount;
      } else {
        amount = pricing[currency].suggested;
      }
      sound?.synth({ type: "sine", tone: 700, duration: 0.05, volume: 0.2 });
      currencyBtn.reposition({ top: 8, right: 8, screen }, currency.toUpperCase());
      tierBtns.forEach((tb, j) => tb.replaceLabel(tierText(j)));
      fetchCheckout(api);
    },
  });

  // Buy button
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
    desc: "A surplus laptop running AC Native OS. Stable commands. Nothing extra. Like a blank tape waiting to be filled.",
  };
}

export { boot, paint, act, meta };
