// blank, 26.03.20
// AC Blank — AC Native Laptop product page & checkout

const { floor, sin, min, max } = Math;

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

// UI elements
let buyBtn = null;
let tierBtns = [];
let currencyBtn = null;

// Animation
let frame = 0;

const pad = 6;
const charW = 8;
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

function setupButtons(ui, screen) {
  const w = screen.width;
  const h = screen.height;

  // Buy button (bottom center)
  const buyText = getBuyText();
  const buyW = buyText.length * charW + pad * 4;
  const buyH = charH + pad * 3;
  buyBtn = new ui.Button(
    floor((w - buyW) / 2),
    h - buyH - 12,
    buyW,
    buyH,
  );

  // Tier buttons (3 across, above buy button)
  const tierW = floor(min(w - 24, 280));
  const tierH = charH + pad * 2;
  const tierY = buyBtn.box.y - (tierH + 6) * 3 - 12;
  tierBtns = tiers.map((t, i) => {
    const btn = new ui.Button(
      floor((w - tierW) / 2),
      tierY + i * (tierH + 6),
      tierW,
      tierH,
    );
    btn._tierIndex = i;
    return btn;
  });

  // Currency toggle (top right)
  const curText = currency.toUpperCase();
  const curW = curText.length * charW + pad * 2;
  currencyBtn = new ui.Button(w - curW - 8, 8, curW, charH + pad);
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

  // Tier buttons
  tierBtns.forEach((btn, i) => {
    const t = tiers[i];
    const tierAmt = currency === "dkk" ? t.dkk : t.amount;
    const isSelected = amount === tierAmt;
    const isHover = btn.down;

    let fillColor, borderColor, textColor;
    if (isSelected) {
      fillColor = [40, 50, 40];
      borderColor = [120, 200, 120];
      textColor = [220, 255, 220];
    } else if (isHover) {
      fillColor = [35, 35, 40];
      borderColor = [150, 150, 180];
      textColor = [200, 200, 220];
    } else {
      fillColor = [22, 22, 25];
      borderColor = [60, 60, 65];
      textColor = [140, 140, 145];
    }

    ink(...fillColor).box(btn.box, "fill");
    ink(...borderColor).box(btn.box, "outline");

    // Left: price
    const priceText = displayAmount(tierAmt, currency);
    ink(...textColor).write(priceText, {
      x: btn.box.x + pad * 2,
      y: btn.box.y + pad,
    });

    // Right: description
    ink(...(isSelected ? [160, 200, 160] : [90, 90, 95])).write(t.desc, {
      x: btn.box.x + btn.box.w - t.desc.length * charW - pad * 2,
      y: btn.box.y + pad,
    });

    // Selection indicator
    if (isSelected) {
      ink(120, 200, 120).write(">", {
        x: btn.box.x + pad - 2,
        y: btn.box.y + pad,
      });
    }
  });

  // Buy button
  if (buyBtn) {
    const buyText = getBuyText();
    const isHover = buyBtn.down;
    const isPending = buyPending;

    // Recalculate width for current text
    const buyW = buyText.length * charW + pad * 4;
    buyBtn.box.w = buyW;
    buyBtn.box.x = floor((w - buyW) / 2);

    let fillColor, borderColor, textColor;
    if (isPending) {
      const pulse = sin(performance.now() / 150) * 0.5 + 0.5;
      fillColor = [30 + pulse * 30, 40 + pulse * 20, 30];
      borderColor = [150 + pulse * 105, 200 + pulse * 55, 100];
      textColor = [200 + pulse * 55, 220 + pulse * 35, 180];
    } else if (checkoutError) {
      fillColor = [50, 25, 25];
      borderColor = [200, 80, 80];
      textColor = [255, 120, 120];
    } else if (isHover) {
      fillColor = [40, 55, 40];
      borderColor = [150, 255, 150];
      textColor = [200, 255, 200];
    } else {
      const blink = sin(performance.now() / 500) * 0.3 + 0.7;
      fillColor = [25, 35, 25];
      borderColor = [
        floor(80 + blink * 70),
        floor(160 + blink * 95),
        floor(80 + blink * 70),
      ];
      textColor = [180, 230, 180];
    }

    ink(...fillColor).box(buyBtn.box, "fill");
    ink(...borderColor).box(buyBtn.box, "outline");
    ink(...textColor).write(buyText, {
      x: buyBtn.box.x + pad * 2,
      y: buyBtn.box.y + floor(pad * 1.5),
    });
  }

  // Currency toggle (top right)
  if (currencyBtn) {
    const curText = currency.toUpperCase();
    const isHover = currencyBtn.down;
    ink(isHover ? 200 : 80).write(curText, {
      x: currencyBtn.box.x + pad,
      y: currencyBtn.box.y + floor(pad / 2),
    });
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
          fetchCheckout(api);
        }
      },
    });
  });

  // Currency toggle
  currencyBtn?.act(e, {
    push: () => {
      currency = currency === "usd" ? "dkk" : "usd";
      // Map current amount to equivalent tier in new currency
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
      // Update currency button size
      const curText = currency.toUpperCase();
      const curW = curText.length * charW + pad * 2;
      currencyBtn.box.w = curW;
      currencyBtn.box.x = screen.width - curW - 8;
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
        // Retry
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
