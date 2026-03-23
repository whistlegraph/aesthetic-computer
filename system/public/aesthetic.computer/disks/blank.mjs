// blank, 26.03.20
// AC Blank — AC Native Laptop product page & checkout

const { floor, sin, min, max } = Math;

const PRODUCT_IMG =
  "https://assets.aesthetic.computer/thinkpad-11e-yoga-gen6.png";

// Module state
let amount = 12800;
let productImg = null;
let checkoutUrl = null;
let checkoutReady = false;
let checkoutError = null;
let buyPending = false;
let thanks = false;

// UI elements
let buyBtn = null;

// Animation
let scrollY = 0;

const charH = 16;

// Scrolling text lines
const scrollLines = [
  "Buy a @jeffrey approved",
  "Thinkpad 11e Yoga Gen 6 (refurbished / used)",
  "(what he uses to develop AC OS)",
  "pre-flashed with AC Native today!",
  "",
  "Comes decorated with recovery USB.",
  "(No USB C Charger Included)",
  "",
];

function displayAmount(amt) {
  return `$${(amt / 100).toFixed(0)}`;
}

function getBuyText() {
  if (buyPending) return "CHECKING OUT...";
  return `BUY ${displayAmount(amount)}`;
}

async function boot({ params, ui, screen, cursor, hud, api, net, dark }) {
  cursor("native");
  hud.labelBack();

  if (params[0] === "thanks") {
    thanks = true;
    return;
  }

  setupButtons(ui, screen);
  fetchCheckout(api);

  // Load product image
  net.preload(PRODUCT_IMG).then((result) => {
    if (result?.img) productImg = result.img;
  });
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
  const w = screen.width;
  const h = screen.height;

  // Theme colors
  const bg = isDark ? [12, 12, 14] : [245, 243, 240];
  const fg = isDark ? 255 : 20;
  const fgDim = isDark ? 120 : 100;
  const fgFaint = isDark ? 60 : 160;
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

  // 💻 Product image (centered above buttons)
  if (productImg) {
    const areaH = contentBottom - 20;
    const maxW = floor(w * 0.85);
    const maxH = floor(areaH * 0.85);
    const scale = min(maxW / productImg.width, maxH / productImg.height);
    const drawW = floor(productImg.width * scale);
    const drawH = floor(productImg.height * scale);
    const dx = floor((w - drawW) / 2);
    const dy = floor(20 + (areaH - drawH) / 2);
    ink(255).paste(productImg, dx, dy, { width: drawW, height: drawH });
  } else {
    ink(fgDim).write("loading...", { center: "x", y: floor(contentBottom / 2), screen });
  }

  // Scrolling text (description, between laptop and buttons)
  {
    scrollY += 0.3;
    const scrollAreaTop = contentBottom - 10;
    const lineH = charH + 4;
    const totalScrollH = scrollLines.length * lineH;
    const scrollOffset = scrollY % totalScrollH;

    for (let i = 0; i < scrollLines.length; i++) {
      const ln = scrollLines[i];
      if (!ln) continue;
      const rawY = scrollAreaTop - scrollOffset + i * lineH;
      // Wrap around
      const y = rawY < scrollAreaTop - totalScrollH
        ? rawY + totalScrollH
        : rawY;
      if (y < scrollAreaTop - lineH || y > scrollAreaTop + lineH * 2) continue;
      // Fade out as it scrolls up
      const dist = scrollAreaTop - y;
      const alpha = max(0, min(255, 255 - floor(dist * 3)));
      const color = i === 0 ? fgDim : fgFaint;
      ink(color, color, color, alpha).write(ln, { center: "x", y: floor(y), screen });
    }
  }

  const $btn = { ink };

  // Buy button
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
        // Silently retry
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
    desc: "Buy a @jeffrey approved Thinkpad 11e Yoga Gen 6 (refurbished / used) pre-flashed with AC Native today! Comes decorated with recovery USB. (No USB C Charger Included)",
  };
}

export { boot, paint, act, meta };
