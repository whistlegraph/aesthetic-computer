// Give, 2026.10.07
// Keep Aesthetic Computer online and free.

const colors = { ground: [40, 28, 58], panel: [57, 38, 74], text: [255, 239, 215],
  pink: [255, 112, 190], mint: [159, 240, 201], muted: [196, 172, 204], shadow: [99, 42, 98] };
let count, countFailed, busy, error, source, thanks;
let buttons = {}, controller, scroll = 0, scrollMax = 0, frame = 0, focused = null;

function boot($) {
  controller?.abort();
  controller = new AbortController();
  buttons = {}; focused = null; count = null; countFailed = false; busy = false; error = ""; scroll = 0; frame = 0;
  source = $.query?.source === "homepage" ? "homepage" : "give-piece";
  thanks = $.query?.thanks === "1";
  $.hud?.labelBack();
  const lifetime = controller;
  const signal = AbortSignal.any([lifetime.signal, AbortSignal.timeout(8000)]);
  fetch("/api/gives?summary=subscribers", { signal }).then(async response => {
    if (!response.ok) throw Error("Count unavailable");
    const data = await response.json();
    if (!Number.isSafeInteger(data.activeSubscribers) || data.activeSubscribers < 0) throw Error("Invalid count");
    if (!signal.aborted) count = data.activeSubscribers;
  }).catch(() => { if (!lifetime.signal.aborted) countFailed = true; })
    .finally(() => { if (!lifetime.signal.aborted) $.needsPaint(); });
}

function open($, url) {
  if ($.net?.iframe) $.send({ type: "post-to-parent", content: { type: "openExternal", url } });
  else $.jump(url);
}

async function checkout($) {
  if (busy) return;
  busy = true; error = ""; $.needsPaint();
  const lifetime = controller;
  const signal = AbortSignal.any([lifetime.signal, AbortSignal.timeout(20000)]);
  try {
    const response = await fetch("/api/give", { method: "POST", signal,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ amount: 800, currency: "usd", recurring: true, source, surface: "piece" }) });
    const data = await response.json();
    if (!response.ok || !data.url) throw Error("Checkout unavailable");
    const url = new URL(data.url);
    if (url.protocol !== "https:" || !["checkout.stripe.com", "pay.aesthetic.computer"].includes(url.hostname))
      throw Error("Invalid checkout URL");
    if (!signal.aborted) open($, url.href);
  } catch {
    if (!lifetime.signal.aborted) error = "Couldn't open checkout. Try again.";
  } finally {
    if (controller === lifetime) { busy = false; $.needsPaint(); }
  }
}

function paint($) {
  const { ink, wipe, screen, ui } = $;
  wipe(colors.ground);
  const cx = Math.floor(screen.width / 2), width = Math.min(208, screen.width - 16);
  const left = Math.floor(cx - width / 2), compact = screen.height < 350;
  const hero = compact ? 44 : 64, total = hero + 149;
  scrollMax = Math.max(0, total + 28 - screen.height);
  scroll = Math.max(0, Math.min(scrollMax, scroll));
  const top = Math.max(24, Math.floor((screen.height - total) / 2)) - scroll;
  const line = (text, y, color = colors.text, size = 1) => {
    ink(color).write(text, { x: cx, y, center: "x", size }, undefined, undefined, false, "MatrixChunky8");
  };
  const button = (id, label, x, y, w, h, action, selected = false, primary = false) => {
    const item = buttons[id] ||= { button: new ui.Button() };
    item.action = action;
    const b = item.button;
    Object.assign(b.box, { x, y, w, h });
    b.disabled = busy || y < 20 || y + h > screen.height - 2;
    const active = !b.disabled && (b.down || b.over || focused === id);
    const fill = primary ? colors.pink : selected ? colors.mint : active ? colors.shadow : colors.panel;
    ink(colors.shadow).box(x + 2, y + 2, w, h, "fill");
    ink(fill).box(b.box, "fill");
    ink(active ? colors.text : selected ? colors.mint : primary ? colors.pink : colors.muted).box(b.box, "outline");
    ink(primary || selected ? colors.ground : colors.text).write(label,
      { x: x + w / 2, y: y + Math.floor((h - 10) / 2), center: "x" }, undefined, undefined, false, "MatrixChunky8");
  };
  // The live count is the picture: oversized native pixels, a gently moving shadow.
  const countText = count === null ? countFailed ? "?" : "..." : count.toLocaleString();
  const size = Math.min(compact ? 4 : 6, width / (countText.length * 7));
  const sway = Math.round(Math.sin(frame / 90));
  ink(colors.shadow).write(countText, { x: cx + 3 + sway, y: top + 3, center: "x", size }, undefined, undefined, false, "MatrixChunky8");
  line(countText, top, colors.pink, size);
  line(countFailed ? "Count unavailable" : `monthly supporter${count === 1 ? "" : "s"}`, top + hero + 3);
  line(thanks ? "Thank you for supporting AC!" : "Keep AC online & free.", top + hero + 21, colors.muted);
  button("give", busy ? "Opening..." : "Give $8 / month", left, top + hero + 47, width, 34,
    () => checkout($), false, true);
  line(error || "Renews monthly. Cancel anytime.", top + hero + 91,
    error ? colors.pink : colors.muted);
  line("Not tax deductible.", top + hero + 103, colors.muted);
  const half = Math.floor((width - 6) / 2), y = top + hero + 125;
  button("bills", "Bills", left, y, half, 20, () => open($, "https://bills.aesthetic.computer"));
  button("more-options", "Other ways", left + half + 6, y, width - half - 6, 20,
    () => open($, `https://give.aesthetic.computer/?source=${source}`));
  // Leave AC's corner navigation clear when the short-screen layout scrolls.
  ink(colors.ground).box(0, 0, screen.width, 20, "fill");
  if (scrollMax) ink(colors.mint).box(screen.width - 3, 22 + (screen.height - 32) * scroll / scrollMax, 1, 8, "fill");
}

function act($) {
  const e = $.event;
  if (e.is("keyboard:down:tab")) {
    const enabled = Object.keys(buttons).filter(id => !buttons[id].button.disabled);
    focused = enabled[(enabled.indexOf(focused) + 1) % enabled.length] || null;
    $.needsPaint(); return;
  }
  if (focused && (e.is("keyboard:down:enter") || e.is("keyboard:down:space"))) {
    if (!busy && !buttons[focused].button.disabled) { error = ""; buttons[focused].action(); }
    $.needsPaint(); return;
  }
  if (e.is("scroll")) scroll = Math.max(0, Math.min(scrollMax, scroll - e.y));
  if (e.is("draw:1") && !Object.values(buttons).some(item => item.button.down))
    scroll = Math.max(0, Math.min(scrollMax, scroll - e.delta.y));
  for (const { button, action } of Object.values(buttons)) button.act(e, { push: () => {
    if (busy) return;
    error = ""; action(); $.needsPaint();
  } });
  $.needsPaint();
}

function sim() { frame++; }
function leave() { controller?.abort(); }
function meta() { return { title: "Give", desc: "Keep Aesthetic Computer online and free." }; }
export { boot, paint, act, sim, leave, meta };
