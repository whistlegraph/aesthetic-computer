// Give, 2026.10.07
// Keep Aesthetic Computer online and free.

const colors = { ground: [40, 28, 58], panel: [57, 38, 74], text: [255, 239, 215],
  pink: [255, 112, 190], mint: [159, 240, 201], muted: [196, 172, 204], shadow: [99, 42, 98] };
const currencies = { usd: { symbol: "$", initial: 8, min: 1, max: 2500, presets: [4, 8, 16, 32] },
  dkk: { symbol: "kr", initial: 50, min: 5, max: 17500, presets: [25, 50, 100, 200] } };
let count, countFailed, amount, currency, monthly, busy, error, source, thanks;
let buttons = {}, controller, scroll = 0, scrollMax = 0, frame = 0, focused = null;

function boot($) {
  controller?.abort();
  controller = new AbortController();
  buttons = {}; focused = null; count = null; countFailed = false; busy = false; error = ""; scroll = 0; frame = 0;
  currency = $.query?.currency === "dkk" ? "dkk" : "usd";
  monthly = $.query?.frequency !== "once";
  amount = currencies[currency].initial;
  const initial = Number($.query?.amount || $.params?.[0]);
  if (Number.isFinite(initial) && initial >= currencies[currency].min && initial <= currencies[currency].max)
    amount = Math.round(initial * 100) / 100;
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

function money(value = amount) {
  return currency === "usd" ? `$${value}` : `${value} kr`;
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
      body: JSON.stringify({ amount: Math.round(amount * 100), currency, recurring: monthly, source, surface: "piece" }) });
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
  const hero = compact ? 44 : 64, total = hero + 248;
  scrollMax = Math.max(0, total + 28 - screen.height);
  scroll = Math.max(0, Math.min(scrollMax, scroll));
  const top = Math.max(24, Math.floor((screen.height - total) / 2)) - scroll;
  const line = (text, y, color = colors.text, size = 1, small = false) => {
    ink(color).write(text, { x: cx, y, center: "x", size }, undefined, undefined, false, small ? "MatrixChunky8" : undefined);
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
      { x: x + w / 2, y: y + Math.floor((h - 10) / 2), center: "x" });
  };
  // The live count is the picture: oversized native pixels, a gently moving shadow.
  const countText = count === null ? countFailed ? "?" : "..." : count.toLocaleString();
  const size = Math.min(compact ? 4 : 6, width / (countText.length * 7));
  const sway = Math.round(Math.sin(frame / 90));
  ink(colors.shadow).write(countText, { x: cx + 3 + sway, y: top + 3, center: "x", size });
  line(countText, top, colors.pink, size);
  line(countFailed ? "Count unavailable" : `monthly supporter${count === 1 ? "" : "s"}`, top + hero + 3);
  line(thanks ? "Thank you for supporting AC!" : "Keep AC online & free.", top + hero + 21, colors.muted, 1, true);
  let y = top + hero + 45;
  const half = Math.floor((width - 6) / 2);
  button("monthly", "Monthly", left, y, half, 24, () => { monthly = true; }, monthly);
  button("once", "Once", left + half + 6, y, width - half - 6, 24, () => { monthly = false; }, !monthly);
  y += 34;
  const change = delta => { amount = Math.round(Math.max(currencies[currency].min, Math.min(currencies[currency].max, amount + delta)) * 100) / 100; };
  button("less", "-", left, y, 28, 30, () => change(-1));
  button("more", "+", left + width - 28, y, 28, 30, () => change(1));
  line(money(), y + 5, colors.text, money().length > 6 ? 1 : 2);
  y = top + hero + 121;
  const presetWidth = Math.floor((width - 18) / 4);
  currencies[currency].presets.forEach((value, i) => button(`preset${i}`, String(value),
    left + i * (presetWidth + 6), y, presetWidth, 24, () => { amount = value; }, amount === value));
  y = top + hero + 153;
  button("give", busy ? "Opening..." : `Give ${money()}${monthly ? " / month" : ""}`, left, y, width, 30, () => checkout($), false, true);
  line(error || (monthly ? "Renews monthly. Cancel anytime." : "One-time gift."), top + hero + 191,
    error ? colors.pink : colors.muted, 1, true);
  line("Not tax deductible.", top + hero + 203, colors.muted, 1, true);
  y = top + hero + 224;
  button("currency", currency.toUpperCase(), left, y, 38, 20, () => {
    currency = currency === "usd" ? "dkk" : "usd";
    amount = currencies[currency].initial;
  });
  button("bills", "Bills", left + 44, y, 44, 20, () => open($, "https://bills.aesthetic.computer"));
  button("more-options", "More", left + width - 66, y, 66, 20, () => open($, "https://give.aesthetic.computer"));
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
