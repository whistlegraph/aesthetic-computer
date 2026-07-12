// bag, 26.07.12
// A "bag" is a curated container of mixed media — pieces, paintings, kidlisp,
// tapes, whatever. Carry it by its handle: type `^reel` (the ^ is the handle,
// the box below holds the bag's name) to open a bag's listing. Bare `bag`
// lists every bag. Tap an item to open it at the right address for its type.
//
// Scrolling matches colors.mjs / chat.mjs: drag (draw → delta.y), two-finger /
// wheel (scroll → e.y), plus inertial fling + spring bounce at the edges, and
// arrow / page keys.
//
// Bags are admin-curated in /aesthetic.computer/bags.json (@jeffrey only for
// now; a user-facing bagging flow can come later without changing this piece).

const ROW_H = 22; // item row height
const HEADER_H = 34; // masked header band
const LEFT = 8;

// scroll physics (from chat.mjs)
const FRICTION = 0.92;
const MIN_VEL = 0.5;
const BOUNCE_K = 0.15;
const BOUNCE_DAMP = 0.7;
const OVERSCROLL = 60;
const DRAG_THRESH = 5;

// Per-type accent + the sigil used to open that media type.
const TYPES = {
  piece: { color: [140, 220, 255], sigil: "" }, // aesthetic.computer/<code>
  kidlisp: { color: [150, 255, 150], sigil: "$" }, // $<code>
  painting: { color: [255, 150, 255], sigil: "#" }, // #<code>
  tape: { color: [255, 200, 100], sigil: "!" }, // !<code>
};
const typeInfo = (t) => TYPES[t] || { color: [200, 200, 200], sigil: "" };
const itemAddress = (it) => typeInfo(it.type).sigil + it.code;

let state = "loading"; // loading | ready | index | error
let bagName = null;
let bag = null; // { title, description, color, items:[] }
let bags = null; // whole map, for index mode
let error = "";

// scroll state (scroll <= 0; 0 = top). Mirrors colors.mjs sign convention.
let scroll = 0;
let scrollVel = 0;
let flinging = false;
let dragging = false;
let dragStart = null;
let contentH = 0; // total content height (set each paint)
let viewH = 0; // scroll viewport height (set each paint)
let rows = []; // per-paint hitboxes: { y0, y1, kind, ref }

const minScroll = () => Math.min(0, viewH - contentH); // most-negative scroll
function clampScroll(soft) {
  const min = minScroll();
  const pad = soft ? OVERSCROLL : 0;
  if (scroll > pad) scroll = pad;
  if (scroll < min - pad) scroll = min - pad;
}
const rowAt = (y) =>
  y >= HEADER_H ? rows.find((r) => y >= r.y0 && y < r.y1) : null;

async function load() {
  try {
    const res = await fetch("/aesthetic.computer/bags.json");
    if (!res.ok) throw new Error("HTTP " + res.status);
    const data = await res.json();
    bags = data.bags || {};
    if (!bagName) {
      state = "index";
      return;
    }
    const found = bags[bagName];
    if (!found) {
      state = "error";
      error = "no bag named ^" + bagName;
      return;
    }
    bag = found;
    state = "ready";
  } catch (e) {
    state = "error";
    error = String(e?.message || e);
  }
}

function boot({ params, wipe, hud }) {
  wipe(8, 8, 12);
  const raw = (params?.[0] || "").replace(/^\^/, "").trim().toLowerCase();
  bagName = raw || null;
  // AC draws the corner label — set it once here so the bag name shows there
  // (and don't redraw it in the piece, or it looks doubled).
  hud?.label?.(bagName ? "^" + bagName : "bag");
  load();
}

// Inertial fling + spring bounce (chat.mjs physics, colors.mjs sign).
function sim() {
  if (!flinging) return;
  const min = minScroll();
  const out = scroll > 0 || scroll < min;
  if (out) {
    const target = scroll > 0 ? 0 : min;
    const disp = scroll - target;
    scrollVel -= disp * BOUNCE_K;
    scrollVel *= BOUNCE_DAMP;
    scroll += scrollVel;
    if (Math.abs(disp) < 0.5 && Math.abs(scrollVel) < MIN_VEL) {
      scroll = target;
      scrollVel = 0;
      flinging = false;
    }
  } else {
    scrollVel *= FRICTION;
    scroll += scrollVel;
    if (scroll > 0 || scroll < min) scrollVel *= 0.5;
    if (Math.abs(scrollVel) < MIN_VEL) {
      scrollVel = 0;
      flinging = false;
    }
  }
}

function paint({ wipe, ink, write, box, screen }) {
  const w = screen.width;
  const h = screen.height;
  wipe(8, 8, 12);

  if (state === "loading") {
    ink(160).write("opening bag…", { center: "xy" });
    return;
  }
  if (state === "error") {
    ink(255, 120, 120).write(error, { center: "x", y: h / 2 - 4 });
    ink(120).write("try `bag` for all bags", { center: "x", y: h / 2 + 8 });
    return;
  }

  rows = [];
  const viewTop = HEADER_H;
  viewH = h - HEADER_H;

  // ---- INDEX MODE: list all bags -------------------------------------------
  if (state === "index") {
    const names = Object.keys(bags);
    contentH = names.length * ROW_H + 8;
    let y = viewTop + 4 + scroll;
    for (const name of names) {
      const b = bags[name];
      if (y + ROW_H > viewTop && y < h) {
        ink(255, 230, 90).write("^" + name, { x: LEFT, y: y + 3 });
        const count = (b.items || []).length + "";
        ink(120).write(count, { x: w - LEFT - count.length * 6, y: y + 3 });
        if (b.description)
          ink(150).write(b.description, { x: LEFT, y: y + 12 });
      }
      rows.push({ y0: y, y1: y + ROW_H, kind: "bag", ref: name });
      y += ROW_H;
    }
    drawScrollbar(ink, box, w, viewTop);
    ink(10, 10, 16).box(0, 0, w, HEADER_H);
    ink(120).write(names.length + " total", { x: LEFT, y: 20 });
    ink(40, 40, 52).box(0, HEADER_H - 1, w, 1);
    return;
  }

  // ---- BAG MODE: list this bag's items -------------------------------------
  const items = bag.items || [];
  contentH = items.length * ROW_H + 8;
  let y = viewTop + 4 + scroll;
  for (const it of items) {
    const info = typeInfo(it.type);
    if (y + ROW_H > viewTop && y < h) {
      const chip = it.type;
      ink(...info.color, 40).box(LEFT, y + 2, chip.length * 6 + 6, 13);
      ink(...info.color).write(chip, { x: LEFT + 3, y: y + 4 });
      ink(230).write(it.name || it.code, {
        x: LEFT + chip.length * 6 + 16,
        y: y + 4,
      });
      // address hint on the right — only when it differs from the name (i.e.
      // typed media like $kidlisp / #painting), so piece rows aren't redundant.
      const addr = itemAddress(it);
      const nm = it.name || it.code;
      if (addr !== nm)
        ink(90).write(addr, { x: w - LEFT - addr.length * 6, y: y + 4 });
    }
    rows.push({ y0: y, y1: y + ROW_H, kind: "item", ref: it });
    y += ROW_H;
  }
  drawScrollbar(ink, box, w, viewTop);

  // header (drawn last to mask scrolled rows). The bag NAME is shown by AC's
  // corner label (set via hud.label in boot) — don't draw it again here.
  ink(10, 10, 16).box(0, 0, w, HEADER_H);
  const count = items.length + " items";
  ink(120).write(count, { x: w - LEFT - count.length * 6, y: 6 });
  if (bag.description) ink(150).write(bag.description, { x: LEFT, y: 20 });
  ink(40, 40, 52).box(0, HEADER_H - 1, w, 1);
}

function drawScrollbar(ink, box, w, viewTop) {
  if (contentH <= viewH) return;
  const thumbH = Math.max(12, (viewH / contentH) * viewH);
  const frac = -scroll / (contentH - viewH); // 0..1
  const thumbY = viewTop + Math.max(0, Math.min(1, frac)) * (viewH - thumbH);
  ink(40, 40, 52).box(w - 3, viewTop, 2, viewH);
  ink(120, 140, 190).box(w - 3, thumbY, 2, thumbH);
}

function act({ event: e, jump }) {
  if (state === "loading" || state === "error") return;

  if (e.is("touch")) {
    dragStart = { x: e.x, y: e.y };
    dragging = false;
    flinging = false;
    scrollVel = 0;
  }

  // Drag scroll (direct manipulation) — colors.mjs: scroll += delta.y.
  if (e.is("draw")) {
    if (dragStart && !dragging) {
      if (
        Math.abs(e.y - dragStart.y) > DRAG_THRESH ||
        Math.abs(e.x - dragStart.x) > DRAG_THRESH
      )
        dragging = true;
    }
    flinging = false;
    scrollVel = scrollVel * 0.4 + (e.delta?.y || 0) * 0.6; // EMA for fling
    scroll += e.delta?.y || 0;
    clampScroll(true); // soft: allow overscroll while dragging
  }

  // Two-finger / trackpad / wheel — colors.mjs: scroll -= e.y.
  if (e.is("scroll")) {
    flinging = false;
    scroll -= e.y || e.delta?.y || 0;
    clampScroll(false);
  }

  // Keyboard scrolling.
  if (e.is("keyboard:down:arrowdown")) { scroll -= ROW_H; clampScroll(false); }
  if (e.is("keyboard:down:arrowup")) { scroll += ROW_H; clampScroll(false); }
  if (e.is("keyboard:down:pagedown")) { scroll -= viewH; clampScroll(false); }
  if (e.is("keyboard:down:pageup")) { scroll += viewH; clampScroll(false); }

  if (e.is("lift")) {
    if (dragging && Math.abs(scrollVel) > MIN_VEL) {
      flinging = true; // fling with momentum
    } else if (!dragging && dragStart) {
      const hit = rowAt(dragStart.y); // it was a tap → open it
      if (hit) {
        if (hit.kind === "bag") jump("^" + hit.ref);
        else jump(itemAddress(hit.ref));
      }
    }
    dragging = false;
    dragStart = null;
  }
}

export { boot, sim, paint, act };
