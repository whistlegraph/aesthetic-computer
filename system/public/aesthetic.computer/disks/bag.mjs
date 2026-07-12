// bag, 26.07.12
// A "bag" is a curated container of mixed media — pieces, paintings, kidlisp,
// tapes, whatever. Carry it by its handle: type `^reel` (the ^ is the handle,
// the box below holds the bag's name) to open a bag's listing. Bare `bag`
// lists every bag. Tap an item to open it at the right address for its type.
//
// Bags are admin-curated in /aesthetic.computer/bags.json (@jeffrey only for
// now; a user-facing bagging flow can come later without changing this piece).

const ROW_H = 22; // item row height
const HEADER_H = 34; // masked header band
const LEFT = 8;

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

let scroll = 0;
let maxScroll = 0;
let rows = []; // per-paint hitboxes: { y0, y1, kind, ref }

// tap-vs-drag tracking
let downY = null;
let downScroll = 0;
let moved = 0;

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

function boot({ params, wipe }) {
  wipe(8, 8, 12);
  const raw = (params?.[0] || "").replace(/^\^/, "").trim().toLowerCase();
  bagName = raw || null;
  load();
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
  const viewH = h - HEADER_H;

  // ---- INDEX MODE: list all bags -------------------------------------------
  if (state === "index") {
    const names = Object.keys(bags);
    let y = viewTop + 4 + scroll;
    for (const name of names) {
      const b = bags[name];
      const rowTop = y;
      if (y + ROW_H > viewTop && y < h) {
        ink(255, 230, 90).write("^" + name, { x: LEFT, y: y + 3 });
        const count = (b.items || []).length + "";
        ink(120).write(count, { x: w - LEFT - count.length * 6, y: y + 3 });
        if (b.description)
          ink(150).write(b.description, { x: LEFT, y: y + 12 });
      }
      rows.push({ y0: rowTop, y1: rowTop + ROW_H, kind: "bag", ref: name });
      y += ROW_H;
    }
    const total = names.length * ROW_H + 8;
    maxScroll = Math.max(0, total - viewH);

    ink(10, 10, 16).box(0, 0, w, HEADER_H);
    ink(255, 230, 90).write("bags", { x: LEFT, y: 8 });
    ink(120).write(names.length + " total", { x: LEFT, y: 20 });
    ink(40, 40, 52).box(0, HEADER_H - 1, w, 1);
    return;
  }

  // ---- BAG MODE: list this bag's items -------------------------------------
  let y = viewTop + 4 + scroll;
  for (const it of bag.items || []) {
    const info = typeInfo(it.type);
    const rowTop = y;
    if (y + ROW_H > viewTop && y < h) {
      // type chip
      const chip = it.type;
      ink(...info.color, 40).box(LEFT, y + 2, chip.length * 6 + 6, 13);
      ink(...info.color).write(chip, { x: LEFT + 3, y: y + 4 });
      // name
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
    rows.push({ y0: rowTop, y1: rowTop + ROW_H, kind: "item", ref: it });
    y += ROW_H;
  }
  const total = (bag.items || []).length * ROW_H + 8;
  maxScroll = Math.max(0, total - viewH);

  // scrollbar
  if (maxScroll > 0) {
    const thumbH = Math.max(12, (viewH / total) * viewH);
    const thumbY = viewTop + (-scroll / maxScroll) * (viewH - thumbH);
    ink(40, 40, 52).box(w - 3, viewTop, 2, viewH);
    ink(120, 140, 190).box(w - 3, thumbY, 2, thumbH);
  }

  // header (drawn last to mask scrolled rows)
  ink(10, 10, 16).box(0, 0, w, HEADER_H);
  const titleColor = bag.color || "yellow";
  ink(titleColor).write("^" + bagName, { x: LEFT, y: 6 });
  const count = (bag.items || []).length + " items";
  ink(120).write(count, { x: w - LEFT - count.length * 6, y: 6 });
  if (bag.description) ink(150).write(bag.description, { x: LEFT, y: 20 });
  ink(40, 40, 52).box(0, HEADER_H - 1, w, 1);
  return false;
}

function act({ event: e, jump, screen }) {
  if (state === "loading" || state === "error") return;

  if (e.is("touch")) {
    downY = e.y;
    downScroll = scroll;
    moved = 0;
  }

  if (e.is("draw")) {
    if (downY !== null) {
      const dy = e.y - downY;
      moved = Math.max(moved, Math.abs(dy));
      scroll = downScroll + dy;
      if (scroll > 0) scroll = 0;
      if (scroll < -maxScroll) scroll = -maxScroll;
    }
  }

  if (e.is("scroll")) {
    scroll -= e.y || e.delta?.y || 0;
    if (scroll > 0) scroll = 0;
    if (scroll < -maxScroll) scroll = -maxScroll;
  }

  if (e.is("lift")) {
    const wasTap = moved < 6 && downY !== null && downY >= HEADER_H;
    if (wasTap) {
      const hit = rows.find((r) => downY >= r.y0 && downY < r.y1);
      if (hit) {
        if (hit.kind === "bag") jump("^" + hit.ref);
        else jump(itemAddress(hit.ref));
      }
    }
    downY = null;
    moved = 0;
  }
}

export { boot, paint, act };
