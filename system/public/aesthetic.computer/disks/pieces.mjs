// pieces, 26.04.21
// Most recently added / edited pieces.

const { floor, max, min } = Math;
const LIMIT = 50;
const ROW_HEIGHT = 14;
const LEFT_MARGIN = 6;
const HEADER_HEIGHT = 16;
const COMPACT_FONT = "MatrixChunky8";
const COMPACT_CHAR_W = 4;

const pal = {
  dark: {
    bg: [16, 16, 24],
    headerBg: [24, 24, 36],
    name: [100, 220, 150],
    date: [80, 100, 140],
    msg: [80, 80, 100],
    highlight: [255, 255, 100],
    loading: [80, 100, 140],
  },
  light: {
    bg: [240, 240, 245],
    headerBg: [230, 230, 238],
    name: [20, 120, 60],
    date: [120, 130, 160],
    msg: [130, 130, 150],
    highlight: [200, 160, 0],
    loading: [120, 130, 160],
  },
};

let items = [];      // [{ name, date, message }]
let scroll = 0;
let anyDown = false;
let itemButtons = [];
let loaded = false;

function formatDate(dateStr) {
  if (!dateStr) return "";
  const d = new Date(dateStr);
  const now = new Date();
  const diff = floor((now - d) / 86400000);
  if (diff === 0) return "today";
  if (diff === 1) return "1d ago";
  if (diff < 30) return `${diff}d ago`;
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
}

async function boot({ net }) {
  const [docsResult, commitsResult] = await Promise.all([
    net.requestDocs().catch(() => null),
    fetch("/api/piece-commits").then((r) => r.json()).catch(() => ({ commits: {} })),
  ]);

  const commits = commitsResult?.commits || {};
  const docs = docsResult?.pieces || {};

  // Only include pieces that exist in docs (not hidden) or have a commit entry
  const names = new Set([
    ...Object.keys(docs).filter((k) => !docs[k]?.hidden),
    ...Object.keys(commits),
  ]);

  items = Array.from(names)
    .map((name) => ({
      name,
      date: commits[name]?.date ? new Date(commits[name].date) : null,
      message: commits[name]?.message || "",
    }))
    .filter((item) => item.date) // only show pieces with a known date
    .sort((a, b) => b.date - a.date)
    .slice(0, LIMIT);

  loaded = true;
  buildButtons();
}

function buildButtons() {
  itemButtons = items.map((item, i) => ({
    name: item.name,
    y: HEADER_HEIGHT + 4 + i * ROW_HEIGHT,
    down: false,
  }));
}

function paint({ wipe, ink, screen, dark, paintCount }) {
  const c = dark ? pal.dark : pal.light;
  wipe(c.bg);

  if (!loaded) {
    if (paintCount > 8n) ink(c.loading).write("Loading...", { center: "xy" });
    return;
  }

  if (items.length === 0) {
    ink(c.loading).write("No recent pieces.", { center: "xy" });
    return;
  }

  const contentTop = HEADER_HEIGHT + 4;
  const viewH = screen.height - contentTop;

  // Rows
  items.forEach((item, i) => {
    const y = contentTop + i * ROW_HEIGHT + scroll;
    if (y < contentTop - ROW_HEIGHT || y > screen.height) return;

    const btn = itemButtons[i];
    if (btn?.down) ink(c.highlight, 50).box(0, y, screen.width, ROW_HEIGHT);

    // Name
    ink(btn?.down ? c.highlight : c.name).write(item.name, { x: LEFT_MARGIN, y: y + 1 });

    // Date (right-aligned, compact font)
    const dateStr = formatDate(item.date);
    const dateW = dateStr.length * COMPACT_CHAR_W;
    ink(c.date).write(dateStr, { x: screen.width - dateW - 4, y: y + 2 }, undefined, undefined, false, COMPACT_FONT);

    // Commit message (between name and date, truncated)
    if (item.message) {
      const nameW = item.name.length * 6 + LEFT_MARGIN + 8;
      const maxW = screen.width - nameW - dateW - 12;
      const maxChars = floor(maxW / COMPACT_CHAR_W);
      if (maxChars > 4) {
        const msg = item.message.length > maxChars
          ? item.message.slice(0, maxChars - 2) + ".."
          : item.message;
        ink(c.msg).write(msg, { x: nameW, y: y + 2 }, undefined, undefined, false, COMPACT_FONT);
      }
    }
  });

  // Scrollbar
  const totalH = items.length * ROW_HEIGHT;
  if (totalH > viewH) {
    const thumbH = max(8, (viewH / totalH) * viewH);
    const thumbY = contentTop + (-scroll / totalH) * viewH;
    ink(dark ? [60, 60, 80] : [180, 180, 200]).box(screen.width - 3, contentTop, 2, viewH);
    ink(dark ? [100, 120, 180] : [120, 130, 170]).box(screen.width - 3, thumbY, 2, thumbH);
  }

  // Header (painted last to mask scroll)
  ink(c.headerBg).box(0, 0, screen.width, HEADER_HEIGHT);
  const countStr = `${items.length} recent`;
  ink(c.date).write(countStr, { x: LEFT_MARGIN, y: 3 }, undefined, undefined, false, COMPACT_FONT);
}

function act({ event: e, screen, hud, piece, jump, needsPaint, beep }) {
  const contentTop = HEADER_HEIGHT + 4;
  const viewH = screen.height - contentTop;
  const totalH = items.length * ROW_HEIGHT;

  if (e.is("scroll")) {
    scroll = max(-(totalH - viewH), min(0, scroll - e.y));
    needsPaint();
    return;
  }

  if (!anyDown && e.is("draw:1")) {
    scroll = max(-(totalH - viewH), min(0, scroll + e.delta.y));
    needsPaint();
  }

  itemButtons.forEach((btn) => {
    const y = btn.y + scroll;
    const inBounds = e.x >= 0 && e.x <= screen.width && e.y >= y && e.y <= y + ROW_HEIGHT;

    if (e.is("touch") && inBounds) {
      btn.down = true;
      anyDown = true;
      hud.label(btn.name, "white");
      needsPaint();
    }

    if (e.is("lift") && btn.down) {
      btn.down = false;
      anyDown = false;
      beep?.();
      jump(btn.name);
    }
  });

  if (e.is("lift")) {
    anyDown = false;
    itemButtons.forEach((b) => (b.down = false));
    hud.label(piece);
    needsPaint();
  }
}

function meta() {
  return {
    title: "Pieces",
    desc: "Most recently added or edited pieces.",
  };
}

export { boot, paint, act, meta };
