// Opinion, 2026.02.17
// Native pixel-text renderer for opinion markdown files.
// Reads from /opinion/*.md with YAML frontmatter.
// Index view at aesthetic.computer/opinion
// Single view at aesthetic.computer/opinion:slug-name

const { min, max, floor, ceil } = Math;

const BODY_FONT = "MatrixChunky8";
const BODY_CHAR_W = 4;
const BODY_LINE_H = 10; // 8px font height + 2px gap
const HEAD_CHAR_W = 6;
const HEAD_LINE_H = 11; // 10px font height + 1px gap
const MARGIN = 6;
const TOP_PAD = 4;
const BOTTOM_PAD = 10;

const scheme = {
  dark: {
    bg: [12, 14, 20],
    title: [255, 220, 140],
    h2: [140, 180, 255],
    h3: [180, 200, 220],
    body: [180, 180, 190],
    quote: [140, 160, 180],
    quoteBar: [60, 80, 120],
    bullet: [120, 140, 180],
    bold: [255, 255, 255],
    rule: [40, 50, 70],
    meta: [100, 110, 130],
    scrollTrack: [30, 35, 50],
    scrollThumb: [80, 100, 160],
    listTitle: [140, 220, 160],
    listAuthor: [160, 140, 200],
    listDate: [100, 110, 130],
    back: [100, 160, 220],
  },
  light: {
    bg: [245, 243, 238],
    title: [40, 30, 10],
    h2: [40, 60, 120],
    h3: [60, 70, 80],
    body: [50, 50, 55],
    quote: [80, 90, 100],
    quoteBar: [160, 170, 190],
    bullet: [100, 110, 130],
    bold: [0, 0, 0],
    rule: [200, 200, 210],
    meta: [120, 120, 130],
    scrollTrack: [220, 220, 225],
    scrollThumb: [160, 170, 200],
    listTitle: [20, 100, 60],
    listAuthor: [100, 60, 140],
    listDate: [120, 120, 130],
    back: [40, 100, 180],
  },
};

// State
let scroll = 0;
let layoutItems = [];
let contentHeight = 0;
let opinionMeta = {};
let blocks = [];
let loading = true;
let error = null;
let indexData = null;
let isIndex = false;
let lastWidth = 0;

// --- Frontmatter parser ---
function parseFrontmatter(md) {
  const match = md.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!match) return { meta: {}, body: md };
  const meta = {};
  match[1].split("\n").forEach((line) => {
    const idx = line.indexOf(":");
    if (idx === -1) return;
    const key = line.slice(0, idx).trim();
    let val = line.slice(idx + 1).trim();
    if (val.startsWith('"') && val.endsWith('"')) val = val.slice(1, -1);
    meta[key] = val;
  });
  return { meta, body: match[2] };
}

// --- Markdown block parser ---
function parseMarkdown(body) {
  const result = [];
  const lines = body.split("\n");
  let paragraph = [];

  const flush = () => {
    if (paragraph.length) {
      result.push({ type: "paragraph", text: paragraph.join(" ") });
      paragraph = [];
    }
  };

  for (const line of lines) {
    if (/^###\s/.test(line)) {
      flush();
      result.push({ type: "h3", text: line.replace(/^###\s*/, "") });
    } else if (/^##\s/.test(line)) {
      flush();
      result.push({ type: "h2", text: line.replace(/^##\s*/, "") });
    } else if (/^#\s/.test(line)) {
      flush();
      result.push({ type: "h1", text: line.replace(/^#\s*/, "") });
    } else if (/^>\s/.test(line)) {
      flush();
      result.push({ type: "quote", text: line.replace(/^>\s*/, "") });
    } else if (/^[-*]\s/.test(line)) {
      flush();
      result.push({ type: "list", text: line.replace(/^[-*]\s*/, "") });
    } else if (/^---+$/.test(line)) {
      flush();
      result.push({ type: "rule" });
    } else if (line.trim() === "") {
      flush();
      result.push({ type: "spacer" });
    } else {
      paragraph.push(line.trim());
    }
  }
  flush();
  return result;
}

// Strip markdown formatting for display (bold, links)
function stripMd(str) {
  return str
    .replace(/\*\*(.+?)\*\*/g, "$1")
    .replace(/\*(.+?)\*/g, "$1")
    .replace(/`(.+?)`/g, "$1")
    .replace(/\[([^\]]+)\]\([^)]+\)/g, "$1");
}

// --- Layout engine ---
function computeLayout(textApi, screenWidth) {
  const items = [];
  let y = TOP_PAD;
  const wrapW = screenWidth - MARGIN * 2;
  const quoteIndent = 10;
  const listIndent = 8;

  // Title
  if (opinionMeta.title) {
    const clean = stripMd(opinionMeta.title);
    const tb = textApi.box(clean, { x: 0, y: 0 }, wrapW);
    const h = tb ? tb.box.height : HEAD_LINE_H;
    items.push({ type: "title", text: clean, y, height: h });
    y += h + 4;
  }

  // Meta line: author + date
  if (opinionMeta.author || opinionMeta.date) {
    const parts = [];
    if (opinionMeta.author) parts.push(opinionMeta.author);
    if (opinionMeta.date) parts.push(opinionMeta.date);
    const metaText = parts.join(" · ");
    const tb = textApi.box(metaText, { x: 0, y: 0 }, wrapW, 1, true, BODY_FONT);
    const h = tb ? tb.box.height : BODY_LINE_H;
    items.push({ type: "meta", text: metaText, y, height: h });
    y += h + 2;

    // Source URL line
    if (opinionMeta.source) {
      const srcText = opinionMeta.source;
      const tb2 = textApi.box(srcText, { x: 0, y: 0 }, wrapW, 1, true, BODY_FONT);
      const h2 = tb2 ? tb2.box.height : BODY_LINE_H;
      items.push({ type: "source", text: srcText, y, height: h2 });
      y += h2 + 2;
    }

    // Rule after meta
    items.push({ type: "rule", y, height: 7 });
    y += 7;
  }

  // Body blocks
  for (const block of blocks) {
    if (block.type === "spacer") {
      y += 6;
      continue;
    }
    if (block.type === "rule") {
      items.push({ type: "rule", y, height: 7 });
      y += 7;
      continue;
    }
    if (block.type === "h1") {
      const clean = stripMd(block.text);
      const tb = textApi.box(clean, { x: 0, y: 0 }, wrapW);
      const h = tb ? tb.box.height : HEAD_LINE_H;
      items.push({ type: "h1", text: clean, y, height: h });
      y += h + 6;
    } else if (block.type === "h2") {
      const clean = stripMd(block.text);
      const tb = textApi.box(clean, { x: 0, y: 0 }, wrapW);
      const h = tb ? tb.box.height : HEAD_LINE_H;
      items.push({ type: "h2", text: clean, y, height: h });
      y += h + 4;
    } else if (block.type === "h3") {
      const clean = stripMd(block.text);
      const tb = textApi.box(clean, { x: 0, y: 0 }, wrapW, 1, true, BODY_FONT);
      const h = tb ? tb.box.height : BODY_LINE_H;
      items.push({ type: "h3", text: clean, y, height: h });
      y += h + 3;
    } else if (block.type === "paragraph") {
      const clean = stripMd(block.text);
      const tb = textApi.box(clean, { x: 0, y: 0 }, wrapW, 1, true, BODY_FONT);
      const h = tb ? tb.box.height : BODY_LINE_H;
      items.push({ type: "paragraph", text: clean, y, height: h });
      y += h + 4;
    } else if (block.type === "quote") {
      const clean = stripMd(block.text);
      const tb = textApi.box(
        clean, { x: 0, y: 0 }, wrapW - quoteIndent, 1, true, BODY_FONT,
      );
      const h = tb ? tb.box.height : BODY_LINE_H;
      items.push({ type: "quote", text: clean, y, height: h });
      y += h + 4;
    } else if (block.type === "list") {
      const clean = stripMd(block.text);
      const tb = textApi.box(
        clean, { x: 0, y: 0 }, wrapW - listIndent, 1, true, BODY_FONT,
      );
      const h = tb ? tb.box.height : BODY_LINE_H;
      items.push({ type: "list", text: clean, y, height: h });
      y += h + 2;
    }
  }

  y += BOTTOM_PAD;
  return { items, totalHeight: y };
}

function clampScroll(screen) {
  const viewH = screen.height;
  const minScroll = -max(0, contentHeight - viewH);
  scroll = max(minScroll, min(0, scroll));
}

// 🥾
async function boot({ colon, params }) {
  const slug = colon?.[0];

  if (!slug) {
    isIndex = true;
    try {
      const res = await fetch("/opinion/index.json");
      if (!res.ok) throw new Error("fetch failed");
      indexData = await res.json();
    } catch {
      error = "Failed to load opinions.";
    }
    loading = false;
  } else {
    try {
      const res = await fetch(`/opinion/${slug}.md`);
      if (!res.ok) throw new Error("not found");
      const md = await res.text();
      const { meta, body } = parseFrontmatter(md);
      opinionMeta = meta;
      blocks = parseMarkdown(body);
    } catch {
      error = `Opinion "${slug}" not found.`;
    }
    loading = false;
  }
}

// 🎨
function paint({ wipe, ink, screen, text, line, dark, paintCount }) {
  const pal = dark ? scheme.dark : scheme.light;
  wipe(pal.bg);

  if (loading) {
    if (paintCount > 4n) {
      ink(pal.meta).write("Loading...", { center: "xy" }, undefined, undefined, true, BODY_FONT);
    }
    return;
  }

  if (error) {
    ink(255, 80, 80).write(error, { center: "xy" }, undefined, undefined, true, BODY_FONT);
    return;
  }

  if (isIndex) {
    paintIndex({ wipe, ink, screen, text, line, dark, pal });
    return;
  }

  // Recompute layout if screen width changed
  if (screen.width !== lastWidth) {
    const result = computeLayout(text, screen.width);
    layoutItems = result.items;
    contentHeight = result.totalHeight;
    lastWidth = screen.width;
    clampScroll(screen);
  }

  const viewTop = 0;
  const viewBottom = screen.height;

  for (const item of layoutItems) {
    const y = item.y + scroll;
    // Skip off-screen items
    if (y + item.height < viewTop || y > viewBottom) continue;

    if (item.type === "title") {
      ink(pal.title).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true);
    } else if (item.type === "meta") {
      ink(pal.meta).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true, BODY_FONT);
    } else if (item.type === "source") {
      ink(pal.h2).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true, BODY_FONT);
    } else if (item.type === "rule") {
      const ruleY = y + 3;
      ink(pal.rule).line(MARGIN, ruleY, screen.width - MARGIN, ruleY);
    } else if (item.type === "h1") {
      ink(pal.title).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true);
    } else if (item.type === "h2") {
      ink(pal.h2).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true);
    } else if (item.type === "h3") {
      ink(pal.h3).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true, BODY_FONT);
    } else if (item.type === "paragraph") {
      ink(pal.body).write(item.text, { x: MARGIN, y }, undefined, screen.width - MARGIN * 2, true, BODY_FONT);
    } else if (item.type === "quote") {
      ink(pal.quoteBar).line(MARGIN + 2, y, MARGIN + 2, y + item.height);
      ink(pal.quote).write(item.text, { x: MARGIN + 10, y }, undefined, screen.width - MARGIN * 2 - 10, true, BODY_FONT);
    } else if (item.type === "list") {
      ink(pal.bullet).write("-", { x: MARGIN, y }, undefined, undefined, false, BODY_FONT);
      ink(pal.body).write(item.text, { x: MARGIN + 8, y }, undefined, screen.width - MARGIN * 2 - 8, true, BODY_FONT);
    }
  }

  // Scrollbar
  const viewH = screen.height;
  if (contentHeight > viewH) {
    const trackX = screen.width - 3;
    const ratio = viewH / contentHeight;
    const thumbH = max(8, floor(ratio * viewH));
    const thumbY = floor((-scroll / contentHeight) * viewH);
    ink(pal.scrollTrack).line(trackX, 0, trackX, viewH);
    ink(pal.scrollThumb).line(trackX, thumbY, trackX, thumbY + thumbH);
  }
}

function paintIndex({ ink, screen, text, line, pal }) {
  // Header
  ink(pal.title).write("Opinions", { x: MARGIN, y: TOP_PAD });
  const headerBottom = TOP_PAD + HEAD_LINE_H + 2;
  ink(pal.rule).line(MARGIN, headerBottom, screen.width - MARGIN, headerBottom);

  if (!indexData || indexData.length === 0) {
    ink(pal.meta).write("No opinions yet.", { x: MARGIN, y: headerBottom + 8 }, undefined, undefined, true, BODY_FONT);
    return;
  }

  let y = headerBottom + 6 + scroll;
  const wrapW = screen.width - MARGIN * 2;

  for (const entry of indexData) {
    if (y > screen.height) break;

    // Title
    const titleH = HEAD_LINE_H;
    if (y + titleH > 0) {
      ink(pal.listTitle).write(entry.title, { x: MARGIN, y });
    }
    y += titleH;

    // Author + date
    const metaText = [entry.author, entry.date].filter(Boolean).join(" · ");
    if (y + BODY_LINE_H > 0) {
      ink(pal.listAuthor).write(metaText, { x: MARGIN + 4, y }, undefined, undefined, true, BODY_FONT);
    }
    y += BODY_LINE_H + 8;
  }

  // Update content height for index scrolling
  contentHeight = (headerBottom + 6) + indexData.length * (HEAD_LINE_H + BODY_LINE_H + 8) + BOTTOM_PAD;
}

// 🎪
function act({ event: e, screen, needsPaint, jump, net }) {
  if (loading) return;

  // Scroll wheel
  if (e.is("scroll")) {
    scroll -= e.y;
    clampScroll(screen);
    needsPaint();
    return;
  }

  // Drag scroll
  if (e.is("draw:1")) {
    scroll += e.delta.y;
    clampScroll(screen);
    needsPaint();
    return;
  }

  // Keyboard scrolling
  if (e.is("keyboard:down:arrowdown")) {
    scroll -= 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:arrowup")) {
    scroll += 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:pagedown")) {
    scroll -= screen.height - 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:pageup")) {
    scroll += screen.height - 20;
    clampScroll(screen);
    needsPaint();
  }
  if (e.is("keyboard:down:home")) {
    scroll = 0;
    needsPaint();
  }
  if (e.is("keyboard:down:end")) {
    scroll = -max(0, contentHeight - screen.height);
    needsPaint();
  }

  // Back to index from single view
  if (!isIndex && e.is("keyboard:down:escape")) {
    jump("opinion");
    return;
  }

  // Click on index items
  if (isIndex && indexData && e.is("lift")) {
    const headerBottom = TOP_PAD + HEAD_LINE_H + 2 + 6;
    const entryH = HEAD_LINE_H + BODY_LINE_H + 8;

    for (let i = 0; i < indexData.length; i++) {
      const entryY = headerBottom + i * entryH + scroll;
      if (e.y >= entryY && e.y < entryY + entryH && e.x >= MARGIN) {
        jump("opinion:" + indexData[i].slug);
        return;
      }
    }
  }

  // Click source URL to open externally
  if (!isIndex && opinionMeta.source && e.is("lift")) {
    // Check if clicking in the source area
    for (const item of layoutItems) {
      if (item.type === "source") {
        const y = item.y + scroll;
        if (e.y >= y && e.y < y + item.height && e.x >= MARGIN) {
          net.web(opinionMeta.source);
          return;
        }
      }
    }
  }
}

function meta() {
  return {
    desc: "Opinions on computing, creativity, and the body.",
  };
}

export { boot, paint, act, meta };
