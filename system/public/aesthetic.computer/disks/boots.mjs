// boots, 2026.01.24
// Boot telemetry viewer

const { max, floor } = Math;
const FONT = "MatrixChunky8";
const POLL_INTERVAL = 15000;
const ROW_HEIGHT = 9;
const TOP_MARGIN = 19;
const BOTTOM_MARGIN = 12;

let boots = [];
let loading = true;
let error = null;
let scroll = 0;
let totalScrollHeight = 0;
let chatHeight = 0;
let lastFetch = 0;
let pollTimer = null;
let autoScroll = false;

function timeAgo(ts) {
  if (!ts) return "?";
  const now = Date.now();
  const past = new Date(ts).getTime();
  const seconds = Math.floor((now - past) / 1000);
  const units = [
    { name: "d", seconds: 86400 },
    { name: "h", seconds: 3600 },
    { name: "m", seconds: 60 },
    { name: "s", seconds: 1 },
  ];
  for (const unit of units) {
    const count = Math.floor(seconds / unit.seconds);
    if (count >= 1) return `${count}${unit.name}`;
  }
  return "now";
}

function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
  }
}

async function fetchBoots() {
  try {
    loading = boots.length === 0;
    error = null;
    const response = await fetch("/api/boot-log?limit=80");
    if (!response.ok) throw new Error(`boot-log ${response.status}`);
    const data = await response.json();
    boots = data.boots || [];
    lastFetch = Date.now();
  } catch (err) {
    error = err.message || String(err);
  } finally {
    loading = false;
  }
}

function formatBootRow(boot) {
  const status = boot.status || "?";
  const host = boot.meta?.host || "?";
  const path = boot.meta?.path || "?";
  const user = boot.meta?.user?.handle || boot.meta?.user?.sub || "anon";
  const elapsed = boot.summary?.elapsedTotal ? `${Math.round(boot.summary.elapsedTotal)}ms` : "";
  const when = timeAgo(boot.createdAt || boot.updatedAt);
  const local = boot.meta?.localDev ? "local" : "prod";
  return `[${status}] ${when} ${local} ${host}${path} @${user} ${elapsed}`;
}

function layout(screen) {
  chatHeight = screen.height - TOP_MARGIN - BOTTOM_MARGIN;
  totalScrollHeight = boots.length * ROW_HEIGHT;
  boundScroll();
}

function paint({ screen, wipe, ink, write }) {
  wipe(0, 0);
  layout(screen);

  if (loading) {
    ink(255, 255, 0).write("loading boots...", 6, TOP_MARGIN);
    return;
  }

  if (error) {
    ink(255, 80, 80).write(`error: ${error}`, 6, TOP_MARGIN);
    return;
  }

  const startIndex = max(0, floor(scroll / ROW_HEIGHT));
  const visibleRows = floor(chatHeight / ROW_HEIGHT) + 2;
  const endIndex = Math.min(boots.length, startIndex + visibleRows);

  for (let i = startIndex; i < endIndex; i += 1) {
    const boot = boots[i];
    const y = TOP_MARGIN + (i * ROW_HEIGHT - scroll);
    const line = formatBootRow(boot);
    ink(180, 200, 220).write(line, 6, y, undefined, undefined, false, FONT);
  }

  const footer = `boots: ${boots.length}  last: ${timeAgo(lastFetch)}  scroll:${Math.round(scroll)}`;
  ink(120, 140, 160).write(footer, 6, screen.height - 9, undefined, undefined, false, FONT);
}

function act({ event: e, api }) {
  if (e.is("scroll") || e.is("wheel")) {
    scroll += e.delta.y;
    boundScroll();
    api.needsPaint();
  }
  if (e.is("key") && e.key === " ") {
    autoScroll = !autoScroll;
  }
}

function sim({ api }) {
  if (autoScroll) {
    scroll += 0.3;
    boundScroll();
    api.needsPaint();
  }
}

async function boot() {
  await fetchBoots();
  pollTimer = setInterval(fetchBoots, POLL_INTERVAL);
}

boot();

export { paint, act, sim };
