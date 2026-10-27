// visitors, 26.10.26
// Who's on AC right now: handles by name (and where they are), guests as a count.
// Polls the session server's `/visitors`; `visitors:local` reads a dev server.

const POLL = 5000; // ms between polls.
const KEEP = 120; // samples of history for the sparkline (~10 min).

let url;
let now = { handles: [], guests: 0 }; // the latest reading.
let history = [];
let lastAt = 0; // when the latest reading landed.
let failed = false;
let polling = false;
let nextPoll = 0;

function boot({ params }) {
  url =
    params[0] === "local"
      ? "https://localhost:8889/visitors"
      : "https://session-server.aesthetic.computer/visitors";
}

async function poll() {
  polling = true;
  try {
    const res = await fetch(url, { cache: "no-store" });
    if (!res.ok) throw new Error(res.status);
    now = await res.json();
    lastAt = Date.now();
    failed = false;
    history.push(now.handles.length + now.guests);
    if (history.length > KEEP) history.shift();
  } catch {
    failed = true; // keep showing the last reading, marked stale.
  }
  polling = false;
}

function sim() {
  const t = Date.now();
  if (!polling && t >= nextPoll) {
    nextPoll = t + POLL;
    poll();
  }
}

// A steady hue per handle, so a name keeps its color across polls.
function hue(handle) {
  let h = 0;
  for (const ch of handle) h = (h * 31 + ch.charCodeAt(0)) >>> 0;
  const a = (h % 360) / 60, x = 1 - Math.abs((a % 2) - 1), i = Math.floor(a);
  const [r, g, b] = [[1, x, 0], [x, 1, 0], [0, 1, x], [0, x, 1], [x, 0, 1], [1, 0, x]][i];
  return [120 + r * 135, 120 + g * 135, 120 + b * 135].map(Math.round);
}

function paint({ wipe, ink, line, screen }) {
  const { width: w, height: h } = screen;
  const big = w >= 640 ? 3 : 2; // text scale for the headline numbers.
  const total = now.handles.length + now.guests;

  wipe(10, 12, 18);

  // Headline: the total, then the split. Starts below the corner label.
  let y = 28;
  const headline = `${total} on AC`;
  const hs = Math.max(1, Math.min(big * 2, Math.floor((w - 16) / (headline.length * 6))));
  ink(255).write(headline, { x: 8, y, size: hs });
  y += 10 * hs + 6;
  const plural = (n, word) => `${n} ${word}${n === 1 ? "" : "s"}`;
  const named = plural(now.handles.length, "handle");
  const unnamed = plural(now.guests, "guest");
  ink(130, 220, 255).write(named, { x: 8, y, size: big });
  let gx = 8 + (named.length + 2) * 6 * big;
  if (gx + unnamed.length * 6 * big > w - 8) (gx = 8), (y += 10 * big + 4); // wrap on phones.
  ink(200, 200, 140).write(unnamed, { x: gx, y, size: big });
  y += 10 * big + 12;
  const top = y; // where the handle columns restart.

  // The handles, in columns when they run past the bottom.
  const ls = w >= 1000 ? 3 : w >= 480 ? 2 : 1; // list text scale.
  const rowH = 12 * ls;
  const colW = 6 * 28 * ls;
  const bottom = h - 34; // leave room for the sparkline + status.
  let x = 8;
  let shown = 0;
  for (const v of now.handles) {
    if (y > bottom - rowH) {
      y = top;
      x += colW;
    }
    // Last slot that fits holds the overflow count instead of a name.
    const lastSlot = x + 2 * colW > w && y + 2 * rowH > bottom;
    if (lastSlot && now.handles.length - shown > 1) {
      ink(110).write(`+${now.handles.length - shown} more`, { x, y, size: ls });
      break;
    }
    ink(...hue(v.handle)).write(v.handle, { x, y, size: ls });
    const where = v.piece ? v.piece : "…";
    const tabs = v.tabs > 1 ? ` x${v.tabs}` : "";
    ink(110).write(`${where}${tabs}`, { x: x + 6 * ls * (v.handle.length + 1), y, size: ls });
    y += rowH;
    shown += 1;
  }
  if (!now.handles.length) ink(80).write("no handles on right now", { x, y, size: ls });

  // Sparkline of the total along the bottom edge.
  const sy = h - 22, sh = 14;
  const peak = Math.max(1, ...history);
  ink(40, 44, 60).line(8, sy, w - 8, sy);
  for (let i = 1; i < history.length; i++) {
    const step = (w - 16) / (KEEP - 1);
    const x0 = 8 + (i - 1) * step, x1 = 8 + i * step;
    ink(130, 220, 255).line(
      x0,
      sy - (history[i - 1] / peak) * sh,
      x1,
      sy - (history[i] / peak) * sh,
    );
  }

  // Status, bottom-right.
  const ago = lastAt ? Math.round((Date.now() - lastAt) / 1000) : null;
  const status = failed
    ? `stale ${ago ?? "?"}s`
    : ago === null
      ? "connecting…"
      : `peak ${peak} · ${ago}s ago`;
  ink(failed ? [255, 120, 100] : 90).write(status, {
    x: w - 8 - status.length * 6,
    y: h - 14,
  });
}

function meta() {
  return {
    title: "Visitors",
    desc: "Who's on Aesthetic Computer right now: handles and guests.",
  };
}

export { boot, sim, paint, meta };
