// The oskiewar wall, drawn by the engine.
//
// This is a piece, not a page: it is handed the same drawing surface oskiewar
// itself gets — `wipe`, `box`, `line`, `write` (the 5x7 HUD font) and
// `comicWrite` (Comic Relief) — and it defines boot/sim/paint like any other.
// Nothing here knows about the DOM, which is the point: the same file can hang
// off a browser shell today and a native host once one exists on Linux.
//
// It draws one pane. Which pane comes from the host, because Mutter will not let
// a single window cover both of a stacked pair of monitors, so the wall is two
// processes and each is told what it is:
//
//   top     what happened today, at eye level
//   bottom  all time, and how the reels did
//
// Numbers are written with `write` at a large size rather than with a system
// font, so they are the game's own letters — the same ones the HUD counts rounds
// with. Headings use Comic Relief, which is the voice the title screen speaks in.

// The HUD's palette, by role rather than by name, so the two themes swap
// together and nothing has a colour defined in only one of them.
const palettes = {
  dark: {
    ground: [11, 11, 13], panel: [21, 21, 26], edge: [38, 38, 46],
    ink: [242, 242, 245], dim: [138, 138, 153], faint: [74, 74, 88],
    hot: [255, 210, 63], cool: [79, 195, 247], live: [110, 231, 168],
  },
  light: {
    ground: [244, 244, 242], panel: [255, 255, 255], edge: [222, 222, 222],
    ink: [20, 20, 26], dim: [106, 106, 118], faint: [180, 180, 192],
    hot: [184, 134, 11], cool: [2, 119, 189], live: [27, 138, 85],
  },
};

let pane = "top";
let stats = null;
let theme = palettes.dark;
let view = { width: 2560, height: 1440 };
// A count of frames painted, only so a wall that has stopped repainting is
// distinguishable from one that is merely quiet.
let frames = 0;

function boot() {
  pane = (typeof wallPane === "function" ? wallPane() : "top") === "bottom"
    ? "bottom" : "top";
  frames = 0;
}

function sim() {
  stats = typeof wallStats === "function" ? wallStats() : null;
  // The game's HUD takes its palette from a Los Angeles clock — light in the
  // daytime, dark at night — so the wall in the same room agrees with it.
  const zone = stats?.timezone || "America/Los_Angeles";
  const hour = Number(new Intl.DateTimeFormat("en-US", {
    timeZone: zone, hour: "numeric", hourCycle: "h23",
  }).format(new Date()));
  theme = hour >= 7 && hour < 19 ? palettes.light : palettes.dark;
  if (typeof gameView === "function") {
    const measured = gameView();
    if (measured?.width && measured?.height) view = measured;
  }
}

// --- letters -----------------------------------------------------------------

// `write` advances six cells per character and a cell is size/7, so a string's
// width is knowable without measuring — which is what lets things be centred
// and right-aligned in a font that has no metrics.
const cellOf = (size) => Math.max(2, Math.floor(size / 7));
const textWidth = (text, size) => String(text).length * cellOf(size) * 6;
const writeRight = (text, right, y, size, ink) =>
  write(text, right - textWidth(text, size), y, size, ...ink);

// Thousands separators, done here because the block font has a comma and
// `toLocaleString` would also hand back characters it does not.
function commas(value) {
  const digits = String(Math.round(Number(value) || 0));
  let out = "";
  for (let index = 0; index < digits.length; index++) {
    if (index > 0 && (digits.length - index) % 3 === 0) out += ",";
    out += digits[index];
  }
  return out;
}

// --- furniture ---------------------------------------------------------------

function panel(x, y, width, height) {
  box(x, y, width, height, ...theme.panel);
  // A one-pixel edge rather than a border radius: the HUD has no curves.
  line(x, y, x + width, y, 1, ...theme.edge);
  line(x, y + height, x + width, y + height, 1, ...theme.edge);
  line(x, y, x, y + height, 1, ...theme.edge);
  line(x + width, y, x + width, y + height, 1, ...theme.edge);
}

function heading(text, x, y, note) {
  write(text.toUpperCase(), x, y, 26, ...theme.dim);
  if (note) write(note.toUpperCase(), x + textWidth(text, 26) + 28, y, 20, ...theme.faint);
}

// One number, as big as the pane can afford. Zero is drawn faint so a quiet day
// never reads as a broken feed.
function tile(label, value, x, y, width, size, note) {
  write(label.toUpperCase(), x, y, 22, ...theme.dim);
  const text = commas(value);
  const ink = Number(value) > 0 ? theme.ink : theme.faint;
  write(text, x, y + 40, size, ...ink);
  if (note) write(note.toUpperCase(), x, y + 40 + size + 14, 18, ...theme.faint);
  return width;
}

// Bars are boxes. An empty column still gets a floor so the axis reads as a row
// of zeroes rather than as missing data.
function bars(series, x, y, width, height, highlightLast) {
  const count = Math.max(1, series.length);
  const gap = Math.max(1, Math.floor(width / count / 8));
  const barWidth = Math.max(1, Math.floor((width - gap * (count - 1)) / count));
  let peak = 1;
  for (const value of series) if (value > peak) peak = value;
  for (let index = 0; index < series.length; index++) {
    const value = series[index] || 0;
    const tall = value ? Math.max(3, Math.round(value / peak * height)) : 2;
    const ink = !value ? theme.edge
      : (highlightLast && index === series.length - 1 ? theme.hot : theme.cool);
    box(x + index * (barWidth + gap), y + height - tall, barWidth, tall, ...ink);
  }
}

function splitBar(x, y, width, height, dummy, human) {
  const total = dummy + human;
  if (!total) {
    box(x, y, width, height, ...theme.edge);
    write("NO ROUNDS YET", x + 18, y + Math.floor(height / 2) - 8, 20, ...theme.dim);
    return;
  }
  const dummyWidth = Math.round(width * (dummy / total));
  box(x, y, dummyWidth, height, ...theme.faint);
  box(x + dummyWidth, y, width - dummyWidth, height, ...theme.live);
  const dummyLabel = `DUMMY ${Math.round(dummy / total * 100)}%`;
  const humanLabel = `PLAYER ${Math.round(human / total * 100)}%`;
  if (dummyWidth > textWidth(dummyLabel, 20) + 24)
    write(dummyLabel, x + 16, y + Math.floor(height / 2) - 8, 20, ...theme.ink);
  if (width - dummyWidth > textWidth(humanLabel, 20) + 24)
    write(humanLabel, x + dummyWidth + 16, y + Math.floor(height / 2) - 8, 20,
      ...theme.ground);
}

// --- the two panes -----------------------------------------------------------

function chrome() {
  const { width } = view;
  wipe(...theme.ground);
  comicWrite("oskie", 48, 34, 46, ...theme.ink);
  comicWrite("war", 48 + 118, 34, 46, ...theme.hot);
  const stamp = stats
    ? `${stats.day}  ${pane}`
    : "waiting for the stats endpoint";
  writeRight(stamp.toUpperCase(), width - 48, 46, 22, theme.dim);
  return 110;
}

function paintTop(top) {
  const { width, height } = view;
  const today = stats.today;
  const margin = 48;
  const inner = width - margin * 2;

  // The hero takes the slack. A chart given the remainder stretches an empty
  // axis over half the monitor on a day with no play, which reads as breakage.
  const heroHeight = height - top - 340 - margin;
  panel(margin, top, inner, heroHeight);
  const column = Math.floor(inner / 3);
  const heroSize = Math.min(190, Math.floor(heroHeight * 0.52));
  const heroY = top + Math.floor(heroHeight / 2) - Math.floor(heroSize / 2) - 30;
  tile("rounds today", today.rounds, margin + 40, heroY, column, heroSize);
  tile("players today", today.players, margin + 40 + column, heroY, column, heroSize);
  tile("pops today", today.pops, margin + 40 + column * 2, heroY, column, heroSize);

  const chartY = top + heroHeight + 24;
  panel(margin, chartY, inner, 200);
  heading("rounds, last 24 hours", margin + 40, chartY + 26,
    "each bar one hour, now at right");
  bars(stats.hourly, margin + 40, chartY + 78, inner - 80, 96, true);

  const splitY = chartY + 224;
  panel(margin, splitY, inner, height - splitY - margin);
  const live = today.rounds > 0;
  heading("who was on the other side", margin + 40, splitY + 26,
    live ? "today" : "all time - nothing played today yet");
  splitBar(margin + 40, splitY + 74, inner - 80, 52,
    live ? today.dummy : stats.allTime.dummy,
    live ? today.localPlayer : stats.allTime.localPlayer);
  write(`MATCHES TODAY ${commas(today.matches)}`, margin + 40, splitY + 146, 22,
    ...theme.dim);
}

function paintBottom(top) {
  const { width, height } = view;
  const all = stats.allTime;
  const margin = 48;
  const inner = width - margin * 2;

  panel(margin, top, inner, 210);
  const column = Math.floor(inner / 4);
  tile("rounds all time", all.rounds, margin + 40, top + 30, column, 96);
  tile("matches finished", all.matches, margin + 40 + column, top + 30, column, 96,
    `${(all.completionRatio * 100).toFixed(1)}% of rounds`);
  tile("matches started", all.series, margin + 40 + column * 2, top + 30, column, 96);
  tile("players ever", all.players, margin + 40 + column * 3, top + 30, column, 96,
    "dummy excluded");

  const chartY = top + 234;
  panel(margin, chartY, inner, 250);
  heading("rounds per day", margin + 40, chartY + 26,
    `${stats.daily.length} days - today at right`);
  bars(stats.daily.map((row) => row.rounds), margin + 40, chartY + 78,
    inner - 80, 140, true);

  const reelY = chartY + 274;
  panel(margin, reelY, inner, height - reelY - margin);
  const reels = stats.reels;
  if (!reels) {
    heading("instagram reels", margin + 40, reelY + 26);
    write("FIGURES NEED THE WALL KEY", margin + 40, reelY + 84, 26, ...theme.faint);
    return;
  }
  heading("instagram reels", margin + 40, reelY + 26,
    `${reels.live} live - ${reels.measured} measured`);

  // The reels themselves, looping, to the right of the figures. A reel is 9:16,
  // so the tiles are too, and each is captioned with the one number that judges
  // a hook: how many people skipped it in the first three seconds.
  const reelHeight = height - reelY - margin - 96;
  const tileHeight = Math.max(120, reelHeight);
  const tileWidth = Math.round(tileHeight * 9 / 16);
  const shown = Math.min(typeof reelCount === "function" ? reelCount() : 0, 4);
  const stripWidth = shown * (tileWidth + 20);
  const stripX = margin + inner - 40 - stripWidth + 20;
  for (let index = 0; index < shown; index++) {
    const x = stripX + index * (tileWidth + 20);
    const y = reelY + 70;
    // The frame is drawn behind the video so a reel still loading reads as an
    // empty slot rather than as a hole in the panel.
    box(x, y, tileWidth, tileHeight, ...theme.edge);
    const drew = reelFrame(index, x, y, tileWidth, tileHeight);
    if (!drew) write("LOADING", x + 12, y + 12, 18, ...theme.faint);
    const post = reels.posts[index];
    if (post) {
      write(String(post.segment || "").toUpperCase(), x, y + tileHeight + 10, 18,
        ...theme.cool);
      if (post.insights?.reels_skip_rate != null)
        write(`SKIP ${post.insights.reels_skip_rate}%`, x, y + tileHeight + 34, 18,
          ...theme.dim);
    }
  }
  // The table gets whatever the strip left, so the two never overlap.
  const tableRight = stripX - 40;
  // Right-aligned columns, placed from the panel's right edge inward so the
  // widest number decides the layout rather than the header text.
  const right = tableRight;
  const columns = [
    ["skip", right],
    ["watch", right - 200],
    ["reach", right - 420],
    ["views", right - 660],
  ];
  for (const [label, edge] of columns)
    writeRight(label.toUpperCase(), edge, reelY + 76, 20, theme.dim);
  write("DAY", margin + 40, reelY + 76, 20, ...theme.dim);
  write("SEGMENT", margin + 320, reelY + 76, 20, ...theme.dim);

  let rowY = reelY + 116;
  for (const post of reels.posts.slice(0, 8)) {
    const got = post.insights;
    write(String(post.day), margin + 40, rowY, 24, ...theme.ink);
    write(String(post.segment || "-").toUpperCase(), margin + 320, rowY, 24, ...theme.cool);
    writeRight(got ? commas(got.views) : "-", right - 660, rowY, 24, theme.ink);
    writeRight(got ? commas(got.reach) : "-", right - 420, rowY, 24, theme.dim);
    writeRight(got?.ig_reels_avg_watch_time != null
      ? `${(got.ig_reels_avg_watch_time / 1000).toFixed(1)}S` : "-",
      right - 200, rowY, 24, theme.dim);
    writeRight(got?.reels_skip_rate != null ? `${got.reels_skip_rate}%` : "-",
      right, rowY, 24, theme.dim);
    rowY += 44;
  }
  line(margin + 40, rowY + 4, right, rowY + 4, 1, ...theme.edge);
  write("TOTAL", margin + 40, rowY + 20, 24, ...theme.ink);
  writeRight(commas(reels.totals.views), right - 660, rowY + 20, 24, theme.ink);
  writeRight(commas(reels.totals.reach), right - 420, rowY + 20, 24, theme.ink);
}

// A wall must say when it stopped being true, so the one thing painted even
// without stats is why there are none.
function paintWaiting(top) {
  const { width } = view;
  const margin = 48;
  panel(margin, top, width - margin * 2, 200);
  write("NO STATS YET", margin + 40, top + 60, 40, ...theme.dim);
  const reason = (typeof wallError === "function" ? wallError() : "") || "";
  if (reason) write(String(reason).toUpperCase().slice(0, 72),
    margin + 40, top + 124, 22, ...theme.faint);
}

function paint() {
  frames++;
  const top = chrome();
  if (!stats) { paintWaiting(top); return; }
  if (pane === "bottom") paintBottom(top); else paintTop(top);

  // Freshness, bottom right, in the smallest thing worth reading from a doorway.
  const age = typeof wallAge === "function" ? wallAge() : null;
  if (age != null) {
    const stale = age > 150;
    writeRight(stale ? `STALE ${Math.round(age)}S` : `UPDATED ${Math.round(age)}S AGO`,
      view.width - 48, view.height - 40, 20, stale ? theme.hot : theme.faint);
  }
}
