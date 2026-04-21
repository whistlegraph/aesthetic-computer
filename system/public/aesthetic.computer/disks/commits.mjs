// commits, 2025.1.14
// Live Tangled commit feed for aesthetic.computer/core.
// Source: https://tangled.org/aesthetic.computer/core (via /api/commits)

const { max, min, floor, sin } = Math;

const TANGLED_REPO_URL = "https://tangled.org/aesthetic.computer/core";
const POLL_INTERVAL = 30000; // 30 seconds
const COMMITS_PER_PAGE = 30;

let commits = [];
let scroll = 0;
let totalScrollHeight = 0;
let chatHeight = 0;
let loading = true;
let loadingMore = false;
let error = null;
let lastFetch = 0;
let pollTimer = null;
let rowHeight = 10;
let topMargin = 18;
let bottomMargin = 22;
let hue = 0;
let pulsePhase = 0;
let needsLayout = true;
let autoScroll = false;
let autoScrollDelay = 2000;
let loadTime = 0;
let autoScrollSpeed = 0.25;
let currentPage = 1;
let hasMoreCommits = true;
let showDetailedView = true;
let frameCount = 0;

// UI buttons (created in boot, painted in paint, handled in act).
let tangledBtn = null;
let detailBtn = null;
let playBtn = null;
let refreshBtn = null;

const FONT = "MatrixChunky8";
const COLORS = {
  bg: [10, 12, 18],
  line: [35, 40, 55],
  sha: [255, 180, 100],
  shaNew: [150, 255, 150],
  author: [180, 150, 255],
  message: [200, 200, 220],
  time: [90, 100, 120],
  additions: [100, 220, 130],
  deletions: [255, 120, 120],
  files: [140, 180, 220],
  year: [255, 200, 100],
  month: [180, 160, 220],
  week: [140, 180, 140],
  day: [120, 140, 160],
};

// Parse relative time.
function timeAgo(dateStr) {
  const now = new Date();
  const past = new Date(dateStr);
  const seconds = Math.floor((now - past) / 1000);
  const units = [
    { name: "y", seconds: 31536000 },
    { name: "mo", seconds: 2592000 },
    { name: "w", seconds: 604800 },
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

function formatNum(n) {
  if (n >= 1000) return (n / 1000).toFixed(1) + "k";
  return String(n);
}

// Extract a handle-like label from git author names. Tangled commits are
// authored as e.g. "prompt.ac/@jeffrey" — we want the `@jeffrey` part.
function formatAuthor(name) {
  if (!name) return "@?";
  const atMatch = name.match(/@([A-Za-z0-9._-]+)/);
  if (atMatch) return "@" + atMatch[1];
  const first = name.split(/\s+/)[0].toLowerCase();
  return "@" + first;
}

function statsBar(add, del, maxWidth = 30) {
  const total = add + del;
  if (total === 0) return { addW: 0, delW: 0 };
  const scale = min(1, total / 200);
  const w = floor(maxWidth * scale);
  const addW = total > 0 ? max(1, floor((add / total) * w)) : 0;
  const delW = total > 0 ? max(1, floor((del / total) * w)) : 0;
  return { addW, delW };
}

function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
  }
}

function getTimelineMarker(dateStr, prevDateStr) {
  const d = new Date(dateStr);
  const prev = prevDateStr ? new Date(prevDateStr) : null;
  const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

  if (!prev || d.getFullYear() !== prev.getFullYear()) {
    return { type: "year", label: `◆ ${d.getFullYear()} ◆`, color: COLORS.year };
  }
  if (d.getMonth() !== prev.getMonth()) {
    return { type: "month", label: `── ${months[d.getMonth()]} ──`, color: COLORS.month };
  }
  const weekOfYear = (date) => {
    const start = new Date(date.getFullYear(), 0, 1);
    return Math.ceil(((date - start) / 86400000 + start.getDay() + 1) / 7);
  };
  if (weekOfYear(d) !== weekOfYear(prev)) {
    return { type: "week", label: `· Week ${weekOfYear(d)} ·`, color: COLORS.week };
  }
  if (d.getDate() !== prev.getDate()) {
    return { type: "day", label: `${days[d.getDay()]} ${d.getDate()}`, color: COLORS.day };
  }
  return null;
}

async function fetchCommits(page = 1, append = false) {
  try {
    if (page === 1) loading = commits.length === 0;
    else loadingMore = true;

    const response = await fetch(`/api/commits?page=${page}&per_page=${COMMITS_PER_PAGE}`);
    if (!response.ok) throw new Error(`commits API error: ${response.status}`);
    const data = await response.json();
    if (data.error) throw new Error(data.error);

    hasMoreCommits = !!data.hasMore;

    const newCommits = (data.commits || []).map((c) => ({
      sha: c.shortSha,
      fullSha: c.sha,
      message: (c.message || "").split("\n")[0],
      fullMessage: c.message || "",
      author: c.author,
      email: c.email,
      date: c.date,
      parents: c.parents || 0,
      additions: c.additions || 0,
      deletions: c.deletions || 0,
      files: c.files || 0,
    }));

    if (append) {
      const existing = new Set(commits.map((c) => c.fullSha));
      commits = [...commits, ...newCommits.filter((c) => !existing.has(c.fullSha))];
    } else {
      const hadCommits = commits.length > 0;
      const oldFirstSha = commits[0]?.fullSha;
      commits = newCommits;
      if (hadCommits && commits[0]?.fullSha !== oldFirstSha) {
        hue = 120;
      }
    }

    lastFetch = Date.now();
    loading = false;
    loadingMore = false;
    error = null;
    needsLayout = true;
    currentPage = page;

    if (page === 1 && !append && loadTime === 0) loadTime = Date.now();
  } catch (err) {
    error = err.message;
    loading = false;
    loadingMore = false;
    console.error("Failed to fetch commits:", err);
  }
}

async function loadMoreIfNeeded() {
  if (loadingMore || !hasMoreCommits) return;
  if (scroll > totalScrollHeight - chatHeight - 200) {
    await fetchCommits(currentPage + 1, true);
  }
}

function buildButtons({ ui, screen }) {
  tangledBtn = new ui.TextButton("tangled", { right: 4, top: 4, screen });
  detailBtn = new ui.TextButton(showDetailedView ? "compact" : "detail", {
    left: 4,
    bottom: 4,
    screen,
  });
  playBtn = new ui.TextButton(autoScroll ? "pause" : "play", {
    left: 4 + detailBtn.width + 4,
    bottom: 4,
    screen,
  });
  refreshBtn = new ui.TextButton("refresh", { right: 4, bottom: 4, screen });
}

function syncButtons({ screen }) {
  if (!tangledBtn) return;
  // Labels track state.
  detailBtn.replaceLabel(showDetailedView ? "compact" : "detail");
  playBtn.replaceLabel(autoScroll ? "pause" : "play");
  // Reposition (widths change with label).
  tangledBtn.reposition({ right: 4, top: 4, screen });
  refreshBtn.reposition({ right: 4, bottom: 4, screen });
  detailBtn.reposition({ left: 4, bottom: 4, screen });
  playBtn.reposition({ left: 4 + detailBtn.width + 4, bottom: 4, screen });
}

// Color schemes: [fill, outline, text] / [hover fill, hover outline, hover text]
const BTN_SCHEME = [[20, 24, 34], [80, 90, 120], [180, 190, 220]];
const BTN_HOVER = [[30, 35, 50], [140, 160, 210], [220, 230, 255]];
const LINK_SCHEME = [[0, 0, 0, 0], [40, 60, 110], [130, 170, 255]];
const LINK_HOVER = [[20, 30, 60], [140, 180, 255], [220, 235, 255]];
const PLAY_SCHEME = [[14, 28, 20], [60, 160, 100], [140, 230, 170]];
const PLAY_HOVER = [[20, 40, 30], [110, 220, 150], [200, 255, 220]];

function boot({ ui, screen }) {
  fetchCommits();
  pollTimer = setInterval(() => fetchCommits(1, false), POLL_INTERVAL);
  scroll = 0;
  autoScroll = false;
  loadTime = 0;
  frameCount = 0;
  buildButtons({ ui, screen });
  topMargin = 4 + (tangledBtn?.height || 18) + 4;
  bottomMargin = 4 + (detailBtn?.height || 18) + 4;
}

function paint($) {
  const { wipe, ink, screen, line, text, box, needsPaint, mask, unmask, ui } = $;
  const { width: w, height: h } = screen;
  frameCount++;
  pulsePhase += 0.05;

  const bgPulse = sin(pulsePhase * 0.3) * 2;
  wipe(COLORS.bg[0] + bgPulse, COLORS.bg[1] + bgPulse, COLORS.bg[2] + bgPulse);

  // Ensure buttons exist (in case of hot-reload) and keep them in sync.
  if (!tangledBtn) buildButtons({ ui, screen });
  syncButtons({ screen });
  topMargin = 4 + tangledBtn.height + 4;
  bottomMargin = 4 + detailBtn.height + 4;

  // Top divider line with gradient effect
  const topLineY = topMargin - 1;
  for (let i = 0; i < w; i++) {
    const alpha = 40 + sin(i * 0.02 + pulsePhase) * 15;
    ink(50, 55, 75, alpha).box(i, topLineY, 1, 1);
  }

  // Bottom divider line
  const botLineY = h - bottomMargin;
  for (let i = 0; i < w; i++) {
    const alpha = 40 + sin(i * 0.02 - pulsePhase) * 15;
    ink(50, 55, 75, alpha).box(i, botLineY, 1, 1);
  }

  if (loading && commits.length === 0) {
    const dots = ".".repeat((floor(frameCount / 10) % 4));
    ink(150, 150, 180).write(
      `Loading commits${dots}`,
      { center: "xy", x: w / 2, y: h / 2 },
      false,
      undefined,
      false,
      FONT,
    );
    paintButtons($);
    needsPaint();
    return;
  }

  if (error && commits.length === 0) {
    ink(255, 100, 100).write(
      "Error: " + error.slice(0, 40),
      { center: "xy", x: w / 2, y: h / 2 },
      false,
      undefined,
      false,
      FONT,
    );
    paintButtons($);
    return;
  }

  chatHeight = h - topMargin - bottomMargin;
  const baseCommitHeight = rowHeight + 2;
  const expandedCommitHeight = rowHeight * 4 + 4;
  const commitHeight = showDetailedView ? expandedCommitHeight : baseCommitHeight;
  const yearMarkerHeight = 18;
  const monthMarkerHeight = 14;
  const smallMarkerHeight = rowHeight + 4;

  if (needsLayout) {
    let height = 0;
    for (let i = 0; i < commits.length; i++) {
      const prevDate = i > 0 ? commits[i - 1].date : null;
      const marker = getTimelineMarker(commits[i].date, prevDate);
      if (marker) {
        if (marker.type === "year") height += yearMarkerHeight;
        else if (marker.type === "month") height += monthMarkerHeight;
        else height += smallMarkerHeight;
      }
      height += commitHeight;
    }
    totalScrollHeight = height;
    needsLayout = false;
  }

  mask({ x: 0, y: topMargin, width: w, height: chatHeight });

  let y = topMargin - scroll;

  for (let i = 0; i < commits.length; i++) {
    const commit = commits[i];
    const prevDate = i > 0 ? commits[i - 1].date : null;
    const marker = getTimelineMarker(commit.date, prevDate);

    if (marker) {
      let markerH = smallMarkerHeight;
      if (marker.type === "year") markerH = yearMarkerHeight;
      else if (marker.type === "month") markerH = monthMarkerHeight;

      if (y + markerH >= topMargin - markerH && y < h - bottomMargin + markerH) {
        const bgAlpha = marker.type === "year" ? 220 : marker.type === "month" ? 180 : 100;
        ink(30, 28, 45, bgAlpha).box(0, y, w, markerH);
        const textY = y + Math.floor((markerH - 8) / 2);

        if (marker.type === "year") {
          const yearGlow = 180 + sin(pulsePhase * 2) * 40;
          ink(marker.color[0], marker.color[1], marker.color[2], yearGlow).write(
            marker.label,
            { center: "x", x: w / 2, y: textY },
            false,
            undefined,
            false,
            FONT,
          );
        } else if (marker.type === "month") {
          ink(...marker.color).write(marker.label, { x: 4, y: textY }, false, undefined, false, FONT);
          const labelW = text.width(marker.label, FONT) + 8;
          ink(marker.color[0], marker.color[1], marker.color[2], 60).line(
            labelW,
            y + markerH / 2,
            w - 4,
            y + markerH / 2,
          );
        } else {
          ink(marker.color[0], marker.color[1], marker.color[2], 180).write(
            marker.label,
            { x: 4, y: textY },
            false,
            undefined,
            false,
            FONT,
          );
        }
      }
      y += markerH;
    }

    if (y + commitHeight < topMargin - 20) {
      y += commitHeight;
      continue;
    }
    if (y > h - bottomMargin + 20) {
      y += commitHeight;
      continue;
    }

    if (showDetailedView) {
      const rowPadding = 3;
      if (i % 2 === 0) {
        ink(18, 20, 28, 100).box(0, y + rowPadding, w, commitHeight - rowPadding * 2);
      }
      ink(40, 45, 60, 40).line(4, y + commitHeight - 1, w - 4, y + commitHeight - 1);
    } else if (i % 2 === 0) {
      ink(18, 20, 28, 60).box(0, y, w, commitHeight);
    }

    const contentY = y + (showDetailedView ? 4 : 1);
    const isNew = i === 0 && hue > 60;
    const shaPulse = isNew ? 200 + sin(pulsePhase * 4) * 55 : 0;
    const shaColor = isNew ? [150 + shaPulse * 0.4, 255, 150 + shaPulse * 0.2] : COLORS.sha;

    ink(30, 32, 42).box(2, contentY, 34, rowHeight);
    ink(...shaColor).write(commit.sha, { x: 4, y: contentY + 1 }, false, undefined, false, FONT);

    let xOffset = 38;
    if (commit.parents > 1) {
      ink(180, 140, 200).write("⊕", { x: xOffset, y: contentY + 1 }, false, undefined, false, FONT);
      xOffset += 10;
    }

    const ago = timeAgo(commit.date);
    ink(...COLORS.time).write(ago, { x: xOffset, y: contentY + 1 }, false, undefined, false, FONT);
    xOffset += text.width(ago + "  ", FONT);

    const author = formatAuthor(commit.author);
    ink(...COLORS.author).write(author, { x: xOffset, y: contentY + 1 }, false, undefined, false, FONT);
    const authorEndX = xOffset + text.width(author, FONT);
    const charWidth = 4;

    if (!showDetailedView) {
      const msgX = authorEndX + 6;
      const availableChars = Math.floor((w - msgX - 8) / charWidth);
      const msg = commit.message;
      if (msg.length <= availableChars) {
        ink(...COLORS.message).write(msg, { x: msgX, y: contentY + 1 }, false, undefined, false, FONT);
      } else {
        const separator = "   ·   ";
        const fullTicker = msg + separator;
        const tickerLen = fullTicker.length;
        const speed = 0.06;
        const offset = Math.floor(frameCount * speed) % tickerLen;
        const doubled = fullTicker + fullTicker;
        const visible = doubled.slice(offset, offset + availableChars);
        ink(...COLORS.message).write(visible, { x: msgX, y: contentY + 1 }, false, undefined, false, FONT);
      }
    }

    if (showDetailedView) {
      const msgY = contentY + rowHeight + 2;
      const maxChars = Math.floor((w - 8) / charWidth);
      const msg = commit.message.slice(0, maxChars);
      ink(...COLORS.message).write(msg, { x: 4, y: msgY }, false, undefined, false, FONT);

      const statsY = contentY + rowHeight * 2 + 4;
      let sx = 4;

      const addText = `+${formatNum(commit.additions)}`;
      ink(...COLORS.additions).write(addText, { x: sx, y: statsY }, false, undefined, false, FONT);
      sx += text.width(addText + " ", FONT);

      const delText = `-${formatNum(commit.deletions)}`;
      ink(...COLORS.deletions).write(delText, { x: sx, y: statsY }, false, undefined, false, FONT);
      sx += text.width(delText + " ", FONT);

      const filesText = `${commit.files}f`;
      ink(...COLORS.files).write(filesText, { x: sx, y: statsY }, false, undefined, false, FONT);
      sx += text.width(filesText + " ", FONT);

      const { addW, delW } = statsBar(commit.additions, commit.deletions, 35);
      if (addW > 0) ink(...COLORS.additions, 180).box(sx, statsY + 2, addW, 4);
      if (delW > 0) ink(...COLORS.deletions, 180).box(sx + addW, statsY + 2, delW, 4);

      const sepY = y + commitHeight - 1;
      for (let sxp = 4; sxp < w - 4; sxp++) {
        const alpha = 20 + sin(sxp * 0.1) * 10;
        ink(35, 38, 50, alpha).box(sxp, sepY, 1, 1);
      }
    }

    y += commitHeight;
  }

  if (loadingMore) {
    const loadDots = ".".repeat((floor(frameCount / 10) % 4));
    ink(150, 150, 180).write(
      `Loading more${loadDots}`,
      { x: 4, y: h - bottomMargin - rowHeight - 4 },
      false,
      undefined,
      false,
      FONT,
    );
  }

  unmask();

  // 📜 Scroll bar
  if (totalScrollHeight > chatHeight) {
    ink(25, 28, 35).box(w - 4, topMargin, 3, chatHeight);
    ink(40, 45, 55).line(w - 5, topMargin, w - 5, topMargin + chatHeight);
    ink(40, 45, 55).line(w - 1, topMargin, w - 1, topMargin + chatHeight);
    const segHeight = max(8, floor((chatHeight / totalScrollHeight) * chatHeight));
    const scrollRatio = scroll / max(1, totalScrollHeight - chatHeight);
    const boxY = topMargin + floor(scrollRatio * (chatHeight - segHeight));
    const thumbColor = autoScroll ? COLORS.additions : [200, 150, 255];
    const thumbGlow = 50 + sin(pulsePhase * 2) * 30;
    ink(thumbColor[0], thumbColor[1], thumbColor[2], thumbGlow).box(w - 6, boxY - 1, 7, segHeight + 2);
    ink(...thumbColor).box(w - 4, boxY, 3, segHeight);
  }

  paintButtons($);

  // Status between buttons (commit count + next-poll countdown).
  const sinceLastFetch = Date.now() - lastFetch;
  const nextPoll = Math.max(0, Math.ceil((POLL_INTERVAL - sinceLastFetch) / 1000));
  const countText = hasMoreCommits ? `${commits.length}+ · ${nextPoll}s` : `${commits.length} · ${nextPoll}s`;
  const countX = Math.floor(w / 2 - text.width(countText, FONT) / 2);
  ink(100, 105, 130).write(
    countText,
    { x: countX, y: h - bottomMargin + 7 },
    false,
    undefined,
    false,
    FONT,
  );

  const sinceLoad = Date.now() - loadTime;
  const waitingToScroll =
    loadTime > 0 && !autoScroll && sinceLoad < autoScrollDelay;

  if (autoScroll || waitingToScroll) needsPaint();
}

function paintButtons($) {
  if (!tangledBtn) return;
  tangledBtn.paint($, LINK_SCHEME, LINK_HOVER);
  detailBtn.paint($, BTN_SCHEME, BTN_HOVER);
  playBtn.paint($, PLAY_SCHEME, PLAY_HOVER);
  refreshBtn.paint($, BTN_SCHEME, BTN_HOVER);
}

function act({ event: e, screen, jump, ui, net }) {
  if (!tangledBtn) buildButtons({ ui, screen });

  // Buttons.
  tangledBtn.act(e, () => {
    jump(`out:${TANGLED_REPO_URL}`);
  });

  detailBtn.act(e, () => {
    showDetailedView = !showDetailedView;
    needsLayout = true;
  });

  playBtn.act(e, () => {
    autoScroll = !autoScroll;
    if (autoScroll) scroll = 0;
  });

  refreshBtn.act(e, () => {
    fetchCommits(1, false);
  });

  // Scroll via drag — disables auto-scroll.
  if (e.is("draw")) {
    autoScroll = false;
    scroll -= e.delta.y;
    boundScroll();
    loadMoreIfNeeded();
  }

  if (e.is("reframed")) {
    syncButtons({ screen });
  }

  // Back to prompt (standard AC convention).
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) jump("prompt");
  if (e.is("keyboard:down:backspace")) jump("prompt");
}

function sim() {
  if (hue > 0) hue = Math.max(0, hue - 0.5);

  const sinceLoad = Date.now() - loadTime;
  if (loadTime > 0 && !autoScroll && sinceLoad >= autoScrollDelay && scroll === 0) {
    autoScroll = true;
  }

  if (autoScroll && totalScrollHeight > chatHeight) {
    scroll += autoScrollSpeed;
    boundScroll();
    if (scroll >= totalScrollHeight - chatHeight) scroll = 0;
    loadMoreIfNeeded();
  }
}

function leave() {
  if (pollTimer) {
    clearInterval(pollTimer);
    pollTimer = null;
  }
}

function meta() {
  return {
    title: "Commits",
    desc: "Live Tangled commit feed for aesthetic.computer/core.",
  };
}

export { boot, paint, act, sim, leave, meta };
