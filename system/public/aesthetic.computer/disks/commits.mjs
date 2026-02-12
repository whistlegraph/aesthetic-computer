// commits, 2025.1.14
// Live GitHub commit feed for aesthetic-computer
// ╔═══════════════════════════════════════════════════════════╗
// ║  A typographically ornate commit history visualization    ║
// ╚═══════════════════════════════════════════════════════════╝

const { max, min, floor, ceil, abs, sin, cos, PI } = Math;

const REPO = "whistlegraph/aesthetic-computer";
const POLL_INTERVAL = 30000; // 30 seconds
const COMMITS_PER_PAGE = 30; // Fewer per page since we fetch details

let commits = [];
let scroll = 0;
let totalScrollHeight = 0;
let chatHeight = 0;
let loading = true;
let loadingMore = false;
let error = null;
let lastFetch = 0;
let pollTimer = null;
let rowHeight = 10; // MatrixChunky8 is 8px + 2px spacing for elegance
let topMargin = 28; // Below HUD label with breathing room
let bottomMargin = 20; // Footer area
let hue = 0;
let pulsePhase = 0;
let needsLayout = true;
let autoScroll = false; // Start paused
let autoScrollDelay = 2000; // 2 second delay before auto-scroll
let loadTime = 0; // When commits first loaded
let autoScrollSpeed = 0.25;
let currentPage = 1;
let hasMoreCommits = true;
let showDetailedView = true; // Toggle detailed stats
let frameCount = 0;
let hoveredCommit = null;
let selectedCommit = null;

// Stats cache for detailed commit info
const statsCache = new Map();
const statsFetching = new Set();

// GitHub link box for click detection
let githubLinkBox = null;

// Visual theming
const FONT = "MatrixChunky8";
const COLORS = {
  bg: [10, 12, 18],
  bgAccent: [16, 18, 26],
  line: [35, 40, 55],
  lineAccent: [50, 55, 75],
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

// Parse relative time
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

// Format numbers with commas
function formatNum(n) {
  if (n >= 1000) return (n / 1000).toFixed(1) + "k";
  return String(n);
}

// Generate sparkline bar for additions/deletions
function statsBar(add, del, maxWidth = 30) {
  const total = add + del;
  if (total === 0) return { addW: 0, delW: 0 };
  const scale = min(1, total / 200); // Scale to max 200 changes
  const w = floor(maxWidth * scale);
  const addW = total > 0 ? max(1, floor((add / total) * w)) : 0;
  const delW = total > 0 ? max(1, floor((del / total) * w)) : 0;
  return { addW, delW };
}

// Bound scroll like chat.mjs does
function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
  }
}

// Fetch detailed stats for a single commit
async function fetchCommitStats(sha) {
  if (statsCache.has(sha) || statsFetching.has(sha)) return;
  statsFetching.add(sha);
  
  try {
    const response = await fetch(
      `https://api.github.com/repos/${REPO}/commits/${sha}`
    );
    if (response.ok) {
      const data = await response.json();
      statsCache.set(sha, {
        additions: data.stats?.additions || 0,
        deletions: data.stats?.deletions || 0,
        files: data.files?.length || 0,
        fileList: (data.files || []).slice(0, 5).map(f => ({
          name: f.filename.split('/').pop(),
          path: f.filename,
          add: f.additions,
          del: f.deletions,
          status: f.status,
        })),
      });
    }
  } catch (e) {
    console.warn("Failed to fetch commit stats:", sha, e);
  } finally {
    statsFetching.delete(sha);
  }
}

// Get timeline marker info for a date
function getTimelineMarker(dateStr, prevDateStr) {
  const d = new Date(dateStr);
  const prev = prevDateStr ? new Date(prevDateStr) : null;
  
  const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  
  // Check for year change (biggest)
  if (!prev || d.getFullYear() !== prev.getFullYear()) {
    return { type: "year", label: `◆ ${d.getFullYear()} ◆`, color: COLORS.year };
  }
  
  // Check for month change
  if (d.getMonth() !== prev.getMonth()) {
    return { type: "month", label: `── ${months[d.getMonth()]} ──`, color: COLORS.month };
  }
  
  // Check for week change (Sunday boundary)
  const weekOfYear = (date) => {
    const start = new Date(date.getFullYear(), 0, 1);
    return Math.ceil(((date - start) / 86400000 + start.getDay() + 1) / 7);
  };
  if (weekOfYear(d) !== weekOfYear(prev)) {
    return { type: "week", label: `· Week ${weekOfYear(d)} ·`, color: COLORS.week };
  }
  
  // Check for day change
  if (d.getDate() !== prev.getDate()) {
    return { type: "day", label: `${days[d.getDay()]} ${d.getDate()}`, color: COLORS.day };
  }
  
  return null;
}

// Fetch commits from GitHub API
async function fetchCommits(page = 1, append = false) {
  try {
    if (page === 1) loading = commits.length === 0;
    else loadingMore = true;
    
    const response = await fetch(
      `https://api.github.com/repos/${REPO}/commits?per_page=${COMMITS_PER_PAGE}&page=${page}`
    );
    
    if (!response.ok) {
      throw new Error(`GitHub API error: ${response.status}`);
    }
    
    const data = await response.json();
    
    // Check if we got fewer commits than requested (end of history)
    if (data.length < COMMITS_PER_PAGE) {
      hasMoreCommits = false;
    }
    
    const newCommits = data.map(c => ({
      sha: c.sha.slice(0, 7),
      fullSha: c.sha,
      message: c.commit.message.split("\n")[0], // First line only
      fullMessage: c.commit.message, // Keep full message for expanded view
      author: c.commit.author.name,
      email: c.commit.author.email,
      date: c.commit.author.date,
      avatar: c.author?.avatar_url,
      parents: c.parents?.length || 0, // For merge detection
    }));
    
    if (append) {
      // Filter out duplicates
      const existingShas = new Set(commits.map(c => c.fullSha));
      const unique = newCommits.filter(c => !existingShas.has(c.fullSha));
      commits = [...commits, ...unique];
    } else {
      // Check for new commits at top
      const hadCommits = commits.length > 0;
      const oldFirstSha = commits[0]?.fullSha;
      commits = newCommits;
      
      // Flash if new commits arrived
      if (hadCommits && commits[0]?.fullSha !== oldFirstSha) {
        hue = 120; // Flash green for new commit
      }
    }
    
    // Fetch stats for visible commits
    newCommits.slice(0, 10).forEach(c => fetchCommitStats(c.fullSha));
    
    lastFetch = Date.now();
    loading = false;
    loadingMore = false;
    error = null;
    needsLayout = true;
    currentPage = page;
    
    // Track when commits first loaded for auto-scroll delay
    if (page === 1 && !append && loadTime === 0) {
      loadTime = Date.now();
    }
  } catch (err) {
    error = err.message;
    loading = false;
    loadingMore = false;
    console.error("Failed to fetch commits:", err);
  }
}

// Load more commits when scrolling near bottom
async function loadMoreIfNeeded() {
  if (loadingMore || !hasMoreCommits) return;
  
  const scrollNearBottom = scroll > totalScrollHeight - chatHeight - 200;
  if (scrollNearBottom) {
    await fetchCommits(currentPage + 1, true);
  }
}

function boot({ screen, store }) {
  // Initial fetch
  fetchCommits();
  
  // Start polling for new commits
  pollTimer = setInterval(() => fetchCommits(1, false), POLL_INTERVAL);
  
  // Always start at top with fresh state
  scroll = 0;
  autoScroll = false;
  loadTime = 0;
  frameCount = 0;
}

// Draw decorative elements
function drawDecor(ink, line, box, w, h, phase) {
  // Subtle corner decorations
  const cornerSize = 6;
  const c = [40, 45, 60, 150 + sin(phase) * 30];
  
  // Top-left corner
  ink(...c).line(0, cornerSize, 0, 0);
  ink(...c).line(0, 0, cornerSize, 0);
  
  // Top-right corner
  ink(...c).line(w - cornerSize, 0, w - 1, 0);
  ink(...c).line(w - 1, 0, w - 1, cornerSize);
  
  // Bottom-left corner  
  ink(...c).line(0, h - cornerSize, 0, h - 1);
  ink(...c).line(0, h - 1, cornerSize, h - 1);
  
  // Bottom-right corner
  ink(...c).line(w - cornerSize, h - 1, w - 1, h - 1);
  ink(...c).line(w - 1, h - cornerSize, w - 1, h - 1);
}

// Render stats bars with smooth animation
function drawStats(ink, box, x, y, stats, w) {
  if (!stats) return y;
  
  const { addW, delW } = statsBar(stats.additions, stats.deletions, min(w - 60, 40));
  const barY = y;
  const barH = 4;
  
  // Stats bar background
  ink(25, 28, 35).box(x, barY, addW + delW + 2, barH);
  
  // Additions bar (green)
  if (addW > 0) {
    ink(...COLORS.additions).box(x, barY, addW, barH);
  }
  
  // Deletions bar (red)
  if (delW > 0) {
    ink(...COLORS.deletions).box(x + addW, barY, delW, barH);
  }
  
  return barY + barH + 2;
}

function paint({ wipe, ink, screen, line, text, box, typeface, num, needsPaint, mask, unmask }) {
  const { width: w, height: h } = screen;
  frameCount++;
  pulsePhase += 0.05;
  
  // Rich dark background with subtle gradient simulation
  const bgPulse = sin(pulsePhase * 0.3) * 2;
  wipe(COLORS.bg[0] + bgPulse, COLORS.bg[1] + bgPulse, COLORS.bg[2] + bgPulse);
  
  // Draw decorative corner elements
  drawDecor(ink, line, box, w, h, pulsePhase);

  // Header: title left, GitHub link right
  const headerY = 8;
  ink(200, 200, 220).write("Commits", { x: 4, y: headerY }, false, undefined, false, FONT);
  const ghText = "GitHub →";
  const ghTextW = text.width(ghText, FONT);
  const ghX = w - ghTextW - 4;
  const ghGlow = 150 + sin(pulsePhase * 1.5) * 40;
  ink(100, 140, 255, ghGlow).write(ghText, { x: ghX, y: headerY }, false, undefined, false, FONT);
  githubLinkBox = { x: ghX - 2, y: headerY - 2, w: ghTextW + 4, h: 12 };

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
    // Animated loading indicator
    const dots = ".".repeat((floor(frameCount / 10) % 4));
    const loadText = `Loading commits${dots}`;
    ink(150, 150, 180).write(loadText, { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    needsPaint();
    return;
  }
  
  if (error && commits.length === 0) {
    ink(255, 100, 100).write("Error: " + error.slice(0, 40), { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }
  
  // Calculate heights - expanded view with stats
  chatHeight = h - topMargin - bottomMargin;
  const baseCommitHeight = rowHeight * 2 + 4; // Add padding between commits
  const expandedCommitHeight = rowHeight * 5 + 6; // Extra space for stats + generous padding
  const commitHeight = showDetailedView ? expandedCommitHeight : baseCommitHeight;
  const yearMarkerHeight = 18; // More prominent year markers
  const monthMarkerHeight = 14;
  const smallMarkerHeight = rowHeight + 4;
  
  // Calculate total height including timeline markers
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
  
  // Mask off the scrollable area
  mask({
    x: 0,
    y: topMargin,
    width: w,
    height: chatHeight,
  });
  
  // Fetch stats for visible commits
  let visibleStart = floor(scroll / commitHeight);
  let visibleEnd = ceil((scroll + chatHeight) / commitHeight);
  for (let i = max(0, visibleStart - 2); i < min(commits.length, visibleEnd + 2); i++) {
    const c = commits[i];
    if (c && !statsCache.has(c.fullSha) && !statsFetching.has(c.fullSha)) {
      fetchCommitStats(c.fullSha);
    }
  }
  
  // Draw commits with timeline markers
  let y = topMargin - scroll;
  
  for (let i = 0; i < commits.length; i++) {
    const commit = commits[i];
    const prevDate = i > 0 ? commits[i - 1].date : null;
    const marker = getTimelineMarker(commit.date, prevDate);
    const stats = statsCache.get(commit.fullSha);
    
    // Timeline marker
    if (marker) {
      let markerH = smallMarkerHeight;
      
      if (marker.type === "year") {
        markerH = yearMarkerHeight;
      } else if (marker.type === "month") {
        markerH = monthMarkerHeight;
      }
      
      if (y + markerH >= topMargin - markerH && y < h - bottomMargin + markerH) {
        // Background for marker with gradient
        const bgAlpha = marker.type === "year" ? 220 : (marker.type === "month" ? 180 : 100);
        ink(30, 28, 45, bgAlpha).box(0, y, w, markerH);
        
        // Marker text
        const textY = y + Math.floor((markerH - 8) / 2);
        
        if (marker.type === "year") {
          // Year: centered, ornate
          const yearGlow = 180 + sin(pulsePhase * 2) * 40;
          ink(marker.color[0], marker.color[1], marker.color[2], yearGlow).write(
            marker.label, { center: "x", x: w / 2, y: textY }, false, undefined, false, FONT
          );
        } else if (marker.type === "month") {
          // Month: left aligned with accent line
          ink(...marker.color).write(marker.label, { x: 4, y: textY }, false, undefined, false, FONT);
          // Accent line
          const labelW = text.width(marker.label, FONT) + 8;
          ink(marker.color[0], marker.color[1], marker.color[2], 60).line(labelW, y + markerH / 2, w - 4, y + markerH / 2);
        } else {
          // Week/Day: subtle with dot
          ink(marker.color[0], marker.color[1], marker.color[2], 180).write(
            marker.label, { x: 4, y: textY }, false, undefined, false, FONT
          );
        }
      }
      y += markerH;
    }
    
    // Skip if outside visible area (with buffer)
    if (y + commitHeight < topMargin - 20) {
      y += commitHeight;
      continue;
    }
    if (y > h - bottomMargin + 20) {
      y += commitHeight;
      continue;
    }
    
    // Commit row background (alternating subtle stripes with inner padding)
    const rowPadding = 3;
    if (i % 2 === 0) {
      ink(18, 20, 28, 100).box(0, y + rowPadding, w, commitHeight - rowPadding * 2);
    }
    
    // Subtle separator line between commits
    ink(40, 45, 60, 40).line(4, y + commitHeight - 1, w - 4, y + commitHeight - 1);
    
    // Row 1: SHA, time, author (with vertical offset for padding)
    const contentY = y + 4; // Top padding within commit block
    const isNew = i === 0 && hue > 60;
    const shaPulse = isNew ? 200 + sin(pulsePhase * 4) * 55 : 0;
    const shaColor = isNew ? [150 + shaPulse * 0.4, 255, 150 + shaPulse * 0.2] : COLORS.sha;
    
    // SHA with subtle box
    ink(30, 32, 42).box(2, contentY, 34, rowHeight);
    ink(...shaColor).write(commit.sha, { x: 4, y: contentY + 1 }, false, undefined, false, FONT);
    
    // Merge indicator
    let xOffset = 38;
    if (commit.parents > 1) {
      ink(180, 140, 200).write("⊕", { x: xOffset, y: contentY + 1 }, false, undefined, false, FONT);
      xOffset += 10;
    }
    
    // Time ago
    const ago = timeAgo(commit.date);
    ink(...COLORS.time).write(ago, { x: xOffset, y: contentY + 1 }, false, undefined, false, FONT);
    xOffset += text.width(ago + "  ", FONT); // Extra space
    
    // Author (truncate to fit)
    const author = "@" + commit.author.split(" ")[0].toLowerCase().slice(0, 12);
    ink(...COLORS.author).write(author, { x: xOffset, y: contentY + 1 }, false, undefined, false, FONT);
    
    // Row 2: Message (with extra line spacing)
    const msgY = contentY + rowHeight + 2;
    const charWidth = 4;
    const maxChars = Math.floor((w - 8) / charWidth);
    const msg = commit.message.slice(0, maxChars);
    ink(...COLORS.message).write(msg, { x: 4, y: msgY }, false, undefined, false, FONT);
    
    // Row 3-4: Stats (if detailed view and stats loaded)
    if (showDetailedView) {
      const statsY = contentY + rowHeight * 2 + 4; // Extra spacing before stats
      
      if (stats) {
        // Stats line: +additions -deletions files
        let sx = 4;
        
        // Additions
        const addText = `+${formatNum(stats.additions)}`;
        ink(...COLORS.additions).write(addText, { x: sx, y: statsY }, false, undefined, false, FONT);
        sx += text.width(addText + " ", FONT);
        
        // Deletions
        const delText = `-${formatNum(stats.deletions)}`;
        ink(...COLORS.deletions).write(delText, { x: sx, y: statsY }, false, undefined, false, FONT);
        sx += text.width(delText + " ", FONT);
        
        // Files changed
        const filesText = `${stats.files}f`;
        ink(...COLORS.files).write(filesText, { x: sx, y: statsY }, false, undefined, false, FONT);
        sx += text.width(filesText + " ", FONT);
        
        // Mini bar chart
        const { addW, delW } = statsBar(stats.additions, stats.deletions, 35);
        if (addW > 0) {
          ink(...COLORS.additions, 180).box(sx, statsY + 2, addW, 4);
        }
        if (delW > 0) {
          ink(...COLORS.deletions, 180).box(sx + addW, statsY + 2, delW, 4);
        }
        
        // File names preview (Row 4)
        if (stats.fileList?.length > 0) {
          const fileY = statsY + rowHeight + 2; // Extra spacing for file list
          let fx = 4;
          const maxFileWidth = w - 8;
          
          for (const f of stats.fileList.slice(0, 3)) {
            const statusChar = f.status === "added" ? "+" : (f.status === "removed" ? "-" : "~");
            const statusColor = f.status === "added" ? COLORS.additions : 
                               (f.status === "removed" ? COLORS.deletions : COLORS.files);
            
            const fileStr = `${statusChar}${f.name.slice(0, 12)}`;
            if (fx + text.width(fileStr, FONT) > maxFileWidth) break;
            
            ink(...statusColor, 150).write(fileStr, { x: fx, y: fileY }, false, undefined, false, FONT);
            fx += text.width(fileStr + " ", FONT);
          }
        }
      } else if (statsFetching.has(commit.fullSha)) {
        // Loading indicator for stats
        const loadDots = ".".repeat((floor(frameCount / 8) % 4));
        ink(80, 80, 100).write(`loading${loadDots}`, { x: 4, y: statsY }, false, undefined, false, FONT);
      } else {
        // Placeholder
        ink(50, 50, 60).write("···", { x: 4, y: statsY }, false, undefined, false, FONT);
      }
    }
    
    // Subtle separator with gradient
    const sepY = y + commitHeight - 1;
    for (let sx = 4; sx < w - 4; sx++) {
      const alpha = 20 + sin(sx * 0.1) * 10;
      ink(35, 38, 50, alpha).box(sx, sepY, 1, 1);
    }
    
    y += commitHeight;
  }
  
  // Loading more indicator at bottom
  if (loadingMore) {
    const loadDots = ".".repeat((floor(frameCount / 10) % 4));
    ink(150, 150, 180).write(`Loading more${loadDots}`, { x: 4, y: h - bottomMargin - rowHeight - 4 }, false, undefined, false, FONT);
  }
  
  unmask(); // End masking
  
  // 📜 Scroll bar (ornate version)
  if (totalScrollHeight > chatHeight) {
    // Track
    ink(25, 28, 35).box(w - 4, topMargin, 3, chatHeight);
    
    // Decorative track edges
    ink(40, 45, 55).line(w - 5, topMargin, w - 5, topMargin + chatHeight);
    ink(40, 45, 55).line(w - 1, topMargin, w - 1, topMargin + chatHeight);
    
    const segHeight = max(8, floor((chatHeight / totalScrollHeight) * chatHeight));
    const scrollRatio = scroll / max(1, totalScrollHeight - chatHeight);
    const boxY = topMargin + floor(scrollRatio * (chatHeight - segHeight));
    
    // Thumb with glow
    const thumbColor = autoScroll ? COLORS.additions : [200, 150, 255];
    const thumbGlow = 50 + sin(pulsePhase * 2) * 30;
    ink(thumbColor[0], thumbColor[1], thumbColor[2], thumbGlow).box(w - 6, boxY - 1, 7, segHeight + 2);
    ink(...thumbColor).box(w - 4, boxY, 3, segHeight);
  }
  
  // ═══════════════════════════════════════════════════════════
  // Footer area (ornate status bar)
  // ═══════════════════════════════════════════════════════════
  const footerY = h - bottomMargin + 3;
  const sinceLastFetch = Date.now() - lastFetch;
  const nextPoll = Math.max(0, Math.ceil((POLL_INTERVAL - sinceLastFetch) / 1000));
  
  // Auto-scroll delay progress bar
  const sinceLoad = Date.now() - loadTime;
  const delayProgress = loadTime > 0 ? Math.min(1, sinceLoad / autoScrollDelay) : 0;
  const waitingToScroll = loadTime > 0 && !autoScroll && delayProgress < 1;
  
  // Left section: playback state
  if (waitingToScroll) {
    // Progress bar during delay
    const barWidth = 24;
    const barX = 4;
    ink(30, 32, 40).box(barX, footerY, barWidth, 7);
    ink(100, 200, 150).box(barX, footerY, Math.floor(barWidth * delayProgress), 7);
    ink(60, 65, 80).box(barX, footerY, barWidth, 7, "outline");
  } else {
    // Auto-scroll indicator with icon
    const playIcon = autoScroll ? "►" : "║║";
    const playColor = autoScroll ? COLORS.additions : [100, 100, 120];
    ink(...playColor).write(playIcon, { x: 4, y: footerY }, false, undefined, false, FONT);
  }
  
  // Poll countdown with subtle animation
  const pollAlpha = 150 + sin(pulsePhase + nextPoll * 0.2) * 50;
  ink(80, 85, 105, pollAlpha).write(`${nextPoll}s`, { x: waitingToScroll ? 32 : 20, y: footerY }, false, undefined, false, FONT);
  
  // Center: view toggle hint
  const toggleHint = showDetailedView ? "[d]etail" : "[d]etail";
  const hintX = floor(w / 2) - floor(text.width(toggleHint, FONT) / 2);
  ink(60, 65, 80).write(toggleHint, { x: hintX, y: footerY }, false, undefined, false, FONT);
  
  // Right section: commit count
  const countText = hasMoreCommits ? `${commits.length}+` : `${commits.length}`;
  const countX = w - text.width(countText, FONT) - 4;
  ink(100, 105, 130).write(countText, { x: countX, y: footerY }, false, undefined, false, FONT);
  
  // Stats cache indicator
  const cacheText = `${statsCache.size}★`;
  ink(60, 80, 60).write(cacheText, { x: countX - text.width(cacheText + " ", FONT), y: footerY }, false, undefined, false, FONT);
  
  // Keep painting for animations
  if (autoScroll || waitingToScroll || statsFetching.size > 0) needsPaint();
}

function act({ event: e, screen, store, jump }) {
  const { height: h } = screen;
  
  // 📜 Scrolling - any manual scroll disables auto-scroll
  if (e.is("draw")) {
    autoScroll = false;
    scroll -= e.delta.y; // Invert for natural scroll direction
    boundScroll();
    loadMoreIfNeeded();
  }
  
  // Keyboard controls
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    autoScroll = false;
    scroll -= rowHeight * 4;
    boundScroll();
  }
  
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    autoScroll = false;
    scroll += rowHeight * 4;
    boundScroll();
    loadMoreIfNeeded();
  }
  
  if (e.is("keyboard:down:home")) {
    autoScroll = false;
    scroll = 0;
  }
  
  if (e.is("keyboard:down:end")) {
    autoScroll = false;
    scroll = max(0, totalScrollHeight - chatHeight + 5);
    loadMoreIfNeeded();
  }
  
  // Page up/down
  if (e.is("keyboard:down:pageup")) {
    autoScroll = false;
    scroll -= chatHeight * 0.8;
    boundScroll();
  }
  
  if (e.is("keyboard:down:pagedown")) {
    autoScroll = false;
    scroll += chatHeight * 0.8;
    boundScroll();
    loadMoreIfNeeded();
  }
  
  // Toggle auto-scroll with Space
  if (e.is("keyboard:down: ")) {
    autoScroll = !autoScroll;
    if (autoScroll) scroll = 0; // Reset to top when enabling
  }
  
  // Toggle detailed view with D
  if (e.is("keyboard:down:d")) {
    showDetailedView = !showDetailedView;
    needsLayout = true;
  }
  
  // Refresh on R
  if (e.is("keyboard:down:r")) {
    statsCache.clear();
    fetchCommits(1, false);
  }
  
  // GitHub link click
  if (e.is("touch") && githubLinkBox) {
    const { x, y } = e;
    if (x >= githubLinkBox.x && x <= githubLinkBox.x + githubLinkBox.w &&
        y >= githubLinkBox.y && y <= githubLinkBox.y + githubLinkBox.h) {
      jump(`out:https://github.com/${REPO}`);
    }
  }

  // Back to prompt
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

function sim({ store }) {
  // Decay the new-commit flash
  if (hue > 0) hue = Math.max(0, hue - 0.5);
  
  // Auto-start scrolling after delay
  const sinceLoad = Date.now() - loadTime;
  if (loadTime > 0 && !autoScroll && sinceLoad >= autoScrollDelay && scroll === 0) {
    autoScroll = true;
  }
  
  // Auto-scroll
  if (autoScroll && totalScrollHeight > chatHeight) {
    scroll += autoScrollSpeed;
    boundScroll();
    
    // Loop back to top when reaching end
    if (scroll >= totalScrollHeight - chatHeight) {
      scroll = 0;
    }
    
    loadMoreIfNeeded();
  }
}

function leave() {
  // Clean up polling
  if (pollTimer) {
    clearInterval(pollTimer);
    pollTimer = null;
  }
  // Clear caches
  statsCache.clear();
  statsFetching.clear();
}

function meta() {
  return {
    title: "Commits",
    desc: "╔═══════════════════════════════════════╗\n║ Live GitHub commit feed with stats    ║\n║ +additions -deletions • files changed ║\n╚═══════════════════════════════════════╝",
  };
}

export { boot, paint, act, sim, leave, meta };
