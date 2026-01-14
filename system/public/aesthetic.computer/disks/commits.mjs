// commits, 2025.1.14
// Live GitHub commit feed for aesthetic-computer

const { max, floor, ceil } = Math;

const REPO = "whistlegraph/aesthetic-computer";
const POLL_INTERVAL = 30000; // 30 seconds
const COMMITS_PER_PAGE = 100;

let commits = [];
let scroll = 0;
let totalScrollHeight = 0;
let chatHeight = 0;
let loading = true;
let loadingMore = false;
let error = null;
let lastFetch = 0;
let pollTimer = null;
let rowHeight = 9; // MatrixChunky8 is 8px + 1px spacing
let topMargin = 19; // Below HUD label
let bottomMargin = 12; // Footer area
let hue = 0;
let needsLayout = true;
let autoScroll = false; // Start paused
let autoScrollDelay = 2000; // 2 second delay before auto-scroll
let loadTime = 0; // When commits first loaded
let autoScrollSpeed = 0.3;
let currentPage = 1;
let hasMoreCommits = true;
const FONT = "MatrixChunky8";

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

// Bound scroll like chat.mjs does
function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
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
    return { type: "year", label: `${d.getFullYear()}`, color: [255, 200, 100] };
  }
  
  // Check for month change
  if (d.getMonth() !== prev.getMonth()) {
    return { type: "month", label: `${months[d.getMonth()]}`, color: [180, 160, 220] };
  }
  
  // Check for week change (Sunday boundary)
  const weekOfYear = (date) => {
    const start = new Date(date.getFullYear(), 0, 1);
    return Math.ceil(((date - start) / 86400000 + start.getDay() + 1) / 7);
  };
  if (weekOfYear(d) !== weekOfYear(prev)) {
    return { type: "week", label: `Week ${weekOfYear(d)}`, color: [140, 180, 140] };
  }
  
  // Check for day change
  if (d.getDate() !== prev.getDate()) {
    return { type: "day", label: `${days[d.getDay()]} ${d.getDate()}`, color: [120, 140, 160] };
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
      author: c.commit.author.name,
      date: c.commit.author.date,
      avatar: c.author?.avatar_url,
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
}

function paint({ wipe, ink, screen, line, text, typeface, num, needsPaint, mask, unmask }) {
  const { width: w, height: h } = screen;
  
  // Background with subtle hue shift
  hue = (hue + 0.1) % 360;
  const bgHue = hue * 0.1;
  wipe(12 + Math.sin(bgHue) * 2, 12, 20);
  
  // Top divider line (below HUD label area)
  ink(50, 50, 70).line(0, topMargin - 1, w, topMargin - 1);
  
  // Bottom divider line (above footer)
  ink(50, 50, 70).line(0, h - bottomMargin, w, h - bottomMargin);
  
  if (loading && commits.length === 0) {
    ink(150, 150, 180).write("Loading commits...", { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }
  
  if (error && commits.length === 0) {
    ink(255, 100, 100).write("Error: " + error.slice(0, 40), { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }
  
  // Calculate heights
  chatHeight = h - topMargin - bottomMargin;
  const commitHeight = rowHeight * 2; // Each commit takes 2 rows
  const yearMarkerHeight = 12; // Bigger for years
  const monthMarkerHeight = 10; // Medium for months
  const smallMarkerHeight = rowHeight; // Small for weeks/days
  
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
  
  // Mask off the scrollable area (like chat.mjs)
  mask({
    x: 0,
    y: topMargin,
    width: w,
    height: chatHeight,
  });
  
  // Draw commits with timeline markers
  let y = topMargin - scroll;
  
  for (let i = 0; i < commits.length; i++) {
    const commit = commits[i];
    const prevDate = i > 0 ? commits[i - 1].date : null;
    const marker = getTimelineMarker(commit.date, prevDate);
    
    // Timeline marker
    if (marker) {
      let markerH = smallMarkerHeight;
      
      if (marker.type === "year") {
        markerH = yearMarkerHeight;
      } else if (marker.type === "month") {
        markerH = monthMarkerHeight;
      }
      
      if (y + markerH >= topMargin - markerH && y < h - bottomMargin + markerH) {
        // Background for marker
        const bgAlpha = marker.type === "year" ? 200 : (marker.type === "month" ? 150 : 80);
        ink(40, 35, 60, bgAlpha).box(0, y, w, markerH);
        
        // Marker text - use default font for year/month, MatrixChunky8 for week/day
        const useDefaultFont = marker.type === "year" || marker.type === "month";
        const textY = y + Math.floor((markerH - (useDefaultFont ? 10 : 8)) / 2);
        
        if (marker.type === "year") {
          // Year: centered, default font
          ink(...marker.color).write(`- ${marker.label} -`, { center: "x", x: w / 2, y: textY });
        } else if (marker.type === "month") {
          // Month: left aligned, default font
          ink(...marker.color).write(`- ${marker.label}`, { x: 4, y: textY });
        } else {
          // Week/Day: subtle, MatrixChunky8
          ink(...marker.color, 180).write(`${marker.label}`, { x: 4, y: textY }, false, undefined, false, FONT);
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
    
    // Commit SHA with color based on recency
    const isNew = i === 0 && hue > 60;
    const shaColor = isNew ? [150, 255, 150] : [255, 180, 100];
    ink(...shaColor).write(commit.sha, { x: 4, y }, false, undefined, false, FONT);
    
    // Time ago
    const ago = timeAgo(commit.date);
    const agoX = 4 + text.width(commit.sha + " ", FONT);
    ink(100, 100, 130).write(ago, { x: agoX, y }, false, undefined, false, FONT);
    
    // Author (truncate to fit)
    const authorX = agoX + text.width(ago + " ", FONT);
    const author = "@" + commit.author.split(" ")[0].toLowerCase().slice(0, 10);
    ink(180, 150, 255).write(author, { x: authorX, y }, false, undefined, false, FONT);
    
    // Message on second line
    const msgY = y + rowHeight;
    const charWidth = 4;
    const maxChars = Math.floor((w - 8) / charWidth);
    const msg = commit.message.slice(0, maxChars);
    ink(200, 200, 220).write(msg, { x: 4, y: msgY }, false, undefined, false, FONT);
    
    // Subtle separator
    const sepY = y + commitHeight - 1;
    ink(30, 30, 42).line(4, sepY, w - 4, sepY);
    
    y += commitHeight;
  }
  
  // Loading more indicator at bottom
  if (loadingMore) {
    ink(150, 150, 180).write("Loading more...", { x: 4, y: h - bottomMargin - rowHeight - 2 }, false, undefined, false, FONT);
  }
  
  unmask(); // End masking
  
  // 📜 Scroll bar (outside mask so it's always visible)
  if (totalScrollHeight > chatHeight) {
    ink(40, 40, 50).box(w - 2, topMargin, 2, chatHeight); // Backdrop
    
    const segHeight = max(4, floor((chatHeight / totalScrollHeight) * chatHeight));
    const scrollRatio = scroll / max(1, totalScrollHeight - chatHeight);
    const boxY = topMargin + floor(scrollRatio * (chatHeight - segHeight));
    
    ink(autoScroll ? [100, 255, 150] : [255, 150, 200]).box(w - 2, boxY, 2, segHeight);
  }
  
  // Footer area (below mask)
  const footerY = h - bottomMargin + 2;
  const sinceLastFetch = Date.now() - lastFetch;
  const nextPoll = Math.max(0, Math.ceil((POLL_INTERVAL - sinceLastFetch) / 1000));
  
  // Auto-scroll delay progress bar
  const sinceLoad = Date.now() - loadTime;
  const delayProgress = loadTime > 0 ? Math.min(1, sinceLoad / autoScrollDelay) : 0;
  const waitingToScroll = loadTime > 0 && !autoScroll && delayProgress < 1;
  
  if (waitingToScroll) {
    // Show progress bar during delay
    const barWidth = 40;
    const barX = 4;
    ink(40, 40, 50).box(barX, footerY, barWidth, 7);
    ink(100, 200, 150).box(barX, footerY, Math.floor(barWidth * delayProgress), 7);
  } else {
    // Auto-scroll indicator
    const autoLabel = autoScroll ? ">" : "||";
    ink(autoScroll ? [100, 255, 150] : [100, 100, 120]).write(autoLabel, { x: 4, y: footerY }, false, undefined, false, FONT);
  }
  
  // Poll countdown
  ink(80, 80, 100).write(`${nextPoll}s`, { x: waitingToScroll ? 50 : 16, y: footerY }, false, undefined, false, FONT);
  
  // Commit count / loading status
  const countText = hasMoreCommits ? `${commits.length}+` : `${commits.length}`;
  ink(80, 80, 100).write(countText, { x: w - text.width(countText, FONT) - 4, y: footerY }, false, undefined, false, FONT);
  
  // Keep painting for auto-scroll or delay progress
  if (autoScroll || waitingToScroll) needsPaint();
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
    scroll -= rowHeight * 2;
    boundScroll();
  }
  
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    autoScroll = false;
    scroll += rowHeight * 2;
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
  
  // Toggle auto-scroll with Space
  if (e.is("keyboard:down: ")) {
    autoScroll = !autoScroll;
    if (autoScroll) scroll = 0; // Reset to top when enabling
  }
  
  // Refresh on R
  if (e.is("keyboard:down:r")) {
    fetchCommits(1, false);
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
}

function meta() {
  return {
    title: "Commits",
    desc: "Live GitHub commit feed for aesthetic-computer",
  };
}

export { boot, paint, act, sim, leave, meta };
