// handles, 2025.2.3
// Browse all user handles in a scrollable list
// ╔═══════════════════════════════════════════════════════════╗
// ║  A typographically styled user handle directory          ║
// ╚═══════════════════════════════════════════════════════════╝

const { max, min, floor, ceil, abs, sin, cos, PI } = Math;

const POLL_INTERVAL = 60000; // 60 seconds (handles change less frequently)
const HANDLES_PER_PAGE = 100;

let handles = [];
let scroll = 0;
let totalScrollHeight = 0;
let listHeight = 0;
let loading = true;
let loadingMore = false;
let error = null;
let lastFetch = 0;
let pollTimer = null;
let rowHeight = 12; // MatrixChunky8 is 8px + 4px spacing
let topMargin = 24; // Below HUD label
let bottomMargin = 16; // Footer area
let hue = 0;
let pulsePhase = 0;
let needsLayout = true;
let autoScroll = false; // Start paused
let autoScrollDelay = 2000; // 2 second delay before auto-scroll
let loadTime = 0;
let autoScrollSpeed = 0.3;
let searchQuery = "";
let frameCount = 0;
let hoveredHandle = null;
let totalCount = 0;

// Visual theming (matching commits.mjs style)
const FONT = "MatrixChunky8";
const COLORS = {
  bg: [10, 12, 18],
  bgAccent: [16, 18, 26],
  line: [35, 40, 55],
  handle: [255, 150, 255], // Magenta/pink for handles
  handleAlt: [200, 120, 255], // Purple variant
  index: [100, 120, 140],
  count: [180, 180, 200],
};

// Bound scroll like chat.mjs does
function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - listHeight + 5) {
    scroll = totalScrollHeight - listHeight + 5;
  }
}

// Fetch handles from API
async function fetchHandles(append = false) {
  try {
    if (!append) loading = handles.length === 0;
    else loadingMore = true;
    
    const searchParam = searchQuery ? `&search=${encodeURIComponent(searchQuery)}` : "";
    const response = await fetch(
      `/handles?limit=${HANDLES_PER_PAGE}${searchParam}`
    );
    
    if (!response.ok) {
      throw new Error(`API error: ${response.status}`);
    }
    
    const data = await response.json();
    
    handles = data.handles || [];
    totalCount = data.count || handles.length;
    
    lastFetch = Date.now();
    loading = false;
    loadingMore = false;
    error = null;
    needsLayout = true;
    
    // Track when handles first loaded for auto-scroll delay
    if (loadTime === 0) {
      loadTime = Date.now();
    }
  } catch (err) {
    error = err.message;
    loading = false;
    loadingMore = false;
    console.error("Failed to fetch handles:", err);
  }
}

function boot({ screen, store }) {
  // Initial fetch
  fetchHandles();
  
  // Start polling for new handles
  pollTimer = setInterval(() => fetchHandles(false), POLL_INTERVAL);
  
  // Always start at top with fresh state
  scroll = 0;
  autoScroll = false;
  loadTime = 0;
  frameCount = 0;
}

// Draw decorative elements
function drawDecor(ink, line, box, w, h, phase) {
  // Subtle corner decorations
  const cornerSize = 5;
  const c = [100, 60, 130, 120 + sin(phase) * 30];
  
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

function paint({ wipe, ink, screen, line, text, box, typeface, num, needsPaint, mask, unmask, jump }) {
  const { width: w, height: h } = screen;
  frameCount++;
  pulsePhase += 0.05;
  
  // Rich dark background
  const bgPulse = sin(pulsePhase * 0.3) * 2;
  wipe(COLORS.bg[0] + bgPulse, COLORS.bg[1] + bgPulse, COLORS.bg[2] + bgPulse);
  
  // Draw decorative corner elements
  drawDecor(ink, line, box, w, h, pulsePhase);
  
  // Top divider line with gradient effect
  const topLineY = topMargin - 1;
  for (let i = 0; i < w; i++) {
    const alpha = 40 + sin(i * 0.02 + pulsePhase) * 15;
    ink(80, 50, 100, alpha).box(i, topLineY, 1, 1);
  }
  
  // Bottom divider line
  const botLineY = h - bottomMargin;
  for (let i = 0; i < w; i++) {
    const alpha = 40 + sin(i * 0.02 - pulsePhase) * 15;
    ink(80, 50, 100, alpha).box(i, botLineY, 1, 1);
  }
  
  if (loading && handles.length === 0) {
    // Animated loading indicator
    const dots = ".".repeat((floor(frameCount / 10) % 4));
    const loadText = `Loading handles${dots}`;
    ink(200, 150, 220).write(loadText, { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    needsPaint();
    return;
  }
  
  if (error && handles.length === 0) {
    ink(255, 100, 100).write("Error: " + error.slice(0, 40), { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }
  
  // Calculate heights
  listHeight = h - topMargin - bottomMargin;
  
  // Calculate total height
  if (needsLayout) {
    totalScrollHeight = handles.length * rowHeight;
    needsLayout = false;
  }
  
  // Mask off the scrollable area
  mask({
    x: 0,
    y: topMargin,
    width: w,
    height: listHeight,
  });
  
  // Draw handles
  let y = topMargin - scroll;
  
  for (let i = 0; i < handles.length; i++) {
    const handle = handles[i];
    
    // Skip if outside visible area (with buffer)
    if (y + rowHeight < topMargin - 10) {
      y += rowHeight;
      continue;
    }
    if (y > h - bottomMargin + 10) {
      y += rowHeight;
      continue;
    }
    
    // Row background (alternating subtle stripes)
    if (i % 2 === 0) {
      ink(18, 14, 24, 100).box(0, y, w, rowHeight);
    }
    
    // Index number (subtle)
    const indexText = `${i + 1}.`;
    const indexWidth = text.width(indexText, FONT);
    ink(...COLORS.index, 150).write(indexText, { x: 4, y: y + 2 }, false, undefined, false, FONT);
    
    // Handle with @ prefix
    const handleText = `@${handle}`;
    const useAltColor = i % 3 === 0;
    const handleColor = useAltColor ? COLORS.handleAlt : COLORS.handle;
    const handlePulse = sin(pulsePhase + i * 0.3) * 20;
    ink(handleColor[0] + handlePulse, handleColor[1], handleColor[2]).write(
      handleText, { x: 24, y: y + 2 }, false, undefined, false, FONT
    );
    
    // Subtle separator
    if (i < handles.length - 1) {
      ink(40, 35, 50, 40).line(24, y + rowHeight - 1, w - 8, y + rowHeight - 1);
    }
    
    y += rowHeight;
  }
  
  unmask(); // End masking
  
  // 📜 Scroll bar
  if (totalScrollHeight > listHeight) {
    // Track
    ink(25, 20, 35).box(w - 4, topMargin, 3, listHeight);
    
    // Decorative track edges
    ink(50, 40, 60).line(w - 5, topMargin, w - 5, topMargin + listHeight);
    ink(50, 40, 60).line(w - 1, topMargin, w - 1, topMargin + listHeight);
    
    const segHeight = max(8, floor((listHeight / totalScrollHeight) * listHeight));
    const scrollRatio = scroll / max(1, totalScrollHeight - listHeight);
    const boxY = topMargin + floor(scrollRatio * (listHeight - segHeight));
    
    // Thumb with glow
    const thumbColor = autoScroll ? [150, 255, 150] : [220, 150, 255];
    const thumbGlow = 50 + sin(pulsePhase * 2) * 30;
    ink(thumbColor[0], thumbColor[1], thumbColor[2], thumbGlow).box(w - 6, boxY - 1, 7, segHeight + 2);
    ink(...thumbColor).box(w - 4, boxY, 3, segHeight);
  }
  
  // ═══════════════════════════════════════════════════════════
  // Footer area
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
    const barWidth = 24;
    const barX = 4;
    ink(30, 25, 40).box(barX, footerY, barWidth, 7);
    ink(180, 120, 220).box(barX, footerY, Math.floor(barWidth * delayProgress), 7);
    ink(80, 60, 100).box(barX, footerY, barWidth, 7, "outline");
  } else {
    const playIcon = autoScroll ? "►" : "║║";
    const playColor = autoScroll ? [150, 255, 150] : [100, 100, 120];
    ink(...playColor).write(playIcon, { x: 4, y: footerY }, false, undefined, false, FONT);
  }
  
  // Poll countdown
  const pollAlpha = 150 + sin(pulsePhase + nextPoll * 0.2) * 50;
  ink(100, 80, 120, pollAlpha).write(`${nextPoll}s`, { x: waitingToScroll ? 32 : 20, y: footerY }, false, undefined, false, FONT);
  
  // Right section: total count
  const countText = `${handles.length} of ${totalCount}`;
  const countX = w - text.width(countText, FONT) - 4;
  ink(...COLORS.count).write(countText, { x: countX, y: footerY }, false, undefined, false, FONT);
  
  // Keep painting for animations
  if (autoScroll || waitingToScroll) needsPaint();
}

function act({ event: e, screen, store, jump }) {
  const { height: h } = screen;
  
  // 📜 Scrolling - any manual scroll disables auto-scroll
  if (e.is("draw")) {
    autoScroll = false;
    scroll -= e.delta.y;
    boundScroll();
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
  }
  
  if (e.is("keyboard:down:home")) {
    autoScroll = false;
    scroll = 0;
  }
  
  if (e.is("keyboard:down:end")) {
    autoScroll = false;
    scroll = max(0, totalScrollHeight - listHeight + 5);
  }
  
  // Page up/down
  if (e.is("keyboard:down:pageup")) {
    autoScroll = false;
    scroll -= listHeight * 0.8;
    boundScroll();
  }
  
  if (e.is("keyboard:down:pagedown")) {
    autoScroll = false;
    scroll += listHeight * 0.8;
    boundScroll();
  }
  
  // Toggle auto-scroll with Space
  if (e.is("keyboard:down: ")) {
    autoScroll = !autoScroll;
    if (autoScroll) scroll = 0;
  }
  
  // Refresh on R
  if (e.is("keyboard:down:r")) {
    fetchHandles(false);
  }
  
  // Back to prompt
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

function sim({ store }) {
  // Decay the flash
  if (hue > 0) hue = Math.max(0, hue - 0.5);
  
  // Auto-start scrolling after delay
  const sinceLoad = Date.now() - loadTime;
  if (loadTime > 0 && !autoScroll && sinceLoad >= autoScrollDelay && scroll === 0) {
    autoScroll = true;
  }
  
  // Auto-scroll
  if (autoScroll && totalScrollHeight > listHeight) {
    scroll += autoScrollSpeed;
    boundScroll();
    
    // Loop back to top when reaching end
    if (scroll >= totalScrollHeight - listHeight) {
      scroll = 0;
    }
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
    title: "Handles",
    desc: "╔═══════════════════════════════════╗\n║ Browse all user handles           ║\n║ Scroll or auto-scroll through     ║\n╚═══════════════════════════════════╝",
  };
}

export { boot, paint, act, sim, leave, meta };
