// clocks, 2025.6.26
// Browse saved clocks from the database

const { max, floor } = Math;

const CLOCKS_PER_PAGE = 100;

let clocks = [];
let scroll = 0;
let totalScrollHeight = 0;
let chatHeight = 0;
let loading = true;
let error = null;
let rowHeight = 9; // MatrixChunky8 is 8px + 1px spacing
let topMargin = 19; // Below HUD label
let bottomMargin = 12; // Footer area
let hue = 0;
let needsLayout = true;
let sortBy = "recent"; // 'recent' or 'hits'
let selectedIndex = -1; // Currently highlighted clock
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

// Fetch clocks from API
async function fetchClocks(sort = "recent") {
  try {
    loading = clocks.length === 0;
    sortBy = sort;

    const response = await fetch(
      `/api/store-clock?recent=true&limit=${CLOCKS_PER_PAGE}&sort=${sort}`
    );

    if (!response.ok) {
      throw new Error(`API error: ${response.status}`);
    }

    const data = await response.json();
    clocks = data.recent || [];

    loading = false;
    error = null;
    needsLayout = true;
    selectedIndex = clocks.length > 0 ? 0 : -1;
  } catch (err) {
    error = err.message;
    loading = false;
    console.error("Failed to fetch clocks:", err);
  }
}

// Calculate Y position for a clock at given index
function getClockY(index) {
  const clockHeight = rowHeight * 2; // Each clock takes 2 rows
  return topMargin + index * clockHeight - scroll;
}

// Get clock index at Y position
function getClockAtY(y, screenHeight) {
  const clockHeight = rowHeight * 2;
  const relativeY = y - topMargin + scroll;
  const index = Math.floor(relativeY / clockHeight);
  if (index >= 0 && index < clocks.length) {
    const clockY = getClockY(index);
    // Check if within visible bounds
    if (clockY >= topMargin - clockHeight && clockY < screenHeight - bottomMargin + clockHeight) {
      return index;
    }
  }
  return -1;
}

function boot({ screen, store }) {
  // Initial fetch
  fetchClocks();

  // Always start at top with fresh state
  scroll = 0;
  selectedIndex = -1;
}

function paint({ wipe, ink, screen, line, text, typeface, num, needsPaint, mask, unmask, box }) {
  const { width: w, height: h } = screen;

  // Background with subtle hue shift
  hue = (hue + 0.1) % 360;
  const bgHue = hue * 0.1;
  wipe(12 + Math.sin(bgHue) * 2, 12, 20);

  // Top divider line (below HUD label area)
  ink(50, 50, 70).line(0, topMargin - 1, w, topMargin - 1);

  // Bottom divider line (above footer)
  ink(50, 50, 70).line(0, h - bottomMargin, w, h - bottomMargin);

  if (loading && clocks.length === 0) {
    ink(150, 150, 180).write("Loading clocks...", { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }

  if (error && clocks.length === 0) {
    ink(255, 100, 100).write("Error: " + error.slice(0, 40), { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }

  if (clocks.length === 0) {
    ink(150, 150, 180).write("No clocks found", { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }

  // Calculate heights
  chatHeight = h - topMargin - bottomMargin;
  const clockHeight = rowHeight * 2; // Each clock takes 2 rows

  // Calculate total height
  if (needsLayout) {
    totalScrollHeight = clocks.length * clockHeight;
    needsLayout = false;
  }

  // Mask off the scrollable area
  mask({
    x: 0,
    y: topMargin,
    width: w,
    height: chatHeight,
  });

  // Draw clocks
  for (let i = 0; i < clocks.length; i++) {
    const clock = clocks[i];
    const y = getClockY(i);

    // Skip if outside visible area (with buffer)
    if (y + clockHeight < topMargin - 20) continue;
    if (y > h - bottomMargin + 20) continue;

    // Selection highlight
    const isSelected = i === selectedIndex;
    if (isSelected) {
      ink(40, 40, 60, 200).box(0, y, w, clockHeight);
    }

    // Code with * prefix
    const codeColor = isSelected ? [255, 200, 100] : [200, 150, 80];
    ink(...codeColor).write(`*${clock.code}`, { x: 4, y }, false, undefined, false, FONT);

    // Time ago
    const ago = timeAgo(clock.when);
    const agoX = 4 + text.width(`*${clock.code} `, FONT);
    ink(100, 100, 130).write(ago, { x: agoX, y }, false, undefined, false, FONT);

    // Hits count
    const hitsX = agoX + text.width(ago + " ", FONT);
    ink(80, 130, 80).write(`${clock.hits}x`, { x: hitsX, y }, false, undefined, false, FONT);

    // Handle/author (if available)
    if (clock.handle) {
      const handleX = hitsX + text.width(`${clock.hits}x `, FONT);
      ink(180, 150, 255).write(clock.handle.slice(0, 12), { x: handleX, y }, false, undefined, false, FONT);
    }

    // Preview on second line (truncated)
    const msgY = y + rowHeight;
    const charWidth = 4;
    const maxChars = Math.floor((w - 8) / charWidth);
    const preview = clock.preview || clock.source?.slice(0, maxChars) || "";
    ink(200, 200, 220).write(preview.slice(0, maxChars), { x: 4, y: msgY }, false, undefined, false, FONT);

    // Subtle separator
    const sepY = y + clockHeight - 1;
    ink(30, 30, 42).line(4, sepY, w - 4, sepY);
  }

  unmask(); // End masking

  // 📜 Scroll bar (outside mask so it's always visible)
  if (totalScrollHeight > chatHeight) {
    ink(40, 40, 50).box(w - 2, topMargin, 2, chatHeight); // Backdrop

    const segHeight = max(4, floor((chatHeight / totalScrollHeight) * chatHeight));
    const scrollRatio = scroll / max(1, totalScrollHeight - chatHeight);
    const boxY = topMargin + floor(scrollRatio * (chatHeight - segHeight));

    ink(255, 150, 200).box(w - 2, boxY, 2, segHeight);
  }

  // Footer area (below mask)
  const footerY = h - bottomMargin + 2;

  // Sort indicator
  const sortLabel = sortBy === "hits" ? "🔥 popular" : "🕐 recent";
  ink(100, 100, 120).write(sortLabel, { x: 4, y: footerY }, false, undefined, false, FONT);

  // Clock count
  const countText = `${clocks.length}`;
  ink(80, 80, 100).write(countText, { x: w - text.width(countText, FONT) - 4, y: footerY }, false, undefined, false, FONT);

  // Instructions
  const helpText = "↑↓:nav ⏎:play s:sort";
  const helpX = Math.floor((w - text.width(helpText, FONT)) / 2);
  ink(60, 60, 80).write(helpText, { x: helpX, y: footerY }, false, undefined, false, FONT);
}

function act({ event: e, screen, store, jump }) {
  const { height: h } = screen;

  // 📜 Scrolling
  if (e.is("draw")) {
    scroll -= e.delta.y; // Invert for natural scroll direction
    boundScroll();
  }

  // Navigation up
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    if (selectedIndex > 0) {
      selectedIndex--;
      // Ensure selected item is visible
      const clockY = getClockY(selectedIndex);
      if (clockY < topMargin) {
        scroll = selectedIndex * rowHeight * 2;
      }
    }
    boundScroll();
  }

  // Navigation down
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    if (selectedIndex < clocks.length - 1) {
      selectedIndex++;
      // Ensure selected item is visible
      const clockY = getClockY(selectedIndex);
      const clockHeight = rowHeight * 2;
      if (clockY + clockHeight > h - bottomMargin) {
        scroll = (selectedIndex + 1) * clockHeight - (h - topMargin - bottomMargin);
      }
    }
    boundScroll();
  }

  // Home key - go to top
  if (e.is("keyboard:down:home")) {
    scroll = 0;
    selectedIndex = 0;
  }

  // End key - go to bottom
  if (e.is("keyboard:down:end")) {
    scroll = max(0, totalScrollHeight - chatHeight + 5);
    selectedIndex = clocks.length - 1;
  }

  // Enter - run selected clock
  if (e.is("keyboard:down:enter")) {
    if (selectedIndex >= 0 && selectedIndex < clocks.length) {
      const clock = clocks[selectedIndex];
      jump(`*${clock.code}`);
    }
  }

  // Click to select and run
  if (e.is("touch") && e.y >= topMargin && e.y < h - bottomMargin) {
    const clickedIndex = getClockAtY(e.y, h);
    if (clickedIndex >= 0) {
      const clock = clocks[clickedIndex];
      jump(`*${clock.code}`);
    }
  }

  // Toggle sort with S key
  if (e.is("keyboard:down:s")) {
    const newSort = sortBy === "recent" ? "hits" : "recent";
    fetchClocks(newSort);
    scroll = 0;
  }

  // Refresh on R
  if (e.is("keyboard:down:r")) {
    fetchClocks(sortBy);
    scroll = 0;
    selectedIndex = 0;
  }

  // Back to prompt
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

function sim({ store }) {
  // Decay the hue
  if (hue > 0) hue = Math.max(0, hue - 0.5);
}

function meta() {
  return {
    title: "Clocks",
    desc: "Browse saved clock melodies",
  };
}

export { boot, paint, act, sim, meta };
