// tapes, 2026.2.1
// Browse recently posted tapes from the database
// Shows tapes as !codes with tappable UI text buttons

const { max, floor } = Math;

const TAPES_PER_PAGE = 50;

let tapes = [];
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
let selectedIndex = -1; // Currently highlighted tape
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

// Fetch tapes from API
async function fetchTapes() {
  try {
    loading = tapes.length === 0;

    const response = await fetch(`/api/tv/tapes?limit=${TAPES_PER_PAGE}`);

    if (!response.ok) {
      throw new Error(`API error: ${response.status}`);
    }

    const data = await response.json();
    tapes = data.tapes || [];

    loading = false;
    error = null;
    needsLayout = true;
    selectedIndex = tapes.length > 0 ? 0 : -1;
  } catch (err) {
    error = err.message;
    loading = false;
    console.error("Failed to fetch tapes:", err);
  }
}

// Calculate Y position for a tape at given index
function getTapeY(index) {
  const tapeHeight = rowHeight * 2; // Each tape takes 2 rows
  return topMargin + index * tapeHeight - scroll;
}

// Get tape index at Y position
function getTapeAtY(y, screenHeight) {
  const tapeHeight = rowHeight * 2;
  const relativeY = y - topMargin + scroll;
  const index = Math.floor(relativeY / tapeHeight);
  if (index >= 0 && index < tapes.length) {
    const tapeY = getTapeY(index);
    // Check if within visible bounds
    if (tapeY >= topMargin - tapeHeight && tapeY < screenHeight - bottomMargin + tapeHeight) {
      return index;
    }
  }
  return -1;
}

function boot({ screen, store }) {
  // Initial fetch
  fetchTapes();

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

  if (loading && tapes.length === 0) {
    ink(150, 150, 180).write("Loading tapes...", { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }

  if (error && tapes.length === 0) {
    ink(255, 100, 100).write("Error: " + error.slice(0, 40), { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }

  if (tapes.length === 0) {
    ink(150, 150, 180).write("No tapes found", { center: "xy", x: w / 2, y: h / 2 }, false, undefined, false, FONT);
    return;
  }

  // Calculate heights
  chatHeight = h - topMargin - bottomMargin;
  const tapeHeight = rowHeight * 2; // Each tape takes 2 rows

  // Calculate total height
  if (needsLayout) {
    totalScrollHeight = tapes.length * tapeHeight;
    needsLayout = false;
  }

  // Mask off the scrollable area
  mask({
    x: 0,
    y: topMargin,
    width: w,
    height: chatHeight,
  });

  // Draw tapes
  for (let i = 0; i < tapes.length; i++) {
    const tape = tapes[i];
    const y = getTapeY(i);

    // Skip if outside visible area (with buffer)
    if (y + tapeHeight < topMargin - 20) continue;
    if (y > h - bottomMargin + 20) continue;

    // Selection highlight
    const isSelected = i === selectedIndex;
    if (isSelected) {
      ink(40, 40, 60, 200).box(0, y, w, tapeHeight);
    }

    // Code with ! prefix (tappable)
    const codeColor = isSelected ? [255, 200, 100] : [200, 150, 80];
    ink(...codeColor).write(`!${tape.id}`, { x: 4, y }, false, undefined, false, FONT);

    // Time ago
    const ago = timeAgo(tape.created_at);
    const agoX = 4 + text.width(`!${tape.id} `, FONT);
    ink(100, 100, 130).write(ago, { x: agoX, y }, false, undefined, false, FONT);

    // Handle/owner (if available, truncated for display)
    if (tape.owner) {
      const handleX = agoX + text.width(ago + " ", FONT);
      ink(180, 150, 255).write(tape.owner.slice(0, 12), { x: handleX, y }, false, undefined, false, FONT);
    }

    // Second line: title or "Tape {CODE}"
    const msgY = y + rowHeight;
    const charWidth = 4; // MatrixChunky8 character width
    const maxChars = Math.floor((w - 8) / charWidth);
    const title = tape.title || `Tape ${tape.id.toUpperCase()}`;
    ink(200, 200, 220).write(title.slice(0, maxChars), { x: 4, y: msgY }, false, undefined, false, FONT);

    // Subtle separator
    const sepY = y + tapeHeight - 1;
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

  // Label
  ink(100, 100, 120).write("📼 recent", { x: 4, y: footerY }, false, undefined, false, FONT);

  // Tape count
  const countText = `${tapes.length}`;
  ink(80, 80, 100).write(countText, { x: w - text.width(countText, FONT) - 4, y: footerY }, false, undefined, false, FONT);

  // Instructions
  const helpText = "↑↓:nav ⏎:play";
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
      const tapeY = getTapeY(selectedIndex);
      if (tapeY < topMargin) {
        scroll = selectedIndex * rowHeight * 2;
      }
    }
    boundScroll();
  }

  // Navigation down
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    if (selectedIndex < tapes.length - 1) {
      selectedIndex++;
      // Ensure selected item is visible
      const tapeY = getTapeY(selectedIndex);
      const tapeHeight = rowHeight * 2;
      if (tapeY + tapeHeight > h - bottomMargin) {
        scroll = (selectedIndex + 1) * tapeHeight - (h - topMargin - bottomMargin);
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
    selectedIndex = tapes.length - 1;
  }

  // Enter - play selected tape
  if (e.is("keyboard:down:enter")) {
    if (selectedIndex >= 0 && selectedIndex < tapes.length) {
      const tape = tapes[selectedIndex];
      jump(`video~!${tape.id}`);
    }
  }

  // Click to select and play
  if (e.is("touch") && e.y >= topMargin && e.y < h - bottomMargin) {
    const clickedIndex = getTapeAtY(e.y, h);
    if (clickedIndex >= 0) {
      const tape = tapes[clickedIndex];
      jump(`video~!${tape.id}`);
    }
  }

  // Refresh on R
  if (e.is("keyboard:down:r")) {
    fetchTapes();
    scroll = 0;
    selectedIndex = 0;
  }

  // Back to prompt
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

function sim({ store }) {
  // Animation tick (hue is incremented in paint for smooth background)
}

function meta() {
  return {
    title: "Tapes",
    desc: "Browse recently posted tapes",
  };
}

export { boot, paint, act, sim, meta };
