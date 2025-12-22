// mugs, 24.12.21
// Browse recent mugs - preview on top, scrollable list below

const { floor, min, max } = Math;

// Module state
let mugs = [];
let loading = true;
let error = null;
let selectedIndex = 0;
let scrollY = 0; // Pixel-based scroll position
let targetScrollY = 0; // For smooth scrolling
let preloadAnimatedWebp = null;
let previewCache = {}; // Cache loaded previews by code
let viewBtn = null;
let isDragging = false;
let lastDragY = 0;
let velocity = 0;

const itemHeight = 36; // Height of each list item
const charWidth = 8;
const charHeight = 16;
const btnPadding = 6;
const previewHeight = 180; // Fixed height for preview area

function boot({ net, cursor, hud, ui, screen }) {
  cursor("native");
  hud.labelBack();
  preloadAnimatedWebp = net.preloadAnimatedWebp;
  
  // Create VIEW button
  const btnText = "☕ VIEW";
  const btnW = btnText.length * charWidth + btnPadding * 2;
  const btnH = charHeight + btnPadding * 2;
  viewBtn = new ui.Button(0, 0, btnW, btnH);
  viewBtn.text = btnText;
  
  fetchMugs();
}

async function fetchMugs() {
  try {
    const res = await fetch("/api/mugs?limit=50");
    const data = await res.json();
    
    if (data.mugs) {
      mugs = data.mugs;
      // Start preloading the first few previews
      for (let i = 0; i < Math.min(3, mugs.length); i++) {
        preloadPreview(mugs[i]);
      }
    }
    loading = false;
  } catch (e) {
    error = e.message;
    loading = false;
  }
}

async function preloadPreview(mug) {
  if (!mug.preview || previewCache[mug.code]) return;
  
  try {
    const result = await preloadAnimatedWebp(mug.preview);
    previewCache[mug.code] = {
      width: result.width,
      height: result.height,
      frames: result.frames,
      frameIndex: 0,
      lastFrameTime: 0,
    };
  } catch (e) {
    console.warn("Failed to preload preview:", mug.code, e);
  }
}

function paint({ wipe, ink, box, paste, screen, line }) {
  wipe(25);
  
  if (loading) {
    ink(255).write("Loading mugs...", { center: "xy", screen });
    return;
  }
  
  if (error) {
    ink(255, 100, 100).write(error, { center: "xy", screen });
    return;
  }
  
  if (mugs.length === 0) {
    ink(150).write("No mugs yet!", { center: "xy", screen });
    ink(100).write("Create one with: mug #CODE color", { center: "x", y: screen.height / 2 + 20, screen });
    return;
  }
  
  // Smooth scroll animation
  const scrollDiff = targetScrollY - scrollY;
  scrollY += scrollDiff * 0.2;
  if (Math.abs(scrollDiff) < 0.5) scrollY = targetScrollY;
  
  // Apply velocity for momentum scrolling
  if (!isDragging && Math.abs(velocity) > 0.5) {
    targetScrollY += velocity;
    velocity *= 0.92; // Friction
    clampScroll(screen);
  }
  
  // === TOP: Preview area ===
  const selectedMug = mugs[selectedIndex];
  const previewAreaHeight = previewHeight;
  
  // Dark preview background
  ink(20).box(0, 0, screen.width, previewAreaHeight, "fill");
  
  if (selectedMug) {
    const preview = previewCache[selectedMug.code];
    
    // Title centered above preview
    const titleY = 6;
    const colorRgb = getColorRgb(selectedMug.color);
    const titleText = selectedMug.color.toUpperCase() + " #" + selectedMug.sourceCode;
    const titleWidth = titleText.length * charWidth;
    ink(...colorRgb).write(titleText, { center: "x", y: titleY, screen }, undefined, undefined, false, "unifont");
    
    if (preview) {
      // Animate frames
      const now = performance.now();
      const delay = preview.frames[preview.frameIndex]?.delay || 800;
      if (now - preview.lastFrameTime > delay) {
        preview.frameIndex = (preview.frameIndex + 1) % preview.frames.length;
        preview.lastFrameTime = now;
      }
      
      // Draw preview centered
      const frame = preview.frames[preview.frameIndex];
      const bitmap = {
        width: preview.width,
        height: preview.height,
        pixels: frame.pixels,
      };
      
      const maxH = previewAreaHeight - 60;
      const maxW = screen.width - 120;
      const scale = min(maxW / bitmap.width, maxH / bitmap.height);
      const w = floor(bitmap.width * scale);
      const h = floor(bitmap.height * scale);
      const x = floor((screen.width - w) / 2);
      const y = 24 + floor((maxH - h) / 2);
      
      paste(bitmap, x, y, { scale });
      
      // VIEW button next to preview
      const btnW = viewBtn.text.length * charWidth + btnPadding * 2;
      const btnH = charHeight + btnPadding * 2;
      viewBtn.box.x = screen.width - btnW - 8;
      viewBtn.box.y = previewAreaHeight - btnH - 8;
      viewBtn.box.w = btnW;
      viewBtn.box.h = btnH;
      
      const isHover = viewBtn.down;
      const fillColor = isHover ? [60, 80, 100] : [35, 45, 55];
      const borderColor = isHover ? [255, 200, 100] : [100, 150, 200];
      const textColor = isHover ? [255, 220, 150] : [255, 255, 255];
      
      ink(...fillColor).box(viewBtn.box, "fill");
      ink(...borderColor).box(viewBtn.box, "outline");
      ink(...textColor).write(viewBtn.text, {
        x: viewBtn.box.x + btnPadding,
        y: viewBtn.box.y + btnPadding,
      }, undefined, undefined, false, "unifont");
    } else {
      // Loading indicator
      const pulse = Math.sin(performance.now() / 300) * 0.3 + 0.7;
      ink(100 * pulse).write("☕", { center: "x", y: previewAreaHeight / 2 - 8, screen }, undefined, undefined, false, "unifont");
      preloadPreview(selectedMug);
    }
  }
  
  // Divider line
  ink(50).line(0, previewAreaHeight, screen.width, previewAreaHeight);
  
  // === BOTTOM: Scrollable list ===
  const listY = previewAreaHeight + 1;
  const listHeight = screen.height - listY;
  const totalContentHeight = mugs.length * itemHeight;
  const maxScroll = max(0, totalContentHeight - listHeight);
  
  // Draw list items (only visible ones)
  const startIdx = max(0, floor(scrollY / itemHeight));
  const endIdx = min(mugs.length, startIdx + Math.ceil(listHeight / itemHeight) + 1);
  
  for (let i = startIdx; i < endIdx; i++) {
    const mug = mugs[i];
    const itemY = listY + (i * itemHeight) - scrollY;
    
    // Skip if off screen
    if (itemY + itemHeight < listY || itemY > screen.height) continue;
    
    const isSelected = i === selectedIndex;
    
    // Highlight selected
    if (isSelected) {
      ink(45, 50, 60).box(0, itemY, screen.width, itemHeight, "fill");
      ink(100, 150, 255).box(0, itemY, 3, itemHeight, "fill"); // Left accent
    }
    
    // Color indicator
    const colorRgb = getColorRgb(mug.color);
    ink(...colorRgb).box(10, itemY + 8, 16, 16, "fill");
    ink(60).box(10, itemY + 8, 16, 16, "outline");
    
    // Code and color text
    const codeText = "#" + mug.sourceCode;
    const colorText = mug.color.toUpperCase();
    
    ink(isSelected ? 255 : 180).write(codeText, { x: 34, y: itemY + 6 }, undefined, undefined, false, "unifont");
    ink(...colorRgb).write(colorText, { x: 34 + (codeText.length + 1) * charWidth, y: itemY + 6 }, undefined, undefined, false, "unifont");
    
    // Time ago
    const timeAgo = getTimeAgo(mug.createdAt);
    ink(70).write(timeAgo, { x: 34, y: itemY + 20 }, undefined, undefined, false, "unifont");
    
    // Subtle divider between items
    if (i < mugs.length - 1) {
      ink(35).line(10, itemY + itemHeight - 1, screen.width - 10, itemY + itemHeight - 1);
    }
  }
  
  // Scrollbar (chat-style)
  if (totalContentHeight > listHeight) {
    const scrollbarTrackHeight = listHeight - 4;
    const scrollbarHeight = max(30, floor((listHeight / totalContentHeight) * scrollbarTrackHeight));
    const scrollProgress = scrollY / maxScroll;
    const scrollbarY = listY + 2 + floor(scrollProgress * (scrollbarTrackHeight - scrollbarHeight));
    
    // Track
    ink(30).box(screen.width - 6, listY + 2, 4, scrollbarTrackHeight, "fill");
    // Thumb
    ink(70).box(screen.width - 6, scrollbarY, 4, scrollbarHeight, "fill");
  }
}

function act({ event: e, jump, screen }) {
  if (loading || mugs.length === 0) return;
  
  const listY = previewHeight + 1;
  const listHeight = screen.height - listY;
  const totalContentHeight = mugs.length * itemHeight;
  const maxScroll = max(0, totalContentHeight - listHeight);
  
  // Keyboard navigation
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:w")) {
    selectedIndex = max(0, selectedIndex - 1);
    scrollToSelected(screen);
    preloadNearby();
  }
  
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:s")) {
    selectedIndex = min(mugs.length - 1, selectedIndex + 1);
    scrollToSelected(screen);
    preloadNearby();
  }
  
  // Enter opens
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:space")) {
    openSelectedMug(jump);
  }
  
  // VIEW button click
  viewBtn?.act(e, {
    push: () => openSelectedMug(jump),
  });
  
  // Touch/drag scrolling (chat-style)
  if (e.is("touch")) {
    if (e.y > listY) {
      // Check if clicking on an item
      const clickedIndex = floor((e.y - listY + scrollY) / itemHeight);
      if (clickedIndex >= 0 && clickedIndex < mugs.length) {
        selectedIndex = clickedIndex;
        preloadNearby();
      }
      isDragging = true;
      lastDragY = e.y;
      velocity = 0;
    }
  }
  
  if (e.is("draw") && isDragging) {
    const deltaY = lastDragY - e.y;
    targetScrollY += deltaY;
    velocity = deltaY;
    lastDragY = e.y;
    clampScroll(screen);
  }
  
  if (e.is("lift")) {
    isDragging = false;
  }
  
  // Scroll wheel
  if (e.is("wheel")) {
    targetScrollY += e.delta * 0.5;
    clampScroll(screen);
  }
}

function clampScroll(screen) {
  const listY = previewHeight + 1;
  const listHeight = screen.height - listY;
  const totalContentHeight = mugs.length * itemHeight;
  const maxScroll = max(0, totalContentHeight - listHeight);
  targetScrollY = max(0, min(maxScroll, targetScrollY));
}

function scrollToSelected(screen) {
  const listY = previewHeight + 1;
  const listHeight = screen.height - listY;
  const itemTop = selectedIndex * itemHeight;
  const itemBottom = itemTop + itemHeight;
  
  // Scroll to keep selected item visible
  if (itemTop < targetScrollY) {
    targetScrollY = itemTop;
  } else if (itemBottom > targetScrollY + listHeight) {
    targetScrollY = itemBottom - listHeight;
  }
  clampScroll(screen);
}

function preloadNearby() {
  for (let i = -1; i <= 2; i++) {
    const idx = selectedIndex + i;
    if (idx >= 0 && idx < mugs.length) {
      preloadPreview(mugs[idx]);
    }
  }
}

function openSelectedMug(jump) {
  const mug = mugs[selectedIndex];
  if (mug) {
    jump("mug~" + mug.sourceCode + "~" + mug.color);
  }
}

function getColorRgb(color) {
  const colors = {
    white: [255, 255, 255],
    black: [40, 40, 40],
    blue: [100, 150, 255],
    pink: [255, 150, 200],
    orange: [255, 180, 100],
    green: [100, 220, 150],
    red: [255, 100, 100],
    yellow: [255, 220, 100],
    darkblue: [50, 80, 180],
    darkgreen: [50, 150, 80],
  };
  return colors[color?.toLowerCase()] || [150, 150, 150];
}

function getTimeAgo(dateStr) {
  if (!dateStr) return "";
  const date = new Date(dateStr);
  const now = new Date();
  const diff = now - date;
  
  const mins = floor(diff / 60000);
  if (mins < 1) return "just now";
  if (mins < 60) return mins + "m ago";
  
  const hours = floor(mins / 60);
  if (hours < 24) return hours + "h ago";
  
  const days = floor(hours / 24);
  if (days < 7) return days + "d ago";
  
  return date.toLocaleDateString();
}

function meta() {
  return {
    title: "Recent Mugs",
    desc: "Browse and purchase recently created mugs",
  };
}

export { boot, paint, act, meta };
