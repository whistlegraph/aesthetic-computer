// mugs, 24.12.21
// Browse recent mugs - scrolling list with VIEW button

const { floor, min } = Math;

// Module state
let mugs = [];
let loading = true;
let error = null;
let selectedIndex = 0;
let scrollOffset = 0;
let preloadAnimatedWebp = null;
let previewCache = {}; // Cache loaded previews by code
let viewBtn = null;

const itemHeight = 32; // Height of each list item
const charWidth = 8;
const charHeight = 16;
const btnPadding = 6;

function boot({ net, cursor, hud, ui, screen }) {
  cursor("native");
  hud.labelBack();
  preloadAnimatedWebp = net.preloadAnimatedWebp;
  
  // Create VIEW button (positioned in paint)
  const btnText = "☕ VIEW MUG";
  const btnW = btnText.length * charWidth + btnPadding * 2;
  const btnH = charHeight + btnPadding * 2;
  viewBtn = new ui.Button(0, 0, btnW, btnH);
  viewBtn.text = btnText;
  
  fetchMugs();
}

async function fetchMugs() {
  try {
    const res = await fetch("/api/mugs?limit=30");
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
  
  // Layout: list on left, preview on right
  const listWidth = floor(screen.width * 0.4);
  const previewX = listWidth + 10;
  const previewWidth = screen.width - previewX - 10;
  
  // Title
  ink(255).write("☕ RECENT MUGS", { x: 8, y: 6 }, undefined, undefined, false, "unifont");
  ink(100).write(`${mugs.length}`, { x: 8 + 14 * charWidth, y: 6 }, undefined, undefined, false, "unifont");
  
  // Draw list
  const listY = 28;
  const visibleItems = floor((screen.height - listY - 10) / itemHeight);
  
  // Smooth scroll - selectedIndex drives scroll
  const targetScroll = Math.max(0, Math.min(selectedIndex - floor(visibleItems / 2), mugs.length - visibleItems));
  scrollOffset = targetScroll;
  
  for (let i = 0; i < visibleItems && i + scrollOffset < mugs.length; i++) {
    const idx = i + scrollOffset;
    const mug = mugs[idx];
    const y = listY + i * itemHeight;
    const isSelected = idx === selectedIndex;
    
    // Highlight selected
    if (isSelected) {
      ink(50, 50, 60).box(0, y - 2, listWidth, itemHeight, "fill");
      ink(100, 150, 255).box(0, y - 2, 3, itemHeight, "fill"); // Left accent
    }
    
    // Color indicator
    const colorRgb = getColorRgb(mug.color);
    ink(...colorRgb).box(8, y + 4, 12, 12, "fill");
    ink(80).box(8, y + 4, 12, 12, "outline");
    
    // Code and color text
    const codeText = "#" + mug.sourceCode;
    const colorText = mug.color.toUpperCase();
    
    ink(isSelected ? 255 : 180).write(codeText, { x: 26, y: y + 2 }, undefined, undefined, false, "unifont");
    ink(...colorRgb).write(colorText, { x: 26 + (codeText.length + 1) * charWidth, y: y + 2 }, undefined, undefined, false, "unifont");
    
    // Time ago
    const timeAgo = getTimeAgo(mug.createdAt);
    ink(80).write(timeAgo, { x: 26, y: y + 14 }, undefined, undefined, false, "unifont");
  }
  
  // Scrollbar
  if (mugs.length > visibleItems) {
    const scrollbarHeight = Math.max(20, floor((visibleItems / mugs.length) * (screen.height - listY - 10)));
    const scrollbarY = listY + floor((scrollOffset / (mugs.length - visibleItems)) * (screen.height - listY - 10 - scrollbarHeight));
    ink(60).box(listWidth - 4, scrollbarY, 3, scrollbarHeight, "fill");
  }
  
  // Divider line
  ink(50).line(listWidth, listY, listWidth, screen.height - 10);
  
  // Draw preview of selected mug
  const selectedMug = mugs[selectedIndex];
  if (selectedMug) {
    const preview = previewCache[selectedMug.code];
    
    // Title above preview
    const colorRgb = getColorRgb(selectedMug.color);
    ink(...colorRgb).write(selectedMug.color.toUpperCase(), { x: previewX, y: 8 }, undefined, undefined, false, "unifont");
    ink(180).write(" MUG of ", { x: previewX + selectedMug.color.length * charWidth, y: 8 }, undefined, undefined, false, "unifont");
    ink(150, 200, 255).write("#" + selectedMug.sourceCode, { x: previewX + (selectedMug.color.length + 8) * charWidth, y: 8 }, undefined, undefined, false, "unifont");
    
    if (preview) {
      // Animate frames
      const now = performance.now();
      const delay = preview.frames[preview.frameIndex]?.delay || 800;
      if (now - preview.lastFrameTime > delay) {
        preview.frameIndex = (preview.frameIndex + 1) % preview.frames.length;
        preview.lastFrameTime = now;
      }
      
      // Draw preview
      const frame = preview.frames[preview.frameIndex];
      const bitmap = {
        width: preview.width,
        height: preview.height,
        pixels: frame.pixels,
      };
      
      const btnHeight = 40;
      const maxPreviewHeight = screen.height - 70 - btnHeight;
      const scale = min(previewWidth / bitmap.width, maxPreviewHeight / bitmap.height);
      const w = floor(bitmap.width * scale);
      const h = floor(bitmap.height * scale);
      const x = previewX + floor((previewWidth - w) / 2);
      const y = 30 + floor((maxPreviewHeight - h) / 2);
      
      paste(bitmap, x, y, { scale });
    } else {
      // Show loading for this preview
      const pulse = Math.sin(performance.now() / 300) * 0.3 + 0.7;
      ink(100 * pulse).write("☕", { x: previewX + previewWidth / 2 - 8, y: screen.height / 2 - 30 }, undefined, undefined, false, "unifont");
      ink(80).write("Loading...", { x: previewX + previewWidth / 2 - 40, y: screen.height / 2 });
      // Trigger preload
      preloadPreview(selectedMug);
    }
    
    // VIEW button at bottom
    const btnW = viewBtn.text.length * charWidth + btnPadding * 2;
    const btnH = charHeight + btnPadding * 2;
    viewBtn.box.x = previewX + floor((previewWidth - btnW) / 2);
    viewBtn.box.y = screen.height - btnH - 12;
    viewBtn.box.w = btnW;
    viewBtn.box.h = btnH;
    
    const isHover = viewBtn.down;
    const fillColor = isHover ? [60, 80, 100] : [40, 50, 60];
    const borderColor = isHover ? [255, 200, 100] : [100, 150, 200];
    const textColor = isHover ? [255, 220, 150] : [255, 255, 255];
    
    ink(...fillColor).box(viewBtn.box, "fill");
    ink(...borderColor).box(viewBtn.box, "outline");
    ink(...textColor).write(viewBtn.text, {
      x: viewBtn.box.x + btnPadding,
      y: viewBtn.box.y + btnPadding,
    }, undefined, undefined, false, "unifont");
  }
  
  // Navigation hints
  ink(60).write("↑↓", { x: 8, y: screen.height - 12 }, undefined, undefined, false, "unifont");
}

function act({ event: e, jump, screen }) {
  if (loading || mugs.length === 0) return;
  
  // Keyboard navigation
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:w")) {
    selectedIndex = Math.max(0, selectedIndex - 1);
    preloadNearby();
  }
  
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:s")) {
    selectedIndex = Math.min(mugs.length - 1, selectedIndex + 1);
    preloadNearby();
  }
  
  // Enter opens via button
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:space")) {
    openSelectedMug(jump);
  }
  
  // VIEW button click
  viewBtn?.act(e, {
    push: () => openSelectedMug(jump),
  });
  
  // Click on list to select (but not open)
  if (e.is("touch")) {
    const listY = 28;
    const listWidth = floor(screen.width * 0.4);
    
    if (e.x < listWidth && e.y > listY) {
      const clickedIndex = floor((e.y - listY) / itemHeight) + scrollOffset;
      if (clickedIndex >= 0 && clickedIndex < mugs.length) {
        selectedIndex = clickedIndex;
        preloadNearby();
      }
    }
  }
  
  // Scroll wheel
  if (e.is("wheel")) {
    const delta = e.delta > 0 ? 1 : -1;
    selectedIndex = Math.max(0, Math.min(mugs.length - 1, selectedIndex + delta));
    preloadNearby();
  }
}

function preloadNearby() {
  // Preload previews around the selected index
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
    // Jump to mug piece with this code and color
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
