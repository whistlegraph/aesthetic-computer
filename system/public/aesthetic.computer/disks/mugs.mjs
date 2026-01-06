// mugs, 24.12.21 (Redesigned 2026.1.06)
// Browse recent mugs - preview on top, scrollable list below

const { floor, min, max, abs } = Math;

// 🎨 Color scheme
const scheme = {
  background: [16, 16, 24],
  previewBg: [12, 12, 18],
  listItemBg: [20, 20, 30],
  listItemBgAlt: [24, 24, 36],
  listItemSelected: [35, 40, 55],
  listItemAccent: [100, 150, 255],
  mugColor: [200, 200, 200], // Mug color text
  mugCode: [150, 200, 255],  // Code text (light blue)
  mugVia: [255, 200, 100],   // Via text (gold)
  mugOf: [120, 120, 140],    // "mug of" text
  timeAgo: [70, 70, 90],
  divider: [40, 40, 50],
  scrollbar: [40, 40, 50],
  scrollbarThumb: [80, 100, 140],
};

// Module state
let mugs = [];
let loading = true;
let error = null;
let selectedIndex = 0;
let scroll = 0; // Negative scroll (like colors.mjs)
let preloadAnimatedWebp = null;
let previewCache = {}; // Cache loaded previews by code
let viewBtn = null;

const ROW_HEIGHT = 40; // Height of each list item
const TOP_MARGIN = 4;
const previewHeight = 180; // Fixed height for preview area

// Mug color RGB values
const mugColors = {
  white: [240, 240, 240],
  black: [50, 50, 50],
  blue: [100, 150, 255],
  pink: [255, 150, 200],
  orange: [255, 180, 100],
  green: [100, 220, 150],
  red: [255, 100, 100],
  yellow: [255, 220, 100],
  darkblue: [50, 80, 180],
  darkgreen: [50, 150, 80],
};

function boot({ net, cursor, hud, ui, screen }) {
  cursor("native");
  hud.labelBack();
  preloadAnimatedWebp = net.preloadAnimatedWebp;
  
  // Create VIEW button
  viewBtn = new ui.Button(0, 0, 70, 24);
  viewBtn.text = "☕ VIEW";
  
  fetchMugs();
}

async function fetchMugs() {
  try {
    const res = await fetch("/api/mugs?limit=50");
    const data = await res.json();
    
    if (data.mugs) {
      mugs = data.mugs;
      // Start preloading the first few previews
      for (let i = 0; i < min(3, mugs.length); i++) {
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
  if (previewCache[mug.code]) return;
  
  if (!mug.preview) {
    console.warn("Mug has no preview URL:", mug.code, mug);
    // Mark as broken so we don't keep trying
    previewCache[mug.code] = { broken: true };
    return;
  }
  
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
    console.warn("Failed to preload preview:", mug.code, mug.preview, e);
    previewCache[mug.code] = { broken: true };
  }
}

function paint({ wipe, ink, box, paste, screen, line }) {
  wipe(scheme.background);
  
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
    ink(100).write("Create one at kidlisp.com", { center: "x", y: screen.height / 2 + 20, screen });
    return;
  }
  
  // === TOP: Preview area ===
  const selectedMug = mugs[selectedIndex];
  
  // Dark preview background
  ink(scheme.previewBg).box(0, 0, screen.width, previewHeight, "fill");
  
  if (selectedMug) {
    const preview = previewCache[selectedMug.code];
    const colorRgb = mugColors[selectedMug.color?.toLowerCase()] || [150, 150, 150];
    
    if (preview && preview.frames) {
      // Animate frames
      const now = performance.now();
      const delay = preview.frames[preview.frameIndex]?.delay || 800;
      if (now - preview.lastFrameTime > delay) {
        preview.frameIndex = (preview.frameIndex + 1) % preview.frames.length;
        preview.lastFrameTime = now;
      }
      
      // Draw preview centered at top (image first, title below)
      const frame = preview.frames[preview.frameIndex];
      const bitmap = {
        width: preview.width,
        height: preview.height,
        pixels: frame.pixels,
      };
      
      const titleHeight = 24;
      const maxH = previewHeight - titleHeight - 16;
      const maxW = screen.width - 120;
      const scale = min(maxW / bitmap.width, maxH / bitmap.height);
      const w = floor(bitmap.width * scale);
      const h = floor(bitmap.height * scale);
      const x = floor((screen.width - w) / 2);
      const y = floor((maxH - h) / 2) + 4;
      
      paste(bitmap, x, y, { scale });
      
      // Title below image: "color mug of CODE" or "color mug of CODE in $via"
      const imageBottomY = y + h;
      const titleY = imageBottomY + 6;
      
      // sourceCode is the painting hash (e.g., "sb9"), via is the kidlisp code (e.g., "bop")
      // Show: "white mug of #sb9 in $bop" for kidlisp mugs, "white mug of #abc" for direct painting mugs
      const displayCode = selectedMug.sourceCode || selectedMug.code;
      const colorName = (selectedMug.color || "white").toLowerCase();
      const viaPart = selectedMug.via ? ` in $${selectedMug.via}` : "";
      
      // Calculate total width for centering
      const totalWidth = (colorName.length + " mug of ".length + displayCode.length + viaPart.length) * 6;
      let titleX = floor((screen.width - totalWidth) / 2);
      
      // Draw color name in its color
      ink(...colorRgb).write(colorName, { x: titleX, y: titleY });
      titleX += colorName.length * 6;
      
      // Draw " mug of " in muted color
      ink(scheme.mugOf).write(" mug of ", { x: titleX, y: titleY });
      titleX += " mug of ".length * 6;
      
      // Draw code in light blue
      ink(scheme.mugCode).write(displayCode, { x: titleX, y: titleY });
      titleX += displayCode.length * 6;
      
      // Draw " in $kidlispcode" in gold if present
      if (selectedMug.via) {
        ink(scheme.mugVia).write(viaPart, { x: titleX, y: titleY });
      }
      
      // VIEW button
      viewBtn.box.x = screen.width - 78;
      viewBtn.box.y = previewHeight - 32;
      viewBtn.box.w = 70;
      viewBtn.box.h = 24;
      
      const isHover = viewBtn.down;
      const blink = Math.sin(performance.now() / 400) * 0.5 + 0.5;
      const fillColor = isHover ? [50, 60, 80] : [30, 35, 45];
      const borderColor = isHover ? [255, 200, 100] : [60 + blink * 60, 100 + blink * 40, 150 + blink * 40];
      const textColor = isHover ? [255, 220, 150] : [180 + blink * 50, 180 + blink * 50, 200 + blink * 50];
      
      ink(fillColor).box(viewBtn.box, "fill");
      ink(borderColor).box(viewBtn.box, "outline");
      ink(textColor).write(viewBtn.text, {
        x: viewBtn.box.x + 8,
        y: viewBtn.box.y + 8,
      });
    } else if (preview?.broken) {
      // Broken/missing preview - draw X and "no mug"
      const cx = floor(screen.width / 2);
      const cy = floor(previewHeight / 2) - 10;
      const size = 30;
      ink(80, 50, 50).line(cx - size, cy - size, cx + size, cy + size);
      ink(80, 50, 50).line(cx - size, cy + size, cx + size, cy - size);
      ink(100, 60, 60).write("no preview", { center: "x", y: cy + size + 10, screen });
    } else {
      // Loading indicator
      const pulse = Math.sin(performance.now() / 300) * 0.3 + 0.7;
      ink(100 * pulse, 100 * pulse, 120 * pulse).write("☕ Loading...", { center: "x", y: previewHeight / 2 - 8, screen });
      preloadPreview(selectedMug);
    }
  }
  
  // Divider line
  ink(scheme.divider).line(0, previewHeight, screen.width, previewHeight);
  
  // === BOTTOM: Scrollable list ===
  const listY = previewHeight + 1;
  const listHeight = screen.height - listY;
  
  // Draw list items (only visible ones)
  mugs.forEach((mug, i) => {
    const itemY = scroll + listY + TOP_MARGIN + (i * ROW_HEIGHT);
    
    // Skip if off screen
    if (itemY + ROW_HEIGHT < listY || itemY > screen.height) return;
    
    const isSelected = i === selectedIndex;
    
    // Alternating background
    const bgColor = isSelected ? scheme.listItemSelected : (i % 2 === 0 ? scheme.listItemBg : scheme.listItemBgAlt);
    ink(bgColor).box(0, itemY, screen.width - 6, ROW_HEIGHT, "fill");
    
    // Left accent for selected
    if (isSelected) {
      ink(scheme.listItemAccent).box(0, itemY, 3, ROW_HEIGHT, "fill");
    }
    
    // Color indicator square
    const colorRgb = mugColors[mug.color?.toLowerCase()] || [150, 150, 150];
    ink(colorRgb).box(12, itemY + 10, 18, 18, "fill");
    ink(60, 60, 70).box(12, itemY + 10, 18, 18, "outline");
    
    // sourceCode is the painting hash (e.g., "sb9"), via is the kidlisp code (e.g., "bop")
    const displayCode = mug.sourceCode || mug.code;
    const colorText = (mug.color || "white").toLowerCase();
    
    // Row 1: "color mug of CODE in $via"
    let textX = 38;
    ink(colorRgb).write(colorText, { x: textX, y: itemY + 8 });
    textX += colorText.length * 6;
    
    ink(scheme.mugOf).write(" mug of ", { x: textX, y: itemY + 8 });
    textX += " mug of ".length * 6;
    
    ink(scheme.mugCode).write(displayCode, { x: textX, y: itemY + 8 });
    
    if (mug.via) {
      textX += displayCode.length * 6;
      ink(scheme.mugVia).write(` in $${mug.via}`, { x: textX, y: itemY + 8 });
    }
    
    // Row 2: Time ago
    const timeAgo = getTimeAgo(mug.createdAt);
    ink(scheme.timeAgo).write(timeAgo, { x: 38, y: itemY + 22 });
  });
  
  // Scrollbar (flush right like colors.mjs)
  const contentHeight = mugs.length * ROW_HEIGHT;
  const visibleHeight = listHeight - TOP_MARGIN;
  
  if (contentHeight > visibleHeight) {
    const scrollBarX = screen.width - 4;
    const scrollBarHeight = listHeight;
    
    // Background track
    ink(scheme.scrollbar).box(scrollBarX, listY, 4, scrollBarHeight);
    
    // Calculate thumb
    const thumbHeight = max(20, floor((visibleHeight / contentHeight) * scrollBarHeight));
    const maxScroll = max(0, contentHeight - visibleHeight);
    const scrollRatio = maxScroll > 0 ? abs(scroll) / maxScroll : 0;
    const thumbY = listY + (scrollBarHeight - thumbHeight) * scrollRatio;
    
    // Thumb
    ink(scheme.scrollbarThumb).box(scrollBarX, floor(thumbY), 4, thumbHeight);
  }
}

function act({ event: e, jump, screen, sound, store }) {
  if (loading || mugs.length === 0) return;
  
  const listY = previewHeight + 1;
  const listHeight = screen.height - listY;
  const contentHeight = mugs.length * ROW_HEIGHT;
  const visibleHeight = listHeight - TOP_MARGIN;
  
  // Scrolling (like colors.mjs - negative scroll)
  if (e.is("draw")) {
    scroll += e.delta.y;
    boundScroll(visibleHeight, contentHeight);
  }
  
  if (e.is("scroll")) {
    scroll -= e.y;
    boundScroll(visibleHeight, contentHeight);
  }
  
  // Keyboard navigation
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:w")) {
    selectedIndex = max(0, selectedIndex - 1);
    scrollToSelected(visibleHeight, contentHeight);
    preloadNearby();
  }
  
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:s")) {
    selectedIndex = min(mugs.length - 1, selectedIndex + 1);
    scrollToSelected(visibleHeight, contentHeight);
    preloadNearby();
  }
  
  // Enter opens
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:space")) {
    openSelectedMug(jump);
  }
  
  // VIEW button click
  viewBtn?.act(e, {
    down: () => {
      sound?.synth({ type: "sine", tone: 440, duration: 0.05, volume: 0.3 });
    },
    push: () => {
      sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
      openSelectedMug(jump);
    },
  });
  
  // Click to select item in list
  if (e.is("touch") && e.y > listY) {
    const clickedIndex = floor((e.y - listY - TOP_MARGIN - scroll) / ROW_HEIGHT);
    if (clickedIndex >= 0 && clickedIndex < mugs.length) {
      selectedIndex = clickedIndex;
      preloadNearby();
    }
  }
}

function boundScroll(visibleHeight, contentHeight) {
  const maxNegativeScroll = -max(0, contentHeight - visibleHeight);
  if (scroll < maxNegativeScroll) scroll = maxNegativeScroll;
  if (scroll > 0) scroll = 0;
}

function scrollToSelected(visibleHeight, contentHeight) {
  const itemTop = selectedIndex * ROW_HEIGHT;
  const itemBottom = itemTop + ROW_HEIGHT;
  
  // Convert to negative scroll space
  const visibleTop = -scroll;
  const visibleBottom = visibleTop + visibleHeight;
  
  if (itemTop < visibleTop) {
    scroll = -itemTop;
  } else if (itemBottom > visibleBottom) {
    scroll = -(itemBottom - visibleHeight);
  }
  boundScroll(visibleHeight, contentHeight);
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
    // Use product code (+CODE) as the universal identifier
    jump("mug~+" + mug.code);
  }
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
