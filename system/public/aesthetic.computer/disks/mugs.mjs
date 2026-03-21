// mugs, 24.12.21 (Redesigned 2026.1.06)
// Browse recent mugs - preview on top, scrollable list below

const { floor, ceil, min, max, abs, sin } = Math;

// 🎨 Color scheme
const scheme = {
  background: [16, 16, 24],
  baseBg: [16, 16, 24], // Base background for tinting
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
  // Character-level syntax highlighting
  hashPrefix: [0, 188, 212],   // cyan for #
  dollarPrefix: [255, 215, 0], // gold for $
  codeChar: [135, 206, 235],   // light blue for code chars
  viaChar: [0, 229, 255],      // cyan for via identifier
};

// Module state
let mugs = [];
let loading = true;
let error = null;
let selectedIndex = 0;
let scroll = 0; // Negative scroll (like colors.mjs)
let preloadAnimatedWebp = null;
let previewCache = {}; // Cache loaded previews by code
let buyBtn = null;
let viewBtn = null; // VIEW button to go to mug page

const btnCharWidth = 8; // unifont char width
const btnCharHeight = 16; // unifont char height
const btnPadding = 6;

// Auto-play state
let autoPlay = true; // Auto-advance through mugs
let autoPlayInterval = 4000; // 4 seconds per mug
let lastAutoAdvance = 0;
let userInteracted = false; // Pause auto-play on user interaction
let autoPlayPauseTime = 0;
const AUTO_PLAY_RESUME_DELAY = 8000; // Resume after 8s of no interaction

// Background color transition
let currentBgColor = [...scheme.baseBg];
let targetBgColor = [...scheme.baseBg];

// Checkout state (for buy button)
let checkoutUrls = {}; // Cache checkout URLs by mug code
let checkoutLoading = {}; // Track loading state by mug code
let buyPending = false;
let apiRef = null;

const ROW_HEIGHT = 24; // Condensed row height
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

function boot({ net, cursor, hud, ui, screen, api }) {
  cursor("native");
  hud.labelBack();
  preloadAnimatedWebp = net.preloadAnimatedWebp;
  apiRef = api;
  
  // Create buttons
  buyBtn = new ui.Button(0, 0, 100, 28);
  viewBtn = new ui.Button(0, 0, 60, 24); // VIEW button
  
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

function paint({ wipe, ink, box, paste, screen, line, mask, unmask }) {
  const time = performance.now() / 1000;
  const now = performance.now();
  
  // Auto-play: advance to next mug periodically
  if (autoPlay && mugs.length > 1 && !loading) {
    // Check if we should resume after user interaction
    if (userInteracted && now - autoPlayPauseTime > AUTO_PLAY_RESUME_DELAY) {
      userInteracted = false;
    }
    
    if (!userInteracted && now - lastAutoAdvance > autoPlayInterval) {
      selectedIndex = (selectedIndex + 1) % mugs.length;
      // Auto-scroll to keep selected visible
      const listY = previewHeight + 1;
      const listHeight = screen.height - listY;
      const visibleHeight = listHeight - TOP_MARGIN;
      const contentHeight = mugs.length * ROW_HEIGHT;
      scrollToSelected(visibleHeight, contentHeight);
      preloadNearby();
      lastAutoAdvance = now;
    }
  }
  
  // Update target background color based on selected mug
  if (mugs.length > 0 && mugs[selectedIndex]) {
    const mugColorName = mugs[selectedIndex].color?.toLowerCase() || "white";
    const mugRgb = mugColors[mugColorName] || [150, 150, 150];
    // Tint: blend mug color into dark background at 15% intensity
    targetBgColor = [
      floor(scheme.baseBg[0] * 0.85 + mugRgb[0] * 0.15),
      floor(scheme.baseBg[1] * 0.85 + mugRgb[1] * 0.15),
      floor(scheme.baseBg[2] * 0.85 + mugRgb[2] * 0.15),
    ];
  }
  
  // Smoothly transition background color
  const lerpSpeed = 0.08;
  currentBgColor[0] += (targetBgColor[0] - currentBgColor[0]) * lerpSpeed;
  currentBgColor[1] += (targetBgColor[1] - currentBgColor[1]) * lerpSpeed;
  currentBgColor[2] += (targetBgColor[2] - currentBgColor[2]) * lerpSpeed;
  
  // Animated background - subtle floating particles with tinted color
  wipe(floor(currentBgColor[0]), floor(currentBgColor[1]), floor(currentBgColor[2]));
  
  for (let i = 0; i < 15; i++) {
    const seed = i * 137.5;
    const x = (seed * 7.3 + time * 12 * (i % 2 === 0 ? 1 : -1)) % (screen.width + 40) - 20;
    const baseY = screen.height - ((seed * 3.7 + time * 18) % (screen.height + 100));
    const y = baseY + sin(time * 2 + seed) * 6;
    const size = 2 + sin(seed) * 1;
    const alpha = 0.12 + sin(time * 1.5 + seed * 0.5) * 0.08;
    ink(100 * alpha, 120 * alpha, 180 * alpha).box(floor(x), floor(y), floor(size), floor(size), "fill");
  }
  
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
  
  // Dark preview background with slight transparency
  ink(scheme.previewBg[0], scheme.previewBg[1], scheme.previewBg[2], 0.9).box(0, 0, screen.width, previewHeight, "fill");
  
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
      
      const displayCode = selectedMug.sourceCode || selectedMug.code;
      const colorName = (selectedMug.color || "white").toLowerCase();
      const viaPart = selectedMug.via ? ` in $${selectedMug.via}` : "";
      
      // Calculate total width for centering (include # prefix)
      const totalWidth = (colorName.length + " mug of ".length + 1 + displayCode.length + viaPart.length) * 6;
      let titleX = floor((screen.width - totalWidth) / 2);
      
      // Draw color name in its color
      ink(...colorRgb).write(colorName, { x: titleX, y: titleY });
      titleX += colorName.length * 6;
      
      // Draw " mug of " in muted color
      ink(scheme.mugOf).write(" mug of ", { x: titleX, y: titleY });
      titleX += " mug of ".length * 6;
      
      // Draw # prefix in cyan, then code chars in light blue (character-by-character)
      ink(scheme.hashPrefix).write("#", { x: titleX, y: titleY });
      titleX += 6;
      for (const char of displayCode) {
        ink(scheme.codeChar).write(char, { x: titleX, y: titleY });
        titleX += 6;
      }
      
      // Draw " in $kidlispcode" with $ in gold and code in cyan (character-by-character)
      if (selectedMug.via) {
        ink(scheme.mugOf).write(" in ", { x: titleX, y: titleY });
        titleX += " in ".length * 6;
        ink(scheme.dollarPrefix).write("$", { x: titleX, y: titleY });
        titleX += 6;
        for (const char of selectedMug.via) {
          ink(scheme.viaChar).write(char, { x: titleX, y: titleY });
          titleX += 6;
        }
      }
      
      // VIEW button - goes to mug page
      const viewText = "👁 VIEW";
      const viewBtnW = viewText.length * 6 + 12;
      const viewBtnH = 22;
      viewBtn.box.x = 8;
      viewBtn.box.y = previewHeight - viewBtnH - 12;
      viewBtn.box.w = viewBtnW;
      viewBtn.box.h = viewBtnH;
      
      // Draw VIEW button
      const viewHover = viewBtn.down;
      const viewFill = viewHover ? [50, 60, 80] : [30, 40, 55];
      const viewBorder = viewHover ? [120, 150, 200] : [80, 100, 140];
      ink(viewFill).box(viewBtn.box, "fill");
      ink(viewBorder).box(viewBtn.box, "outline");
      ink(viewHover ? [200, 220, 255] : [150, 180, 220]).write(viewText, {
        x: viewBtn.box.x + 6,
        y: viewBtn.box.y + 6,
      });
      
      // BUY button - shows product code and checkout
      const mugCode = selectedMug.code;
      const btnText = buyPending ? "☕ CHECKING OUT..." : `☕ BUY +${mugCode.slice(0, 8)} $18`;
      const btnW = btnText.length * 6 + 16;
      const btnH = 24;
      buyBtn.box.x = screen.width - btnW - 8;
      buyBtn.box.y = previewHeight - btnH - 12;
      buyBtn.box.w = btnW;
      buyBtn.box.h = btnH;
      
      const isHover = buyBtn.down;
      const blink = sin(time * 2.5) * 0.5 + 0.5;
      const breathe = sin(time * 1.5) * 0.2 + 0.8;
      
      // Coffee brown gradient feel
      let fillColor, borderColor, textColor;
      if (buyPending) {
        // Pending state - pulsing
        const pulse = sin(time * 6) * 0.3 + 0.7;
        fillColor = [floor(60 * pulse), floor(50 * pulse), floor(30 * pulse)];
        borderColor = [200, 180, 100];
        textColor = [255, 220, 150];
      } else if (isHover) {
        fillColor = [80, 60, 40];
        borderColor = [255, 200, 100];
        textColor = [255, 230, 180];
      } else {
        fillColor = [floor(45 * breathe), floor(35 * breathe), floor(25 * breathe)];
        borderColor = [floor(139 * blink + 80), floor(90 * blink + 60), floor(43 * blink + 40)];
        textColor = [floor(200 + blink * 55), floor(180 + blink * 40), floor(140 + blink * 30)];
      }
      
      // Button with rounded corners effect (double outline)
      ink(fillColor).box(buyBtn.box, "fill");
      ink(borderColor).box(buyBtn.box, "outline");
      // Inner glow line
      ink(borderColor[0] * 0.5, borderColor[1] * 0.5, borderColor[2] * 0.5, 0.5)
        .box(buyBtn.box.x + 1, buyBtn.box.y + 1, buyBtn.box.w - 2, buyBtn.box.h - 2, "outline");
      
      ink(textColor).write(btnText, {
        x: buyBtn.box.x + 8,
        y: buyBtn.box.y + 8,
      });
      
      // Pre-fetch checkout URL for this mug
      if (!checkoutUrls[mugCode] && !checkoutLoading[mugCode]) {
        fetchCheckoutForMug(selectedMug);
      }
    } else if (preview?.broken) {
      // Broken/missing preview - draw X and "no preview"
      const cx = floor(screen.width / 2);
      const cy = floor(previewHeight / 2) - 10;
      const size = 30;
      ink(80, 50, 50).line(cx - size, cy - size, cx + size, cy + size);
      ink(80, 50, 50).line(cx - size, cy + size, cx + size, cy - size);
      ink(100, 60, 60).write("no preview", { center: "x", y: cy + size + 10, screen });
    } else {
      // Loading indicator
      const pulse = sin(performance.now() / 300) * 0.3 + 0.7;
      ink(100 * pulse, 100 * pulse, 120 * pulse).write("☕ Loading...", { center: "x", y: previewHeight / 2 - 8, screen });
      preloadPreview(selectedMug);
    }
  }
  
  // Auto-play progress bar (below preview, above list)
  const progressBarH = 4;
  const progressBarY = previewHeight - progressBarH;
  
  if (mugs.length > 1 && !loading) {
    // Background track
    ink(30, 30, 40, 0.9).box(0, progressBarY, screen.width, progressBarH, "fill");
    
    // Calculate progress
    let progress = 0;
    const isAutoPlaying = autoPlay && !userInteracted;
    
    if (isAutoPlaying) {
      const elapsed = now - lastAutoAdvance;
      progress = min(1, elapsed / autoPlayInterval);
    } else {
      // Show paused state - subtle pulsing partial bar
      const pausePulse = sin(time * 2) * 0.1 + 0.3;
      progress = pausePulse;
    }
    
    // Progress fill - use mug color
    const mugColorName = selectedMug?.color?.toLowerCase() || "white";
    const barRgb = mugColors[mugColorName] || [150, 150, 150];
    const barW = floor(screen.width * progress);
    
    if (isAutoPlaying) {
      // Active: solid color with slight glow
      ink(barRgb[0], barRgb[1], barRgb[2], 0.9).box(0, progressBarY, barW, progressBarH, "fill");
      // Bright tip
      if (barW > 2) {
        ink(255, 255, 255, 0.7).box(barW - 2, progressBarY, 2, progressBarH, "fill");
      }
    } else {
      // Paused: dimmer, pulsing
      const pauseAlpha = sin(time * 3) * 0.2 + 0.4;
      ink(barRgb[0] * 0.6, barRgb[1] * 0.6, barRgb[2] * 0.6, pauseAlpha).box(0, progressBarY, barW, progressBarH, "fill");
    }
    
    // Auto-play indicator (top-left corner, above progress bar)
    const indicatorY = 4;
    const indicatorX = 6;
    if (isAutoPlaying) {
      // Playing: show ▶ AUTO with pulse
      const autoPulse = sin(time * 2) * 0.2 + 0.8;
      ink(100 * autoPulse, 200 * autoPulse, 100 * autoPulse).write("▶ AUTO", { x: indicatorX, y: indicatorY });
    } else {
      // Paused: show ⏸ PAUSED
      const pausePulse = sin(time * 3) * 0.3 + 0.6;
      ink(200 * pausePulse, 180 * pausePulse, 100 * pausePulse).write("⏸ PAUSED", { x: indicatorX, y: indicatorY });
    }
  }
  
  // Divider line
  ink(scheme.divider).line(0, previewHeight, screen.width, previewHeight);
  
  // === BOTTOM: Scrollable list (MASKED) ===
  const listY = previewHeight + 1;
  const listHeight = screen.height - listY;
  
  // Draw solid dark background for list area (not tinted)
  ink(scheme.baseBg[0], scheme.baseBg[1], scheme.baseBg[2]).box(0, listY, screen.width, listHeight, "fill");
  
  // Mask the list area so items don't draw over the preview
  mask({
    x: 0,
    y: listY,
    width: screen.width,
    height: listHeight,
  });
  
  // Draw list items - OPTIMIZED: only iterate visible range
  const visibleHeight = listHeight - TOP_MARGIN;
  
  // Calculate visible index range based on scroll position
  const firstVisibleIdx = max(0, floor((-scroll - TOP_MARGIN) / ROW_HEIGHT) - 1);
  const lastVisibleIdx = min(mugs.length - 1, ceil((-scroll + visibleHeight) / ROW_HEIGHT) + 1);
  
  for (let i = firstVisibleIdx; i <= lastVisibleIdx; i++) {
    const mug = mugs[i];
    if (!mug) continue;
    
    const itemY = scroll + listY + TOP_MARGIN + (i * ROW_HEIGHT);
    
    // Skip if actually off screen (belt and suspenders)
    if (itemY + ROW_HEIGHT < listY || itemY > screen.height) continue;
    
    const isSelected = i === selectedIndex;
    
    // Enhanced striped rows with more contrast
    const isEven = i % 2 === 0;
    let bgColor;
    if (isSelected) {
      // Selected: brighter with subtle pulse
      const pulse = sin(time * 3) * 0.1 + 0.9;
      bgColor = [floor(45 * pulse), floor(55 * pulse), floor(75 * pulse)];
    } else if (isEven) {
      bgColor = [18, 20, 28]; // Darker stripe
    } else {
      bgColor = [28, 32, 42]; // Lighter stripe
    }
    ink(bgColor[0], bgColor[1], bgColor[2], 0.92).box(0, itemY, screen.width - 6, ROW_HEIGHT, "fill");
    
    // Left accent bar for selected (animated)
    if (isSelected) {
      const accentPulse = sin(time * 4) * 0.3 + 0.7;
      ink(floor(100 * accentPulse), floor(180 * accentPulse), 255).box(0, itemY, 3, ROW_HEIGHT, "fill");
    }
    
    // Get mug info
    const colorRgb = mugColors[mug.color?.toLowerCase()] || [150, 150, 150];
    const displayCode = mug.sourceCode || mug.code;
    const colorText = (mug.color || "white").toLowerCase();
    const timeAgo = getTimeAgo(mug.createdAt);
    
    // Single condensed line: "color mug of CODE in $via · 2h ago"
    let textX = 10;
    const textY = itemY + 7;
    
    ink(...colorRgb).write(colorText, { x: textX, y: textY });
    textX += colorText.length * 6;
    
    ink(scheme.mugOf).write(" mug of ", { x: textX, y: textY });
    textX += " mug of ".length * 6;
    
    ink(scheme.mugCode).write(displayCode, { x: textX, y: textY });
    textX += displayCode.length * 6;
    
    if (mug.via) {
      ink(scheme.mugVia).write(` in $${mug.via}`, { x: textX, y: textY });
      textX += (` in $${mug.via}`).length * 6;
    }
    
    // Time ago - inline after a separator
    if (timeAgo) {
      ink(scheme.timeAgo).write(` · ${timeAgo}`, { x: textX, y: textY });
    }
  }
  
  // Unmask before drawing scrollbar and anything else
  unmask();
  
  // Scrollbar (flush right like colors.mjs)
  const contentHeight = mugs.length * ROW_HEIGHT;
  // visibleHeight already declared above
  
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
  
  // Keyboard navigation (pauses auto-play)
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:w")) {
    selectedIndex = max(0, selectedIndex - 1);
    scrollToSelected(visibleHeight, contentHeight);
    preloadNearby();
    userInteracted = true;
    autoPlayPauseTime = performance.now();
  }
  
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:s")) {
    selectedIndex = min(mugs.length - 1, selectedIndex + 1);
    scrollToSelected(visibleHeight, contentHeight);
    preloadNearby();
    userInteracted = true;
    autoPlayPauseTime = performance.now();
  }
  
  // Enter opens
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:space")) {
    openSelectedMug(jump);
  }
  
  // VIEW button click - go to mug page
  viewBtn?.act(e, {
    down: () => {
      sound?.synth({ type: "sine", tone: 500, duration: 0.04, volume: 0.25 });
    },
    push: () => {
      const mug = mugs[selectedIndex];
      if (mug) {
        sound?.synth({ type: "sine", tone: 700, duration: 0.08, volume: 0.3 });
        jump("mug~+" + mug.code);
      }
    },
  });
  
  // BUY button click - checkout
  buyBtn?.act(e, {
    down: () => {
      sound?.synth({ type: "sine", tone: 440, duration: 0.05, volume: 0.3 });
    },
    push: () => {
      if (buyPending) return;
      
      const mug = mugs[selectedIndex];
      if (!mug) return;
      
      const url = checkoutUrls[mug.code];
      if (url) {
        // Instant redirect!
        sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
        jump(url);
      } else {
        // URL still loading - show pending state
        buyPending = true;
        sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.3 });
        waitForCheckout(mug, jump, sound);
      }
    },
  });
  
  // Click to select item in list (pauses auto-play)
  if (e.is("touch") && e.y > listY) {
    const clickedIndex = floor((e.y - listY - TOP_MARGIN - scroll) / ROW_HEIGHT);
    if (clickedIndex >= 0 && clickedIndex < mugs.length) {
      selectedIndex = clickedIndex;
      preloadNearby();
      userInteracted = true;
      autoPlayPauseTime = performance.now();
    }
  }
  
  // Scrolling pauses auto-play
  if (e.is("scroll") || e.is("draw")) {
    userInteracted = true;
    autoPlayPauseTime = performance.now();
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

// Pre-fetch checkout URL for a mug
async function fetchCheckoutForMug(mug) {
  const code = mug.code;
  if (checkoutUrls[code] || checkoutLoading[code]) return;
  
  checkoutLoading[code] = true;
  
  try {
    const headers = { "Content-Type": "application/json" };
    const token = await apiRef?.authorize?.();
    if (token) headers.Authorization = `Bearer ${token}`;
    
    // Build return slug
    let returnSlug = `mug~${mug.sourceCode || code}~${mug.color || "white"}`;
    if (mug.via) {
      returnSlug += `~via~${mug.via}`;
    }
    
    // Build API URL
    let apiUrl = `/api/mug?new=true&pixels=${mug.sourceCode || code}.png&color=${mug.color || "white"}`;
    if (mug.via) {
      apiUrl += `&via=${mug.via}`;
    }
    
    const res = await fetch(apiUrl, {
      method: "POST",
      headers,
      body: JSON.stringify({ quantity: 1, slug: returnSlug }),
    });
    
    if (res.ok) {
      const data = await res.json();
      if (data?.location) {
        checkoutUrls[code] = data.location;
      }
    }
  } catch (e) {
    console.warn("Failed to fetch checkout for mug:", code, e);
  } finally {
    checkoutLoading[code] = false;
  }
}

// Poll for checkout URL if user clicked before it was ready
async function waitForCheckout(mug, jump, sound) {
  const code = mug.code;
  const maxWait = 10000; // 10 seconds max
  const startTime = Date.now();
  
  // Ensure we're fetching
  if (!checkoutUrls[code] && !checkoutLoading[code]) {
    fetchCheckoutForMug(mug);
  }
  
  while (!checkoutUrls[code] && Date.now() - startTime < maxWait) {
    await new Promise(r => setTimeout(r, 100));
  }
  
  buyPending = false;
  
  if (checkoutUrls[code]) {
    sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
    jump(checkoutUrls[code]);
  } else {
    sound?.synth({ type: "square", tone: 200, duration: 0.15, volume: 0.3 });
  }
}

function meta() {
  return {
    title: "Recent Mugs",
    desc: "Browse and purchase recently created mugs",
  };
}

export { boot, paint, act, meta };
