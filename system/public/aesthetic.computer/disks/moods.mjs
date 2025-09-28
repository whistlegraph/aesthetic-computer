// Moods, 2023.9.28.01.40.14.735
// A live list of all our moods.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Move extra long moods left and right horizontally.
  - [] Add time information.
  - [] Filter out new line characters.
  + Done
  - [x] Make moods scrollable by line.
  - [x] Render moods.
#endregion */

let moods;
let originalMoodCount = 0; // Track total number of moods fetched
let totalContentItems = 0; // Track total items including date headers

let retrieving = true,
  failed = false;

let ellipsisTicker; // For animated "Retrieving..." text

let scroll = 0; // Start immediately, no delay
let scale = 1;

// NEW ARCHITECTURE: Virtual Viewport System
let allItems = [], // All items in one continuous list (moods + date headers)
  itemBuffers = new Map(), // Pre-rendered buffers for all items
  itemPositions = [], // Fixed Y positions for each item
  viewport = { // Current visible area
    top: 0,
    bottom: 0,
    firstVisibleIndex: 0,
    lastVisibleIndex: 0
  },
  contentHeight = 0, // Total height of all content
  viewportY = 48, // Starting Y position for content
  itemRowHeight = 0; // Height of each row

const blockWidth = 6 * scale;
let bounceCount = 1;

let calcItemPositions; // Function to calculate item positions

// Helper function to get day color theme
function getDayColorTheme(dateString, isDark = true) {
  // Create a simple hash from the date string for consistent colors
  let hash = 0;
  for (let i = 0; i < dateString.length; i++) {
    hash = ((hash << 5) - hash + dateString.charCodeAt(i)) & 0xffffffff;
  }
  
  const themes = isDark ? scheme.dark.dayThemes : scheme.light.dayThemes;
  return themes[Math.abs(hash) % themes.length];
}

const { floor, min, abs, ceil, sin } = Math;

// Track current day theme for mood backgrounds
let currentDayTheme = null;

// Color schemes for light and dark modes
const scheme = {
  dark: {
    background: [0],
    retrievingText: [64, 127],
    failedText: "red",
    moodBackgroundDefault: [64, 64, 80],
    moodBackgroundFocal: [200, 180, 60],
    labelBackground: [230, 230, 230],
    labelBackgroundFocal: [255, 255, 100],
    timestampDefault: [160, 160, 160],
    timestampFocal: [220, 220, 220],
    handleDefault: [180, 200, 255],
    handleFocal: [200, 220, 255],
    moodTextDefault: "red",
    moodTextFocal: undefined,
    dateBackground: [0, 0, 0],
    dateText: [64, 64, 64],
    scrollBarTrack: [64, 64, 64],
    scrollBarThumb: [128, 128, 255],
    divider: [48, 48, 48],
    dayThemes: [
      { header: [100, 120, 180], bg: [40, 45, 65] },   // Blue theme
      { header: [180, 100, 120], bg: [65, 40, 45] },   // Red theme  
      { header: [120, 180, 100], bg: [45, 65, 40] },   // Green theme
      { header: [180, 160, 100], bg: [65, 60, 40] },   // Yellow theme
      { header: [160, 100, 180], bg: [60, 40, 65] },   // Purple theme
      { header: [100, 180, 160], bg: [40, 65, 60] },   // Cyan theme
      { header: [180, 140, 100], bg: [65, 55, 40] },   // Orange theme
    ]
  },
  light: {
    background: [240, 240, 240],
    retrievingText: [80, 80, 80],
    failedText: "darkred",
    moodBackgroundDefault: [200, 200, 220],
    moodBackgroundFocal: [255, 255, 150],
    labelBackground: [240, 240, 240],
    labelBackgroundFocal: [255, 255, 255],
    timestampDefault: [60, 60, 60],
    timestampFocal: [20, 20, 20],
    handleDefault: [80, 100, 160],
    handleFocal: [40, 60, 120],
    moodTextDefault: "darkblue",
    moodTextFocal: "black",
    dateBackground: [255, 255, 255],
    dateText: [120, 120, 120],
    scrollBarTrack: [180, 180, 180],
    scrollBarThumb: [100, 100, 200],
    divider: [200, 200, 200],
    dayThemes: [
      { header: [200, 220, 255], bg: [220, 225, 240] },   // Light Blue theme
      { header: [255, 200, 220], bg: [240, 220, 225] },   // Light Red theme  
      { header: [220, 255, 200], bg: [225, 240, 220] },   // Light Green theme
      { header: [255, 240, 200], bg: [240, 235, 220] },   // Light Yellow theme
      { header: [240, 200, 255], bg: [235, 220, 240] },   // Light Purple theme
      { header: [200, 255, 240], bg: [220, 240, 235] },   // Light Cyan theme
      { header: [255, 220, 200], bg: [240, 230, 220] },   // Light Orange theme
    ]
  }
};

// 🥾 Boot
function boot({ wipe, screen, colon, params, typeface, gizmo }) {
  scale = parseInt(colon[0]) || 1;
  // Calculate row heights for new architecture
  const labelPadding = 4 * scale;
  itemRowHeight = (12 * scale) + (6 * scale) + labelPadding; // Height of each mood row
  if (scale === 2) viewportY *= scale / 1.5;
  wipe(0);
  
  // Initialize ellipsis ticker for "Retrieving..." animation
  ellipsisTicker = new gizmo.EllipsisTicker();
  let query = `/api/mood/all`;
  if (params.length > 0) query += `?for=${params.join(',')}`;
  fetch(query)
    .then((res) => res.json())
    .then((body) => {
      if (body.moods) {
        moods = body.moods;
        originalMoodCount = body.moods.length; // Store the original total count
        scroll = 0; // Reset scroll position to start
        // console.log("😃 Moods:", moods);
        calcItemPositions = (screen) => {
          // NEW: Build continuous item list with fixed positions
          allItems = [];
          itemPositions = [];
          let currentY = viewportY;
          let lastDate = null;
          
          moods.forEach(mood => {
            const moodDateObj = new Date(mood.when);
            const moodDate = moodDateObj.toLocaleDateString('en-US', { 
              weekday: 'long', 
              year: 'numeric', 
              month: 'long', 
              day: 'numeric' 
            });
            
            // Add date header if needed
            if (lastDate !== moodDate) {
              allItems.push({ type: 'date', date: moodDate });
              itemPositions.push(currentY);
              currentY += itemRowHeight + (16 * scale) + (4 * scale); // Date height + spacing
              lastDate = moodDate;
            }
            
            // Add mood item
            allItems.push({ type: 'mood', ...mood });
            itemPositions.push(currentY);
            currentY += itemRowHeight; // Mood height
          });
          
          // Store total content dimensions
          totalContentItems = allItems.length;
          contentHeight = currentY - viewportY;
          

        };
        calcItemPositions(screen);
        retrieving = false;
      } else {
        throw new Error(body.message);
      }
    })
    .catch((err) => {
      failed = true;
      retrieving = false;
      console.warn("📶🙁 Mood error:", err);
    });
}

// 🎨 Paint - NEW VIRTUAL VIEWPORT ARCHITECTURE
function paint({ wipe, ink, text, pan, unpan, screen, num, help: { choose, repeat }, painting, paste, net, clock, dark }) {
  const currentScheme = dark ? scheme.dark : scheme.light;
  
  scale > 1 ? ink(0, 64).box(screen) : wipe(...currentScheme.background);
  if (retrieving) {
    const ellipsis = ellipsisTicker ? ellipsisTicker.text(repeat) : "...";
    ink(...currentScheme.retrievingText).write(`Retrieving${ellipsis}`, { center: "xy" });
  }
  if (failed) ink(currentScheme.failedText).write("failed", { center: "xy" });
  if (allItems.length > 0) {
    // NEW: Update viewport based on scroll position
    updateViewport(screen);
    
    // NEW: Render only visible items using smooth transforms
    renderVisibleItems({ ink, text, screen, painting, paste, num, dark });
    
    // NEW: Smooth continuous scrolling
    scroll += 0.25; // Constant scroll speed
    
    // NEW: Loop back to beginning when we reach the end
    if (scroll >= contentHeight) {
      scroll = 0;
    }
    
    bounceCount += 1;
  }
  
  // Fixed position scroll bar for new architecture
  if (allItems.length > 0) {
    const progressHeight = screen.height;
    const maxScrollableItems = Math.max(0, totalContentItems - Math.floor(screen.height / itemRowHeight));
    const scrollProgress = maxScrollableItems > 0 ? (scroll / contentHeight) : 0;
    const scrollPercentage = Math.min(100, scrollProgress * 100);
    
    const thumbHeight = Math.max(8, Math.floor((screen.height / totalContentItems) * progressHeight));
    const availableThumbSpace = progressHeight - thumbHeight;
    const thumbPosition = Math.floor(scrollProgress * availableThumbSpace);
    
    const progressX = screen.width - 4;
    const progressY = 0;
    
    ink(...currentScheme.scrollBarTrack).box(progressX, progressY, 4, progressHeight);
    ink(...currentScheme.scrollBarThumb).box(progressX, progressY + thumbPosition, 4, thumbHeight);
  }
}


// NEW: Virtual Viewport Functions
function updateViewport(screen) {
  // Calculate which items should be visible based on scroll position
  viewport.top = scroll;
  viewport.bottom = scroll + screen.height;
  
  // Find first and last visible items
  viewport.firstVisibleIndex = 0;
  viewport.lastVisibleIndex = allItems.length - 1;
  
  for (let i = 0; i < itemPositions.length; i++) {
    const itemY = itemPositions[i];
    const itemHeight = allItems[i].type === 'date' ? itemRowHeight + (16 * scale) + (4 * scale) : itemRowHeight;
    
    if (itemY + itemHeight >= viewport.top && viewport.firstVisibleIndex === 0) {
      viewport.firstVisibleIndex = i;
    }
    if (itemY <= viewport.bottom) {
      viewport.lastVisibleIndex = i;
    }
  }
  
  // Add buffer for smooth rendering
  viewport.firstVisibleIndex = Math.max(0, viewport.firstVisibleIndex - 2);
  viewport.lastVisibleIndex = Math.min(allItems.length - 1, viewport.lastVisibleIndex + 2);
}

function renderVisibleItems({ ink, text, screen, painting, paste, num, dark }) {
  const screenCenter = screen.height / 2;
  let closestMoodIndex = -1;
  let closestDistance = Infinity;
  
  // Find the mood closest to screen center for highlighting
  for (let i = viewport.firstVisibleIndex; i <= viewport.lastVisibleIndex; i++) {
    const item = allItems[i];
    if (item.type === 'mood') {
      const itemY = itemPositions[i] - scroll;
      const itemCenter = itemY + (itemRowHeight / 2);
      const distance = Math.abs(itemCenter - screenCenter);
      
      if (distance < closestDistance) {
        closestDistance = distance;
        closestMoodIndex = i;
      }
    }
  }
  
  // Render visible items
  for (let i = viewport.firstVisibleIndex; i <= viewport.lastVisibleIndex; i++) {
    const item = allItems[i];
    const itemY = itemPositions[i] - scroll; // Transform-based positioning
    
    // Skip items that are completely off-screen
    const itemHeight = item.type === 'date' ? itemRowHeight + (16 * scale) + (4 * scale) : itemRowHeight;
    if (itemY + itemHeight < 0 || itemY > screen.height) continue;
    
    if (item.type === 'date') {
      renderDateItem(item, itemY, { ink, text, screen, dark });
    } else {
      renderMoodItem(item, itemY, i === closestMoodIndex, { ink, text, screen, painting, paste, num, dark });
    }
  }
}

function renderDateItem(item, y, { ink, text, screen, dark }) {
  const currentScheme = dark ? scheme.dark : scheme.light;
  
  // Get color theme for this day
  const dayTheme = getDayColorTheme(item.date, dark);
  
  // Calculate dimensions
  const dateHeaderHeight = 16 * scale;
  const dateSpacing = 4 * scale;
  const fullBackgroundWidth = screen.width - 4;
  const fullDateHeight = itemRowHeight + dateHeaderHeight + dateSpacing;
  
  // Draw background
  ink(...currentScheme.dateBackground).box(0, y, fullBackgroundWidth, fullDateHeight);
  
  // Draw centered date text
  const tabHeight = 20;
  const tabY = y + fullDateHeight - tabHeight;
  ink(...currentScheme.dateText).write(item.date, { 
    x: screen.width / 2, 
    y: tabY - 6, 
    size: 2, 
    center: "x" 
  }, undefined, undefined, false, "MatrixChunky8");
}

function renderMoodItem(item, y, isFocal, { ink, text, screen, painting, paste, num, dark }) {
  const currentScheme = dark ? scheme.dark : scheme.light;
  const mood = item.mood.trim().replace(/\n/g, ' ').replace(/\r/g, '');
  const timestamp = new Date(item.when).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
  
  // Layout calculations
  const hb = text.box(item.handle, undefined, undefined, scale, "MatrixChunky8").box.width;
  const timeWidth = text.box(timestamp, undefined, undefined, scale).box.width;
  const maxHandleWidth = text.box("WWWWWWWWWWWWWWWW", undefined, undefined, scale, "MatrixChunky8").box.width;
  const labelWidth = Math.max(timeWidth, maxHandleWidth) + 20;
  
  const moodStartX = 4;
  const moodEndX = screen.width - labelWidth - 16;
  const moodWidth = moodEndX - moodStartX;
  const labelStartX = moodEndX + 8;
  const timeX = labelStartX + 4;
  
  // Create or get cached buffer
  const moodKey = `${item.handle}_${item.when}_${mood.substring(0, 50)}_${dark}`;
  let moodBuffer = itemBuffers.get(moodKey);
  
  if (!moodBuffer) {
    const labelPadding = 4 * scale;
    const bufferHeight = (12 * scale) + (6 * scale) + labelPadding;
    
    moodBuffer = painting(moodWidth, bufferHeight, (api) => {
      api.wipe(...currentScheme.moodBackgroundDefault); // Themed background
    });
    
    itemBuffers.set(moodKey, moodBuffer);
  }
  
  // Calculate marquee animation
  const mb = text.box(mood, undefined, undefined, scale, "unifont").box.width;
  let textX = 0;
  if (mb > moodWidth) {
    const gap = mb - moodWidth;
    const osc = (sin(num.radians(bounceCount % 360)) + 1) / 2;
    textX = -osc * gap;
  }
  
  // Create animated buffer
  const labelPadding = 4 * scale;
  const bufferHeight = (12 * scale) + (6 * scale) + labelPadding;
  
  const animatedBuffer = painting(moodWidth, bufferHeight, (api) => {
    const moodColor = isFocal ? currentScheme.moodTextFocal : currentScheme.moodTextDefault;
    api.ink(moodColor).write(mood, { x: textX, y: 2, size: scale }, undefined, undefined, false, "unifont");
  });
  
  // Background colors
  const bgColor = isFocal ? currentScheme.moodBackgroundFocal : currentScheme.moodBackgroundDefault;
  const labelBgColor = isFocal ? currentScheme.labelBackgroundFocal : currentScheme.labelBackground;
  
  // Draw backgrounds
  const moodBackgroundWidth = labelStartX;
  ink(...bgColor).box(0, y, moodBackgroundWidth, bufferHeight);
  ink(...labelBgColor).box(labelStartX, y, labelWidth, bufferHeight);
  
  // Draw text
  const timestampColor = isFocal ? currentScheme.timestampFocal : currentScheme.timestampDefault;
  const handleColor = isFocal ? currentScheme.handleFocal : currentScheme.handleDefault;
  
  ink(...timestampColor).write(timestamp, { x: timeX, y: y + 2, size: scale });
  ink(...handleColor).write(item.handle, { x: timeX, y: y + 1 + (12 * scale) }, undefined, undefined, false, "MatrixChunky8");
  
  // Paste mood buffer
  paste(animatedBuffer, moodStartX, y);
  
  // Draw horizontal divider at the bottom
  const dividerY = y + bufferHeight - 1;
  ink(...currentScheme.divider).box(0, dividerY, screen.width - 4, 1);
}

// 🎪 Act
function act({ event: e, screen }) {
  if (e.is("reframed")) calcItemPositions?.(screen);
}

// 🧮 Sim
function sim({ clock }) {
  if (scroll < 0) bounceCount += 0.25;
  // Update ellipsis ticker for "Retrieving..." animation
  ellipsisTicker?.update(clock.time());
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Moods",
    desc: "A live list of all our moods.",
  };
}

// 🖼️ Preview
function preview({ wipe, slug }) {
  wipe("purple").ink("red").write(slug, { center: "xy", size: 1 });
}

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
