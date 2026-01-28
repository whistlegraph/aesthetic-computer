// Moods, 2023.9.28.01.40.14.735
// A live list of all our moods.

import { colorizeText, hasHotlinks, parseMessageElements, calculateElementPosition, isClickInsideElement, getElementAction } from "../lib/hotlink.mjs";

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

// Single mood view (permalink mode)
let singleMoodView = false;
let singleMood = null;

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

  // Check for single mood permalink: moods~@handle~rkey or moods~handle~rkey
  // params would be ['@handle', 'rkey'] or ['handle', 'rkey']
  if (params.length === 2 && params[1].length > 5) {
    // Looks like a permalink (second param is rkey which is ~13 chars)
    singleMoodView = true;
    const handle = params[0].startsWith('@') ? params[0] : `@${params[0]}`;
    const rkey = params[1];
    
    fetch(`/api/mood/single?handle=${encodeURIComponent(handle)}&rkey=${encodeURIComponent(rkey)}`)
      .then((res) => res.json())
      .then((body) => {
        if (body.mood) {
          singleMood = body;
          retrieving = false;
        } else {
          throw new Error(body.message || "Mood not found");
        }
      })
      .catch((err) => {
        failed = true;
        retrieving = false;
        console.warn("📶🙁 Single mood error:", err);
      });
    return; // Exit early, don't fetch all moods
  }

  // Regular list view
  singleMoodView = false;
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

  // Single mood permalink view
  if (singleMoodView && singleMood) {
    renderSingleMood({ ink, text, screen, dark });
    return;
  }

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

// Render single mood permalink view
function renderSingleMood({ ink, text, screen, dark }) {
  const currentScheme = dark ? scheme.dark : scheme.light;
  const mood = singleMood;
  
  // Background
  ink(...currentScheme.background).box(0, 0, screen.width, screen.height);
  
  // Clean mood text
  const moodText = mood.mood
    .trim()
    .replace(/[\n\r]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
  
  // Format timestamp
  const date = new Date(mood.when);
  const dateStr = date.toLocaleDateString('en-US', { 
    weekday: 'long', 
    year: 'numeric', 
    month: 'long', 
    day: 'numeric' 
  });
  const timeStr = date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
  
  // Layout
  const centerX = screen.width / 2;
  let y = 40;
  
  // Handle
  ink(...currentScheme.handleDefault).write(mood.handle, { 
    x: centerX, 
    y, 
    size: 2, 
    center: "x" 
  }, undefined, undefined, false, "MatrixChunky8");
  y += 30;
  
  // Mood text (larger, centered, with wrapping consideration)
  const moodColor = dark ? [255, 255, 255] : [30, 30, 30];
  
  // Simple word wrap for long moods
  const maxWidth = screen.width - 40;
  const words = moodText.split(' ');
  let lines = [];
  let currentLine = '';
  
  words.forEach(word => {
    const testLine = currentLine ? `${currentLine} ${word}` : word;
    const lineWidth = text.box(testLine, undefined, undefined, 2, "unifont").box.width;
    if (lineWidth > maxWidth && currentLine) {
      lines.push(currentLine);
      currentLine = word;
    } else {
      currentLine = testLine;
    }
  });
  if (currentLine) lines.push(currentLine);
  
  // Limit to 5 lines max
  if (lines.length > 5) {
    lines = lines.slice(0, 5);
    lines[4] = lines[4].substring(0, lines[4].length - 3) + '...';
  }
  
  // Render mood lines
  lines.forEach(line => {
    ink(...moodColor).write(line, { 
      x: centerX, 
      y, 
      size: 2, 
      center: "x" 
    }, undefined, undefined, false, "unifont");
    y += 28;
  });
  
  y += 10;
  
  // Date and time
  ink(...currentScheme.timestampDefault).write(`${dateStr} at ${timeStr}`, { 
    x: centerX, 
    y, 
    size: 1, 
    center: "x" 
  });
  y += 30;
  
  // Engagement stats (if available from Bluesky)
  if (mood.engagement) {
    const { likes, reposts, replies, webUrl } = mood.engagement;
    
    // Stats line
    const statsText = `♥ ${likes}  ↻ ${reposts}  💬 ${replies?.length || 0}`;
    ink(255, 100, 150).write(statsText, { 
      x: centerX, 
      y, 
      size: 1, 
      center: "x" 
    });
    y += 25;
    
    // Replies
    if (replies && replies.length > 0) {
      ink(...currentScheme.divider).box(20, y, screen.width - 40, 1);
      y += 15;
      
      ink(...currentScheme.timestampDefault).write("Replies from Bluesky:", { 
        x: 20, 
        y 
      });
      y += 20;
      
      // Show up to 5 replies
      const displayReplies = replies.slice(0, 5);
      displayReplies.forEach(reply => {
        // Author
        ink(100, 150, 255).write(`@${reply.author.handle}`, { 
          x: 20, 
          y, 
          size: 1 
        }, undefined, undefined, false, "MatrixChunky8");
        y += 14;
        
        // Reply text (truncate if too long)
        let replyText = reply.text.replace(/[\n\r]+/g, ' ').trim();
        if (replyText.length > 80) {
          replyText = replyText.substring(0, 77) + '...';
        }
        ink(...moodColor).write(replyText, { 
          x: 20, 
          y, 
          size: 1 
        });
        y += 18;
      });
      
      if (replies.length > 5) {
        ink(...currentScheme.timestampDefault).write(`+ ${replies.length - 5} more replies`, { 
          x: 20, 
          y 
        });
        y += 18;
      }
    }
    
    // Link to Bluesky
    if (webUrl) {
      y = screen.height - 30;
      ink(100, 150, 255).write("View on Bluesky ↗", { 
        x: centerX, 
        y, 
        size: 1, 
        center: "x" 
      });
    }
  } else if (mood.bluesky) {
    // Has Bluesky mirror but no engagement fetched yet
    ink(...currentScheme.timestampDefault).write("Mirrored to Bluesky", { 
      x: centerX, 
      y, 
      size: 1, 
      center: "x" 
    });
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
  // Store original y for logging
  const originalY = y;
  
  // Round y to whole pixels to prevent subpixel rendering flicker
  y = Math.floor(y);
  
  const currentScheme = dark ? scheme.dark : scheme.light;
  // Clean mood text: remove newlines, carriage returns, and multiple spaces
  const mood = item.mood
    .trim()
    .replace(/[\n\r]+/g, ' ')  // Replace newlines/returns with single space
    .replace(/\s+/g, ' ')      // Collapse multiple spaces into one
    .trim();
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
  
  // Simple static text without marquee animation
  const labelPadding = 4 * scale;
  const bufferHeight = (12 * scale) + (6 * scale) + labelPadding;
  
  // Calculate marquee animation with proper width measurement
  const mb = text.box(mood, undefined, undefined, scale, "unifont").box.width;
  let textX = 0;
  
  // Always add subtle bounce for visual interest, even on short text
  const bounceAmount = mb > moodWidth ? (mb - moodWidth) : 4; // At least 4px bounce
  const osc = (sin(num.radians(bounceCount % 360)) + 1) / 2;
  textX = Math.floor(-osc * bounceAmount);
  
  const animatedBuffer = painting(moodWidth, bufferHeight, (api) => {
    const moodColor = isFocal ? currentScheme.moodTextFocal : currentScheme.moodTextDefault;
    // Colorize mood text with hotlinks (handles, URLs, prompts, etc.)
    const coloredMood = colorizeText(mood, moodColor);
    // Marquee text with rounded x position
    api.ink(moodColor).write(coloredMood, { x: textX, y: 2, size: scale }, undefined, undefined, false, "unifont");
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
let pendingJump = null; // Store jump action for confirmation

function act({ event: e, screen, jump, text }) {
  if (e.is("reframed")) calcItemPositions?.(screen);
  
  // Handle clicks on mood text for hotlinks
  if (e.is("lift") && allItems.length > 0) {
    // Find which item was clicked based on Y position
    const clickY = e.y + Math.abs(scroll);
    
    for (let i = 0; i < allItems.length; i++) {
      const item = allItems[i];
      if (item.type !== 'mood') continue;
      
      const itemY = itemPositions[i];
      const itemHeight = itemRowHeight;
      
      // Check if click is within this mood's bounds
      if (clickY >= itemY && clickY < itemY + itemHeight) {
        // Clean the mood text the same way as in render
        const mood = item.mood
          .trim()
          .replace(/[\n\r]+/g, ' ')
          .replace(/\s+/g, ' ')
          .trim();
        
        // Check if click hit a hotlink
        const elements = parseMessageElements(mood);
        if (elements.length > 0) {
          // Calculate relative position within mood text area
          const moodStartX = 4;
          const relativeX = e.x - moodStartX;
          const relativeY = clickY - itemY;
          
          // Get text lines (single line for mood)
          const textLines = [mood];
          
          for (const element of elements) {
            const positions = calculateElementPosition(
              element, mood, textLines, text, itemRowHeight, "unifont"
            );
            
            if (positions && isClickInsideElement(relativeX, relativeY, positions, text, "unifont")) {
              const action = getElementAction(element, mood, elements);
              if (action && action.jumpTarget) {
                jump(action.jumpTarget);
                break;
              }
            }
          }
        }
        break; // Found the clicked mood, stop searching
      }
    }
  }
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
  if (singleMoodView && singleMood) {
    // Truncate mood for title
    let moodPreview = singleMood.mood.replace(/[\n\r]+/g, ' ').trim();
    if (moodPreview.length > 50) {
      moodPreview = moodPreview.substring(0, 47) + '...';
    }
    return {
      title: `${singleMood.handle}: "${moodPreview}"`,
      desc: `Mood from ${singleMood.handle} on Aesthetic Computer`,
    };
  }
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
