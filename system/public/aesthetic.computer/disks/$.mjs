// $, 2025.8.29.15.30
// A live feed of recent KidLisp cached codes.
// Updated to use new $code shorthand for simplified preview execution.

import { isKidlispSource, getSyntaxHighlightingColors, tokenize, KidLisp } from "../lib/kidlisp.mjs";

/* #region 📚 README 
  Shows a scrolling feed of recently cached KidLisp codes with their handles,
  similar to the moods feed. Useful for discovering new code snippets and
  seeing what the community is creating.
#endregion */

/* #region 🏁 TODO 
  - [] Add click interaction to copy codes or navigate to them
  - [] Add relative timestamps ("2h ago", "1d ago")
  - [] Add filter by popularity or user
  - [] Add real-time updates via polling
  + Done
  - [x] Fetch recent codes from API
  - [x] Display scrolling feed like moods
  - [x] Show handles with codes
#endregion */

let codes;

let retrieving = true,
  failed = false;

let scroll = 0; // Use smooth scroll like colors.mjs
let scale = 1;

let codeEntries = [];
let buttons = [];

// Preview system for embedded KidLisp - simplified with $code shorthand
let selectedEntry = null;

// Marquee scrolling state
let marqueeOffsets = new Map(); // Track scroll offset for each entry
let lastMarqueeUpdate = 0;
const MARQUEE_SPEED = 0.5; // Pixels per frame
const MARQUEE_PAUSE_TIME = 60; // Frames to pause at start/end

// Layout constants (similar to colors.mjs)
const LEFT_MARGIN = 6;
const TOP_MARGIN = 4;
const ROW_HEIGHT = 40; // Height of each entry row

// Layout state for inbox interface
let leftPanelWidth = 300;
let selectedIndex = -1;

// Preview panel buttons (stored at module level for act function access)
let copyButton = null;
let runButton = null;
let closeButton = null;

// Responsive breakpoints
const MOBILE_BREAKPOINT = 600;
const TABLET_BREAKPOINT = 900;

const { floor, min, abs, ceil, sin } = Math;

// 🥾 Boot
function boot({ wipe, screen, colon, params, typeface, ui: { Button } }) {
  scale = parseInt(colon[0]) || 1;
  wipe(0);
  
  const limit = params[0] ? parseInt(params[0]) : 30;
  let query = `/api/store-kidlisp?recent=true&limit=${limit}`;
  
  fetch(query)
    .then((res) => res.json())
    .then((body) => {
      if (body.recent) {
        codes = body.recent;
        console.log("💾 Recent codes:", codes);
        
        // Build structured entries like colors.mjs
        codeEntries = codes.map((c, index) => {
          const codeText = `$${c.code}`;
          const preview = c.preview.trim();
          const handle = c.handle || "anon";
          const hits = `${c.hits} hits`;
          
          const entry = {
            code: c.code,
            codeText,
            preview,
            handle,
            hits,
            hitCount: c.hits,
            timestamp: c.timestamp,
          };
          
          // Only create UI button if ui is available
          if (Button) {
            // Calculate button dimensions
            const [gw, gh] = typeface.glyphs[0].resolution;
            const w = gw * codeText.length;
            const h = gh + 1;
            
            entry.button = new Button(
              LEFT_MARGIN,
              scroll + TOP_MARGIN + ROW_HEIGHT * index,
              Math.max(screen.width - 60, w), // Leave space for margin
              ROW_HEIGHT - 2 // Leave small gap between rows
            );
          }
          
          return entry;
        });
        
        // Initialize button array (filter out entries without buttons)
        buttons = codeEntries.filter(entry => entry.button).map(entry => entry.button);
        
        // Select first entry by default
        if (codeEntries.length > 0) {
          selectedEntry = codeEntries[0];
          selectedIndex = 0;
        }
        
        retrieving = false;
      } else {
        throw new Error(body.message || 'Failed to fetch codes');
      }
    })
    .catch((err) => {
      failed = true;
      retrieving = false;
      console.warn("📶💾 Codes error:", err);
    });
}

// Function to render syntax-highlighted marquee text
function renderSyntaxMarquee(code, previewText, x, y, maxWidth, scale, ink, frameCount) {
  if (!previewText || previewText.length === 0) return;
  
  // Better approach: preserve the original text structure more carefully
  let compressedText;
  if (isKidlispSource(previewText)) {
    // For KidLisp, just replace newlines with spaces but preserve other spacing
    compressedText = previewText.replace(/\n+/g, ' ').trim();
    // Don't collapse all spaces - this was breaking tokenization
  } else {
    // For non-KidLisp text, full compression is fine
    compressedText = previewText.replace(/\n+/g, ' ').replace(/\s+/g, ' ').trim();
  }
  
  // Get or initialize marquee state for this code
  const marqueeKey = `${code}-${y}`; // Use y position to handle multiple instances
  if (!marqueeOffsets.has(marqueeKey)) {
    marqueeOffsets.set(marqueeKey, { offset: 0, pauseCounter: MARQUEE_PAUSE_TIME });
  }
  
  const marqueeState = marqueeOffsets.get(marqueeKey);
  
  // Calculate text width (approximate)
  const charWidth = 6 * scale; // Approximate character width
  const textWidth = compressedText.length * charWidth;
  
  // Only scroll if text is longer than available width
  if (textWidth <= maxWidth) {
    // Text fits, just render normally with syntax highlighting
    if (isKidlispSource(compressedText)) {
      renderSyntaxHighlightedText(compressedText, x, y, scale, ink);
    } else {
      ink([160, 160, 160]).write(compressedText, { x, y, size: scale });
    }
    return;
  }
  
  // Update marquee position for smooth looping like prompt.mjs
  if (marqueeState.pauseCounter > 0) {
    marqueeState.pauseCounter--;
  } else {
    marqueeState.offset += MARQUEE_SPEED;
    
    // Create seamless loop - reset when we've scrolled one full width
    const loopWidth = textWidth + (20 * scale); // Add spacing between loops
    if (marqueeState.offset > loopWidth) {
      marqueeState.offset = 0;
      marqueeState.pauseCounter = MARQUEE_PAUSE_TIME;
    }
  }
  
  // Render multiple copies for seamless looping effect like prompt.mjs history
  const startX = x - marqueeState.offset;
  const loopSpacing = 20 * scale;
  
  // Render primary text
  if (isKidlispSource(compressedText)) {
    renderSyntaxHighlightedText(compressedText, startX, y, scale, ink, maxWidth);
  } else {
    ink([160, 160, 160]).write(compressedText, { x: startX, y, size: scale });
  }
  
  // Render loop copy if needed for seamless scrolling
  const loopX = startX + textWidth + loopSpacing;
  if (loopX < x + maxWidth) {
    if (isKidlispSource(compressedText)) {
      renderSyntaxHighlightedText(compressedText, loopX, y, scale, ink, maxWidth);
    } else {
      ink([160, 160, 160]).write(compressedText, { x: loopX, y, size: scale });
    }
  }
}

// Function to render syntax-highlighted text using the same logic as the prompt
function renderSyntaxHighlightedText(source, x, y, scale, ink, maxWidth = null) {
  try {
    // Use the same tokenizer and coloring logic as the KidLisp prompt
    const tokens = tokenize(source);
    
    // Create a temporary KidLisp instance to get colors the same way as the prompt
    const tempKidlisp = new KidLisp();
    
    let currentX = x;
    const charWidth = 6 * scale;
    
    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];
      
      // Skip if we're beyond the visible area
      if (maxWidth !== null && currentX - x > maxWidth) break;
      
      // Get the color using the same method as the prompt, with safe fallback
      let colorStr;
      try {
        colorStr = tempKidlisp.getTokenColor(token, tokens, i);
      } catch (error) {
        colorStr = null;
      }
      
      // Handle the color safely
      let inkColor = [160, 160, 160]; // Default fallback
      
      if (colorStr && typeof colorStr === 'string') {
        if (colorStr.includes(',')) {
          // RGB format like "255,0,0"
          try {
            const [r, g, b] = colorStr.split(',').map(c => parseInt(c.trim()));
            inkColor = [r || 160, g || 160, b || 160];
          } catch (error) {
            inkColor = [160, 160, 160];
          }
        } else {
          // Named color - let ink() handle it
          inkColor = colorStr;
        }
      } else if (colorStr === null || colorStr === undefined) {
        // Handle undefined colors (like zebra) with a default
        inkColor = [160, 160, 160];
      }
      
      ink(inkColor).write(token, {
        x: currentX,
        y,
        size: scale
      });
      
      currentX += token.length * charWidth;
      
      // Add space after token, with special handling for commas
      if (i < tokens.length - 1) {
        const nextToken = tokens[i + 1];
        
        // For comma syntax, add space after commas and most tokens
        if (token === ",") {
          currentX += charWidth; // Space after comma
        } else if (nextToken !== "," && !["(", ")"].includes(token) && !["(", ")"].includes(nextToken)) {
          currentX += charWidth; // Space between regular tokens
        }
      }
    }
  } catch (error) {
    console.warn("Syntax highlighting error:", error);
    // Fallback to normal rendering
    ink([160, 160, 160]).write(source, { x, y, size: scale });
  }
}

// Calculate responsive layout
function calculateLayout(screen) {
  const isMobile = screen.width < MOBILE_BREAKPOINT;
  const isTablet = screen.width < TABLET_BREAKPOINT;
  
  if (isMobile) {
    // On mobile, full width for list, overlay for preview
    return {
      listWidth: screen.width,
      previewWidth: screen.width * 0.9,
      previewX: screen.width * 0.05,
      previewY: 40,
      showPreviewOverlay: true
    };
  } else if (isTablet) {
    // On tablet, adjust ratios
    return {
      listWidth: Math.floor(screen.width * 0.45),
      previewWidth: Math.floor(screen.width * 0.55),
      previewX: Math.floor(screen.width * 0.45),
      previewY: 0,
      showPreviewOverlay: false
    };
  } else {
    // Desktop - fixed left panel, remaining for preview
    const fixedListWidth = Math.min(400, screen.width * 0.4);
    return {
      listWidth: fixedListWidth,
      previewWidth: screen.width - fixedListWidth,
      previewX: fixedListWidth,
      previewY: 0,
      showPreviewOverlay: false
    };
  }
}

// 🎨 Paint
function paint({ wipe, ink, text, screen, ui: { Button }, num, help: { choose }, lisp, api, kidlisp }) {
  // Reset zebra cache at the beginning of each frame for proper color cycling
  num.resetZebraCache();
  
  wipe(0);
  
  if (retrieving) {
    ink(choose(64, 127)).write("Retrieving codes...", { center: "xy" });
    return;
  }
  
  if (failed) {
    ink("red").write("Failed to load codes", { center: "xy" });
    return;
  }
  
  if (codeEntries.length === 0) {
    ink("gray").write("No recent codes found", { center: "xy" });
    return;
  }

  // Split screen layout: top half for preview, bottom half for list
  const splitY = Math.floor(screen.height / 2);
  
  // Draw separator line between top and bottom halves
  ink([64, 64, 64]).line(0, splitY, screen.width, splitY);

  // Render top half: Preview area
  if (selectedEntry) {
    renderPreviewArea(screen, ink, splitY, api, Button, kidlisp);
  } else {
    renderPreviewPlaceholder(screen, ink, splitY);
  }

  // Render bottom half: Scrollable code list
  renderCodeListArea(screen, ink, Button, splitY, api);
}

// Render the top half - preview area
function renderPreviewArea(screen, ink, splitY, api, Button, kidlisp) {
  const previewHeight = splitY;
  
  // Draw preview background
  ink([16, 16, 24]).box(0, 0, screen.width, previewHeight);
  
  // Use simplified KidLisp execution with $code shorthand
  if (selectedEntry && kidlisp) {
    try {
      // Use the new $code shorthand with noCache option for animations
      kidlisp(
        4,                    // x
        4,                    // y  
        screen.width - 8,     // width
        previewHeight - 8,    // height
        `$${selectedEntry.code}`, // Use dollar sign shortcut
        { noCache: true }     // Disable caching to allow animations
      );
    } catch (error) {
      console.warn(`❌ KidLisp execution error for $${selectedEntry.code}:`, error);
      // Fallback to showing error message
      ink("red").write("Execution Error", { 
        x: 4, 
        y: previewHeight / 2, 
        size: 1, 
        font: "MatrixChunky8" 
      });
    }
  }
}

// Render placeholder when no code is selected
function renderPreviewPlaceholder(screen, ink, splitY) {
  const previewHeight = splitY;
  
  // Draw preview background
  ink([16, 16, 24]).box(0, 0, screen.width, previewHeight);
}

// Render the bottom half - scrollable code list
function renderCodeListArea(screen, ink, Button, splitY, api) {
  const listY = splitY + 1;
  const listHeight = screen.height - listY;
  const itemHeight = 24; // Taller items for more metadata
  const margin = 4;
  
  // Draw list background
  ink([8, 8, 12]).box(0, listY, screen.width, listHeight);
  
  // Calculate visible range
  const visibleItems = Math.ceil(listHeight / itemHeight) + 1;
  const startIndex = Math.max(0, Math.floor(-scroll / itemHeight));
  const endIndex = Math.min(codeEntries.length, startIndex + visibleItems);
  
  // Draw list items
  for (let i = startIndex; i < endIndex; i++) {
    const entry = codeEntries[i];
    const y = listY + scroll + (i * itemHeight);
    
    // Skip if not visible
    if (y < listY - itemHeight || y > screen.height) continue;
    
    const isSelected = i === selectedIndex;
    
    // Draw selection highlight
    if (isSelected) {
      ink([32, 32, 48]).box(0, y, screen.width, itemHeight);
    }
    
    // Draw alternating row backgrounds
    if (!isSelected && i % 2 === 1) {
      ink([12, 12, 16]).box(0, y, screen.width, itemHeight);
    }
    
    // Create/update list button
    if (!entry.listButton) {
      entry.listButton = new Button(0, y, screen.width, itemHeight);
      entry.listButton.fn = () => {
        selectedEntry = entry;
        selectedIndex = i;
      };
    } else {
      entry.listButton.box.y = y;
    }
    
    // Line 1: Code name (bigger and prominent)
    const line1Y = y + 2;
    const codeColor = isSelected ? [255, 255, 255] : [0, 200, 255];
    ink(codeColor).write(`$${entry.code}`, { x: margin, y: line1Y }, undefined, undefined, false, "MatrixChunky8");
    
    // Line 2: Handle, hits, and preview snippet
    const line2Y = y + 14;
    const metaColor = isSelected ? [200, 200, 200] : [120, 120, 120];
    ink(metaColor).write(`by ${entry.handle} • ${entry.hits} hits`, { x: margin, y: line2Y }, undefined, undefined, false, "MatrixChunky8");
    
    // Show preview snippet on the right if available
    if (entry.preview && entry.preview.trim()) {
      const previewSnippet = entry.preview.trim().substring(0, 30) + (entry.preview.length > 30 ? "..." : "");
      const previewX = screen.width - 150; // Right side
      if (previewX > margin + 200) { // Only show if there's space
        ink([100, 100, 100]).write(previewSnippet, { x: previewX, y: line2Y }, undefined, undefined, false, "MatrixChunky8");
      }
    }
  }
  
  // Draw scroll indicator
  if (codeEntries.length * itemHeight > listHeight) {
    drawScrollIndicator(screen, ink, listY, listHeight, itemHeight);
  }
}

// Draw scroll indicator on the right side
function drawScrollIndicator(screen, ink, listY, listHeight, itemHeight) {
  const scrollBarX = screen.width - 4;
  const scrollBarWidth = 4;
  
  // Background track
  ink([32, 32, 32]).box(scrollBarX, listY, scrollBarWidth, listHeight);
  
  // Calculate thumb
  const contentHeight = codeEntries.length * itemHeight;
  const thumbHeight = Math.max(8, Math.floor((listHeight / contentHeight) * listHeight));
  const maxScroll = Math.max(0, contentHeight - listHeight);
  const scrollRatio = maxScroll > 0 ? Math.abs(scroll) / maxScroll : 0;
  const thumbY = listY + (listHeight - thumbHeight) * scrollRatio;
  
  // Scroll thumb
  ink("white").box(scrollBarX, Math.floor(thumbY), scrollBarWidth, thumbHeight);
}

// Render syntax highlighted preview text in the top area
function renderSyntaxHighlightedPreview(previewText, x, y, maxWidth, maxHeight, ink, api) {
  if (!previewText || previewText.length === 0) return;
  
  const lineHeight = 10; // Compact line height for MatrixChunky8
  let currentY = y;
  
  // Check if it's KidLisp source
  if (isKidlispSource(previewText)) {
    try {
      // Tokenize the code
      const tokens = tokenize(previewText);
      
      let currentX = x;
      
      tokens.forEach((token) => {
        // Check if we need to wrap to next line
        const tokenWidth = token.text.length * 4; // 4px per char for MatrixChunky8
        if (currentX + tokenWidth > x + maxWidth) {
          currentY += lineHeight;
          currentX = x;
          
          // Stop if we exceed max height
          if (currentY + lineHeight > y + maxHeight) return;
        }
        
        // Get color for this token
        const color = getTokenColor(token);
        
        // Render the token
        ink(color).write(token.text, { x: currentX, y: currentY }, undefined, undefined, false, "MatrixChunky8");
        
        // Add space after token
        currentX += tokenWidth + 4; // 4px space between tokens
      });
    } catch (error) {
      // Fallback to plain text if tokenization fails
      ink("white").write(previewText.substring(0, 100), { x, y }, undefined, undefined, false, "MatrixChunky8");
    }
  } else {
    // Non-KidLisp text - just show as plain text
    const lines = previewText.split('\n');
    lines.forEach((line, index) => {
      if (currentY + lineHeight > y + maxHeight) return;
      
      const trimmedLine = line.substring(0, Math.floor(maxWidth / 4)); // Approximate character limit
      ink("white").write(trimmedLine, { x, y: currentY }, undefined, undefined, false, "MatrixChunky8");
      currentY += lineHeight;
    });
  }
}

// Render action buttons in the preview area
function renderPreviewButtons(screen, ink, buttonY, margin, Button) {
  const buttonWidth = 60;
  const buttonHeight = 20;
  const buttonSpacing = 8;
  
  // Copy button
  if (!copyButton) {
    copyButton = new Button(margin, buttonY, buttonWidth, buttonHeight);
    copyButton.fn = () => {
      console.log("Copying code:", selectedEntry.code);
      // TODO: Implement clipboard copy
    };
  } else {
    copyButton.box.x = margin;
    copyButton.box.y = buttonY;
  }
  
  copyButton.paint((b) => {
    const bgColor = b.down ? [64, 64, 96] : (b.hovered ? [48, 48, 72] : [32, 32, 48]);
    ink(bgColor).box(b.box.x, b.box.y, b.box.w, b.box.h);
    ink(b.hovered ? "white" : [200, 200, 200]).write("Copy", {
      x: b.box.x + 6,
      y: b.box.y + 6,
    }, undefined, undefined, false, "MatrixChunky8");
  });
  
  // Run button
  const runX = margin + buttonWidth + buttonSpacing;
  if (!runButton) {
    runButton = new Button(runX, buttonY, buttonWidth, buttonHeight);
    runButton.fn = () => {
      console.log("Running code:", selectedEntry.code);
      // TODO: Implement code execution
    };
  } else {
    runButton.box.x = runX;
    runButton.box.y = buttonY;
  }
  
  runButton.paint((b) => {
    const bgColor = b.down ? [96, 64, 64] : (b.hovered ? [72, 48, 48] : [48, 32, 32]);
    ink(bgColor).box(b.box.x, b.box.y, b.box.w, b.box.h);
    ink(b.hovered ? "white" : [200, 200, 200]).write("Run", {
      x: b.box.x + 6,
      y: b.box.y + 6,
    }, undefined, undefined, false, "MatrixChunky8");
  });
}

// Helper function to get color for syntax tokens
function getTokenColor(token) {
  try {
    const kidlisp = new KidLisp();
    return kidlisp.getTokenColor(token);
  } catch (error) {
    // Fallback colors if KidLisp is not available
    switch (token.type) {
      case 'FUNCTION': return 'cyan';
      case 'NUMBER': return 'yellow';
      case 'STRING': return 'green';
      case 'COMMENT': return 'gray';
      default: return 'white';
    }
  }
}

// 🚀 Act (User interactions)
function act({ event, screen, jump, hud, sound, needsPaint }) {
  // Handle responsive reframe events
  if (event.is("reframed")) {
    console.log("🖼️ $ feed reframed:", { 
      width: screen.width, 
      height: screen.height 
    });
    needsPaint(); // Force repaint with new responsive layout
    return;
  }

  // Handle scrolling for bottom half
  if (event.is("draw")) {
    scroll += event.delta.y * 0.5; // Slower scrolling
    boundScroll(screen);
  }

  if (event.is("scroll")) {
    scroll -= event.y * 2; // Adjust scroll sensitivity
    boundScroll(screen);
  }

  // Handle button interactions
  if (codeEntries && codeEntries.length > 0) {
    // Handle list buttons
    codeEntries.forEach((entry, index) => {
      if (entry.listButton) {
        entry.listButton.act(event, () => {
          if (entry.listButton.fn) {
            entry.listButton.fn();
          }
          
          // Show feedback in HUD
          hud.label(`Previewing ${entry.codeText}`, "cyan");
          
          // Play interaction sound
          if (sound) {
            sound.synth({
              type: "triangle",
              tone: 800,
              attack: 0.005,
              decay: 0.08,
              sustain: 0.1,
              release: 0.15,
              duration: 0.12,
            });
          }
          needsPaint();
        });
      }
    });
  }

  // Handle preview panel buttons
  if (copyButton) {
    copyButton.act(event, () => {
      if (copyButton.fn) {
        copyButton.fn();
      }
      needsPaint();
    });
  }

  if (runButton) {
    runButton.act(event, () => {
      if (runButton.fn) {
        runButton.fn();
      }
      needsPaint();
    });
  }
}

// Bound scroll function for the bottom list area
function boundScroll(screen) {
  if (!codeEntries.length) return;
  
  const listHeight = Math.floor(screen.height / 2) - 1; // Bottom half minus separator
  const itemHeight = 16;
  const contentHeight = codeEntries.length * itemHeight;
  const maxNegativeScroll = -(Math.max(0, contentHeight - listHeight));
  
  if (scroll < maxNegativeScroll) scroll = maxNegativeScroll;
  if (scroll > 0) scroll = 0;
}

// 📰 Meta
function meta() {
  return {
    title: "KidLisp Feed",
    desc: "A live feed of recent KidLisp cached codes displayed with split-screen preview.",
  };
}

// 🖼️ Preview
function preview({ wipe, ink, slug }) {
  wipe("black").ink("cyan").write("$", { center: "xy", size: 4 });
}

export { boot, paint, act, meta, preview };
