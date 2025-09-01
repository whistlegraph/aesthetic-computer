// $, 2025.8.29.15.30
// A live feed of recent KidLisp cached codes.

/* #region 📚 README 
  Shows a scrolling feed of recently cached KidLisp codes with their handles,
  similar to the moods feed. Useful for discovering new code snippets and
  seeing what the community is creating.
#endregion */

/* #region 🏁 TODO 
  - [] Add click interaction to copy codes or navigate to them
  - [] Add relative timestamps ("2h ago", "1d ago")
  - [] Add filtering by popularity or user
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

// Preview system for embedded KidLisp
let selectedEntry = null;
let previewBuffer = null;
let previewSize = 120; // Size of the preview area
let previewX, previewY; // Position of preview area
let cachedSources = new Map(); // Store cached sources here
let loadingStates = new Map(); // Track loading states to prevent retries
let erroredCodes = new Set(); // Track codes that have errored to prevent repeated execution

// Layout constants (similar to colors.mjs)
const LEFT_MARGIN = 6;
const TOP_MARGIN = 20;
const ROW_HEIGHT = 36; // More space per row
const CODE_PREVIEW_HEIGHT = 12; // Height for preview text
const HIT_COUNT_HEIGHT = 10; // Height for hit count

const { floor, min, abs, ceil, sin } = Math;

// 🥾 Boot
function boot({ wipe, screen, colon, params, typeface, ui }) {
  scale = parseInt(colon[0]) || 1;
  wipe(0);
  
  // Calculate preview position (top-right corner with margin)
  previewX = screen.width - previewSize - 20;
  previewY = 20;
  
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
          if (ui) {
            // Calculate button dimensions
            const [gw, gh] = typeface.glyphs[0].resolution;
            const w = gw * codeText.length;
            const h = gh + 1;
            
            entry.button = new ui.Button(
              LEFT_MARGIN,
              scroll + TOP_MARGIN + ROW_HEIGHT * index,
              Math.max(screen.width - previewSize - 60, w), // Leave space for preview area
              ROW_HEIGHT - 2 // Leave small gap between rows
            );
          }
          
          return entry;
        });
        
        // Initialize button array (filter out entries without buttons)
        buttons = codeEntries.filter(entry => entry.button).map(entry => entry.button);
        
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

// 🎨 Paint
function paint({ wipe, ink, text, screen, ui, num, help: { choose }, lisp, api, kidlisp }) {
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
  
  // Draw alternating row backgrounds (like colors.mjs)
  codeEntries.forEach((entry, index) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
    
    // Only draw if visible on screen
    if (y > -ROW_HEIGHT && y < screen.height) {
      // Draw alternating row background
      const rowColor = index % 2 === 0 ? [8, 8, 12] : [12, 12, 16]; // Subtle alternating backgrounds
      ink(rowColor).box(0, y, screen.width - previewSize - 40, ROW_HEIGHT); // Leave space for preview
    }
  });
  
  // Draw code entries
  codeEntries.forEach((entry, index) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
    
    // Only draw if visible on screen
    if (y > -ROW_HEIGHT && y < screen.height) {
      // Update button position as we scroll and draw content
      if (entry.button) {
        entry.button.box.y = y;
        
        // Draw the interactive button area
        entry.button.paint((b) => {
          const textY = y + 4; // Small top margin
          let currentX = LEFT_MARGIN + 4; // Small left margin within button
          
          // Code name in cyan/blue (main identifier)
          const codeColor = b.down ? "yellow" : b.hovered ? "white" : [0, 200, 255];
          ink(codeColor).write(entry.codeText, {
            x: currentX,
            y: textY,
            size: scale,
          });
          
          // Preview text on second line (truncated if too long)
          const previewY = textY + 12;
          const maxPreviewWidth = screen.width - previewSize - 80; // Account for preview area
          const maxPreviewChars = Math.floor(maxPreviewWidth / 6); // Approximate character width
          let displayPreview = entry.preview;
          
          if (displayPreview.length > maxPreviewChars) {
            displayPreview = displayPreview.substring(0, maxPreviewChars - 3) + "...";
          }
          
          const previewColor = b.down ? [255, 255, 200] : b.hovered ? [220, 220, 220] : [160, 160, 160];
          ink(previewColor).write(displayPreview, {
            x: currentX,
            y: previewY,
            size: scale,
          });
          
          // Handle and hit count on third line
          const metaY = textY + 24;
          const handleColor = b.down ? [255, 200, 255] : b.hovered ? [200, 150, 200] : [140, 100, 140];
          ink(handleColor).write(entry.handle, {
            x: currentX,
            y: metaY,
            size: scale,
          });
          
          // Hit count aligned to the right (but before preview area)
          const hitColor = b.down ? [200, 255, 200] : b.hovered ? [150, 200, 150] : [100, 140, 100];
          const hitText = entry.hits;
          const hitWidth = hitText.length * 6 * scale; // Approximate text width
          ink(hitColor).write(hitText, {
            x: screen.width - previewSize - hitWidth - 60, // Before preview area
            y: metaY,
            size: scale,
          });
          
          // Hover effect background
          if (b.hovered && !b.down) {
            ink([32, 32, 48, 64]).box(b.box.x, b.box.y, b.box.w, b.box.h);
          }
          
          // Down effect
          if (b.down) {
            ink([64, 64, 96, 128]).box(b.box.x, b.box.y, b.box.w, b.box.h);
          }
        });
      } else {
        // Fallback rendering without button interaction
        const textY = y + 4;
        let currentX = LEFT_MARGIN + 4;
        
        // Code name in cyan/blue
        ink([0, 200, 255]).write(entry.codeText, {
          x: currentX,
          y: textY,
          size: scale,
        });
        
        // Preview text on second line
        const previewY = textY + 12;
        const maxPreviewWidth = screen.width - previewSize - 80;
        const maxPreviewChars = Math.floor(maxPreviewWidth / 6);
        let displayPreview = entry.preview;
        
        if (displayPreview.length > maxPreviewChars) {
          displayPreview = displayPreview.substring(0, maxPreviewChars - 3) + "...";
        }
        
        ink([160, 160, 160]).write(displayPreview, {
          x: currentX,
          y: previewY,
          size: scale,
        });
        
        // Handle and hit count on third line
        const metaY = textY + 24;
        ink([140, 100, 140]).write(entry.handle, {
          x: currentX,
          y: metaY,
          size: scale,
        });
        
        // Hit count aligned to the right
        const hitText = entry.hits;
        const hitWidth = hitText.length * 6 * scale;
        ink([100, 140, 100]).write(hitText, {
          x: screen.width - previewSize - hitWidth - 60,
          y: metaY,
          size: scale,
        });
      }
    }
  });
  
  // Draw scroll bar (like colors.mjs)
  if (codeEntries.length > 0) {
    const scrollBarX = screen.width - previewSize - 30; // Before preview area
    const scrollBarWidth = 4;
    const scrollBarHeight = screen.height;
    const scrollBarY = 0;
    
    // Background track
    ink([64, 64, 64]).box(scrollBarX, scrollBarY, scrollBarWidth, scrollBarHeight);
    
    // Calculate thumb position and size
    const contentHeight = codeEntries.length * ROW_HEIGHT;
    const visibleHeight = screen.height - TOP_MARGIN;
    const thumbHeight = Math.max(8, Math.floor((visibleHeight / contentHeight) * scrollBarHeight));
    
    // Calculate thumb position based on scroll ratio
    const maxScroll = Math.max(0, contentHeight - visibleHeight);
    const scrollRatio = maxScroll > 0 ? Math.abs(scroll) / maxScroll : 0;
    const thumbY = scrollBarY + (scrollBarHeight - thumbHeight) * scrollRatio;
    
    // Scroll thumb
    ink([128, 128, 160]).box(scrollBarX, Math.floor(thumbY), scrollBarWidth, thumbHeight);
  }
  
  // Draw preview area background
  ink([20, 20, 30]).box(previewX, previewY, previewSize, previewSize);
  ink([80, 80, 120]).box(previewX, previewY, previewSize, previewSize, "outline");
  
  // Draw preview content
  if (selectedEntry) {
    // Draw preview header
    ink([0, 200, 255]).write(selectedEntry.codeText, {
      x: previewX + 4,
      y: previewY - 12,
      size: 1,
    });
    
    // Debug: Show what we have for this entry
    console.log(`Preview for ${selectedEntry.code}:`, {
      hasCached: cachedSources.has(selectedEntry.code),
      cachedValue: cachedSources.get(selectedEntry.code),
      loadingState: loadingStates.get(selectedEntry.code)
    });
    
    // Check if we have cached source for this entry
    if (cachedSources.has(selectedEntry.code)) {
      const cachedSource = cachedSources.get(selectedEntry.code);
      
      // Skip codes that have already errored to prevent infinite error loops
      if (erroredCodes.has(selectedEntry.code)) {
        ink("red").write("Error", {
          x: previewX + 4,
          y: previewY + previewSize / 2,
          size: 1,
        });
        return;
      }
      
      // More strict validation - must be valid KidLisp source
      if (cachedSource && 
          typeof cachedSource === 'string' && 
          cachedSource.trim().length > 0 &&
          !cachedSource.includes('Loading') && // Don't run loading messages
          cachedSource.startsWith('(') &&      // KidLisp should start with (
          kidlisp) {
        
        console.log(`Running KidLisp for ${selectedEntry.code}:`, cachedSource.substring(0, 50) + '...');
        
        // Detect problematic patterns that cause continuous errors
        const hasProblematicFunctions = /\b(scroll frame)\b/.test(cachedSource); // Specific problematic function combos
        
        if (hasProblematicFunctions) {
          console.warn(`Skipping KidLisp code ${selectedEntry.code} due to problematic function combinations`);
          erroredCodes.add(selectedEntry.code);
          ink("orange").write("Complex", {
            x: previewX + 4,
            y: previewY + previewSize / 2,
            size: 1,
          });
          return;
        }
        
        try {
          // Run the original KidLisp code - let the kidlisp function handle internal errors
          kidlisp(
            previewX + 4,
            previewY + 4,
            previewSize - 8,
            previewSize - 8,
            cachedSource
          );
        } catch (error) {
          console.warn(`Preview render error for ${selectedEntry.code}:`, error);
          // Mark this code as errored to avoid repeated attempts
          erroredCodes.add(selectedEntry.code);
          
          // Show error message instead of crashing
          ink("red").write("Error", {
            x: previewX + 4,
            y: previewY + previewSize / 2,
            size: 1,
          });
        }
      } else {
        // Show what we got instead of running it
        const debugText = cachedSource ? 
          (typeof cachedSource === 'string' ? 
            `"${cachedSource.substring(0, 20)}..."` : 
            typeof cachedSource) :
          'null';
        
        console.log(`Not running KidLisp for ${selectedEntry.code}, got:`, debugText);
        
        // Check why we're not running it  
        let message = "No source";
        if (cachedSource && typeof cachedSource === 'string') {
          if (!cachedSource.startsWith('(')) {
            message = "Invalid format";
          } else if (cachedSource.includes('Loading')) {
            message = "Loading text";
          }
        }
        
        ink("gray").write(message, {
          x: previewX + 4,
          y: previewY + previewSize / 2,
          size: 1,
        });
      }
    } else if (loadingStates.get(selectedEntry.code) === 'loading') {
      // Currently loading - just show text, never run kidlisp
      ink("orange").write("Loading...", {
        x: previewX + 4,
        y: previewY + previewSize / 2,
        size: 1,
      });
    } else if (loadingStates.get(selectedEntry.code) === 'failed') {
      // Previously failed to load
      ink("red").write("Load failed", {
        x: previewX + 4,
        y: previewY + previewSize / 2,
        size: 1,
      });
    } else if (loadingStates.get(selectedEntry.code) === 'error') {
      // Previously had a runtime error
      ink("red").write("Code error", {
        x: previewX + 4,
        y: previewY + previewSize / 2,
        size: 1,
      });
    } else if (lisp) {
      // Haven't tried loading yet, start loading
      loadingStates.set(selectedEntry.code, 'loading');
      
      console.log(`Starting load for ${selectedEntry.code}`);
      
      // Show loading message
      ink("orange").write("Loading...", {
        x: previewX + 4,
        y: previewY + previewSize / 2,
        size: 1,
      });
      
      // Load the cached source (this will be available on next frame)
      lisp.getCachedCodeMultiLevel(selectedEntry.code)
        .then((cachedSource) => {
          console.log(`Loaded source for ${selectedEntry.code}:`, typeof cachedSource, cachedSource?.substring(0, 50));
          // Store the result (even if null)
          cachedSources.set(selectedEntry.code, cachedSource || null);
          loadingStates.set(selectedEntry.code, 'loaded');
        })
        .catch((error) => {
          console.warn(`Failed to load cached source for ${selectedEntry.code}:`, error);
          cachedSources.set(selectedEntry.code, null);
          loadingStates.set(selectedEntry.code, 'failed');
        });
    } else {
      // No lisp system available
      ink("gray").write("Preview unavailable", {
        x: previewX + 4,
        y: previewY + previewSize / 2,
        size: 1,
      });
    }
  } else {
    // No selection - show instructions
    ink([120, 120, 120]).write("Tap a code", {
      x: previewX + 4,
      y: previewY + previewSize / 2 - 6,
      size: 1,
    });
    ink([120, 120, 120]).write("to preview", {
      x: previewX + 4,
      y: previewY + previewSize / 2 + 6,
      size: 1,
    });
  }
}

// 🎪 Act
function act({ event, screen, jump, hud, sound }) {
  // Handle scrolling (like colors.mjs)
  if (event.is("draw")) {
    scroll += event.delta.y * 0.5; // Slower scrolling
    boundScroll(screen);
  }

  if (event.is("scroll")) {
    scroll -= event.y * 2; // Adjust scroll sensitivity
    boundScroll(screen);
  }

  // Handle button interactions
  buttons.forEach((button, index) => {
    button.act(event, () => {
      const entry = codeEntries[index];
      
      // Set this entry as selected for preview
      selectedEntry = entry;
      
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
    });
  });
}

// Bound scroll function (like colors.mjs)
function boundScroll(screen) {
  if (!codeEntries.length) return;
  
  const contentHeight = codeEntries.length * ROW_HEIGHT;
  const visibleHeight = screen.height - TOP_MARGIN;
  const maxNegativeScroll = -(Math.max(0, contentHeight - visibleHeight));
  
  if (scroll < maxNegativeScroll) scroll = maxNegativeScroll;
  if (scroll > 0) scroll = 0;
}

// 📰 Meta
function meta() {
  return {
    title: "KidLisp Feed",
    desc: "A live feed of recent KidLisp cached codes.",
  };
}

// 🖼️ Preview
function preview({ wipe, ink, slug }) {
  wipe("black").ink("cyan").write("$", { center: "xy", size: 4 });
}

export { boot, paint, act, meta, preview };

// 📚 Library
//   (Useful functions used throughout the piece)
