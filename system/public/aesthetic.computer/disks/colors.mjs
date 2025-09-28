// Colors, 2025.8.11
// A simple scrollable list of CSS colors and fade examples with preview squares.

import { cssColors, completeColorIndex, staticColorMap, zebra } from "../lib/num.mjs";

// Color syntax highlighting logic extracted from kidlisp.mjs
function getColorForColorName(colorName) {
  // Special case for "rainbow" - return ROYGBIV array
  if (colorName === "rainbow") {
    return "RAINBOW"; // Special marker for rainbow rendering
  }
  
  // Special case for "zebra" - return special marker
  if (colorName === "zebra") {
    return "ZEBRA"; // Special marker for zebra rendering
  }
  
  // Check if it's a CSS color name and return the actual color
  if (cssColors[colorName]) {
    return cssColors[colorName];
  }
  
  // Default to white if not a recognized color
  return "white";
}

// Calculate the brightness of an RGB color (0-255 scale)
function getColorBrightness(rgb) {
  if (!Array.isArray(rgb) || rgb.length < 3) return 128; // Default to medium brightness
  const [r, g, b] = rgb;
  // Use standard luminance formula
  return (r * 0.299 + g * 0.587 + b * 0.114);
}

// Get appropriate shadow color based on text color brightness
function getShadowColor(textColorRgb) {
  const brightness = getColorBrightness(textColorRgb);
  // If text is dark (brightness < 128), use light shadow
  // If text is light (brightness >= 128), use dark shadow
  return brightness < 128 ? [192, 192, 192] : [32, 32, 32]; // Light gray or dark gray
}

// Render text with color syntax highlighting and dynamic shadow
function writeColoredText(ink, text, x, y) {
  if (text === "rainbow") {
    // ROYGBIV rainbow colors for each character
    const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
    let currentX = x;
    for (let charIndex = 0; charIndex < text.length; charIndex++) {
      const charColor = rainbowColors[charIndex % rainbowColors.length];
      const charColorRgb = getColorForColorName(charColor);
      const shadowColor = getShadowColor(charColorRgb);
      
      // Draw shadow first
      ink(shadowColor).write(text[charIndex], {
        x: currentX + 1,
        y: y + 1,
      });
      
      // Draw main character
      ink(charColor).write(text[charIndex], {
        x: currentX,
        y: y,
      });
      currentX += 6; // Advance for each character in default font
    }
  } else if (text === "zebra") {
    // Zebra colors for each character (alternating black/white)
    const zebraColors = ["black", "white"];
    let currentX = x;
    for (let charIndex = 0; charIndex < text.length; charIndex++) {
      const charColor = zebraColors[charIndex % zebraColors.length];
      const charColorRgb = getColorForColorName(charColor);
      const shadowColor = getShadowColor(charColorRgb);
      
      // Draw shadow first
      ink(shadowColor).write(text[charIndex], {
        x: currentX + 1,
        y: y + 1,
      });
      
      // Draw main character
      ink(charColor).write(text[charIndex], {
        x: currentX,
        y: y,
      });
      currentX += 6; // Advance for each character in default font
    }
  } else if (text.includes(' / ')) {
    // Handle alias format like "gray / grey"
    const parts = text.split(' / ');
    const primaryColor = parts[0]; // Use first color for the color
    const color = getColorForColorName(primaryColor);
    const colorRgb = Array.isArray(color) ? color : cssColors[primaryColor] || [255, 255, 255];
    const shadowColor = getShadowColor(colorRgb);
    
    // Draw shadow first
    ink(shadowColor).write(text, {
      x: x + 1,
      y: y + 1,
    });
    
    // Draw main text in the primary color
    ink(color).write(text, {
      x: x,
      y: y,
    });
  } else {
    // Use the color name's actual color
    const color = getColorForColorName(text);
    const colorRgb = Array.isArray(color) ? color : cssColors[text] || [255, 255, 255];
    const shadowColor = getShadowColor(colorRgb);
    
    // Draw shadow first
    ink(shadowColor).write(text, {
      x: x + 1,
      y: y + 1,
    });
    
    // Draw main text
    ink(color).write(text, {
      x: x,
      y: y,
    });
  }
}

let scroll = 0;
let colorEntries = [];
let buttons = [];
let swatchButtons = [];
let swatchToEntryMap = []; // Map swatch button index to entry index - moved to module scope
let chosenColor = null; // Track the chosen color

const { keys } = Object;

// Layout constants
const LEFT_MARGIN = 52;
const TOP_MARGIN = 30; // Extra space to avoid HUD overlap
const ROW_HEIGHT = 28;
const SQUARE_SIZE = 20;
const COLOR_SQUARE_MARGIN = 8;

// Special color words and fade examples
const specialColors = [
  {
    name: "fade:red-blue", 
    description: "Linear fade from red to blue",
    type: "fade",
    colors: ["red", "blue"],
    example: [128, 0, 128], // Show purple as example
  },
  {
    name: "fade:red-blue:vertical",
    description: "Vertical red to blue fade",
    type: "fade",
    colors: ["red", "blue"],
    example: [128, 0, 128], // Show purple as example
  },
  {
    name: "fade:zebra-rainbow",
    description: "Fade from zebra to rainbow colors",
    type: "fade",
    colors: ["zebra", "rainbow"],
    example: [127, 127, 127], // Show gray as example
  },
  {
    name: "fade:zebra-zebra-zebra",
    description: "Zebra stripes - alternating black/white",
    type: "fade",
    colors: ["zebra", "zebra", "zebra"],
    example: [127, 127, 127], // Show gray as example
  },
];

async function boot({ ui, typeface, store }) {
  // Retrieve scroll if it exists
  scroll = (await store.retrieve("colors:scroll")) || 0;
  
  // Build entries using standardized static color map with aliases
  const rgbToStaticIndex = new Map(); // Map RGB values to static indices
  const processedIndices = new Set(); // Track which indices we've already processed
  
  // First pass: create reverse lookup from RGB to static index
  Object.entries(staticColorMap).forEach(([index, rgb]) => {
    const rgbKey = rgb.join(',');
    if (!rgbToStaticIndex.has(rgbKey)) {
      rgbToStaticIndex.set(rgbKey, parseInt(index));
    }
  });
  
  // Second pass: group CSS colors by RGB values that match static colors
  const rgbToNames = new Map(); // Map RGB values to arrays of color names
  
  Object.entries(cssColors).forEach(([colorName, rgb]) => {
    const rgbKey = rgb.join(',');
    // Only include CSS colors that have a corresponding static index
    if (rgbToStaticIndex.has(rgbKey)) {
      if (!rgbToNames.has(rgbKey)) {
        rgbToNames.set(rgbKey, []);
      }
      rgbToNames.get(rgbKey).push(colorName);
    }
  });
  
  // Third pass: create entries with static indices and aliases
  const cssEntries = [];
  
  // Process in static index order to maintain consistent indexing
  Object.entries(staticColorMap).forEach(([staticIndex, rgb]) => {
    const rgbKey = rgb.join(',');
    const index = parseInt(staticIndex);
    
    if (!processedIndices.has(index)) {
      processedIndices.add(index);
      
      const aliases = rgbToNames.get(rgbKey) || [`color${index}`]; // Fallback name if no CSS match
      const displayName = aliases.length > 1 ? aliases.join(' / ') : aliases[0];
      
      const [gw, gh] = typeface.glyphs[0].resolution;
      const w = gw * displayName.length;
      const h = gh + 1;
      
      cssEntries.push({
        name: displayName,
        aliases: aliases, // Store all aliases for interaction handling
        rgb: rgb,
        type: "css",
        staticIndex: `c${index}`, // Use the standardized static index
        button: new ui.Button(
          LEFT_MARGIN + SQUARE_SIZE + COLOR_SQUARE_MARGIN,
          scroll + TOP_MARGIN + ROW_HEIGHT * cssEntries.length,
          w,
          h
        ),
      });
    }
  });

  // Add palette colors (p0, p1, etc.)
  const paletteEntries = [];
  
  // Add a divider before palette colors
  paletteEntries.push({
    name: "SEQUENCES",
    type: "divider",
  });
  
  // Add p0 (rainbow)
  const [gw0, gh0] = typeface.glyphs[0].resolution;
  const w0 = gw0 * "rainbow".length;
  const h0 = gh0 + 1;
  
  paletteEntries.push({
    name: "rainbow",
    description: "Animated rainbow effect - cycles through spectrum (p0)",
    type: "special",
    staticIndex: "p0",
    example: [255, 0, 255], // Show magenta as example
    button: new ui.Button(
      LEFT_MARGIN + SQUARE_SIZE + COLOR_SQUARE_MARGIN,
      scroll + TOP_MARGIN + ROW_HEIGHT * (cssEntries.length + paletteEntries.length),
      w0,
      h0
    ),
  });
  
  // Add p1 (zebra)
  const [gw1, gh1] = typeface.glyphs[0].resolution;
  const w1 = gw1 * "zebra".length;
  const h1 = gh1 + 1;
  
  paletteEntries.push({
    name: "zebra",
    description: "Alternating black and white stripes (p1)",
    type: "special",
    staticIndex: "p1",
    example: [128, 128, 128], // Show gray as example
    button: new ui.Button(
      LEFT_MARGIN + SQUARE_SIZE + COLOR_SQUARE_MARGIN,
      scroll + TOP_MARGIN + ROW_HEIGHT * (cssEntries.length + paletteEntries.length),
      w1,
      h1
    ),
  });

  // Build entries for special colors (rainbow, fades) 
  const specialEntries = [];
  let specialIndex = 0;
  
  // Add a divider before special colors
  specialEntries.push({
    name: "HOW TO FADE",
    type: "divider",
  });
  
  // Process special colors 
  specialColors.forEach((special) => {
    const [gw, gh] = typeface.glyphs[0].resolution;
    const w = gw * special.name.length;
    const h = gh + 1;
    
    specialEntries.push({
      name: special.name,
      description: special.description,
      type: special.type,
      colors: special.colors,
      rgb: special.example,
      button: new ui.Button(
        LEFT_MARGIN + SQUARE_SIZE + COLOR_SQUARE_MARGIN,
        scroll + TOP_MARGIN + ROW_HEIGHT * (cssEntries.length + paletteEntries.length + specialEntries.length),
        w,
        h
      ),
    });
  });

  // Combine all entries
  colorEntries = [...cssEntries, ...paletteEntries, ...specialEntries];
  
  // Add footer space
  colorEntries.push({
    name: "",
    type: "footer",
  });
  
  // Initialize button arrays
  buttons = colorEntries.filter(entry => entry.button).map(entry => entry.button);
  
  // Create swatch buttons for color selection (only for non-divider entries)
  swatchButtons = [];
  swatchToEntryMap = [];
  let swatchIndex = 0;
  
  colorEntries.forEach((entry, entryIndex) => {
    if (entry.type !== "divider" && entry.type !== "footer") {
      swatchButtons.push(new ui.Button(35, 0, SQUARE_SIZE, SQUARE_SIZE));
      swatchToEntryMap[swatchIndex] = entryIndex;
      swatchIndex++;
    }
  });
}

function paint({ wipe, ink, ui, hud, screen, $api }) {
  wipe("black");
  
  // Draw alternating row backgrounds first (behind everything)
  colorEntries.forEach((entry, index) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
    
    // Only draw if visible on screen
    if (y > -ROW_HEIGHT && y < screen.height) {
      // Draw alternating row background (spreadsheet-like)
      const rowColor = index % 2 === 0 ? [16, 16, 16] : [24, 24, 24]; // Subtle alternating grays
      ink(rowColor).box(0, y, screen.width - 4, ROW_HEIGHT); // Leave space for flush right scroll bar
    }
  });
  
  // Draw color entries  
  colorEntries.forEach((entry, index) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
    
    // Only draw if visible on screen
    if (y > -ROW_HEIGHT && y < screen.height) {
      // Draw static index or handle dividers/footers
      if (entry.type === "divider") {
        // Draw divider line above the text
        ink("gray").box(4, y + 4, screen.width - 20, 1);
        // Draw divider text in gray at 2x size, left-aligned
        ink("gray").write(entry.name, {
          left: 6, // Explicitly position 6px from left edge
          y: y + 2 + (SQUARE_SIZE / 2) - 4,
          size: 2, // 2x size for section titles
        });
        return; // Skip the rest for dividers
      } else if (entry.type === "footer") {
        // Footer - just empty space, skip all rendering
        return;
      } else if (entry.staticIndex) {
        // Draw static index (c0, c1, p0, etc.) - right-aligned for consistency
        const indexText = entry.staticIndex;
        ink("white").write(indexText, {
          x: 30 - (indexText.length * 6), // Right-align to x=30, assuming 6px per character
          y: y + 2 + (SQUARE_SIZE / 2) - 4,
        });
      }
      // If no staticIndex (fades), draw nothing in the left column
      
      // Draw color preview square - more space from index numbers
      const squareX = 35; // More space from the index numbers
      if (entry.type === "fade") {
        drawFadeSquare(ink, entry, squareX, y + 2, SQUARE_SIZE, $api);
      } else if (entry.type === "special" && entry.name === "rainbow") {
        drawRainbowSquare(ink, squareX, y + 2, SQUARE_SIZE, $api);
      } else if (entry.type === "special" && entry.name === "zebra") {
        drawZebraSquare(ink, squareX, y + 2, SQUARE_SIZE, $api);
      } else {
        ink(entry.rgb).box(squareX, y + 2, SQUARE_SIZE, SQUARE_SIZE);
      }
      
      // Draw border around square (skip for dividers)
      if (entry.type !== "divider") {
        ink("white").box(squareX - 1, y + 1, SQUARE_SIZE + 2, SQUARE_SIZE + 2, "outline");
      }
      
      // Draw color name with button interaction - positioned higher (skip for dividers)
      if (entry.button) {
        entry.button.paint((b) => {
          const textColor = b.down ? "yellow" : (entry.type === "fade" ? "cyan" : entry.type === "special" ? "lime" : "white");
          const textY = y + 2; // Moved up a bit more
          const textX = squareX + SQUARE_SIZE + 8; // Left aligned with space from square
          
          // Draw color name with syntax highlighting (unless button is interacted with)
          if (b.down) {
            ink("yellow").write(entry.name, {
              x: textX,
              y: textY,
            });
          } else if (entry.type === "fade") {
            ink("cyan").write(entry.name, {
              x: textX,
              y: textY,
            });
          } else if (entry.type === "special" && entry.name === "rainbow") {
            // Use rainbow coloring for rainbow text
            writeColoredText(ink, entry.name, textX, textY);
          } else if (entry.type === "special") {
            ink("lime").write(entry.name, {
              x: textX,
              y: textY,
            });
          } else {
            // Use color syntax highlighting for CSS color names
            writeColoredText(ink, entry.name, textX, textY);
          }
          
          // Show RGB values or description with dynamic colors using ink commands
          if (entry.description) {
          ink("gray").write(entry.description, {
            x: textX,
            y: textY + 12, // Moved back down
          });
        } else {
          // Create dynamic RGB color display
          const [r, g, b] = entry.rgb;
          const rgbText = `[${r}, ${g}, ${b}]`;
          
          // Position to align with the swatch - moved back down
          const rgbY = textY + 12;
          let currentX = textX;
          
          // Draw opening bracket
          ink("gray").write("[", {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
          currentX += 4; // Advance for bracket (MatrixChunky8)
          
          // Draw red value in red
          ink([Math.max(128, r), 0, 0]).write(r.toString(), {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
          currentX += r.toString().length * 4; // Advance for red value (MatrixChunky8)
          
          // Draw comma (no space after)
          ink("gray").write(",", {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
          currentX += 4; // Advance for comma only
          
          // Draw green value in green
          ink([0, Math.max(128, g), 0]).write(g.toString(), {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
          currentX += g.toString().length * 4; // Advance for green value (MatrixChunky8)
          
          // Draw comma (no space after)
          ink("gray").write(",", {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
          currentX += 4; // Advance for comma only
          
          // Draw blue value in blue
          ink([0, 0, Math.max(128, b)]).write(b.toString(), {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
          currentX += b.toString().length * 4; // Advance for blue value (MatrixChunky8)
          
          // Draw closing bracket
          ink("gray").write("]", {
            x: currentX,
            y: rgbY,
          }, undefined, undefined, false, "MatrixChunky8");
        }
        });
      }
    }
  });

  // Draw scroll bar on the right side - flush right
  if (colorEntries.length > 0) {
    const scrollBarX = screen.width - 4; // Flush right
    const scrollBarWidth = 4;
    const scrollBarHeight = screen.height; // Full screen height
    const scrollBarY = 0; // Start at top
    
    // Background track
    ink("gray").box(scrollBarX, scrollBarY, scrollBarWidth, scrollBarHeight);
    
    // Calculate thumb position and size
    const contentHeight = colorEntries.length * ROW_HEIGHT;
    const visibleHeight = screen.height - TOP_MARGIN;
    const thumbHeight = Math.max(8, Math.floor((visibleHeight / contentHeight) * scrollBarHeight));
    
    // Calculate thumb position based on scroll ratio
    const maxScroll = Math.max(0, contentHeight - visibleHeight);
    const scrollRatio = maxScroll > 0 ? Math.abs(scroll) / maxScroll : 0;
    const thumbY = scrollBarY + (scrollBarHeight - thumbHeight) * scrollRatio;
    
    // Scroll thumb
    ink("white").box(scrollBarX, Math.floor(thumbY), scrollBarWidth, thumbHeight);
  }

  // Update button positions as user scrolls
  let swatchButtonIndex = 0;
  colorEntries.forEach((entry, index) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
    
    // Update text button positions
    if (entry.button) {
      entry.button.box.y = y;
    }
    
    // Update swatch button positions (only for non-dividers)
    if (entry.type !== "divider") {
      swatchButtons[swatchButtonIndex].box.x = 35; // Updated to match
      swatchButtons[swatchButtonIndex].box.y = y + 2;
      swatchButtonIndex++;
    }
  });
  
  // Paint swatch buttons for color selection
  swatchButtons.forEach((swatchButton, swatchIndex) => {
    const entryIndex = swatchToEntryMap[swatchIndex];
    const entry = colorEntries[entryIndex];
    swatchButton.paint((b) => {
      if (b.down) {
        // Visual feedback when pressing swatch - more prominent
        ink("yellow").box(b.box.x - 2, b.box.y - 2, b.box.w + 4, b.box.h + 4, "outline");
        ink("white").box(b.box.x - 1, b.box.y - 1, b.box.w + 2, b.box.h + 2, "outline");
      } else if (b.hovered) {
        // Hover state - subtle highlight
        ink([128, 128, 128]).box(b.box.x - 1, b.box.y - 1, b.box.w + 2, b.box.h + 2, "outline");
      }
    });
  });  // Paint name buttons with hover states
  buttons.forEach((button, index) => {
    const entry = colorEntries[index];
    button.paint((b) => {
      if (b.hovered && !b.down) {
        // Hover state for name buttons
        const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
        if (y > -ROW_HEIGHT && y < screen.height) {
          ink([32, 32, 32]).box(b.box.x - 2, b.box.y - 2, b.box.w + 4, b.box.h + 4);
        }
      }
    });
  });

  // Draw chosen color in top right corner if one is selected (drawn last to appear on top)
  if (chosenColor) {
    const chosenSize = 40;
    const chosenX = screen.width - chosenSize - 20; // More margin
    const chosenY = 12; // Moved back up a bit
    
    // Draw chosen color square
    if (chosenColor.type === "fade") {
      drawFadeSquare(ink, chosenColor, chosenX, chosenY, chosenSize, $api);
    } else if (chosenColor.type === "special" && chosenColor.name === "rainbow") {
      drawRainbowSquare(ink, chosenX, chosenY, chosenSize, $api);
    } else if (chosenColor.type === "special" && chosenColor.name === "zebra") {
      drawZebraSquare(ink, chosenX, chosenY, chosenSize, $api);
    } else {
      // Special handling for black color to make it visible
      if (chosenColor.name === "black") {
        // Draw a dark gray background first
        ink([32, 32, 32]).box(chosenX, chosenY, chosenSize, chosenSize);
        // Then draw the black color slightly inset
        ink(chosenColor.rgb).box(chosenX + 2, chosenY + 2, chosenSize - 4, chosenSize - 4);
      } else {
        ink(chosenColor.rgb).box(chosenX, chosenY, chosenSize, chosenSize);
      }
    }
    
    // Border around chosen color
    ink("white").box(chosenX - 1, chosenY - 1, chosenSize + 2, chosenSize + 2, "outline");
    
    // Pokedex-style info display to the left and close to top
    const indexText = chosenColor.staticIndex || ""; // Use static index if available
    const indexWidth = indexText.length * 6; // Default font for larger number
    
    // Calculate text widths for right alignment to swatch
    let actualNameWidth;
    if (chosenColor.name === "rainbow") {
      // Rainbow uses individual character positioning, so calculate actual width
      actualNameWidth = chosenColor.name.length * 6;
    } else {
      // Regular text width
      actualNameWidth = chosenColor.name.length * 6;
    }
    
    // RGB text width calculation
    let rgbWidth = 0;
    if (chosenColor.rgb) {
      const [r, g, b] = chosenColor.rgb;
      const rgbText = `[${r},${g},${b}]`;
      rgbWidth = rgbText.length * 4; // MatrixChunky8 character width
    }
    
    // Find the maximum width to align everything to
    const maxWidth = Math.max(actualNameWidth, indexWidth, rgbWidth);
    
    // Center the info vertically within the swatch (40px tall)
    const swatchCenter = chosenY + (chosenSize / 2); // Center Y of the swatch
    const totalInfoHeight = 26; // Total height of all info (name + spacing + rgb + spacing + index)
    const infoY = swatchCenter - (totalInfoHeight / 2) - 4; // Start Y to center the block, moved up 4px
    
    const rgbX = chosenX - rgbWidth - 10; // Right-aligned
    const indexX = chosenX - indexWidth - 10; // Right-aligned, larger font
    
    // Right-align the color name properly
    const nameX = chosenX - actualNameWidth - 10; // Right-aligned based on actual width
    
    // Color name with syntax highlighting (right-aligned and vertically centered)
    writeColoredText(ink, chosenColor.name, nameX, infoY);
    
    // RGB values in small font (more spacing)
    if (chosenColor.rgb) {
      const [r, g, b] = chosenColor.rgb;
      let currentX = rgbX;
      const rgbY = infoY + 14; // More spacing from name
      
      // Compact RGB display
      ink("gray").write("[", { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
      currentX += 4;
      
      ink([Math.max(128, r), 0, 0]).write(r.toString(), { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
      currentX += r.toString().length * 4;
      
      ink("gray").write(",", { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
      currentX += 4;
      
      ink([0, Math.max(128, g), 0]).write(g.toString(), { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
      currentX += g.toString().length * 4;
      
      ink("gray").write(",", { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
      currentX += 4;
      
      ink([0, 0, Math.max(128, b)]).write(b.toString(), { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
      currentX += b.toString().length * 4;
      
      ink("gray").write("]", { x: currentX, y: rgbY }, undefined, undefined, false, "MatrixChunky8");
    }
    
    // Index number (larger, under RGB, more spacing) - only if static index exists
    if (indexText) {
      ink("white").write(indexText, {
        x: indexX,
        y: infoY + 26, // More spacing from RGB
      });
    }
  }
}

function act({ event, store, jump, hud, screen, sound }) {
  // Handle scrolling (like list)
  if (event.is("draw")) {
    scroll += event.delta.y;
    boundScroll(screen);
    store["colors:scroll"] = scroll;
  }

  if (event.is("scroll")) {
    scroll -= event.y;
    boundScroll(screen);
    store["colors:scroll"] = scroll;
  }

  // Handle button interactions
  let anyDown = false;
  buttons.forEach((button, index) => {
    button.act(event, () => {
      // Copy color name to clipboard and show HUD feedback
      const entry = colorEntries[index];
      // For CSS colors with aliases, use the first (primary) alias for copying
      const primaryName = entry.aliases ? entry.aliases[0] : entry.name;
      hud.label(primaryName, entry.type === "fade" ? "cyan" : entry.type === "special" ? "lime" : "white");
      
      // Play copy sound - cuter lower pitch
      if (sound) {
        sound.synth({
          type: "triangle",
          tone: 900,
          attack: 0.005,
          decay: 0.06,
          sustain: 0.08,
          release: 0.12,
          duration: 0.1,
        });
      }
      
      // Show usage example in HUD
      setTimeout(() => {
        if (entry.type === "fade") {
          hud.label(`ink("${primaryName}").line(x1, y1, x2, y2)`, "gray");
        } else if (entry.type === "special") {
          hud.label(`ink("${primaryName}").box(x, y, w, h)`, "gray");
        } else {
          hud.label(`ink("${primaryName}") or ink(${entry.rgb[0]}, ${entry.rgb[1]}, ${entry.rgb[2]})`, "gray");
        }
      }, 1500);
    });
    
    if (button.down) anyDown = true;
  });

  // Handle swatch button interactions for color selection
  swatchButtons.forEach((swatchButton, swatchIndex) => {
    swatchButton.act(event, () => {
      // Select this color using the mapping
      const entryIndex = swatchToEntryMap[swatchIndex];
      chosenColor = colorEntries[entryIndex];
      
      // Play selection sound - cuter higher pitch
      if (sound) {
        sound.synth({
          type: "triangle",
          tone: 1200,
          attack: 0.005,
          decay: 0.08,
          sustain: 0.1,
          release: 0.15,
          duration: 0.12,
        });
      }
    });
  });

  // Change cursor when hovering over buttons
  if (anyDown) {
    // The system will automatically show a pointer cursor
  }
  
  // Check if any dynamic colors are currently visible and need animation
  let hasDynamicColors = false;
  colorEntries.forEach((entry, index) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * index;
    // Only check if visible on screen
    if (y > -ROW_HEIGHT && y < screen.height) {
      if (entry.type === "special" && (entry.name === "rainbow" || entry.name === "zebra")) {
        hasDynamicColors = true;
      } else if (entry.type === "fade" && entry.colors && 
                (entry.colors.includes("rainbow") || entry.colors.includes("zebra"))) {
        hasDynamicColors = true;
      }
    }
  });
  
  // Also check if the chosen color is dynamic
  if (chosenColor && 
      ((chosenColor.type === "special" && (chosenColor.name === "rainbow" || chosenColor.name === "zebra")) ||
       (chosenColor.type === "fade" && chosenColor.colors && 
        (chosenColor.colors.includes("rainbow") || chosenColor.colors.includes("zebra"))))) {
    hasDynamicColors = true;
  }
  
  // Return true to keep animating if we have dynamic colors
  return hasDynamicColors;
}

// Bound scroll function with proper bottom limit
function boundScroll(screen) {
  const contentHeight = colorEntries.length * ROW_HEIGHT;
  const visibleHeight = screen.height - TOP_MARGIN;
  const maxNegativeScroll = -(Math.max(0, contentHeight - visibleHeight)); // No extra padding
  
  if (scroll < maxNegativeScroll) scroll = maxNegativeScroll;
  if (scroll > 0) scroll = 0;
}

function leave({ store }) {
  store["colors:scroll"] = scroll;
  store.persist("colors:scroll");
}

export { boot, paint, act, leave };

// Helper function to draw gradient squares for fade entries
function drawFadeSquare(ink, entry, x, y, size, $api) {
  // Parse direction from fade name (e.g., "fade:red-blue:vertical" or "fade:fire:45")
  let direction = "horizontal"; // default
  if (entry.name.includes(":")) {
    const parts = entry.name.split(":");
    if (parts.length >= 3) {
      direction = parts[2]; // vertical, diagonal, 45, etc.
    }
  }
  
  // Get the CSS colors for this fade - preserve dynamic color markers
  const colors = entry.colors?.map(colorName => {
    // Keep dynamic colors as markers for special handling
    if (colorName === "rainbow" || colorName === "zebra") {
      return colorName; // Keep as string marker
    }
    
    // Look up color in cssColors first
    if (cssColors[colorName]) {
      return cssColors[colorName];
    }
    // Fallback colors for common names
    const fallbacks = {
      red: [255, 0, 0],
      blue: [0, 0, 255],
      orange: [255, 165, 0],
      yellow: [255, 255, 0],
      pink: [255, 192, 203],
      cyan: [0, 255, 255],
      navy: [0, 0, 128]
    };
    return fallbacks[colorName] || [255, 255, 255];
  }) || [[255, 0, 0], [0, 0, 255]]; // Default red to blue
  
  if (colors.length === 1) {
    // Single color
    ink(colors[0]).box(x, y, size, size);
    return;
  }
  
  // Determine gradient direction
  const isVertical = direction === "vertical" || direction === "vertical-reverse";
  const isDiagonal = direction === "diagonal" || direction === "diagonal-reverse";
  const isAngle = !isNaN(parseFloat(direction)); // numeric angle like "45"
  const isReverse = direction.includes("reverse");
  
  if (isDiagonal) {
    // Diagonal gradient
    for (let px = 0; px < size; px++) {
      for (let py = 0; py < size; py++) {
        let t = (px + py) / (2 * (size - 1)); // diagonal progress
        if (isReverse) t = 1 - t;
        t = Math.max(0, Math.min(1, t));
        
        const color = interpolateColors(colors, t, $api, entry);
        ink(color).box(x + px, y + py, 1, 1);
      }
    }
  } else if (isAngle) {
    // Angled gradient (approximate with diagonal for simplicity)
    const angle = parseFloat(direction);
    const radians = (angle * Math.PI) / 180;
    
    for (let px = 0; px < size; px++) {
      for (let py = 0; py < size; py++) {
        // Project point onto angle direction
        const centerX = size / 2;
        const centerY = size / 2;
        const relX = px - centerX;
        const relY = py - centerY;
        
        // Project onto the angle vector
        const projection = relX * Math.cos(radians) + relY * Math.sin(radians);
        let t = (projection + size / 2) / size;
        t = Math.max(0, Math.min(1, t));
        
        const color = interpolateColors(colors, t, $api, entry);
        ink(color).box(x + px, y + py, 1, 1);
      }
    }
  } else if (isVertical) {
    // Vertical gradient
    for (let py = 0; py < size; py++) {
      let t = py / (size - 1);
      if (isReverse) t = 1 - t;
      
      const color = interpolateColors(colors, t, $api, entry);
      ink(color).box(x, y + py, size, 1);
    }
  } else {
    // Horizontal gradient (default)
    for (let px = 0; px < size; px++) {
      let t = px / (size - 1);
      if (isReverse) t = 1 - t;
      
      const color = interpolateColors(colors, t, $api, entry);
      ink(color).box(x + px, y, 1, size);
    }
  }
}

// Helper function to interpolate between multiple colors - supports dynamic colors
function interpolateColors(colors, t, $api, entry) {
  // Convert any dynamic color names to actual colors based on current state
  const resolvedColors = colors.map((color, index) => {
    if (Array.isArray(color)) {
      return color; // Already an RGB array
    }
    
    // Handle dynamic color string markers
    if (color === "rainbow") {
      // Get current rainbow color state - simulate what rainbow() would return
      const animFrame = $api?.paintCount ? Number($api.paintCount) : Date.now() / 100;
      const rainbowColors = [
        [255, 0, 0],     // Red
        [255, 165, 0],   // Orange  
        [255, 255, 0],   // Yellow
        [0, 255, 0],     // Green
        [0, 0, 255],     // Blue
        [75, 0, 130],    // Indigo  
        [238, 130, 238], // Violet
      ];
      // Slow animation to show the progression - each color shows for ~60 frames
      const speed = 0.05;
      const colorIdx = Math.floor(animFrame * speed) % rainbowColors.length;
      return rainbowColors[colorIdx];
    } else if (color === "zebra") {
      // Get current zebra color state with offset for stripes
      const animFrame = $api?.paintCount ? Number($api.paintCount) : Date.now() / 100;
      const zebraColors = [
        [0, 0, 0],       // Black
        [255, 255, 255], // White
      ];
      // Faster animation to show the alternation - each color shows for ~30 frames  
      const speed = 0.1;
      
      // Count how many zebra instances come before this one for offset
      const zebraOffset = colors.slice(0, index).filter(c => c === "zebra").length;
      const colorIdx = (Math.floor(animFrame * speed) + zebraOffset) % zebraColors.length;
      return zebraColors[colorIdx];
    }
    
    return color; // Return as-is if not dynamic
  });

  if (resolvedColors.length === 1) return resolvedColors[0];
  if (resolvedColors.length === 2) {
    const r = Math.round(resolvedColors[0][0] * (1 - t) + resolvedColors[1][0] * t);
    const g = Math.round(resolvedColors[0][1] * (1 - t) + resolvedColors[1][1] * t);
    const b = Math.round(resolvedColors[0][2] * (1 - t) + resolvedColors[1][2] * t);
    return [r, g, b];
  }
  
  // Multi-color interpolation
  const segmentFloat = t * (resolvedColors.length - 1);
  const segment = Math.floor(segmentFloat);
  const localT = segmentFloat - segment;
  
  if (segment >= resolvedColors.length - 1) {
    return resolvedColors[resolvedColors.length - 1];
  }
  
  const color1 = resolvedColors[segment];
  const color2 = resolvedColors[segment + 1];
  const r = Math.round(color1[0] * (1 - localT) + color2[0] * localT);
  const g = Math.round(color1[1] * (1 - localT) + color2[1] * localT);
  const b = Math.round(color1[2] * (1 - localT) + color2[2] * localT);
  return [r, g, b];
}

// Helper function to draw animated rainbow square
function drawRainbowSquare(ink, x, y, size, $api) {
  // Rainbow colors sequence
  const rainbowColors = [
    [255, 0, 0],     // Red
    [255, 165, 0],   // Orange  
    [255, 255, 0],   // Yellow
    [0, 255, 0],     // Green
    [0, 0, 255],     // Blue
    [128, 0, 128],   // Purple
  ];
  
  // Create animation based on paint count or time
  const animFrame = Number($api?.paintCount || 0n) || Date.now() / 100;
  const speed = 0.05; // Slower animation speed
  
  // Calculate which color to show (cycling through the sequence)
  const colorIndex = Math.floor(animFrame * speed) % rainbowColors.length;
  const currentColor = rainbowColors[colorIndex];
  
  // Draw the current color for the entire square
  ink(currentColor).box(x, y, size, size);
}

// Helper function to draw animated zebra square
function drawZebraSquare(ink, x, y, size, $api) {
  // Zebra colors sequence (black and white)
  const zebraColors = [
    [0, 0, 0],       // Black
    [255, 255, 255], // White
  ];
  
  // Create animation based on paint count or time
  const animFrame = Number($api?.paintCount || 0n) || Date.now() / 100;
  const speed = 0.1; // Faster animation speed for zebra
  
  // Calculate which color to show (cycling between black and white)
  const colorIndex = Math.floor(animFrame * speed) % zebraColors.length;
  const currentColor = zebraColors[colorIndex];
  
  // Draw the current color for the entire square
  ink(currentColor).box(x, y, size, size);
}
