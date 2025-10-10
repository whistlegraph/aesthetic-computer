// Gamepad, 2024.11.08.02.41.47.840
// Test your gamepad connectivity and view device information.

/* 📝 Notes
  - Shows connected gamepad information (type, buttons, axes)
  - Displays real-time input values through bios event system
  - No direct navigator.getGamepads() access (not available in paint context)
  - Color-coded buttons/axes using lib/gamepad-mappings.mjs
*/

import { 
  getButtonName, 
  getAxisName, 
  getButtonColors, 
  getAxisColors,
  getButtonMapping,
  getAxisMapping
} from "../lib/gamepad-mappings.mjs";

import { cssColors } from "../lib/num.mjs";

const { keys } = Object;
const connectedGamepads = {};
const buttonHistory = []; // Store button press history
const MAX_HISTORY = 256; // Enough frames to fill full screen height
const recentEvents = [];
const MAX_EVENTS = 10;
const FONT = "MatrixChunky8"; // Pixel-perfect font

let timelinePainting; // Separate painting buffer for timeline

function drawGamepadDiagram({ ink, line, circle, box }, gp, x, y) {
  const baseX = x;
  const baseY = y;
  const gamepadId = gp.id || "standard";
  
  // Get button colors from mapping
  const getColors = (btnIndex) => {
    const colors = getButtonColors(gamepadId, btnIndex);
    const isPressed = gp.pressedButtons.includes(btnIndex);
    // Return active color when pressed, inactive when not
    return isPressed ? colors.active : colors.inactive;
  };
  
  const getAxisColor = (axisIndex, direction = null) => {
    const colors = getAxisColors(gamepadId, axisIndex);
    const value = parseFloat(gp.axes[axisIndex.toString()] || 0);
    
    // If axis has directional colors (object with left/right or up/down)
    if (typeof colors.active === "object" && direction) {
      // Check if this specific direction is currently active
      const isActive = Math.abs(value) > 0.5;
      if (isActive) {
        // Return bright color if pressed in this direction
        if (direction === "left" && value < -0.5) return colors.active.left;
        if (direction === "right" && value > 0.5) return colors.active.right;
        if (direction === "up" && value < -0.5) return colors.active.up;
        if (direction === "down" && value > 0.5) return colors.active.down;
      }
      // When not pressed, show the general inactive color
      return colors.inactive;
    }
    
    // Simple active/inactive (backwards compatibility)
    return Math.abs(value) > 0.5 ? colors.active : colors.inactive;
  };
  
  // 8BitDo Micro - ultra-compact design
  const width = 48;
  const height = 24;
  
  // Check if any input is active
  const hasActiveInput = gp.pressedButtons.length > 0 || 
    Object.values(gp.axes).some(v => Math.abs(parseFloat(v)) > 0.5);
  
  // Solid background box - lightens when any input is active
  ink(hasActiveInput ? "slategray" : "dimgray").box(baseX, baseY, width, height);
  
  // D-Pad on left side - moved more to the left
  const dpadX = baseX + 8;
  const dpadY = baseY + 12;
  const dpadSize = 5;
  
  // D-pad as perfect cross (5 pieces touching)
  ink(getAxisColor(1, "up")).box(dpadX, dpadY - 5, dpadSize, dpadSize);      // Up
  ink(getAxisColor(1, "down")).box(dpadX, dpadY + 5, dpadSize, dpadSize);    // Down
  ink(getAxisColor(0, "left")).box(dpadX - 5, dpadY, dpadSize, dpadSize);    // Left
  ink(getAxisColor(0, "right")).box(dpadX + 5, dpadY, dpadSize, dpadSize);   // Right
  ink("black").box(dpadX, dpadY, dpadSize, dpadSize);                        // Center
  
  // Face buttons on right side - moved more to the left
  const faceX = baseX + 35;
  const faceY = baseY + 12;
  const btnSize = 5;
  
  // Draw buttons in perfect diamond layout
  ink(getColors(3)).box(faceX, faceY - 6, btnSize, btnSize);    // Top (X)
  ink(getColors(1)).box(faceX, faceY + 6, btnSize, btnSize);    // Bottom (B)
  ink(getColors(4)).box(faceX - 6, faceY, btnSize, btnSize);    // Left (Y)
  ink(getColors(0)).box(faceX + 6, faceY, btnSize, btnSize);    // Right (A)
  
  // Select/Start in center
  const centerX = baseX + 24;
  ink(getColors(10)).box(centerX - 5, baseY + 3, 4, 3); // Select
  ink(getColors(11)).box(centerX + 1, baseY + 3, 4, 3);  // Start
  
  // L/R shoulders FLUSH with top of box
  const shoulderY = baseY;
  ink(getColors(6)).box(baseX, shoulderY, 14, 2);  // L (wider)
  ink(getColors(7)).box(baseX + 34, shoulderY, 14, 2); // R (wider)
  
  // L2/R2 triggers - with gap in center
  ink(getColors(8)).box(baseX + 14, shoulderY, 8, 2);  // L2 (narrower, gap on right)
  ink(getColors(9)).box(baseX + 26, shoulderY, 8, 2); // R2 (narrower, gap on left)
  
  // Home/Heart button at bottom center
  ink(getColors(12)).box(centerX - 2, baseY + 19, 4, 3);
}

function paint({ wipe, ink, write, screen, line, circle, box, painting, paste, help }) {
  wipe("black");
  
  const lineHeight = 10;
  const hudHeight = 20; // Space for prompt HUD at top
  
  // Show connected gamepads
  const gamepadIndices = keys(connectedGamepads);
  
  // Update button history every frame (even if empty)
  if (gamepadIndices.length > 0) {
    const gp = connectedGamepads[gamepadIndices[0]];
    const gamepadId = gp.id || "standard";
    
    // Capture current frame's active buttons with their colors
    const frameData = {
      buttons: gp.pressedButtons.map(bi => ({
        index: bi,
        color: getButtonColors(gamepadId, bi).active
      })),
      axes: keys(gp.axes).map(ai => {
        const axisIndex = parseInt(ai);
        const value = parseFloat(gp.axes[ai]);
        if (Math.abs(value) > 0.5) {
          const colors = getAxisColors(gamepadId, axisIndex);
          if (typeof colors.active === "object") {
            // Get directional color
            if (value < -0.5) return { index: axisIndex, color: colors.active.left || colors.active.up };
            if (value > 0.5) return { index: axisIndex, color: colors.active.right || colors.active.down };
          }
          return { index: axisIndex, color: colors.active };
        }
        return null;
      }).filter(a => a !== null)
    };
    
    // Add to history
    buttonHistory.unshift(frameData);
    if (buttonHistory.length > MAX_HISTORY) {
      buttonHistory.pop();
    }
  }
  
  if (gamepadIndices.length > 0) {
    const gp = connectedGamepads[gamepadIndices[0]];
    const gamepadId = gp.id || "standard";
    
    // Draw controller diagram at bottom center
    const diagramWidth = 48;
    const diagramHeight = 24;
    const diagramX = (screen.width - diagramWidth) / 2;
    const diagramY = screen.height - diagramHeight - 50; // Near bottom with space
    
    // Get current active inputs
    const currentButtons = gp.pressedButtons.map(bi => ({
      color: getButtonColors(gamepadId, bi).active
    }));
    
    const currentAxes = [];
    keys(gp.axes).forEach(ai => {
      const axisIndex = parseInt(ai);
      const value = parseFloat(gp.axes[ai]);
      if (Math.abs(value) > 0.5) {
        const colors = getAxisColors(gamepadId, axisIndex);
        let color;
        
        if (typeof colors.active === "object") {
          // Get directional color based on axis value
          if (axisIndex === 0) { // Horizontal axis
            color = value < 0 ? colors.active.left : colors.active.right;
          } else if (axisIndex === 1) { // Vertical axis
            color = value < 0 ? colors.active.up : colors.active.down;
          } else {
            color = colors.active.left || colors.active.right || colors.active.up || colors.active.down;
          }
        } else {
          color = colors.active;
        }
        
        currentAxes.push({ color });
      }
    });
    
    const currentInputs = [...currentButtons, ...currentAxes];
    
    // Initialize timeline painting buffer
    const timelineHeight = diagramY - 20;
    if (!timelinePainting) {
      timelinePainting = painting(screen.width, timelineHeight);
      // Fill with dimgray initially
      for (let i = 0; i < timelinePainting.pixels.length; i += 4) {
        timelinePainting.pixels[i + 0] = 105; // dimgray R
        timelinePainting.pixels[i + 1] = 105; // dimgray G
        timelinePainting.pixels[i + 2] = 105; // dimgray B
        timelinePainting.pixels[i + 3] = 255; // A
      }
    }
    
    // Update timeline: scroll pixels up by 1, add new line at bottom
    // Scroll existing pixels up
    for (let y = 0; y < timelineHeight - 1; y++) {
      for (let x = 0; x < screen.width; x++) {
        const srcIdx = (x + (y + 1) * screen.width) * 4;
        const dstIdx = (x + y * screen.width) * 4;
        timelinePainting.pixels[dstIdx + 0] = timelinePainting.pixels[srcIdx + 0];
        timelinePainting.pixels[dstIdx + 1] = timelinePainting.pixels[srcIdx + 1];
        timelinePainting.pixels[dstIdx + 2] = timelinePainting.pixels[srcIdx + 2];
        timelinePainting.pixels[dstIdx + 3] = timelinePainting.pixels[srcIdx + 3];
      }
    }
    
    // Draw new line at bottom (newest frame)
    const newLineY = timelineHeight - 1;
    const totalInputs = currentInputs.length;
    
    if (totalInputs > 0) {
      // Get all RGB values
      const colors = currentInputs.map(input => cssColors[input.color] || [100, 100, 100]);
      
      if (totalInputs === 1) {
        // Single input: solid color
        const rgb = colors[0];
        for (let x = 0; x < screen.width; x++) {
          const pixelIdx = (x + newLineY * screen.width) * 4;
          timelinePainting.pixels[pixelIdx + 0] = rgb[0];
          timelinePainting.pixels[pixelIdx + 1] = rgb[1];
          timelinePainting.pixels[pixelIdx + 2] = rgb[2];
          timelinePainting.pixels[pixelIdx + 3] = 255;
        }
      } else {
        // Multiple inputs: create gradient blend across width
        for (let x = 0; x < screen.width; x++) {
          // Map x position to smooth interpolation between colors
          const position = (x / screen.width) * totalInputs;
          const idx1 = Math.floor(position) % totalInputs;
          const idx2 = (idx1 + 1) % totalInputs;
          const t = position - Math.floor(position); // 0 to 1 blend factor
          
          const color1 = colors[idx1];
          const color2 = colors[idx2];
          
          // Smooth interpolation between colors
          const r = Math.floor(color1[0] * (1 - t) + color2[0] * t);
          const g = Math.floor(color1[1] * (1 - t) + color2[1] * t);
          const b = Math.floor(color1[2] * (1 - t) + color2[2] * t);
          
          const pixelIdx = (x + newLineY * screen.width) * 4;
          timelinePainting.pixels[pixelIdx + 0] = r;
          timelinePainting.pixels[pixelIdx + 1] = g;
          timelinePainting.pixels[pixelIdx + 2] = b;
          timelinePainting.pixels[pixelIdx + 3] = 255;
        }
      }
    }
    // Don't draw anything when no inputs (keep previous pixels)
    
    // Paste timeline buffer to screen
    paste(timelinePainting, 0, 0);
    
    drawGamepadDiagram({ ink, line, circle, box }, gp, diagramX, diagramY);
    
    // Active inputs display - top left (below HUD, no label)
    let y = hudHeight + 10;
    
    // Show pressed buttons
    if (gp.pressedButtons && gp.pressedButtons.length > 0) {
      const btnNames = gp.pressedButtons.map(bi => getButtonName(gamepadId, bi)).join(" ");
      ink("lime").write(btnNames, { x: 10, y }, undefined, undefined, false, FONT);
      y += lineHeight;
    }
    
    // Show active axes
    if (gp.axes && keys(gp.axes).length > 0) {
      const axisStrs = keys(gp.axes).map(ai => {
        const name = getAxisName(gamepadId, parseInt(ai));
        return `${name}`;
      });
      axisStrs.forEach(str => {
        ink("orange").write(str, { x: 10, y }, undefined, undefined, false, FONT);
        y += lineHeight;
      });
    }
    
    // Gamepad name - bottom left with 1px shadow
    if (gp.id) {
      const bottomY = screen.height - lineHeight - 10;
      // Shadow
      ink("black").write(gp.id, { x: 11, y: bottomY + 1 }, undefined, undefined, false, FONT);
      // Text
      ink("yellow").write(gp.id, { x: 10, y: bottomY }, undefined, undefined, false, FONT);
    }
  } else {
    ink("orange").write("No gamepads", { x: 10, y }, undefined, undefined, false, FONT);
    y += lineHeight;
    ink("gray").write("Press any button", { x: 10, y }, undefined, undefined, false, FONT);
    y += lineHeight;
    ink("gray").write("to connect.", { x: 10, y }, undefined, undefined, false, FONT);
  }
}

function act({ event: e }) {
  if (e.is("gamepad")) {
    const gpIndex = e.gamepad;
    
    // Initialize gamepad entry if needed
    if (!connectedGamepads[gpIndex]) {
      connectedGamepads[gpIndex] = {
        id: e.gamepadId || null, // Capture gamepad name/type from event
        pressedButtons: [],
        axes: {},
        lastEvent: null,
      };
    }
    
    const gp = connectedGamepads[gpIndex];
    
    // Update gamepad ID if available (in case it wasn't set initially)
    if (e.gamepadId && !gp.id) {
      gp.id = e.gamepadId;
    }
    
    // Update last event
    gp.lastEvent = e.name;
    
    // Track button state
    if (e.is(`gamepad:${gpIndex}:button`)) {
      const buttonIndex = e.button;
      if (e.action === "push") {
        if (!gp.pressedButtons.includes(buttonIndex)) {
          gp.pressedButtons.push(buttonIndex);
        }
      } else if (e.action === "release") {
        gp.pressedButtons = gp.pressedButtons.filter(b => b !== buttonIndex);
      }
      
      // Add to recent events
      recentEvents.push(`P${gpIndex + 1} Button ${buttonIndex}: ${e.action}`);
      if (recentEvents.length > MAX_EVENTS) recentEvents.shift();
    }
    
    // Track axis state
    if (e.is(`gamepad:${gpIndex}:axis`)) {
      const axisIndex = e.axis;
      const value = e.value.toFixed(2);
      
      // Only track if above deadzone
      if (Math.abs(e.value) > 0.1) {
        gp.axes[axisIndex] = value;
      } else {
        delete gp.axes[axisIndex];
      }
    }
  }
}

// 📚 Library
// function boot() {
//   Runs once at the start.
// }

// function act({ event }) {
//   Respond to user input here.
// }

// function sim() {
//   Runs once per logic frame (120fps locked).
// }

// function beat() {
//   Runs once per metronomic BPM.
// }

// function leave() {
//   Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
//   Render a custom thumbnail image.
// }

// function icon() {
//   Render an application icon, aka favicon.
// }

export { paint, act };
