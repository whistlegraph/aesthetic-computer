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

const timelinePaintings = [null, null, null, null]; // One timeline per gamepad

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
  
  // Get connected gamepad indices
  const connectedIndices = keys(connectedGamepads).map(k => parseInt(k)).filter(i => connectedGamepads[i]);
  const numConnected = connectedIndices.length;
  
  const playerColors = ["cyan", "magenta", "lime", "yellow"];
  const playerLabels = ["P1", "P2", "P3", "P4"];
  
  if (numConnected === 0) {
    // No gamepads - show message
    const msgY = hudHeight + 50;
    ink("orange").write("No gamepads connected", { x: 10, y: msgY }, undefined, undefined, false, FONT);
    ink("gray").write("Press any button to connect", { x: 10, y: msgY + lineHeight }, undefined, undefined, false, FONT);
    return;
  }
  
  // Calculate layout based on number of connected controllers
  let cols, rows;
  if (numConnected === 1) {
    cols = 1; rows = 1;
  } else if (numConnected === 2) {
    cols = 2; rows = 1;
  } else if (numConnected <= 4) {
    cols = 2; rows = 2;
  } else {
    cols = 2; rows = 2; // Max 4
  }
  
  const slotWidth = screen.width / cols;
  const slotHeight = (screen.height - hudHeight) / rows;
  
  // Draw each connected gamepad
  connectedIndices.slice(0, 4).forEach((gpIndex, slotNum) => {
    const gp = connectedGamepads[gpIndex];
    if (!gp) return;
    
    const col = slotNum % cols;
    const row = Math.floor(slotNum / cols);
    const slotX = col * slotWidth;
    const slotY = hudHeight + row * slotHeight;
    const centerX = slotX + slotWidth / 2;
    const centerY = slotY + slotHeight / 2;
    
    // Player color
    const playerColor = playerColors[gpIndex];
    const playerLabel = playerLabels[gpIndex];
    
    // Draw border (only if multiple gamepads)
    if (numConnected > 1) {
      ink("dimgray").box(slotX, slotY, slotWidth - 1, slotHeight - 1, "line");
    }
    
    // Player label
    const labelY = slotY + 8;
    ink(playerColor).write(playerLabel, { x: slotX + 8, y: labelY }, undefined, undefined, false, FONT);
    
    // Calculate space for timeline - account for metadata section at bottom
    const diagramHeight = 24;
    const metadataHeight = 50; // Reserve space for controller name + readout
    const timelineTop = labelY + lineHeight + 4;
    const timelineBottom = slotY + slotHeight - diagramHeight - metadataHeight - 16;
    const timelineHeight = timelineBottom - timelineTop;
    
    if (timelineHeight > 20) {
      // Get current active inputs
      const gamepadId = gp.id || "standard";
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
            if (axisIndex === 0) {
              color = value < 0 ? colors.active.left : colors.active.right;
            } else if (axisIndex === 1) {
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
      
      // Initialize timeline painting buffer for this gamepad
      if (!timelinePaintings[gpIndex]) {
        timelinePaintings[gpIndex] = painting(slotWidth, timelineHeight);
        // Fill with dimgray initially
        for (let i = 0; i < timelinePaintings[gpIndex].pixels.length; i += 4) {
          timelinePaintings[gpIndex].pixels[i + 0] = 105;
          timelinePaintings[gpIndex].pixels[i + 1] = 105;
          timelinePaintings[gpIndex].pixels[i + 2] = 105;
          timelinePaintings[gpIndex].pixels[i + 3] = 255;
        }
      }
      
      const timeline = timelinePaintings[gpIndex];
      
      // Scroll pixels up by 1
      for (let y = 0; y < timelineHeight - 1; y++) {
        for (let x = 0; x < slotWidth; x++) {
          const srcIdx = (x + (y + 1) * slotWidth) * 4;
          const dstIdx = (x + y * slotWidth) * 4;
          timeline.pixels[dstIdx + 0] = timeline.pixels[srcIdx + 0];
          timeline.pixels[dstIdx + 1] = timeline.pixels[srcIdx + 1];
          timeline.pixels[dstIdx + 2] = timeline.pixels[srcIdx + 2];
          timeline.pixels[dstIdx + 3] = timeline.pixels[srcIdx + 3];
        }
      }
      
      // Draw new line at bottom with player color as base
      const newLineY = timelineHeight - 1;
      const baseColor = cssColors[playerColor] || [100, 100, 100];
      
      if (currentInputs.length > 0) {
        const colors = currentInputs.map(input => cssColors[input.color] || baseColor);
        
        for (let x = 0; x < slotWidth; x++) {
          const position = (x / slotWidth) * currentInputs.length;
          const idx1 = Math.floor(position) % currentInputs.length;
          const idx2 = (idx1 + 1) % currentInputs.length;
          const t = position - Math.floor(position);
          
          const color1 = colors[idx1];
          const color2 = colors[idx2];
          
          const r = Math.floor(color1[0] * (1 - t) + color2[0] * t);
          const g = Math.floor(color1[1] * (1 - t) + color2[1] * t);
          const b = Math.floor(color1[2] * (1 - t) + color2[2] * t);
          
          const pixelIdx = (x + newLineY * slotWidth) * 4;
          timeline.pixels[pixelIdx + 0] = r;
          timeline.pixels[pixelIdx + 1] = g;
          timeline.pixels[pixelIdx + 2] = b;
          timeline.pixels[pixelIdx + 3] = 255;
        }
      } else {
        // No input - draw dim player color
        for (let x = 0; x < slotWidth; x++) {
          const pixelIdx = (x + newLineY * slotWidth) * 4;
          timeline.pixels[pixelIdx + 0] = Math.floor(baseColor[0] * 0.2);
          timeline.pixels[pixelIdx + 1] = Math.floor(baseColor[1] * 0.2);
          timeline.pixels[pixelIdx + 2] = Math.floor(baseColor[2] * 0.2);
          timeline.pixels[pixelIdx + 3] = 255;
        }
      }
      
      // Paste timeline
      paste(timeline, slotX, timelineTop);
    }
    
    // Draw controller diagram and metadata at bottom
    const diagramWidth = 48;
    const diagramX = centerX - diagramWidth / 2;
    const diagramY = slotY + slotHeight - diagramHeight - metadataHeight - 8;
    
    drawGamepadDiagram({ ink, line, circle, box }, gp, diagramX, diagramY);
    
    // Show full gamepad name below diagram
    if (gp.id) {
      const nameY = diagramY + diagramHeight + 4;
      const maxWidth = slotWidth - 8;
      
      // Detect controller type and show friendly name
      let displayName = gp.id;
      if (displayName.includes("045e") && displayName.includes("0b13")) {
        displayName = "Xbox Controller";
      } else if (displayName.includes("2dc8") && displayName.includes("9020")) {
        displayName = "8BitDo Micro";
      } else {
        // Clean up generic names - remove vendor/product IDs
        displayName = displayName.replace(/\s*\(STANDARD GAMEPAD[^)]*\)/i, "");
        displayName = displayName.replace(/\s*\(Vendor:[^)]*\)/i, "");
      }
      
      // Wrap controller name if needed - use MatrixChunky8 font
      ink("white").write(displayName, { x: slotX + 4, y: nameY }, undefined, maxWidth, true, FONT);
      
      let currentY = nameY + lineHeight + 2;
      
      // Show pressed buttons readout - each button in its own box
      if (gp.pressedButtons.length > 0) {
        const buttonNames = gp.pressedButtons.map(b => getButtonName(gp.id, b));
        let buttonX = slotX + 4;
        
        buttonNames.forEach((name, i) => {
          // Measure text width (approximate for MatrixChunky8)
          const boxPadding = 3;
          const charWidth = 6; // Approximate width per character in MatrixChunky8
          const textWidth = name.length * charWidth;
          const boxWidth = textWidth + boxPadding * 2;
          
          // Check if this box would overflow the line
          if (buttonX + boxWidth > slotX + slotWidth - 4) {
            buttonX = slotX + 4; // Reset to left margin
            currentY += lineHeight + 4; // Move to next line
          }
          
          // Draw box
          ink(playerColor, 60).box(buttonX, currentY, boxWidth, lineHeight, "fill");
          ink(playerColor).box(buttonX, currentY, boxWidth, lineHeight, "line");
          
          // Draw text
          ink("white").write(name, { x: buttonX + boxPadding, y: currentY + 1 }, undefined, undefined, false, FONT);
          
          buttonX += boxWidth + 3; // Move right for next box
        });
        
        currentY += lineHeight + 4;
      }
      
      // Show active axes readout - format compactly
      const activeAxes = keys(gp.axes).filter(a => Math.abs(parseFloat(gp.axes[a])) > 0.1);
      if (activeAxes.length > 0) {
        // Group left stick and right stick separately for clarity
        const leftStick = activeAxes.filter(a => parseInt(a) <= 1);
        const rightStick = activeAxes.filter(a => parseInt(a) >= 2 && parseInt(a) <= 3);
        const otherAxes = activeAxes.filter(a => parseInt(a) > 3);
        
        const parts = [];
        if (leftStick.length > 0) {
          const values = leftStick.map(a => gp.axes[a]).join(",");
          parts.push(`LS:${values}`);
        }
        if (rightStick.length > 0) {
          const values = rightStick.map(a => gp.axes[a]).join(",");
          parts.push(`RS:${values}`);
        }
        if (otherAxes.length > 0) {
          const values = otherAxes.map(a => `A${a}:${gp.axes[a]}`).join(" ");
          parts.push(values);
        }
        
        const axesText = parts.join(" ");
        ink(playerColor, 180).write(axesText, { x: slotX + 4, y: currentY }, undefined, maxWidth, true, FONT);
      }
    }
  });
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
        lastLoggedAxes: {}, // Track last logged axis values to avoid spam
        lastEvent: null,
      };
      console.log(`🎮 Gamepad ${gpIndex} connected:`, e.gamepadId);
      console.log("📋 Xbox Controller Button Mapping Reference:");
      console.log("Button 0: A (bottom face button)");
      console.log("Button 1: B (right face button)");
      console.log("Button 2: X (left face button)");
      console.log("Button 3: Y (top face button)");
      console.log("Button 4: LB (left bumper)");
      console.log("Button 5: RB (right bumper)");
      console.log("Button 6: LT (left trigger)");
      console.log("Button 7: RT (right trigger)");
      console.log("Button 8: Back/View");
      console.log("Button 9: Start/Menu");
      console.log("Button 10: LS (left stick press)");
      console.log("Button 11: RS (right stick press)");
      console.log("Button 12: D-Pad Up");
      console.log("Button 13: D-Pad Down");
      console.log("Button 14: D-Pad Left");
      console.log("Button 15: D-Pad Right");
      console.log("Button 16: Xbox/Guide button");
      console.log("📐 Axis Mapping:");
      console.log("Axis 0: Left stick X (left: -1, right: +1)");
      console.log("Axis 1: Left stick Y (up: -1, down: +1)");
      console.log("Axis 2: Right stick X (left: -1, right: +1)");
      console.log("Axis 3: Right stick Y (up: -1, down: +1)");
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
        console.log(`🔴 Button ${buttonIndex} PRESSED -`, getButtonName(gp.id, buttonIndex));
      } else if (e.action === "release") {
        gp.pressedButtons = gp.pressedButtons.filter(b => b !== buttonIndex);
        console.log(`⚪ Button ${buttonIndex} RELEASED -`, getButtonName(gp.id, buttonIndex));
      }
      
      // Add to recent events
      recentEvents.push(`P${gpIndex + 1} Button ${buttonIndex}: ${e.action}`);
      if (recentEvents.length > MAX_EVENTS) recentEvents.shift();
    }
    
    // Track axis state
    if (e.is(`gamepad:${gpIndex}:axis`)) {
      const axisIndex = e.axis;
      const value = e.value;
      
      // Only track/log if above deadzone
      if (Math.abs(value) > 0.1) {
        gp.axes[axisIndex] = value.toFixed(2);
        
        // Only log if value changed significantly (>0.15 difference from last logged value)
        const lastLogged = gp.lastLoggedAxes[axisIndex] || 0;
        if (Math.abs(value - lastLogged) > 0.15) {
          console.log(`🕹️ Axis ${axisIndex} -`, getAxisName(gp.id, axisIndex), "value:", value.toFixed(2));
          gp.lastLoggedAxes[axisIndex] = value;
        }
      } else {
        // Reset to center
        delete gp.axes[axisIndex];
        if (gp.lastLoggedAxes[axisIndex] !== 0) {
          console.log(`🕹️ Axis ${axisIndex} -`, getAxisName(gp.id, axisIndex), "centered");
          gp.lastLoggedAxes[axisIndex] = 0;
        }
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
