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
  
  // Show up to 4 gamepads in a 2x2 grid
  const playerColors = ["cyan", "magenta", "lime", "yellow"];
  const playerLabels = ["P1", "P2", "P3", "P4"];
  
  // Calculate grid positions
  const gridWidth = screen.width / 2;
  const gridHeight = (screen.height - hudHeight) / 2;
  const gridPositions = [
    { x: 0, y: hudHeight },                    // Top-left (P1)
    { x: gridWidth, y: hudHeight },           // Top-right (P2)
    { x: 0, y: hudHeight + gridHeight },      // Bottom-left (P3)
    { x: gridWidth, y: hudHeight + gridHeight } // Bottom-right (P4)
  ];
  
  // Draw each gamepad slot
  for (let i = 0; i < 4; i++) {
    const pos = gridPositions[i];
    const centerX = pos.x + gridWidth / 2;
    const centerY = pos.y + gridHeight / 2;
    
    // Draw border around each quadrant
    ink("dimgray").box(pos.x, pos.y, gridWidth - 1, gridHeight - 1, "line");
    
    // Player label at top
    const labelY = pos.y + 8;
    ink(playerColors[i]).write(playerLabels[i], { x: pos.x + 8, y: labelY }, undefined, undefined, false, FONT);
    
    // Check if gamepad is connected
    const gp = connectedGamepads[i];
    
    if (gp && gp.id) {
      // Gamepad connected - draw diagram
      const diagramWidth = 48;
      const diagramHeight = 24;
      const diagramX = centerX - diagramWidth / 2;
      const diagramY = centerY - diagramHeight / 2;
      
      drawGamepadDiagram({ ink, line, circle, box }, gp, diagramX, diagramY);
      
      // Show gamepad name below diagram
      const nameY = diagramY + diagramHeight + 8;
      const nameText = gp.id.length > 20 ? gp.id.substring(0, 17) + "..." : gp.id;
      ink("white").write(nameText, { x: pos.x + 8, y: nameY }, undefined, undefined, false, FONT);
      
      // Show active buttons
      let infoY = nameY + lineHeight;
      if (gp.pressedButtons && gp.pressedButtons.length > 0) {
        const btnNames = gp.pressedButtons.map(bi => getButtonName(gp.id || "standard", bi)).join(" ");
        const btnText = btnNames.length > 15 ? btnNames.substring(0, 12) + "..." : btnNames;
        ink("lime").write(btnText, { x: pos.x + 8, y: infoY }, undefined, undefined, false, FONT);
        infoY += lineHeight;
      }
      
      // Show active axes
      if (gp.axes && keys(gp.axes).length > 0) {
        const axisNames = keys(gp.axes).slice(0, 2).map(ai => {
          return getAxisName(gp.id || "standard", parseInt(ai));
        });
        const axisText = axisNames.join(" ");
        if (axisText.length > 0) {
          ink("orange").write(axisText.substring(0, 15), { x: pos.x + 8, y: infoY }, undefined, undefined, false, FONT);
        }
      }
    } else {
      // No gamepad connected
      const msgY = centerY;
      ink("gray").write("No gamepad", { x: pos.x + 8, y: msgY }, undefined, undefined, false, FONT);
      ink("darkgray").write("Press button", { x: pos.x + 8, y: msgY + lineHeight }, undefined, undefined, false, FONT);
      ink("darkgray").write("to connect", { x: pos.x + 8, y: msgY + lineHeight * 2 }, undefined, undefined, false, FONT);
    }
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
