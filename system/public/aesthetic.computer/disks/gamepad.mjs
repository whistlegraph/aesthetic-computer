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

// Stable pitches make the test useful as a tiny instrument. The M30 face
// layout (A B C / X Y Z) lands on two related three-note rows.
const BUTTON_TONES = [261.63, 329.63, 523.25, 659.25, 783.99, 392.0,
  174.61, 196.0, 220.0, 246.94, 293.66, 349.23, 880.0, 987.77, 1174.66,
  1318.51, 1567.98];
const buttonVoices = new Map();
const latencySamples = [];
const MAX_LATENCY_SAMPLES = 60;
const SAMPLE_RATES = [44100, 48000, 96000];
let lastLatency = null;
let audioLatency = null;
let audioProbe = null;
let probeSequence = 0;
let requestedSampleRate = 48000;
let sampleRateIndex = 1;
let changingSampleRate = false;
const rateStats = new Map();
let sendToBios = null;
let lightMode = false;
const gamepadTheme = () => lightMode
  ? { bg: [244, 242, 235], panel: [224, 220, 211], activePanel: [184, 190, 196], border: [135, 130, 142], text: [45, 42, 54], quiet: [94, 90, 104], timeline: [205, 202, 194] }
  : { bg: [0, 0, 0], panel: [105, 105, 105], activePanel: [112, 128, 144], border: [128, 128, 128], text: [255, 255, 255], quiet: [128, 128, 128], timeline: [105, 105, 105] };

const timelinePaintings = [null, null, null, null]; // One timeline per gamepad

function drawXboxControllerDiagram({ ink, line, circle, box }, gp, x, y) {
  const baseX = x;
  const baseY = y;
  const gamepadId = gp.id;
  
  // Get button colors from mapping
  const getColors = (btnIndex) => {
    const colors = getButtonColors(gamepadId, btnIndex);
    const isPressed = gp.pressedButtons.includes(btnIndex);
    return isPressed ? colors.active : colors.inactive;
  };
  
  const width = 56;
  const height = 28;
  
  // Check if any input is active
  const hasActiveInput = gp.pressedButtons.length > 0 || 
    Object.values(gp.axes).some(v => Math.abs(parseFloat(v)) > 0.5);
  
  // Solid background box - lightens when any input is active
  ink(...(hasActiveInput ? gamepadTheme().activePanel : gamepadTheme().panel)).box(baseX, baseY, width, height);
  
  // LEFT SIDE: Left stick at top-left, D-pad below (REVERSED from before)
  const lStickX = baseX + 8;  // Moved further left
  const lStickY = baseY + 7;
  const axisVal0 = parseFloat(gp.axes["0"] || 0);
  const axisVal1 = parseFloat(gp.axes["1"] || 0);
  const lStickPressed = gp.pressedButtons.includes(10);
  const lStickActive = Math.abs(axisVal0) > 0.1 || Math.abs(axisVal1) > 0.1 || lStickPressed;
  
  // Draw left stick circle (larger)
  ink(lStickActive ? "cyan" : "gray").circle(lStickX, lStickY, 4);
  
  // Draw dot showing stick position
  const dotX = lStickX + Math.round(axisVal0 * 3);
  const dotY = lStickY + Math.round(axisVal1 * 3);
  ink(lStickPressed ? "white" : "cyan").box(dotX, dotY, 1, 1);
  
  // D-pad below left stick (buttons 12-15)
  const dpadX = lStickX + 2;  // Shifted right to give more space
  const dpadY = baseY + 18;
  const dpadSize = 4;
  
  ink(getColors(12)).box(dpadX, dpadY - 4, dpadSize, dpadSize);      // Up
  ink(getColors(13)).box(dpadX, dpadY + 4, dpadSize, dpadSize);      // Down
  ink(getColors(14)).box(dpadX - 4, dpadY, dpadSize, dpadSize);      // Left
  ink(getColors(15)).box(dpadX + 4, dpadY, dpadSize, dpadSize);      // Right
  ink("black").box(dpadX, dpadY, dpadSize, dpadSize);                // Center
  
  // RIGHT SIDE: ABXY buttons at top-right, Right stick below (moved down a bit)
  const faceX = baseX + 46;
  const faceY = baseY + 9;  // Moved down from 6
  const btnSize = 4;
  
  // ABXY in diamond formation (0-3)
  ink(getColors(3)).box(faceX, faceY - 4, btnSize, btnSize);         // Y (top)
  ink(getColors(0)).box(faceX, faceY + 4, btnSize, btnSize);         // A (bottom)
  ink(getColors(2)).box(faceX - 4, faceY, btnSize, btnSize);         // X (left)
  ink(getColors(1)).box(faceX + 4, faceY, btnSize, btnSize);         // B (right)
  
  // Right stick below ABXY (axis 2-3, button 11)
  const rStickX = faceX - 5;  // Moved further left
  const rStickY = baseY + 21;  // Moved down lower
  const axisVal2 = parseFloat(gp.axes["2"] || 0);
  const axisVal3 = parseFloat(gp.axes["3"] || 0);
  const rStickPressed = gp.pressedButtons.includes(11);
  const rStickActive = Math.abs(axisVal2) > 0.1 || Math.abs(axisVal3) > 0.1 || rStickPressed;
  
  // Draw right stick circle (larger)
  ink(rStickActive ? "yellow" : "gray").circle(rStickX, rStickY, 4);
  
  // Draw dot showing stick position
  const dotX2 = rStickX + Math.round(axisVal2 * 3);
  const dotY2 = rStickY + Math.round(axisVal3 * 3);
  ink(rStickPressed ? "white" : "yellow").box(dotX2, dotY2, 1, 1);
  
  // CENTER: Xbox button with View/Menu below it
  const centerX = baseX + width / 2;
  const xboxBtnY = baseY + 8;  // Moved down
  
  // Xbox button with X inside
  ink(getColors(16)).circle(centerX, xboxBtnY, 2);
  ink("black").line(centerX - 1, xboxBtnY - 1, centerX + 1, xboxBtnY + 1);  // X diagonal 1
  ink("black").line(centerX - 1, xboxBtnY + 1, centerX + 1, xboxBtnY - 1);  // X diagonal 2
  
  // View (Select) and Menu (Start) buttons below Xbox button
  ink(getColors(8)).box(centerX - 5, xboxBtnY + 5, 3, 2);   // View/Select (left)
  ink(getColors(9)).box(centerX + 2, xboxBtnY + 5, 3, 2);   // Menu/Start (right)
  
  // TOP: Triggers (outer) and Bumpers (inner) - SWAPPED
  const topY = baseY;
  ink(getColors(6)).box(baseX + 2, topY, 12, 2);         // LT (outer/left)
  ink(getColors(4)).box(baseX + 14, topY, 8, 2);         // LB (inner)

  ink(getColors(7)).box(baseX + width - 14, topY, 12, 2); // RT (outer/right)
  ink(getColors(5)).box(baseX + width - 22, topY, 8, 2); // RB (inner)
}

function draw8BitDoMicroDiagram({ ink, line, circle, box }, gp, x, y) {
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
  ink(...(hasActiveInput ? gamepadTheme().activePanel : gamepadTheme().panel)).box(baseX, baseY, width, height);
  
  // D-Pad on left side - using AXES like original mapping
  const dpadX = baseX + 8;
  const dpadY = baseY + 12;
  const dpadSize = 5;
  
  // D-pad as perfect cross (using axes 0-1 for 8BitDo) - always visible
  const upColor = getAxisColor(1, "up");
  const downColor = getAxisColor(1, "down");
  const leftColor = getAxisColor(0, "left");
  const rightColor = getAxisColor(0, "right");
  
  // Use gray as default if inactive color is too dark
  ink(upColor === "dimgray" ? "gray" : upColor).box(dpadX, dpadY - 5, dpadSize, dpadSize);      // Up
  ink(downColor === "dimgray" ? "gray" : downColor).box(dpadX, dpadY + 5, dpadSize, dpadSize);  // Down
  ink(leftColor === "dimgray" ? "gray" : leftColor).box(dpadX - 5, dpadY, dpadSize, dpadSize);  // Left
  ink(rightColor === "dimgray" ? "gray" : rightColor).box(dpadX + 5, dpadY, dpadSize, dpadSize);// Right
  ink("black").box(dpadX, dpadY, dpadSize, dpadSize);                                            // Center
  
  // Face buttons on right side (ABXY = buttons 0-3)
  const faceX = baseX + 35;
  const faceY = baseY + 12;
  const btnSize = 5;
  
  // Draw buttons in perfect diamond layout (original mapping)
  ink(getColors(3)).box(faceX, faceY - 6, btnSize, btnSize);    // Top (X)
  ink(getColors(1)).box(faceX, faceY + 6, btnSize, btnSize);    // Bottom (B)
  ink(getColors(4)).box(faceX - 6, faceY, btnSize, btnSize);    // Left (Y)
  ink(getColors(0)).box(faceX + 6, faceY, btnSize, btnSize);    // Right (A)
  
  // Select/Start in center (original mapping)
  const centerX = baseX + 24;
  ink(getColors(10)).box(centerX - 5, baseY + 3, 4, 3); // Select
  ink(getColors(11)).box(centerX + 1, baseY + 3, 4, 3);  // Start
  
  // L/R shoulders FLUSH with top of box (original mapping)
  const shoulderY = baseY;
  ink(getColors(6)).box(baseX, shoulderY, 14, 2);  // L (wider)
  ink(getColors(7)).box(baseX + 34, shoulderY, 14, 2); // R (wider)
  
  // L2/R2 triggers - with gap in center (original mapping)
  ink(getColors(8)).box(baseX + 14, shoulderY, 8, 2);  // L2 (narrower, gap on right)
  ink(getColors(9)).box(baseX + 26, shoulderY, 8, 2); // R2 (narrower, gap on left)

  // Home/Heart button at bottom center
  ink(getColors(12)).box(centerX - 2, baseY + 19, 4, 3);
}

function draw8BitDoM30Diagram({ ink }, gp, x, y) {
  const color = (button) => {
    const colors = getButtonColors(gp.id, button);
    return gp.pressedButtons.includes(button) ? colors.active : colors.inactive;
  };
  const axisX = parseFloat(gp.axes["0"] || 0);
  const axisY = parseFloat(gp.axes["1"] || 0);
  const active = gp.pressedButtons.length || Math.abs(axisX) > 0.3 || Math.abs(axisY) > 0.3;
  const w = 56;

  // The M30's low, rounded silhouette and its defining two rows of three.
  ink(...(active ? gamepadTheme().activePanel : gamepadTheme().panel)).box(x + 4, y, w - 8, 24);
  ink(...(active ? gamepadTheme().activePanel : gamepadTheme().panel)).box(x, y + 5, w, 14);

  const dx = x + 10, dy = y + 12, ds = 4;
  ink(axisY < -0.3 || gp.pressedButtons.includes(12) ? "lime" : "gray").box(dx, dy - 5, ds, ds);
  ink(axisY > 0.3 || gp.pressedButtons.includes(13) ? "lime" : "gray").box(dx, dy + 5, ds, ds);
  ink(axisX < -0.3 || gp.pressedButtons.includes(14) ? "lime" : "gray").box(dx - 5, dy, ds, ds);
  ink(axisX > 0.3 || gp.pressedButtons.includes(15) ? "lime" : "gray").box(dx + 5, dy, ds, ds);
  ink("black").box(dx, dy, ds, ds);

  const fx = x + 34, fy = y + 7, bs = 4, gap = 6;
  [2, 3, 4].forEach((button, i) => ink(color(button)).box(fx + i * gap, fy, bs, bs));
  [0, 1, 5].forEach((button, i) => ink(color(button)).box(fx + i * gap, fy + 8, bs, bs));
  ink(color(8)).box(x + 23, y + 8, 3, 2);
  ink(color(9)).box(x + 27, y + 8, 3, 2);
}

function drawGamepadDiagram({ ink, line, circle, box }, gp, x, y) {
  // Detect controller type and use appropriate diagram
  if (/m30|^xbox one game controller$/i.test(gp.id || "")) {
    draw8BitDoM30Diagram({ ink, line, circle, box }, gp, x, y);
  } else if (/xbox|xinput|045e/i.test(gp.id || "")) {
    // Xbox Controller
    drawXboxControllerDiagram({ ink, line, circle, box }, gp, x, y);
  } else {
    // 8BitDo Micro or generic
    draw8BitDoMicroDiagram({ ink, line, circle, box }, gp, x, y);
  }
}

function paint({ wipe, ink, write, screen, line, circle, box, painting, paste, help, text, dark }) {
  const nextLightMode = dark === false;
  if (nextLightMode !== lightMode) timelinePaintings.fill(null);
  lightMode = nextLightMode;
  const theme = gamepadTheme();
  wipe(...theme.bg);
  
  const lineHeight = 10;
  const hudHeight = 20; // Space for prompt HUD at top
  
  // Get connected gamepad indices
  const connectedIndices = keys(connectedGamepads).map(k => parseInt(k)).filter(i => connectedGamepads[i]);
  const numConnected = connectedIndices.length;
  
  const playerColors = lightMode
    ? ["blue", "purple", "green", "darkorange"]
    : ["cyan", "magenta", "lime", "yellow"];
  const playerLabels = ["P1", "P2", "P3", "P4"];

  // These cover the software path only: input poll -> piece delivery, then
  // time spent submitting the synth. HDMI/TV acoustic latency is downstream.
  if (lastLatency) {
    const average = latencySamples.reduce((sum, sample) => sum + sample, 0) /
      latencySamples.length;
    const browserAudio = audioLatency
      ? ` BASE ${audioLatency.base.toFixed(1)}ms OUT ${audioLatency.output.toFixed(1)}ms Q ${audioLatency.quantum.toFixed(1)}ms`
      : " AUDIO --";
    const worker = audioProbe
      ? ` P>B ${audioProbe.pieceToBios.toFixed(1)}ms WRT ${audioProbe.workletRoundTrip.toFixed(1)}ms ATK ${audioProbe.attackMs.toFixed(1)}ms/${audioProbe.attackFrames}smp ${audioProbe.payloadBytes}B V${audioProbe.runningCount}`
      : " WORK --";
    const latencyText = `IN ${lastLatency.delivery.toFixed(0)}ms AVG ${average.toFixed(0)}ms SYN ${lastLatency.synth.toFixed(2)}ms${browserAudio}${worker}`;
    const width = latencyText.length * 4 + 6;
    const x = Math.max(2, screen.width - width - 4);
    const y = hudHeight;
    ink(...(lightMode ? [255, 253, 247, 225] : [0, 0, 0, 210])).box(x, y, width, lineHeight + 2);
    const latencyColor = lightMode
      ? lastLatency.delivery <= 20 ? "green" : lastLatency.delivery <= 35 ? "darkorange" : "red"
      : lastLatency.delivery <= 20 ? "lime" : lastLatency.delivery <= 35 ? "yellow" : "red";
    ink(latencyColor)
      .write(latencyText, { x: x + 3, y: y + 2 }, undefined, width - 6, false, FONT);

    const summaries = SAMPLE_RATES.map((rate) => {
      const stat = rateStats.get(rate);
      if (!stat?.count) return `${rate / 1000}k --`;
      return `${rate / 1000}k n${stat.count} w${(stat.worklet / stat.count).toFixed(2)} q${stat.quantum.toFixed(2)} o${stat.output.toFixed(1)}`;
    });
    const rateText = `${changingSampleRate ? "SWITCHING " : ""}MENU: HZ  ${summaries.join("  ")}`;
    const rateWidth = rateText.length * 4 + 6;
    const rateX = Math.max(2, screen.width - rateWidth - 4);
    ink(...(lightMode ? [255, 253, 247, 225] : [0, 0, 0, 210])).box(rateX, y + lineHeight + 3, rateWidth, lineHeight + 2);
    ink(...theme.text).write(rateText, { x: rateX + 3, y: y + lineHeight + 5 }, undefined, rateWidth - 6, false, FONT);
  }
  
  if (numConnected === 0) {
    // No gamepads - show message
    const msgY = hudHeight + 50;
    ink("orange").write("No gamepads connected", { x: 10, y: msgY }, undefined, undefined, false, FONT);
    ink(...theme.quiet).write("Press any button to connect", { x: 10, y: msgY + lineHeight }, undefined, undefined, false, FONT);
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
      ink(...theme.border).box(slotX, slotY, slotWidth - 1, slotHeight - 1, "line");
      
      // Draw vertical separator line on right edge
      if (col < cols - 1) {
        ink(...theme.border).line(slotX + slotWidth - 1, slotY, slotX + slotWidth - 1, slotY + slotHeight);
      }
    }
    
    // Player label
    const labelY = slotY + 8;
    ink(playerColor).write(playerLabel, { x: slotX + 8, y: labelY }, undefined, undefined, false, FONT);
    
    // Calculate space for timeline - use consistent height regardless of controller type
    // Place metadata section at bottom with more space
    const diagramHeight = 28; // Use max height for all controllers
    const metadataHeight = 60; // More space for controller name + readout
    const timelineTop = labelY + lineHeight + 4;
    const timelineBottom = slotY + slotHeight - diagramHeight - metadataHeight - 12;
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
        if (Math.abs(value) > 0.25) {  // Higher deadzone to filter drift
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
      
      // Initialize or recreate timeline painting buffer if size changed
      const needsRecreate = !timelinePaintings[gpIndex] || 
                            timelinePaintings[gpIndex].width !== slotWidth ||
                            timelinePaintings[gpIndex].height !== timelineHeight;
      
      if (needsRecreate) {
        timelinePaintings[gpIndex] = painting(slotWidth, timelineHeight);
        // Fill with dimgray initially
        for (let i = 0; i < timelinePaintings[gpIndex].pixels.length; i += 4) {
          timelinePaintings[gpIndex].pixels[i + 0] = theme.timeline[0];
          timelinePaintings[gpIndex].pixels[i + 1] = theme.timeline[1];
          timelinePaintings[gpIndex].pixels[i + 2] = theme.timeline[2];
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
    
    // Show controller name and diagram at bottom
    if (gp.id) {
      const nameY = slotY + slotHeight - diagramHeight - metadataHeight - 8;
      const maxWidth = slotWidth - 8;

      // Detect controller type and show friendly name
      let displayName = gp.id;
      if (/m30|^xbox one game controller$/i.test(displayName)) {
        displayName = "8BitDo M30 — 6 Button";
      } else if (/xbox|xinput|045e/i.test(displayName)) {
        displayName = "Xbox Controller";
      } else if (displayName.includes("2dc8") && displayName.includes("9020")) {
        displayName = "8BitDo Micro";
      } else {
        // Clean up generic names - remove vendor/product IDs
        displayName = displayName.replace(/\s*\(STANDARD GAMEPAD[^)]*\)/i, "");
        displayName = displayName.replace(/\s*\(Vendor:[^)]*\)/i, "");
      }
      
      // Controller name - use MatrixChunky8 font
      ink(...theme.text).write(displayName, { x: slotX + 4, y: nameY }, undefined, maxWidth, true, FONT);
      
      // Draw controller diagram right below name
      const diagramWidth = 48;
      const diagramX = centerX - diagramWidth / 2;
      const diagramY = nameY + lineHeight + 2;
      
      drawGamepadDiagram({ ink, line, circle, box }, gp, diagramX, diagramY);
      
      let currentY = diagramY + diagramHeight + 4;
      
      // Show pressed buttons readout - each button in its own box with proper sizing
      if (gp.pressedButtons.length > 0) {
        const buttonNames = gp.pressedButtons.map(b => getButtonName(gp.id, b));
        let buttonX = slotX + 4;
        
        buttonNames.forEach((name, i) => {
          // Measure text width accurately using text.box API
          const measurement = text.box(name, { x: 0, y: 0 }, undefined, 1, false, FONT);
          const textWidth = measurement?.box?.width || (name.length * 4);
          const boxPadding = 2;
          const boxWidth = textWidth + boxPadding * 2;
          const boxHeight = lineHeight;
          
          // Check if this box would overflow the line
          if (buttonX + boxWidth > slotX + slotWidth - 4) {
            buttonX = slotX + 4; // Reset to left margin
            currentY += lineHeight + 4; // Move to next line
          }
          
          // Get button color from mapping
          const buttonIndex = gp.pressedButtons[i];
          const colors = getButtonColors(gp.id, buttonIndex);
          const buttonColor = colors.active;
          
          // Draw box with button-specific color
          ink(buttonColor, 180).box(buttonX, currentY, boxWidth, boxHeight, "fill");
          ink(buttonColor).box(buttonX, currentY, boxWidth, boxHeight, "line");
          
          // Draw text
          ink("white").write(name, { x: buttonX + boxPadding, y: currentY + 1 }, undefined, undefined, false, FONT);
          
          buttonX += boxWidth + 3; // Move right for next box
        });
        
        currentY += lineHeight + 4;
      }
      
      // Show active axes readout - format compactly
      const activeAxes = keys(gp.axes).filter(a => Math.abs(parseFloat(gp.axes[a])) > 0.1);
      if (activeAxes.length > 0) {
        // For 8BitDo Micro, ignore axes beyond 1 (only has D-pad on axes 0-1)
        const is8BitDo = gp.id && gp.id.includes("2dc8") && gp.id.includes("9020");
        const validAxes = is8BitDo ? activeAxes.filter(a => parseInt(a) <= 1) : activeAxes;
        
        if (validAxes.length > 0) {
          // Group left stick and right stick separately for clarity
          const leftStick = validAxes.filter(a => parseInt(a) <= 1);
          const rightStick = validAxes.filter(a => parseInt(a) >= 2 && parseInt(a) <= 3);
          const otherAxes = validAxes.filter(a => parseInt(a) > 3);
          
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
    }
  });
}

function act({ event: e, sound }) {
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
      const voiceKey = `${gpIndex}:${buttonIndex}`;
      if (e.action === "push") {
        if (buttonIndex === 9 && !changingSampleRate) {
          for (const voice of buttonVoices.values()) voice?.kill?.(0.005);
          buttonVoices.clear();
          sampleRateIndex = (sampleRateIndex + 1) % SAMPLE_RATES.length;
          requestedSampleRate = SAMPLE_RATES[sampleRateIndex];
          changingSampleRate = true;
          console.log(`🧪 AUDIO_RATE_TEST requesting=${requestedSampleRate}Hz`);
          sendToBios?.({
            type: "audio:reinit",
            content: { latencyHint: 0.003, sampleRate: requestedSampleRate },
          });
          return;
        }
        const synthStartedAt = performance.now();
        if (!gp.pressedButtons.includes(buttonIndex)) {
          gp.pressedButtons.push(buttonIndex);
        }
        buttonVoices.get(voiceKey)?.kill?.(0.005);
        const voice = sound?.synth?.({
          type: "triangle",
          tone: BUTTON_TONES[buttonIndex] || 220 + buttonIndex * 30,
          attack: 0.001,
          duration: "🔁",
          decay: 0.85,
          volume: 0.22,
          immediate: true,
          probe: {
            id: `gamepad-${++probeSequence}`,
            button: buttonIndex,
            pieceAt: performance.now(),
            pieceEpoch: Date.now(),
          },
        });
        if (voice) buttonVoices.set(voiceKey, voice);
        const synthMs = performance.now() - synthStartedAt;
        const deliveryMs = e.createdAt ? Date.now() - e.createdAt : null;
        if (deliveryMs !== null) {
          lastLatency = { delivery: deliveryMs, synth: synthMs };
          latencySamples.push(deliveryMs);
          if (latencySamples.length > MAX_LATENCY_SAMPLES) latencySamples.shift();
        }
        console.log(
          `🎮 AUDIO_LATENCY button=${buttonIndex}` +
          ` delivery=${deliveryMs ?? "?"}ms synth=${synthMs.toFixed(2)}ms`,
        );
        console.log(`🔴 Button ${buttonIndex} PRESSED -`, getButtonName(gp.id, buttonIndex));
      } else if (e.action === "release") {
        gp.pressedButtons = gp.pressedButtons.filter(b => b !== buttonIndex);
        buttonVoices.get(voiceKey)?.kill?.(0.025);
        buttonVoices.delete(voiceKey);
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

function leave() {
  for (const voice of buttonVoices.values()) voice?.kill?.(0.01);
  buttonVoices.clear();
}

function boot({ send }) {
  sendToBios = send;
  // gamepad is an input-latency diagnostic: recreate the already-running
  // context with Chrome's most aggressive practical target. Xbox Edge uses
  // 48 kHz natively, avoiding an extra resampling stage on the console path.
  sendToBios?.({
    type: "audio:reinit",
    content: { latencyHint: 0.003, sampleRate: 48000 },
  });
}

function receive({ type, content }) {
  if (!content) return;
  if (type === "audio:reinit-complete") {
    changingSampleRate = false;
    requestedSampleRate = Number(content.sampleRate) || requestedSampleRate;
    const rateIndex = SAMPLE_RATES.indexOf(requestedSampleRate);
    if (rateIndex >= 0) sampleRateIndex = rateIndex;
    console.log(
      `🧪 AUDIO_RATE_READY requested=${content.requestedSampleRate}Hz` +
      ` actual=${content.sampleRate}Hz quantum=${Number(content.quantum).toFixed(2)}ms` +
      ` output=${Number(content.output).toFixed(2)}ms`,
    );
    return;
  }
  if (type === "audio:probe") {
    audioProbe = content;
    const rate = Number(content.context?.sampleRate || content.sampleRate) || requestedSampleRate;
    changingSampleRate = false;
    requestedSampleRate = rate;
    const rateIndex = SAMPLE_RATES.indexOf(rate);
    if (rateIndex >= 0) sampleRateIndex = rateIndex;
    if (content.context) {
      audioLatency = {
        base: Number(content.context.base) || 0,
        output: Number(content.context.output) || 0,
        quantum: Number(content.context.quantum) || 0,
        total: (Number(content.context.base) || 0) + (Number(content.context.output) || 0),
        sampleRate: rate,
      };
    }
    const stat = rateStats.get(rate) || { count: 0, worklet: 0, pieceToBios: 0, quantum: 128 / rate * 1000, output: 0 };
    stat.count++;
    stat.worklet += Number(content.workletRoundTrip) || 0;
    stat.pieceToBios += Number(content.pieceToBios) || 0;
    stat.quantum = 128 / rate * 1000;
    stat.output = audioLatency?.output || 0;
    rateStats.set(rate, stat);
    console.log(
      `🔬 AUDIO_PIPELINE id=${content.id} button=${content.button}` +
      ` pieceToBios=${content.pieceToBios.toFixed(2)}ms` +
      ` workletRT=${content.workletRoundTrip.toFixed(2)}ms` +
      ` attack=${content.attackMs.toFixed(2)}ms/${content.attackFrames}frames` +
      ` payload=${content.payloadBytes}B queue=${content.queueLength}` +
      ` voices=${content.runningCount} frame=${content.workletFrame}`,
    );
    return;
  }
  if (type !== "audio:latency-info") return;
  audioLatency = {
    base: Number(content.base) || 0,
    output: Number(content.output) || 0,
    quantum: Number(content.quantum) || 0,
    total: Number(content.total) || 0,
    sampleRate: Number(content.sampleRate) || 0,
  };
  changingSampleRate = false;
  requestedSampleRate = Number(content.requestedSampleRate) || audioLatency.sampleRate;
  const actualIndex = SAMPLE_RATES.indexOf(audioLatency.sampleRate);
  if (actualIndex >= 0) sampleRateIndex = actualIndex;
  console.log(
    `🎧 AUDIO_CONTEXT requested=${requestedSampleRate}Hz actual=${audioLatency.sampleRate}Hz` +
    ` base=${audioLatency.base.toFixed(2)}ms` +
    ` output=${audioLatency.output.toFixed(2)}ms` +
    ` quantum=${audioLatency.quantum.toFixed(2)}ms` +
    ` rate=${audioLatency.sampleRate}Hz`,
  );
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

export { boot, paint, act, receive, leave };
