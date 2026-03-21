// 1v1, 2024.12.24.10.00 - Fixed self.id.slice error
// Multiplayer Quake-like FPS game.

/* #region 📚 README 
  A real-time multiplayer first-person shooter.
  Two players battle in a 3D arena with simple hitscan weapons.
#endregion */

/* #region 🏁 TODO 
  - [] Add UDP support for lower latency
  - [] Add sound effects (gunshots, hits)
  - [] Add death/respawn system
  - [] Add kill counter and match timer
  - [] Better player models (not just boxes)
  + Done
  - [x] Clone fps.mjs as foundation
  - [x] Add socket networking
  - [x] Basic player state management
  - [x] Add gamepad debug HUD panel
#endregion */

// 🎮 Gamepad mapping utilities (shared with gamepad.mjs)
import { 
  getButtonName, 
  getAxisName, 
  getButtonColors, 
  getAxisColors,
  getGamepadMapping
} from "../lib/gamepad-mappings.mjs";

// 📱 QR code for join URL
import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

// 🎮 Mini gamepad diagram for HUD (simplified from gamepad.mjs)
function drawMiniControllerDiagram(ink, gp, x, y) {
  const baseX = x;
  const baseY = y;
  const gamepadId = gp.id || "standard";
  const is8BitDo = gamepadId.includes("8BitDo") || gamepadId.includes("2dc8");
  
  // Get button colors from mapping
  const getColors = (btnIndex) => {
    const colors = getButtonColors(gamepadId, btnIndex);
    const isPressed = gp.pressedButtons.includes(btnIndex);
    return isPressed ? colors.active : colors.inactive;
  };
  
  const width = is8BitDo ? 40 : 48;
  const height = is8BitDo ? 20 : 24;
  
  // Check if any input is active
  const hasActiveInput = gp.pressedButtons.length > 0 || 
    Object.values(gp.axes).some(v => Math.abs(parseFloat(v)) > 0.3);
  
  // Background
  ink(hasActiveInput ? "slategray" : "dimgray").box(baseX, baseY, width, height);
  
  if (is8BitDo) {
    // 8BitDo Micro layout - D-pad left, face buttons right
    // D-Pad (using axes)
    const dpadX = baseX + 8;
    const dpadY = baseY + 10;
    const dpadSize = 4;
    
    const axisX = parseFloat(gp.axes["0"] || 0);
    const axisY = parseFloat(gp.axes["1"] || 0);
    
    ink(axisY < -0.3 ? "lime" : "gray").box(dpadX, dpadY - 5, dpadSize, dpadSize);  // Up
    ink(axisY > 0.3 ? "lime" : "gray").box(dpadX, dpadY + 5, dpadSize, dpadSize);   // Down
    ink(axisX < -0.3 ? "lime" : "gray").box(dpadX - 5, dpadY, dpadSize, dpadSize);  // Left
    ink(axisX > 0.3 ? "lime" : "gray").box(dpadX + 5, dpadY, dpadSize, dpadSize);   // Right
    ink("black").box(dpadX, dpadY, dpadSize, dpadSize);  // Center
    
    // Face buttons (ABXY diamond) - right side
    const faceX = baseX + 30;
    const faceY = baseY + 10;
    const btnSize = 4;
    
    ink(getColors(3)).box(faceX, faceY - 5, btnSize, btnSize);    // X (top)
    ink(getColors(1)).box(faceX, faceY + 5, btnSize, btnSize);    // B (bottom)
    ink(getColors(4)).box(faceX - 5, faceY, btnSize, btnSize);    // Y (left)
    ink(getColors(0)).box(faceX + 5, faceY, btnSize, btnSize);    // A (right)
    
    // Shoulder buttons
    ink(getColors(6)).box(baseX, baseY, 12, 2);      // L
    ink(getColors(7)).box(baseX + 28, baseY, 12, 2); // R
  } else {
    // Standard Xbox-style layout
    // Left stick position
    const lStickX = baseX + 10;
    const lStickY = baseY + 8;
    const axisX = parseFloat(gp.axes["0"] || 0);
    const axisY = parseFloat(gp.axes["1"] || 0);
    const lStickActive = Math.abs(axisX) > 0.1 || Math.abs(axisY) > 0.1;
    
    ink(lStickActive ? "cyan" : "gray").box(lStickX - 3, lStickY - 3, 6, 6, "line");
    const dotX = lStickX + Math.round(axisX * 2);
    const dotY = lStickY + Math.round(axisY * 2);
    ink("cyan").box(dotX, dotY, 1, 1);
    
    // D-pad (buttons 12-15)
    const dpadX = baseX + 10;
    const dpadY = baseY + 18;
    const dpadSize = 3;
    
    ink(getColors(12)).box(dpadX, dpadY - 4, dpadSize, dpadSize);  // Up
    ink(getColors(13)).box(dpadX, dpadY + 4, dpadSize, dpadSize);  // Down
    ink(getColors(14)).box(dpadX - 4, dpadY, dpadSize, dpadSize);  // Left
    ink(getColors(15)).box(dpadX + 4, dpadY, dpadSize, dpadSize);  // Right
    
    // Face buttons (right side)
    const faceX = baseX + 38;
    const faceY = baseY + 10;
    const btnSize = 4;
    
    ink(getColors(3)).box(faceX, faceY - 5, btnSize, btnSize);    // Y (top)
    ink(getColors(0)).box(faceX, faceY + 5, btnSize, btnSize);    // A (bottom)
    ink(getColors(2)).box(faceX - 5, faceY, btnSize, btnSize);    // X (left)
    ink(getColors(1)).box(faceX + 5, faceY, btnSize, btnSize);    // B (right)
    
    // Right stick
    const rStickX = baseX + 28;
    const rStickY = baseY + 18;
    const axis2 = parseFloat(gp.axes["2"] || 0);
    const axis3 = parseFloat(gp.axes["3"] || 0);
    const rStickActive = Math.abs(axis2) > 0.1 || Math.abs(axis3) > 0.1;
    
    ink(rStickActive ? "yellow" : "gray").box(rStickX - 3, rStickY - 3, 6, 6, "line");
    const dot2X = rStickX + Math.round(axis2 * 2);
    const dot2Y = rStickY + Math.round(axis3 * 2);
    ink("yellow").box(dot2X, dot2Y, 1, 1);
    
    // Bumpers/Triggers
    ink(getColors(4)).box(baseX + 2, baseY, 10, 2);       // LB
    ink(getColors(6)).box(baseX + 2, baseY + 2, 8, 2);    // LT
    ink(getColors(5)).box(baseX + 36, baseY, 10, 2);      // RB
    ink(getColors(7)).box(baseX + 38, baseY + 2, 8, 2);   // RT
  }
}

// Draw mini keyboard controls display (WASD + Arrows + Mouse) - compact version
function drawKeyboardControls(ink, baseX, baseY, pressedKeys = {}) {
  const keySize = 8;
  const keySpacing = 1;
  const hudFont = "MatrixChunky8";
  
  // Helper to draw a key
  const drawKey = (x, y, label, isPressed) => {
    const bgColor = isPressed ? [255, 255, 0, 220] : [40, 40, 40, 180];
    const textColor = isPressed ? [0, 0, 0] : [200, 200, 200];
    ink(...bgColor).box(x, y, keySize, keySize);
    ink(100, 100, 100).box(x, y, keySize, keySize, "outline");
    ink(...textColor).write(label, { x: x + 2, y: y + 1 }, undefined, undefined, false, hudFont);
  };
  
  // Row 1: W and UP arrow (with labels inline)
  let row1Y = baseY;
  ink(100, 100, 100).write("W", { x: baseX, y: row1Y + 1 }, undefined, undefined, false, hudFont);
  drawKey(baseX + 8, row1Y, "^", pressedKeys.w);
  
  ink(100, 100, 100).write("^", { x: baseX + 35, y: row1Y + 1 }, undefined, undefined, false, hudFont);
  drawKey(baseX + 43, row1Y, "^", pressedKeys.arrowup);
  
  // Row 2: ASD and arrow keys
  let row2Y = baseY + keySize + keySpacing;
  drawKey(baseX, row2Y, "A", pressedKeys.a);
  drawKey(baseX + keySize + keySpacing, row2Y, "S", pressedKeys.s);
  drawKey(baseX + (keySize + keySpacing) * 2, row2Y, "D", pressedKeys.d);
  
  drawKey(baseX + 35, row2Y, "<", pressedKeys.arrowleft);
  drawKey(baseX + 35 + keySize + keySpacing, row2Y, "v", pressedKeys.arrowdown);
  drawKey(baseX + 35 + (keySize + keySpacing) * 2, row2Y, ">", pressedKeys.arrowright);
  
  // Row 3: Space and Shift
  let row3Y = baseY + (keySize + keySpacing) * 2 + 2;
  const spaceBg = pressedKeys.space ? [255, 255, 0, 220] : [40, 40, 40, 180];
  const spaceText = pressedKeys.space ? [0, 0, 0] : [200, 200, 200];
  ink(...spaceBg).box(baseX, row3Y, 28, keySize);
  ink(100, 100, 100).box(baseX, row3Y, 28, keySize, "outline");
  ink(...spaceText).write("SPC", { x: baseX + 6, y: row3Y + 1 }, undefined, undefined, false, hudFont);
  
  const shiftBg = pressedKeys.shift ? [255, 255, 0, 220] : [40, 40, 40, 180];
  const shiftText = pressedKeys.shift ? [0, 0, 0] : [200, 200, 200];
  ink(...shiftBg).box(baseX + 32, row3Y, 20, keySize);
  ink(100, 100, 100).box(baseX + 32, row3Y, 20, keySize, "outline");
  ink(...shiftText).write("SH", { x: baseX + 36, y: row3Y + 1 }, undefined, undefined, false, hudFont);
  
  // Mouse icon (compact)
  const mouseX = baseX + 58;
  const mouseY = baseY;
  ink(60, 60, 60).box(mouseX, mouseY, 10, 16);
  ink(80, 80, 80).box(mouseX, mouseY, 10, 16, "outline");
  ink(pressedKeys.lmb ? "yellow" : "gray").box(mouseX + 1, mouseY + 1, 4, 5);
  ink(pressedKeys.rmb ? "yellow" : "gray").box(mouseX + 5, mouseY + 1, 4, 5);
}

// 📱 Draw join QR code and IP in bottom-right corner
function drawJoinQR({ ink, box, write, screen }) {
  if (!joinQRCells || !showJoinQR) return;
  
  const hudFont = "MatrixChunky8";
  
  // Calculate QR size - compact but still scannable
  const scale = 1;  // Small scale for compact display
  const qrSize = joinQRCells.length * scale;
  const margin = 8;
  const textHeight = 20; // Space for URL and host label
  
  // Position in bottom-right corner
  const ox = screen.width - qrSize - margin;
  const oy = screen.height - qrSize - margin - textHeight;
  
  // Semi-transparent background
  ink(0, 0, 0, 180).box(ox - 4, oy - 4, qrSize + 8, qrSize + textHeight + 8);
  
  // Draw QR code cells
  for (let y = 0; y < joinQRCells.length; y++) {
    for (let x = 0; x < joinQRCells.length; x++) {
      const isBlack = joinQRCells[y][x];
      ink(isBlack ? 0 : 255).box(ox + x * scale, oy + y * scale, scale);
    }
  }
  
  // White border around QR
  ink(255, 255, 255).box(ox - 1, oy - 1, qrSize + 2, qrSize + 2, "outline");
  
  // Display URL below QR (strip https:// for brevity)
  // Right-align text to prevent cutoff (approx 6px per char)
  const displayIP = joinURL?.replace('https://', '').replace('/1v1', '') || '';
  const ipTextWidth = displayIP.length * 6;
  const ipX = screen.width - margin - Math.max(ipTextWidth, qrSize);
  ink(255, 255, 0).write(displayIP, { x: ipX, y: oy + qrSize + 4 }, undefined, undefined, false, hudFont);
  
  // Show host machine label if available (also right-aligned)
  if (hostMachineInfo?.hostLabel) {
    const labelWidth = hostMachineInfo.hostLabel.length * 6;
    const labelX = screen.width - margin - Math.max(labelWidth, qrSize);
    ink(200, 200, 200).write(hostMachineInfo.hostLabel, 
      { x: labelX, y: oy + qrSize + 12 }, undefined, undefined, false, hudFont);
  } else if (hostMachineInfo?.hostName) {
    const nameWidth = hostMachineInfo.hostName.length * 6;
    const nameX = screen.width - margin - Math.max(nameWidth, qrSize);
    ink(180, 180, 180).write(hostMachineInfo.hostName, 
      { x: nameX, y: oy + qrSize + 12 }, undefined, undefined, false, hudFont);
  }
}

// Track keyboard state for display
let pressedKeys = {};

// Helper function to create face geometry (eyes + mouth) for character visualization
// Creates primitive lines that form a simple face on the front of the camera box
function createFaceGeometry(boxSize, r, g, b) {
  // Face is on the FRONT of the box (negative Z)
  const frontZ = -boxSize - 0.01;  // Slightly in front of the box
  
  // Eye positions (two circles/diamonds made of lines)
  const eyeY = boxSize * 0.3;     // Eyes above center
  const eyeSpacing = boxSize * 0.5;  // Distance between eyes
  const eyeSize = boxSize * 0.2;   // Size of each eye
  
  // Mouth position (curved line or simple smile)
  const mouthY = -boxSize * 0.3;   // Below center
  const mouthWidth = boxSize * 0.6;
  const mouthCurve = boxSize * 0.15;
  
  const positions = [
    // Left eye (diamond shape)
    [-eyeSpacing, eyeY + eyeSize, frontZ, 1], [-eyeSpacing - eyeSize, eyeY, frontZ, 1],
    [-eyeSpacing - eyeSize, eyeY, frontZ, 1], [-eyeSpacing, eyeY - eyeSize, frontZ, 1],
    [-eyeSpacing, eyeY - eyeSize, frontZ, 1], [-eyeSpacing + eyeSize, eyeY, frontZ, 1],
    [-eyeSpacing + eyeSize, eyeY, frontZ, 1], [-eyeSpacing, eyeY + eyeSize, frontZ, 1],
    
    // Right eye (diamond shape)
    [eyeSpacing, eyeY + eyeSize, frontZ, 1], [eyeSpacing - eyeSize, eyeY, frontZ, 1],
    [eyeSpacing - eyeSize, eyeY, frontZ, 1], [eyeSpacing, eyeY - eyeSize, frontZ, 1],
    [eyeSpacing, eyeY - eyeSize, frontZ, 1], [eyeSpacing + eyeSize, eyeY, frontZ, 1],
    [eyeSpacing + eyeSize, eyeY, frontZ, 1], [eyeSpacing, eyeY + eyeSize, frontZ, 1],
    
    // Pupils (small dots represented as tiny X)
    [-eyeSpacing - eyeSize*0.2, eyeY - eyeSize*0.2, frontZ - 0.01, 1], [-eyeSpacing + eyeSize*0.2, eyeY + eyeSize*0.2, frontZ - 0.01, 1],
    [-eyeSpacing + eyeSize*0.2, eyeY - eyeSize*0.2, frontZ - 0.01, 1], [-eyeSpacing - eyeSize*0.2, eyeY + eyeSize*0.2, frontZ - 0.01, 1],
    [eyeSpacing - eyeSize*0.2, eyeY - eyeSize*0.2, frontZ - 0.01, 1], [eyeSpacing + eyeSize*0.2, eyeY + eyeSize*0.2, frontZ - 0.01, 1],
    [eyeSpacing + eyeSize*0.2, eyeY - eyeSize*0.2, frontZ - 0.01, 1], [eyeSpacing - eyeSize*0.2, eyeY + eyeSize*0.2, frontZ - 0.01, 1],
    
    // Mouth (smile made of 3 line segments for curve)
    [-mouthWidth, mouthY, frontZ, 1], [-mouthWidth * 0.3, mouthY - mouthCurve, frontZ, 1],
    [-mouthWidth * 0.3, mouthY - mouthCurve, frontZ, 1], [mouthWidth * 0.3, mouthY - mouthCurve, frontZ, 1],
    [mouthWidth * 0.3, mouthY - mouthCurve, frontZ, 1], [mouthWidth, mouthY, frontZ, 1],
  ];
  
  // Colors: eyes in player color, pupils in white, mouth in player color
  const eyeColor = [r, g, b, 1];
  const pupilColor = [1, 1, 1, 1];  // White pupils
  const mouthColor = [r, g, b, 1];
  
  const colors = [
    // Left eye (8 vertices = 4 lines)
    eyeColor, eyeColor, eyeColor, eyeColor, eyeColor, eyeColor, eyeColor, eyeColor,
    // Right eye (8 vertices = 4 lines)
    eyeColor, eyeColor, eyeColor, eyeColor, eyeColor, eyeColor, eyeColor, eyeColor,
    // Pupils (8 vertices = 4 lines for 2 X's)
    pupilColor, pupilColor, pupilColor, pupilColor, pupilColor, pupilColor, pupilColor, pupilColor,
    // Mouth (6 vertices = 3 lines)
    mouthColor, mouthColor, mouthColor, mouthColor, mouthColor, mouthColor,
  ];
  
  return { positions, colors };
}

// Networking
let server;
let self = {
  id: null,
  handle: "player",
  pos: { x: 0, y: 1.6, z: 0 },
  rot: { x: 0, y: 0, z: 0 },
  vel: { x: 0, y: 0, z: 0 },
  health: 100,
  kills: 0,
  deaths: 0,
  lastShot: 0,
};
let others = {}; // Map of other players by ID
let gameState = "connecting"; // connecting, lobby, playing, dead, gameover
let frameCount = 0; // Global frame counter for throttling

// 🤖 Bot system - runs around inside the main square for players to follow
let bot = {
  enabled: true,
  pos: { x: 0, y: -0.5, z: 0 },  // Start at center, match player eye height (Y=-0.5)
  rot: { x: 0, y: 0, z: 0 },
  vel: { x: 0, y: 0, z: 0 },
  health: 100,
  state: "patrol", // patrol, chase, flee, dead
  targetPos: null,
  // Patrol points inside the main square (ground is -3 to +3 on X/Z)
  patrolPoints: [
    { x: 2, z: 2 },    // Front-right
    { x: -2, z: 2 },   // Front-left  
    { x: -2, z: -2 },  // Back-left
    { x: 2, z: -2 },   // Back-right
    { x: 0, z: 0 },    // Center (adds variety)
  ],
  patrolIndex: 0,
  speed: 0.015,     // Slow, chill movement
  turnSpeed: 1.5,   // Slow, smooth turning
  pauseTimer: 0,    // Pause at waypoints
  lastStateChange: 0,
  respawnTimer: 0,
};
let botModel = null; // Visual representation

// 🎭 Spectator mode - activated when same handle joins from another tab/device
let isSpectator = false;
let spectatorReason = null; // Why we're in spectator mode

// 🩰 Network connectivity tracking
let udpConnected = false;
let wsConnected = false;
let lastUdpReceiveTime = 0;
let lastWsReceiveTime = 0;
let udpMessageCount = 0;
let wsMessageCount = 0;
let networkDebugLog = []; // Rolling log of recent network events
const MAX_DEBUG_LOG = 20;

// 🎮 Gamepad state tracking for HUD display
let showGamepadPanel = true; // Toggle with 'G' key (default ON)
const connectedGamepads = {};

// 📱 Join QR code state
let joinQRCells = null;  // QR code cell grid
let joinURL = null;      // URL displayed below QR
let showJoinQR = true;   // Toggle with 'Q' key (default ON)
let hostMachineInfo = null; // Host machine identity (MacBook, Thinkpad, etc)

// 🎵 Background Music
let bgmSfx = null;        // The preloaded audio handle
let bgmPlaying = null;    // The playing audio instance
let bgmLoaded = false;    // Track if audio is loaded

// Action mappings - what each control does in FPS mode
const FPS_ACTIONS = {
  buttons: {
    // 8BitDo Micro mappings (when no right stick)
    "8bitdo_0": "Look Right",   // A - right face button
    "8bitdo_1": "Look Down",    // B - bottom face button
    "8bitdo_3": "Look Up",      // X - top face button
    "8bitdo_4": "Look Left",    // Y - left face button
    "8bitdo_6": "L Shoulder",   // L
    "8bitdo_7": "R Shoulder",   // R
    "8bitdo_10": "Select",      // Select
    "8bitdo_11": "Start",       // Start
    // Standard controller mappings
    "standard_0": "A/Jump",     
    "standard_1": "B/Back",     
    "standard_2": "X/Action",   
    "standard_3": "Y/Reload",   
    "standard_4": "LB",         
    "standard_5": "RB",         
    "standard_6": "LT",         
    "standard_7": "RT/Shoot",   
    "standard_10": "LS Click",  
    "standard_11": "RS Click",  
    "standard_12": "D-Up",      
    "standard_13": "D-Down",    
    "standard_14": "D-Left",    
    "standard_15": "D-Right",   
  },
  axes: {
    // For 8BitDo Micro (D-pad on axes)
    "8bitdo_0": { neg: "Strafe Left", pos: "Strafe Right" },
    "8bitdo_1": { neg: "Move Forward", pos: "Move Back" },
    // Standard controller (left stick move, right stick look)
    "standard_0": { neg: "Strafe Left", pos: "Strafe Right" },
    "standard_1": { neg: "Move Forward", pos: "Move Back" },
    "standard_2": { neg: "Look Left", pos: "Look Right" },
    "standard_3": { neg: "Look Up", pos: "Look Down" },
  }
};

function logNetwork(msg, level = "info") {
  const entry = { time: Date.now(), msg, level };
  networkDebugLog.push(entry);
  if (networkDebugLog.length > MAX_DEBUG_LOG) networkDebugLog.shift();
  const prefix = level === "error" ? "❌" : level === "warn" ? "⚠️" : "📡";
  console.log(`${prefix} [1v1 NET] ${msg}`);
}

// 3D Scene
let cube, triangle, filledTriangle, texturedQuad, quadTexture, groundPlane, groundTexture, groundWireframe, penLocked = false;
// Camera frustums are now created dynamically per player in playerBoxes
let showWireframes = true; // Toggle with 'V' key (start with wireframes ON)
let graphAPI; // Store graph API reference
let graphInstance; // Store graph instance for camera access
let systemInstance; // Store system reference for render stats access
let showDebugPanel = false; // Toggle with 'P' key (start OFF)
let frameTimes = []; // Track frame times for FPS calculation
let lastFrameTime = performance.now();
let paintingTextureFetchPromise; // Track ongoing painting texture fetch
let paintingTextureLoaded = false; // Track if we've loaded the painting

// 3D Text (sign) - stored from boot for use in networking callbacks
let globalSign;
let globalGlyphs;

// 🩰 UDP channel for low-latency position updates
let udpChannel;

// Combat
const SHOOT_COOLDOWN = 200; // ms between shots
const DAMAGE_PER_HIT = 20;
let playerBoxes = {}; // Visual representation of other players

// 🎯 Target tracking system - for looking at other players/bot
let currentTarget = null;  // Current target to look at (player id, "bot", or null)
let targetList = [];       // List of available targets
let isAutoTracking = false; // Whether to continuously face the target
const TURN_SPEED = 3;      // Degrees per frame when auto-tracking

// Calculate yaw angle to face a target position from our position
function getAngleToTarget(fromPos, toPos) {
  const dx = toPos.x - fromPos.x;
  const dz = toPos.z - fromPos.z;
  // atan2 gives angle in radians, convert to degrees
  // NEGATE because camera.rotY is internally negated in the transform matrix
  // This makes rotY=0 look toward +Z, rotY=90 look toward -X
  return -Math.atan2(dx, dz) * (180 / Math.PI);
}

// Get the position of the current target
function getTargetPosition() {
  if (!currentTarget) return null;
  
  if (currentTarget === "bot" && bot.enabled) {
    return bot.pos;
  }
  
  // Find player by id
  const player = others[currentTarget];
  if (player) {
    return player.pos;
  }
  
  return null;
}

// Update the list of available targets (other players + bot)
function updateTargetList() {
  targetList = [];
  
  // Add bot if enabled
  if (bot.enabled && bot.state !== "dead") {
    targetList.push("bot");
  }
  
  // Add all other players
  for (const id of Object.keys(others)) {
    targetList.push(id);
  }
  
  // If current target is no longer valid, clear it
  if (currentTarget && !targetList.includes(currentTarget)) {
    currentTarget = null;
    isAutoTracking = false;
  }
}

// Cycle to next target
function cycleTarget() {
  updateTargetList();
  
  if (targetList.length === 0) {
    currentTarget = null;
    logNetwork("No targets available");
    return;
  }
  
  const currentIndex = currentTarget ? targetList.indexOf(currentTarget) : -1;
  const nextIndex = (currentIndex + 1) % targetList.length;
  currentTarget = targetList[nextIndex];
  
  const targetName = currentTarget === "bot" ? "🤖 BOT" : (others[currentTarget]?.handle || currentTarget);
  logNetwork(`🎯 Target: ${targetName}`);
}

// Snap camera to face current target instantly
function snapToTarget() {
  if (!currentTarget || !graphInstance) return;
  
  const targetPos = getTargetPosition();
  if (!targetPos) return;
  
  const angle = getAngleToTarget(self.pos, targetPos);
  graphInstance.rotY = angle;
  
  const targetName = currentTarget === "bot" ? "🤖 BOT" : (others[currentTarget]?.handle || currentTarget);
  logNetwork(`👁️ Snapped to ${targetName}`);
}

// Toggle auto-tracking mode
function toggleAutoTrack() {
  if (!currentTarget) {
    // If no target, select one first
    cycleTarget();
  }
  isAutoTracking = !isAutoTracking;
  logNetwork(`🎯 Auto-track: ${isAutoTracking ? "ON" : "OFF"}`);
}

// Function to log detailed scene debug info
function logSceneDebug() {
  if (!graphInstance) {
    console.error("❌ Camera not available");
    return;
  }
  
  // Access render stats directly from system.fps.renderStats
  const stats = systemInstance?.fps?.renderStats;
  const hasStats = stats && typeof stats.originalTriangles === 'number';
  
  // Build warning messages
  let warnings = [];
  if (hasStats) {
    if (stats.pixelsDrawn > 50000) {
      warnings.push("High pixel count - large triangle(s) filling screen");
    }
    if (stats.subdividedTriangles > 10) {
      warnings.push("High subdivision - triangle edge at screen boundary");
    }
  }
  
  console.log(
    "%c FPS %c·%c Scene Debug %c\n\n" +
    
    "%c Camera \n" +
    `%c   position %c(${graphInstance.x.toFixed(2)}, ${graphInstance.y.toFixed(2)}, ${graphInstance.z.toFixed(2)})%c\n` +
    `%c   looking  %cpitch ${graphInstance.rotX.toFixed(1)}° · yaw ${graphInstance.rotY.toFixed(1)}° · roll ${graphInstance.rotZ.toFixed(1)}°%c\n\n` +
    
    "%c Objects \n" +
    `%c   cube      %cat (${cube.position[0]}, ${cube.position[1]}, ${cube.position[2]}) · spinning · wireframe%c\n` +
    `%c   triangle  %cat (${triangle.position[0]}, ${triangle.position[1]}, ${triangle.position[2]}) · gradient · wireframe%c\n` +
    `%c   triangle  %cat (${filledTriangle.position[0]}, ${filledTriangle.position[1]}, ${filledTriangle.position[2]}) · filled · gradient%c\n` +
    `%c   quad      %cat (${texturedQuad.position[0]}, ${texturedQuad.position[1]}, ${texturedQuad.position[2]}) · textured · subdivided%c\n` +
    `%c   ground    %cat (${groundPlane.position[0]}, ${groundPlane.position[1]}, ${groundPlane.position[2]}) · 6×6 units · textured%c\n\n` +
    
    (hasStats 
      ? "%c Performance \n" +
        `%c   triangles %c${stats.originalTriangles} original · ${stats.clippedTriangles} clipped · ${stats.subdividedTriangles} subdivided · ${stats.trianglesRejected || 0} rejected%c\n` +
        `%c   pixels    %c${stats.pixelsDrawn?.toLocaleString() || 0}${stats.pixelsDrawn > 50000 ? " %c⚠%c" : "%c %c"}%c drawn this frame%c\n` +
        `%c   wireframe %c${stats.wireframeSegmentsTotal} segments%c\n`
      : "%c Performance \n" +
        `%c   waiting for first frame to render%c\n`
    ) +
    
    (warnings.length > 0 
      ? "\n%c ⚠ Warning \n" +
        `%c   ${warnings.join("\n   ")}%c\n`
      : ""
    ),
    
    // Title styles - pink and cyan like Aesthetic.Computer
    "background: rgba(199, 21, 133, 0.8); color: rgb(252, 231, 243); font-weight: bold; font-size: 11px; padding: 2px 6px; border-radius: 3px 0 0 3px;",
    "color: #4ecdc4; font-weight: bold; font-size: 11px;",
    "background: rgba(78, 205, 196, 0.8); color: rgb(10, 20, 40); font-weight: bold; font-size: 11px; padding: 2px 6px; border-radius: 0 3px 3px 0;",
    "",
    
    // Camera section - cyan theme
    "color: #4ecdc4; font-weight: bold; font-size: 12px;",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    
    // Objects section - pink theme
    "color: #ff6b9d; font-weight: bold; font-size: 12px;",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #ffc107; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    
    // Performance section - green/yellow theme
    ...(hasStats 
      ? [
          "color: #6ee7b7; font-weight: bold; font-size: 12px;",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;",
          stats.pixelsDrawn > 50000 ? "color: #ffc107; font-weight: bold;" : "", "",
          "color: #6c757d; font-size: 10px;", "",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", ""
        ]
      : [
          "color: #6ee7b7; font-weight: bold; font-size: 12px;",
          "color: #6c757d; font-size: 10px; font-style: italic;", ""
        ]
    ),
    
    // Warning section - yellow/red
    ...(warnings.length > 0 
      ? [
          "color: #ffc107; font-weight: bold; font-size: 12px;",
          "color: #ffb74d; font-size: 10px;", ""
        ]
      : []
    )
  );
}


function boot({ Form, CUBEL, QUAD, penLock, system, get, net: { socket, udp, host, lan, devIdentity, preload }, handle, help, sign, glyphs, platform, sound }) {
  console.log("🎮 1v1 boot", { hasForm: !!Form, hasSocket: !!socket, hasHandle: !!handle, hasSign: !!sign, hasUdp: !!udp });
  
  // Store sign and glyphs for use in networking callbacks
  globalSign = sign;
  globalGlyphs = glyphs;
  
  penLock();
  
  // Store system and graph instance for camera and stats access
  systemInstance = system;
  graphInstance = system?.fps?.doll?.cam;
  
  // 📱 Generate join QR code
  if (lan) {
    joinURL = `${lan}/1v1`;
  } else if (host?.startsWith("localhost")) {
    joinURL = "https://local.aesthetic.computer/1v1";
  } else if (host) {
    joinURL = `https://${host}/1v1`;
  }
  
  if (joinURL) {
    try {
      // Use low error correction (L) for smaller QR code
      joinQRCells = qr(joinURL, { errorCorrectLevel: 1 }).modules; // 1 = L (7% recovery)
      console.log("📱 Join QR generated:", joinURL, `(${joinQRCells?.length}x${joinQRCells?.length} low-EC)`);
    } catch (e) {
      console.warn("📱 Failed to generate QR:", e);
    }
  }
  
  // Store host machine identity if available
  hostMachineInfo = devIdentity || null;
  if (hostMachineInfo) {
    console.log("🖥️ Host machine:", hostMachineInfo.hostLabel || hostMachineInfo.hostName);
  }
  
  // 🎵 Load background music (using simple name like booted-by.mjs)
  preload("oskie/tokyo-90bpm").then((sfx) => {
    bgmSfx = sfx;
    bgmLoaded = true;
    console.log("🎵 1v1: Background music loaded");
    // Start playing immediately if we're already connected
    if (wsConnected && !bgmPlaying && sound) {
      bgmPlaying = sound.play(bgmSfx, { loop: true, volume: 0.4 });
      console.log("🎵 1v1: Background music started (post-load)");
    }
  }).catch((err) => {
    console.warn("🎵 1v1: Failed to load background music:", err);
  });
  
  // Set player handle - use logged-in handle or generate unique guest handle
  // Add random suffix to prevent collisions in split view (same browser, two tabs)
  const randomSuffix = Math.floor(Math.random() * 1000);
  self.handle = handle() || help.choose("red", "blue", "green", "yellow", "orange", "purple", "cyan", "pink") + "_" + randomSuffix;
  
  // 🩰 Initialize UDP for low-latency position updates
  udpChannel = udp((type, content) => {
    udpMessageCount++;
    lastUdpReceiveTime = Date.now();
    // Log occasionally (~5%) to avoid spamming
    if (Math.random() < 0.05) {
      logNetwork(`UDP recv: ${type} from ${content?.handle || 'unknown'}`);
    }
    if (type === "1v1:move") {
      // Parse content if it's a string
      const data = typeof content === 'string' ? JSON.parse(content) : content;
      
      // Find player by handle (UDP doesn't use WebSocket IDs)
      if (data.handle && data.handle !== self.handle) {
        const playerId = Object.keys(others).find(id => others[id].handle === data.handle);
        if (playerId && others[playerId]) {
          // Update position and rotation - paint() will use these to move frustums
          others[playerId].pos = data.pos;
          others[playerId].rot = data.rot;
        } else if (Math.random() < 0.01) {
          // Log occasionally when we receive UDP for an unknown player
          // This happens if UDP arrives before WebSocket join
          logNetwork(`UDP move from unknown handle: ${data.handle} (known: ${Object.keys(others).map(id => others[id].handle).join(', ')})`, "warn");
        }
      }
    }
  });
  logNetwork(`UDP channel initialized, connected: ${udpChannel?.connected}`);
  udpConnected = udpChannel?.connected || false;
  
  // Initialize WebSocket networking (for reliable events: join/leave/combat)
  
  // Initialize WebSocket networking (for reliable events: join/leave/combat)
  server = socket((id, type, content) => {
    // Track WebSocket connectivity
    wsMessageCount++;
    lastWsReceiveTime = Date.now();
    
    if (type === "left") {
      logNetwork(`Player left: ${others[id]?.handle || id}`);
      delete others[id];
      delete playerBoxes[id];
    }
    
    if (type === "joined") {
      logNetwork(`Player joined notification: ${id}`);
    }
    
    if (type.startsWith("connected")) {
      self.id = id;
      wsConnected = true;
      gameState = "lobby";
      logNetwork(`WebSocket connected as: ${self.handle} (${id})`);
      
      // 🎵 Start background music when connected (if loaded)
      if (bgmSfx && !bgmPlaying && sound) {
        bgmPlaying = sound.play(bgmSfx, { loop: true, volume: 0.4 });
        console.log("🎵 1v1: Background music started");
      }
      
      // Send initial join message
      server.send("1v1:join", {
        handle: self.handle,
        pos: self.pos,
        rot: self.rot,
        health: self.health,
      });
    }
    
    // Handle messages from other players
    if (server.id !== id) {
      if (type === "1v1:join") {
        // Only respond if we don't already know about this player
        const isNewPlayer = !others[id];
        
        logNetwork(`Player join request: ${content.handle} (${id})`);
        
        // 🎭 Check if this is a duplicate handle (same user from another tab/device)
        if (content.handle === self.handle) {
          isSpectator = true;
          spectatorReason = "Same handle connected from another location";
          logNetwork(`SPECTATOR MODE: ${spectatorReason}`, "warn");
        }
        
        others[id] = {
          handle: content.handle,
          pos: content.pos || { x: 0, y: 1.6, z: 0 },
          rot: content.rot || { x: 0, y: 0, z: 0 },
          health: content.health || 100,
        };
        
        // Only send our state back to truly NEW players (not in response to their response)
        if (isNewPlayer) {
          server.send("1v1:state", {
            handle: self.handle,
            pos: self.pos,
            rot: self.rot,
            health: self.health,
          });
        }
        
        // Create CAMERA FRUSTUM visualization for this player
        // Shows where their camera is and what direction they're looking
        if (!playerBoxes[id]) {
          // Assign a color to this player based on index
          const playerColors = [
            [0, 1, 0],      // Lime
            [1, 0.5, 0],    // Orange  
            [0, 1, 1],      // Cyan
            [1, 0, 1],      // Magenta
            [1, 1, 0],      // Yellow
            [0.5, 0.5, 1],  // Light blue
          ];
          const colorIndex = Object.keys(playerBoxes).length % playerColors.length;
          const playerColor = playerColors[colorIndex];
          const [r, g, b] = playerColor;
          
          // Camera frustum visualization
          // A small box at camera position + pyramid showing view direction
          const boxSize = 0.15;  // Size of camera "head" box
          const frustumLength = 0.5;  // How far the view cone extends
          const frustumSpread = 0.3;  // Width of frustum at far end
          
          playerBoxes[id] = {
            color: playerColor,
            // Camera box (cube wireframe at camera position)
            cameraBox: new Form(
              { type: "line", positions: [
                // Front face
                [-boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, -boxSize, 1],
                [boxSize, -boxSize, -boxSize, 1], [boxSize, boxSize, -boxSize, 1],
                [boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, -boxSize, 1],
                [-boxSize, boxSize, -boxSize, 1], [-boxSize, -boxSize, -boxSize, 1],
                // Back face
                [-boxSize, -boxSize, boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                [boxSize, -boxSize, boxSize, 1], [boxSize, boxSize, boxSize, 1],
                [boxSize, boxSize, boxSize, 1], [-boxSize, boxSize, boxSize, 1],
                [-boxSize, boxSize, boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                // Connecting edges
                [-boxSize, -boxSize, -boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                [boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                [boxSize, boxSize, -boxSize, 1], [boxSize, boxSize, boxSize, 1],
                [-boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, boxSize, 1],
              ], colors: [
                // All edges in player color
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
              ]},
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // View frustum (pyramid pointing in look direction, -Z is forward)
            frustum: new Form(
              { type: "line", positions: [
                // Lines from camera to frustum corners
                [0, 0, 0, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
                [0, 0, 0, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                [0, 0, 0, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                [0, 0, 0, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                // Far plane rectangle
                [-frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                [frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                [frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                [-frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
              ], colors: [
                // Frustum edges - use red for visibility
                [1, 0, 0, 1], [1, 0, 0, 1],
                [1, 0, 0, 1], [1, 0, 0, 1],
                [1, 0, 0, 1], [1, 0, 0, 1],
                [1, 0, 0, 1], [1, 0, 0, 1],
                // Far plane in player color
                [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1],
              ]},
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // Vertical line to ground (so you can see where they are standing)
            groundLine: new Form(
              { type: "line", positions: [
                [0, 0, 0, 1], [0, -2, 0, 1],  // Line down to ground
              ], colors: [
                [r, g, b, 0.5], [r, g, b, 0.5],  // Semi-transparent
              ]},
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // Face (eyes + mouth on front of box)
            face: new Form(
              { type: "line", ...createFaceGeometry(boxSize, r, g, b) },
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // Name sign above player (using 3D text)
            nameSign: globalSign ? globalSign(content.handle || id.slice(0, 6), {
              scale: 0.03,
              color: playerColor,
              align: "center",
              glyphs: globalGlyphs?.("MatrixChunky8") || {},
            }) : null,
          };
          console.log("🎮 Created camera frustum for:", id, content.handle, "color:", playerColor);
        }
        
        gameState = "playing";
      }
      
      // Handle state response (sent by existing players to new joiners)
      // This does NOT trigger a response back - breaks the infinite loop
      if (type === "1v1:state") {
        console.log(`Player state received:`, content.handle, id);
        
        // Update or create player entry
        if (!others[id]) {
          others[id] = {
            handle: content.handle,
            pos: content.pos || { x: 0, y: 1.6, z: 0 },
            rot: content.rot || { x: 0, y: 0, z: 0 },
            health: content.health || 100,
          };
          
          // Create player visualization (same as in 1v1:join)
          if (!playerBoxes[id]) {
            const playerColors = [
              [0, 1, 0], [1, 0.5, 0], [0, 1, 1], [1, 0, 1], [1, 1, 0], [0.5, 0.5, 1],
            ];
            const colorIndex = Object.keys(playerBoxes).length % playerColors.length;
            const playerColor = playerColors[colorIndex];
            const [r, g, b] = playerColor;
            const boxSize = 0.15;
            const frustumLength = 0.5;
            const frustumSpread = 0.3;
            
            playerBoxes[id] = {
              color: playerColor,
              cameraBox: new Form(
                { type: "line", positions: [
                  [-boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, -boxSize, 1],
                  [boxSize, -boxSize, -boxSize, 1], [boxSize, boxSize, -boxSize, 1],
                  [boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, -boxSize, 1],
                  [-boxSize, boxSize, -boxSize, 1], [-boxSize, -boxSize, -boxSize, 1],
                  [-boxSize, -boxSize, boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                  [boxSize, -boxSize, boxSize, 1], [boxSize, boxSize, boxSize, 1],
                  [boxSize, boxSize, boxSize, 1], [-boxSize, boxSize, boxSize, 1],
                  [-boxSize, boxSize, boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                  [-boxSize, -boxSize, -boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                  [boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                  [boxSize, boxSize, -boxSize, 1], [boxSize, boxSize, boxSize, 1],
                  [-boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, boxSize, 1],
                ], colors: Array(24).fill([r, g, b, 1])},
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              frustum: new Form(
                { type: "line", positions: [
                  [0, 0, 0, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
                  [0, 0, 0, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                  [0, 0, 0, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                  [0, 0, 0, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                  [-frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                  [frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                  [frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                  [-frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
                ], colors: Array(16).fill([1, 0, 0, 1])},
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              groundLine: new Form(
                { type: "line", positions: [[0, 0, 0, 1], [0, -2, 0, 1]], colors: [[r, g, b, 0.5], [r, g, b, 0.5]]},
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              face: new Form(
                { type: "line", ...createFaceGeometry(boxSize, r, g, b) },
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              nameSign: globalSign ? globalSign(content.handle || id.slice(0, 6), {
                scale: 0.03, color: playerColor, align: "center",
                glyphs: globalGlyphs?.("MatrixChunky8") || {},
              }) : null,
            };
            console.log("🎮 Created visualization for existing player:", id, content.handle);
          }
        } else {
          // Just update position if we already know them
          others[id].pos = content.pos;
          others[id].rot = content.rot;
          others[id].health = content.health;
        }
        
        gameState = "playing";
      }
      
      // WebSocket fallback for position (UDP is primary, but WS works if UDP fails)
      if (type === "1v1:move") {
        // Log occasionally for debugging
        if (Math.random() < 0.05) {
          logNetwork(`WS recv 1v1:move from ${content.handle || id} pos=${content.pos?.x?.toFixed(1)},${content.pos?.y?.toFixed(1)},${content.pos?.z?.toFixed(1)}`);
        }
        if (others[id]) {
          others[id].pos = content.pos;
          others[id].rot = content.rot;
          
          // Update player box position
          if (playerBoxes[id]) {
            playerBoxes[id].position = [content.pos.x, content.pos.y, content.pos.z];
          }
        } else {
          // We got a move from someone we don't know about yet
          logNetwork(`WS 1v1:move from unknown player ${id}, handle=${content.handle}`, "warn");
        }
      }
      
      if (type === "1v1:shoot") {
        console.log("💥 Player shot:", id);
        // TODO: Show muzzle flash or projectile
      }
      
      if (type === "1v1:hit") {
        if (content.targetId === self.id) {
          self.health -= content.damage;
          console.log(`💔 Hit! Health: ${self.health}`);
          
          if (self.health <= 0) {
            self.health = 0;
            self.deaths++;
            gameState = "dead";
            console.log("💀 You died!");
            
            // Notify killer
            server.send("1v1:death", { killerId: id });
          }
        }
      }
      
      if (type === "1v1:death" && content.killerId === self.id) {
        self.kills++;
        console.log(`💀 Kill! Total: ${self.kills}`);
      }
    }
  });
  
  // Enable clipped wireframes by default if available
  // Note: graphAPI will be set in paint function
  
  // Create a gradient quad (6 vertices = 2 triangles)
  const quadPositions = [
    // Triangle 1
    [-1, -1, 0, 1], // Bottom Left
    [-1, 1, 0, 1],  // Top Left
    [1, 1, 0, 1],   // Top Right
    // Triangle 2
    [-1, -1, 0, 1], // Bottom Left
    [1, 1, 0, 1],   // Top Right
    [1, -1, 0, 1],  // Bottom Right
  ];
  
  // Gradient colors: corners go Red, Green, Blue, Yellow
  const quadColors = [
    [1.0, 0.0, 0.0, 1.0], // Bottom Left: RED
    [0.0, 1.0, 0.0, 1.0], // Top Left: GREEN
    [0.0, 0.0, 1.0, 1.0], // Top Right: BLUE
    [1.0, 0.0, 0.0, 1.0], // Bottom Left: RED (repeated)
    [0.0, 0.0, 1.0, 1.0], // Top Right: BLUE (repeated)
    [1.0, 1.0, 0.0, 1.0], // Bottom Right: YELLOW
  ];
  
  texturedQuad = new Form(
    { type: "triangle", positions: quadPositions, colors: quadColors },
    { pos: [-2, 0.5, -6], rot: [0, 0, 0], scale: 1 }
  );
  
  // Create a simple ground plane - just one quad (2 triangles)
  // Very small size to avoid clipping issues
  const groundSize = 3; // Very small ground plane (6x6 units total)
  
  const groundPositions = [
    // Triangle 1 (counter-clockwise winding when viewed from above)
    [-groundSize, -1.5, -groundSize, 1], // Back Left
    [-groundSize, -1.5, groundSize, 1],  // Front Left  
    [groundSize, -1.5, groundSize, 1],   // Front Right
    // Triangle 2
    [-groundSize, -1.5, -groundSize, 1], // Back Left
    [groundSize, -1.5, groundSize, 1],   // Front Right
    [groundSize, -1.5, -groundSize, 1],  // Back Right
  ];
  
  // UV coordinates tile the texture across the ground
  const groundTexCoords = [
    [0, 0],
    [0, 2],  // Tile 2x in each direction
    [2, 2],
    [0, 0],
    [2, 2],
    [2, 0]
  ];
  
  // White colors (6 vertices = 2 triangles)
  const groundColors = [
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0]
  ];
  
  groundPlane = new Form(
    { type: "triangle", positions: groundPositions, colors: groundColors, texCoords: groundTexCoords },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  
  // Disable fading for the ground plane so it stays visible
  groundPlane.noFade = true;
  
  console.log("🌍 Ground plane created:", groundPositions.length / 3, "triangles");
  
  // Create wireframe version of ground plane for debugging
  const wireframePositions = [];
  const wireframeColors = [];
  
  // Convert each triangle to 3 lines (only 2 triangles now)
  for (let i = 0; i < groundPositions.length; i += 3) {
    const v0 = groundPositions[i];
    const v1 = groundPositions[i + 1];
    const v2 = groundPositions[i + 2];
    
    // Line 1: v0 to v1
    wireframePositions.push(v0, v1);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]); // Yellow
    
    // Line 2: v1 to v2
    wireframePositions.push(v1, v2);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]);
    
    // Line 3: v2 to v0
    wireframePositions.push(v2, v0);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]);
  }
  
  groundWireframe = new Form(
    { type: "line", positions: wireframePositions, colors: wireframeColors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  groundWireframe.noFade = true;
  
  // Create a spinning wireframe cube
  cube = new Form(
    CUBEL,
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
  
  // Create a gradient wireframe triangle to the right (as line pairs)
  const trianglePositions = [
    // Line 1: Top to Bottom-right
    [0, 0.6, 0, 1], [0.5, -0.3, 0, 1],
    // Line 2: Bottom-right to Bottom-left
    [0.5, -0.3, 0, 1], [-0.5, -0.3, 0, 1],
    // Line 3: Bottom-left to Top
    [-0.5, -0.3, 0, 1], [0, 0.6, 0, 1],
  ];
  
  const triangleColors = [
    [1.0, 0.0, 0.0, 1.0], [0.0, 1.0, 0.0, 1.0],     // RED to GREEN
    [0.0, 1.0, 0.0, 1.0], [0.0, 0.0, 1.0, 1.0],     // GREEN to BLUE  
    [0.0, 0.0, 1.0, 1.0], [1.0, 0.0, 0.0, 1.0],     // BLUE to RED
  ];
  
  triangle = new Form(
    { type: "line", positions: trianglePositions, colors: triangleColors },
    { pos: [2, 0.5, -4], rot: [0, 0, 0], scale: 0.7 }
  );
  
  // Create a FILLED gradient triangle on the LEFT side of the cube
  const filledTriPositions = [
    [0, 0.6, 0, 1],      // Top vertex
    [0.5, -0.3, 0, 1],   // Bottom-right vertex
    [-0.5, -0.3, 0, 1],  // Bottom-left vertex
  ];
  
  const filledTriColors = [
    [1.0, 0.0, 0.0, 1.0],    // RED at top
    [0.0, 1.0, 0.0, 1.0],    // GREEN at bottom-right
    [0.0, 0.0, 1.0, 1.0],    // BLUE at bottom-left
  ];
  
  filledTriangle = new Form(
    { type: "triangle", positions: filledTriPositions, colors: filledTriColors },
    { pos: [-2, 0.5, -4], rot: [0, 0, 0], scale: 0.7 }
  );
  
  // 🤖 Create bot model (red/orange color scheme to distinguish from players)
  if (bot.enabled) {
    const boxSize = 0.2;
    const frustumLength = 0.6;
    const frustumSpread = 0.35;
    const r = 1, g = 0.3, b = 0; // Orange-red color
    
    botModel = {
      color: [r, g, b],
      // Camera box (cube wireframe at bot position)
      cameraBox: new Form(
        { type: "line", positions: [
          // Front face
          [-boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, -boxSize, 1],
          [boxSize, -boxSize, -boxSize, 1], [boxSize, boxSize, -boxSize, 1],
          [boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, -boxSize, 1],
          [-boxSize, boxSize, -boxSize, 1], [-boxSize, -boxSize, -boxSize, 1],
          // Back face
          [-boxSize, -boxSize, boxSize, 1], [boxSize, -boxSize, boxSize, 1],
          [boxSize, -boxSize, boxSize, 1], [boxSize, boxSize, boxSize, 1],
          [boxSize, boxSize, boxSize, 1], [-boxSize, boxSize, boxSize, 1],
          [-boxSize, boxSize, boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
          // Connecting edges
          [-boxSize, -boxSize, -boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
          [boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, boxSize, 1],
          [boxSize, boxSize, -boxSize, 1], [boxSize, boxSize, boxSize, 1],
          [-boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, boxSize, 1],
        ], colors: Array(24).fill([r, g, b, 1])},
        { pos: [bot.pos.x, bot.pos.y, bot.pos.z], scale: 1 }
      ),
      // View frustum (shows where bot is looking)
      frustum: new Form(
        { type: "line", positions: [
          [0, 0, 0, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
          [0, 0, 0, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
          [0, 0, 0, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
          [0, 0, 0, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
          [-frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
          [frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
          [frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
          [-frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
        ], colors: Array(16).fill([1, 0.5, 0, 1])},  // Bright orange frustum
        { pos: [bot.pos.x, bot.pos.y, bot.pos.z], scale: 1 }
      ),
      // Ground line
      groundLine: new Form(
        { type: "line", positions: [[0, 0, 0, 1], [0, -2, 0, 1]], colors: [[r, g, b, 0.5], [r, g, b, 0.5]]},
        { pos: [bot.pos.x, bot.pos.y, bot.pos.z], scale: 1 }
      ),
      // Face (eyes + mouth)
      face: new Form(
        { type: "line", ...createFaceGeometry(boxSize, r, g, b) },
        { pos: [bot.pos.x, bot.pos.y, bot.pos.z], scale: 1 }
      ),
      // Name sign
      nameSign: sign ? sign("🤖 BOT", {
        scale: 0.03, color: [r, g, b], align: "center",
        glyphs: glyphs?.("MatrixChunky8") || {},
      }) : null,
    };
    console.log("🤖 Bot model created at", bot.pos);
  }
  
  // Player camera frustums are created dynamically when players join
  
  // Load the latest painting texture from TV endpoint
  if (get) {
    console.log("🎨 Starting to load painting from TV endpoint...");
    loadLatestPaintingTexture(get);
  }
}

function sim() {
  frameCount++; // Increment global frame counter
  
  // 🎯 Auto-tracking: smoothly turn toward current target
  if (isAutoTracking && currentTarget && graphInstance) {
    const targetPos = getTargetPosition();
    if (targetPos) {
      const targetAngle = getAngleToTarget(self.pos, targetPos);
      let angleDiff = targetAngle - graphInstance.rotY;
      
      // Normalize angle difference to -180 to 180
      while (angleDiff > 180) angleDiff -= 360;
      while (angleDiff < -180) angleDiff += 360;
      
      // Smoothly turn toward target
      if (Math.abs(angleDiff) > 1) {
        const turnAmount = Math.sign(angleDiff) * Math.min(Math.abs(angleDiff), TURN_SPEED);
        graphInstance.rotY += turnAmount;
        
        // Normalize rotation
        while (graphInstance.rotY > 180) graphInstance.rotY -= 360;
        while (graphInstance.rotY < -180) graphInstance.rotY += 360;
      }
    }
  }
  
  // Update target list periodically (every 30 frames)
  if (frameCount % 30 === 0) {
    updateTargetList();
  }
  
  // Network update - send position via UDP for low latency
  // Send in both lobby and playing states so players can see each other
  if ((gameState === "playing" || gameState === "lobby") && graphInstance) {
    self.pos = { 
      x: graphInstance.x, 
      y: graphInstance.y, 
      z: graphInstance.z 
    };
    self.rot = { 
      x: graphInstance.rotX, 
      y: graphInstance.rotY, 
      z: graphInstance.rotZ 
    };
    
    // Send position updates - try UDP first (low latency), fall back to WebSocket
    // 🎭 Skip sending updates if we're in spectator mode (but don't skip rest of sim)
    if (!isSpectator) {
      const moveData = {
        handle: self.handle,  // Use handle to identify player (links WS and UDP)
        pos: self.pos,
        rot: self.rot,
      };
      
      // Track UDP connected state for HUD
      udpConnected = udpChannel?.connected || false;
      
      if (udpChannel?.connected) {
        // 🩰 Send position over UDP (low latency, unreliable but fast)
        // Throttle to every 2 frames (~30 updates/sec at 60fps) to reduce spam
        if (frameCount % 2 === 0) {
          // Log every ~60 frames (roughly once per second at 60fps)
          if (frameCount % 60 === 0) {
            logNetwork(`UDP send: pos=${moveData.pos.x.toFixed(1)},${moveData.pos.y.toFixed(1)},${moveData.pos.z.toFixed(1)}`);
          }
          udpChannel.send("1v1:move", moveData);
        }
      } else if (server) {
        // 🕸️ Fallback: Send position over WebSocket (reliable but higher latency)
        // Only send every 3rd frame to reduce bandwidth (~20 updates/sec at 60fps)
        if (frameCount % 3 === 0) {
          server.send("1v1:move", moveData);
        }
      }
    }
  }
  
  // Rotate the cube around its local center
  cube.rotation[0] += 0.3;
  cube.rotation[1] += 0.5;
  
  // Rotate the wireframe triangle around its local center
  triangle.rotation[2] += 1.0; // Fast spin around Z axis
  triangle.rotation[1] += 0.2; // Slight Y wobble
  
  // Rotate the filled triangle differently
  filledTriangle.rotation[0] += 0.4; // X axis spin
  filledTriangle.rotation[2] += 0.6; // Z axis spin
  
  // Rotate the textured quad
  texturedQuad.rotation[1] += 0.5; // Spin on Y axis
  
  // 🤖 Bot AI simulation - bot runs around for players to follow
  if (bot.enabled && botModel) {
    // Handle respawn timer
    if (bot.state === "dead") {
      bot.respawnTimer--;
      if (bot.respawnTimer <= 0) {
        bot.state = "patrol";
        bot.health = 100;
        bot.pos = { ...bot.patrolPoints[0], y: -0.5 };  // Match player eye height
        console.log("🤖 Bot respawned!");
      }
    } else {
      // Bot just patrols freely - players follow it (chill behavior)
      
      // Check if bot is pausing at a waypoint
      if (bot.pauseTimer > 0) {
        bot.pauseTimer--;
      } else {
        // Move toward current patrol point
        const target = bot.patrolPoints[bot.patrolIndex];
        const tdx = target.x - bot.pos.x;
        const tdz = target.z - bot.pos.z;
        const distToTarget = Math.sqrt(tdx * tdx + tdz * tdz);
        
        if (distToTarget < 0.3) {
          // Reached patrol point - pause for a bit, then pick next
          bot.pauseTimer = 60 + Math.floor(Math.random() * 120); // 1-3 seconds pause
          if (Math.random() < 0.3) {
            // Occasionally skip to a random point for unpredictability
            bot.patrolIndex = Math.floor(Math.random() * bot.patrolPoints.length);
          } else {
            bot.patrolIndex = (bot.patrolIndex + 1) % bot.patrolPoints.length;
          }
        } else {
          // Calculate target angle and smoothly turn toward it
          const targetAngle = Math.atan2(tdx, tdz) * (180 / Math.PI);
          let angleDiff = targetAngle - bot.rot.y;
          // Normalize angle difference to -180 to 180
          while (angleDiff > 180) angleDiff -= 360;
          while (angleDiff < -180) angleDiff += 360;
          
          // Smooth turning - turn toward target
          bot.rot.y += Math.sign(angleDiff) * Math.min(Math.abs(angleDiff), bot.turnSpeed);
          // Normalize rotation
          while (bot.rot.y > 180) bot.rot.y -= 360;
          while (bot.rot.y < -180) bot.rot.y += 360;
          
          // Only move forward once mostly facing the target (within 30 degrees)
          if (Math.abs(angleDiff) < 30) {
            // Move in the direction bot is FACING (rot.y)
            bot.pos.x += Math.sin(bot.rot.y * Math.PI / 180) * bot.speed;
            bot.pos.z += Math.cos(bot.rot.y * Math.PI / 180) * bot.speed;
          }
          
          // Keep bot inside arena bounds (-2.8 to 2.8 to stay safely inside)
          bot.pos.x = Math.max(-2.8, Math.min(2.8, bot.pos.x));
          bot.pos.z = Math.max(-2.8, Math.min(2.8, bot.pos.z));
        }
      }
      
      // Update bot model positions
      const px = bot.pos.x;
      const py = -bot.pos.y;  // Negate Y like other players
      const pz = bot.pos.z;
      const pitch = 0;
      const yaw = bot.rot.y;
      
      botModel.cameraBox.position = [px, py, pz];
      botModel.frustum.position = [px, py, pz];
      botModel.frustum.rotation = [pitch, yaw, 0];
      botModel.groundLine.position = [px, py, pz];
      if (botModel.face) {
        botModel.face.position = [px, py, pz];
        botModel.face.rotation = [pitch, yaw, 0];
      }
      if (botModel.nameSign) {
        botModel.nameSign.position = [px, py + 0.4, pz];
        botModel.nameSign.rotation = [0, (self.rot?.y || 0) + 180, 0];
      }
    }
  }
}

function paint({ wipe, ink, painting, screen, line: drawLine, box: drawBox, clearWireframeBuffer, drawBufferedWireframes, getRenderStats, setShowClippedWireframes }) {
  // Store render stats function for debug logging  
  if (!graphAPI) {
    graphAPI = { getRenderStats };
  }
  
  // Calculate FPS
  const now = performance.now();
  const deltaTime = now - lastFrameTime;
  lastFrameTime = now;
  
  // Keep last 60 frame times for smoothed FPS
  frameTimes.push(deltaTime);
  if (frameTimes.length > 60) frameTimes.shift();
  
  const avgFrameTime = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const currentFPS = Math.round(1000 / avgFrameTime);
  
  // FIRST: Set wireframe visibility BEFORE any rendering happens
  if (setShowClippedWireframes) {
    setShowClippedWireframes(showWireframes);
  }
  
  // SECOND: Clear wireframe buffer at start of frame
  if (clearWireframeBuffer) {
    clearWireframeBuffer();
  }
  
  if (!paint.frameCount) paint.frameCount = 0;
  paint.frameCount++;
  
  // Create ground texture with a checkerboard pattern if it doesn't exist
  if (!groundTexture) {
    const texSize = 64;
    groundTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box, line } = api;
      
      // Fill with dark yellow/orange
      wipe(100, 80, 0); // Darker yellow base
      
      // Draw dark blue checkerboard squares
      const squareSize = 8;
      ink(0, 0, 80); // Dark blue ink
      for (let y = 0; y < texSize / squareSize; y++) {
        for (let x = 0; x < texSize / squareSize; x++) {
          if ((x + y) % 2 === 1) {
            box(x * squareSize, y * squareSize, squareSize, squareSize);
          }
        }
      }
    });
    
    groundPlane.texture = groundTexture;
    console.log("🎨 Ground texture created:", groundTexture?.width, "x", groundTexture?.height, "=", groundTexture?.pixels?.length, "bytes");
    console.log("🎨 First few pixels:", groundTexture?.pixels?.slice(0, 16));
    console.log("🎨 Ground plane has texture:", !!groundPlane.texture);
  }
  
  // Create checkerboard texture for quad if it doesn't exist
  if (!quadTexture) {
    // Use a placeholder checkerboard texture while loading
    const texSize = 64;
    quadTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box } = api;
      wipe(50, 50, 50); // Dark gray base
      
      // Draw checkerboard pattern
      const squareSize = 8;
      for (let y = 0; y < texSize / squareSize; y++) {
        for (let x = 0; x < texSize / squareSize; x++) {
          if ((x + y) % 2 === 0) {
            ink(200, 200, 200); // Light gray squares
            box(x * squareSize, y * squareSize, squareSize, squareSize);
          }
        }
      }
    });
    
    // Assign texture to quad - now with perspective-correct mapping!
    texturedQuad.texture = quadTexture;
  }
  
  // Dark background for FPS arena
  wipe(20, 20, 30)
    .form(groundPlane); // Ground plane only
  
  // Render camera debug markers (height reference)
  // Player camera frustums are rendered below in the playerBoxes loop
  
  // Render other players as camera frustums
  for (const [id, playerModel] of Object.entries(playerBoxes)) {
    const other = others[id];
    if (other && playerModel) {
      const px = other.pos.x;
      const py = -other.pos.y;  // NEGATE Y - camera Y is inverted in the coordinate system
      const pz = other.pos.z;
      const pitch = -(other.rot?.x || 0);  // NEGATED - camera pitch is inverted
      const yaw = other.rot?.y || 0;       // Player's yaw (look left/right)
      
      // Position camera box at player's exact camera position
      playerModel.cameraBox.position = [px, py, pz];
      
      // Frustum follows camera position and FULL rotation (pitch + yaw)
      playerModel.frustum.position = [px, py, pz];
      playerModel.frustum.rotation = [pitch, yaw, 0];  // Pitch and yaw
      
      // Ground line shows where they're standing
      playerModel.groundLine.position = [px, py, pz];
      
      // Face follows camera box position and rotation (so it looks where they look)
      if (playerModel.face) {
        playerModel.face.position = [px, py, pz];
        playerModel.face.rotation = [pitch, yaw, 0];  // Face looks where player looks
      }
      
      // Name sign above player (billboard - always face camera)
      if (playerModel.nameSign) {
        playerModel.nameSign.position = [px, py + 0.35, pz];  // Above the camera box
        // Billboard rotation: face toward our camera by using inverse of our yaw
        playerModel.nameSign.rotation = [0, (self.rot?.y || 0) + 180, 0];
      }
      
      // Render camera visualization (colors come from vertex colors)
      ink(255, 255, 255).form(playerModel.cameraBox);
      ink(255, 255, 255).form(playerModel.frustum);
      ink(255, 255, 255).form(playerModel.groundLine);
      
      // Render face (eyes + mouth)
      if (playerModel.face) {
        ink(255, 255, 255).form(playerModel.face);
      }
      
      // Render name sign
      if (playerModel.nameSign) {
        ink(255, 255, 255).form(playerModel.nameSign);
      }
    }
  }
  
  // 🤖 Render bot
  if (bot.enabled && botModel && bot.state !== "dead") {
    ink(255, 255, 255).form(botModel.cameraBox);
    ink(255, 255, 255).form(botModel.frustum);
    ink(255, 255, 255).form(botModel.groundLine);
    if (botModel.face) {
      ink(255, 255, 255).form(botModel.face);
    }
    if (botModel.nameSign) {
      ink(255, 255, 255).form(botModel.nameSign);
    }
  }
  
  // Draw wireframes on top AFTER all other forms to ensure they're always visible
  if (showWireframes) {
    // Note: Ground plane wireframes are now generated automatically via buffered system
    // No need to render separate groundWireframe form
    
    // Draw all buffered clipped wireframes
    if (drawBufferedWireframes) {
      drawBufferedWireframes();
    }
  }
  
  const hudFont = "MatrixChunky8";
  
  // === 2D HUD OVERLAY ===
  
  // === TOP RIGHT: FPS counter (above minimap) ===
  const fpsColor = currentFPS >= 55 ? [0, 255, 0] : currentFPS >= 30 ? [255, 255, 0] : [255, 0, 0];
  const fpsText = `${currentFPS} FPS`;
  // Right-align FPS text (approx 6px per char in MatrixChunky8)
  const fpsTextWidth = fpsText.length * 6;
  ink(...fpsColor).write(fpsText, { x: screen.width - fpsTextWidth - 8, y: 4 }, undefined, undefined, false, hudFont);
  
  // Draw crosshair FIRST (center of screen)
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const crosshairSize = 8;
  ink(255, 255, 255, 200)
    .line(centerX - crosshairSize, centerY, centerX + crosshairSize, centerY)
    .line(centerX, centerY - crosshairSize, centerX, centerY + crosshairSize);
  
  // === TOP LEFT: Network status + game state (offset to avoid prompt label) ===
  const hudStartY = 24; // Start below prompt HUD corner label
  const netNow = Date.now();
  
  // UDP status
  const udpStatus = udpChannel?.connected ? "UDP:ON" : "UDP:OFF";
  const udpAge = lastUdpReceiveTime ? Math.floor((netNow - lastUdpReceiveTime) / 1000) : "?";
  const udpColor = udpChannel?.connected ? (udpAge < 5 ? [0, 255, 0] : [255, 255, 0]) : [255, 0, 0];
  ink(...udpColor).write(`${udpStatus} (${udpMessageCount}msg ${udpAge}s)`, { x: 6, y: hudStartY }, undefined, undefined, false, hudFont);
  
  // WebSocket status  
  const wsStatus = wsConnected ? "WS:ON" : "WS:OFF";
  const wsAge = lastWsReceiveTime ? Math.floor((netNow - lastWsReceiveTime) / 1000) : "?";
  const wsColor = wsConnected ? (wsAge < 10 ? [0, 255, 0] : [255, 255, 0]) : [255, 0, 0];
  ink(...wsColor).write(`${wsStatus} (${wsMessageCount}msg ${wsAge}s)`, { x: 6, y: hudStartY + 10 }, undefined, undefined, false, hudFont);
  
  // Identity/handle
  const selfIdStr = typeof self.id === "string" ? self.id.slice(0, 6) : "...";
  ink(150, 150, 255).write(`${self.handle} (${selfIdStr})`, { x: 6, y: hudStartY + 20 }, undefined, undefined, false, hudFont);
  
  // Game state + spectator mode
  const stateColor = isSpectator ? [255, 165, 0] : [255, 255, 255];
  const stateText = isSpectator ? `${gameState.toUpperCase()} [SPECTATOR]` : gameState.toUpperCase();
  ink(...stateColor).write(stateText, { x: 6, y: hudStartY + 30 }, undefined, undefined, false, hudFont);
  
  // 🎯 Target tracking indicator
  if (currentTarget || targetList.length > 0) {
    const targetName = currentTarget 
      ? (currentTarget === "bot" ? "🤖BOT" : (others[currentTarget]?.handle || "???"))
      : "---";
    const trackIcon = isAutoTracking ? "🎯" : "👁️";
    const targetColor = isAutoTracking ? [0, 255, 255] : [200, 200, 200];
    ink(...targetColor).write(`${trackIcon} ${targetName} [Tab/T/F]`, { x: 6, y: hudStartY + 40 }, undefined, undefined, false, hudFont);
  }
  
  // Gamepad status indicator (small)
  const gpCount = Object.keys(connectedGamepads).length;
  if (gpCount > 0) {
    const gpIndicator = showGamepadPanel ? `🎮${gpCount}` : `🎮${gpCount} [G]`;
    ink(0, 200, 200).write(gpIndicator, { x: 6, y: hudStartY + 50 }, undefined, undefined, false, hudFont);
  }
  
  // === TOP RIGHT: MINIMAP ===
  const playerCount = Object.keys(others).length;
  const rightMargin = 6;
  
  // Minimap settings
  const mapSize = 60;  // 60x60 pixel minimap
  const mapX = screen.width - mapSize - rightMargin;
  const mapY = 4;
  const mapScale = mapSize / 6;  // Arena is 6 units (-3 to +3)
  const mapCenterX = mapX + mapSize / 2;
  const mapCenterY = mapY + mapSize / 2;
  
  // Minimap background (semi-transparent)
  ink(0, 0, 0, 180).box(mapX, mapY, mapSize, mapSize);
  ink(40, 40, 40).box(mapX, mapY, mapSize, mapSize, "outline");
  
  // Arena boundary outline (the ground plane is -3 to +3)
  ink(60, 60, 60).box(mapX + 2, mapY + 2, mapSize - 4, mapSize - 4, "outline");
  
  // Helper to convert world coords to minimap coords
  const worldToMap = (wx, wz) => ({
    mx: mapCenterX + wx * mapScale,
    my: mapCenterY - wz * mapScale  // Flip Z for top-down view (north = up)
  });
  
  // Draw self (green triangle pointing in look direction)
  const selfMap = worldToMap(self.pos.x, self.pos.z);
  const selfAngle = (self.rot?.y || 0) * Math.PI / 180;
  const triSize = 4;
  // Triangle pointing in direction of rotation
  const tipX = selfMap.mx + Math.sin(selfAngle) * triSize;
  const tipY = selfMap.my - Math.cos(selfAngle) * triSize;
  const baseAngle1 = selfAngle + 2.5;  // ~140 degrees back
  const baseAngle2 = selfAngle - 2.5;
  ink(0, 255, 0).line(tipX, tipY, selfMap.mx + Math.sin(baseAngle1) * triSize * 0.6, selfMap.my - Math.cos(baseAngle1) * triSize * 0.6);
  ink(0, 255, 0).line(tipX, tipY, selfMap.mx + Math.sin(baseAngle2) * triSize * 0.6, selfMap.my - Math.cos(baseAngle2) * triSize * 0.6);
  ink(0, 255, 0).box(selfMap.mx - 1, selfMap.my - 1, 2, 2);  // Center dot
  
  // Draw other players (colored dots)
  for (const [id, other] of Object.entries(others)) {
    const pMap = worldToMap(other.pos.x, other.pos.z);
    const pColor = playerBoxes[id]?.color;
    if (pColor) {
      ink(pColor[0] * 255, pColor[1] * 255, pColor[2] * 255).box(pMap.mx - 2, pMap.my - 2, 4, 4);
    } else {
      ink(255, 150, 50).box(pMap.mx - 2, pMap.my - 2, 4, 4);
    }
  }
  
  // Draw bot (orange/yellow pulsing dot)
  if (bot.enabled && bot.state !== "dead") {
    const botMap = worldToMap(bot.pos.x, bot.pos.z);
    const pulse = Math.sin(Date.now() / 200) * 0.3 + 0.7;  // Pulsing effect
    ink(255 * pulse, 165 * pulse, 0).box(botMap.mx - 2, botMap.my - 2, 4, 4);
    // Show bot facing direction
    const botAngle = bot.rot.y * Math.PI / 180;
    const botDirX = botMap.mx + Math.sin(botAngle) * 5;
    const botDirY = botMap.my - Math.cos(botAngle) * 5;
    ink(255, 165, 0, 150).line(botMap.mx, botMap.my, botDirX, botDirY);
  }
  
  // Player count below minimap
  const countText = `${playerCount + 1}P`;
  ink(255, 255, 0).write(countText, { x: mapX + mapSize - countText.length * 4, y: mapY + mapSize + 2 }, undefined, undefined, false, hudFont);
  
  // === TOP CENTER: Health bar ===
  const healthBarWidth = 100;
  const healthBarHeight = 10;
  const healthBarX = centerX - healthBarWidth / 2;
  const healthBarY = 4;  // Top of screen
  const healthPercent = self.health / 100;
  
  // Background
  ink(30, 30, 30, 200).box(healthBarX, healthBarY, healthBarWidth, healthBarHeight);
  
  // Fill color based on health
  const healthColor = healthPercent > 0.5 ? [0, 220, 0] : healthPercent > 0.25 ? [220, 220, 0] : [220, 0, 0];
  ink(...healthColor).box(healthBarX + 1, healthBarY + 1, (healthBarWidth - 2) * healthPercent, healthBarHeight - 2);
  
  // HP text centered below bar
  const hpText = `HP ${Math.ceil(self.health)}`;
  ink(255, 255, 255).write(hpText, { x: centerX - hpText.length * 2, y: healthBarY + healthBarHeight + 2 }, undefined, undefined, false, hudFont);
  
  // === BOTTOM RIGHT: K/D ===
  const kdText = `K${self.kills} D${self.deaths}`;
  ink(255, 255, 255).write(kdText, { x: screen.width - rightMargin - kdText.length * 4, y: screen.height - 14 }, undefined, undefined, false, hudFont);
  
  // === BOTTOM CENTER: Instructions when solo ===
  if (playerCount === 0) {
    const soloText = "WAITING...";
    ink(150, 150, 150).write(soloText, { x: centerX - soloText.length * 2, y: screen.height - 14 }, undefined, undefined, false, hudFont);
  }
  
  // Show "Waiting for opponent" message in lobby
  if (gameState === "lobby") {
    ink(255, 255, 0, 200).write("WAITING FOR OPPONENT...", { 
      x: screen.width / 2 - 100, 
      y: screen.height / 2 
    });
  }
  
  // Show death screen
  if (gameState === "dead") {
    ink(255, 0, 0, 200).write("YOU DIED", { 
      x: screen.width / 2 - 40, 
      y: screen.height / 2 - 20 
    });
    ink(255, 255, 255, 150).write("Respawning in 3s...", { 
      x: screen.width / 2 - 70, 
      y: screen.height / 2 + 10 
    });
  }

  // Draw debug panel in top-right corner
  if (showDebugPanel) {
    const stats = system?.fps?.renderStats || {
      originalTriangles: 0,
      clippedTriangles: 0,
      subdividedTriangles: 0,
      wireframeSegmentsTotal: 0,
      wireframeSegmentsTextured: 0,
      wireframeSegmentsGradient: 0,
      pixelsDrawn: 0,
      trianglesRejected: 0,
    };

    const charWidth = 4;
    const lineHeight = 8;
    const title = "FPS DEBUG";
    const panelPadding = 6;
    const spacingAfterTitle = 2;
    const marginFromEdge = 8;

    const lines = [
      { color: "lime", text: `FPS: ${currentFPS}` },
      { color: "orange", text: `Frame: ${avgFrameTime.toFixed(2)}ms` },
      { color: "red", text: `Pixels: ${stats.pixelsDrawn}` },
      { color: "yellow", text: `Wireframes: ${showWireframes ? "ON" : "OFF"}` },
      { color: "cyan", text: `Original Tris: ${stats.originalTriangles}` },
      { color: "magenta", text: `Clipped Tris: ${stats.clippedTriangles}` },
      { color: "yellow", text: `Subdivided: ${stats.subdividedTriangles}` },
      { color: "red", text: `Rejected: ${stats.trianglesRejected || 0}` },
      { color: "white", text: `WF Total: ${stats.wireframeSegmentsTotal}` },
      { color: "white", text: `- Textured: ${stats.wireframeSegmentsTextured}` },
      { color: "white", text: `- Gradient: ${stats.wireframeSegmentsGradient}` },
      { color: "gray", text: "V: Wireframe" },
      { color: "gray", text: "P: Panel" },
      { color: "gray", text: "G: Gamepad" },
      { color: "gray", text: "Q: QR Code" },
      { color: "gray", text: "L: Log Debug" },
    ];

    const measureWidth = (text) => text.length * charWidth;
    const maxLineWidth = Math.max(measureWidth(title), ...lines.map((line) => measureWidth(line.text)));
    const minPanelWidth = 120;
    const panelWidth = Math.max(minPanelWidth, panelPadding * 2 + maxLineWidth);
    const panelHeight = panelPadding * 2 + lineHeight + spacingAfterTitle + lines.length * lineHeight;
    const panelX = screen.width - panelWidth - marginFromEdge;
    const panelY = 10;

    // Semi-transparent background
    ink(0, 0, 0, 180);
    drawBox(panelX, panelY, panelWidth, panelHeight);

    // Panel border
    ink(255, 255, 0, 255);
    drawLine(panelX, panelY, panelX + panelWidth, panelY);
    drawLine(panelX + panelWidth, panelY, panelX + panelWidth, panelY + panelHeight);
    drawLine(panelX + panelWidth, panelY + panelHeight, panelX, panelY + panelHeight);
    drawLine(panelX, panelY + panelHeight, panelX, panelY);

    // Title
    let textY = panelY + panelPadding;
    ink("white").write(title, { x: panelX + panelPadding, y: textY }, undefined, undefined, false, debugFont);
    textY += lineHeight + spacingAfterTitle;

    // Content lines
    lines.forEach(({ color, text }) => {
      ink(color).write(text, { x: panelX + panelPadding, y: textY }, undefined, undefined, false, debugFont);
      textY += lineHeight;
    });
  }
  
  // === GAMEPAD/KEYBOARD CONTROLS PANEL - VERY BOTTOM LEFT (toggle with 'G' key) ===
  if (showGamepadPanel) {
    const gpPanelX = 6;
    const kbPanelHeight = 38;  // Compact height for new layout
    const kbPanelWidth = 75;   // Compact width
    const gpPanelY = screen.height - kbPanelHeight - 4; // Very bottom, small margin
    const gpCharWidth = 4;
    const gpLineHeight = 9;
    const gpPadding = 4;
    
    const gpIndices = Object.keys(connectedGamepads).map(k => parseInt(k));
    
    if (gpIndices.length === 0) {
      // No gamepad - show keyboard controls instead
      ink(0, 0, 0, 180).box(gpPanelX, gpPanelY, kbPanelWidth, kbPanelHeight);
      ink(0, 200, 200, 255)
        .line(gpPanelX, gpPanelY, gpPanelX + kbPanelWidth, gpPanelY)
        .line(gpPanelX + kbPanelWidth, gpPanelY, gpPanelX + kbPanelWidth, gpPanelY + kbPanelHeight)
        .line(gpPanelX + kbPanelWidth, gpPanelY + kbPanelHeight, gpPanelX, gpPanelY + kbPanelHeight)
        .line(gpPanelX, gpPanelY + kbPanelHeight, gpPanelX, gpPanelY);
      drawKeyboardControls(ink, gpPanelX + gpPadding, gpPanelY + gpPadding, pressedKeys);
    } else {
      // Draw gamepad info for each connected controller
      gpIndices.forEach((gpIndex, slotNum) => {
        const gp = connectedGamepads[gpIndex];
        if (!gp) return;
        
        const slotY = gpPanelY + slotNum * 120;
        const mapping = getGamepadMapping(gp.id || "standard");
        const is8BitDo = gp.id?.includes("8BitDo") || gp.id?.includes("2dc8");
        const controllerType = is8BitDo ? "8bitdo" : "standard";
        
        // Build content
        const contentLines = [];
        
        // Controller type
        let displayName = gp.id || "Unknown";
        if (displayName.includes("8BitDo Micro") || (displayName.includes("2dc8") && displayName.includes("9020"))) {
          displayName = "8BitDo Micro";
        } else if (displayName.includes("045e") && displayName.includes("0b13")) {
          displayName = "Xbox Controller";
        } else {
          displayName = displayName.slice(0, 24);
        }
        contentLines.push({ color: "cyan", text: displayName });
        
        // Pressed buttons with mapped actions
        if (gp.pressedButtons.length > 0) {
          const btnParts = gp.pressedButtons.map(bi => {
            const btnName = getButtonName(gp.id, bi);
            const actionKey = `${controllerType}_${bi}`;
            const action = FPS_ACTIONS.buttons[actionKey] || "";
            return action ? `${btnName}→${action}` : btnName;
          });
          contentLines.push({ color: "lime", text: `BTN: ${btnParts.join(" ")}` });
        } else {
          contentLines.push({ color: "gray", text: "BTN: (none)" });
        }
        
        // Active axes with mapped actions
        const activeAxes = Object.keys(gp.axes);
        if (activeAxes.length > 0) {
          activeAxes.forEach(ai => {
            const axisIndex = parseInt(ai);
            const value = parseFloat(gp.axes[ai]);
            const axisName = getAxisName(gp.id, axisIndex);
            const actionKey = `${controllerType}_${axisIndex}`;
            const axisAction = FPS_ACTIONS.axes[actionKey];
            let actionStr = "";
            if (axisAction) {
              actionStr = value < 0 ? `→${axisAction.neg}` : `→${axisAction.pos}`;
            }
            
            // Visual bar for axis value
            const barWidth = 20;
            const barFill = Math.abs(value) * barWidth;
            contentLines.push({ 
              color: value < 0 ? "yellow" : "orange", 
              text: `${axisName}: ${value > 0 ? "+" : ""}${value}${actionStr}`,
              axisValue: value
            });
          });
        } else {
          contentLines.push({ color: "gray", text: "AXES: (centered)" });
        }
        
        // Calculate panel size - include space for mini diagram
        const diagramHeight = is8BitDo ? 24 : 28;
        const maxTextWidth = Math.max(...contentLines.map(l => l.text.length)) * gpCharWidth;
        const gpPanelWidth = Math.max(160, maxTextWidth + gpPadding * 2);
        const gpPanelHeight = gpPadding * 2 + contentLines.length * gpLineHeight + gpLineHeight + diagramHeight + 4; // +title + diagram
        
        // Draw panel background
        ink(0, 0, 0, 200).box(gpPanelX, slotY, gpPanelWidth, gpPanelHeight);
        
        // Panel border (cyan for gamepad theme)
        ink(0, 200, 200, 255)
          .line(gpPanelX, slotY, gpPanelX + gpPanelWidth, slotY)
          .line(gpPanelX + gpPanelWidth, slotY, gpPanelX + gpPanelWidth, slotY + gpPanelHeight)
          .line(gpPanelX + gpPanelWidth, slotY + gpPanelHeight, gpPanelX, slotY + gpPanelHeight)
          .line(gpPanelX, slotY + gpPanelHeight, gpPanelX, slotY);
        
        // Title
        let gpTextY = slotY + gpPadding;
        ink("white").write(`🎮 P${gpIndex + 1}`, { x: gpPanelX + gpPadding, y: gpTextY }, undefined, undefined, false, hudFont);
        gpTextY += gpLineHeight;
        
        // Mini controller diagram
        const diagramX = gpPanelX + gpPadding;
        drawMiniControllerDiagram(ink, gp, diagramX, gpTextY);
        gpTextY += diagramHeight + 4;
        
        // Content
        contentLines.forEach(({ color, text, axisValue }) => {
          ink(color).write(text, { x: gpPanelX + gpPadding, y: gpTextY }, undefined, undefined, false, hudFont);
          
          // Draw axis visual bar if present
          if (axisValue !== undefined) {
            const barX = gpPanelX + gpPanelWidth - 30;
            const barY = gpTextY + 1;
            const barWidth = 24;
            const barHeight = 6;
            const centerX = barX + barWidth / 2;
            
            // Background bar
            ink(40, 40, 40).box(barX, barY, barWidth, barHeight);
            
            // Value indicator
            const fillWidth = Math.abs(axisValue) * (barWidth / 2);
            if (axisValue < 0) {
              ink("yellow").box(centerX - fillWidth, barY, fillWidth, barHeight);
            } else {
              ink("orange").box(centerX, barY, fillWidth, barHeight);
            }
            
            // Center line
            ink("white").line(centerX, barY, centerX, barY + barHeight);
          }
          
          gpTextY += gpLineHeight;
        });
      });
    }
  }
  
  // Network status is now in top left - this section removed
  
  // 📱 Draw join QR code in bottom-right corner
  drawJoinQR({ ink, box: drawBox, write: (text, opts, ...rest) => ink(255).write(text, opts, ...rest), screen });
  
  // Note: Crosshair is now rendered via DOM element in bios.mjs when pointer lock is enabled
}

function act({ event: e, penLock, setShowClippedWireframes }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;
  if (!penLocked && e.is("touch")) penLock();
  
  // Shooting with left mouse button
  if (e.is("draw") && gameState === "playing") {
    const now = performance.now();
    if (now - self.lastShot >= SHOOT_COOLDOWN) {
      self.lastShot = now;
      shoot();
    }
  }
  
  // Toggle wireframe mode with 'V' key
  if (e.is("keyboard:down:v")) {
    showWireframes = !showWireframes;
    // Toggle clipped triangle wireframes if available
    if (setShowClippedWireframes) {
      setShowClippedWireframes(showWireframes);
    }
  }
  
  // Toggle debug panel with 'P' key
  if (e.is("keyboard:down:p")) {
    showDebugPanel = !showDebugPanel;
  }
  
  // Toggle gamepad panel with 'G' key
  if (e.is("keyboard:down:g")) {
    showGamepadPanel = !showGamepadPanel;
    console.log(`🎮 Gamepad panel: ${showGamepadPanel ? "ON" : "OFF"}`);
  }
  
  // Toggle join QR code with 'Q' key
  if (e.is("keyboard:down:q")) {
    showJoinQR = !showJoinQR;
    console.log(`📱 Join QR: ${showJoinQR ? "ON" : "OFF"}`);
  }
  
  // Log scene debug info with 'L' key
  if (e.is("keyboard:down:l")) {
    logSceneDebug();
  }
  
  // 🎯 Target tracking controls
  // Tab - cycle through targets (other players + bot)
  if (e.is("keyboard:down:Tab")) {
    cycleTarget();
  }
  
  // T - toggle auto-tracking (continuously face target)
  if (e.is("keyboard:down:t")) {
    toggleAutoTrack();
  }
  
  // F - snap to face current target instantly
  if (e.is("keyboard:down:f")) {
    if (currentTarget) {
      snapToTarget();
    } else {
      // If no target, select one first then snap
      cycleTarget();
      if (currentTarget) snapToTarget();
    }
  }
  
  // ⌨️ Track keyboard state for HUD display
  if (e.is("keyboard:down:w")) pressedKeys.w = true;
  if (e.is("keyboard:up:w")) pressedKeys.w = false;
  if (e.is("keyboard:down:a")) pressedKeys.a = true;
  if (e.is("keyboard:up:a")) pressedKeys.a = false;
  if (e.is("keyboard:down:s")) pressedKeys.s = true;
  if (e.is("keyboard:up:s")) pressedKeys.s = false;
  if (e.is("keyboard:down:d")) pressedKeys.d = true;
  if (e.is("keyboard:up:d")) pressedKeys.d = false;
  if (e.is("keyboard:down:ArrowUp")) pressedKeys.arrowup = true;
  if (e.is("keyboard:up:ArrowUp")) pressedKeys.arrowup = false;
  if (e.is("keyboard:down:ArrowDown")) pressedKeys.arrowdown = true;
  if (e.is("keyboard:up:ArrowDown")) pressedKeys.arrowdown = false;
  if (e.is("keyboard:down:ArrowLeft")) pressedKeys.arrowleft = true;
  if (e.is("keyboard:up:ArrowLeft")) pressedKeys.arrowleft = false;
  if (e.is("keyboard:down:ArrowRight")) pressedKeys.arrowright = true;
  if (e.is("keyboard:up:ArrowRight")) pressedKeys.arrowright = false;
  if (e.is("keyboard:down: ")) pressedKeys.space = true;
  if (e.is("keyboard:up: ")) pressedKeys.space = false;
  if (e.is("keyboard:down:Shift")) pressedKeys.shift = true;
  if (e.is("keyboard:up:Shift")) pressedKeys.shift = false;
  if (e.is("draw")) pressedKeys.lmb = true;
  if (e.is("lift")) pressedKeys.lmb = false;
  
  // 🎮 Track gamepad state for HUD display
  if (e.is("gamepad")) {
    const gpIndex = e.gamepad;
    
    // Initialize gamepad entry if needed
    if (!connectedGamepads[gpIndex]) {
      connectedGamepads[gpIndex] = {
        id: e.gamepadId || null,
        pressedButtons: [],
        axes: {},
        lastEvent: null,
      };
      console.log(`🎮 [1v1] Gamepad ${gpIndex} connected:`, e.gamepadId);
      // Auto-show panel when gamepad connects
      showGamepadPanel = true;
    }
    
    const gp = connectedGamepads[gpIndex];
    
    // Update gamepad ID if available
    if (e.gamepadId && !gp.id) {
      gp.id = e.gamepadId;
    }
    
    // Track last event
    gp.lastEvent = e.name;
    
    // Track button state
    if (e.button !== undefined) {
      const buttonIndex = e.button;
      if (e.action === "push") {
        if (!gp.pressedButtons.includes(buttonIndex)) {
          gp.pressedButtons.push(buttonIndex);
        }
      } else if (e.action === "release") {
        gp.pressedButtons = gp.pressedButtons.filter(b => b !== buttonIndex);
      }
    }
    
    // Track axis state
    if (e.axis !== undefined) {
      const axisIndex = e.axis;
      const value = e.value;
      
      if (Math.abs(value) > 0.1) {
        gp.axes[axisIndex] = value.toFixed(2);
      } else {
        delete gp.axes[axisIndex];
      }
    }
  }
}

// Raycast shooting - check if we hit another player or the bot
function shoot() {
  if (!graphInstance) return;
  
  console.log("🔫 Shooting!");
  if (server) {
    server.send("1v1:shoot", { timestamp: performance.now() });
  }
  
  // Simple raycast from camera forward
  const camPos = { x: graphInstance.x, y: graphInstance.y, z: graphInstance.z };
  const camRot = { x: graphInstance.rotX, y: graphInstance.rotY, z: graphInstance.rotZ };
  
  // Convert rotation to direction vector (simplified)
  const yawRad = (camRot.y * Math.PI) / 180;
  const pitchRad = (camRot.x * Math.PI) / 180;
  
  const dir = {
    x: Math.cos(pitchRad) * Math.sin(yawRad),
    y: -Math.sin(pitchRad),
    z: Math.cos(pitchRad) * Math.cos(yawRad),
  };
  
  // Check collision with other players (simple sphere test)
  const MAX_RANGE = 50;
  let hitPlayerId = null;
  let hitBot = false;
  let minDist = MAX_RANGE;
  
  // Check hit against bot first
  if (bot.enabled && bot.state !== "dead") {
    const dx = bot.pos.x - camPos.x;
    const dy = bot.pos.y - camPos.y;
    const dz = bot.pos.z - camPos.z;
    const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
    
    if (dist <= MAX_RANGE) {
      const dot = dx * dir.x + dy * dir.y + dz * dir.z;
      if (dot > 0) {
        const hitRadius = 0.5;
        const perpDist = Math.sqrt(dx * dx + dy * dy + dz * dz - dot * dot);
        
        if (perpDist < hitRadius && dist < minDist) {
          minDist = dist;
          hitBot = true;
          hitPlayerId = null;
        }
      }
    }
  }
  
  // Check hit against other players
  for (const [id, other] of Object.entries(others)) {
    const dx = other.pos.x - camPos.x;
    const dy = other.pos.y - camPos.y;
    const dz = other.pos.z - camPos.z;
    const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
    
    if (dist > MAX_RANGE) continue;
    
    // Check if player is in front of camera (dot product)
    const dot = dx * dir.x + dy * dir.y + dz * dir.z;
    if (dot <= 0) continue; // Behind us
    
    // Simple sphere collision (player radius ~0.5)
    const hitRadius = 0.5;
    const perpDist = Math.sqrt(dx * dx + dy * dy + dz * dz - dot * dot);
    
    if (perpDist < hitRadius && dist < minDist) {
      minDist = dist;
      hitPlayerId = id;
      hitBot = false;
    }
  }
  
  // Handle bot hit
  if (hitBot) {
    console.log("🤖💥 HIT BOT!");
    bot.health -= DAMAGE_PER_HIT;
    if (bot.health <= 0) {
      bot.state = "dead";
      bot.respawnTimer = 180; // 3 seconds at 60fps
      self.kills++;
      console.log("🤖💀 Bot killed! Kills:", self.kills);
    }
  }
  
  // Handle player hit
  if (hitPlayerId && server) {
    console.log("💥 HIT player:", hitPlayerId);
    server.send("1v1:hit", {
      targetId: hitPlayerId,
      damage: DAMAGE_PER_HIT,
    });
  }
}

// Load the latest painting from the TV endpoint and apply it to the textured quad
async function loadLatestPaintingTexture(get) {
  if (!texturedQuad || !get) {
    console.warn("Cannot load texture: missing quad or get API");
    return;
  }

  try {
    console.log("🖼️ Fetching from TV endpoint...");
    const response = await fetch("/api/tv?types=painting&limit=1");
    if (!response.ok) {
      console.warn("TV endpoint returned", response.status);
      return;
    }
    
    const payload = await response.json();
    console.log("🖼️ TV payload:", payload);
    const latestPainting = payload?.media?.paintings?.[0];
    
    if (!latestPainting) {
      console.warn("No painting found in TV feed");
      return;
    }

    const paintingSlug = latestPainting.slug;
    const paintingHandle = latestPainting.owner?.handle?.replace(/^@/, "") || "anon";
    
    console.log("🖼️ Loading painting:", paintingSlug, "by", paintingHandle);
    
    // Use get.painting() like painting.mjs and profile.mjs do
    const got = await get.painting(paintingSlug).by(paintingHandle);
    if (!got?.img) {
      console.warn("Failed to load painting image");
      return;
    }

    // Replace the placeholder texture with the actual painting
    quadTexture = got.img;
    texturedQuad.texture = got.img;
    paintingTextureLoaded = true;
    
    console.log("🖼️ Successfully loaded painting texture:", got.img.width, "x", got.img.height);
  } catch (error) {
    console.error("Failed to load latest painting texture:", error);
  } finally {
    paintingTextureFetchPromise = null;
  }
}

export const system = "fps";
export { boot, sim, paint, act };
