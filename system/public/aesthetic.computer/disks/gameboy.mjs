// GameBoy emulator display piece

// GameBoy button state
let gameboyButtons = {};

// Track previous state to avoid redundant sends
let previousButtonState = {};

// UI Buttons for GameBoy controls
let uiButtons = {};
let inputInterval = null; // For sustaining input while buttons are held

// Track reframe state
let needsReframe = false;

function createGameBoyButtons({ screen, ui }) {
  const buttonSize = 24; // Smaller buttons for more screen space
  const padding = 8;
  
  // D-Pad (flush with bottom left corner)
  const dpadX = 0; // Flush to left edge
  const dpadY = screen.height - buttonSize * 3; // Flush to bottom edge
  
  const upBtn = new ui.Button(dpadX + buttonSize, dpadY, buttonSize, buttonSize);
  upBtn.id = "gameboy-up";
  uiButtons.up = upBtn;
  
  const downBtn = new ui.Button(dpadX + buttonSize, dpadY + buttonSize * 2, buttonSize, buttonSize);
  downBtn.id = "gameboy-down";
  uiButtons.down = downBtn;
  
  const leftBtn = new ui.Button(dpadX, dpadY + buttonSize, buttonSize, buttonSize);
  leftBtn.id = "gameboy-left";
  uiButtons.left = leftBtn;
  
  const rightBtn = new ui.Button(dpadX + buttonSize * 2, dpadY + buttonSize, buttonSize, buttonSize);
  rightBtn.id = "gameboy-right";
  uiButtons.right = rightBtn;
  
  // A/B buttons together (flush to bottom right corner)
  const actionX = screen.width - buttonSize * 2; // No padding from right edge
  const actionY = screen.height - buttonSize; // No padding from bottom edge
  
  const bBtn = new ui.Button(actionX, actionY, buttonSize, buttonSize);
  bBtn.id = "gameboy-b";
  uiButtons.b = bBtn;
  
  const aBtn = new ui.Button(actionX + buttonSize, actionY, buttonSize, buttonSize); // Flush next to B
  aBtn.id = "gameboy-a";
  uiButtons.a = aBtn;
  
  // Start/Select (top right corner, flush, shorter rectangles)
  const selectStartWidth = buttonSize * 1.5; // Wider to fit full text
  const selectStartHeight = buttonSize * 0.6; // Shorter height
  const topRightX = screen.width - selectStartWidth * 2; // Flush to right edge
  const topRightY = 0; // Flush to top edge
  
  const selectBtn = new ui.Button(topRightX, topRightY, selectStartWidth, selectStartHeight);
  selectBtn.id = "gameboy-select";
  uiButtons.select = selectBtn;
  
  const startBtn = new ui.Button(topRightX + selectStartWidth, topRightY, selectStartWidth, selectStartHeight);
  startBtn.id = "gameboy-start";
  uiButtons.start = startBtn;
}

function sendGameBoyInput(send) {
  const joypadState = {
    up: gameboyButtons.up || false,
    right: gameboyButtons.right || false, 
    down: gameboyButtons.down || false,
    left: gameboyButtons.left || false,
    a: gameboyButtons.a || false,
    b: gameboyButtons.b || false,
    select: gameboyButtons.select || false,
    start: gameboyButtons.start || false
  };
  
  send({
    type: "gameboy:input",
    content: joypadState
  });
}

export async function boot({ ui, screen, send, params, debug }) { 
  // GameBoy piece loaded - emulator managed by bios.mjs
  // Reset all button states
  gameboyButtons = {};
  previousButtonState = {};
  
  // Create buttons immediately in boot
  createGameBoyButtons({ screen, ui });
  
  // Auto-load ROM if specified in params
  if (params && params[0]) {
    const romName = params[0];
    
    // Construct path based on environment
    const basePath = debug 
      ? "/aesthetic.computer/gb-emulator" 
      : "https://aesthetic.computer/gb-emulator";
    
    // Add cache-busting parameter in debug mode
    const cacheBuster = debug ? `?t=${Date.now()}` : "";
    const romUrl = `${basePath}/${romName}.gb${cacheBuster}`;
    
    try {
      console.log("🎮 gameboy: Loading ROM from:", romUrl);
      
      // Fetch the ROM file
      const response = await fetch(romUrl);
      if (!response.ok) {
        throw new Error(`Failed to load ROM: ${response.status} ${response.statusText}`);
      }
      
      const romData = await response.arrayBuffer();
      console.log("🎮 gameboy: ROM loaded:", romData.byteLength, "bytes");
      
      // Create romData object matching bios.mjs format
      const romDataObj = {
        name: romName,
        originalName: `${romName}.gb`,
        romData: romData,
        isGameBoyColor: false
      };
      
      // Send load command to bios
      send({
        type: "gameboy:load-rom",
        content: romDataObj
      });
      
      console.log("🎮 gameboy: ROM load command sent to BIOS");
      
    } catch (error) {
      console.error("🎮 gameboy: Failed to load ROM:", error);
    }
  }
}

// Called when leaving this piece
export function leave({ send }) {
  // Clear all button states and send final "all released" state
  gameboyButtons = {};
  previousButtonState = {};
  
  // Send all-buttons-released state to emulator
  send({
    type: "gameboy:input",
    content: {
      up: false,
      right: false,
      down: false,
      left: false,
      a: false,
      b: false,
      select: false,
      start: false
    }
  });
  
  // The bios.mjs handles pausing the emulator based on piece changes
  if (inputInterval) {
    clearInterval(inputInterval);
    inputInterval = null;
  }
}

export function paint({ ink, wipe, screen, paste, sound, num, hud, ui, send }) {
  // Send joypad state every frame to maintain button holds
  // WasmBoy needs continuous updates to keep buttons held
  const hasButtonsPressed = Object.values(gameboyButtons).some(v => v);
  if (hasButtonsPressed) {
    sendGameBoyInput(send);
  }
  
  // Show GameBoy emulator label with metadata and color coding
  const gameboy = sound?.gameboy;
  let label = "gameboy";
  
  if (gameboy) {
    // Check for custom label override (e.g., from slgb piece)
    if (gameboy.customLabel) {
      label = gameboy.customLabel;
    } else {
      // Use title if available, otherwise fall back to ROM name
      const gameName = gameboy.title || gameboy.romName || "unknown";
      
      // Clean up the name (remove file extension and extra characters)
      const cleanName = gameName
        .replace(/\.(gb|gbc)$/i, "") // Remove file extensions
        .replace(/\s*\([^)]*\)/g, "") // Remove parentheses content like (JU) (V1.1) [S][!]
        .replace(/\s*\[[^\]]*\]/g, "") // Remove bracket content
        .trim();
      
      // Add color coding based on Game Boy Color support
      if (gameboy.isGameBoyColor) {
        label = `gameboy \\yellow\\${cleanName}`;
      } else {
        label = `gameboy \\lime\\${cleanName}`;
      }
    }
  }
  
  hud.label(label);
  
  // Clear screen on reframe to avoid artifacts
  if (needsReframe) {
    wipe();
    needsReframe = false; // Reset flag
  }
  
  // Check for frame data via sound.gameboy API
  if (sound?.gameboy?.frame?.length === 92160) {
    const framePixels = sound.gameboy.frame;
    
    // Game Boy screen is 160x144 pixels
    const gbWidth = 160;
    const gbHeight = 144;
    
    // Calculate safe area for GameBoy display with smaller button layout
    const buttonSize = 24; // Smaller buttons
    const padding = 8;
    const hudHeight = 40;
    const selectStartHeight = buttonSize * 0.6; // Shorter SELECT/START buttons
    const selectStartWidth = buttonSize * 1.5; // Wider to fit full text
    
    // Fixed button layout constraints with smaller buttons:
    // - D-Pad flush to bottom left: 3*buttonSize from bottom, 3*buttonSize from left
    // - A/B flush to bottom right: buttonSize from bottom, 2*buttonSize from right
    // - START/SELECT flush to top right: selectStartHeight from top, selectStartWidth*2 from right
    // - HUD at top: hudHeight from top
    
    const topMargin = Math.max(hudHeight, selectStartHeight + padding); // HUD vs START/SELECT area
    const bottomMargin = buttonSize + padding; // A/B buttons height (single row) + padding
    const leftMargin = buttonSize * 3 + padding; // D-Pad width (flush) + padding
    const rightMargin = Math.max(buttonSize * 2, selectStartWidth * 2) + padding; // A/B buttons or START/SELECT width
    
    // Available safe area for GameBoy display
    const safeWidth = screen.width - leftMargin - rightMargin;
    const safeHeight = screen.height - topMargin - bottomMargin;
    
    // Calculate integer scaling that fits in safe area
    const scaleX = Math.floor(safeWidth / gbWidth);
    const scaleY = Math.floor(safeHeight / gbHeight);
    let scale = Math.min(scaleX, scaleY);
    
    // Ensure minimum scale of 1x
    scale = Math.max(1, scale);
    
    const scaledWidth = gbWidth * scale;
    const scaledHeight = gbHeight * scale;
    
    // Center GameBoy screen in available safe area
    const safeX = leftMargin;
    const safeY = topMargin;
    const startX = safeX + Math.floor((safeWidth - scaledWidth) / 2);
    const startY = safeY + Math.floor((safeHeight - scaledHeight) / 2);
    
    // Clear background black every frame
    wipe("black");
    
    // Create an AC-compatible bitmap object for paste()
    const gameboyBitmap = {
      width: gbWidth,
      height: gbHeight,
      pixels: framePixels
    };
    
    // Use paste() with integer scaling
    paste(gameboyBitmap, startX, startY, scale);
    
    // Draw GameBoy control buttons with gray backdrops
    drawGameBoyButtons({ ink, screen });
    
  } else {
    wipe("black");
  }
}

function drawGameBoyButtons({ ink, screen }) {
  const buttonSize = 24; // Smaller buttons
  const padding = 8;
  
  // Add gray backdrops for button areas
  // D-Pad area backdrop (flush with bottom left corner)
  const dpadX = 0;
  const dpadY = screen.height - buttonSize * 3;
  ink("gray").box(dpadX - padding/2, dpadY - padding/2, buttonSize * 3 + padding, buttonSize * 3 + padding);
  
  // A/B buttons area backdrop (flush with bottom right corner)
  const actionX = screen.width - buttonSize * 2;
  const actionY = screen.height - buttonSize;
  ink("gray").box(actionX - padding/2, actionY - padding/2, buttonSize * 2 + padding, buttonSize + padding);
  
  // Start/Select area backdrop (flush with top right corner)
  const selectStartWidth = buttonSize * 1.5;
  const selectStartHeight = buttonSize * 0.6;
  const topRightX = screen.width - selectStartWidth * 2;
  const topRightY = 0;
  ink("gray").box(topRightX - padding/2, topRightY - padding/2, selectStartWidth * 2 + padding, selectStartHeight + padding);
  
  // D-Pad (flush with bottom left corner) - Blue color scheme
  ink(gameboyButtons.up ? "lightblue" : "lightgray").box(dpadX + buttonSize, dpadY, buttonSize, buttonSize);
  ink(gameboyButtons.down ? "lightblue" : "lightgray").box(dpadX + buttonSize, dpadY + buttonSize * 2, buttonSize, buttonSize);
  ink(gameboyButtons.left ? "lightblue" : "lightgray").box(dpadX, dpadY + buttonSize, buttonSize, buttonSize);
  ink(gameboyButtons.right ? "lightblue" : "lightgray").box(dpadX + buttonSize * 2, dpadY + buttonSize, buttonSize, buttonSize);
  
  // A/B buttons together (flush to bottom right corner) - Red/Green
  ink(gameboyButtons.b ? "lightcoral" : "coral").box(actionX, actionY, buttonSize, buttonSize);
  ink(gameboyButtons.a ? "lightgreen" : "lime").box(actionX + buttonSize, actionY, buttonSize, buttonSize); // Flush next to B
  
  // Start/Select (top right corner, flush, shorter rectangles) - Yellow/Orange color scheme
  ink(gameboyButtons.select ? "lightyellow" : "yellow").box(topRightX, topRightY, selectStartWidth, selectStartHeight);
  ink(gameboyButtons.start ? "lightcoral" : "orange").box(topRightX + selectStartWidth, topRightY, selectStartWidth, selectStartHeight);
  
  // Labels with centered text using GNU Unifont for arrows
  ink("black").write("↑", { x: dpadX + buttonSize + buttonSize/2, y: dpadY + buttonSize/2, center: "xy" }, undefined, undefined, false, "unifont");
  ink("black").write("↓", { x: dpadX + buttonSize + buttonSize/2, y: dpadY + buttonSize * 2 + buttonSize/2, center: "xy" }, undefined, undefined, false, "unifont");
  ink("black").write("←", { x: dpadX + buttonSize/2, y: dpadY + buttonSize + buttonSize/2, center: "xy" }, undefined, undefined, false, "unifont");
  ink("black").write("→", { x: dpadX + buttonSize * 2 + buttonSize/2, y: dpadY + buttonSize + buttonSize/2, center: "xy" }, undefined, undefined, false, "unifont");
  ink("black").write("B", { x: actionX + buttonSize/2, y: actionY + buttonSize/2, center: "xy" });
  ink("black").write("A", { x: actionX + buttonSize + buttonSize/2, y: actionY + buttonSize/2, center: "xy" });
  ink("black").write("SELECT", { x: topRightX + selectStartWidth/2, y: topRightY + selectStartHeight/2, center: "xy" });
  ink("black").write("START", { x: topRightX + selectStartWidth + selectStartWidth/2, y: topRightY + selectStartHeight/2, center: "xy" });
}

// Handle user input and button interactions
export function act({ event: e, send, pens, ui, screen }) {
  // Recreate buttons if screen size changed
  if (e.is("reframed")) {
    createGameBoyButtons({ screen, ui });
    needsReframe = true; // Flag that we need to clear background
  }

  // Handle UI button interactions - directly update state and send to emulator
  Object.keys(uiButtons).forEach(buttonName => {
    const button = uiButtons[buttonName];
    
    button.act(e, {
      down: (btn) => {
        gameboyButtons[buttonName] = true;
        sendGameBoyInput(send);
      },
      up: (btn) => {
        gameboyButtons[buttonName] = false;
        sendGameBoyInput(send);
      },
      cancel: (btn) => {
        gameboyButtons[buttonName] = false;
        sendGameBoyInput(send);
      },
      out: (btn) => {
        gameboyButtons[buttonName] = false;
        sendGameBoyInput(send);
      }
    }, pens?.());
  });

  // Also handle keyboard input
  if (e.is("keyboard:down")) {
    const keyMappings = {
      "ArrowUp": "up",
      "ArrowDown": "down", 
      "ArrowLeft": "left",
      "ArrowRight": "right",
      "KeyZ": "a",
      "KeyX": "b",
      "Space": "select",
      "Enter": "start"
    };
    
    const gamepadButton = keyMappings[e.key];
    if (gamepadButton) {
      gameboyButtons[gamepadButton] = true;
      sendGameBoyInput(send);
    }
  }

  if (e.is("keyboard:up")) {
    const keyMappings = {
      "ArrowUp": "up",
      "ArrowDown": "down",
      "ArrowLeft": "left", 
      "ArrowRight": "right",
      "KeyZ": "a",
      "KeyX": "b",
      "Space": "select",
      "Enter": "start"
    };
    
    const gamepadButton = keyMappings[e.key];
    if (gamepadButton) {
      gameboyButtons[gamepadButton] = false;
      sendGameBoyInput(send);
    }
  }
}
