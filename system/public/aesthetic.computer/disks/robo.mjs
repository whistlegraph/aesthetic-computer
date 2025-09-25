// Robot automation system for aesthetic.computer
// Automates brush drawing by loading and executing brushes with synthetic pen data

// Global robot state
const robotState = {
  active: false,
  state: "idle", // "idle", "painting", "lift"
  brushName: null,
  currentPath: null,
  pathQueue: [],
  completedPaths: [],
  frameCounter: 0,
  speed: 1.0,
  loadedBrush: null,
  painting: null,
  lastCompletedPath: null,
  allowMultipleLift: false // Flag to control duplicate lift calls
};

// Robot path generator - creates drawing paths
class RoboPathGenerator {
  constructor() {
    this.currentColorIndex = 0;
    this.colors = [];
  }

  // Generate rainbow colors using HSL
  generateRainbowColors(count) {
    const colors = [];
    for (let i = 0; i < count; i++) {
      const hue = (i / count) * 360;
      const hsl = `hsl(${hue}, 70%, 50%)`;
      // Convert HSL to RGB for nopaint system
      const rgb = this.hslToRgb(hue / 360, 0.7, 0.5);
      colors.push({
        r: Math.round(rgb[0] * 255),
        g: Math.round(rgb[1] * 255), 
        b: Math.round(rgb[2] * 255)
      });
    }
    return colors;
  }

  // HSL to RGB conversion
  hslToRgb(h, s, l) {
    let r, g, b;
    if (s === 0) {
      r = g = b = l;
    } else {
      const hue2rgb = (p, q, t) => {
        if (t < 0) t += 1;
        if (t > 1) t -= 1;
        if (t < 1/6) return p + (q - p) * 6 * t;
        if (t < 1/2) return q;
        if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
        return p;
      };
      const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
      const p = 2 * l - q;
      r = hue2rgb(p, q, h + 1/3);
      g = hue2rgb(p, q, h);
      b = hue2rgb(p, q, h - 1/3);
    }
    return [r, g, b];
  }

  // Get next color in sequence
  getNextColor() {
    if (this.colors.length === 0) return { r: 255, g: 255, b: 255 };
    const color = this.colors[this.currentColorIndex];
    this.currentColorIndex = (this.currentColorIndex + 1) % this.colors.length;
    return color;
  }

  // Generate grid pattern paths
  generateGrid(width, height, gridCols, gridRows, count, spacing = 40) {
    console.log(`🤖 Generating grid: ${gridCols}x${gridRows} on ${width}x${height} screen`);
    
    // Adjust spacing based on screen size - use smaller spacing for small screens
    const adaptiveSpacing = Math.min(spacing, Math.min(width, height) / 10);
    console.log(`🤖 Adaptive spacing: ${adaptiveSpacing} (original: ${spacing})`);
    
    // Generate colors for the count
    this.colors = this.generateRainbowColors(count);
    console.log(`🤖 Generated ${this.colors.length} rainbow colors`);
    
    const paths = [];
    
    // Calculate available space and ensure positive dimensions
    let finalCols = gridCols;
    let finalRows = gridRows;
    
    // Try original grid size first
    let availableWidth = width - (adaptiveSpacing * (finalCols + 1));
    let availableHeight = height - (adaptiveSpacing * (finalRows + 1));
    
    let cellWidth = Math.max(10, Math.floor(availableWidth / finalCols)); 
    let cellHeight = Math.max(10, Math.floor(availableHeight / finalRows)); 
    
    console.log(`🤖 Initial available space: ${availableWidth}x${availableHeight}`);
    console.log(`🤖 Initial cell size: ${cellWidth}x${cellHeight}, spacing: ${adaptiveSpacing}`);
    
    // If cells are too small, reduce grid size
    if (cellWidth < 10 || cellHeight < 10) {
      console.log(`🤖 Cells too small, falling back to 2x2 grid`);
      finalCols = 2;
      finalRows = 2;
      
      // Recalculate with reduced grid
      availableWidth = width - (adaptiveSpacing * (finalCols + 1));
      availableHeight = height - (adaptiveSpacing * (finalRows + 1));
      cellWidth = Math.max(10, Math.floor(availableWidth / finalCols));
      cellHeight = Math.max(10, Math.floor(availableHeight / finalRows));
      console.log(`🤖 Fallback available space: ${availableWidth}x${availableHeight}`);
      console.log(`🤖 Fallback cell size: ${cellWidth}x${cellHeight}`);
    }
    
    // Use the calculated values as final
    const finalCellWidth = cellWidth;
    const finalCellHeight = cellHeight;
    
    // Center the grid on screen
    const totalWidth = finalCols * finalCellWidth + (finalCols - 1) * adaptiveSpacing;
    const totalHeight = finalRows * finalCellHeight + (finalRows - 1) * adaptiveSpacing;
    const startX = Math.max(adaptiveSpacing, (width - totalWidth) / 2);
    const startY = Math.max(adaptiveSpacing, (height - totalHeight) / 2);
    
    console.log(`🤖 Final grid positioned at (${startX}, ${startY}) with total size ${totalWidth}x${totalHeight}`);
    console.log(`🤖 Screen bounds check: startX+totalWidth=${startX + totalWidth} <= width=${width}, startY+totalHeight=${startY + totalHeight} <= height=${height}`);
    
    const availablePositions = [];
    for (let row = 0; row < finalRows; row++) {
      for (let col = 0; col < finalCols; col++) {
        const x = startX + col * (finalCellWidth + adaptiveSpacing);
        const y = startY + row * (finalCellHeight + adaptiveSpacing);
        
        // Ensure positions are within screen bounds
        if (x >= 0 && y >= 0 && x + finalCellWidth <= width && y + finalCellHeight <= height) {
          availablePositions.push({ 
            x: Math.round(x), 
            y: Math.round(y), 
            w: Math.round(finalCellWidth), 
            h: Math.round(finalCellHeight) 
          });
          console.log(`🤖 Valid position: (${Math.round(x)}, ${Math.round(y)}) size ${Math.round(finalCellWidth)}x${Math.round(finalCellHeight)}`);
        } else {
          console.log(`🤖 Rejected position: (${x}, ${y}) size ${finalCellWidth}x${finalCellHeight} - out of bounds`);
        }
      }
    }
    
    console.log(`🤖 Generated ${availablePositions.length} valid positions within screen bounds`);
    
    // Shuffle positions for random placement
    for (let i = availablePositions.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [availablePositions[i], availablePositions[j]] = [availablePositions[j], availablePositions[i]];
    }
    
    // Create paths for the requested count
    for (let i = 0; i < Math.min(count, availablePositions.length); i++) {
      const pos = availablePositions[i];
      const color = this.getNextColor();
      
      console.log(`🤖 Box ${i + 1}: position (${pos.x}, ${pos.y}) size ${pos.w}x${pos.h} color rgb(${color.r}, ${color.g}, ${color.b})`);
      
      paths.push({
        startPoint: { x: pos.x, y: pos.y },
        endPoint: { x: pos.x + pos.w - 1, y: pos.y + pos.h - 1 }, // Keep within bounds
        boxDimensions: { x: pos.x, y: pos.y, w: pos.w, h: pos.h }, // Store complete box info
        color: color,
        duration: 120 // frames at 120fps = 1 second per box
      });
    }
    
    console.log(`🤖 Generated ${paths.length} total paths`);
    return paths;
  }
}

// Parse robot command parameters
function parseRoboParams(params) {
  const defaults = {
    speed: 1.0,
    grid: "3x3", 
    pattern: "random",
    count: 5
  };
  
  const parsed = { ...defaults };
  
  for (const param of params) {
    if (param.includes(":")) {
      const [key, value] = param.split(":");
      switch (key.toLowerCase()) {
        case "speed":
          parsed.speed = parseFloat(value) || defaults.speed;
          break;
        case "grid":
          parsed.grid = value || defaults.grid;
          break;
        case "pattern":
          parsed.pattern = value || defaults.pattern;
          break;
        case "count":
          parsed.count = parseInt(value) || defaults.count;
          break;
      }
    }
  }
  
  return parsed;
}

// Convert grid string to dimensions
function parseGrid(gridStr) {
  const parts = gridStr.split('x');
  if (parts.length !== 2) return { cols: 3, rows: 3 };
  
  const cols = parseInt(parts[0]) || 3;
  const rows = parseInt(parts[1]) || 3;
  return { cols, rows };
}

// Boot function - Initialize robot with brush and parameters  
async function boot($api) {
  const { params, screen, net } = $api;
  
  if (params.length === 0) {
    console.log("🤖 Usage: robo <brush> [options]");
    console.log("🤖 Options: speed:1.5, grid:3x3, pattern:random, count:5");
    return;
  }

  const brushName = params[0];
  const roboParams = parseRoboParams(params.slice(1));

  console.log(`🤖 Starting robot for brush: ${brushName}`);
  console.log(`🤖 Settings:`, roboParams);

  // Load the target brush internally for robot operations
  try {
    // Fix localhost URLs if needed (for CSP compliance)
    let url = net.pieces;
    if (url.includes('https://localhost')) {
      url = url.replace('https://localhost', 'https://aesthetic.computer');
    }
    // Add cache-busting parameter to force reload
    const cacheBuster = Date.now();
    const fullUrl = `${url}/${brushName}.mjs?v=${cacheBuster}`;
    console.log(`🤖 Loading brush from URL: ${fullUrl}`);
    const brush = await import(fullUrl);
    
    // Create a painting buffer for the robot (like test.mjs does)
    const painting = $api.painting(screen.width, screen.height, (p) => {
      p.wipe(255, 255, 255, 0); // Transparent background
    });
    
    // Set up system for the brush 
    $api.system = { painting };
    
    // Initialize the brush with the API (copy params so brush gets clean params)
    const brushApi = { ...$api, params: [...params.slice(1)] };
    brush?.boot?.(brushApi);
    
    console.log(`🤖 Screen dimensions: ${screen.width}x${screen.height}`);

    // Parse grid dimensions
    const { cols, rows } = parseGrid(roboParams.grid);
    console.log(`🤖 Grid setup: ${cols}x${rows}`);

    // Generate paths using the robot path generator
    const generator = new RoboPathGenerator();
    const paths = generator.generateGrid(
      screen.width, 
      screen.height, 
      cols, 
      rows, 
      1  // Force only 1 path for easier debugging
    );

    if (paths.length === 0) {
      console.log("🤖 No paths generated");
      return;
    }

    console.log(`🤖 Generated ${paths.length} path for execution (forced to 1 for debugging)`);

    // Initialize robot state (modify properties instead of reassigning const)
    robotState.active = true;
    robotState.state = "idle"; // Start in idle state
    robotState.brushName = brushName;
    robotState.pathQueue = [...paths];
    robotState.currentPath = null;
    robotState.completedPaths = [];
    robotState.frameCounter = 0;
    robotState.speed = roboParams.speed;
    robotState.loadedBrush = brush;
    robotState.painting = painting;
    robotState.lastCompletedPath = null;
    robotState.allowMultipleLift = false;

    console.log("🤖 Robot initialized and ready for execution");
    
  } catch (error) {
    console.error(`🤖 Failed to load brush ${brushName}:`, error);
  }
}

// Paint function - execute the loaded brush with robot-generated pen data
function paint($api) {
  if (!robotState.active || !robotState.loadedBrush) return;
  
  // Don't override the existing system - just ensure painting is set
  if (!$api.system.painting) {
    $api.system.painting = robotState.painting;
  }
  
  // The robot doesn't directly call brush paint - it sends events through the robo API
  // which then get processed by the nopaint system
  console.log(`🤖 Paint called - robot state: ${robotState.state}`);
  
  // Monitor nopaint state processing
  if ($api.system?.nopaint) {
    const np = $api.system.nopaint;
    console.log("🤖 Paint: nopaint state check:", {
      needsPresent: np.needsPresent,
      needsBake: np.needsBake,
      bufferExists: !!np.buffer,
      robotActive: robotState.active,
      robotState: robotState.state
    });
    
    // If needsPresent is true, log that we're expecting a present call
    if (np.needsPresent) {
      console.log("🔍 NOPAINT PRESENT FLAG IS TRUE - expecting present() to be called soon");
    }
    
    // Return true to force continuous rendering when nopaint has pending updates
    if (np.needsPresent || np.needsBake) {
      return true;
    }
  }
  
  // Note: Display is handled by nopaint system, not by manual paste
  // The nopaint system will handle presenting the final result after baking
}

// Sim function - 120fps locked timing for robot execution
function sim($api) {
  if (!robotState.active || !robotState.loadedBrush) return;
  
  const { simCount, needsPaint, system } = $api;
  
  // Force continuous rendering if nopaint has pending display updates
  if (system?.nopaint && (system.nopaint.needsPresent || system.nopaint.needsBake)) {
    needsPaint();
  }
  
  // Speed control: advance robot logic every N sim frames
  const speedFrames = Math.max(1, Math.round(120 / (60 * robotState.speed)));
  
  if (simCount % BigInt(speedFrames) === 0n) {
    advanceRobotLogic($api);
    needsPaint(); // Request paint update when robot state changes
  }
}

// Advance robot logic - progress through paths
function advanceRobotLogic($api) {
  // Start new path if needed
  if (!robotState.currentPath && robotState.pathQueue.length > 0) {
    robotState.currentPath = robotState.pathQueue.shift();
    robotState.frameCounter = 0;
    robotState.state = "painting"; // Set state to painting when starting a path
    console.log(`🤖 Starting path ${robotState.completedPaths.length + 1} - state: painting`);
    
    // Send synthetic touch event through the robo API
    const path = robotState.currentPath;
    $api.robo.touch(path.startPoint.x, path.startPoint.y);
    console.log(`🤖 Sent touch event at (${path.startPoint.x}, ${path.startPoint.y})`);
  }

  // Execute current path
  if (robotState.currentPath) {
    robotState.frameCounter++;
    const progress = robotState.frameCounter / robotState.currentPath.duration;
    
    // Generate synthetic draw events during path execution
    if (progress < 1.0) {
      const path = robotState.currentPath;
      const currentX = path.startPoint.x + (path.endPoint.x - path.startPoint.x) * progress;
      const currentY = path.startPoint.y + (path.endPoint.y - path.startPoint.y) * progress;
      const prevX = robotState.frameCounter > 1 ? 
        path.startPoint.x + (path.endPoint.x - path.startPoint.x) * ((robotState.frameCounter - 1) / path.duration) : 
        currentX;
      const prevY = robotState.frameCounter > 1 ?
        path.startPoint.y + (path.endPoint.y - path.startPoint.y) * ((robotState.frameCounter - 1) / path.duration) :
        currentY;
      
      // Send draw event through robo API
      $api.robo.draw(currentX, currentY, prevX, prevY);
    }
    
    if (progress >= 1.0) {
      // Path complete - store path data for lift function and send lift event
      const path = robotState.currentPath;
      robotState.state = "lift"; // Set state to lift when path completes
      robotState.allowMultipleLift = true; // Allow lift to be called once
      console.log(`🤖 Path completed, sending lift event through robo API - state: lift`);
      
      // Store the completed path data for the lift function to access
      robotState.lastCompletedPath = {
        boxDimensions: path.boxDimensions,
        color: path.color,
        endPoint: path.endPoint
      };
      
      console.log(`🤖 Path ${robotState.completedPaths.length + 1} completed`);
      
      // Store the completed path data
      const completedPath = robotState.currentPath;
      robotState.completedPaths.push(completedPath);
      
      // Clear current path but keep lift state until lift function is called
      robotState.currentPath = null;
      robotState.frameCounter = 0;
      // Note: Don't set to "idle" yet - let lift function handle state transition
      
      // Send lift event through robo API so nopaint system calls our lift function
      $api.robo.lift(path.endPoint.x, path.endPoint.y);
      
      // Check if all paths are done (but don't change state yet)
      if (robotState.pathQueue.length === 0) {
        console.log("🤖 All paths completed - waiting for lift to complete");
        robotState.active = false;
      } else {
        console.log(`🤖 Path completed, ${robotState.pathQueue.length} paths remaining - waiting for lift`);
      }
    }
  }
}

// Act function - handle user interactions (required by disk system)
function act($api) {
  const { event: e } = $api;
  
  // Monitor ALL events to debug synthetic robo events
  if (e) {
    console.log(`🔍 ACT EVENT: type="${e.type}" device="${e.device}" x=${e.x} y=${e.y} pressure=${e.pressure}`);
    
    // Special logging for robot events
    if (e.device === "robot") {
      console.log("🤖 ROBOT EVENT DETECTED:", {
        type: e.type,
        hasIs: typeof e.is === 'function',
        isLift: e.is ? e.is('lift:1') : 'no is function',
        nopaintState: $api.system?.nopaint ? {
          needsPresent: $api.system.nopaint.needsPresent,
          needsBake: $api.system.nopaint.needsBake
        } : 'no nopaint'
      });
    }
  }
  
  // Pass through to loaded brush if needed
  if (robotState.loadedBrush?.act) {
    try {
      robotState.loadedBrush.act($api);
    } catch (error) {
      console.error("🤖 Error calling brush act:", error);
    }
  }
}

// Brush delegation functions - called by nopaint system
function overlay(api) {
  console.log("🤖 ROBO OVERLAY ENTRY - API received:", {
    hasPen: !!api.pen,
    penCoords: api.pen ? {x: api.pen.x, y: api.pen.y} : null,
    hasInk: !!api.ink,
    hasColor: !!api.color,
    hasMark: !!api.mark,
    markFromNopaint: api.mark,
    hasScreen: !!api.screen,
    screenSize: api.screen ? {w: api.screen.width, h: api.screen.height} : null,
    hasSystem: !!api.system,
    nopaintSystem: api.system?.nopaint ? {
      state: api.system.nopaint.state,
      translation: api.system.nopaint.translation
    } : null,
    robotState: robotState.state
  });
  
  // If robot is in lift state, delegate to lift function instead
  if (robotState.state === "lift") {
    console.log("🤖 ROBO: Robot in lift state - delegating to lift function");
    return lift(api);
  }
  
  if (robotState.loadedBrush?.overlay) {
    console.log("📦 ROBO: Delegating to box overlay function");
    console.log("📦 ROBO: Loaded brush details:", {
      brushType: typeof robotState.loadedBrush,
      hasOverlay: !!robotState.loadedBrush.overlay,
      overlayType: typeof robotState.loadedBrush.overlay,
      overlayFunction: robotState.loadedBrush.overlay?.toString()?.substring(0, 100),
      hasLift: !!robotState.loadedBrush.lift,
      liftType: typeof robotState.loadedBrush.lift,
      liftFunction: robotState.loadedBrush.lift?.toString()?.substring(0, 100),
      brushKeys: Object.keys(robotState.loadedBrush || {})
    });
    
    // For animated drawing, create a progressive mark based on current path progress
    let progressiveMark = api.mark;
    
    console.log("📦 ROBO: Checking for progressive box calculation");
    console.log("📦 ROBO: Robot state for overlay:", {
      hasCurrentPath: !!robotState.currentPath,
      frameCounter: robotState.frameCounter,
      state: robotState.state,
      currentPathDuration: robotState.currentPath?.duration,
      hasPenDrawing: !!(api.pen && api.pen.drawing)
    });
    
    if (robotState.currentPath && robotState.state === "painting") {
      const path = robotState.currentPath;
      const progress = Math.min(1.0, robotState.frameCounter / path.duration);
      
      // Create a progressive box that grows during painting
      // Calculate current position based on robot's drawing progress
      const currentX = path.startPoint.x + (path.endPoint.x - path.startPoint.x) * progress;
      const currentY = path.startPoint.y + (path.endPoint.y - path.startPoint.y) * progress;
      
      // Progressive box that shows the drawing as it happens
      progressiveMark = {
        x: Math.min(path.boxDimensions.x, currentX),
        y: Math.min(path.boxDimensions.y, currentY), 
        w: Math.max(1, Math.abs(currentX - path.boxDimensions.x) + 1),
        h: Math.max(1, Math.abs(currentY - path.boxDimensions.y) + 1)
      };
      
      console.log(`📦 Progressive box at progress ${(progress * 100).toFixed(1)}%: currentPos(${currentX.toFixed(1)}, ${currentY.toFixed(1)}) box:`, progressiveMark);
    } else {
      console.log("📦 ROBO: No progressive box - using api.mark:", progressiveMark);
    }
    
    // Create proper context for the brush overlay function
    const pathColor = robotState.currentPath?.color;
    let colorArray;
    
    if (pathColor && typeof pathColor === 'object' && pathColor.r !== undefined) {
      // Convert {r, g, b} to [r, g, b] array
      colorArray = [pathColor.r, pathColor.g, pathColor.b];
    } else if (api.color && Array.isArray(api.color)) {
      colorArray = api.color;
    } else {
      colorArray = [255, 255, 255]; // White fallback
    }
    
    const brushApi = {
      ...api,
      ink: api.ink,
      color: colorArray,
      mark: progressiveMark || (robotState.currentPath ? robotState.currentPath.boxDimensions : null)
    };
    
    console.log("📦 ROBO: Calling box overlay with brushApi:", {
      mark: brushApi.mark,
      color: brushApi.color,
      hasPen: !!brushApi.pen,
      penCoords: brushApi.pen ? {x: brushApi.pen.x, y: brushApi.pen.y} : null,
      hasInk: !!brushApi.ink,
      originalApiMark: api.mark,
      progressiveMark: progressiveMark,
      currentPathBox: robotState.currentPath?.boxDimensions,
      pathColorOriginal: pathColor,
      colorArrayConverted: colorArray
    });
    
    console.log("📦 ROBO: About to call robotState.loadedBrush.overlay with brushApi:", {
      markPassed: brushApi.mark,
      colorPassed: brushApi.color,
      inkPassed: !!brushApi.ink
    });
    
    try {
      const result = robotState.loadedBrush.overlay(brushApi);
      console.log("📦 ROBO: Box overlay returned:", result);
      
      return result;
    } catch (error) {
      console.error("📦 ROBO: Error calling box overlay:", error);
      return null;
    }
  } else {
    console.warn("🤖 No overlay function in loaded brush");
  }
}

function lift(api) {
  console.log("🤖🤖🤖 ROBO LIFT FUNCTION CALLED! 🤖🤖🤖");
  console.log(`🤖 ROBO LIFT CALLED - current state: ${robotState.state} - this should trigger box drawing!`);
  
  // Prevent multiple lift calls when already in idle state (fix for duplicate calls)
  if (robotState.state === "idle" && !robotState.allowMultipleLift) {
    console.log("🤖 ROBO LIFT: Already in idle state, ignoring duplicate lift call");
    return;
  }
  
  console.log("🤖 Lift API context:", {
    hasInk: !!api.ink,
    inkType: typeof api.ink,
    hasColor: !!api.color,
    color: api.color,
    hasMark: !!api.mark,
    mark: api.mark,
    hasPen: !!api.pen,
    pen: api.pen,
    screenSize: { width: api.screen?.width, height: api.screen?.height },
    hasCurrentPath: !!robotState.currentPath,
    hasLoadedBrush: !!robotState.loadedBrush,
    hasLoadedBrushLift: !!robotState.loadedBrush?.lift
  });
  
  if (robotState.loadedBrush?.lift) {
    console.log("📦 ROBO: About to call box brush lift function");
    
    // Use lastCompletedPath data if available, otherwise use API data
    const lastCompleted = robotState.lastCompletedPath;
    let finalBox, finalColor;
    
    if (lastCompleted && lastCompleted.boxDimensions) {
      finalBox = lastCompleted.boxDimensions;
      finalColor = [lastCompleted.color.r, lastCompleted.color.g, lastCompleted.color.b]; // Convert to RGB array
      console.log("📦 ROBO: Using last completed path data:", { box: finalBox, color: finalColor });
    } else {
      // Fallback to API data
      finalBox = api.mark;
      finalColor = api.color || [255, 255, 255];
      console.log("📦 ROBO: Using API data (no completed path):", { box: finalBox, color: finalColor });
    }
    
    // Create proper context for the brush lift function
    const brushApi = {
      ...api,
      ink: api.ink,
      color: finalColor,
      mark: finalBox,
      system: api.system // Make sure to pass the system context to lift
    };
    
    console.log("📦 ROBO: Calling box brush lift with:", {
      hasInk: !!brushApi.ink,
      color: brushApi.color,
      mark: brushApi.mark,
      markIsValid: !!(brushApi.mark && brushApi.mark.x !== undefined && brushApi.mark.y !== undefined && brushApi.mark.w > 0 && brushApi.mark.h > 0)
    });
    
    console.log("📦 ROBO: About to call lift function:", {
      liftExists: !!robotState.loadedBrush.lift,
      liftType: typeof robotState.loadedBrush.lift,
      liftString: robotState.loadedBrush.lift?.toString()?.substring(0, 200)
    });
    
    try {
      // Call the box brush lift function - this should draw the final box
      const result = robotState.loadedBrush.lift(brushApi);
      console.log("📦 ROBO: Box brush lift completed, result:", result);
      
      // For box brush, always assume drawing happened since it's a simple drawing operation
      // Even if the lift function doesn't return true, we know it drew something
      console.log("🤖 ROBO: Lift function completed - forcing display update regardless of return value");
      
      // Force immediate display like manual drawing - bypass nopaint baking
      api.needsPaint();
      console.log("🤖 ROBO: Called needsPaint() after lift - bypassing nopaint baking like manual drawing");
      
      // Reset nopaint flags to prevent automatic baking
      if (api.system?.nopaint) {
        api.system.nopaint.needsBake = false;
        api.system.nopaint.needsPresent = false;
        console.log("🤖 ROBO: Disabled nopaint baking - robot will draw directly like manual");
      }
      
      // Now that lift is complete, transition state back to idle and check for next path
      robotState.state = "idle";
      robotState.allowMultipleLift = false; // Reset flag to prevent duplicate calls
      console.log("🤖 Lift completed - state set to idle");
      
      // If there are more paths in the queue, they will be started on the next sim cycle
      if (robotState.pathQueue.length > 0) {
        console.log(`🤖 Lift completed, ${robotState.pathQueue.length} paths remaining - will start next path`);
      } else {
        console.log("🤖 Lift completed, all paths finished");
      }
      
      return result;
    } catch (error) {
      console.error("📦 ROBO: Error calling box brush lift:", error);
      // Even on error, transition back to idle
      robotState.state = "idle";
      robotState.allowMultipleLift = false; // Reset flag on error too
      console.log("🤖 Lift error - state set to idle");
    }
  } else {
    console.warn("🤖 ROBO: No lift function in loaded brush!");
    // If no lift function, still transition back to idle
    robotState.state = "idle";
    robotState.allowMultipleLift = false; // Reset flag here too
    console.log("🤖 No lift function - state set to idle");
  }
}

// 🍪 Bake - Transfer drawn content to the final painting
function bake({ paste, system, page, needsPaint }) {
  console.log("🤖🍪 ROBO BAKE CALLED! 🍪🤖");
  console.log("🤖 Robo bake: Current nopaint state:", {
    hasBuffer: !!system.nopaint.buffer,
    needsPresent: system.nopaint.needsPresent,
    needsBake: system.nopaint.needsBake,
    bufferSize: system.nopaint.buffer ? `${system.nopaint.buffer.width}x${system.nopaint.buffer.height}` : "none"
  });
  
  if (system.nopaint.buffer) {
    console.log("🤖 BAKING: About to paste nopaint buffer to final painting");
    
    // Try direct paste to main painting buffer
    const result = paste(system.nopaint.buffer);
    console.log("🤖 BAKING: Paste result:", result);
    
    console.log("🤖 BAKING: Pasted buffer, now wiping nopaint buffer");
    page(system.nopaint.buffer).wipe(255, 255, 255, 0);
    console.log("🤖🍪 ROBO BAKE COMPLETED - Successfully transferred to final painting! 🍪🤖");
    
    // Reset nopaint flags to indicate baking is done
    system.nopaint.needsBake = false;
    system.nopaint.needsPresent = false;
    console.log("🤖 BAKING: Reset nopaint flags after baking");
    
    // Force immediate display update of the main painting
    needsPaint();
    console.log("🤖 BAKING: Called needsPaint() to display final result");
    
    // Signal that baking is complete and changes were made
    return true;
  } else {
    console.warn("🤖 Robo bake: No nopaint buffer to paste - nothing to bake");
    return false;
  }
}

export { boot, paint, sim, act, overlay, lift, bake };

// Export system type so disk.mjs recognizes this as a nopaint piece
export const system = "nopaint";

// Debug: Log that lift function is exported
console.log("🤖 ROBO MODULE: lift function exported:", typeof lift);