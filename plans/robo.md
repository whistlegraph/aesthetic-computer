# Robo System Plan

**Date:** 2025.09.16  
**Purpose:** Create an automated brush system that can robotically execute brushes in non-interactive mode

## Overview

The `robo` command will create an automated drawing system that can import and execute existing brush files (like `box.mjs`) in a programmatic, non-interactive mode. This creates a "plotter-like" system that can generate procedural art using the existing brush ecosystem.

## Core Concept

```
robo box               # Load box.mjs brush and run automatically
robo box fade:red-blue # Load box.mjs with color parameters
robo circle count:50   # Load circle.mjs and draw 50 circles
```

## Architecture

### 1. Robo Piece Structure (`robo.mjs`)

```javascript
// robo.mjs - Main robo system piece
function boot({ params, colon }) {
  const brushName = params[0];        // e.g., "box"
  const brushParams = params.slice(1); // e.g., ["fade:red-blue"]
  const robotSettings = parseColon(colon); // e.g., speed:2, count:10
}

async function paint({ frame }) {
  // Execute frame-by-frame robotic drawing
  if (loadedBrush && robotState.active) {
    executeRoboticFrame(frame);
  }
}
```

### 2. Dynamic Brush Loading

Based on research of `disk.mjs` loading mechanisms:

```javascript
// Inside robo.mjs boot function
async function loadBrush(brushName) {
  try {
    const brushModule = await import(`../disks/${brushName}.mjs`);
    return {
      overlay: brushModule.overlay,
      lift: brushModule.lift,
      brush: brushModule.brush, // Legacy support
      boot: brushModule.boot
    };
  } catch (err) {
    console.error(`Failed to load brush: ${brushName}`, err);
    return null;
  }
}
```

### 3. Path Generation System

The robo system needs to generate synthetic path data that mimics user interaction:

```javascript
class RoboPathGenerator {
  constructor() {
    this.currentPath = [];
    this.pathQueue = [];
  }

  // Generate geometric patterns
  generateGrid(rows, cols, spacing) {
    for (let row = 0; row < rows; row++) {
      for (let col = 0; col < cols; col++) {
        this.pathQueue.push(this.createBoxPath(
          col * spacing, 
          row * spacing, 
          spacing * 0.8, 
          spacing * 0.8
        ));
      }
    }
  }

  generateSpiral(centerX, centerY, radius, steps) {
    // Create spiral path
  }

  generateRandom(count, bounds) {
    // Create random placement paths
  }

  createBoxPath(x, y, w, h) {
    return {
      type: 'drag',
      startPoint: { x, y },
      endPoint: { x: x + w, y: y + h },
      duration: 60 // frames
    };
  }
}
```

### 4. Brush Execution Engine

Mock the nopaint system's brush execution:

```javascript
class RoboBrushExecutor {
  constructor(brushModule, screen) {
    this.brush = brushModule;
    this.screen = screen;
    this.mockSystem = this.createMockNopaintSystem();
  }

  createMockNopaintSystem() {
    return {
      nopaint: {
        color: [255, 0, 0, 255], // Default red
        brush: null,
        finalDragBox: null,
        buffer: null // Will be created
      }
    };
  }

  executePath(path, frame) {
    const progress = frame / path.duration;
    const currentBox = this.interpolateDragBox(path, progress);
    
    // Mock the overlay call during drawing
    if (progress < 1.0) {
      this.mockSystem.nopaint.brush = { dragBox: currentBox };
      this.callBrushFunction('overlay', {
        mark: currentBox,
        color: this.mockSystem.nopaint.color,
        ink: this.createMockInk()
      });
    }
    
    // Mock the lift call at completion
    if (progress >= 1.0) {
      this.mockSystem.nopaint.finalDragBox = currentBox;
      this.callBrushFunction('lift', {
        mark: currentBox,
        color: this.mockSystem.nopaint.color,
        ink: this.createMockInk()
      });
      return true; // Path complete
    }
    
    return false; // Path in progress
  }

  interpolateDragBox(path, progress) {
    const { startPoint, endPoint } = path;
    const currentEnd = {
      x: startPoint.x + (endPoint.x - startPoint.x) * progress,
      y: startPoint.y + (endPoint.y - startPoint.y) * progress
    };
    
    return {
      x: startPoint.x,
      y: startPoint.y,
      w: currentEnd.x - startPoint.x,
      h: currentEnd.y - startPoint.y
    };
  }

  callBrushFunction(functionName, api) {
    if (this.brush[functionName]) {
      this.brush[functionName](api);
    }
  }

  createMockInk() {
    // Return ink function that interfaces with the robo rendering system
    return (color) => ({
      box: (mark, mode = 'fill') => {
        this.renderBox(mark, color, mode);
        return this; // Chainable
      }
    });
  }
}
```

### 5. Robo Timing System with `sim` Function

Based on AC disk API research, use the `sim` function for precise timing control:

```javascript
// In robo.mjs - Main timing controller
let robotState = {
  active: false,
  currentPath: null,
  pathQueue: [],
  completedPaths: [],
  frameCounter: 0,
  speed: 1.0,
  pattern: 'grid',
  brushName: null
};

// 🧮 Sim (Runs once per logic frame at 120fps locked)
function sim({ simCount, needsPaint }) {
  if (!robotState.active) return;
  
  // Speed control: advance robot logic every N sim frames
  const speedFrames = Math.max(1, Math.round(120 / (60 * robotState.speed)));
  
  if (simCount % BigInt(speedFrames) === 0n) {
    advanceRobotLogic();
    needsPaint(); // Request paint update when robot state changes
  }
  
  // Update debug panel state
  debugPanel.update(robotState);
}

function advanceRobotLogic() {
  if (!robotState.currentPath && robotState.pathQueue.length > 0) {
    // Start next path
    robotState.currentPath = robotState.pathQueue.shift();
    robotState.frameCounter = 0;
  }
  
  if (robotState.currentPath) {
    robotState.frameCounter++;
    
    // Calculate progress through current path
    const progress = robotState.frameCounter / robotState.currentPath.duration;
    
    if (progress >= 1.0) {
      // Path complete - trigger lift
      executePathCompletion(robotState.currentPath);
      robotState.completedPaths.push(robotState.currentPath);
      robotState.currentPath = null;
      robotState.frameCounter = 0;
    } else {
      // Path in progress - trigger overlay
      executePathProgress(robotState.currentPath, progress);
    }
  }
  
  // Check if all paths complete
  if (!robotState.currentPath && robotState.pathQueue.length === 0) {
    robotState.active = false;
    console.log("🤖 Robot drawing complete!");
  }
}
```

**Key `sim` Function Benefits:**
- **120fps locked timing** - Consistent logic updates regardless of display refresh rate
- **`simCount`** - BigInt counter for precise frame tracking
- **`needsPaint()`** - Request paint updates only when robot state changes (performance)
- **Speed control** - Use modulo with simCount to control robot execution speed
- **Frame precision** - Perfect for smooth path interpolation and timing control

Support various robotic drawing modes:

```javascript
const ROBO_MODES = {
  // Grid patterns
  'grid': { rows: 10, cols: 10, spacing: 50 },
  'grid:5x3': { rows: 5, cols: 3, spacing: 80 },
  
  // Organic patterns  
  'random': { count: 20, bounds: 'screen' },
  'spiral': { center: 'screen', radius: 200, steps: 50 },
  
  // Animation patterns
  'wave': { amplitude: 100, frequency: 0.1 },
  'orbit': { center: 'screen', radius: 150 }
};

// Example: robo box grid:5x3 fade:red-blue speed:2
```

### 6. Time Control

```javascript
class RoboTimeController {
  constructor() {
    this.speed = 1.0;           // 1.0 = normal speed
    this.pauseRequested = false;
    this.stepMode = false;
  }

  // Speed control: robo box speed:0.5 (half speed)
  setSpeed(multiplier) {
    this.speed = Math.max(0.1, Math.min(10.0, multiplier));
  }

  // Step through frame by frame: robo box step
  enableStepMode() {
    this.stepMode = true;
    this.speed = 0;
  }

  shouldAdvanceFrame() {
    return !this.pauseRequested && (this.speed > 0 || this.stepMode);
  }
}
```

### 7. Virtual Orange Robot Cursor

Add a CSS virtual cursor that shows where the robot is currently drawing:

```javascript
// Virtual cursor management
function updateVirtualCursor(x, y, isDrawing = false) {
  let cursor = document.getElementById('robo-virtual-cursor');
  
  if (!cursor) {
    cursor = document.createElement('div');
    cursor.id = 'robo-virtual-cursor';
    cursor.style.cssText = `
      position: absolute;
      width: 12px;
      height: 12px;
      background: orange;
      border: 2px solid white;
      border-radius: 50%;
      pointer-events: none;
      z-index: 10000;
      box-shadow: 0 0 4px rgba(255, 165, 0, 0.6);
      transition: all 0.1s ease;
      ${isDrawing ? 'transform: scale(1.2);' : ''}
    `;
    document.body.appendChild(cursor);
  }
  
  // Position cursor at robot drawing location
  cursor.style.left = `${x - 6}px`;
  cursor.style.top = `${y - 6}px`;
  cursor.style.opacity = robotState.active ? '1' : '0';
  
  // Scale effect when drawing
  cursor.style.transform = isDrawing ? 'scale(1.2)' : 'scale(1.0)';
}

function removeVirtualCursor() {
  const cursor = document.getElementById('robo-virtual-cursor');
  if (cursor) cursor.remove();
}
```

**Virtual Cursor Features:**
- **Orange robot dot** - Distinct from regular UI cursors
- **Real-time position** - Shows exact robot drawing location  
- **Drawing state feedback** - Scales up when actively drawing
- **Always visible** - Appears even when browser cursor is elsewhere
- **Smooth movement** - CSS transitions for fluid robot motion

Similar to the nopaint performance overlay, create a visual debug panel:

```javascript
class RoboDebugPanel {
  constructor() {
    this.visible = true;
    this.position = { x: 10, y: 10 };
    this.size = { w: 300, h: 200 };
  }

  render({ ink, write, screen }) {
    if (!this.visible) return;

    const { x, y } = this.position;
    const { w, h } = this.size;

    // Semi-transparent background panel
    ink(0, 0, 0, 180).box(x, y, w, h);
    ink(255, 255, 255, 80).box(x, y, w, h, "outline");

    // Title
    ink("cyan").write("🤖 ROBO DEBUG", x + 8, y + 16);

    // Current state info
    let lineY = y + 32;
    const lineHeight = 12;

    ink("white").write(`Brush: ${this.currentBrush || 'none'}`, x + 8, lineY);
    lineY += lineHeight;

    ink("yellow").write(`Pattern: ${this.currentPattern || 'none'}`, x + 8, lineY);
    lineY += lineHeight;

    ink("lime").write(`Speed: ${this.speed.toFixed(1)}x`, x + 8, lineY);
    lineY += lineHeight;

    ink("orange").write(`Frame: ${this.currentFrame}/${this.totalFrames}`, x + 8, lineY);
    lineY += lineHeight;

    // Progress bar
    const progressW = w - 16;
    const progress = this.totalFrames > 0 ? this.currentFrame / this.totalFrames : 0;
    ink(50, 50, 50).box(x + 8, lineY, progressW, 8);
    ink("cyan").box(x + 8, lineY, progressW * progress, 8);
    lineY += 16;

    // Current path info
    if (this.currentPath) {
      ink("magenta").write(`Path: ${this.currentPath.type}`, x + 8, lineY);
      lineY += lineHeight;
      
      ink("gray").write(`Start: ${this.currentPath.startPoint.x}, ${this.currentPath.startPoint.y}`, x + 8, lineY);
      lineY += lineHeight;
      
      ink("gray").write(`End: ${this.currentPath.endPoint.x}, ${this.currentPath.endPoint.y}`, x + 8, lineY);
      lineY += lineHeight;
    }

    // Queue status
    ink("white").write(`Queue: ${this.pathQueue.length} paths`, x + 8, lineY);
    lineY += lineHeight;

    // Mini grid visualization
    this.renderMiniGrid(ink, x + 8, lineY, 100, 60);
  }

  renderMiniGrid(ink, x, y, w, h) {
    // Background
    ink(30, 30, 30).box(x, y, w, h);
    ink(80, 80, 80).box(x, y, w, h, "outline");

    // Grid lines
    const gridSize = 10;
    ink(60, 60, 60);
    for (let gx = gridSize; gx < w; gx += gridSize) {
      ink().line(x + gx, y, x + gx, y + h);
    }
    for (let gy = gridSize; gy < h; gy += gridSize) {
      ink().line(x, y + gy, x + w, y + gy);
    }

    // Show completed paths as green dots
    this.completedPaths?.forEach(path => {
      const miniX = x + (path.startPoint.x / this.screenW) * w;
      const miniY = y + (path.startPoint.y / this.screenH) * h;
      ink("lime").circle(miniX, miniY, 2, true);
    });

    // Show current path as yellow
    if (this.currentPath) {
      const miniX = x + (this.currentPath.startPoint.x / this.screenW) * w;
      const miniY = y + (this.currentPath.startPoint.y / this.screenH) * h;
      ink("yellow").circle(miniX, miniY, 3, true);
    }

    // Show queued paths as gray dots
    this.pathQueue?.forEach(path => {
      const miniX = x + (path.startPoint.x / this.screenW) * w;
      const miniY = y + (path.startPoint.y / this.screenH) * h;
      ink("gray").circle(miniX, miniY, 1, true);
    });
  }

  update(roboState) {
    this.currentBrush = roboState.brushName;
    this.currentPattern = roboState.pattern;
    this.speed = roboState.speed;
    this.currentFrame = roboState.frame;
    this.totalFrames = roboState.totalFrames;
    this.currentPath = roboState.currentPath;
    this.pathQueue = roboState.pathQueue;
    this.completedPaths = roboState.completedPaths;
    this.screenW = roboState.screenWidth;
    this.screenH = roboState.screenHeight;
  }

  toggle() {
    this.visible = !this.visible;
  }
}
```

Integration in main robo piece:

```javascript
// In robo.mjs
let debugPanel = new RoboDebugPanel();

function paint({ ink, write, screen, keyboard }) {
  // Toggle debug panel with 'd' key
  if (keyboard.pressed.d) {
    debugPanel.toggle();
  }

  // Execute robotic drawing
  if (robotState.active) {
    executeRoboticFrame();
  }

  // Update and render debug panel
  debugPanel.update(robotState);
  debugPanel.render({ ink, write, screen });
}
```

### 8. Visual Pattern Preview

Show upcoming pattern visually:

```javascript
function renderPatternPreview(ink, x, y, w, h) {
  ink(0, 0, 0, 100).box(x, y, w, h);
  
  // Show the complete pattern as wireframe
  pathGenerator.pathQueue.forEach((path, index) => {
    const alpha = Math.max(50, 255 - index * 10); // Fade distant paths
    const previewX = x + (path.startPoint.x / screen.width) * w;
    const previewY = y + (path.startPoint.y / screen.height) * h;
    const previewW = (path.endPoint.x - path.startPoint.x) / screen.width * w;
    const previewH = (path.endPoint.y - path.startPoint.y) / screen.height * h;
    
    ink(100, 100, 255, alpha).box(previewX, previewY, previewW, previewH, "outline");
  });
}

## Implementation TODO

1. **Create basic robo.mjs piece** (Start here!)
   - Core structure with sim() function for 120fps logic timing
   - Simple grid pattern generator (3x3 box grid)
   - Robot state management with speed control
   - Command parsing: `robo grid 3 box 0.5` (pattern, size, brush, speed)
   - needsPaint() integration for performance
   - Basic console logging for debug feedback

### Phase 2: Path Generation
1. Implement `RoboPathGenerator` class
2. Add basic patterns: grid, random, spiral
3. Create path interpolation system
4. **Add `RoboDebugPanel` with mini-grid visualization**
5. Test automated box drawing in patterns

### Phase 3: Advanced Features
1. Add time controls (speed, pause, step mode)
2. Implement complex patterns (waves, orbits)
3. Add parameter parsing for brush configuration
4. Support fade colors and brush-specific parameters
5. **Enhance debug panel with pattern preview and state visualization**

### Phase 4: Integration & Polish
1. Integrate with existing color system
2. Add recording/playback capabilities
3. Create export functionality for generated art
4. Add real-time control interface

## Technical Research Findings

### Brush Loading Pattern
- Use `await import()` for dynamic loading (found in `disk.mjs:4776`)
- Brushes export `overlay`, `lift`, and optionally `brush` functions
- Need to mock the nopaint system structure for brush execution

### Nopaint System Structure
- Brushes expect `mark` (dragBox coordinates) and `color` parameters
- `overlay` function shows preview during drawing
- `lift` function executes final drawing when stroke completes
- System provides `ink()` function for actual rendering

### Drawing Data Flow
- User interaction creates `dragBox` coordinates
- `dragBox` contains `{x, y, w, h}` rectangle data
- Brushes use this data to determine what/where to draw
- For robo mode, we synthesize this data programmatically

## Example Usage

```bash
# Basic automated box drawing
robo box

# Grid of red boxes with debug panel
robo box grid fade:red debug

# Spiral pattern with debug visualization
robo box spiral fade:blue-green speed:0.5 debug

# Random placement with custom count and mini-grid view
robo box random count:50 fade:rainbow debug

# Step-through mode for debugging (press 'd' to toggle panel)
robo box grid:3x3 step
```

**Debug Panel Features:**
- 🤖 Real-time robo state display
- 📊 Progress bar and frame counter  
- 🗺️ Mini-grid showing completed/current/queued paths
- ⏱️ Speed and timing information
- 🎯 Current path coordinates and type
- 👀 Pattern preview wireframe overlay

## Future Extensions

1. **Brush Composition**: Combine multiple brushes in sequence
2. **Physics Simulation**: Add gravity, collision for realistic movement
3. **AI Integration**: Use ML to generate interesting patterns
4. **Live Coding**: Real-time pattern modification while drawing
5. **Network Sync**: Coordinate multiple robo instances across devices

## Benefits

1. **Automated Art Generation**: Create complex artworks without manual drawing
2. **Brush Testing**: Systematically test brushes across different scenarios
3. **Pattern Libraries**: Build reusable pattern generation systems
4. **Educational Tool**: Demonstrate brush behavior and geometric concepts
5. **Performance Art**: Live automated drawing performances

This system would essentially create a "turtle graphics" style plotter that can use any existing brush in the Aesthetic Computer ecosystem, opening up powerful possibilities for procedural art generation and automated drawing systems.