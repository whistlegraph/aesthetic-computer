# Aesthetic Computer Piece Structure

*Understanding how to create and structure AC pieces*

## Overview

Aesthetic Computer pieces are JavaScript modules that define interactive experiences. Each piece exports specific functions that hook into the AC lifecycle, providing entry points for initialization, rendering, input handling, and cleanup.

## Piece Lifecycle

### Core Functions

Every piece can export these optional functions:

#### `boot({ ... })`
**Purpose**: Initialize the piece when it starts
**When Called**: Once when the piece loads
**Common Uses**: Setup variables, load resources, configure settings

```javascript
export function boot({ wipe, screen, resolution }) {
  // Initialize piece state
  resolution(800, 600);
  console.log(`Piece started with screen: ${screen.width}x${screen.height}`);
}
```

#### `paint({ ... })`
**Purpose**: Render the visual output
**When Called**: Every frame at the display refresh rate (typically 60fps)
**Common Uses**: Draw graphics, update animations, render UI

```javascript
export function paint({ wipe, ink, box, circle }) {
  wipe("black");         // Clear screen
  ink("white");          // Set color
  box(50, 50, 100, 100); // Draw rectangle
  circle(200, 200, 50);  // Draw circle
}
```

#### `act({ event, ... })`
**Purpose**: Handle user input and system events
**When Called**: When input events occur (clicks, key presses, etc.)
**Common Uses**: Respond to user interaction, state changes

```javascript
export function act({ event, pointer, keyboard }) {
  if (event.is("pointer:down")) {
    console.log(`Clicked at: ${pointer.x}, ${pointer.y}`);
  }
  
  if (event.is("keyboard:down:space")) {
    console.log("Space key pressed");
  }
}
```

#### `sim({ ... })`
**Purpose**: Update simulation logic
**When Called**: At a fixed rate (typically 120fps), independent of rendering
**Common Uses**: Physics calculations, game logic, state updates

```javascript
let position = 0;
export function sim({ }) {
  // Update position independent of framerate
  position += 0.5;
  if (position > 800) position = 0;
}
```

#### `beat({ ... })`
**Purpose**: Synchronize with musical timing
**When Called**: On metronome beats based on BPM setting
**Common Uses**: Musical synchronization, rhythmic animations

```javascript
export function beat({ bpm }) {
  console.log(`Beat at ${bpm} BPM`);
  // Trigger rhythmic events
}
```

#### `leave({ ... })`
**Purpose**: Clean up before the piece unloads
**When Called**: Right before switching to another piece
**Common Uses**: Save state, clean up resources, send final data

```javascript
export function leave({ store }) {
  store["myPiece:lastPosition"] = position;
  console.log("Piece is leaving");
}
```

### Optional Functions

#### `meta({ ... })`
**Purpose**: Define piece metadata
**When Called**: Once during piece initialization
**Common Uses**: Set title, description, configuration

```javascript
export function meta({ store }) {
  return {
    title: "My Amazing Piece",
    description: "An interactive art experience",
    author: "Artist Name"
  };
}
```

#### `preview({ ... })`
**Purpose**: Create custom thumbnail
**When Called**: When generating piece previews
**Common Uses**: Draw representative thumbnail image

```javascript
export function preview({ wipe, ink, write, slug }) {
  wipe(64);
  ink(255);
  write(slug, { center: "xy", size: 1 });
}
```

## API Access

Each lifecycle function receives an API object containing all available functions and data:

### Common API Properties
- **Graphics**: `wipe`, `ink`, `box`, `circle`, `line`, `plot`, `write`
- **Input**: `pointer`, `keyboard`, `event`
- **Screen**: `screen` (width, height), `resolution`
- **Time**: `frame`, `time`, `beat`
- **Storage**: `store` (persistent data)
- **Network**: `send`, `receive`, `session`
- **Audio**: `tone`, `noise`, `sound`
- **Utilities**: `random`, `map`, `constrain`

### Destructuring Pattern
```javascript
export function paint({ wipe, ink, box, screen, pointer }) {
  // Only destructure what you need
  wipe("black");
  ink("cyan");
  box(pointer.x, pointer.y, 50, 50);
}
```

## Piece Types

### Interactive Pieces
Focus on user input and response:
```javascript
let drawing = [];

export function paint({ wipe, ink, line }) {
  wipe("black");
  ink("white");
  
  // Draw all lines in drawing array
  for (let i = 0; i < drawing.length - 1; i++) {
    line(drawing[i].x, drawing[i].y, 
         drawing[i+1].x, drawing[i+1].y);
  }
}

export function act({ event, pointer }) {
  if (event.is("pointer:down") || event.is("pointer:drag")) {
    drawing.push({ x: pointer.x, y: pointer.y });
  }
}
```

### Generative Pieces
Focus on procedural generation:
```javascript
export function paint({ wipe, ink, circle, screen, random }) {
  wipe("navy");
  ink("cyan");
  
  // Generate random circles
  for (let i = 0; i < 50; i++) {
    circle(
      random() * screen.width,
      random() * screen.height,
      random() * 20 + 5
    );
  }
}
```

### Animation Pieces
Focus on time-based changes:
```javascript
let rotation = 0;

export function sim() {
  rotation += 0.02; // Update rotation in sim for smooth animation
}

export function paint({ wipe, ink, box, screen, sin, cos }) {
  wipe("black");
  ink("red");
  
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const radius = 100;
  
  const x = centerX + sin(rotation) * radius;
  const y = centerY + cos(rotation) * radius;
  
  box(x, y, 20, 20, "center");
}
```

## Configuration & Exports

### System Configuration
```javascript
// Export configuration for special systems
export const system = "nopaint"; // Use nopaint system
export const palette = "custom";  // Custom color palette
```

### Piece Settings
```javascript
// Piece-specific settings
export const wrap = true;      // Enable coordinate wrapping
export const autolock = false; // Disable auto-locking
```

## Best Practices

### Performance
- Keep `paint` function efficient (called 60+ times per second)
- Use `sim` for heavy calculations
- Minimize object creation in hot paths
- Cache expensive calculations

### State Management
- Use module-level variables for piece state
- Use `store` for persistent data across sessions
- Initialize state in `boot`, clean up in `leave`

### User Experience
- Provide visual feedback for interactions
- Handle edge cases gracefully
- Consider accessibility (keyboard navigation, etc.)

### Code Organization
```javascript
// State at module level
let gameState = { score: 0, level: 1 };
let particles = [];

// Helper functions
function createParticle(x, y) {
  return { x, y, vx: random(-2, 2), vy: random(-2, 2) };
}

// Lifecycle functions
export function boot({ screen }) {
  // Initialize particles
  for (let i = 0; i < 10; i++) {
    particles.push(createParticle(
      random() * screen.width,
      random() * screen.height
    ));
  }
}

export function sim() {
  // Update particle positions
  particles.forEach(p => {
    p.x += p.vx;
    p.y += p.vy;
  });
}

export function paint({ wipe, ink, circle }) {
  wipe("black");
  ink("white");
  
  // Render particles
  particles.forEach(p => {
    circle(p.x, p.y, 3);
  });
}
```

## Template Examples

See the `/templates` folder for complete piece templates demonstrating different patterns and use cases.

---

*For more details on the API functions available in each lifecycle function, see the [JavaScript API Reference](../api/javascript-api.md).*