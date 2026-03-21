# JavaScript API Reference

*Generated from disk.mjs analysis*

## Overview

The Aesthetic Computer JavaScript API is provided through `disk.mjs` and serves as the primary interface for creating interactive pieces. This API provides graphics, input, audio, networking, and utility functions.

## API Categories

### 🎨 Graphics & Drawing
Core functions for visual output and screen manipulation.

#### Screen Management
- `wipe(color)` - Clear screen with color
- `resolution(width, height)` - Set canvas resolution  
- `screen` - Object containing width/height properties

#### Drawing Primitives
- `ink(color)` - Set drawing color
- `ink(r, g, b)` - Set RGB color
- `ink(color, alpha)` - Set color with transparency
- `line(x1, y1, x2, y2)` - Draw line
- `box(x, y, w, h, mode?)` - Draw rectangle
- `circle(x, y, radius)` - Draw circle
- `plot(x, y)` - Set pixel
- `tri(x1, y1, x2, y2, x3, y3)` - Draw triangle

#### Text & Typography
- `write(text, x, y, size?)` - Draw text
- `type` - Typography utilities and typeface loading

### 🖱️ Input & Interaction
Functions for handling user input and events.

#### Pointer/Mouse
- `pointer` - Mouse/touch position and state
- `pointer.x, pointer.y` - Current coordinates
- `pointer.down, pointer.up` - Click states

#### Keyboard
- `keyboard` - Keyboard input state
- `key` - Current key being pressed

#### Touch & Gesture
- Touch events and multi-touch handling

### 🔊 Audio & Sound
Sound generation, playback, and audio processing.

#### Sound Generation
- `tone(frequency, duration)` - Generate tone
- `noise()` - Generate noise

#### Audio Playback
- Audio file loading and playback

### 🌐 Networking & Sessions
Communication and data sharing capabilities.

#### Socket Communication
- WebSocket connections for real-time communication

#### Session Management
- User sessions and persistence

### 🔧 Utilities & Math
Mathematical functions and general utilities.

#### Math Operations
- Built-in JavaScript Math functions
- Additional utility functions

#### Data Structures
- Array and object manipulation helpers

## Usage Patterns

### Basic Piece Structure
```javascript
// boot: Initialize piece
function boot({ wipe, screen }) {
  // Setup code
}

// paint: Render each frame
function paint({ wipe, ink, box }) {
  wipe("black");
  ink("white");
  box(50, 50, 100, 100);
}

// act: Handle input
function act({ event, pointer }) {
  if (event.is("pointer:down")) {
    // Handle click
  }
}
```

### Common Patterns
- Screen clearing: `wipe(color)`
- Setting colors: `ink(color)` before drawing
- Coordinate systems: (0,0) at top-left
- Event handling: Check `event.is(type)` in act function

## API Analysis Status

**⚠️ This documentation is under development**

- [ ] Complete function inventory from disk.mjs
- [ ] Parameter types and validation  
- [ ] Return value documentation
- [ ] Usage examples for each function
- [ ] Cross-references with pieces that use each function

## Next Steps

1. **Automated Analysis**: Tool to extract all exported functions from disk.mjs
2. **Type Discovery**: Analyze function signatures and parameter types
3. **Usage Examples**: Create working examples for each API function
4. **Coverage Mapping**: Compare with manual documentation in docs.js
5. **Testing Suite**: Validate API documentation against implementation

---

*This is a living document that will be updated as the API analysis tools are developed.*