# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Aesthetic Computer (AC) is a mobile-first runtime and social network for creative computing. It functions as a musical instrument-like interface where users discover memorizable paths through commands and published "pieces" (interactive software toys). The platform encourages exploration, improvisation, and creative expression through code.

## Core Architecture

### System Structure
- **system/**: Main application directory containing the web runtime
- **system/public/aesthetic.computer/**: Core client-side application
  - **boot.mjs**: Application bootstrap and initialization
  - **bios.mjs**: Core system functionality and API imports
  - **lib/**: Shared library modules (2D/3D graphics, audio, input, etc.)
  - **disks/**: Individual "pieces" - the creative software toys (.mjs and .lisp files)
- **system/netlify/**: Serverless functions for backend operations
- **session-server/**: Real-time session management using Jamsocket
- **nanos/**: Chat and conductor services
- **vscode-extension/**: VS Code integration for piece development

### Key Concepts
- **Pieces**: Single-file programs (.mjs or .lisp) that run in the AC environment
- **Prompt System**: Command-line interface for navigating between pieces
- **KidLisp**: Custom Lisp dialect designed for creative and educational programming
  - Built-in graphics functions: `(wipe "blue")`, `(ink "red")`, `(line x1 y1 x2 y2)`
  - Time-based expressions: `(3s ...)` for 3-second intervals, `(0.5s ...)` for half-second
  - Creative shortcuts: `(? val1 val2 ...)` for random selection, `(wiggle amount)` for randomness
  - Visual effects: `(blur n)`, `(zoom factor)`, `(spin angle)`, `(scroll offset)`
  - Function definitions: `(def name value)`, `(later funcname args ...)`
- **Real-time Collaboration**: Multi-user sessions and chat functionality

## Development Commands

### Primary Development
```bash
# Start full development environment
npm run aesthetic

# Start individual services
npm run site              # Main web application (system/)
npm run server:session    # Session server for real-time features
npm run tunnel            # Ngrok tunneling for external access
```

### Testing
```bash
# Run KidLisp tests
npm run test:kidlisp

# Run system tests
cd system && npm test
```

### Code Quality
```bash
# Format code (Prettier configured with 2-space tabs)
npx prettier --write .

# Type checking (TypeScript configured for JS files)
cd system && npx tsc --noEmit
```

### Piece Development
```bash
# Create new piece template
npm run new

# Reload specific piece during development
npm run reload-piece
```

### Asset Management
```bash
# Sync assets with DigitalOcean Spaces
npm run assets:sync:down    # Download from cloud
npm run assets:sync:up      # Upload to cloud
```

## Piece Development Patterns

### Basic Piece Structure
```javascript
// Standard piece with paint function
function paint({ wipe, ink, line, pen, screen }) {
  wipe("gray");           // Clear background
  ink("yellow");          // Set color
  line(0, 0, 100, 100);   // Draw primitives
}

// Optional lifecycle functions
function boot() { }        // Initialize
function act({ event }) { } // Handle input
function sim() { }         // Logic updates (120fps)
function beat() { }        // Metronome sync
function leave() { }       // Cleanup
```

### API Patterns
- Graphics: `wipe()`, `ink()`, `line()`, `box()`, `circle()`, `write()`
- Input: `pen` (mouse/touch), `event` handling, `keyboard`
- Audio: `tone()`, `bleep()`, metronome integration
- Utilities: `choose()`, `timestamp()`, `radians()`

### KidLisp Pieces
KidLisp files use `.lisp` extension and provide a creative programming environment with:

```lisp
; Basic graphics and math
(wipe "blue")                    ; Clear screen with color
(ink "lime")                     ; Set drawing color
(line 90 (+ 10 20 30) 30 80)   ; Draw line with computed coordinates
(box 8 32 (- width 20) 16)     ; Draw rectangle

; Time-based programming
(3s (now slide (% slide+1 4)))  ; Execute every 3 seconds
(0.5s (zoom 0.97))              ; Execute every half second

; Creative utilities
(? white black rainbow)          ; Random choice from options
(wiggle 32)                     ; Random value ±32
(repeat 5 (line))               ; Repeat operation

; Visual effects
(blur 7)                        ; Blur effect
(spin 0.28888)                  ; Rotation
(scroll width/5)                ; Scrolling offset
(mask x y w h)                  ; Masking regions

; Function definitions
(def x 10)                      ; Define variable
(later cross x y                ; Define function
  (line x-10 y-10 x+10 y+10))
(cross 16 32)                   ; Call function
```

The KidLisp interpreter (`kidlisp.mjs`) handles parsing, evaluation, and integration with AC's graphics API.

## Development Workflow

1. **Local Development**: Use `npm run aesthetic` to start all services
2. **Piece Creation**: Use `npm run new` or VS Code extension
3. **Testing**: Drag & drop pieces into running AC instance
4. **Publishing**: Use `publish` command within AC interface
5. **Real-time Features**: Session server handles multi-user collaboration

## Important Notes

- The platform uses Netlify for hosting with edge functions
- Real-time features use Jamsocket for session management
- Assets are stored on DigitalOcean Spaces
- Authentication via Auth0 for registered users
- Mobile-first design with touch/gesture support
- WebGL for graphics rendering with fallbacks

## Platform Integration

- **Netlify**: Main hosting and serverless functions
- **Firebase**: Authentication and real-time features
- **DigitalOcean**: Asset storage and some compute
- **Stripe**: Payment processing for physical prints
- **MongoDB**: User data and piece storage
- **Jamsocket**: Real-time session orchestration

## VS Code Extension

The project includes a VS Code extension for piece development:
```bash
cd vscode-extension
npm run build    # Build extension
npm run watch    # Watch mode for development
npm run publish  # Publish to marketplace
```

## File Patterns

- Pieces: `/system/public/aesthetic.computer/disks/*.mjs` or `*.lisp`
- Libraries: `/system/public/aesthetic.computer/lib/*.mjs`
- Functions: `/system/netlify/functions/*.js`
- Assets: `/system/public/assets/`
- Tests: `/spec/` (Jasmine) and `/system/tests/` (Jest)