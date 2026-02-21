# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Aesthetic Computer (AC) is a mobile-first runtime and social network for creative computing. It's designed as a musical instrument-like interface where users discover memorizable paths through commands and published "pieces" (interactive programs). The system supports both JavaScript (.mjs) and KidLisp (.lisp) pieces.

## Opener Storage (Jeffrey's Dev Sessions)

When @jeffrey is working, **the very first prompt of each Claude Code session is automatically stored** to the MongoDB `openers` collection via the `UserPromptSubmit` hook. The model name is stored alongside it. This captures jeffrey's creative/developer voice over time for pattern analysis.

- **Hook**: `.claude/settings.json` → `UserPromptSubmit` → `node ants/store-opener.mjs`
- **Post-commit**: `.git/hooks/post-commit` also logs each commit message to `openers` (source: "post-commit")
- **Script**: `ants/store-opener.mjs` — writes directly to MongoDB using env vars (`ADMIN_SUB`, `MONGODB_CONNECTION_STRING`, `MONGODB_NAME`)
- **Collection**: `openers` — fields: `user` (auth0 sub, same as `moods`/`kidlisp`/`paintings`), `text`, `model`, `when`, `source`, `context` — handle resolved via `$lookup` on `@handles` when needed

**First things first: store opener.** This happens automatically — no manual step needed.

## AestheticAnts & Score.md

This repository uses an automated maintenance system called "AestheticAnts" (AA). The `ants/score.md` file defines the mindset, philosophy, tasks, and rules for all agents (human and AI) working on the codebase. Read it before contributing.

**Important:** Do not modify `ants/score.md` unless you are the queen (@jeffrey).

## Development Commands

### Running the Development Environment

The system consists of multiple servers that run concurrently:

```bash
# Run all servers (site, session, edge, stripe) - primary dev command
npm run aesthetic
# or shorthand:
npm run ac

# Run individual servers:
npm run site              # Main Netlify dev server (port 8888)
npm run server:session    # Session backend (port 8889)
npm run stripe            # Stripe webhook listener
```

### Testing

```bash
# Run all tests
npm test

# KidLisp tests (with auto-reload on changes)
npm run test:kidlisp

# KidLisp tests (direct, no watch)
npm run test:kidlisp:direct

# Performance tests
npm run test:perf
npm run test:perf:chrome
npm run test:perf:lighthouse
```

### Creating New Pieces

```bash
# Generate a new piece from the blank.mjs template
npm run new piece-name "Description of the piece"
```

### Working with Sessions

```bash
# List active session backends
npm run session:alive

# View logs for a specific session
npm run server:session:logs SESSION_ID

# Reset/terminate a session
npm run session:reset SESSION_ID

# Deploy session server
npm run session:publish
```

### Assets Management

```bash
# Sync assets down from Digital Ocean Spaces
npm run assets:sync:down

# Sync assets up to Digital Ocean Spaces
npm run assets:sync:up
```

### Notation

- **compush** - commit & push

## Architecture

### Core Components

1. **Boot System** (`system/public/aesthetic.computer/boot.mjs`)
   - Entry point that loads the BIOS and initializes the runtime
   - Manages service worker registration for module caching
   - Handles WebSocket module loader for hot reloading
   - Boot telemetry system logs to `/api/boot-log`

2. **BIOS** (`system/public/aesthetic.computer/bios.mjs`)
   - Main runtime coordinator
   - Manages piece lifecycle (boot, act, paint, sim, etc.)
   - Provides the API surface for pieces
   - Handles routing and navigation

3. **Disk System** (`system/public/aesthetic.computer/lib/disk.mjs`)
   - Large module (~572KB) that provides the core API for pieces
   - Graphics primitives, audio, input handling, UI components
   - All pieces interact with AC through the Disk API

4. **Module Loader** (`system/public/aesthetic.computer/module-loader.mjs`)
   - WebSocket-based dynamic module loading
   - Enables hot reloading during development
   - Prefetches commonly used modules

### Pieces

Pieces are the fundamental unit of content in AC. Each piece is a single `.mjs` or `.lisp` file located in `system/public/aesthetic.computer/disks/`.

#### Piece Structure (JavaScript)

Pieces export lifecycle functions that receive an API object:

```javascript
// boot: runs once when the piece loads
function boot({ wipe, screen, params, colon, api }) {
  // Initialize state
}

// paint: runs every frame
function paint({ wipe, ink, line, circle, screen }) {
  // Render graphics
}

// act: handles user input and events
function act({ event: e }) {
  if (e.is("keyboard:down:space")) {
    // Handle spacebar press
  }
}

// sim: simulation/game logic that runs every frame
function sim() {
  // Update game state
}

export { boot, paint, act, sim };
```

Common lifecycle functions:
- `boot` - Initialization, runs once
- `paint` - Rendering, runs every frame
- `act` - Event handling (input, network, etc.)
- `sim` - Simulation/logic, runs every frame
- `leave` - Cleanup when exiting the piece
- `preview` - Static preview image generation

#### Piece API Surface

The API is provided through function parameters. Common APIs:
- **Graphics**: `wipe`, `ink`, `line`, `box`, `circle`, `plot`, `paste`, etc.
- **Text**: `write`, `type`, `paste`, `help`
- **Input**: `event`, `pen`, `hand`, `gamepad`
- **Audio**: `sound`, `speaker`, `microphone`
- **UI**: `ui.Button`, `ui.TextInput`, `cursor`
- **System**: `screen`, `params`, `colon`, `store`, `net`, `jump`, `send`

### Servers and Services

1. **System Server** (`system/`)
   - Netlify dev server serving the main application
   - Edge functions in `netlify/edge-functions/`
   - Serverless functions in `netlify/functions/`
   - Handles piece serving, authentication, storage APIs

2. **Session Server** (`session-server/`)
   - Per-session backend using Jamsocket for ephemeral deployments
   - WebSocket server using Geckos.io for real-time communication
   - Manages chat, multiplayer state, and real-time features
   - Uses Redis for state synchronization

3. **Feed Server** (`dp1-feed/`)
   - Cloudflare Worker for activity feeds
   - Deployed separately to Cloudflare Workers

### KidLisp

KidLisp is a minimal Lisp dialect for generative art, with 118 built-in functions across 12 categories. See `kidlisp/README.md` for comprehensive documentation.

**Key files:**
- `system/public/aesthetic.computer/lib/kidlisp.mjs` - Main evaluator
- `system/netlify/functions/store-kidlisp.mjs` - Storage API
- `kidlisp/tools/` - Analysis and debugging tools

**Running KidLisp tools:**
```bash
# Analyze piece structure (requires dev server running)
./kidlisp/tools/source-tree.mjs $cow

# Get source code
./kidlisp/tools/get-source.mjs $piece-code
```

### Data Storage

- **MongoDB**: User data, handles, chat messages, moods
- **Redis**: Session state, real-time coordination
- **Firebase**: Authentication, cloud messaging
- **Digital Ocean Spaces**: Asset storage (CDN)

### Routing and URLs

- Pieces are URL-addressable: `https://aesthetic.computer/piece-name`
- Parameters: `piece-name:param1:param2`
- User pieces: `@handle/piece-name`
- QR code sharing: `share piece-name`

## Development Workflow

### Local Development in Codespaces

When running in GitHub Codespaces, the server is accessible at:
```
https://{CODESPACE_NAME}-8888.app.github.dev
```

Get your Codespace name: `echo $CODESPACE_NAME`

### Hot Reloading

The module loader provides hot reloading during development:
- Piece changes are reflected immediately when saved
- WebSocket connection shown in boot canvas
- Use `channel custom-name` for multi-device testing

### Publishing

```bash
# In the AC prompt:
publish                    # Publish current piece
publish piece-name         # Publish with custom name
source                     # Download blank template
source piece-name          # Fork existing piece
```

## Important Directories

- `system/public/aesthetic.computer/disks/` - All pieces (.mjs and .lisp files)
- `system/public/aesthetic.computer/lib/` - Shared libraries and utilities
- `system/netlify/functions/` - Serverless backend functions
- `session-server/` - Real-time session backend
- `shared/` - Code shared between system and session servers
- `kidlisp/` - KidLisp language documentation and tools
- `spec/` - Jasmine tests for KidLisp
- `ants/` - AestheticAnts automated maintenance system

## Key Patterns

### Event Handling in Pieces

Events use a string-based pattern matching system:
```javascript
event.is("keyboard:down:a")          // 'a' key pressed
event.is("touch")                     // Any touch event
event.is("lift")                      // Touch/click released
event.is("draw")                      // Drag with pen down
event.is("keyboard:down:arrowup")    // Arrow key
```

### State Management

Pieces maintain state in module-level variables:
```javascript
let score = 0;
let enemies = [];

function boot() {
  // Initialize state
}

function sim() {
  // Update state
  score += 1;
}
```

### API Requests from Pieces

Use the `net` API for HTTP requests:
```javascript
function boot({ net }) {
  net.pieces("@user/list").then((data) => {
    // Handle response
  });
}
```

### UI Components

```javascript
function boot({ ui: { Button, TextInput } }) {
  const btn = new Button("Click me", { box: [10, 10, 100, 40] });
}

function act({ event: e }) {
  if (btn.trigger(e)) {
    // Button was clicked
  }
}
```

## Notes

- The codebase uses `.mjs` extension for ES modules
- Prefer `const` destructuring for API parameters to minimize imports
- Graphics operations are immediate-mode (no retained scene graph)
- All coordinates are in pixels
- Default color depth is 8-bit RGB (0-255 per channel)
- The `wipe` function clears the screen and should be called first in `paint`
- When making changes, consult `ants/score.md` for the project's working philosophy
