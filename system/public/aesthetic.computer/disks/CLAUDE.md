# Pieces — Authoring Guide

Pieces are the fundamental unit of content in AC. Each piece is a single
`.mjs` or `.lisp` file in this directory. This guide loads when you work
here; the repo-wide rules live in the root `CLAUDE.md`.

## Piece Structure (JavaScript)

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

## Piece API Surface

The API is provided through function parameters. Common APIs:
- **Graphics**: `wipe`, `ink`, `line`, `box`, `circle`, `plot`, `paste`, etc.
- **Text**: `write`, `type`, `paste`, `help`
- **Input**: `event`, `pen`, `hand`, `gamepad`
- **Audio**: `sound`, `speaker`, `microphone`
- **UI**: `ui.Button`, `ui.TextInput`, `cursor`
- **System**: `screen`, `params`, `colon`, `store`, `net`, `jump`, `send`

## Event Handling

Events use a string-based pattern matching system:
```javascript
event.is("keyboard:down:a")          // 'a' key pressed
event.is("touch")                     // Any touch event
event.is("lift")                      // Touch/click released
event.is("draw")                      // Drag with pen down
event.is("keyboard:down:arrowup")    // Arrow key
```

## State Management

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

## API Requests from Pieces

Use the `net` API for HTTP requests:
```javascript
function boot({ net }) {
  net.pieces("@user/list").then((data) => {
    // Handle response
  });
}
```

## Multiplayer Networking (Dual-Channel Pattern)

Multiplayer pieces use both WebSocket (reliable) and UDP (low-latency)
channels. **`squash.mjs` is the canonical implementation — read it before
writing multiplayer code.** Other references: `1v1.mjs` (3D FPS),
`udp.mjs` (minimal UDP test).

The shape: in `boot({ net: { socket, udp }, handle })`,
- `udp(handler)` opens the low-latency channel for high-frequency position
  sync (may drop packets); send with `udpChannel.send("game:move", {...})`.
  UDP content may arrive as a JSON string — parse defensively.
- `socket(handler)` opens the reliable channel for join/leave, scoring,
  round control; send with `server.send("game:join", {...})`. Handler
  receives `(id, type, content)`; watch for `connected*`, `left`, and your
  own `game:*` types.

Session-server routing (`session-server/session.mjs`):
- UDP handlers: add `channel.on("game:move", ...)` in the geckos section
- WebSocket: position messages use `others()` (relay to all except sender),
  game events use `everyone()` (catch-all relay)
- Chat invites: typing `'piece-name'` in chat creates a clickable join link

## UI Components

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

## Creating and Publishing

```bash
npm run new piece-name "Description"   # scaffold from blank.mjs template
```

In the AC prompt:
```
publish                    # Publish current piece
publish piece-name         # Publish with custom name
source                     # Download blank template
source piece-name          # Fork existing piece
```

## Notes

- Prefer `const` destructuring for API parameters to minimize imports
- Graphics are immediate-mode (no retained scene graph); coordinates in pixels
- Default color depth is 8-bit RGB (0-255 per channel)
- `wipe` clears the screen and should be called first in `paint`
- Pieces are URL-addressable: `aesthetic.computer/piece-name`, params via
  `piece-name:param1:param2`, user pieces via `@handle/piece-name`,
  QR sharing via `share piece-name`
- Leaves (pieces) stay small and can be loop-generated — see `HAND.md`
