<!-- the piece authoring guide
     Bundled with aesel from system/public/aesthetic.computer/disks/CLAUDE.md in the Aesthetic Computer repository.
     Do not edit here — edit the source and run `npm run context`. -->

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
- **System**: `screen`, `params`, `colon`, `store`, `net`, `clock`, `jump`, `send`

## Musical Timing

When adding sound, prefer the shared network clock for sequences, loops, pulses,
and their visuals. The API is `clock.time()` (a `Date`), not `net.time()`.
Call `clock.resync()` in `boot` and periodically while running; it refreshes the
server offset asynchronously. The clock uses local time until synchronized.

Default to **120 BPM with Unix epoch zero as the beat origin**, unless the user
requests another tempo or transport. A common clock alone does not synchronize
pieces with different tempos or start offsets. Compute pattern position from
absolute beats, including subdivisions and swing, rather than counting frames,
accumulating sim ticks, or starting a `performance.now()`/timer clock at load.
Join on the next boundary; after a stall, skip missed notes instead of replaying
them. Drive rhythmic visuals from the same beat. Direct touch/key sounds can
remain immediate unless the user requests quantization.

```javascript
const BPM = 120;
const notes = ["c4", "e4", "g4", "e4"];
let lastStep;
let resyncAt = 0;

function boot({ clock }) {
  clock.resync();
  lastStep = undefined;
  resyncAt = clock.time().getTime() + 30000;
}

function sim({ clock, sound }) {
  const now = clock.time().getTime();
  if (now >= resyncAt) {
    clock.resync();
    resyncAt = now + 30000;
  }
  const step = Math.floor(now * BPM / 60000);
  if (lastStep === undefined) lastStep = step; // Wait for the next beat.
  if (step <= lastStep) return;
  lastStep = step;
  sound?.synth?.({ type: "sine", tone: notes[step % notes.length], duration: 0.2, volume: 0.2 });
}

export { boot, sim };
```

This aligns the musical grid; frame delivery and audio output latency still
limit onset precision. Do not describe it as sample-accurate synchronization.

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

Note both channels relay through the server — geckos.io is not peer-to-peer.
For frame-critical 1v1 (rollback netcode), see `docs/rollback-netcode.md` and
the `fight` piece; its simulation lives in `../lib/fight/`.

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
