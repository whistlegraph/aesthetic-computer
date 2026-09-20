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
- **Audio**: `sound` (see [Sound](#sound)), `speaker`, `microphone`
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

## Sound

`sound.synth({ tone, type, duration, attack, decay, volume, pan })` plays one
voice and returns it: `{ kill(fade), update(props), progress() }`. `tone` is Hz
or a note name (`"c4"`). `duration` is seconds, or `"🔁"` to hold until
`voice.kill(fade)` — `fade` in seconds; a fade of 0 clicks.

Types: `sine`, `triangle`, `square`, `sawtooth`, `noise-white` (alias `noise`;
`tone` sets the resonant filter center), `harp` (aliases `pluck`, `guitar`,
`string`; Karplus-Strong pluck), `whistle` (aliases `ocarina`, `flute`;
waveguide flute), `custom` (pass `generator`), `sample`.

Expressive options on the same call:

| option | meaning | example |
| --- | --- | --- |
| `slide` | target tone; exponential glide from `tone` over `slideDuration` (default: the whole note; 0.25 s for held voices) | `slide: "g5"` |
| `vibrato` | semitones at 5 Hz, or `{ rate, depth, delay }` | `vibrato: 0.3` |
| `tremolo` | depth 0..1 at 6 Hz, or `{ rate, depth }` | `tremolo: { rate: 12, depth: 0.5 }` |
| `drift` | semitones of slow random pitch wander | `drift: 0.2` |
| `noise` | 0..1 white noise mixed into the source (breath, rasp) | `noise: 0.3` |
| `lowpass` | cutoff Hz, or `{ cutoff, resonance, sweep, sweepDuration }` | `lowpass: { cutoff: 400, sweep: 3000, sweepDuration: 0.5 }` |
| `formant` | vowel `"a"`/`"e"`/`"i"`/`"o"`/`"u"`, `[{ freq, bw, gain }]`, or `{ vowel, scale }` (scale < 1 = bigger body) | `formant: { vowel: "o", scale: 0.8 }` |

`voice.update({ tone, volume, duration, vibrato, tremolo, drift, noise, lowpass, formant, slide })`
glides tone and volume linearly over `duration` seconds (default 0.1) and
replaces the modulation settings. Call it from `sim` each frame to drive a
contour by hand:

```javascript
let voice, t = 0;
function act({ event, sound }) {
  if (event.is("touch")) voice = sound.synth({ type: "sawtooth", tone: 200, duration: "🔁", volume: 0.3 });
  if (event.is("lift")) { voice?.kill(0.2); voice = undefined; }
}
function sim() {
  if (!voice) return;
  t += 1 / 60;
  voice.update({ tone: 200 + 150 * Math.sin(t * 3), duration: 0.05 });
}
```

Organic generators return the same voice shape (`kill`; `update` glides numeric
params over `duration`) and take `volume`, `pan`, and `duration` (seconds or `"🔁"`):

- `sound.howl({ pitch = 220, slide = 330, vowel = "o", scale = 1, vibrato = 0.4, rasp = 0.1, attack = 0.08, release = 0.3, duration = 1.5 })` — source-filter vocal model; the general animal call.
- `sound.growl({ pitch = 55, rasp = 0.6, size = 1, tremor = 0.5, duration = 1.2 })`
- `sound.breath({ pressure = 0.6, cutoff = 1200, direction = "out" | "in", duration = 0.8 })`
- `sound.chirp({ pitch = 2500, slide = 4200, count = 3, rate = 8, duration = 0.5 })`
- `sound.bubble({ radius, rise })`, `sound.fart({ pressure, pitch, rasp })` — physical models.

### A donkey bray, two ways

Hee — rising, thin, `"i"` — then haw — falling, open `"a"`, raspy — a beat later.

```javascript
let hawAt;
function act({ event, sound, clock }) {
  if (!event.is("touch")) return;
  sound.howl({ pitch: 300, slide: 520, vowel: "i", scale: 0.9, vibrato: 0.6, rasp: 0.15, duration: 0.5 });
  hawAt = clock.time().getTime() + 500; // one beat at 120 BPM
}
function sim({ sound, clock }) {
  if (hawAt === undefined || clock.time().getTime() < hawAt) return;
  hawAt = undefined;
  sound.howl({ pitch: 260, slide: 140, vowel: "a", scale: 0.8, vibrato: 0.3, rasp: 0.5, attack: 0.03, release: 0.4, duration: 0.7 });
}
```

The same bray on `sound.synth`, scheduled the same way:

```javascript
sound.synth({ type: "sawtooth", tone: 300, slide: 520, duration: 0.5, attack: 0.05, decay: 0.2, volume: 0.35, noise: 0.15, formant: { vowel: "i", scale: 0.9 }, vibrato: 0.6 });
// a beat later, from sim:
sound.synth({ type: "sawtooth", tone: 260, slide: 140, duration: 0.7, attack: 0.03, decay: 0.4, volume: 0.4, noise: 0.5, formant: { vowel: "a", scale: 0.8 }, vibrato: 0.3, lowpass: { cutoff: 2500, sweep: 600, sweepDuration: 0.7 } });
```

When a request names a sound (roar, bray, bark, wind), design it as a pitch
contour plus a noise layer plus an envelope — never a fixed-pitch stab. Layer
two or three voices at most.

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
