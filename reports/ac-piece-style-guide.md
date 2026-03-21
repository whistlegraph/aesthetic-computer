# AC Piece (Disk) Style Guide

How we write interactive pieces on aesthetic.computer.

---

## Philosophy

One `.mjs` file per piece. Each piece lives in `system/public/aesthetic.computer/disks/`. A piece is a self-contained program that exports lifecycle functions. The system calls them at the right time -- you just fill in the blanks.

Pieces are resolution-adaptive, input-agnostic (mouse, touch, gamepad), and run inside a sandboxed worker. They range from single-screen toys (sprinkles.mjs: 18 lines) to full apps (keep.mjs: 3600+ lines). Keep it as simple as the idea demands.

---

## File Structure

```
system/public/aesthetic.computer/disks/
  mypiece.mjs          <- your piece

system/public/aesthetic.computer/lib/
  num.mjs              <- math utilities (rand, lerp, clamp, etc.)
  geo.mjs              <- geometry (Box, Grid, Circle, Line)
  note-colors.mjs      <- music note color mapping
  gamepad-mappings.mjs  <- controller button/axis mapping
  ...

system/public/aesthetic.computer/dep/
  ...                  <- third-party dependencies
```

One file is the norm. Import from `../lib/` or `../dep/` when truly needed (notepat imports note-colors, 1v1 imports gamepad-mappings). Don't create utility files for single-piece use.

---

## Lifecycle Functions

Export any subset of these. All are optional. All receive a single `api` object to destructure.

```javascript
// 🥾 Boot (Runs once when piece loads)
function boot({ screen, num, resolution, hud, cursor, store }) {}

// 🧮 Sim (Runs at 120fps for logic/physics, frame-independent)
function sim({ num, screen, simCount }) {}

// 🎨 Paint (Runs once per display refresh for rendering)
function paint({ wipe, ink, screen, pen, write, paste }) {}

// 🎪 Act (Runs on user interaction)
function act({ event: e, needsPaint, sound }) {}

// 🥁 Beat (Runs once per metronomic BPM)
function beat({ sound }) {}

// 👋 Leave (Runs once before piece unloads -- cleanup, save state)
function leave({ store }) {}

// 📰 Meta (Returns metadata for documentation/search)
function meta() {
  return { title: "Name", desc: "What it does." };
}

export { boot, sim, paint, act, meta };
```

**Execution order:** `boot` -> (`sim` at 120fps, `paint` at display rate, `act` on input, `beat` on BPM) -> `leave`

---

## The API Object

Every lifecycle function receives a single object. Destructure only what you need.

### Drawing

| API | Usage |
|-----|-------|
| `wipe(r, g, b)` or `wipe("color")` | Clear screen |
| `ink(r, g, b, a)` | Set color, returns chainable draw object |
| `ink().plot(x, y)` | Draw single pixel |
| `ink().line(x1, y1, x2, y2)` | Draw line |
| `ink().box(x, y, w, h)` | Draw filled box |
| `ink().box(x, y, w, h, "outline")` | Draw box outline |
| `ink().circle(x, y, r, filled?)` | Draw circle |
| `ink().poly(points)` | Draw polygon |
| `ink().write(text, {x, y})` | Draw text |
| `paste(bitmap, x, y, scale?)` | Blit image/painting |
| `stamp(bitmap, cx, cy)` | Center-paste image |

**Chaining:** `ink(255, 0, 0).box(0, 0, 10, 10)` -- set color and draw in one call.

**Named colors:** `ink("red")`, `ink("blue")`, `ink("cyan")`, `ink("lime")`, etc.

### Screen & Pixels

```javascript
function paint({ screen }) {
  const { width, height, pixels } = screen;
  // pixels is a Uint8ClampedArray (RGBA, 4 bytes per pixel)
  // Index: (y * width + x) * 4
}
```

**Use `screen.pixels` for per-pixel work.** Calling `ink().plot()` in a loop over thousands of pixels is slow. Write directly to the pixel buffer instead:

```javascript
// Fast: direct pixel buffer
const pi = (y * width + x) << 2;
pixels[pi] = r;
pixels[pi + 1] = g;
pixels[pi + 2] = b;
pixels[pi + 3] = a;

// Slow: function call per pixel (fine for < 100 points)
ink(r, g, b, a).plot(x, y);
```

### Input

```javascript
function act({ event: e }) {
  if (e.is("touch"))    { /* finger/mouse down at e.x, e.y */ }
  if (e.is("draw"))     { /* finger/mouse moved while down */ }
  if (e.is("lift"))     { /* finger/mouse released */ }
  if (e.is("keyboard")) { /* key pressed: e.key */ }
}

// Continuous pointer position (available in paint/sim too):
function paint({ pen }) {
  if (pen) { /* pen.x, pen.y, pen.pressure (0-1), pen.device */ }
}
```

### Math (`num`)

```javascript
function boot({ num }) {
  num.rand()                  // Random float 0-1
  num.randInt(n)              // Random int 0-n (inclusive)
  num.randIntRange(lo, hi)    // Random int lo-hi (inclusive)
  num.randIntArr(n, count)    // Array of random ints
  num.randInd(arr)            // Random index into array
  num.clamp(val, min, max)
  num.lerp(a, b, t)
  num.dist(x1, y1, x2, y2)
  num.radians(degrees)
  num.degrees(radians)
  num.map(val, inLo, inHi, outLo, outHi)
}
```

### Helpers

```javascript
function sim({ help: { repeat, choose } }) {
  repeat(10, (i) => { /* runs 10 times */ });
  choose("red", "blue", "green")  // random pick
}
```

### Resolution

Set a custom virtual resolution in `boot`. The system scales the canvas to fill the viewport.

```javascript
function boot({ resolution, screen }) {
  resolution(128, 128);          // Low-res pixel art
  resolution(512, 512);          // Fixed square
  resolution(screen.width / 2, screen.height / 2);  // Half-res
  // No call = native screen resolution (default)
}
```

**Note:** After calling `resolution()`, `screen.width`/`screen.height` in subsequent frames will reflect the new size.

### Cursor

```javascript
function boot({ cursor }) {
  cursor("none");    // Hide system cursor
  cursor("native");  // Show default system cursor
  // Default: AC custom cursor
}
```

### HUD / Label

The corner label shows the piece name by default.

```javascript
function boot({ hud }) {
  hud.label("mypiece");          // Custom label text
  hud.label("mypiece 42");       // Label with state info
  hud.label();                   // Hide label entirely
}
```

### Navigation

```javascript
function act({ event: e, jump }) {
  if (e.key === "Enter") jump("other-piece");
  // jump("piece:param1:param2")  -- with parameters
  // jump("piece", true)          -- ahistorical (no back button)
}
```

### Persistence

```javascript
async function boot({ store }) {
  const saved = await store.retrieve("key", "local:db");
  if (saved) state = saved;
}

function leave({ store }) {
  store["key"] = state;
  store.persist("key", "local:db");
}
```

### Sound

```javascript
function act({ sound }) {
  sound.synth({
    type: "sine",     // "sine", "triangle", "square", "sawtooth"
    tone: 440,        // Hz
    duration: 0.5,    // seconds
    attack: 0.1,
    decay: 0.9,
    volume: 0.5       // 0-1
  });
}
```

### Images & Network

```javascript
async function boot({ net: { preload }, get }) {
  // Preload an image asset
  const { img } = await preload("https://assets.aesthetic.computer/rain/guy.png");

  // Get another user's painting
  const { img: painting } = await get.painting("slug").by("handle");
}
```

### Painting Buffers

Create off-screen canvases for layered rendering:

```javascript
let buf;
function boot({ painting, screen }) {
  buf = painting(screen.width, screen.height, (api) => api.wipe(0));
}

function paint({ page, paste, ink, screen }) {
  page(buf);                     // Switch render target to buffer
  ink("red").circle(50, 50, 10); // Draw into buffer
  page(screen);                  // Switch back to screen
  paste(buf);                    // Composite buffer onto screen
}
```

### UI Buttons

```javascript
let btn;
function boot({ ui: { Button } }) {
  btn = new Button({ x: 10, y: 10, w: 60, h: 20 });
}

function paint({ ink }) {
  ink("lime").box(btn.box);
  ink("black").write("Go", { x: btn.box.x + 4, y: btn.box.y + 4 });
}

function act({ event: e, jump }) {
  btn.act(e, () => jump("target"));
}
```

---

## System Modes

Control how the piece integrates with the AC shell by exporting a `system` string.

```javascript
// Default: standard piece with prompt HUD
// (no export needed)

// Hide the prompt HUD entirely (fullscreen experiences)
export const system = "noprompt";

// Inherit the nopaint raster editor system (drawing tools)
export const system = "nopaint";

// Nopaint + auto-save painting on leave
export const system = "nopaint:bake-on-leave";

// Prompt/chat mode (LLM-powered character pieces)
export const system = "prompt:character";

// 3D world mode
export const system = "world";
```

### When to use `noprompt`

Use it for fullscreen simulations, games, visualizations -- anything where the typing prompt would get in the way. The ant simulation, metaballs, games, etc.

### When to use `nopaint`

Use it when building a drawing tool that needs a persistent canvas (rect, line, spray, smear, word, icon). The `nopaint` system provides:
- `system.painting` -- the persistent canvas with `.pixels` (Uint8ClampedArray)
- `system.nopaint.buffer` -- overlay layer for previews
- `system.nopaint.undo` -- undo history
- `system.nopaint.zoomLevel` / `system.nopaint.translation` -- pan/zoom state
- `system.nopaint.updateBrush()` -- transform pointer coords
- `system.nopaint.present()` -- render painting + overlay to screen

---

## Parameters

Pieces receive URL parameters. For `/mypiece:hello:42`:

```javascript
function boot({ params }) {
  params[0] // "hello"
  params[1] // "42"
}
```

Colon params (after `~`) for configuration: `/mypiece~thick~aa`

```javascript
function boot({ colon }) {
  colon[0] // "thick"
  colon[1] // "aa"
}
```

---

## Performance Guide

### Pixel-heavy rendering

For simulations, fractals, or effects that touch every pixel:

1. **Write to `screen.pixels` directly** instead of `ink().plot()` per pixel
2. **Use bitwise ops** for integer conversion: `x | 0` instead of `Math.floor(x)`, `i << 2` instead of `i * 4`
3. **Use typed arrays** (Float32Array, Uint8ClampedArray) for grids/buffers
4. **Combined wipe + draw** -- skip `wipe()` and fill pixels in your own loop

```javascript
// Good: single pass, no function calls per pixel
function paint({ screen }) {
  const { width, height, pixels } = screen;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const pi = (y * width + x) << 2;
      pixels[pi] = r; pixels[pi+1] = g; pixels[pi+2] = b; pixels[pi+3] = 255;
    }
  }
}
```

### Object-based rendering

For pieces with a manageable number of entities (< 1000):

- `ink().plot()`, `ink().circle()`, `ink().box()` are fine
- `wipe()` then draw everything is the standard pattern
- Use `needsPaint()` in `act()` to trigger repaints only when needed

### Simulation

- `sim()` runs at 120fps -- keep it cheap
- Heavy math belongs in `sim()`, not `paint()`
- Pre-compute what you can in `boot()`

---

## Conventions

### Naming

Piece names are short, evocative, lowercase: `ant`, `rain`, `tone`, `notepat`, `spray`, `wand`. The name becomes the URL slug: `aesthetic.computer/ant`.

### File Header

```javascript
// PieceName, YYYY.M.DD
// One-line description.
```

### TODO Regions

```javascript
/* #region 🏁 TODO
  - [] Incomplete task.
  - [x] Completed task.
#endregion */
```

### Section Comments

Use emoji markers for section clarity (optional, common in larger pieces):

```javascript
// 🥾 Boot
function boot() {}

// 🧮 Sim
function sim() {}

// 🎨 Paint
function paint() {}

// 🎪 Act
function act() {}

// 📚 Library
function helper() {}
```

### State

Top-level `let` variables. No state management library.

```javascript
let ants = [];
let score = 0;
let gridW, gridH;
```

### Exports

Named exports at the bottom. System mode first if present.

```javascript
export const system = "noprompt";
export { boot, sim, paint, act, meta };
```

### Library Section

Helper functions go at the bottom, after the exports:

```javascript
export { boot, sim, paint, act, meta };

// 📚 Library

function gridIndex(x, y) { ... }
function sense(ant, trail, offset) { ... }
```

---

## Minimal Piece Template

```javascript
// MyPiece, 2026.2.10
// What it does in one sentence.

function boot({ screen }) {
  // Initialize state.
}

function paint({ wipe, ink, screen }) {
  wipe(0);
  ink("white").write("hello", { x: 6, y: 6 });
}

function act({ event: e }) {
  if (e.is("touch")) {
    // Respond to input.
  }
}

function meta() {
  return { title: "MyPiece", desc: "What it does." };
}

export { boot, paint, act, meta };
```

---

## Fullscreen Simulation Template

```javascript
// MySim, 2026.2.10
// Pixel-level simulation with direct buffer access.

let state;

function boot({ screen }) {
  state = new Float32Array(screen.width * screen.height);
}

function sim({ screen }) {
  // Update state at 120fps.
}

function paint({ screen }) {
  const { width, height, pixels } = screen;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const pi = (y * width + x) << 2;
      const v = state[y * width + x];
      pixels[pi] = v; pixels[pi+1] = v; pixels[pi+2] = v; pixels[pi+3] = 255;
    }
  }
}

function act({ event: e }) {
  if (e.is("touch")) { /* interact */ }
}

function meta() {
  return { title: "MySim", desc: "A simulation." };
}

export const system = "noprompt";
export { boot, sim, paint, act, meta };
```

---

## Reference Implementations

| Category | Piece | Lines | Notes |
|----------|-------|-------|-------|
| Minimal | [sprinkles.mjs](../system/public/aesthetic.computer/disks/sprinkles.mjs) | ~32 | Random pixel plotting, no boot |
| Simulation | [rain.mjs](../system/public/aesthetic.computer/disks/rain.mjs) | ~93 | Particles, preloaded images, sim+paint |
| Pixel buffer | [metaballs.mjs](../system/public/aesthetic.computer/disks/metaballs.mjs) | ~200 | Direct screen.pixels, per-pixel math |
| Drawing tool | [spray.mjs](../system/public/aesthetic.computer/disks/spray.mjs) | ~90 | nopaint system, painting buffers |
| Game | [1v1.mjs](../system/public/aesthetic.computer/disks/1v1.mjs) | ~2000 | Multiplayer, gamepad, 3D rendering |
| Music | [notepat.mjs](../system/public/aesthetic.computer/disks/notepat.mjs) | ~4000 | Touch UI, sound synthesis, responsive layout |
| Complex app | [keep.mjs](../system/public/aesthetic.computer/disks/keep.mjs) | ~3600 | Multi-mode app, persistence, blockchain |

---

## What We Don't Use

- Frameworks or build tools inside pieces
- TypeScript (pieces are plain .mjs)
- External CSS or HTML (everything is code-drawn)
- `requestAnimationFrame` (the system handles the render loop)
- `document` or `window` (pieces run in a worker sandbox)
- `Math.random()` (use `num.rand()` for consistency)
