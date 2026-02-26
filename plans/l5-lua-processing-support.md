# Plan: L5 (Processing-style Lua) Support on Aesthetic Computer

## Context

AC supports two piece languages: JavaScript (`.mjs`) and KidLisp (`.lisp`). We want to add **Lua (`.lua`)** as a third, enabling L5/Processing-style sketches to run natively in the browser. This uses **Wasmoon** — a Lua 5.4 VM compiled to WebAssembly (~130KB gzipped) — following the exact same integration pattern as KidLisp.

L5 is a Processing library for Lua (runs on Love2D desktop). We're not porting L5 itself — we're implementing the **Processing API surface** in Lua, bridged to AC's drawing primitives via Wasmoon's JS↔Lua interop.

---

## Files to Create/Modify

| File | Action |
|---|---|
| `system/public/aesthetic.computer/dep/wasmoon/` | **Create** — vendor Wasmoon ESM + WASM binary |
| `system/public/aesthetic.computer/lib/l5.mjs` | **Create** — Lua runtime adapter (~400 lines) |
| `system/public/aesthetic.computer/lib/disk.mjs` | **Modify** — import l5, add `.lua` detection + fallback |
| `system/public/aesthetic.computer/disks/l5-hello.lua` | **Create** — test piece |

---

## Step 1: Vendor Wasmoon

Install `wasmoon` via npm and copy the ESM bundle + WASM binary into `dep/wasmoon/`. The `dep/` directory already holds vendored packages (three, gl-matrix, wasmboy, etc.).

```
dep/wasmoon/
  index.mjs        # ESM entry (LuaFactory, LuaEngine exports)
  glue.wasm        # Lua 5.4 VM compiled to WebAssembly
```

The WASM URL is constructed relative to the module:
```javascript
const WASM_URL = new URL("../dep/wasmoon/glue.wasm", import.meta.url).href;
```

---

## Step 2: Create `lib/l5.mjs` — The Lua Runtime Adapter

Follows the KidLisp singleton pattern. Exports one function: `module(source)` → returns `{ boot, paint, sim, act, leave }`.

### 2.1 Architecture

```
l5.mjs
  ├── ensureFactory()     — lazy-init Wasmoon LuaFactory singleton
  ├── module(source)      — main export: compile Lua → lifecycle object
  ├── createDrawingState()— fill/stroke/transform state manager
  ├── injectConstants()   — PI, TWO_PI, CENTER, CORNER, etc.
  ├── injectMathGlobals() — sin, cos, random, lerp, map, dist, noise, etc.
  ├── injectDrawingAPI()  — background, fill, stroke, rect, circle, line, text, etc.
  ├── updateInputGlobals()— mouseX, mouseY, width, height, frameCount per frame
  └── dispatchEvent()     — AC events → Lua callbacks (keyPressed, mousePressed, etc.)
```

### 2.2 Drawing State

Processing has stateful fill/stroke that AC doesn't — the adapter tracks:

```javascript
{
  fillColor: [255,255,255,255],   // current fill RGBA
  strokeColor: [0,0,0,255],       // current stroke RGBA
  fillEnabled: true,               // noFill() toggles
  strokeEnabled: true,             // noStroke() toggles
  strokeWeight: 1,
  colorMode: "RGB",                // RGB | HSB | HSL
  colorMax: [255,255,255,255],     // for colorMode scaling
  rectMode: "CORNER",              // CORNER | CENTER | CORNERS | RADIUS
  ellipseMode: "CENTER",
  textSizeVal: 8,
  textAlignH: "LEFT",
  // Transform stack for push()/pop()
  currentTransform: { tx: 0, ty: 0 },
  transformStack: [],
  styleStack: [],
}
```

Shape drawing applies fill then stroke:
```javascript
function drawShape($, state, fillFn, strokeFn) {
  if (state.fillEnabled) {
    $.ink(...state.fillColor);
    fillFn($);
  }
  if (state.strokeEnabled) {
    $.ink(...state.strokeColor);
    strokeFn($);
  }
}
```

### 2.3 Lifecycle Bridge

```
L5 setup()          → AC boot($)    — called once
L5 draw()           → AC paint($)   — called every frame
L5 keyPressed() etc → AC act($)     — dispatched per event
L5 sim() (custom)   → AC sim($)     — if defined in Lua
```

Before each `paint()` call, input globals are updated:
```javascript
engine.global.set("mouseX", $.pen?.x ?? 0);
engine.global.set("mouseY", $.pen?.y ?? 0);
engine.global.set("width", $.screen?.width ?? 128);
engine.global.set("height", $.screen?.height ?? 128);
engine.global.set("frameCount", frameCount++);
```

### 2.4 Async Note

Unlike `lisp.module()` which is synchronous, `l5.module()` is **async** (Wasmoon engine creation requires `await`). The loading path in disk.mjs already uses `await` for dynamic imports, so this fits naturally.

---

## Step 3: Full L5 → AC API Mapping

### Lifecycle

| L5 Function | AC Equivalent | Notes |
|---|---|---|
| `setup()` | `boot($)` | Runs once on load |
| `draw()` | `paint($)` | Every frame |
| `keyPressed()` | `act($)` on `keyboard:down:*` | |
| `keyReleased()` | `act($)` on `keyboard:up:*` | |
| `mousePressed()` | `act($)` on `touch` | |
| `mouseReleased()` | `act($)` on `lift` | |
| `mouseMoved()` | `act($)` on `move` | |
| `mouseDragged()` | `act($)` on `draw` | |
| `mouseClicked()` | Not mapped v1 | Need tap detection |
| `mouseWheel()` | Not mapped v1 | |

### Screen & Background

| L5 | AC | Notes |
|---|---|---|
| `background(r,g,b)` | `$.wipe(r,g,b)` | Also supports single gray value |
| `clear()` | `$.wipe(0,0,0,0)` | Transparent clear |
| `size(w,h)` | `$.resolution(w,h)` | Set canvas resolution |

### Color State

| L5 | AC | Notes |
|---|---|---|
| `fill(r,g,b,a)` | Sets `state.fillColor`, applied via `$.ink()` before each shape | |
| `noFill()` | `state.fillEnabled = false` | |
| `stroke(r,g,b,a)` | Sets `state.strokeColor` | |
| `noStroke()` | `state.strokeEnabled = false` | |
| `strokeWeight(w)` | `state.strokeWeight = w` | Used for line thickness |
| `colorMode(mode, max...)` | Internal state | Converts HSB/HSL → RGB for `$.ink()` |
| `lerpColor(c1,c2,t)` | `num.blend(c1,c2,t)` or manual lerp | |
| `color(r,g,b,a)` | Returns `[r,g,b,a]` table | Used as color value |
| `red(c)` / `green(c)` / `blue(c)` / `alpha(c)` | Extract from color array | |

### Shape Drawing

| L5 | AC | Notes |
|---|---|---|
| `point(x,y)` | `$.plot(x,y)` | Uses stroke color |
| `line(x1,y1,x2,y2)` | `$.line(x1,y1,x2,y2)` | Uses stroke color |
| `rect(x,y,w,h)` | `$.box(x,y,w,h)` filled + `$.box(x,y,w,h,"outline")` stroked | Respects `rectMode` |
| `square(x,y,s)` | Delegates to `rect(x,y,s,s)` | |
| `circle(x,y,d)` | `$.circle(x,y,d/2)` | L5 uses diameter, AC uses radius |
| `ellipse(x,y,w,h)` | `$.oval(x,y,w/2,h/2)` | Respects `ellipseMode` |
| `triangle(x1,y1,...)` | `$.tri(x1,y1,x2,y2,x3,y3)` | |
| `quad(x1,y1,...x4,y4)` | `$.poly([x1,y1,...])` | |
| `arc(x,y,w,h,start,stop)` | `$.pie(x,y,w/2,start,stop)` | Approximate |
| `beginShape()` / `vertex(x,y)` / `endShape(mode)` | Collect vertices → `$.shape(verts)` or `$.poly(verts)` | |

### Text

| L5 | AC | Notes |
|---|---|---|
| `text(str,x,y)` | `$.write(str, x, y)` | Uses fill color |
| `textSize(s)` | `state.textSizeVal = s` | AC bitmap font is fixed; pass as scale option |
| `textAlign(h,v)` | `state.textAlignH/V` | Internal tracking |
| `textWidth(str)` | `$.text.width(str)` | |

### Transforms

| L5 | AC | Notes |
|---|---|---|
| `translate(x,y)` | `state.currentTransform.tx += x` | Coordinate offset applied to all shapes |
| `push()` | `state.push()` | Saves transform + style state |
| `pop()` | `state.pop()` | Restores transform + style state |
| `resetMatrix()` | Reset transform to `{tx:0, ty:0}` | |
| `rotate(angle)` | **Not supported v1** | Would need software matrix transform |
| `scale(sx,sy)` | **Not supported v1** | Same limitation |

### Math

| L5 | AC / JS | Notes |
|---|---|---|
| `abs`, `ceil`, `floor`, `round`, `sqrt`, `pow`, `exp`, `log` | `Math.*` | Direct delegation |
| `sq(x)` | `x * x` | |
| `min`, `max` | `Math.min/max` | |
| `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2` | `Math.*` | |
| `radians(deg)` | `deg * PI / 180` | Also in `num.radians` |
| `degrees(rad)` | `rad * 180 / PI` | Also in `num.degrees` |
| `random(min?, max?)` | `Math.random()` scaled | |
| `constrain(v,lo,hi)` | `num.clamp(v,lo,hi)` | |
| `dist(x1,y1,x2,y2)` | `num.dist(x1,y1,x2,y2)` | |
| `lerp(a,b,t)` | `num.lerp(a,b,t)` | |
| `map(v,iL,iH,oL,oH)` | `num.map(v,iL,iH,oL,oH)` | |
| `noise(x,y?,z?)` | `num.perlin(x,y)` | z dimension ignored v1 |
| `norm(v,lo,hi)` | `(v-lo)/(hi-lo)` | |
| `fract(x)` | `x - Math.floor(x)` | |
| `randomSeed(s)` | No-op v1 | |
| `noiseSeed(s)` | No-op v1 | |
| `randomGaussian()` | Box-Muller transform | |

### Input Globals (updated each frame)

| L5 | Source | Notes |
|---|---|---|
| `mouseX`, `mouseY` | `$.pen.x`, `$.pen.y` | |
| `pmouseX`, `pmouseY` | Previous frame's pen position | Tracked internally |
| `movedX`, `movedY` | `mouseX - pmouseX` | Computed |
| `mouseIsPressed` | `$.pen.drawing` | |
| `mouseButton` | From event data | LEFT/RIGHT/CENTER |
| `key` | From keyboard event | Last key pressed |
| `keyCode` | Char code of key | |
| `keyIsPressed` | Any key currently held | Tracked in `keyState` map |
| `width`, `height` | `$.screen.width/height` | |
| `frameCount` | Internal counter | |
| `deltaTime` | Computed from frame timing | |
| `focused` | `true` | Always focused in AC |

### Constants

| L5 | Value |
|---|---|
| `PI` | `Math.PI` |
| `HALF_PI` | `Math.PI / 2` |
| `QUARTER_PI` | `Math.PI / 4` |
| `TWO_PI` / `TAU` | `Math.PI * 2` |
| `DEGREES` / `RADIANS` | Mode strings |
| `RGB` / `HSB` / `HSL` | Color mode strings |
| `LEFT` / `CENTER` / `RIGHT` | Alignment strings |
| `TOP` / `BOTTOM` / `BASELINE` | Vertical alignment |
| `CORNER` / `CORNERS` / `RADIUS` | Shape mode strings |
| `CLOSE` | For `endShape(CLOSE)` |

### Environment

| L5 | AC | Notes |
|---|---|---|
| `frameRate(fps)` | `$.fps(fps)` | |
| `cursor()` / `noCursor()` | `$.cursor()` | |
| `loop()` / `noLoop()` / `isLooping()` | Internal flag; skip `draw()` calls when noLoop | |
| `redraw()` | `$.needsPaint()` | |
| `print(...)` / `println(...)` | `console.log(...)` | |
| `millis()` | `performance.now()` | |
| `day()`, `month()`, `year()`, `hour()`, `minute()`, `second()` | `new Date()` methods | |

### Image (basic support)

| L5 | AC | Notes |
|---|---|---|
| `loadImage(url)` | `$.get.picture(url)` | Async — returns promise |
| `image(img,x,y)` | `$.paste(img,x,y)` | |
| `get(x,y)` | Read from `$.screen.pixels` | |
| `set(x,y,c)` | `$.ink(c); $.plot(x,y)` | |

### Not Mapped (v1 scope exclusions)

- `rotate()` / `scale()` — needs software matrix (significant effort)
- `bezier()` / `curve()` — not in AC
- `loadFont()` — AC has fixed bitmap fonts
- `loadVideo()` / video playback — not in AC
- `filter()` effects — partial (blur/invert exist)
- `createGraphics()` — could map to `$.painting()` later
- `smooth()` / `noSmooth()` — AC is pixel-native
- `save()` / `loadStrings()` / file I/O — different paradigm
- `blend()` modes — partial support
- `tint()` / `noTint()` — not directly available
- `applyMatrix()` — needs full matrix support

---

## Step 4: Modify `disk.mjs`

### 4.1 Add import (near line 68)

```javascript
import * as l5 from "./l5.mjs";
```

### 4.2 Add `.lua` detection (after KidLisp block ~line 7563, before the `else` at ~7639)

```javascript
} else if (
  (path && path.endsWith(".lua")) ||
  (sourceToRun.trim().startsWith("--") && /function\s+(setup|draw)\s*\(/.test(sourceToRun))
) {
  sourceCode = sourceToRun;
  originalCode = sourceCode;
  pieceMetadata = { code: slug || "l5", trustLevel: "l5", anonymous: true };
  send({ type: "boot-log", content: `compiling lua ...` });
  loadedModule = await l5.module(sourceToRun);
  send({ type: "boot-file", content: { filename: path, source: sourceCode.slice(0, 8000) } });
  if (devReload) store["publishable-piece"] = { slug, source: sourceToRun, ext: "lua" };
}
```

### 4.3 Extend fallback chain (~line 7736)

After `.mjs` 404, try `.lua` before `.lisp`:
```
.mjs (404) → .lua (404) → .lisp (404) → 404 piece
```

---

## Step 5: Error Handling

- **Compile errors**: `engine.doString()` throws → return a module with `paint()` that displays the error on screen in red text
- **Runtime errors**: Each lifecycle call wrapped in `try/catch`, logged with `"L5"` prefix
- **Infinite loops**: v1 documents the limitation; v2 could use Lua debug hooks for instruction-count limits
- **Cleanup**: `leave()` calls `engine.global.close()` to free WASM memory

---

## Step 6: Verification

1. `npm run site` — start dev server
2. Create `disks/l5-hello.lua` with basic `setup()`/`draw()` using `background`, `fill`, `circle`, `text`
3. Navigate to `localhost:8888/l5-hello` — verify it renders
4. Test input: click to add circles, verify `mouseX`/`mouseY`/`mousePressed()` work
5. Test `push()`/`pop()`/`translate()` with nested transforms
6. Test error display: intentionally break Lua syntax, verify error shows on screen
7. Test fallback: delete `.mjs`, verify `.lua` loads via the fallback chain
8. Test hot reload: edit `.lua` file, verify changes appear

---

## Step 7: L5 Documentation + Showcase Rollout

### Goal

Ship a **graspable proof page** that shows what L5 API surface AC supports, what is partial, and what is not yet implemented.

### 7.1 Extend Existing `/docs` System First (lowest-risk)

Use the current docs pipeline (`docs` command → `/docs` → `docs.js`) and add an L5 section there first:

- Add `docs.api.l5` in `system/netlify/functions/docs.js`
- Add index links under a new `<h2>L5 (Lua)</h2>` section
- Add entries like:
  - `overview`
  - `lifecycle`
  - `graphics`
  - `color`
  - `text`
  - `math`
  - `input`
  - `environment`
  - `compatibility` (full/partial/unsupported table)
  - `unsupported` (explicit v1 exclusions)

Resulting URLs:

- `/docs/l5:overview`
- `/docs/l5:compatibility`
- etc.

### 7.2 Add a Dedicated `/l5` Landing Page (show-off page)

Create a static HTML page following AC frontend conventions (single-file page, no framework):

- `system/public/l5.aesthetic.computer/index.html`

Page sections:

1. Hero: “L5 on Aesthetic Computer”
2. “Run now” links (sample pieces)
3. Compatibility matrix (L5 API → AC API mapping)
4. “What’s different from Processing/Love2D”
5. Live code snippets (Lua examples)
6. CTA: “open docs”, “open prompt”, “try example”

Style direction:

- Reuse AC fonts (`Berkeley Mono`, `YWFT Processing`) and color token pattern from frontend style guide
- Responsive layout for desktop/mobile
- Keep everything in one HTML file unless it exceeds practical size

### 7.3 Routing

Add redirects in `system/netlify.toml`:

- `from = "/l5"` → `/l5.aesthetic.computer/index.html`
- `from = "https://l5.aesthetic.computer"` → `/l5.aesthetic.computer/index.html`
- `from = "https://l5.aesthetic.computer/*"` → `/l5.aesthetic.computer/index.html`

Optional aliases:

- `/L5` → `/l5` (case convenience)

### 7.4 Prompt/Command Integration

Add quick entry commands in `system/public/aesthetic.computer/disks/prompt.mjs`:

- `l5docs` → `out:/docs/l5:overview`
- `l5` (or `l5learn`) → `out:/l5`

Keep existing `docs` command unchanged.

### 7.5 Single Source of Truth for Compatibility

Avoid hand-maintaining the matrix in two places.

Create one source object/file (for example `system/public/aesthetic.computer/lib/l5-reference.mjs`) with:

- `name`
- `l5Sig`
- `acEquivalent`
- `status` (`full`, `partial`, `unsupported`)
- `notes`

Use it to render:

- `/docs` L5 entries
- `/l5` compatibility table

This prevents docs drift as runtime support changes.

### 7.6 Accuracy Guardrails (important)

Do **not** claim “full L5 parity” in v1.

Label unsupported/partial APIs clearly (e.g. `rotate`, `scale`, `bezier`, file/video APIs). The page should be a trustable status board, not marketing-only copy.

---

## Step 8: Verification for Docs/Showcase

1. Run `npm start`
2. Verify:
   - `/docs` shows new “L5 (Lua)” section
   - `/docs/l5:overview` and `/docs/l5:compatibility` render correctly
   - `/docs.json` includes `api.l5`
3. Verify `/l5` loads and works on desktop + mobile breakpoints
4. Verify prompt commands:
   - `l5docs`
   - `l5` (or chosen alias)
5. Confirm every compatibility claim matches real runtime behavior
6. Run `npm test` before merge
