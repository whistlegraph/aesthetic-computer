# Low-Hanging Fruit: Web Pieces for AC Native

Which pieces from `system/public/aesthetic.computer/disks/` could run unmodified on
AC Native right now (with the clock.mjs runtime patches already in place)?

---

## Tier 1: Works Right Now (zero runtime changes)

These use only APIs already in native: `wipe`, `ink` (with color names), `write`,
`line`, `box`, `circle`, `plot`, `screen`, `num.clamp/rand/randIntRange/lerp/map/dist`,
`sound.synth` (with working `.kill()`), `event.is()`, `params`, `colon`,
`performance.now()`, `console.log`.

| Piece | Lines | What it does | Notes |
|-------|-------|-------------|-------|
| **brick-breaker.mjs** | 61 | Paddle + ball + bricks | `ink().box("*center")` chaining, `num.randIntRange` |
| **dync.mjs** | 71 | Percussive pad template | `wipe("blue")`, ink, line, box |
| **3x3.mjs** | 45 | 3x3 ortholinear pad | `wipe("blue")`, `e.is("keyboard:down")`, logs `e` |
| **basic-line-pointer.mjs** | 39 | Line follows mouse | `wipe().ink().line()` chain, `event.name === "move"` |
| **error.mjs** | 64 | Error display screen | `wipe("black")`, `ink([r,g,b]).write()`, params parsing |
| **gostop.mjs** | 35 | Go/stop rhythm game | `wipe(r,g,b)`, `sound.synth({tone, beats, decay})` |

### What makes them work
- No imports (all self-contained)
- No `system = "nopaint"` or `system = "world"` or `system = "prompt:code"`
- No `net`, `store`, `hud`, `pen.drawing`, `cursor()`, `paste`, `page`, `geo.*`
- Uses only numeric or string-named colors (which we just added)

---

## Tier 2: Works with 1-2 Small Runtime Additions

These need minor additions to the native runtime (not the pieces).

### Needs `num.randInt(max)` and `num.randIntArr(max, count)`

These are trivially implementable (~10 LOC each in js-bindings.c).

| Piece | Lines | What it does | Blocking API |
|-------|-------|-------------|-------------|
| **sprinkles.mjs** | 32 | Random pixel emitter | `num.randInt`, `num.randIntArr`, `cursor("none")` |

### Needs `lineAngle(x, y, length, angle)`

Simple trig wrapper (~15 LOC in graph.c/js-bindings.c).

| Piece | Lines | What it does | Blocking API |
|-------|-------|-------------|-------------|
| **metronome.mjs** | 547 | Visual metronome + tap BPM | `lineAngle`, `sound.bpm()` as function, `sound.synth().progress()`, `circle` with fill, `store` (guarded), `query` |

### Needs `sound.bpm(val)` as a callable function (not just a property)

Currently `sound.bpm` is a float property. Several pieces call `sound.bpm(120)`.
Fix: make it a getter/setter or a function that updates `audio->bpm`.

| Piece | Lines | What it does | Blocking API |
|-------|-------|-------------|-------------|
| **beat.mjs** | 57 | Percussive mouse instrument | `wipe("brown")`, `synth()`, `e.device`, `e.button` — works except `beat()` lifecycle needs `sound.bpm()` |
| **metronome.mjs** | 547 | Full metronome | (see above) |

### Needs `circle` with filled parameter

Native `circle(x, y, r, filled)` exists but clock's `circle(x, y, r, true)` needs
the boolean to work. Verify this is handled.

| Piece | Lines | What it does | Notes |
|-------|-------|-------------|-------|
| **balls.mjs** | 67 | Bouncing ball on purple bg | `geo.Circle` class (needs stub), `circle(..., true)` |

### Needs `num.radians(deg)`

Simple: `function(d) { return d * Math.PI / 180; }` — one line.

| Piece | Lines | What it does | Notes |
|-------|-------|-------------|-------|
| **starfield.mjs** | 99 | Classic 3D starfield | `num.radians`, `num.randInt`, `ink().plot()` — pure math, no browser deps |

---

## Tier 3: Interesting but Needs More Work

### Needs `beat()` lifecycle

Native has `beat()` support already (synced to audio BPM). These pieces could work
if `sound.bpm()` is made callable.

| Piece | Lines | What it does | Blocking |
|-------|-------|-------------|----------|
| **gostop.mjs** | 35 | Go/stop rhythm | `beat()` + `sound.bpm()` + `needsPaint()` |
| **melody.mjs** | 258 | Score playback | `beat()`, `sound.bpm()`, `geo.Grid`, `gizmo.Hourglass`, imports `common/music.mjs` |

### Needs `system = "nopaint"` awareness

These export `system = "nopaint"` which means they're painting tools, not standalone
pieces. They'd need native to understand the nopaint system or be stripped of it.
**Not recommended** — these are fundamentally web painting tools.

| Piece | What it does |
|-------|-------------|
| wipe.mjs, fill.mjs, oval.mjs, spray.mjs, sparkle.mjs, box.mjs | Painting tools |

### Needs `system = "world"` or `system = "prompt:code"`

Special piece system modes not available in native.

| Piece | What it does |
|-------|-------------|
| horizon.mjs | World-system side-scroller |
| sing.mjs | LLM prompt-based note generator |
| game.mjs | `system = "game"` — different runtime |

---

## Tier 4: Not Viable

| Reason | Examples |
|--------|---------|
| Heavy imports (`../lib/hand.mjs`, `../lib/gesture.mjs`) | wave.mjs, spline.mjs |
| Needs `net.socket()`, multiplayer | spray.mjs (networking), chat pieces |
| Needs `ui.Button` rendering (not just constructor) | whistlegraph.mjs |
| Needs `noise16DIGITPAIN`, `display`, `resolution` | noise.mjs |
| Needs `mask()`/`unmask()`, gradient ink strings | gradient-test.mjs |
| Needs `pen.drawing`, `pen.dragBox`, nopaint system | All nopaint tools |
| Needs external asset loading | digitpain*.mjs, melody.mjs (fonts) |

---

## Recommended Action Plan

### Phase 1: Ship Now (0 additional runtime work)
Bundle these 6 pieces alongside clock.mjs in the next OTA:
- `brick-breaker.mjs` — a playable game
- `dync.mjs` — percussive pad starter
- `3x3.mjs` — keyboard instrument pad
- `basic-line-pointer.mjs` — interactive line demo
- `error.mjs` — useful system screen
- `gostop.mjs` — rhythm game (if `beat()` lifecycle already works)

### Phase 2: Quick Wins (~30 LOC runtime additions)
Add `num.randInt(max)`, `num.randIntArr(max, count)`, `num.radians(deg)`:
- `starfield.mjs` — visually impressive, pure math
- `sprinkles.mjs` — generative pixel art

### Phase 3: Medium Effort (~50 LOC)
Make `sound.bpm(val)` callable + add `lineAngle`:
- `metronome.mjs` — musical tool, great companion to clock
- `beat.mjs` — percussion instrument

### Total: ~11 web pieces runnable on native with ~80 LOC of runtime additions.
