# AC Native: Self-Contained Web Pieces — Full Mapping

Every `.mjs` piece in `disks/` with **zero imports** and **no special system mode**,
evaluated against the AC Native runtime API surface.

---

## Native API Status (after clock.mjs patches)

**Available:** `wipe(r,g,b)`, `wipe("name")`, `ink(r,g,b,a)`, `ink("name")`,
`ink().write/line/box/circle/plot` chaining, `write(text, {x,y,size,center,font})`,
`line(x0,y0,x1,y1)`, `box(x,y,w,h,mode)`, `circle(x,y,r,filled)`, `plot(x,y)`,
`scroll(dx,dy)`, `blur(n)`, `zoom(n)`, `contrast(n)`, `spin(rad)`, `qr(text,x,y,s)`,
`painting(w,h,cb)`, `screen.width/height`, `params`, `colon`,
`sound.synth({...})` with `.kill()` and `.update()`, `sound.kill(id,fade)`,
`sound.speaker.waveforms/amplitudes/poll()`, `sound.bpm` (read-only property),
`clock.time()` (Date), `clock.resync()`, `event.is("...")`, `e.key`, `e.x/y`,
`num.clamp/rand/randIntRange/lerp/map/dist/floor/ceil/round/abs/sign/shiftRGB`,
`performance.now()`, `system.jump()`, `system.listPieces()`, `paintCount`, `simCount`,
`store` (stubs), `net` (stubs), `hud` (stub), `ui.Button` (stub constructor)

**Not available:** `num.randInt`, `num.randIntArr`, `num.radians`, `num.hslToRgb`,
`num.rgbToHsl`, `num.parseColor`, `num.arrMax`, `num.p2.*`, `help.choose`,
`help.repeat`, `help.resampleArray`, `lineAngle`, `poly`, `shape`, `stamp`,
`mask/unmask`, `paste`, `page`, `cursor()`, `resize()`, `resolution()`, `fps()`,
`geo.*`, `gizmo.*`, `text.box()`, `sound.bpm()` (as function), `sound.paint.*`,
`sound.progress()`, `needsPaint()`, `noise16*`, `pen.drawing/dragBox`,
`system.nopaint`, DOM/window/document

---

## GREEN: Works Now (0 runtime changes needed)

| # | Piece | Lines | Description | APIs Used |
|---|-------|-------|-------------|-----------|
| 1 | **3x3.mjs** | 45 | Keyboard pad instrument | `wipe("blue")`, `ink`, `e.is("keyboard:down")`, `e.key` |
| 2 | **404.mjs** | 57 | Error page | `wipe`, `ink`, `write`, `params`, `screen` |
| 3 | **beat.mjs** | 57 | Mouse percussion | `wipe("brown")`, `sound.synth()`, `e.device`, `e.button`, `e.is("touch")` |
| 4 | **brick-breaker.mjs** | 61 | Paddle+ball game | `wipe`, `ink().box("*center")`, `screen`, `num.randIntRange` |
| 5 | **dync.mjs** | 71 | Percussive pad | `wipe("blue")`, `ink`, `line`, `box` |
| 6 | **error.mjs** | 64 | Error display | `wipe("black")`, `ink([r,g,b]).write()`, `params`, `screen` |
| 7 | **gostop.mjs** | 35 | Go/stop rhythm | `wipe(r,g,b)`, `sound.synth({tone,beats,decay})` |
| 8 | **hop.mjs** | 61 | Jump game sketch | `wipe`, `ink`, `box`, `screen`, `num` |
| 9 | **shh.mjs** | 62 | Quiet mode | `wipe`, `ink`, `write` |
| 10 | **doodle.mjs** | 84 | Drawing canvas | `wipe`, `ink`, `plot`, `e.is("draw")`, `e.x/y` |
| 11 | **f3ral3xp.mjs** | 60 | Feral expression | `wipe`, `ink`, `write`, `box`, `screen` |
| 12 | **hha.mjs** | 16 | Happy hands | `wipe`, `ink`, `write` |
| 13 | **hw.mjs** | 9 | Hello world | `write` |
| 14 | **keys.mjs** | 114 | Keyboard input test | `wipe`, `ink`, `write`, `e.is("keyboard:*")`, `e.key` |
| 15 | **multipen.mjs** | 31 | Multi-touch test | `wipe`, `ink`, `circle`, `e` |
| 16 | **ptt.mjs** | 67 | Push-to-talk sketch | `wipe`, `ink`, `write`, `box` |

**Total: 16 pieces ready to ship.**

---

## YELLOW: Needs `num` Helpers (~20 LOC C each)

### `num.randInt(max)` — `return Math.floor(Math.random() * (max + 1))`

| # | Piece | Lines | Description | Other deps |
|---|-------|-------|-------------|------------|
| 17 | **sprinkles.mjs** | 32 | Random pixel emitter | `num.randInt`, `num.randIntArr`, cursor (ignored) |
| 18 | **starfield.mjs** | 99 | 3D starfield | `num.randInt`, `num.radians` |
| 19 | **metaballs.mjs** | 443 | Metaball visualization | `num.randInt`, screen.pixels |

### `num.randIntArr(max, count)` — returns array of N random ints

| # | Piece | Lines | Description | Other deps |
|---|-------|-------|-------------|------------|
| 20 | **squaresong.mjs** | 152 | Audiovisual composition | `num.randInt`, `num.randIntArr`, `num.rgbToHsl`, `sound.synth`, `paintCount` |

### `num.hslToRgb(h, s, l)` — color space conversion

| # | Piece | Lines | Description | Other deps |
|---|-------|-------|-------------|------------|
| 21 | **rainbow-x.mjs** | 80 | Rainbow X pattern | `num.hslToRgb`, `circle(x,y,r,true)` |

### `num.radians(deg)` — `deg * Math.PI / 180`

| # | Piece | Lines | Description | Other deps |
|---|-------|-------|-------------|------------|
| 22 | **starfield.mjs** | 99 | (see above) | `num.randInt` |

### `help.choose(...items)` — pick random element from args

| # | Piece | Lines | Description | Other deps |
|---|-------|-------|-------------|------------|
| 23 | **triquilt.mjs** | 103 | Quilt triangle patterns | `help.choose`, `shape()`, `fps()`, `ink("brown")` |

### `help.repeat(n, fn)` — call fn N times

(Already used by spline.mjs which has imports — no standalone pieces need this alone.)

### Implementation path
```c
// In js-bindings.c, add to num object setup:
// num.randInt(max) — random int from 0..max inclusive
static JSValue js_num_randint_single(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_NewInt32(ctx, 0);
    int max; JS_ToInt32(ctx, &max, argv[0]);
    return JS_NewInt32(ctx, rand() % (max + 1));
}

// num.randIntArr(max, count) — array of random ints
static JSValue js_num_randintarr(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_NewArray(ctx);
    int max, count; JS_ToInt32(ctx, &max, argv[0]); JS_ToInt32(ctx, &count, argv[1]);
    JSValue arr = JS_NewArray(ctx);
    for (int i = 0; i < count; i++)
        JS_SetPropertyUint32(ctx, arr, i, JS_NewInt32(ctx, rand() % (max + 1)));
    return arr;
}

// num.radians(deg)
// Can be done in JS shim: "num.radians = function(d) { return d * Math.PI / 180; }"

// num.hslToRgb(h, s, l) — standard HSL to RGB
// ~30 lines of C or inject as JS shim

// help.choose(...args) — random pick
// Can be done in JS shim: "help.choose = function() { return arguments[Math.floor(Math.random() * arguments.length)]; }"
```

**Total: ~60 LOC unlocks 7 more pieces.**

---

## ORANGE: Needs `sound.bpm()` as Function + `lineAngle`

### `sound.bpm(val)` — set BPM, return current

Currently `sound.bpm` is a read-only float. Pieces call `sound.bpm(120)` to set it.
Fix: replace with a function that updates `audio->bpm` and returns the current value.

### `lineAngle(x, y, length, angle_deg)` — draw line at angle

Used by metronome for pendulum animation.

| # | Piece | Lines | Description | Other deps |
|---|-------|-------|-------------|------------|
| 24 | **metronome.mjs** | 548 | Visual metronome + tap BPM | `lineAngle`, `sound.bpm()`, `sound.synth().progress()`, `circle(fill)`, `store` (guarded), `query` |
| 25 | **tone.mjs** | 100 | Single-tone synth | `sound.synth({duration:"🔁"})`, `.kill()`, `.update()`, `sound.paint.waveform`, `num.map/dist`, `help.resampleArray`, `hud.label` |

**tone.mjs** is particularly interesting — the core synth works now (duration `"🔁"` = infinity is already handled), `.kill()` and `.update()` are fixed. The `sound.paint.waveform` call is guarded. It would play tones correctly, just no waveform visualization.

**Total: ~40 LOC unlocks 2 more pieces.**

---

## BLUE: Needs `shape()` / `poly()` / `stamp()`

### `shape(points)` — fill a polygon from array of [x,y] pairs

| # | Piece | Lines | Description |
|---|-------|-------|-------------|
| 26 | **triquilt.mjs** | 103 | Quilt patterns (also needs `help.choose`, `fps()`) |
| 27 | **cards.mjs** | 361 | Card game |
| 28 | **ant.mjs** | 276 | Ant simulation |

### `stamp(painting, x, y)` — blit a painting buffer onto screen

| # | Piece | Lines | Description |
|---|-------|-------|-------------|
| 29 | **flap.mjs** | 93 | Frame animation (also needs `store`, `system.painting`) |

**Implementation:** `shape()` is a polygon fill — moderate effort (~80 LOC in graph.c).
`stamp()` is a framebuffer blit — also moderate (~40 LOC).

---

## PURPLE: Interesting but Complex

These have zero blockers in the scan but use APIs in ways that need verification.

| # | Piece | Lines | What to verify |
|---|-------|-------|----------------|
| 30 | **butterflies.mjs** | 589 | Large piece — uses `screen.pixels` direct access, multi-touch, `sound.synth`. Needs `screen.pixels` as writable Uint8ClampedArray. |
| 31 | **chord.mjs** | 159 | Uses `text.box()` for text measurement, `ink().line(obj)` with line-object syntax |
| 32 | **morpho.mjs** | 684 | Large generative piece — needs careful API audit |
| 33 | **deck.mjs** | 223 | Card deck — uses `jump()` |
| 34 | **boots.mjs** | 134 | Boot screen — needs careful read |
| 35 | **slip.mjs** | 200 | Uses sound + screen |

---

## Implementation Priority Path

### Wave 1: Ship Now (16 pieces, 0 work)
```
3x3  404  beat  brick-breaker  dync  error  gostop  hop
shh  doodle  f3ral3xp  hha  hw  keys  multipen  ptt
```

### Wave 2: num Helpers (~60 LOC C)
Add `num.randInt`, `num.randIntArr`, `num.radians`, `num.hslToRgb`, `help.choose`.
```
sprinkles  starfield  rainbow-x  squaresong  metaballs  triquilt(partial)
```
+6 pieces = 22 total

### Wave 3: Sound Functions (~40 LOC C)
Add `sound.bpm()` as callable, `lineAngle()`.
```
metronome  tone
```
+2 pieces = 24 total

### Wave 4: Polygon + Blit (~120 LOC C)
Add `shape(points)`, `stamp(painting, x, y)`.
```
triquilt  cards  ant  flap
```
+4 pieces = 28 total

### Wave 5: Screen Pixels + Advanced (~100 LOC C)
Expose `screen.pixels` as writable buffer, add `text.box()`.
```
butterflies  chord  morpho
```
+3 pieces = 31 total

---

## Build Script Addition

To bundle all Wave 1 pieces, add to `build-and-flash.sh`:

```bash
# Copy web pieces that run unmodified on native
AC_DISKS_DIR="${NATIVE_DIR}/../../system/public/aesthetic.computer/disks"
for web_piece in clock.mjs 3x3.mjs 404.mjs beat.mjs brick-breaker.mjs \
    dync.mjs error.mjs gostop.mjs hop.mjs shh.mjs doodle.mjs \
    f3ral3xp.mjs hha.mjs hw.mjs keys.mjs multipen.mjs ptt.mjs; do
    if [ -f "${AC_DISKS_DIR}/${web_piece}" ]; then
        cp "${AC_DISKS_DIR}/${web_piece}" "${INITRAMFS_DIR}/pieces/"
    fi
done
```

---

## Summary

| Wave | Pieces | Cumulative | Runtime LOC |
|------|--------|------------|-------------|
| 1 | 16 | 16 | 0 |
| 2 | 6 | 22 | ~60 |
| 3 | 2 | 24 | ~100 |
| 4 | 4 | 28 | ~220 |
| 5 | 3 | 31 | ~320 |

**31 web pieces running unmodified on AC Native with ~320 total lines of C.**
