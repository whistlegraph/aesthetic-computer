# Study: Running clock.mjs Unmodified on AC Native

**Goal:** Determine if `system/public/aesthetic.computer/disks/clock.mjs` can run
as-is on AC Native (QuickJS runtime in `fedac/native/`) — zero changes to the .mjs.

## File Stats

- **Size:** ~283KB, ~7030 lines
- **Exports:** `boot`, `paint`, `sim`, `act`, `leave`, `background`
- **Imports:** 4 ES module imports from `../lib/`

---

## API Usage Audit

### Graphics Primitives

| API | Clock Usage | Native Status | Issue? |
|-----|------------|---------------|--------|
| `wipe(r,g,b)` | Yes (2x) | Implemented | |
| `wipe("black")`, `wipe("gray")` | Yes | **Missing** — native only handles numeric args | **BLOCKER** |
| `ink(r,g,b,a)` | Yes (46x) | Implemented | |
| `ink("red")`, `ink("cyan")`, etc. | Yes (heavily) | **Missing** — native only handles numeric args | **BLOCKER** |
| `ink().write()` chain | Yes (heavily) | Works — `ink()` returns `__paintApi` | |
| `ink().line()` chain | Yes | Works | |
| `ink().box()` chain | Yes | Works | |
| `write(text, {x, y, size, center})` | Yes (24x) | Implemented with center, size, font | |
| `line(x0,y0,x1,y1)` | Yes (12x) | Implemented | |
| `box(x,y,w,h)` | Yes (10x) | Implemented | |
| `screen.width/height` | Yes | Implemented | |

### Sound

| API | Clock Usage | Native Status | Issue? |
|-----|------------|---------------|--------|
| `sound.synth({type, tone, duration, volume, attack, decay})` | Yes (core feature) | Implemented — sine/triangle/sawtooth/square/noise | |
| `sound.synth.kill(fade)` | Yes | **Partial** — synth returns obj with `kill` as noop | **BLOCKER** |
| `sound.synth.update({volume, tone, pan})` | Yes | Implemented — `js_synth_obj_update` | |
| `sound.kill(id, fade)` | Yes | Implemented | |
| `sound.speaker.poll()` | Yes (1x) | Stub (noop) | OK (graceful) |
| `sound.speaker.waveforms/amplitudes` | Commented out | N/A | |
| `sound.bubble({...})` | Yes (bubble waveform) | **Missing** — no bubble synthesis | **DEGRADED** |
| `sound.tone` (read/write) | Yes (bubble sliding) | **Missing** | **DEGRADED** |
| `sound.registerSample()` | Yes (1x) | Stub (noop) | OK (graceful) |

### Clock / Time

| API | Clock Usage | Native Status | Issue? |
|-----|------------|---------------|--------|
| `clock.resync()` | Yes (3x) | Stub (noop) | OK (graceful) |
| `clock.time()` | Yes (3x, critical) | **Missing** — not defined on clock object | **BLOCKER** |

### Network / Storage

| API | Clock Usage | Native Status | Issue? |
|-----|------------|---------------|--------|
| `fetch('/api/store-clock', ...)` | Yes (boot, caching) | **Missing** — global `fetch` returns `{ok:false}` | **DEGRADED** (has fallback) |
| `net.preload("startup")` | Yes (1x) | Stub (resolved promise) | OK (graceful) |
| `store.retrieve("stample:sample", "local:db")` | Yes (1x) | Stub (returns null) | OK (graceful) |

### UI / HUD

| API | Clock Usage | Native Status | Issue? |
|-----|------------|---------------|--------|
| `hud.label(...)` | Yes (6x) | **Missing** — not in native API | **DEGRADED** (guarded with `typeof hud !== "undefined"`) |
| `hud.supportsInlineColor` | Yes | **Missing** | **DEGRADED** (guarded) |
| `ui.Button(x, y, w, h)` | Yes (2x octave buttons) | Stub constructor | **DEGRADED** (won't render/respond) |
| `typeface.glyphs` | Yes (measuring text) | **Missing** — typeface is a noop function | **DEGRADED** (guarded with `?.`) |

### Other APIs

| API | Clock Usage | Native Status | Issue? |
|-----|------------|---------------|--------|
| `api.send({type, content})` | Yes (3x) | **Missing** | **DEGRADED** (guarded) |
| `api.system?.taping` | Yes (1x) | **Missing** | OK (optional chaining) |
| `speak(text, voice, "cloud", opts)` | Yes (speech synthesis) | **Missing** | **DEGRADED** (guarded) |
| `help.resampleArray()` | Commented out | N/A | |
| `params`, `colon` | Yes | Implemented | |
| `num.clamp/lerp/map/rand/etc` | Yes | Implemented | |
| `performance.now()` | Yes | Available in QuickJS | |
| `console.log` | Yes (heavily) | Available | |
| `Date` / `Date.now()` | Yes | Available in QuickJS | |
| `paintCount` / `simCount` | Not used | N/A | |
| `background` export | Yes | **Unknown** — native may not check this | Minor |

### ES Module Imports

| Import | Native Status | Issue? |
|--------|---------------|--------|
| `../lib/melody-parser.mjs` | Resolves to `/lib/melody-parser.mjs` — **must exist in initramfs** | **BLOCKER** (not currently bundled) |
| `../lib/notepat-convert.mjs` | Same — must exist in initramfs | **BLOCKER** |
| `../lib/note-colors.mjs` | Same — must exist in initramfs | **BLOCKER** |
| `../lib/pixel-sample.mjs` | Stubbed in module loader (returns null functions) | OK |

---

## Blockers (Must Fix in Native Runtime, NOT in clock.mjs)

### 1. String Color Names in `wipe()` and `ink()`
Clock uses `wipe("black")`, `wipe("gray")`, `ink("red")`, `ink("cyan")`, `ink("white")`,
`ink("yellow")`, `ink("gray")`, `ink("black")` extensively. Native only parses numeric args.

**Fix:** Add a color name lookup table in `js_wipe` and `js_ink` — check `JS_IsString`,
map "black"→(0,0,0), "white"→(255,255,255), "red"→(255,0,0), "cyan"→(0,255,255),
"gray"→(128,128,128), "yellow"→(255,255,0).

**Effort:** ~30 lines of C.

### 2. `clock.time()` Missing
Clock calls `clock.time()` in paint and sim to get the current synced time.
Native only has `clock.resync` (noop). Without `clock.time()`, the clock display
and melody timing are completely broken.

**Fix:** Add `clock.time()` that returns a JS Date object (or a date-like object
with `.getHours()`, `.getMinutes()`, `.getSeconds()`, `.getMilliseconds()`).
Can use `gettimeofday()` or `clock_gettime(CLOCK_REALTIME)`.

**Effort:** ~40 lines of C.

### 3. `sound.synth` Return Object `.kill()` is Noop
The synth return object has `kill` bound to `js_noop`. Clock relies on calling
`synthInstance.kill(fadeTime)` to stop notes. Without this, notes play forever
(or until duration expires), causing audio pileup.

**Fix:** The synth return object needs a proper `kill` method that calls
`audio_voice_kill(audio, id, fade)` using the stored voice ID.

**Effort:** ~20 lines of C.

### 4. Library Files Not in Initramfs
`melody-parser.mjs`, `notepat-convert.mjs`, and `note-colors.mjs` are in
`system/public/aesthetic.computer/lib/` but not bundled into the native initramfs.
The module loader will fail to find them and stub them as empty, crashing the piece.

**Fix:** Copy these 3 files into the initramfs `/lib/` directory during
`build_initramfs()` in `ac-os`, similar to how `kidlisp.mjs` is handled.
These are pure JS with no browser dependencies — they should work in QuickJS as-is.

**Effort:** ~5 lines of shell in `ac-os`.

---

## Degraded Features (Work Without Changes, Just Missing Functionality)

| Feature | Impact | Notes |
|---------|--------|-------|
| Bubble waveform | Falls back to synth or silent | Only affects `{bubble}` melody modifier |
| `fetch('/api/store-clock')` | Can't load cached melodies via `*code` | Melody params still work directly |
| `speak()` / TTS | No speech synthesis | Guarded, just skipped |
| HUD label | No melody preview in header | Guarded with typeof check |
| `ui.Button` | Octave +/- buttons won't work | Keyboard arrows still work |
| `typeface.glyphs` | Can't measure text precisely | Uses fallback values via `?.` |
| `api.send()` | No inter-piece messaging | Piece still functions |
| `store.retrieve()` | No stample sample loading | Returns null gracefully |

---

## Verdict

**Not yet feasible as-is**, but **very close**. There are 4 blockers, all fixable
on the native runtime side without touching clock.mjs:

1. String color names in `ink()`/`wipe()` — ~30 LOC C
2. `clock.time()` returning a Date — ~40 LOC C
3. Synth `.kill()` on return objects — ~20 LOC C
4. Bundle 3 lib files in initramfs — ~5 LOC shell

**Total estimated work: ~95 lines of code changes to the native runtime.**

Once these 4 items are addressed, clock.mjs would boot, display the time,
play melodies via keyboard/params, and render the full timeline visualization —
all without changing a single line of the piece. The degraded features (speech,
bubble waveform, HUD, cached melodies, stample) are all gracefully guarded and
would simply be inactive on native.

This would be a strong proof-of-concept for running web pieces unmodified on
AC Native, and would validate the API compatibility approach for future pieces.
