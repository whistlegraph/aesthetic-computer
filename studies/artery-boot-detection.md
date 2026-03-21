# Study: Artery Boot Detection & Readiness Telemetry

**Date:** 2026-02-21
**Context:** The `test-line.mjs` brush test runs before AC finishes booting. The notepat tests use `waitForBoot()` but it may not work correctly. This study maps the full boot signal graph to build reliable piece-readiness detection.

---

## The Problem

When an artery test calls `client.jump('line')`, the browser navigates to a new URL. The entire AC runtime re-boots from scratch — boot.mjs loads bios.mjs loads the piece. Tests that send pointer events before this completes get silently swallowed.

Current tests handle this two ways:
1. **Hardcoded `sleep()`** — `test-line.mjs` uses `sleep(3000)` after each jump
2. **`waitForBoot()`** — polls `window.currentPiece` which is **broken** (see below)

Neither is reliable.

---

## The Boot Signal Graph

Here's the complete timeline of what happens when AC loads a piece, with every observable `window.*` signal annotated:

```
 NAVIGATION (client.jump sets window.location.href)
 │
 ├── boot.mjs starts
 │   ├── window.acBOOT_START_TIME = performance.now()
 │   ├── window._bootTimings = []
 │   ├── window.acSTARTING_PIECE = "prompt" (or from URL)
 │   ├── window.acHIDE_BOOT_LOG = hideBootLog    ← [SIGNAL A] boot.mjs loaded
 │   ├── window.acBOOT_LOG = bootLog
 │   ├── window.acAuthTiming = { bootStart, ... }
 │   │
 │   └── bios.mjs boot() begins
 │       ├── let currentPiece = null               ← LOCAL variable, NOT on window
 │       ├── window.acDISABLE_HUD_LABEL_CACHE = true
 │       ├── window.acDISK_SEND = function(...)
 │       ├── window.acPieceState = null
 │       ├── window.acGetState = () => (...)
 │       │
 │       ├── Disk worker/module loads
 │       │   └── window.acSEND = send              ← [SIGNAL B] worker connected
 │       │
 │       ├── Piece module loads
 │       │   └── currentPiece = content.path        ← LOCAL, invisible to tests
 │       │
 │       ├── "disk-loaded-and-booted" message
 │       │   ├── window.preloaded = true            ← [SIGNAL C] piece loaded
 │       │   ├── postMessage({ type: "ready" })
 │       │   └── setTimeout(500ms) → acHIDE_BOOT_LOG()
 │       │
 │       └── "piece-paint-ready" message
 │           └── window.acHIDE_BOOT_LOG()           ← [SIGNAL D] first paint done
 │
 FOR NOPAINT/BRUSH PIECES:
 │
 ├── Piece boot() calls nopaint_boot()
 │   ├── system.painting must exist
 │   ├── system.nopaint.buffer created
 │   └── system.nopaint initialized                ← [SIGNAL E] brush ready
 │
 FOR AUDIO PIECES:
 │
 └── User interaction triggers activateSound
     └── window.audioWorkletReady = true            ← [SIGNAL F] audio ready
```

---

## The `waitForBoot()` Bug

**File:** `artery/artery.mjs:427-461`

```javascript
async waitForBoot(timeoutMs = 15000) {
  while (Date.now() - startTime < timeoutMs) {
    const bootStatus = await this.eval(`({
      hasBios: typeof window.currentPiece !== 'undefined',  // ← WRONG
      currentPiece: window.currentPiece || null,            // ← ALWAYS null
      ...
    })`);
    if (bootStatus.currentPiece) {  // ← NEVER true
      return true;
    }
    await new Promise(r => setTimeout(r, 100));
  }
  darkLog('⚠️  Boot timeout - proceeding anyway');  // ← ALWAYS hits this
  return false;
}
```

**Why it's broken:** `currentPiece` is declared as `let currentPiece = null` inside the `boot()` function closure in `bios.mjs:804`. It's never assigned to `window.currentPiece`. The check `typeof window.currentPiece !== 'undefined'` evaluates to `false` (it IS undefined), and `bootStatus.currentPiece` is always `null`.

**Result:** `waitForBoot()` always times out after 15 seconds, then proceeds anyway. Tests that call it simply wait 15 seconds for nothing.

---

## Reliable Readiness Signals

### Signal A: `window.acHIDE_BOOT_LOG` (boot.mjs loaded)
- **When:** Very early — boot.mjs top-level code, before bios even starts
- **Checks:** `typeof window.acHIDE_BOOT_LOG === 'function'`
- **Useful for:** Confirming the page loaded at all
- **Not sufficient for:** Piece readiness

### Signal B: `window.acSEND` (disk worker connected)
- **When:** After bios creates the disk worker/module and connects
- **Checks:** `typeof window.acSEND === 'function'`
- **Useful for:** Confirming the runtime is operational
- **Not sufficient for:** Knowing WHICH piece loaded

### Signal C: `window.preloaded` (piece loaded and booted)
- **When:** After `"disk-loaded-and-booted"` message from the disk
- **Checks:** `window.preloaded === true`
- **Useful for:** Confirming the current piece finished its `boot()` function
- **Caveat:** Reset to `false` on every piece navigation (`bios.mjs:12767`)

### Signal D: Boot canvas hidden (first paint)
- **When:** After `"piece-paint-ready"` or 500ms fallback after disk-loaded
- **Checks:** Boot canvas DOM element visibility (indirect)
- **Useful for:** Confirming the piece has painted at least one frame

### Signal E: Nopaint ready (brush pieces)
- **When:** After `nopaint_boot()` completes in the piece's `boot()` function
- **Checks:** `window.acSYSTEM?.nopaint?.buffer` exists, `window.acSYSTEM?.painting?.pixels?.length > 0`
- **Caveat:** Requires `window.acSYSTEM` to be exposed — need to verify this exists

### Signal F: `window.audioWorkletReady` (audio pieces)
- **When:** After speaker AudioWorklet connects following user interaction
- **Checks:** `window.audioWorkletReady === true`
- **Only relevant for:** Audio-dependent tests

---

## Full Cross-Examination: All 24 Artery Tests

### Audit Summary

Every `test-*.mjs` file was examined for its boot/readiness strategy. **Zero** tests use `window.preloaded`. Most rely on hardcoded `sleep()` or the broken `waitForBoot()`.

| Test | Wait Strategy | Timeout | Reconnects? | activateAudio? | Verdict |
|------|--------------|---------|-------------|----------------|---------|
| `test-line.mjs` | `waitForPiece()` | 10s poll | No | No | **FIXED** — now uses signal-based polling |
| `test-notepat-quick.mjs` | `sleep(2000)` | 2s fixed | No | Yes | FRAGILE — hardcoded sleep, then broken waitForBoot via activateAudio |
| `test-notepat.mjs` | `sleep(2000)` + reconnect + `activateAudio()` | 2s + 15s timeout | Yes | Yes | BROKEN — waitForBoot always times out (15s wasted) |
| `test-hiphop.mjs` | `sleep(2000)` + reconnect + `activateAudio()` | 2s + 15s timeout | Yes | Yes | BROKEN — same 15s waste |
| `test-trapwaltz.mjs` | `sleep(2000)` + reconnect + `activateAudio()` | 2s + 15s timeout | Yes | Yes | BROKEN — same 15s waste |
| `test-notepat-bach-prelude.mjs` | `sleep(2000)` + reconnect + `activateAudio()` | 2s + 15s timeout | Yes | Yes | BROKEN — same pattern |
| `test-notepat-stability.mjs` | `sleep(2000)` + `activateAudio()` | 2s + 15s timeout | No | Yes | BROKEN — same pattern |
| `test-notepat-latency.mjs` | `sleep(2000)` + reconnect + `activateAudio()` | 2s + 15s timeout | Yes | Yes | BROKEN — same pattern |
| `test-metronome.mjs` | `sleep(1000)` + `click()` | 1s fixed | No | No (manual click) | FRAGILE — short sleep, manual audio via click |
| `test-weather.mjs` | `sleep(2000)` | 2s fixed | No | No | FRAGILE — hardcoded |
| `test-prompt-curtain.mjs` | `cdp.waitForReady(15000)` | 15s poll | No | No | OK — polls readyState+bios, sufficient for prompt |
| `test-prompt-interaction.mjs` | None | N/A | No | No | RISKY — no wait at all, assumes already loaded |
| `test-jump.mjs` | `sleep(1000)` | 1s fixed | No | No | FRAGILE — short hardcoded sleep |
| `test-cards.mjs` | `sleep(1000)` | 1s fixed | No | No | FRAGILE — short hardcoded sleep |
| `test-1v1-split.mjs` | `sleep(2000)` | 2s fixed | No | No | FRAGILE — hardcoded |
| `test-1v1-multiplayer.mjs` | `sleep(4000)` | 4s fixed | No | No | FRAGILE — long but still arbitrary |
| `test-1v1-interactive.mjs` | Custom `Camdoll.isReady()` | Variable | No | No | OK — has its own readiness check |
| `test-1v1-udp.mjs` | N/A (diagnostic) | N/A | No | No | N/A — doesn't navigate |
| `test-kidlisp.mjs` | Custom `waitForReady()` | 10s poll | No | No | OK — polls page readiness |
| `test-kidlisp-access.mjs` | None | N/A | No | No | N/A — Monaco editor diagnostic |
| `test-panel-open.mjs` | None | N/A | No | No | N/A — only tests panel open/close |
| `test-perf-report.mjs` | None | N/A | No | No | N/A — reads perf data, no navigation |
| `test-module-loader.mjs` | `sleep(500)` | 500ms fixed | No | No | FRAGILE — very short |
| `test-news-youtube.mjs` | None | N/A | No | No | N/A — YouTube embed diagnostic |
| `test-playlist-memory.mjs` | `cdp.waitForReady(15000)` | 15s poll | No | No | OK — polls readyState+bios |

### Verdict Categories

**FIXED (1):** `test-line.mjs` — now uses `waitForPiece()` with `window.preloaded` polling.

**BROKEN (6):** All notepat/music tests call `activateAudio()` which calls the broken `waitForBoot()`. This always times out at 15 seconds because `window.currentPiece` doesn't exist. The fix to `waitForBoot()` (now polling `window.preloaded`) will automatically fix all 6.

**FRAGILE (7):** Use hardcoded `sleep()` with arbitrary durations. Work on fast systems, fail on slow ones. Should migrate to `waitForPiece()`.

**OK (4):** Use proper polling (`waitForReady`, custom readiness checks, or `waitForPiece`).

**N/A (6):** Diagnostic/non-navigation tests that don't need boot waiting.

### The Reconnect Pattern

5 tests do `client.close()` + `client.connect()` after `jump()`. This is because CDP's WebSocket connection can break when `window.location.href` triggers a full page reload. The CDP target changes — the old WebSocket is dead.

Tests that reconnect:
- `test-notepat.mjs`
- `test-hiphop.mjs`
- `test-trapwaltz.mjs`
- `test-notepat-bach-prelude.mjs`
- `test-notepat-latency.mjs`

Tests that don't reconnect may work because Artery's `connect()` auto-finds the new target, or because the CDP connection survives SPA-style navigations in certain environments.

### Impact of the `waitForBoot()` Fix

The fix changes `waitForBoot()` to poll `window.preloaded === true` + `window.acSEND` instead of the nonexistent `window.currentPiece`. This means:

- **6 BROKEN tests** will now boot-detect correctly (fast return instead of 15s timeout)
- **Typical boot time** should be ~1-3 seconds, saving 12-14 seconds per test run
- `activateAudio()` calls `waitForBoot()` internally, so all audio tests benefit automatically

---

## Applied Fix (2026-02-21)

### 1. Fixed `waitForBoot()` in `artery/artery.mjs`

Changed from polling the nonexistent `window.currentPiece` to polling real signals:

```javascript
async waitForBoot(timeoutMs = 15000) {
  while (Date.now() - startTime < timeoutMs) {
    const bootStatus = await this.eval(`({
      preloaded: window.preloaded === true,       // ← Signal C: piece booted
      hasSend: typeof window.acSEND === 'function', // ← Signal B: worker ready
      hasActivateSound: typeof window.activateSound === 'function',
      canvasExists: !!document.querySelector('canvas'),
      locationPath: window.location?.pathname || '',
      hasAudioContext: !!window.audioContext
    })`);

    if (bootStatus.preloaded && bootStatus.hasSend) {  // ← Real signals
      return true;
    }
    await new Promise(r => setTimeout(r, 100));
  }
  return false;
}
```

This automatically fixes the 6 BROKEN audio tests since `activateAudio()` calls `waitForBoot()` internally.

### 2. Added `waitForPiece()` to `artery/artery.mjs`

New method for targeted piece-readiness detection:

```javascript
async waitForPiece(expectedPath = null, timeoutMs = 10000) {
  while (Date.now() - startTime < timeoutMs) {
    const status = await this.eval(`({
      preloaded: window.preloaded === true,
      hasSend: typeof window.acSEND === 'function',
      path: window.location?.pathname || '',
      hasCanvas: !!document.querySelector('canvas'),
    })`);

    const pathOk = !expectedPath || status.path?.includes(expectedPath);
    if (status.preloaded && status.hasSend && pathOk) {
      return { ready: true, ...status };
    }
    await new Promise(r => setTimeout(r, 150));
  }
  return { ready: false, timedOut: true };
}
```

### 3. Updated `test-line.mjs`

Replaced `sleep(3000)` + `sleep(2000)` with signal-based waiting:

```javascript
await client.jump('new~128');
const newResult = await client.waitForPiece('new', 10000);  // Polls, returns fast

await client.jump('line');
const lineResult = await client.waitForPiece('line', 10000); // Polls, returns fast
```

### Future: `waitForNopaint()` (brush pieces)

Not yet implemented — requires verifying `window.acSYSTEM` is exposed at runtime:

```javascript
async waitForNopaint(timeoutMs = 8000) {
  // Polls for system.painting + system.nopaint.buffer
  // Would provide brush-specific readiness
}
```

---

## The `window.preloaded` Lifecycle

This is the most useful signal. Here's its full lifecycle:

```
1. boot() starts           → window.preloaded is undefined (fresh page)
2. Piece navigated away    → window.preloaded = false      (bios.mjs:12767)
3. New piece loads + boots → window.preloaded = true        (bios.mjs:16642)
4. Next navigation         → window.preloaded = false       (reset again)
```

**Key insight:** Since `client.jump()` sets `window.location.href`, the entire page reloads. So `window.preloaded` starts as `undefined`, then becomes `true` once the piece is fully loaded. Polling for `window.preloaded === true` is the correct general-purpose check.

---

## Recommended Test Pattern for Brush Tests

```javascript
// 1. Navigate
await client.jump('new~128');

// 2. Wait for AC to fully boot with the new painting
const bootResult = await client.waitForPiece('new', 10000);
if (!bootResult.ready) throw new Error('Boot timeout on new~128');

// 3. Navigate to brush
await client.jump('line');

// 4. Wait for piece + nopaint to be ready
const pieceResult = await client.waitForPiece('line', 8000);
if (!pieceResult.ready) throw new Error('Piece timeout on line');

// 5. (Optional) Wait specifically for nopaint painting buffer
const nopaintResult = await client.waitForNopaint(5000);
if (!nopaintResult.ready) console.warn('Nopaint not fully initialized');

// 6. Now safe to draw
await drawStroke(client, points);
```

---

## Open Questions

1. **Is `window.acSYSTEM` exposed?** Need to verify at runtime whether the system object is accessible from `window` scope. If not, we may need to expose it for testing.

2. **Does `jump()` (location.href change) cause a full page reload?** If so, `window.preloaded` starts fresh each time, which is clean. If it's a SPA-style navigation, the signal lifecycle may be different.

3. **Should we add a dedicated `window.acPIECE_READY` signal?** A simple boolean set right after `currentPiece` is assigned would solve the entire detection problem cleanly.

4. **Race condition on `window.preloaded`:** If the piece loads extremely fast (cached), could `preloaded` become `true` before our first poll? In theory yes, but that's fine — we'd catch it on the first poll iteration.

---

## Files Referenced

| File | Key Lines | Signal |
|------|-----------|--------|
| `boot.mjs` | 66, 172, 348, 351, 375 | `acBOOT_START_TIME`, `_bootTimings`, `acHIDE_BOOT_LOG`, `acBOOT_LOG`, `acAuthTiming` |
| `bios.mjs` | 651-654, 700-711, 804, 4283-4294, 12717, 12767, 16637-16677 | `acDISK_SEND`, `acSEND`, `acGetState`, `currentPiece` (local), `preloaded` |
| `artery.mjs` | 427-490 | `waitForBoot()` (FIXED), `waitForPiece()` (NEW) |
| `cdp.mjs` | 237-252 | `waitForReady()` (DOM-only) |
| `nopaint.mjs` | 67-91 | `nopaint_boot()` initialization |

## Change Log

- **2026-02-21:** Initial study. Fixed `waitForBoot()` to use `window.preloaded`. Added `waitForPiece()`. Updated `test-line.mjs`. Full 24-test audit.
