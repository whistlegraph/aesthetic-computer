# $code Loading Issue Investigation

**Date:** January 3, 2026  
**Status:** In Progress  
**Symptom:** `$roz` (cached KidLisp code) doesn't load from prompt, but pasting the same source code works fine.

## Summary

When entering `$roz` in the prompt, the piece never boots even though all the message passing appears to work correctly. The user remains stuck at the prompt.

## Working Flow (Pasting Source Code)

When pasting the actual KidLisp source code, everything works:
1. Source passed to `load()` 
2. `lisp.module()` compiles it
3. `disk-loaded` sent → `loading-complete` received → `hotSwap()` runs
4. Next frame: `paintCount === 0n && loading === false` → boot() runs
5. Piece displays correctly

## Broken Flow ($roz)

When entering `$roz`:
1. ✅ `getCachedCodeMultiLevel("roz")` fetches the source (239 chars)
2. ✅ `lisp.module()` compiles it successfully
3. ✅ `disk-loaded` sent to bios
4. ✅ `loading-complete` sent back to disk
5. ✅ `hotSwap()` runs:
   - `paintCount` reset from 497 to `0n`
   - `loading` set to `false`
   - `booted` set to `false`
6. ❌ **Boot never runs** - no frames show `paintCount=0n` after hotSwap

## Console Log Analysis

```
// Before loading-complete arrives:
🔍 DISK: Boot check - paintCount=496, loading=true, booted=true

// loading-complete arrives and hotSwap runs:
🔍 DISK: Received loading-complete, hotSwap exists? true
🔍 DISK hotSwap: Starting, loadedModule exists? true, has boot? true, has paint? true
🔍 DISK hotSwap: Resetting paintCount from 497 to 0n
🔍 DISK: hotSwap called, loading = false

// AFTER this point - NO MORE FRAME LOGS APPEAR
// The boot check should show: paintCount=0n, loading=false, booted=false
// But we never see it!
```

## Key Observation

After `hotSwap()` completes, frame messages continue (render loop is still running), but:
- No `🔍 DISK FRAME:` logs appear (added for `paintCount < 3n`)
- No `🔍 DISK: Boot check` logs appear

This suggests either:
1. Frames aren't reaching the disk worker after hotSwap
2. Something is blocking/returning early in frame processing
3. The frame handler is stuck or erroring silently

## Relevant Code Locations

- **disk.mjs load()**: Lines 6442+ - loads cached codes via `getCachedCodeMultiLevel()`
- **disk.mjs hotSwap()**: Line 7861 - resets state after loading-complete
- **disk.mjs frame handler**: Line 9961 - processes frames, runs boot check
- **disk.mjs boot check**: Line 11479 - `if (paintCount === 0n && loading === false)`
- **bios.mjs receivedChange()**: Line 3918 - receives messages from worker
- **bios.mjs loading-complete**: Line 11462 - sends loading-complete to disk

## Debug Logs Added

1. **disk.mjs frame handler** (line 9961):
   ```javascript
   if (paintCount < 3n) {
     console.log(`🔍 DISK FRAME: paintCount=${paintCount}, loading=${loading}, booted=${booted}, needsRender=${content.needsRender}`);
   }
   ```

2. **disk.mjs boot check** (line 11479):
   ```javascript
   console.log(`🔍 DISK: Boot check - paintCount=${paintCount}, loading=${loading}, booted=${booted}`);
   ```

3. **bios.mjs onMessage** (line 3363):
   ```javascript
   if (m?.data?.type && m.data.type !== "frame") {
     console.log(`🔍 BIOS onMessage: type="${m.data.type}"`);
   }
   ```

## Theories

### Theory 1: Frame messages blocked after hotSwap
Something in the hotSwap or immediately after might be blocking frame processing.

### Theory 2: Worker message queue issue
The worker might have messages queued that interfere with the new piece's frame handling.

### Theory 3: State not properly reset
Some state variable might not be properly reset, causing frames to be ignored.

### Theory 4: Race condition with boot-file setTimeout
The `boot-file` handler uses `setTimeout` to defer tokenizer work. This setTimeout executes AFTER hotSwap completes, which might cause interference:
```
🔍 BIOS: boot-file setTimeout executing for "aesthetic.computer/disks/$roz"
```

## Next Steps

1. Add more logging at the very start of frame handler (before any checks)
2. Check if frames are being sent by bios but not received by disk
3. Investigate if there's an error being swallowed
4. Check if the GOL transition system is interfering
5. Verify that the worker isn't stuck in some loop

## Files Modified During Investigation

- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`
  - Added error catching wrapper around `makeFrame(e)` in onmessage handler
  - Added logging at makeFrame entry point for all messages when `paintCount < 5n`
  - Added logging in frame handler when `paintCount < 5n`
  - Added logging in `loading-complete` handler before/after hotSwap
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`

## Diagnostic Changes (2026-01-03)

Added the following debug logging to trace the issue:

1. **onmessage wrapper** - Catches any swallowed async errors:
   ```javascript
   makeFrame(e).catch(err => {
     console.error("🛑 DISK makeFrame error:", err, "| type:", e.data?.type);
   });
   ```

2. **makeFrame entry** - Logs all non-beat messages when paintCount < 5n:
   ```javascript
   if (paintCount < 5n && type !== "beat") {
     console.log(`🔍 DISK makeFrame: type="${type}", paintCount=${paintCount}`);
   }
   ```

3. **Frame handler** - Logs frame state:
   ```javascript
   if (paintCount < 5n) {
     console.log(`🔍 DISK FRAME: paintCount=${paintCount}, loading=${loading}, booted=${booted}`);
   }
   ```

4. **loading-complete handler** - Logs state before/after hotSwap:
   ```javascript
   console.log("🔍 DISK: Received loading-complete, paintCount=", paintCount, ...);
   console.log("🔍 DISK: After hotSwap, paintCount=", paintCount, ...);
   ```
