# Stample Recording & Playback Bug Report

**Date:** 2026-02-07  
**File:** `system/public/aesthetic.computer/disks/stample.mjs`

## Symptoms
- Recording "only works once" — after the first recording, the strip buttons stop responding
- Audio goes silent, especially when switching between stample and notepat
- Sample playback doesn't recover after navigating away and back

## Root Causes Found

### Bug 1: Strip Button Stuck in `down=true` State (PRIMARY)
**Location:** `act()` → strip button `up` handler  
**Severity:** Critical

The `up` callback returned `false` when `e.pointer !== btn.downPointer` (pointer ID mismatch between touch-down and lift events). The UI system (`ui.mjs` line 459) interprets `return false` as "keep `btn.down = true`". This permanently blocked future presses because the `down` handler checks `if (btn.down) return false;` as a guard.

On touch devices, pointer IDs can change between down and up events. This made the button permanently stuck after one use.

**Fix:** Always return `true` from the `up` handler — kill the sound unconditionally and release the button.

### Bug 2: Dead Sound References in `sounds[]` Array
**Location:** `act()` and `sim()`  
**Severity:** Medium

After `sounds[index]?.kill(0.1)`, the array entry was NOT cleared (`sounds[index]` remained a dead sound object). In `sim()`, `sounds.find(s => s)` found these killed sounds, causing `bitmapProgress` tracking to poll dead objects.

**Fix:** Set `sounds[index] = undefined` after killing.

### Bug 3: Aggressive Mic Disconnect on Every Strip Press
**Location:** `act()` → strip button `down` handler  
**Severity:** High

Every strip button press called `sound.microphone.disconnect()`, which:
1. Called `detachMicrophone()` in bios
2. Disconnected the mic processor and mic node
3. **Stopped ALL media stream tracks** (`micStream.getTracks().forEach(t => t.stop())`)
4. Set `microphone.connected = false`

This completely destroyed the mic connection, requiring a full `getUserMedia` re-request. But `micRecordButtonLabel` wasn't updated to "Connect", so users thought they could record again immediately (it still said "Record"). They'd press Record, but it would reconnect instead. They had to press **twice** to record again.

**Fix:** Only interrupt the mic if it's actively recording (call `cut()` to stop the partial recording). Leave the connection intact otherwise. Also handle the `"microphone-disconnect"` event to update the button label.

### Bug 4: `microphone-disconnect` Not Dispatched as Act Event
**Location:** `system/public/aesthetic.computer/lib/disk.mjs` line 9631  
**Severity:** Medium

The `"microphone-disconnect"` message from bios was handled in the disk worker but NOT forwarded to `actAlerts`. So `e.is("microphone-disconnect")` in stample's `act()` never fired, meaning the button label couldn't be updated on disconnect.

**Fix:** Added `actAlerts.push("microphone-disconnect")` in the handler.

### Bug 5: No `leave()` Export for Cleanup
**Location:** module exports  
**Severity:** Low-Medium

Stample had no `leave()` function. When jumping to notepat, playing sounds and loops weren't explicitly killed. The bios **does** handle cleanup (`killAllSound`, `clearSoundSampleCache`, `detachMicrophone`), but having explicit cleanup prevents timing issues.

**Fix:** Added `leave()` that kills all active sounds, loops, and resets state.

## Notepat Interaction

When `jump("notepat:stample")` is called:
1. Bios `detachMicrophone()` disconnects the mic
2. Bios `killAllSound()` stops all speaker worklet sounds  
3. Bios `clearSoundSampleCache()` clears the worklet's cached buffers
4. Bios deletes all `sfx[]` entries except the default `sound`

Notepat then boots and:
- Loads the stample sample from IndexedDB (`store["stample:sample"]`)
- Re-registers it via `sound.registerSample()`
- Sets `wave = "stample"` from the colon parameter

When returning to stample:
- `boot()` re-runs, resets all state
- Preloads `"startup"` (default sample) first
- Then retrieves stored sample from IndexedDB, overriding the default
- Re-registers the sample

This flow is correct and should work, **provided the IndexedDB store was persisted**. The persist calls in the recording `up` handler look correct.

## Git History Summary

| Date | Commit | Description |
|------|--------|-------------|
| 2025-01-29 | 86cbd7d6 | Initial creation as `sprample` |
| 2025-01-29 | b5adea25 | Renamed to `stample`, sample spreading WIP |
| 2025-01-29 | 1e9d4380 | Add pats parameter support |
| 2025-01-30 | d87a5a22 | Stample update |
| 2025-02-04 | 7424d24c | Pitch shift test |
| 2025-02-04 | 63c31cd5 | Y-axis scrub test |
| 2025-02-23 | cf10ff3d | Sound system updates |
| 2025-03-02–07 | Multiple | Multi-touch, general WIP |
| 2025-03-25 | 7456c038 | Resize/framing fixes |
| 2025-03-29 | e3380ebe | Quick update for demo |
| 2025-03-30 | 346e72c0 | Stample todos |
| 2025-06-27 | 3ddb6d5f | UI pen updates |
| 2025-09-30 | 75d3cc36 | Fix stample scrub |
| 2026-01-24–26 | Multiple | Major refactor: responsive layout system, bitmap loading, notepat integration |
| 2026-01-27–28 | Multiple | KidLisp `$code` embedding, GPU effects, live audio updates |
| 2026-02-01 | c5d093e2 | Scrub debugging |
| 2026-02-05 | f15dd3f5 | Merge: accept remote stample.mjs |
| 2026-02-06 | 05010c1e | Fix duplicate `floor` declaration |
| 2026-02-07 | 806b4d04 | Stample fix (layout constants, KidLisp support) |

## Files Changed

- `system/public/aesthetic.computer/disks/stample.mjs` — 4 bug fixes
- `system/public/aesthetic.computer/lib/disk.mjs` — 1 fix (microphone-disconnect event dispatch)
