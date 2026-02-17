# Clock Parallel Track Timing Fix

## Problem Statement

When running `clock *lene` (melody: `^f..afafa...efefef..bababa.fgg..b...agfededcd {noise-white}-^f.fffffff`):

- **Track 1**: 31 notes, 13 beats (6500ms cycle)
- **Track 2**: 9 notes (1 rest + 8 f's), 12 beats (6000ms cycle)

**Expected**: Each track loops independently at its own duration.  
**Actual**: Track 2's first cycle shows "EARLY" because we skip leading rests on init.

## Root Cause Analysis

The `-` character creates a 4-beat rest at the start of Track 2. During initialization:
1. We skip leading rests by advancing `noteIndex` to the first audible note
2. **First cycle is shorter**: Only plays 8 beats (4000ms), not 12 beats (6000ms)
3. After loop 1, the rest offset (2000ms) is added, so subsequent cycles are correct

## Fix Applied

The expected duration calculation now accounts for the first cycle being shorter:

```javascript
const isFirstCycle = trackState.loopCount === 1;
const expectedDuration = isFirstCycle 
  ? (trackState.expectedCycleDuration || 0) - (trackState.initialRestOffset || 0)
  : (trackState.expectedCycleDuration || 0);
```

## Console Output (Now Clean)

Only cycle timing logs appear:
```
⏱️ T1 INIT | 13 beats = 6500ms expected cycle
⏱️ T2 INIT | 12 beats = 6000ms expected cycle (has leading rest)
⏱️ T2 loop 1 @ ...ms | cycle: 4000ms (expected: 4000ms ✓)  ← First cycle is 4000ms!
⏱️ T1 loop 1 @ ...ms | cycle: 6500ms (expected: 6500ms ✓)
⏱️ T2 loop 2 @ ...ms | cycle: 6000ms (expected: 6000ms ✓)  ← Subsequent cycles are 6000ms
⏱️ T1 loop 2 @ ...ms | cycle: 6500ms (expected: 6500ms ✓)
```

## Verbose Logs Removed

- `🎵 EARLY MELODY STATE SET`
- `🎵 Fetching cached melody`
- `🎵 Loaded cached melody`
- `🎵 Clock author`
- `🎵 About to process melody`
- `🎵 Original/Converted`
- `🎵 Parsed melodyTracks`
- `🎵 SIM FIRST RUN`
- `📐 RENDER GEOMETRY` (table)
- `✅ No overlapping boxes`
- `📱 HUD QR render check`
- `⌨️📍 [bios pointerup]`
- `⌨️🔴 [input blur event]`
- `🔊 Synth created` (from speaker.mjs)
- `🔊 process() call` (from speaker.mjs)
- `🔊 #processAudio` (from speaker.mjs)

## Files Changed

- `clock.mjs`: Fixed first-cycle expected duration, disabled geometry/melody debug logs
- `speaker.mjs`: Disabled synth creation and process() debug logs  
- `disk.mjs`: Disabled QR render debug log
- `bios.mjs`: Disabled keyboard input debug logs
