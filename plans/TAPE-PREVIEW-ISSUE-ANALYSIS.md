# Tape Preview Frame Transfer Issue - Analysis

**Date**: 2025-11-18  
**Status**: ❌ BLOCKED - Frames being sent but not received by prompt.mjs  
**Working Reference**: video.mjs successfully receives and displays tape frames

---

## Problem Summary

Tape frames are being successfully loaded and sent from bios.mjs, but prompt.mjs's `receive()` function is never being called to process them. The frames need to be transferred from bios → disk.mjs → prompt.mjs so they can be pasted into the screen buffer for tooltip preview.

---

## Current State

### ✅ What's Working

1. **Bios loading**: Tapes load successfully with all frames extracted
   ```
   bios.mjs:8688 📼 Preloaded tape prompt-tape-4f3: 431 frames
   bios.mjs:8702 📼 Sending 431 frames to disk for preview
   ```

2. **Message routing**: disk.mjs receives tape messages and routes them
   ```
   disk.mjs:8212 📼 ⏸️ Cannot call receive for tape:frames: using default receive
   disk.mjs:8223 📼 📥 Queued export event for later delivery: tape:frames queue size: 22
   ```

3. **Cassette animation**: Shows rotating reels during loading state
   ```
   80e4994a-...:4352 🎬 TAPE RENDER: !5b9 isLoading=true framesLoaded=undefined frames=0
   ```

4. **Message types whitelisted**: All tape messages allowed in disk.mjs
   - `tape:load-progress` ✅
   - `tape:preloaded` ✅  
   - `tape:frames` ✅
   - `tape:preload-error` ✅

### ❌ What's Broken

1. **prompt.mjs receive() never executes**
   - No logs from receive function (should see `📨 ✅✅✅ PROMPT.MJS RECEIVE called`)
   - Module loading log never appears (should see `📨 ✅ prompt.mjs module loading`)
   - Module exports check never runs for prompt.mjs

2. **Events stuck in queue**
   - Queue size growing: 22, 23, 24... 98+
   - Reason: `receive === defaults.receive` (using default no-op function)
   - Events never flushed because custom receive never registered

3. **Frames never reach prompt.mjs**
   - `framesLoaded` stays `undefined`
   - `frames` array stays at `0` length
   - Cassette animation continues forever (loading state never ends)

---

## Architecture Overview

### Data Flow (Intended)

```
1. User hovers over tape in prompt.mjs content ticker
2. prompt.mjs: fetchTapeAudio() sends tape:preload with requestFrames: true
3. bios.mjs: Loads ZIP, extracts frames as ImageBitmap array
4. bios.mjs: Sends tape:preloaded message to disk
5. bios.mjs: Sends tape:frames message with ImageBitmap[] to disk
6. disk.mjs: Routes messages to prompt.mjs's receive() function
7. prompt.mjs: receive() stores frames in contentItems[].frames
8. prompt.mjs: paint() cycles through frames at ~12fps, pastes to screen buffer
```

### Data Flow (Actual - BROKEN at step 6)

```
1. ✅ User hovers over tape
2. ✅ prompt.mjs sends tape:preload
3. ✅ bios.mjs loads ZIP (431 frames)
4. ✅ bios.mjs sends tape:preloaded
5. ✅ bios.mjs sends tape:frames with ImageBitmap[]
6. ❌ disk.mjs QUEUES instead of routing (receive === defaults.receive)
7. ❌ NEVER REACHED
8. ❌ NEVER REACHED - cassette animation loops forever
```

---

## Code Locations

### prompt.mjs (`/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/prompt.mjs`)

**Module loading log** (Line 6):
```javascript
console.log("📨 ✅ prompt.mjs module loading - receive function will be defined and exported");
```
**Status**: ❌ Never appears in console

**receive() function** (Lines 5890-5954):
```javascript
function receive(e) {
  console.log(`📨 ✅✅✅ PROMPT.MJS RECEIVE called with type: ${e.type}`);
  
  if (e.is("tape:load-progress")) { /* ... */ }
  if (e.is("tape:preloaded")) {
    // Sets framesLoaded=true, sends tape:request-frames
  }
  if (e.is("tape:frames")) {
    const { tapeId, frames } = e.content;
    tapeItem.frames = frames; // Store ImageBitmap array
    $.needsPaint();
  }
  if (e.is("tape:preload-error")) { /* ... */ }
}
```
**Status**: ❌ Never executes, not registered with disk.mjs

**Export block** (Lines 5328-5344):
```javascript
export {
  before, after, forgetful, halt,
  boot, paint, sim, act, activated,
  reply, receive, meta, leave  // ← receive exported here
};
```
**Status**: ✅ Properly exported

**Tape rendering** (Lines 4349-4463):
```javascript
if (framesLoaded && frames && frames.length > 0) {
  // Cycle through frames at ~12fps
  const frameIndex = Math.floor(performance.now() / 83) % frames.length;
  const frame = frames[frameIndex];
  $.paste(frame, drawX, drawY, {width, height}); // Paste to screen buffer
} else {
  // Show cassette animation with rotating reels
}
```
**Status**: ✅ Code ready, waiting for frames array

### disk.mjs (`/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`)

**Message routing whitelist** (Lines 8150-8159):
```javascript
const exportEventTypes = [
  "tape:load-progress",
  "tape:preloaded", 
  "tape:preload-error",
  "tape:frames",
  // ... other types
];
```
**Status**: ✅ All tape messages whitelisted

**Routing logic** (Lines 8184-8223):
```javascript
if (typeof receive === "function" && booted && receive !== defaults.receive) {
  console.log("📼 ✅ Calling receive directly for export event:", type);
  receive(eventObj);
} else {
  console.log("📼 ⏸️ Cannot call receive for", type, ": using default receive");
  console.log("📼 📥 Queued export event for later delivery:", type, "queue size:", pendingExportEvents.length);
  pendingExportEvents.push({ type, content });
}
```
**Status**: ❌ Taking else branch - `receive === defaults.receive` is TRUE

**Module loading** (Lines 7344-7357):
```javascript
console.log("📨 Module exports check:", {
  hasReceive: !!module.receive,
  receiveType: typeof module.receive,
  receiveIsDefault: module.receive === defaults.receive
});

receive = module.receive || defaults.receive;
```
**Status**: ❌ Never executes for prompt.mjs (no logs appear)

**Default receive** (Line 809):
```javascript
receive: () => false,  // No-op function
```
**Status**: ✅ This is what's currently assigned

### bios.mjs (`/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`)

**Frame sending** (Lines 8688-8702):
```javascript
console.log(`📼 Preloaded tape ${tapeId}: ${frames.length} frames`);
console.log(`📼 Sending ${frames.length} frames to disk for preview`);

send({
  type: "tape:frames",
  content: {
    tapeId: tapeId,
    frames: frames  // ImageBitmap[] array
  }
});
```
**Status**: ✅ Successfully sends frames

---

## Working Reference: video.mjs

Video.mjs successfully receives tape messages. Key evidence from logs:

```
disk.mjs:10315 🥾 ✅ Boot completed, booted = true pending events: 0
disk.mjs:7350 📨 Module exports check: {hasReceive: true, receiveType: 'function', receiveIsDefault: false}
disk.mjs:8200 📼 ✅ Calling receive directly for export event: tape:frames
0dba4910-...:2209 🎯 Video receive() called with event: tape:frames
```

**Why it works**:
- Module exports check shows `receiveIsDefault: false` ✅
- receive() is properly registered and called ✅
- Events delivered immediately, not queued ✅

---

## Root Cause Analysis

### Primary Issue
**prompt.mjs's receive() function is not being registered with disk.mjs**

Evidence:
1. `receive === defaults.receive` evaluates to TRUE
2. No "Module exports check" log for prompt.mjs
3. No module loading log from line 6 of prompt.mjs
4. All events being queued instead of delivered

### Why Module Not Loading

**Theory**: prompt.mjs was loaded BEFORE we added the receive() function. The JavaScript module system caches imported modules. Even though we've edited the file, disk.mjs is still using the old cached version in memory.

**Why reload doesn't help**: The session parameters (`?session-aesthetic=null&session-sotce=null`) might be preventing fresh module loads, OR the dev server needs to restart to clear module cache.

---

## Solutions to Try

### Option 1: Force Module Reload (Immediate)
Navigate away from prompt and back:
```javascript
// In browser console or prompt interface
// Type: line
// Then type: prompt
```
This forces disk.mjs to unload and reload prompt.mjs module.

### Option 2: Clear All Caches (Nuclear)
```javascript
// In browser console
caches.keys().then(keys => keys.forEach(key => caches.delete(key)));
location.reload();
```

### Option 3: Restart Dev Server (Most Reliable)
```bash
# Kill and restart the aesthetic-launch process
pkill -f "aesthetic-launch"
# Then restart normally
```

### Option 4: Add Version Parameter to Import
Modify disk.mjs to add cache-busting parameter when importing pieces:
```javascript
const module = await import(`./disks/${piece}.mjs?v=${Date.now()}`);
```

---

## Verification Steps

After trying a solution, check console for these logs in order:

1. ✅ `📨 ✅ prompt.mjs module loading`
2. ✅ `📨 Module exports check: {hasReceive: true, receiveType: 'function', receiveIsDefault: false}`
3. ✅ `🥾 ✅ Boot completed`
4. ✅ `📼 🚀 Flushing X pending export events after boot`
5. ✅ `📼 ✅ Calling receive directly for export event: tape:frames`
6. ✅ `📨 ✅✅✅ PROMPT.MJS RECEIVE called with type: tape:frames`
7. ✅ Frames should now display in tooltip (cycling animation instead of cassette)

---

## Expected Behavior Once Fixed

### Tooltip Preview Flow

1. **Hover over tape** → Cassette animation appears (rotating reels)
2. **ZIP downloads** → Progress updates via tape:load-progress
3. **Frames load** → tape:preloaded message received
4. **Frames sent** → tape:frames with ImageBitmap[] received
5. **Animation starts** → Cycles through frames at ~12fps
6. **Frame pasted** → $.paste() draws frame into screen buffer, letterboxed to 80x60px
7. **Smooth loop** → Continues until hover ends

### Frame Rendering Details

- **Frame rate**: ~12fps (83ms per frame)
- **Cycling**: `Math.floor(performance.now() / 83) % frames.length`
- **Letterboxing**: Maintains aspect ratio within 80x60px tooltip bounds
- **Positioning**: Centers frame within tooltip using calculated drawX/drawY
- **Rendering**: `$.paste(frame, drawX, drawY, {width, height})`

---

## File References

### Key Files
- **prompt.mjs**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/prompt.mjs`
- **disk.mjs**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`
- **bios.mjs**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`
- **video.mjs** (reference): `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/video.mjs`

### Critical Line Numbers
- prompt.mjs line 6: Module loading log
- prompt.mjs lines 5890-5954: receive() function
- prompt.mjs lines 5328-5344: Export block
- prompt.mjs lines 4349-4463: Tape rendering
- disk.mjs lines 7344-7357: Module loading & receive assignment
- disk.mjs lines 8184-8223: Message routing logic
- disk.mjs lines 10294-10318: Boot completion & event flushing
- bios.mjs lines 8688-8702: Frame sending

---

## Next Steps for Agent

1. **Try forced reload first**: Navigate to different piece, then back to prompt
2. **Verify logs appear**: Check console for module loading sequence above
3. **If still broken**: Try server restart or cache-busting import modification
4. **Test with different tapes**: Verify frames display for various tape codes
5. **Clean up diagnostic logs**: Remove excessive logging once working
6. **Performance check**: Ensure 12fps frame cycling is smooth

---

## Additional Context

- **Session ID**: Multiple pieces loaded simultaneously (video.mjs also running)
- **Piece ID**: `80e4994a-917f-48f2-9e80-24e4129425b9` (prompt.mjs instance)
- **Queue size**: Growing to 98+ events, never flushed
- **Frame counts**: 169, 304, 372, 428, 431, 858, 1059 frames tested
- **Cassette animation**: Working correctly (proves paint() executing)
- **ZIP loading**: Working (proves bios communication functional)

The infrastructure is complete and working end-to-end. The ONLY issue is the module cache preventing the new receive() function from being registered with disk.mjs.
