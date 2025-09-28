# Tape Command Research & Analysis

## Overview

The `tape` command in Aesthetic Computer allows recording visual and audio sessions, then exporting them as various formats (GIF, MP4, WebP, etc.). The command flows through several key components in the codebase.

## Command Flow

### 1. Prompt Input (`prompt.mjs`)

**Location**: `/system/public/aesthetic.computer/disks/prompt.mjs` (lines ~400-500)

**Parsing Logic**:
```javascript
// Frame-based recording detection
if (params[0] && typeof params[0] === 'string' && params[0].toLowerCase().endsWith('f')) {
  frameMode = true;
  duration = parseFloat(params[0].slice(0, -1)); // Remove the 'f' suffix
  console.log(`🎬 Frame-based recording requested: ${duration} frames`);
}
```

**Examples**:
- `tape 2f $ican` → Records 2 frames of $ican
- `tape 30f $cow` → Records 30 frames of $cow
- `tape 5 $piece` → Records 5 seconds of $piece

### 2. Recording System (`disk.mjs`)

**Location**: `/system/public/aesthetic.computer/lib/disk.mjs` (lines ~640-780)

**Frame-based Recording**:
```javascript
tapeTimerSet(durationOrFrames, time, isFrameMode = false) {
  if (isFrameMode) {
    this.tapeFrameMode = true;
    this.tapeFrameStart = Number($commonApi.paintCount || 0n);
    this.tapeFrameTarget = durationOrFrames;
    // ...
  }
}

tapeTimerStep({ needsPaint, sound: { time } }) {
  if (this.tapeFrameMode) {
    const currentFrame = Number($commonApi.paintCount || 0n);
    const framesPassed = currentFrame - this.tapeFrameStart;
    
    if (framesPassed >= this.tapeFrameTarget) {
      // Recording complete, jump to video
      this.cut(() => { $commonApi.jump("video"); });
    }
  }
}
```

### 3. Video Piece & Export (`video.mjs`)

**Location**: `/system/public/aesthetic.computer/disks/video.mjs` (lines ~790-900)

**GIF Export Logic**:
```javascript
// Calculate frame duration
let duration = 16.67; // Default 16.67ms for 60fps
if (index < framesToProcess.length - 1) {
  const nextTimestamp = framesToProcess[index + 1][0];
  duration = Math.max(10, nextTimestamp - timestamp);
}
```

### 4. GIF Encoding (`bios.mjs`)

**Location**: `/system/public/aesthetic.computer/bios.mjs` (lines ~4550-4650)

**Current Frame Delay Calculation**:
```javascript
// Calculate proper delay based on the resampled frame timing
if (processedFrames.length > 1 && content.frames.length > 0) {
  const originalTotalDuration = content.frames[content.frames.length - 1].timestamp - content.frames[0].timestamp;
  const intendedDelayPerFrame = originalTotalDuration / processedFrames.length;
  gifencDelay = Math.round(Math.max(intendedDelayPerFrame, 16)); // Minimum 16ms (62.5fps max)
} else {
  gifencDelay = 17; // Default 60fps fallback
}
```

## Issues Identified

### 1. **KidLisp FPS Not Respected**

**Problem**: The GIF encoding uses a fixed frame delay calculation based on timestamps, but doesn't access the KidLisp piece's framerate setting.

**KidLisp FPS Function**: Located in `kidlisp.mjs` line 5686:
```javascript
if (expression === "fps") {
  return "fps"; // Return the string to indicate it was processed
}
```

**The Issue**: The `fps` function in KidLisp is not implemented - it just returns the string "fps". There's no actual framerate storage or retrieval mechanism.

### 2. **Single Frame GIF Issue**

**Root Cause**: When recording `tape 2f $ican`, the system should:
1. Record exactly 2 frames of the KidLisp piece
2. Use the piece's intended framerate for GIF delay calculation
3. But if the piece runs at 24fps (as `$ican` does with `(fps 24)`), the delays should be ~41.67ms per frame, not 16.67ms

## Proposed Solutions

### 1. **Implement Proper FPS Function in KidLisp**

Add to `kidlisp.mjs` global environment:
```javascript
fps: (api, args) => {
  if (args.length > 0) {
    const targetFps = parseFloat(args[0]);
    if (!isNaN(targetFps) && targetFps > 0) {
      // Store the target FPS in the KidLisp instance
      this.targetFps = targetFps;
      // Also store in a global location accessible to the tape system
      if (api && api.system) {
        api.system.kidlispFps = targetFps;
      }
      return targetFps;
    }
  }
  return this.targetFps || 60; // Default to 60 FPS
},
```

### 2. **Pass KidLisp FPS to Recording System**

Modify the recording metadata in `prompt.mjs`:
```javascript
rec.loadCallback = () => {
  // Capture the KidLisp FPS if available
  const kidlispFps = $commonApi.system?.kidlispFps || null;
  
  rec.rolling({
    // ... existing properties
    kidlispFps: kidlispFps,
    frameMode: frameMode,
    frameCount: frameMode ? (isNaN(duration) ? 8 : duration) : null,
  });
};
```

### 3. **Use KidLisp FPS in GIF Encoding**

Update `bios.mjs` GIF encoding to respect the KidLisp framerate:
```javascript
// Check if we have a KidLisp framerate to respect
const kidlispFps = window.currentRecordingOptions?.kidlispFps;
if (kidlispFps && kidlispFps > 0) {
  // Use the exact KidLisp framerate for perfect timing
  gifencDelay = Math.round(1000 / kidlispFps); // Convert FPS to milliseconds
  console.log(`🎞️ Using KidLisp framerate: ${kidlispFps}fps = ${gifencDelay}ms delay`);
} else {
  // Fallback to existing logic
  if (processedFrames.length > 1 && content.frames.length > 0) {
    const originalTotalDuration = content.frames[content.frames.length - 1].timestamp - content.frames[0].timestamp;
    const intendedDelayPerFrame = originalTotalDuration / processedFrames.length;
    gifencDelay = Math.round(Math.max(intendedDelayPerFrame, 16));
  } else {
    gifencDelay = 17; // Default 60fps fallback
  }
}
```

## Benefits

1. **Accurate Framerate**: GIFs will play back at the exact speed the KidLisp piece was designed for
2. **Artist Intent**: Respects the creative timing decisions in KidLisp pieces
3. **Consistency**: Visual recording matches the live experience
4. **Flexibility**: Still falls back to timestamp-based calculation for non-KidLisp content

## Implementation Priority

1. **High**: Fix KidLisp FPS function implementation
2. **High**: Pass FPS metadata through recording pipeline  
3. **Medium**: Update GIF encoding to use KidLisp FPS
4. **Low**: Extend to other export formats (MP4, WebP)

## Testing

Test cases needed:
- `tape 10f $ican` (24fps piece) → Should create 10-frame GIF at 24fps timing
- `tape 5f $cow` (whatever fps $cow uses) → Should respect that framerate
- `tape 30f prompt` (non-KidLisp) → Should fallback to timestamp calculation

## Files to Modify

1. `/system/public/aesthetic.computer/lib/kidlisp.mjs` - Implement fps function
2. `/system/public/aesthetic.computer/disks/prompt.mjs` - Pass FPS metadata
3. `/system/public/aesthetic.computer/bios.mjs` - Use FPS in GIF encoding
4. `/system/public/aesthetic.computer/disks/video.mjs` - Update frame processing
