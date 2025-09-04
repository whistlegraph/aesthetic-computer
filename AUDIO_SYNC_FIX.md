# Audio Sync Fix for MP4 Tape Exports

## Problem Description
MP4 exports from tape recordings had audio that started with silence and then played audio slightly after the video started, causing sync issues. This affected pieces like `wipppps` but not `notepat` due to timing differences in their audio generation.

## Root Cause
The issue was caused by a timing mismatch between when the `rawAudioProcessor` (ScriptProcessorNode) started capturing audio and when the `MediaRecorder` actually began recording:

1. **Raw Audio Processor**: Started capturing immediately when connected to the audio graph
2. **MediaRecorder**: Had internal startup delays before actually beginning recording
3. **Result**: Raw audio data included samples from before the MediaRecorder started, creating an offset

## Solution Implemented

### 1. Delayed Audio Connection
- Changed raw audio processor setup to prepare the node but not connect it immediately
- Added timing information to captured audio chunks
- Connect the processor only when `MediaRecorder.onstart` fires

### 2. Synchronized Timing
```javascript
// Before: Connected immediately
sfxStreamGain.connect(rawAudioProcessor);

// After: Connect only when MediaRecorder starts
mediaRecorder.onstart = function () {
  mediaRecorderStartTime = performance.now();
  // NOW connect for perfect sync
  if (rawAudioProcessor && sfxStreamGain) {
    sfxStreamGain.connect(rawAudioProcessor);
    rawAudioProcessor.connect(audioContext.destination);
  }
};
```

### 3. Enhanced Audio Capture
- Added timestamp tracking relative to recording start time
- Audio chunks now include timing information for future enhancements
- Maintained existing cleanup procedures

## Files Modified
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`
  - Lines ~7954-7974: Raw audio processor setup
  - Lines ~7995-8016: MediaRecorder start callback
  - Audio capture timing synchronization

## Testing
This fix should resolve:
- ✅ Audio sync in `tape:clean wipppps` exports
- ✅ MP4 exports starting with proper audio timing
- ✅ Maintaining existing functionality for pieces that weren't affected

## Technical Notes
- The fix maintains backward compatibility
- Existing cleanup procedures for the raw audio processor remain unchanged
- The timing offset is now eliminated at the source rather than compensated for later
- This approach is more reliable than post-processing audio trimming
