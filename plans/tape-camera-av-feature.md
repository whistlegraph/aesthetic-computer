# 📹 Tape Camera A/V Feature Plan

> **Goal**: Enable `tape camera` (or shorthand like `tc` or `tok`) to record a TikTok-style video with synchronized audio/video from the device camera + microphone, then play it back and allow posting.

---

## 📋 Current State Analysis

### Camera System (`camera.mjs`)
**Location**: `/system/public/aesthetic.computer/disks/camera.mjs`

**Current Capabilities**:
- ✅ Webcam/device camera access via `video("camera")` API
- ✅ Front/back camera switching ("user" / "environment" facing)
- ✅ Real-time pixel shader effects (sparkles)
- ✅ Frame capture to painting system
- ❌ **No video recording** - only still captures
- ❌ **No audio support** - purely visual
- ❌ **Responsiveness issues** - layout doesn't adapt well to different screen sizes

**Responsiveness Problems**:
- Video fills painting frame size, not screen
- No aspect ratio handling for portrait vs landscape
- Swap button positioning is fixed (`bottom: 6, right: 6`)
- No fullscreen/immersive mode for TikTok-style recording

### Tape System (`prompt.mjs` + `disk.mjs`)
**Location**: `/system/public/aesthetic.computer/disks/prompt.mjs` (lines 5390-5650)

**Current Capabilities**:
- ✅ Screen recording via canvas capture
- ✅ Frame-based or time-based recording modes
- ✅ Microphone support with `tape:mic` / `tape:nomic` flags
- ✅ Recording progress bar overlay
- ✅ Automatic jump to `video.mjs` for playback/export
- ✅ MP4, GIF, WebP export options
- ❌ **No camera video input** - only captures canvas/screen
- ❌ **Audio is separate from video** - mic records to separate track

### Current Tape Commands
```
tape [duration] [flags]     # Record screen for N seconds
tape 30f $code              # Record 30 frames of a KidLisp piece
tape:mic                    # Enable microphone
tape:nomic                  # Disable microphone
tape:cut / cut              # Stop recording early
```

### Video Piece (`video.mjs`)
**Location**: `/system/public/aesthetic.computer/disks/video.mjs`

**Current Capabilities**:
- ✅ Playback of recorded frames
- ✅ Scrubbing (STAMPLE-style lazy following)
- ✅ Export to GIF, MP4, WebP, ZIP
- ✅ POST to ATProto (Bluesky)
- ❌ **Audio playback is buggy** - often out of sync
- ❌ **No camera preview during playback**

### BIOS Recording System (`bios.mjs`)
**Location**: `/system/public/aesthetic.computer/bios.mjs` (lines 730-900, 7300-8700)

**Current Capabilities**:
- ✅ MediaRecorder API for canvas capture
- ✅ Frame buffer storage
- ✅ Timing/timestamp tracking
- ✅ Codec support (H.264 for MP4)
- ❌ **No webcam stream integration** - only canvas capture
- ❌ **Audio mixing** - mic is separate from system audio

---

## 🎯 Proposed Feature: `tape camera`

### User Flow (TikTok-style)

1. **Start**: User types `tape camera` (or `tc` / `tok`)
2. **Permission**: Browser requests camera + microphone access
3. **Preview**: Full-screen camera view with countdown (3, 2, 1...)
4. **Recording**: 
   - Live camera feed shown on screen
   - Red recording indicator
   - Progress bar at top
   - Microphone capturing audio
   - Optional duration limit (default 60s for TikTok style)
5. **Finish**: Tap screen or wait for timer to stop
6. **Playback**: Jump to `video.mjs` with A/V preview
7. **Post**: User can POST to ATProto with one tap

### Shorthand Commands for `prompt.mjs`

| Command | Expands To | Description |
|---------|-----------|-------------|
| `tc` | `tape camera` | Quick TikTok-style recording |
| `tok` | `tape camera` | TikTok-inspired shorthand |
| `toks` | `tape camera:selfie` | Front-facing camera |
| `selfievid` | `tape camera:selfie` | Explicit selfie video |

**Additional Flags**:
```
tape camera          # Default (rear camera, with mic)
tape camera:selfie   # Front-facing camera
tape camera:nomic    # Without microphone
tape camera:15       # 15-second max duration
tape camera~filter   # With filter effect (future)
```

---

## 🏗️ Implementation Plan

### Phase 1: Camera Recording Foundation

#### 1.1 Create `tape-camera.mjs` piece
**New file**: `/system/public/aesthetic.computer/disks/tape-camera.mjs`

```javascript
// Tape Camera - TikTok-style A/V recording
// Combines camera.mjs video feed with tape recording system

let vid, stream;
let recording = false;
let countdown = 3;
let maxDuration = 60; // seconds

function boot({ params, colon }) {
  // Parse params: selfie, duration, nomic
  const selfie = colon.includes('selfie') || params.includes('me');
  maxDuration = parseInt(params.find(p => !isNaN(p))) || 60;
}

function paint({ video, screen, wipe }) {
  if (countdown > 0) {
    // Show countdown overlay
    wipe(0);
    ink(255, 0, 0).write(countdown, { center: "xy", size: 8 });
    return;
  }
  
  // Fullscreen camera feed
  vid?.();
  
  if (recording) {
    // Red recording dot + progress bar
    ink(255, 0, 0).circle(screen.width - 20, 20, 8, true);
  }
}
```

#### 1.2 Extend BIOS for Camera Stream Recording
**Modify**: `/system/public/aesthetic.computer/bios.mjs`

- Add `mediaRecorder` support for camera + mic streams (not just canvas)
- Combine `getUserMedia` video + audio into single MediaRecorder
- Store as WebM/MP4 with embedded audio track

#### 1.3 Update `video.mjs` for A/V Playback
- Load recorded video as HTML `<video>` element (not just frames)
- Sync audio playback with scrubbing
- Support both frame-based (canvas) and stream-based (camera) tapes

### Phase 2: Responsive Camera UI

#### 2.1 Fix `camera.mjs` Responsiveness
**Issues to address**:

1. **Aspect Ratio Handling**:
   ```javascript
   // Calculate camera dimensions to fill screen while maintaining aspect ratio
   function calculateCameraBounds(screen, videoAspect) {
     const screenAspect = screen.width / screen.height;
     let w, h, x, y;
     
     if (videoAspect > screenAspect) {
       // Video is wider - fit to width, crop height
       w = screen.width;
       h = screen.width / videoAspect;
       x = 0;
       y = (screen.height - h) / 2;
     } else {
       // Video is taller - fit to height, crop width
       h = screen.height;
       w = screen.height * videoAspect;
       x = (screen.width - w) / 2;
       y = 0;
     }
     
     return { x, y, w, h };
   }
   ```

2. **Responsive Button Placement**:
   ```javascript
   // Position Swap button based on screen size and orientation
   const isPortrait = screen.height > screen.width;
   const buttonSize = Math.max(30, Math.min(50, screen.width * 0.12));
   
   swap?.reposition({
     bottom: isPortrait ? screen.height * 0.1 : 6,
     right: isPortrait ? 6 : screen.width * 0.05,
     width: buttonSize,
     height: buttonSize,
     screen
   });
   ```

3. **Fullscreen Mode**:
   - Add `system = "fullscreen"` export for immersive recording
   - Hide HUD during recording
   - Show minimal overlay controls

#### 2.2 Portrait/Landscape Detection
```javascript
function getOrientation(screen) {
  return screen.height > screen.width ? 'portrait' : 'landscape';
}

function paint({ screen }) {
  const orientation = getOrientation(screen);
  
  // Adjust layout based on orientation
  if (orientation === 'portrait') {
    // TikTok-style vertical layout
  } else {
    // YouTube-style horizontal layout
  }
}
```

### Phase 3: Audio Integration

#### 3.1 Unified A/V Recording
**New API in `disk.mjs`**:
```javascript
// Start combined camera + microphone recording
rec.rollingAV({
  camera: true,           // Enable camera
  microphone: true,       // Enable microphone
  facing: 'user',         // 'user' or 'environment'
  duration: 60,           // Max duration in seconds
  quality: 'high'         // 'low', 'medium', 'high'
});
```

#### 3.2 Audio Visualization (Optional)
- Show audio waveform during recording
- Level meter for microphone input

### Phase 4: Export & Posting

#### 4.1 Video Export Enhancements
- Ensure audio track is properly muxed in MP4 export
- Add audio to WebP/GIF as separate file option
- Preserve audio in ZIP archive

#### 4.2 ATProto Tape Record with Audio
**Lexicon update**: `computer.aesthetic.tape`
```json
{
  "audio": {
    "type": "blob",
    "accept": ["audio/webm", "audio/mp4"],
    "maxSize": 10485760
  }
}
```

---

## 🔧 Technical Considerations

### Browser Compatibility
- **MediaRecorder**: Chrome, Firefox, Safari 14.1+
- **getUserMedia**: All modern browsers
- **Audio + Video sync**: Use MediaRecorder with combined stream

### Mobile Considerations
- iOS Safari requires user gesture for camera/mic
- Android may have different codec support
- Battery/thermal considerations for long recordings

### Performance
- Camera streams are memory-intensive
- Consider max resolution (720p recommended for mobile)
- Implement auto-stop on memory pressure

---

## 📁 Files to Create/Modify

### New Files
- [ ] `/system/public/aesthetic.computer/disks/tape-camera.mjs` - New A/V recording piece
- [ ] `/system/public/aesthetic.computer/lib/camera-stream.mjs` - Camera stream utilities

### Modified Files
- [ ] `/system/public/aesthetic.computer/disks/camera.mjs` - Fix responsiveness
- [ ] `/system/public/aesthetic.computer/disks/prompt.mjs` - Add shorthand commands
- [ ] `/system/public/aesthetic.computer/disks/video.mjs` - A/V playback support
- [ ] `/system/public/aesthetic.computer/lib/disk.mjs` - Add `rollingAV` method
- [ ] `/system/public/aesthetic.computer/bios.mjs` - Camera stream recording

---

## 🎬 Shorthand Command Implementation

### In `prompt.mjs` (halt function)

```javascript
// Add to the slug matching section around line 5500
} else if (slug === "tc" || slug === "tok") {
  // TikTok-style camera recording
  jump("tape-camera");
  return true;
} else if (slug === "toks" || slug === "selfievid") {
  // Selfie video
  jump("tape-camera~selfie");
  return true;
} else if (text.startsWith("tape camera") || text.startsWith("tape:camera")) {
  // Full tape camera command
  const params = text.replace(/tape[: ]camera/, '').trim();
  jump(`tape-camera${params ? '~' + params.replace(/\s+/g, '~') : ''}`);
  return true;
}
```

---

## 🚀 MVP Priority Order

1. **MVP v1**: Basic `tape camera` recording (camera + mic to video file)
2. **MVP v2**: Responsive camera UI fixes
3. **MVP v3**: Shorthand commands (`tc`, `tok`)
4. **v1.0**: Full A/V playback in `video.mjs`
5. **v1.1**: POST to ATProto with video
6. **Future**: Filters, effects, multi-clip editing

---

## 📝 Notes

- Current `tape` system records canvas frames, not video streams
- Camera integration requires combining two different capture approaches
- Audio sync is the hardest part - MediaRecorder handles this natively
- Consider using `<video>` element playback for smooth A/V sync instead of frame-by-frame

---

*Created: 2026-01-04*
*Author: Copilot/aesthetic-computer*
