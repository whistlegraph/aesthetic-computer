# TV Multi-Tape Transition System
**Created:** 2025-11-10  
**Status:** Planning Phase  
**Context:** Enable smooth video transitions between tapes in tv.mjs (TikTok-style For You Page)

## 🎯 Problem Statement

The current `tv.mjs` piece shows a vertical feed of tapes (like TikTok FYP) but cannot smoothly transition between videos because:

1. **Single underlay limitation**: BIOS maintains only ONE `underlayFrame` div at a time (line 10485 in bios.mjs)
2. **No video caching**: Each tape swap requires full load cycle (fetch ZIP → extract → decode frames → play)
3. **No transform support**: The piece cannot control video position/opacity - video is either visible or not
4. **Synchronous loading**: Cannot preload next/previous tapes while current tape plays

### Current Architecture

```
tv.mjs (piece)
    ↓ tape:play message
bios.mjs
    ↓ Creates single underlayFrame
    ↓ Loads ONE video element
DOM: <div id="underlay"><canvas>video frames</canvas></div>
    ↓ z-index: 0 (below main canvas at z-index: 3)
```

**Result:** Instant video swap on transition, no smooth fade/slide between tapes.

---

## 🏗️ Proposed Architecture

### Single Canvas Multi-Tape System

Use **ONE underlay canvas** that composites multiple tape frames with vertical offsets, while multiple `Tape` class instances manage their own frames, audio, and playback state.

```
tv.mjs (piece)
    ↓ tape:cache (preload next/prev)
    ↓ tape:scroll-offset (animate scroll position)
bios.mjs
    ↓ TapeManager class (manages 3 Tape instances)
    ↓ Single underlayFrame canvas composites all visible tapes
    ↓ Audio crossfade between active tapes
DOM:
<div id="underlay">
  <canvas>← Single canvas, renders:
    - Tape 0 at Y offset -100% (faded out)
    - Tape 1 at Y offset 0% (visible, audio 100%)
    - Tape 2 at Y offset +100% (faded out)
  </canvas>
</div>

Memory:
  Tape instances (prev, current, next)
    - Each stores: frames[], timingData[], audioBuffer
    - Each manages: playback state, frame index
    - Rendered to single canvas based on scroll offset
```

**Benefits:**
- Single canvas = less DOM manipulation, better performance
- Audio crossfade during transitions (both tapes play, blend volumes)
- Smooth frame blending with alpha compositing
- Reuses existing `underlayFrame` infrastructure

---

## 📋 Implementation Plan

### Phase 1: BIOS - Tape Class (Data Container)

**File:** `system/public/aesthetic.computer/bios.mjs`

#### 1.1 Create Tape Class (No DOM/Canvas)

```javascript
class Tape {
  constructor(id) {
    this.id = id;
    this.code = null;
    this.metadata = null;
    
    // Frame data
    this.frames = []; // Array of ImageBitmap objects
    this.timingData = []; // [{ frame, filename, duration, timestamp }, ...]
    this.frameIndex = 0;
    this.lastFrameTime = 0;
    
    // Audio data
    this.audioBuffer = null; // Web Audio AudioBuffer
    this.audioSource = null; // AudioBufferSourceNode
    this.audioGainNode = null; // GainNode for volume control
    this.audioStartTime = 0;
    this.audioStartedAtFrame = 0;
    
    // Playback state (persistent across scroll)
    this.isLoading = false;
    this.isPlaying = false;
    this.isPaused = false;
    this.wasPlayingBeforeScroll = false; // Remember state when scrolling away
    this.savedProgress = 0; // Save progress when pausing/scrolling away
    this.loadProgress = 0;
    this.loadPhase = ""; // download, unpack, frames, audio
    
    // Rendering state (managed by TapeManager)
    this.yOffset = 0; // Vertical offset in pixels (for scrolling)
    this.alpha = 0; // Opacity 0-1
    this.volume = 0; // Audio volume 0-1
  }

  // Load tape from ZIP URL
  async load(code, zipUrl, metadata) {
    this.code = code;
    this.metadata = metadata;
    this.isLoading = true;
    this.loadPhase = "download";
    this.loadProgress = 0;
    
    try {
      // Download ZIP with progress tracking
      const response = await fetch(zipUrl);
      const arrayBuffer = await response.arrayBuffer();
      
      this.loadPhase = "unpack";
      this.loadProgress = 0.3;
      
      // Unpack ZIP using JSZip
      const zip = await JSZip.loadAsync(arrayBuffer);
      
      // Load timing.json
      const timingJson = await zip.file("timing.json").async("text");
      this.timingData = JSON.parse(timingJson);
      
      // Load metadata.json
      const metadataJson = await zip.file("metadata.json").async("text");
      const zipMetadata = JSON.parse(metadataJson);
      
      this.loadPhase = "frames";
      
      // Load all frames as ImageBitmap
      const frameFiles = Object.keys(zip.files).filter(f => f.startsWith("frame-"));
      this.frames = [];
      
      for (let i = 0; i < frameFiles.length; i++) {
        const blob = await zip.file(frameFiles[i]).async("blob");
        const bitmap = await createImageBitmap(blob);
        this.frames.push(bitmap);
        this.loadProgress = 0.3 + (i / frameFiles.length) * 0.5;
      }
      
      this.loadPhase = "audio";
      this.loadProgress = 0.8;
      
      // Load audio if present
      const audioFile = zip.file("audio.webm") || zip.file("audio.wav");
      if (audioFile) {
        const audioBlob = await audioFile.async("blob");
        const audioArrayBuffer = await audioBlob.arrayBuffer();
        
        // Decode audio using Web Audio API
        const audioContext = getAudioContext(); // Get global AudioContext
        this.audioBuffer = await audioContext.decodeAudioData(audioArrayBuffer);
      }
      
      this.loadProgress = 1.0;
      this.isLoading = false;
      
      console.log(`✅ Loaded tape ${this.code}: ${this.frames.length} frames, ${this.audioBuffer ? 'with' : 'no'} audio`);
      
    } catch (err) {
      console.error(`❌ Failed to load tape ${this.code}:`, err);
      this.isLoading = false;
      throw err;
    }
  }

  // Start playback (audio only - frames rendered by TapeManager)
  play(audioContext) {
    if (this.isPlaying || !this.audioBuffer) return;
    
    this.isPlaying = true;
    this.isPaused = false;
    this.wasPlayingBeforeScroll = true; // Remember we were playing
    this.lastFrameTime = performance.now();
    
    // Create audio source
    this.audioSource = audioContext.createBufferSource();
    this.audioSource.buffer = this.audioBuffer;
    
    // Create gain node for volume control (for crossfade)
    this.audioGainNode = audioContext.createGain();
    this.audioGainNode.gain.value = this.volume;
    
    this.audioSource.connect(this.audioGainNode);
    this.audioGainNode.connect(audioContext.destination);
    
    // Start audio from saved progress (for resume after scroll)
    const startOffset = (this.frameIndex / this.frames.length) * this.audioBuffer.duration;
    this.audioStartTime = audioContext.currentTime - startOffset;
    this.audioStartedAtFrame = this.frameIndex;
    this.audioSource.start(0, startOffset);
    
    // Loop audio when it ends
    this.audioSource.onended = () => {
      if (this.isPlaying && !this.isPaused) {
        this.frameIndex = 0; // Reset to beginning
        this.play(audioContext); // Restart
      }
    };
  }

  pause() {
    this.isPaused = true;
    this.wasPlayingBeforeScroll = false; // Manually paused
    this.savedProgress = this.getProgress();
    this.audioSource?.stop();
  }

  stop() {
    this.isPlaying = false;
    this.isPaused = false;
    this.audioSource?.stop();
    this.audioSource = null;
    // Don't reset frameIndex - keep position for resume
    this.savedProgress = this.getProgress();
  }

  // Pause when scrolling away (but remember we were playing)
  pauseForScroll() {
    if (this.isPlaying) {
      this.wasPlayingBeforeScroll = true;
      this.savedProgress = this.getProgress();
      this.audioSource?.stop();
      this.isPlaying = false;
    }
  }

  // Resume when scrolling back
  resumeFromScroll(audioContext) {
    if (this.wasPlayingBeforeScroll && !this.isPlaying) {
      // Restore saved position
      if (this.savedProgress > 0) {
        this.setProgress(this.savedProgress);
      }
      this.play(audioContext);
    }
  }

  // Update frame index based on elapsed time
  updateFrame(now) {
    if (!this.isPlaying || this.isPaused || this.frames.length === 0) return;
    
    const elapsed = now - this.lastFrameTime;
    const currentTiming = this.timingData[this.frameIndex];
    
    if (currentTiming && elapsed >= currentTiming.duration) {
      this.frameIndex = (this.frameIndex + 1) % this.frames.length;
      this.lastFrameTime = now;
    }
  }

  // Get current frame ImageBitmap
  getCurrentFrame() {
    return this.frames[this.frameIndex] || null;
  }

  // Set audio volume (for crossfade)
  setVolume(volume) {
    this.volume = Math.max(0, Math.min(1, volume));
    if (this.audioGainNode) {
      this.audioGainNode.gain.value = this.volume;
    }
  }

  // Seek to specific frame (for scrubbing)
  seekToFrame(frameIndex) {
    if (frameIndex < 0 || frameIndex >= this.frames.length) return;
    
    this.frameIndex = Math.floor(frameIndex);
    
    // If audio is playing, sync audio to frame position
    if (this.isPlaying && this.audioBuffer && this.audioSource) {
      const frameDuration = this.timingData[this.frameIndex]?.duration || 16.67;
      const timePosition = (this.frameIndex * frameDuration) / 1000; // Convert to seconds
      
      // Restart audio from new position
      this.audioSource.stop();
      this.audioSource = this.audioContext.createBufferSource();
      this.audioSource.buffer = this.audioBuffer;
      this.audioSource.connect(this.audioGainNode);
      
      this.audioStartTime = this.audioContext.currentTime - timePosition;
      this.audioSource.start(0, timePosition);
      
      this.audioSource.onended = () => {
        if (this.isPlaying && !this.isPaused) {
          this.play(this.audioContext);
        }
      };
    }
  }

  // Get progress (0-1) based on current frame
  getProgress() {
    if (this.frames.length === 0) return 0;
    return this.frameIndex / (this.frames.length - 1);
  }

  // Set progress (0-1) for scrubbing
  setProgress(progress) {
    const targetFrame = Math.floor(progress * (this.frames.length - 1));
    this.seekToFrame(targetFrame);
  }

  // Cleanup
  destroy() {
    this.stop();
    this.frames.forEach(frame => frame.close?.()); // Release ImageBitmap memory
    this.frames = [];
    this.audioBuffer = null;
  }
}
```

#### 1.2 Create TapeManager (Rendering + Compositing)

```javascript
class TapeManager {
  constructor(options = {}) {
    this.tapes = new Map(); // id -> Tape instance
    this.activeId = null;
    
    // Configurable cache size (3, 6, or 12 tapes)
    this.maxTapes = options.maxTapes || 6; // prev3, prev2, prev1, current, next1, next2, next3
    this.preloadDistance = options.preloadDistance || 3; // How many tapes ahead/behind to preload
    
    // Single canvas rendering
    this.underlayFrame = null;
    this.canvas = null;
    this.ctx = null;
    
    // Scroll state (controlled by piece)
    this.scrollOffset = 0; // In pixels (0 = current tape centered)
    this.targetScrollOffset = 0;
    this.isScrolling = false;
    
    // Animation frame loop
    this.rafId = null;
    this.isRendering = false;
    
    // Audio context
    this.audioContext = null;
    
    // Preload queue management
    this.preloadQueue = []; // Array of {index, code, zipUrl, metadata, tapeId}
    this.isPreloading = false;
    this.currentIndex = 0; // Track current position for smart preloading
  }

  // Initialize underlay canvas
  init() {
    this.underlayFrame = document.createElement("div");
    this.underlayFrame.id = "underlay";
    
    this.canvas = document.createElement("canvas");
    this.ctx = this.canvas.getContext("2d");
    
    this.underlayFrame.appendChild(this.canvas);
    wrapper.appendChild(this.underlayFrame);
    
    // Get or create audio context
    this.audioContext = getAudioContext();
    
    // Start render loop
    this.startRenderLoop();
    
    console.log(`📼 TapeManager initialized (max ${this.maxTapes} tapes, preload ±${this.preloadDistance})`);
  }

  // Get or create tape instance
  getTape(id) {
    if (!this.tapes.has(id)) {
      if (this.tapes.size >= this.maxTapes) {
        // Evict least recently used tape (furthest from current)
        this.evictFurthestTape();
      }
      
      const tape = new Tape(id);
      this.tapes.set(id, tape);
      console.log(`📼 Created tape instance: ${id} (${this.tapes.size}/${this.maxTapes})`);
    }
    return this.tapes.get(id);
  }

  // Evict tape furthest from current position
  evictFurthestTape() {
    // Parse tape IDs to find distance from current
    const distances = new Map();
    
    for (const [id, tape] of this.tapes) {
      if (id === this.activeId) {
        distances.set(id, 0); // Don't evict active
        continue;
      }
      
      // Extract index from tape ID (e.g., "tape-5" -> 5)
      const match = id.match(/tape-(\d+)/);
      if (match) {
        const tapeIndex = parseInt(match[1], 10);
        const distance = Math.abs(tapeIndex - this.currentIndex);
        distances.set(id, distance);
      }
    }
    
    // Find furthest tape
    let furthestId = null;
    let maxDistance = -1;
    
    for (const [id, distance] of distances) {
      if (distance > maxDistance) {
        maxDistance = distance;
        furthestId = id;
      }
    }
    
    if (furthestId) {
      this.destroyTape(furthestId);
      console.log(`📼 Evicted furthest tape: ${furthestId} (distance: ${maxDistance})`);
    }
  }

  // Set current index (used for smart preloading)
  setCurrentIndex(index) {
    this.currentIndex = index;
  }

  // Queue tapes for preloading
  queuePreload(index, code, zipUrl, metadata, tapeId) {
    // Check if already loaded or queued
    if (this.tapes.has(tapeId)) return;
    if (this.preloadQueue.some(item => item.tapeId === tapeId)) return;
    
    this.preloadQueue.push({ index, code, zipUrl, metadata, tapeId });
    this.processPreloadQueue();
  }

  // Process preload queue (one at a time to avoid overwhelming network)
  async processPreloadQueue() {
    if (this.isPreloading || this.preloadQueue.length === 0) return;
    
    this.isPreloading = true;
    
    while (this.preloadQueue.length > 0) {
      // Sort queue by distance from current (preload closest first)
      this.preloadQueue.sort((a, b) => {
        const distA = Math.abs(a.index - this.currentIndex);
        const distB = Math.abs(b.index - this.currentIndex);
        return distA - distB;
      });
      
      const item = this.preloadQueue.shift();
      const tape = this.getTape(item.tapeId);
      
      try {
        console.log(`📼 Preloading tape ${item.code} (${item.tapeId}) distance: ${Math.abs(item.index - this.currentIndex)}`);
        await tape.load(item.code, item.zipUrl, item.metadata);
        console.log(`✅ Preloaded tape ${item.code}`);
      } catch (err) {
        console.error(`❌ Failed to preload tape ${item.code}:`, err);
      }
    }
    
    this.isPreloading = false;
  }

  // Preload tapes in range around current index
  preloadRange(currentIndex, feedLength) {
    this.setCurrentIndex(currentIndex);
    
    // Calculate range to preload
    const startIndex = Math.max(0, currentIndex - this.preloadDistance);
    const endIndex = Math.min(feedLength - 1, currentIndex + this.preloadDistance);
    
    console.log(`📼 Preloading range: ${startIndex} to ${endIndex} (current: ${currentIndex})`);
    
    // Queue all tapes in range (will be sorted by distance in processPreloadQueue)
    for (let i = startIndex; i <= endIndex; i++) {
      if (i === currentIndex) continue; // Current tape already loaded
      
      const tapeId = `tape-${i}`;
      // Tape data will be provided by piece via queuePreload
    }
  }

  // Set active tape and start playback
  setActive(id) {
    // Pause previous active (but keep state)
    if (this.activeId && this.tapes.has(this.activeId)) {
      const prevTape = this.tapes.get(this.activeId);
      prevTape.pauseForScroll(); // Pause but remember we were playing
      prevTape.volume = 0;
    }
    
    this.activeId = id;
    const activeTape = this.tapes.get(id);
    
    if (activeTape && !activeTape.isLoading) {
      // Resume playback if it was playing before we scrolled away
      activeTape.resumeFromScroll(this.audioContext);
      activeTape.volume = 1;
      activeTape.yOffset = 0;
      activeTape.alpha = 1;
    }
  }

  // Set scroll offset (called by piece during transitions)
  setScrollOffset(offset) {
    this.targetScrollOffset = offset;
    this.isScrolling = true;
  }

  // Lerp scroll offset for smooth animation
  updateScrollOffset() {
    if (!this.isScrolling) return;
    
    const speed = 0.15; // Smoothing factor
    this.scrollOffset += (this.targetScrollOffset - this.scrollOffset) * speed;
    
    // Snap when close
    if (Math.abs(this.targetScrollOffset - this.scrollOffset) < 0.5) {
      this.scrollOffset = this.targetScrollOffset;
      this.isScrolling = false;
    }
  }

  // Main render loop - composites all visible tapes
  startRenderLoop() {
    if (this.isRendering) return;
    this.isRendering = true;
    
    const render = (now) => {
      if (!this.isRendering) return;
      
      // Update scroll animation
      this.updateScrollOffset();
      
      // Update all tape frames
      for (const tape of this.tapes.values()) {
        tape.updateFrame(now);
      }
      
      // Render composited frame
      this.renderFrame();
      
      // Audio crossfade based on scroll position
      this.updateAudioCrossfade();
      
      this.rafId = requestAnimationFrame(render);
    };
    
    this.rafId = requestAnimationFrame(render);
  }

  // Composite all tapes onto single canvas
  renderFrame() {
    if (!this.canvas || !this.ctx) return;
    
    const canvasHeight = this.canvas.height;
    
    // Clear canvas
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    
    // Render each tape at its scroll offset
    for (const tape of this.tapes.values()) {
      const frame = tape.getCurrentFrame();
      if (!frame) continue;
      
      // Calculate Y position based on scroll offset and tape's base offset
      const y = tape.yOffset + this.scrollOffset;
      
      // Only render if visible on screen
      if (y > -canvasHeight && y < canvasHeight * 2) {
        // Set alpha for fade effect
        this.ctx.globalAlpha = tape.alpha;
        
        // Draw frame
        this.ctx.drawImage(
          frame,
          0, // source x
          0, // source y
          frame.width,
          frame.height,
          0, // dest x
          y, // dest y (scrolled)
          this.canvas.width,
          canvasHeight
        );
        
        this.ctx.globalAlpha = 1.0; // Reset
      }
    }
  }

  // Update audio volumes based on scroll position (crossfade)
  updateAudioCrossfade() {
    const canvasHeight = this.canvas.height;
    
    for (const tape of this.tapes.values()) {
      if (!tape.isPlaying) continue;
      
      const y = tape.yOffset + this.scrollOffset;
      
      // Calculate volume based on vertical position
      // Center = full volume, edges = silent
      const distanceFromCenter = Math.abs(y);
      const fadeDistance = canvasHeight * 0.5; // Fade over half screen height
      const volume = Math.max(0, 1 - (distanceFromCenter / fadeDistance));
      
      tape.setVolume(volume);
    }
  }

  // Stop render loop
  stopRenderLoop() {
    this.isRendering = false;
    if (this.rafId) {
      cancelAnimationFrame(this.rafId);
      this.rafId = null;
    }
  }

  // Update canvas size (on resize)
  resize(width, height) {
    if (this.canvas) {
      this.canvas.width = width;
      this.canvas.height = height;
    }
  }

  // Destroy specific tape
  destroyTape(id) {
    const tape = this.tapes.get(id);
    if (tape) {
      tape.destroy();
      this.tapes.delete(id);
      console.log(`📼 Destroyed tape: ${id}`);
    }
  }

  // Cleanup everything
  cleanup() {
    this.stopRenderLoop();
    
    for (const tape of this.tapes.values()) {
      tape.destroy();
    }
    this.tapes.clear();
    
    this.underlayFrame?.remove();
    this.underlayFrame = null;
    this.canvas = null;
    this.ctx = null;
    
    console.log("📼 TapeManager cleaned up");
  }
}

// Global instance
let tapeManager = null;
```

#### 1.3 Update Message Handlers

Replace single `underlayFrame` logic with TapeManager:

```javascript
// Initialize manager on first tape:play
if (type === "tape:play") {
  if (!tapeManager) {
    // Configure based on device capabilities
    const isMobile = /Mobi|Android/i.test(navigator.userAgent);
    const maxTapes = isMobile ? 6 : 12; // Mobile: ±3, Desktop: ±6
    const preloadDistance = isMobile ? 3 : 6;
    
    tapeManager = new TapeManager({ maxTapes, preloadDistance });
    tapeManager.init();
  }
  
  const { code, zipUrl, metadata, tapeId = "main", index = 0 } = content;
  const tape = tapeManager.getTape(tapeId);
  
  // Update current index for preload management
  tapeManager.setCurrentIndex(index);
  
  // Load tape asynchronously
  tape.load(code, zipUrl, metadata)
    .then(() => {
      // Set as active and start playback
      tapeManager.setActive(tapeId);
      send({ type: "tape:playback-started", content: { code, tapeId } });
    })
    .catch(err => {
      send({ type: "tape:load-error", content: { code, error: err.message } });
    });
  
  return;
}

// New: Queue tape for preloading
if (type === "tape:preload") {
  if (!tapeManager) return;
  
  const { index, code, zipUrl, metadata, tapeId } = content;
  tapeManager.queuePreload(index, code, zipUrl, metadata, tapeId);
  
  return;
}

// New: Preload range of tapes around current
if (type === "tape:preload-range") {
  if (!tapeManager) return;
  
  const { currentIndex, feedLength } = content;
  tapeManager.preloadRange(currentIndex, feedLength);
  
  return;
}

// New: Preload tape (load but don't play)
if (type === "tape:cache") {
  if (!tapeManager) {
    tapeManager = new TapeManager();
    tapeManager.init();
  }
  
  const { code, zipUrl, metadata, tapeId } = content;
  const tape = tapeManager.getTape(tapeId);
  
  // Load in background, send progress updates
  tape.load(code, zipUrl, metadata)
    .then(() => {
      console.log(`📼 Cached tape ${code} (${tapeId})`);
      send({ type: "tape:cached", content: { code, tapeId } });
    })
    .catch(err => {
      console.error(`❌ Failed to cache tape ${code}:`, err);
    });
  
  return;
}

// New: Set scroll offset for multi-tape scrolling
if (type === "tape:scroll-offset") {
  if (!tapeManager) return;
  
  const { offset, tapePositions } = content;
  
  // Update scroll offset (animated via lerp in render loop)
  tapeManager.setScrollOffset(offset);
  
  // Update each tape's base Y position
  if (tapePositions) {
    for (const [tapeId, position] of Object.entries(tapePositions)) {
      const tape = tapeManager.tapes.get(tapeId);
      if (tape) {
        tape.yOffset = position.y;
        tape.alpha = position.alpha;
      }
    }
  }
  
  return;
}

// New: Set active tape
if (type === "tape:set-active") {
  if (!tapeManager) return;
  
  const { tapeId } = content;
  tapeManager.setActive(tapeId);
  
  return;
}

// New: Scrub to position within tape
if (type === "tape:scrub") {
  if (!tapeManager) return;
  
  const { tapeId, progress } = content;
  const tape = tapeManager.tapes.get(tapeId);
  
  if (tape && !tape.isLoading) {
    tape.setProgress(progress);
    
    // Send current progress back to piece
    send({ 
      type: "tape:scrub-progress", 
      content: { tapeId, progress: tape.getProgress() }
    });
  }
  
  return;
}

// New: Get current progress
if (type === "tape:get-progress") {
  if (!tapeManager) return;
  
  const { tapeId } = content;
  const tape = tapeManager.tapes.get(tapeId);
  
  if (tape) {
    send({
      type: "tape:scrub-progress",
      content: { tapeId, progress: tape.getProgress() }
    });
  }
  
  return;
}

// New: Pause/play current tape (tap to toggle)
if (type === "tape:toggle-play") {
  if (!tapeManager) return;
  
  const { tapeId } = content;
  const tape = tapeManager.tapes.get(tapeId);
  
  if (tape && !tape.isLoading) {
    if (tape.isPlaying) {
      tape.pause();
      console.log(`⏸️ Paused tape ${tapeId}`);
    } else {
      tape.play(tapeManager.audioContext);
      console.log(`▶️ Playing tape ${tapeId}`);
    }
  }
  
  return;
}

// Update tape:stop to handle manager
if (type === "tape:stop") {
  tapeManager?.cleanup();
  tapeManager = null;
  return;
}

// Handle canvas resize
if (type === "reframe" && tapeManager) {
  tapeManager.resize(content.width, content.height);
  return;
}
```

---

### Phase 2: tv.mjs - Multi-Tape Integration

**File:** `system/public/aesthetic.computer/disks/tv.mjs`

#### 2.1 Update State Management

```javascript
let tapes = [];
let currentIndex = 0;
let targetIndex = 0;
let displayIndex = 0; // Fractional for smooth scrolling
let isTransitioning = false;

// Adaptive cache size based on device
const isMobile = /Mobi|Android/i.test(navigator.userAgent);
const PRELOAD_DISTANCE = isMobile ? 3 : 6; // Mobile: ±3, Desktop: ±6

// Screen dimensions (from paint)
let screenHeight = 0;

// Track which tapes are loaded
let loadedTapes = new Set(); // Set of tape indices
```

#### 2.2 Smart Preloading System

```javascript
// Generate tape ID from index
function tapeIdForIndex(index) {
  return `tape-${index}`;
}

// Preload tapes in range around current position
function preloadTapesAroundIndex(index, send) {
  const startIndex = Math.max(0, index - PRELOAD_DISTANCE);
  const endIndex = Math.min(tapes.length - 1, index + PRELOAD_DISTANCE);
  
  console.log(`📼 Preloading tapes ${startIndex}-${endIndex} (current: ${index})`);
  
  for (let i = startIndex; i <= endIndex; i++) {
    if (i === index) continue; // Current tape already loaded/loading
    if (loadedTapes.has(i)) continue; // Already loaded
    
    const tape = tapes[i];
    const tapeId = tapeIdForIndex(i);
    
    send({
      type: "tape:preload",
      content: {
        index: i,
        code: tape.code,
        zipUrl: `${location.origin}/media/tapes/${tape.code}`,
        metadata: tape,
        tapeId: tapeId
      }
    });
    
    loadedTapes.add(i); // Mark as queued
  }
}

// Load and play tape at specific index
function loadAndPlayTape(index, send) {
  if (index < 0 || index >= tapes.length) return;
  
  const tape = tapes[index];
  const tapeId = tapeIdForIndex(index);
  
  send({
    type: "tape:play",
    content: {
      index: index,
      code: tape.code,
      zipUrl: `${location.origin}/media/tapes/${tape.code}`,
      metadata: tape,
      tapeId: tapeId
    }
  });
  
  loadedTapes.add(index);
  
  // Immediately start preloading adjacent tapes
  preloadTapesAroundIndex(index, send);
}
```

#### 2.3 Update Scroll Offset in paint()

```javascript
function paint({ wipe, ink, screen, send }) {
  screenHeight = screen.height;
  
  // Smooth scroll animation
  if (isTransitioning) {
    const transitionSpeed = 0.15;
    displayIndex += (targetIndex - displayIndex) * transitionSpeed;
    
    // Snap when close
    if (Math.abs(targetIndex - displayIndex) < 0.01) {
      displayIndex = targetIndex;
      isTransitioning = false;
      currentIndex = targetIndex;
      
      // Set new active tape
      const activeTapeId = tapeIdForIndex(currentIndex);
      send({ 
        type: "tape:set-active", 
        content: { tapeId: activeTapeId }
      });
      
      // Preload new range of tapes
      preloadTapesAroundIndex(currentIndex, send);
    }
  }
  
  // Calculate scroll offset in pixels
  const scrollOffset = (displayIndex - currentIndex) * screenHeight;
  
  // Calculate positions for all visible tapes in range
  const tapePositions = {};
  const visibleRange = Math.ceil(PRELOAD_DISTANCE / 2); // How many tapes visible at once
  
  for (let i = currentIndex - visibleRange; i <= currentIndex + visibleRange; i++) {
    if (i < 0 || i >= tapes.length) continue;
    
    const tapeId = tapeIdForIndex(i);
    const relativePosition = i - currentIndex; // -2, -1, 0, 1, 2...
    const baseY = relativePosition * screenHeight;
    
    // Calculate alpha based on distance from center (fade out edges)
    const distanceFromCenter = Math.abs(baseY + scrollOffset);
    const fadeDistance = screenHeight * 0.5;
    const alpha = Math.max(0, Math.min(1, 1 - (distanceFromCenter / fadeDistance)));
    
    tapePositions[tapeId] = {
      y: baseY,
      alpha: alpha
    };
  }
  
  // Send scroll state to BIOS
  send({
    type: "tape:scroll-offset",
    content: {
      offset: scrollOffset,
      tapePositions: tapePositions
    }
  });
  
  // Render UI overlay on main canvas
  wipe(0, 0, 0, 0); // Transparent
  renderScrollingUI(displayIndex, scrollOffset, ink, screen);
}
```

#### 2.4 Handle Gesture Detection (Scrub vs Scroll)

```javascript
// Gesture state - commit to scrub OR scroll based on initial direction
let gestureMode = null; // null, "scrub", "scroll"
let isScrubbing = false;
let scrubStartX = 0;
let scrubStartProgress = 0;
let scrubSensitivity = 2.0; // How much horizontal movement affects seek

function act({ event: e, send, screen }) {
  const now = performance.now();
  
  // Touch/draw start - record initial position
  if (e.is("touch") || e.is("draw")) {
    if (!isSwiping && !isTransitioning && !gestureMode) {
      swipeStartY = e.y;
      swipeStartX = e.x;
      isSwiping = true;
      gestureMode = null; // Will be determined on first movement
    }
  }
  
  // Handle drag - determine gesture mode on first movement
  if (e.is("draw") && isSwiping) {
    const deltaX = e.x - swipeStartX;
    const deltaY = e.y - swipeStartY;
    
    // Determine gesture mode if not yet set
    if (!gestureMode) {
      const absX = Math.abs(deltaX);
      const absY = Math.abs(deltaY);
      const threshold = 10; // Min pixels to determine direction
      
      if (absX > threshold || absY > threshold) {
        // Commit to mode based on dominant direction
        if (absX > absY) {
          gestureMode = "scrub"; // Horizontal = scrub within tape
          isScrubbing = true;
          
          // Get current progress from active tape
          const activeTapeId = tapeIdForIndex(currentIndex);
          send({
            type: "tape:get-progress",
            content: { tapeId: activeTapeId }
          });
          
          console.log("🎵 Started scrubbing (horizontal gesture)");
        } else {
          gestureMode = "scroll"; // Vertical = scroll between tapes
          console.log("� Started scrolling (vertical gesture)");
        }
      }
    }
    
    // Execute gesture based on committed mode
    if (gestureMode === "scrub") {
      // Horizontal scrub - seek within current tape
      const scrubDelta = (deltaX / screen.width) * scrubSensitivity;
      const targetProgress = Math.max(0, Math.min(1, scrubStartProgress + scrubDelta));
      
      const activeTapeId = tapeIdForIndex(currentIndex);
      send({
        type: "tape:scrub",
        content: {
          tapeId: activeTapeId,
          progress: targetProgress
        }
      });
    } 
    else if (gestureMode === "scroll") {
      // Vertical scroll - preview next/previous tape
      // Update displayIndex smoothly as user drags
      const scrollDelta = -deltaY / screen.height; // Negative because swipe up = next
      displayIndex = currentIndex + scrollDelta;
      
      // Clamp to valid range
      displayIndex = Math.max(0, Math.min(tapes.length - 1, displayIndex));
    }
  }
  
  if (e.is("lift")) {
    if (gestureMode === "scrub") {
      // End scrubbing
      isScrubbing = false;
      console.log("🎵 Stopped scrubbing");
    } 
    else if (gestureMode === "scroll") {
      // Complete scroll transition
      const swipeDistance = e.y - swipeStartY;
      const swipeDelay = 300;
      
      if (now - lastSwipeTime < swipeDelay || isLoadingTape || isTransitioning) {
        // Reset
        gestureMode = null;
        isSwiping = false;
        displayIndex = currentIndex; // Snap back
        return;
      }
      
      // Determine if swipe was significant enough to change tapes
      if (swipeDistance < -swipeThreshold && currentIndex < tapes.length - 1) {
        // Swipe up -> next tape
        targetIndex = currentIndex + 1;
        isTransitioning = true;
        lastSwipeTime = now;
      }
      else if (swipeDistance > swipeThreshold && currentIndex > 0) {
        // Swipe down -> previous tape
        targetIndex = currentIndex - 1;
        isTransitioning = true;
        lastSwipeTime = now;
      }
      else {
        // Not enough distance - snap back to current
        targetIndex = currentIndex;
        displayIndex = currentIndex;
      }
      
      console.log("📺 Stopped scrolling");
    }
    else if (!gestureMode && isSwiping) {
      // Quick tap without drag - toggle play/pause
      const tapDuration = now - (e.startTime || now);
      if (tapDuration < 200) {
        const activeTapeId = tapeIdForIndex(currentIndex);
        send({
          type: "tape:toggle-play",
          content: { tapeId: activeTapeId }
        });
        console.log("⏯️ Toggled play/pause");
      }
    }
    
    // Reset gesture state
    gestureMode = null;
    isSwiping = false;
  }
}

function receive({ type, content }) {
  if (type === "tape:cached") {
    console.log("📼 Cached tape:", content.code, content.tapeId);
  }
  
  if (type === "tape:playback-started") {
    console.log("📼 Tape playing:", content.code);
  }
  
  if (type === "tape:scrub-progress") {
    // Store current progress for scrubbing calculations
    scrubStartProgress = content.progress;
  }
}
```

---

### Phase 3: CSS Styling

**File:** `system/public/aesthetic.computer/style.css`

No changes needed! Reuses existing `#underlay` styles:

```css
#underlay {
  position: absolute;
  z-index: 0;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  display: flex;
}

#underlay canvas {
  display: block;
  width: 100%;
  height: 100%;
  object-fit: contain;
  background: rgb(32, 32, 32);
}
```

---

## 🎯 Success Criteria

- [ ] Smooth video fade transitions between tapes (300ms default)
- [ ] Preloading 3-6 tapes ahead/behind (mobile: ±3, desktop: ±6)
- [ ] No loading delay when swiping to adjacent tape
- [ ] Memory efficient (LRU eviction based on distance from current)
- [ ] Audio crossfade during transitions (distance-based volume)
- [ ] **Horizontal scrubbing** - drag left/right to seek within current tape (record scratch style)
- [ ] **Vertical scrolling** - drag up/down to browse between tapes (FYP style)
- [ ] **Gesture commitment** - first movement determines mode (scrub OR scroll, not both)
- [ ] **Live scroll preview** - see next/prev tape fade in while dragging vertically
- [ ] **Audio sync** - audio follows scrub position accurately
- [ ] **Stateful playback** - tapes remember play position when scrolling away
- [ ] **Resume on return** - scroll back to tape resumes from saved position if it was playing
- [ ] **Tap to pause** - quick tap toggles play/pause
- [ ] Works on mobile and desktop
- [ ] Backward compatible with existing tape playback (`video.mjs`, `replay.mjs`)
- [ ] Smart preload queue (closest tapes loaded first)
- [ ] Configurable cache size (3, 6, or 12 tapes)

---

## 🧪 Testing Plan

1. **Single tape playback** - Verify existing `replay.mjs` still works
2. **Multi-tape transitions** - Test `tv.mjs` swipe up/down with smooth fades
3. **Horizontal scrubbing** - Test drag left/right seeks within tape (record scratch)
4. **Vertical scrolling** - Test drag up/down shows live preview of next/prev tape
5. **Gesture commitment** - Ensure first direction locks mode:
   - Start horizontal → scrub only (no scroll)
   - Start vertical → scroll only (no scrub)
   - 10px threshold before mode is determined
6. **Audio scrubbing** - Verify audio syncs when seeking
7. **Scroll preview** - Adjacent tapes fade in/out during vertical drag
8. **Stateful playback**:
   - Scroll to tape A, let it play, scroll to B, scroll back to A → A resumes playing
   - Pause tape A, scroll to B, scroll back to A → A stays paused
   - Scrub tape A to 50%, scroll away, scroll back → A at 50%
9. **Tap to pause** - Quick tap without drag toggles play/pause
10. **Preloading efficiency** - Monitor network tab for parallel downloads
11. **Memory usage** - Verify LRU eviction when scrolling through many tapes
12. **Edge cases**:
   - First tape (no previous) - vertical drag down shows resistance/bounce
   - Last tape (no next) - vertical drag up shows resistance/bounce
   - Rapid gestures (mode commitment)
   - Failed tape loads (skip to next working tape)
   - Scrubbing at tape boundaries (0% and 100%)
   - Cancel gesture (lift before threshold) - snap back to original position
   - Scroll through 20 tapes, scroll back to first - state preserved for all cached tapes

---

## 💡 Future Enhancements

- **Infinite scroll**: Fetch more tapes from `/api/tv` when reaching end
- **Gesture velocity**: Faster swipes = faster transitions
- **Scrub inertia**: Like `video.mjs` - needle with velocity/lag for smooth feel
- **Bounce effect**: Elastic resistance at first/last tape (rubber-band effect)
- **Scrub preview**: Show thumbnail or waveform during horizontal drag
- **Pitch shifting**: Audio pitch changes during fast scrub (like vinyl/turntable)
- **Haptic feedback**: Vibrate on tape change or scrub boundaries (mobile)
- **Thumbnail previews**: Show frame preview during vertical scroll
- **Adaptive quality**: Lower resolution frames on slow connections
- **Autoplay next**: After tape ends, auto-transition to next (like YouTube shorts)
- **Background preloading**: Preload in idle time using `requestIdleCallback`
- **Network-aware preloading**: Reduce cache size on slow connections
- **Priority preloading**: Prioritize direction of scroll (more tapes ahead if scrolling down)
- **Two-finger gestures**: Pinch to zoom, rotate to adjust playback speed
- **Waveform visualization**: Show audio waveform overlay during scrub

---

## 📚 References

- **Current implementation**: `tv.mjs` (basic swipe navigation)
- **Single underlay system**: `bios.mjs` lines 10485-10981
- **Tape loading**: `bios.mjs` lines 7686-8070 (`tape:play` handler)
- **Existing tape playback**: `video.mjs`, `replay.mjs`
- **TikTok FYP pattern**: Vertical feed with smooth swipe transitions

---

## ⚠️ Breaking Changes

**None** - This is additive architecture:
- Old `tape:play` still works (uses default `underlayId: "main"`)
- New `tape:cache` and `tape:transition` are opt-in
- Backward compatible with single-tape pieces

---

## 🚀 Rollout Strategy

1. **Phase 1**: Implement BIOS infrastructure (classes + handlers)
2. **Phase 2**: Update `tv.mjs` to use multi-tape system
3. **Phase 3**: Test and refine transition timing/feel
4. **Phase 4**: Document API for future pieces (chat feed, gallery, etc.)
5. **Phase 5**: Consider extracting to shared library (`disks/common/multi-tape.mjs`)

---

**Status:** Ready for implementation  
**Estimated effort:** 2-3 days development + testing  
**Next step:** Review plan, then implement Phase 1 (BIOS classes)
