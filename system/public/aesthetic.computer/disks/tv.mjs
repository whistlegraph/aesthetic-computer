// TV, 2025.11.10.21.00
// A For You page / vertical feed for browsing !tape codes from the tv endpoint.

/* 📝 Features
   - Fetch tape feed from /api/tv endpoint
   - Swipeable vertical feed (FYP style scrolling)
   - Horizontal scrubbing within tapes (record scratch style)
   - Gesture commitment (initial direction determines scrub OR scroll mode)
   - Stateful playback (tapes remember position when scrolling)
   - Smart preloading (6-12 tapes ahead/behind)
   - Smooth transitions with audio crossfade
*/

import { createScrubber } from "./common/scrub.mjs";

let tapes = [];
let currentIndex = 0;
let loadingFeed = false;
let feedLoadError = null;

// Loading state - track multiple tapes loading simultaneously
let loadingTapes = new Map(); // Map<tapeId, {progress: 0-1, phase: string}>
let firstTapeReady = false; // Flag to clear loading screen once first tape plays

// Gesture state
let gestureMode = null; // null, "scrub", or "scroll"
let gestureStartX = 0;
let gestureStartY = 0;

// Scrubber instance (physics-based lazy needle scrubbing)
let scrubber = createScrubber();
let wasPlayingBeforeScrub = false; // Track play state before scrubbing
let lastSeekProgress = 0; // Track last seek position to avoid spam
const seekThreshold = 0.002; // Only seek if needle moved more than 0.2%

// Smooth transition variables
let targetIndex = 0; // Where we're transitioning to
let displayIndex = 0; // Current visual position (fractional for smooth scrolling)
let isTransitioning = false;
let lastSwipeTime = 0;
const swipeThreshold = 30;
const gestureThreshold = 10; // Pixels before committing to scrub or scroll mode

// Preloading state
let maxPreloadDistance = 3; // Will be set in boot based on screen size
let preloadedTapeIds = new Set();
let preloadQueue = []; // Queue for serialized preloading
let isPreloading = false; // Only load one tape at a time

// Logging flags (module-level, not window)
let tvPresentingLogged = false;

// Global send function (set in boot, used everywhere)
let globalSend = null;

// Paint state
let paintCount = 0n;

// Playback state
let isPlaying = false;
let isPaused = false;
let currentProgress = 0; // Track current playback progress for progress bar

// AudioContext state (same as video.mjs)
let audioContextState = "suspended"; // suspended, running, closed
let hasAudioContext = false;
let audioManuallyActivated = false; // Track if user has manually activated audio

// Handle audio context resume on first tap - BIOS will automatically start tape audio
async function handleAudioContextAndPlay() {
  console.log("🎵 Requesting BIOS to resume audio context - BIOS will auto-start tape audio");
  
  // Send resume request - BIOS will automatically detect tape playing and start audio
  if (globalSend) {
    globalSend({
      type: "audio-context:resume-request",
      content: {
        userGesture: true,
        source: "tv-piece-first-tap",
        forceResume: true // Force resume even if state appears to be running
      }
    });
  } else {
    console.warn("🎵 No globalSend available - cannot request audio context resume");
  }
  
  console.log("🎵 Audio context resume requested - BIOS will handle the rest");
}

function boot({ wipe, send, notice, hud, screen }) {
  wipe(0);
  globalSend = send; // Store for use in other functions
  loadingFeed = true;
  
  // Set preload distance based on screen size
  maxPreloadDistance = screen.width > 768 ? 6 : 3; // Desktop: ±6, Mobile: ±3
  
  fetch("/api/tv?types=tape&limit=50")
    .then(res => {
      if (!res.ok) throw new Error("Failed to load feed: " + res.status);
      return res.json();
    })
    .then(data => {
      console.log("📺 TV feed loaded:", data);
      if (data.media && data.media.tapes && data.media.tapes.length > 0) {
        tapes = data.media.tapes;
        loadingFeed = false;
        
        // SIMPLIFIED: Load ONLY first tape for testing
        console.log("📼 Loading ONLY first tape for testing");
        loadTapeAtIndex(0);
        // preloadTapesAroundIndex(0); // DISABLED for testing
      } else {
        throw new Error("No tapes in feed");
      }
    })
    .catch(err => {
      console.error("❌ Failed to load TV feed:", err);
      feedLoadError = err.message;
      loadingFeed = false;
    });
}

function paint({ wipe, ink, screen, rec, line, needsPaint }) {
  paintCount += 1n;
  
  // Smooth transition logic
  if (isTransitioning) {
    const transitionSpeed = 0.15;
    displayIndex += (targetIndex - displayIndex) * transitionSpeed;
    
    // Snap to target when very close
    if (Math.abs(targetIndex - displayIndex) < 0.01) {
      displayIndex = targetIndex;
      isTransitioning = false;
      
      // Update active tape in BIOS when transition completes
      if (currentIndex !== targetIndex) {
        const previousIndex = currentIndex;
        currentIndex = targetIndex;
        
        // Check if this tape exists in our feed
        if (currentIndex < tapes.length) {
          const tape = tapes[currentIndex];
          const tapeId = tapeIdForIndex(currentIndex);
          
          // Load the tape if it hasn't been loaded yet
          if (tape && tape.code) {
            const loadingState = loadingTapes.get(tapeId);
            const isLoaded = preloadedTapeIds.has(tapeId);
            
            if (!isLoaded && !loadingState) {
              // Not loaded and not loading - start loading now
              loadTapeAtIndex(currentIndex);
            } else if (loadingState) {
              // Currently loading - just wait for it
            } else {
              // Already loaded - set it active (TapeManager handles play/pause state)
              globalSend({ type: "tape:set-active", content: { tapeId } });
              // TapeManager's setActive() will automatically resume if it was playing before
            }
            
            // Preload tapes around new position
            preloadTapesAroundIndex(currentIndex);
          }
        } else {
          console.warn(`📺 Cannot switch to tape ${currentIndex} - index out of bounds (have ${tapes.length} tapes)`);
          // Snap back to last valid tape
          currentIndex = tapes.length - 1;
          targetIndex = currentIndex;
          displayIndex = currentIndex;
        }
      }
    }
  }
  
  // Calculate slide offset for scroll preview
  const slideOffset = (displayIndex - currentIndex) * screen.height;
  
  // Send scroll offsets to BIOS for smooth video transitions
  if (isTransitioning && Math.abs(slideOffset) > 1) {
    const tapePositions = [];
    
    // Current tape position
    tapePositions.push({
      id: tapeIdForIndex(currentIndex),
      yOffset: -slideOffset,
      alpha: 1 - Math.abs(slideOffset / screen.height)
    });
    
    // Next/prev tape position
    if (targetIndex !== currentIndex) {
      const nextYOffset = targetIndex > currentIndex
        ? screen.height - slideOffset
        : -screen.height - slideOffset;
      tapePositions.push({
        id: tapeIdForIndex(targetIndex),
        yOffset: nextYOffset,
        alpha: Math.abs(slideOffset / screen.height)
      });
    }
    
    globalSend({ type: "tape:scroll-offset", content: { tapePositions } });
  }
  
  // Transparent wipe when tape is playing to show video
  const fadeAmount = Math.abs(slideOffset / screen.height);
  const hasActiveTape = tapes.length > 0 && !loadingFeed && !feedLoadError;
  
  if (hasActiveTape && firstTapeReady) {
    // Clear screen to show video underneath
    if (isPlaying && !isPaused) {
      wipe(0, 0, 0, 0); // Transparent - allows TapeManager underlay to show through
    } else {
      // Paused - show overlay
      wipe(0, 100).ink(255, 200).write("||", { center: "xy" });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }
    
    // Scrub overlay - show when actively scrubbing
    if (scrubber.isScrubbing) {
      // Dark overlay
      ink(0, 0, 0, 100).box(0, 0, screen.width, screen.height);
      
      // Show target (where user is dragging) vs needle (where playback is)
      const targetX = Math.floor(scrubber.targetProgress * screen.width);
      const needleX = Math.floor(scrubber.needleProgress * screen.width);
      
      // Target line (user drag position) - bright
      ink(255, 200, 0, 200).line(targetX, 0, targetX, screen.height);
      
      // Needle line (playback position) - dimmer, shows lag
      ink(255, 100, 0, 150).line(needleX, 0, needleX, screen.height);
      
      // Progress text
      const targetPercent = Math.floor(scrubber.targetProgress * 100);
      const needlePercent = Math.floor(scrubber.needleProgress * 100);
      ink(255, 255, 255).write(
        `${needlePercent}%`,
        { center: "xy", size: 2 }
      );
    }
    
    // Log once for debugging
    if (!tvPresentingLogged) {
      console.log("📼 🎬 Screen cleared - TapeManager should be rendering to underlay");
      tvPresentingLogged = true;
    }
    
    // TapeManager renders automatically via bios.mjs renderFrame loop
    // No need to call rec.present() - that's for old single-tape system
    
    // Fade overlay during transition
    if (isTransitioning && fadeAmount > 0) {
      const fadeAlpha = Math.floor(fadeAmount * 100);
      ink(0, 0, 0, fadeAlpha).box(0, 0, screen.width, screen.height);
    }
    
    // --- LOADING INDICATOR (for current tape if loading) ---
    // Only show if tape isn't ready yet
    if (!firstTapeReady || isTransitioning) {
      const currentTapeId = tapeIdForIndex(currentIndex);
      const loadingState = loadingTapes.get(currentTapeId);
      
      if (loadingState) {
        // Semi-transparent overlay
        ink(0, 0, 0, 180).box(0, 0, screen.width, screen.height);
        
        // Spinner animation
        const spinnerSize = 40;
        const centerX = screen.width / 2;
        const centerY = screen.height / 2;
        const angle = (Number(paintCount) * 0.1) % (Math.PI * 2);
        
        // Draw spinning circle segments
        for (let i = 0; i < 8; i++) {
          const segAngle = angle + (i * Math.PI / 4);
          const alpha = Math.floor(255 * (1 - i / 8));
          const x = centerX + Math.cos(segAngle) * spinnerSize;
          const y = centerY + Math.sin(segAngle) * spinnerSize;
          ink(255, 200, 100, alpha).circle(x, y, 4, true);
        }
        
        // Loading text
        const phaseText = loadingState.phase === "fetching" 
          ? "LOADING..." 
          : "DOWNLOADING...";
        
        ink(255, 200, 100).write(phaseText, {
          x: centerX,
          y: centerY + 60,
          center: "xy",
        });
        
        // Tape code
        if (loadingState.code) {
          ink(200, 200, 200).write(loadingState.code, {
            x: centerX,
            y: centerY + 80,
            center: "xy",
          });
        }
      }
    }
    
    // --- PROGRESS BAR AT BOTTOM (like video.mjs) ---
    // Set rec.tapeProgress from current progress for VHS-style progress bar
    if (rec && currentProgress > 0 && currentProgress < 1) {
      rec.tapeProgress = currentProgress;
    } else if (rec) {
      rec.tapeProgress = 0;
    }
    
    // Keep repainting to update progress bar
    needsPaint();
    
    // --- AUDIO WARNING (like video.mjs) ---
    const audioSuspended = !audioManuallyActivated && hasAudioContext;
    
    if (audioSuspended) {
      const pulse = Math.abs(Math.sin(Number(paintCount) * 0.1)); // Convert BigInt to Number
      const alpha = Math.floor(128 + pulse * 127);
      const centerX = screen.width / 2;
      const centerY = screen.height / 2;
      
      // Semi-transparent dark overlay
      ink(0, 0, 0, 160).box(0, 0, screen.width, screen.height);
      
      // Pulsing prompt text
      ink(255, 200, 100, alpha).write("TAP TO ENABLE AUDIO", {
        x: centerX,
        y: centerY,
        center: "xy",
      });
      
      // Smaller instruction text
      ink(200, 200, 200, alpha * 0.8).write("Audio requires user interaction", {
        x: centerX,
        y: centerY + 16,
        center: "xy",
      });
      
      needsPaint(); // Keep animating the pulse
    }
  } else {
    wipe(0);
  }
  
  if (loadingFeed) {
    ink("white").write("LOADING FEED...", { center: "xy", size: 2 });
    return;
  }
  
  if (feedLoadError) {
    ink("red").write("FEED ERROR", { center: "xy", size: 2 });
    ink("white").write(feedLoadError, { center: "xy", y: screen.height / 2 + 20 });
    return;
  }
  
  // Show individual progress bars for ALL loading tapes
  if (loadingTapes.size > 0) {
    const progressBars = Array.from(loadingTapes.entries()).filter(([id]) => id !== null);
    
    if (progressBars.length === 0) return;
    
    // Separate current (first) from queue (rest)
    const [currentTapeId, currentState] = progressBars[0];
    const queuedTapes = progressBars.slice(1);
    
    // --- MAIN PROGRESS BAR (CENTER) ---
    const mainBarWidth = screen.width * 0.8;
    const mainBarHeight = 24;
    const mainBarX = (screen.width - mainBarWidth) / 2;
    const mainBarY = (screen.height - mainBarHeight) / 2;
    
    // Extract tape info
    const currentIndex = parseInt(currentTapeId.split('-').pop());
    const currentTape = tapes[currentIndex];
    const currentCode = currentTape?.code || currentTapeId;
    
    // Title above bar
    ink("white").write("LOADING", { center: "x", y: mainBarY - 40 });
    
    // Tape code (large font)
    ink(255, 200, 100).write(currentCode.toUpperCase(), { center: "x", y: mainBarY - 24 }, undefined, undefined, false, "gnu-unifont-16");
    
    // Progress bar background
    ink(0, 80).box(mainBarX, mainBarY, mainBarWidth, mainBarHeight);
    
    // Progress fill
    const mainFillWidth = Math.floor(mainBarWidth * currentState.progress);
    if (mainFillWidth > 0) {
      const color = hslToRgb(120, 70, 60); // Green for main
      ink(...color).box(mainBarX, mainBarY, mainFillWidth, mainBarHeight, "fill");
    }
    
    // Progress bar outline
    ink(200, 200, 200).box(mainBarX, mainBarY, mainBarWidth, mainBarHeight, "outline");
    
    // Percentage below bar
    const mainPercent = Math.floor(currentState.progress * 100);
    ink("white").write(`${mainPercent}%`, { center: "x", y: mainBarY + mainBarHeight + 15 });
    
    // Phase indicator (small font)
    ink(150, 150, 150).write(currentState.phase.toUpperCase(), { center: "x", y: mainBarY + mainBarHeight + 30 }, undefined, undefined, false, "MatrixChunky8");
    
    // --- UP NEXT QUEUE (BOTTOM) ---
    if (queuedTapes.length > 0) {
      const queueY = screen.height - 80;
      ink(150, 150, 150).write("UP NEXT", { center: "x", y: queueY }, undefined, undefined, false, "MatrixChunky8");
      
      const dotSize = 8;
      const dotSpacing = 16;
      const totalWidth = queuedTapes.length * dotSpacing - (dotSpacing - dotSize);
      const startX = (screen.width - totalWidth) / 2;
      
      queuedTapes.forEach(([tapeId, state], index) => {
        const dotX = startX + (index * dotSpacing);
        const dotY = queueY + 15;
        
        // Extract tape code
        const tapeIndex = parseInt(tapeId.split('-').pop());
        const tape = tapes[tapeIndex];
        const code = tape?.code || "?";
        
        // Dot color based on progress
        const hue = (index * 60) % 360;
        const color = hslToRgb(hue, 70, 60);
        const alpha = Math.floor(50 + state.progress * 205); // Fade in as it loads
        
        // Draw dot
        ink(...color, alpha).box(dotX, dotY, dotSize, dotSize, "fill");
        
        // Code label below dot
        ink(120, 120, 120).write(code, { x: dotX - 6, y: dotY + dotSize + 8 }, undefined, undefined, false, "MatrixChunky8");
      });
    }

    return;
  }
  
  if (tapes.length === 0) {
    ink("yellow").write("NO TAPES", { center: "xy", size: 2 });
    return;
  }
  
  // Render scrolling UI with tape info
  renderTapeUI(currentIndex, -slideOffset, ink, line, screen);
  
  // During transition, render next/previous tape UI
  if (isTransitioning) {
    if (targetIndex > currentIndex) {
      renderTapeUI(targetIndex, screen.height - slideOffset, ink, line, screen);
      const dividerY = Math.floor(screen.height - slideOffset);
      if (dividerY > 0 && dividerY < screen.height) {
        ink(80, 80, 80).line(0, dividerY, screen.width, dividerY);
      }
    } else if (targetIndex < currentIndex) {
      renderTapeUI(targetIndex, -screen.height - slideOffset, ink, line, screen);
      const dividerY = Math.floor(-slideOffset);
      if (dividerY > 0 && dividerY < screen.height) {
        ink(80, 80, 80).line(0, dividerY, screen.width, dividerY);
      }
    }
  }
  
  // Show gesture mode debug (optional)
  if (gestureMode) {
    const modeText = gestureMode === "scrub" ? "🎵 SCRUB" : "📺 SCROLL";
    ink("yellow").write(modeText, { right: 10, y: 50 });
  }
}

function renderTapeUI(tapeIndex, yOffset, ink, line, screen) {
  if (tapeIndex < 0 || tapeIndex >= tapes.length) return;
  
  const tape = tapes[tapeIndex];
  const baseY = 10 + yOffset;
  
  if (baseY > -screen.height && baseY < screen.height * 2) {
    if (tape.code) {
      ink("white").write(`!${tape.code}`, { x: 10, y: baseY });
    }
    
    if (tape.owner && tape.owner.handle) {
      ink("yellow").write(tape.owner.handle, { x: 10, y: baseY + 20 });
    }
    
    ink("gray").write(`${tapeIndex + 1} / ${tapes.length}`, { right: 10, y: baseY });
    
    const hintY = baseY + screen.height - 30;
    if (hintY > 0 && hintY < screen.height) {
      ink("gray").write("↕ SCROLL  ↔ SCRUB  TAP PAUSE", { center: "x", y: hintY });
    }
  }
}

function act({ event: e, send, screen }) {
  const now = performance.now();
  
  // Start gesture on INITIAL touch only (not on every draw event)
  if (e.is("touch") && !gestureMode && !isTransitioning) {
    gestureStartX = e.x;
    gestureStartY = e.y;
  }
  
  // Determine gesture mode based on movement (10px threshold)
  // Check on every touch/draw/drag event
  if ((e.is("draw") || e.drag) && !gestureMode && !isTransitioning) {
    const deltaX = Math.abs(e.x - gestureStartX);
    const deltaY = Math.abs(e.y - gestureStartY);
    const totalMovement = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
    
    if (totalMovement > gestureThreshold) {
      if (deltaX > deltaY) {
        // Horizontal movement -> scrub mode
        gestureMode = "scrub";
        
        // Start scrubber with current progress
        wasPlayingBeforeScrub = isPlaying;
        scrubber.start(e, currentProgress, isPlaying);
        lastSeekProgress = currentProgress; // Reset seek tracking
      } else {
        // Vertical movement -> scroll mode
        gestureMode = "scroll";
        displayIndex = currentIndex; // Reset for live preview
      }
    }
  }
  
  // Handle active scrub gesture
  if ((e.is("draw") || e.drag) && gestureMode === "scrub") {
    // Horizontal scrubbing - update target progress
    scrubber.drag(e, screen.width);
    
    // Only send seek command if needle moved significantly (throttle to reduce spam)
    const progressDelta = Math.abs(scrubber.needleProgress - lastSeekProgress);
    if (progressDelta > seekThreshold) {
      const tapeId = tapeIdForIndex(currentIndex);
      globalSend({
        type: "tape:seek",
        content: { tapeId, progress: scrubber.needleProgress }
      });
      lastSeekProgress = scrubber.needleProgress;
    }
  }
  
  // Handle active scroll gesture  
  if ((e.is("draw") || e.drag) && gestureMode === "scroll") {
    // Vertical scrolling with live preview
    const scrollDelta = (e.y - gestureStartY) / screen.height;
    displayIndex = currentIndex - scrollDelta;
    
    // Clamp displayIndex to valid range
    displayIndex = Math.max(0, Math.min(tapes.length - 1, displayIndex));
  }
  
  if (e.is("lift")) {
    if (gestureMode === "scrub") {
      // End scrubbing - activate inertia if has velocity
      scrubber.end();
      
      // Resume playback if it was playing before scrub
      if (wasPlayingBeforeScrub) {
        const tapeId = tapeIdForIndex(currentIndex);
        globalSend({
          type: "tape:play",
          content: { tapeId }
        });
      }
    } 
    else if (gestureMode === "scroll") {
      // Complete scroll transition
      const swipeDistance = e.y - gestureStartY;
      const swipeDelay = 300;
      
      if (now - lastSwipeTime < swipeDelay || isTransitioning) {
        gestureMode = null;
        displayIndex = currentIndex; // Snap back
        return;
      }
      
      // Determine if swipe was significant enough
      if (swipeDistance < -swipeThreshold && currentIndex < tapes.length - 1) {
        targetIndex = currentIndex + 1;
        isTransitioning = true;
        lastSwipeTime = now;
      }
      else if (swipeDistance > swipeThreshold && currentIndex > 0) {
        targetIndex = currentIndex - 1;
        isTransitioning = true;
        lastSwipeTime = now;
      }
      else {
        // Not enough distance or at boundary - snap back
        targetIndex = currentIndex;
        displayIndex = currentIndex;
      }
    }
    else if (!gestureMode) {
      // Quick tap without drag - toggle play/pause
      const tapDuration = now - (e.startTime || now);
      if (tapDuration < 200) {
        // IMPORTANT: Check if user needs to manually activate audio FIRST
        // When AudioContext needs user gesture, tape plays silently
        // but we want the first tap to enable audio, not pause the tape
        if (!audioManuallyActivated && hasAudioContext) {
          audioManuallyActivated = true;
          handleAudioContextAndPlay();
          console.log("🎵 First tap - activating audio context");
          return; // Exit early to prevent pause
        }
        
        // Normal toggle when audio is ready
        const activeTapeId = tapeIdForIndex(currentIndex);
        globalSend({
          type: "tape:toggle-play",
          content: { tapeId: activeTapeId }
        });
      }
    }
    
    // Reset gesture state
    gestureMode = null;
  }
}

function loadTapeAtIndex(index) {
  if (index < 0 || index >= tapes.length) return;
  
  const tape = tapes[index];
  if (!tape || !tape.code) return;
  
  const tapeCode = tape.code;
  const tapeId = tapeIdForIndex(index);
  
  // Skip if already loaded or loading
  if (loadingTapes.has(tapeId) || preloadedTapeIds.has(tapeId)) {
    return tapeId;
  }
  
  // Initialize loading state for this tape
  loadingTapes.set(tapeId, { 
    progress: 0, 
    phase: "fetching",
    index: index,
    code: tapeCode
  });
  
  fetch(`/api/get-tape?code=${tapeCode}`)
    .then(res => {
      if (!res.ok) throw new Error(`Failed to load tape: ${res.status}`);
      return res.json();
    })
    .then(metadata => {
      if (metadata.nuked) {
        throw new Error(`Tape deleted`);
      }
      
      const zipUrl = `${location.origin}/media/tapes/${tapeCode}`;
      
      const tapeState = loadingTapes.get(tapeId);
      if (tapeState) {
        tapeState.phase = "downloading";
        tapeState.metadata = metadata;
      }
      
      // Use preload to load through TapeManager
      globalSend({
        type: "tape:preload",
        content: {
          tapeId: tapeId,
          code: tapeCode,
          zipUrl: zipUrl,
          metadata: metadata
        }
      });
    })
    .catch(err => {
      console.error(`❌ Failed to load tape ${tapeCode}:`, err);
      loadingTapes.delete(tapeId);
    });
  
  return tapeId;
}

function preloadTapesAroundIndex(centerIndex) {
  // Build list of tapes to preload (closest first)
  const toPreload = [];
  
  for (let distance = 1; distance <= maxPreloadDistance; distance++) {
    const prevIndex = centerIndex - distance;
    const nextIndex = centerIndex + distance;
    
    if (prevIndex >= 0) toPreload.push(prevIndex);
    if (nextIndex < tapes.length) toPreload.push(nextIndex);
  }
  
  // Add to queue if not already preloaded or queued
  for (const index of toPreload) {
    const tape = tapes[index];
    const tapeId = tapeIdForIndex(index);
    
    if (!tape || !tape.code) continue;
    if (preloadedTapeIds.has(tapeId)) continue;
    if (preloadQueue.find(item => item.tapeId === tapeId)) continue;
    
    preloadQueue.push({ index, tapeId, code: tape.code });
  }
  
  // Start processing queue
  processPreloadQueue();
}

function processPreloadQueue() {
  // DISABLED for testing - only load first tape
  console.log("📼 processPreloadQueue DISABLED - testing single tape only");
  return;
  
  if (isPreloading || preloadQueue.length === 0) return;
  
  isPreloading = true;
  const { index, tapeId, code } = preloadQueue.shift();
  
  console.log(`📼 Preloading tape ${index}: ${code}`);
  
  fetch(`/api/get-tape?code=${code}`)
    .then(res => {
      if (!res.ok) throw new Error(`Failed to load tape: ${res.status}`);
      return res.json();
    })
    .then(metadata => {
      if (metadata.nuked) {
        isPreloading = false;
        processPreloadQueue(); // Continue to next
        return;
      }
      
      const zipUrl = `${location.origin}/media/tapes/${code}`;
      
      globalSend({
        type: "tape:preload",
        content: {
          tapeId: tapeId,
          code: code,
          zipUrl: zipUrl,
          metadata: metadata
        }
      });
      
      preloadedTapeIds.add(tapeId);
      // Don't set isPreloading = false here - wait for tape:preloaded event
    })
    .catch(err => {
      console.error(`❌ Failed to preload tape ${code}:`, err);
      isPreloading = false;
      processPreloadQueue(); // Continue to next
    });
}

function tapeIdForIndex(index) {
  if (index < 0 || index >= tapes.length) return null;
  return `tv-tape-${index}`;
}

function receive({ content, type, send }) {
  // Minimal logging - only important state changes
  
  if (type === "tape:load-progress") {
    // Update loading progress for specific tape
    // BIOS sends content.code (tape code like "opy"), we need to find tapeId
    let tapeId = content.tapeId;
    
    if (!tapeId && content.code) {
      // Find tapeId by matching code in tapes array
      const index = tapes.findIndex(t => t.code === content.code);
      if (index >= 0) {
        tapeId = tapeIdForIndex(index);
      }
    }
    
    if (!tapeId) {
      tapeId = tapeIdForIndex(currentIndex);
    }
    
    if (!loadingTapes.has(tapeId)) {
      loadingTapes.set(tapeId, { progress: 0, phase: "" });
    }
    
    const tapeState = loadingTapes.get(tapeId);
    if (content.phase) {
      tapeState.phase = content.phase;
    }
    if (typeof content.progress === "number") {
      tapeState.progress = content.progress;
    }
    
    // When tape finishes loading (phase: "complete"), treat like tape:preloaded
    if (content.phase === "complete" && content.progress === 1) {
      // Remove from loading tracking
      loadingTapes.delete(tapeId);
      preloadedTapeIds.add(tapeId);
      
      // Mark preloading as complete (don't process queue - disabled for testing)
      isPreloading = false;
      // processPreloadQueue(); // DISABLED for single tape test
      
      // Find which index this tape is at
      const completedIndex = tapes.findIndex(t => t.code === content.code);
      
      // Only set active and auto-play if this is STILL the current tape
      // (User might have swiped away while it was loading)
      if (completedIndex === currentIndex && !isTransitioning) {
        globalSend({ type: "tape:set-active", content: { tapeId } });
        
        // Start playback if audio is enabled and we should be playing
        if (audioManuallyActivated) {
          globalSend({ 
            type: "tape:toggle-play",
            content: { tapeId, play: true }
          });
        }
        
        // Small delay to let TapeManager render first frame before clearing screen
        if (!firstTapeReady) {
          setTimeout(() => {
            firstTapeReady = true; // Clear loading screen
          }, 100);
        }
      } else if (completedIndex !== currentIndex) {
        // Tape loaded in background - keep it ready for when user swipes back
      }
    }
  }
  
  if (type === "tape:progress-reply" || type === "tape:progress-update" || type === "tape:playback-progress") {
    // Store progress and playback state
    if (content.tapeId === tapeIdForIndex(currentIndex)) {
      currentProgress = content.progress || 0;
      isPlaying = content.isPlaying || false;
      isPaused = content.isPaused || false;
    }
  }
  
  // Handle AudioContext state updates from BIOS (same as video.mjs)
  if (type === "tape:audio-context-state") {
    audioContextState = content.state;
    hasAudioContext = (content.state === "running" || content.state === "suspended");
    
    // If audio context is already running, mark as manually activated
    if (content.state === "running") {
      audioManuallyActivated = true;
    }
    
    console.log(`🎵 AudioContext state: ${content.state}, hasAudioContext: ${hasAudioContext}, audioManuallyActivated: ${audioManuallyActivated}`);
  }
}

// Helper function to convert HSL to RGB for colorful progress bars
function hslToRgb(h, s, l) {
  s /= 100;
  l /= 100;
  const k = n => (n + h / 30) % 12;
  const a = s * Math.min(l, 1 - l);
  const f = n => l - a * Math.max(-1, Math.min(k(n) - 3, Math.min(9 - k(n), 1)));
  return [Math.round(255 * f(0)), Math.round(255 * f(8)), Math.round(255 * f(4))];
}

// Simulation - runs physics for scrubber
function sim({ simCount, needsPaint }) {
  if (scrubber.isScrubbing || scrubber.inertiaActive) {
    // Run physics simulation every frame
    scrubber.simulate();
    
    // Send seek command if needle moved significantly (throttled)
    const progressDelta = Math.abs(scrubber.needleProgress - lastSeekProgress);
    if (progressDelta > seekThreshold) {
      const tapeId = tapeIdForIndex(currentIndex);
      globalSend({
        type: "tape:seek",
        content: { tapeId, progress: scrubber.needleProgress }
      });
      
      lastSeekProgress = scrubber.needleProgress;
      // Also update currentProgress for progress bar
      currentProgress = scrubber.needleProgress;
    }
    
    // Stop inertia when needle settles
    if (scrubber.inertiaActive && Math.abs(scrubber.needleVelocity) < 0.0001) {
      scrubber.reset();
    }
    
    // Keep repainting for smooth animation
    if (needsPaint) needsPaint();
  }
}

export { boot, paint, act, receive, sim };
