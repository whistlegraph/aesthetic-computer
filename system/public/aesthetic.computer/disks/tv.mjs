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

let tapes = [];
let currentIndex = 0;
let loadingFeed = false;
let feedLoadError = null;

// Gesture state
let gestureMode = null; // null, "scrub", or "scroll"
let gestureStartX = 0;
let gestureStartY = 0;
let isScrubbing = false;
let scrubStartProgress = 0;

// Smooth transition variables
let targetIndex = 0; // Where we're transitioning to
let displayIndex = 0; // Current visual position (fractional for smooth scrolling)
let isTransitioning = false;
let lastSwipeTime = 0;
const swipeThreshold = 30;
const gestureThreshold = 10; // Pixels before committing to scrub or scroll mode

// Preloading state
const maxPreloadDistance = window.innerWidth > 768 ? 6 : 3; // Desktop: ±6, Mobile: ±3
let preloadedTapeIds = new Set();

function boot({ wipe, send, notice, hud }) {
  wipe(0);
  loadingFeed = true;
  
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
        
        // Load first tape and preload neighbors
        loadTapeAtIndex(0, send);
        preloadTapesAroundIndex(0, send);
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

function paint({ wipe, ink, screen, rec, line }) {
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
        currentIndex = targetIndex;
        const tapeId = tapeIdForIndex(currentIndex);
        send({ type: "tape:set-active", content: { tapeId } });
        
        // Preload more tapes around new position
        preloadTapesAroundIndex(currentIndex, send);
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
    
    send({ type: "tape:scroll-offset", content: { tapePositions } });
  }
  
  // Transparent wipe when tape is playing to show video
  const fadeAmount = Math.abs(slideOffset / screen.height);
  if (rec && rec.presenting && rec.playing) {
    wipe(0, 0, 0, 0); // Transparent
    
    // Fade overlay during transition
    if (isTransitioning && fadeAmount > 0) {
      const fadeAlpha = Math.floor(fadeAmount * 100);
      ink(0, 0, 0, fadeAlpha).box(0, 0, screen.width, screen.height);
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

function act({ event: e, send }) {
  const now = performance.now();
  
  if (e.is("touch") || e.is("draw")) {
    // Start gesture
    if (!gestureMode && !isTransitioning) {
      gestureStartX = e.x;
      gestureStartY = e.y;
    }
    
    // Determine gesture mode based on initial movement (10px threshold)
    if (!gestureMode && !isTransitioning) {
      const deltaX = Math.abs(e.x - gestureStartX);
      const deltaY = Math.abs(e.y - gestureStartY);
      const totalMovement = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
      
      if (totalMovement > gestureThreshold) {
        if (deltaX > deltaY) {
          // Horizontal movement -> scrub mode
          gestureMode = "scrub";
          isScrubbing = true;
          
          // Get current tape progress
          const tapeId = tapeIdForIndex(currentIndex);
          send({ type: "tape:get-progress", content: { tapeId } });
          
          console.log("🎵 Started scrubbing");
        } else {
          // Vertical movement -> scroll mode
          gestureMode = "scroll";
          displayIndex = currentIndex; // Reset for live preview
          console.log("📺 Started scrolling");
        }
      }
    }
    
    // Handle active gesture
    if (gestureMode === "scrub") {
      // Horizontal scrubbing (record scratch style)
      const scrubDelta = e.x - gestureStartX;
      const scrubSensitivity = 0.002; // Adjust for faster/slower scrubbing
      const progress = Math.max(0, Math.min(1, scrubStartProgress + scrubDelta * scrubSensitivity));
      
      const tapeId = tapeIdForIndex(currentIndex);
      send({
        type: "tape:scrub",
        content: { tapeId, progress }
      });
    } else if (gestureMode === "scroll") {
      // Vertical scrolling with live preview
      const scrollDelta = (e.y - gestureStartY) / screen.height;
      displayIndex = currentIndex - scrollDelta;
      
      // Clamp displayIndex to valid range
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
        // Not enough distance - snap back
        targetIndex = currentIndex;
        displayIndex = currentIndex;
      }
      
      console.log("📺 Stopped scrolling");
    }
    else if (!gestureMode) {
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
  }
}

function loadTapeAtIndex(index, send) {
  if (index < 0 || index >= tapes.length) return;
  
  const tape = tapes[index];
  if (!tape || !tape.code) return;
  
  const tapeCode = tape.code;
  const tapeId = tapeIdForIndex(index);
  
  console.log("📼 Loading tape:", tapeCode);
  
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
      
      send({
        type: "tape:play",
        content: {
          code: tapeCode,
          zipUrl: zipUrl,
          metadata: metadata
        }
      });
      
      // Mark as active once loaded
      send({ type: "tape:set-active", content: { tapeId } });
    })
    .catch(err => {
      console.error("❌ Failed to load tape:", err);
    });
}

function preloadTapesAroundIndex(centerIndex, send) {
  // Preload tapes ±maxPreloadDistance from center
  const toPreload = [];
  
  for (let distance = 1; distance <= maxPreloadDistance; distance++) {
    const prevIndex = centerIndex - distance;
    const nextIndex = centerIndex + distance;
    
    if (prevIndex >= 0) toPreload.push(prevIndex);
    if (nextIndex < tapes.length) toPreload.push(nextIndex);
  }
  
  // Preload in order of distance (closest first)
  for (const index of toPreload) {
    const tape = tapes[index];
    const tapeId = tapeIdForIndex(index);
    
    if (!tape || !tape.code) continue;
    if (preloadedTapeIds.has(tapeId)) continue; // Already preloaded
    
    const tapeCode = tape.code;
    
    fetch(`/api/get-tape?code=${tapeCode}`)
      .then(res => {
        if (!res.ok) throw new Error(`Failed to load tape: ${res.status}`);
        return res.json();
      })
      .then(metadata => {
        if (metadata.nuked) return;
        
        const zipUrl = `${location.origin}/media/tapes/${tapeCode}`;
        
        send({
          type: "tape:preload",
          content: {
            tapeId: tapeId,
            code: tapeCode,
            zipUrl: zipUrl,
            metadata: metadata
          }
        });
        
        preloadedTapeIds.add(tapeId);
        console.log(`📼 Preloading tape ${index}: ${tapeCode}`);
      })
      .catch(err => {
        console.error(`❌ Failed to preload tape ${index}:`, err);
      });
  }
}

function tapeIdForIndex(index) {
  if (index < 0 || index >= tapes.length) return null;
  return `tv-tape-${index}`;
}

function receive({ content, type, send }) {
  console.log("📺 TV receive:", type, content);
  
  if (type === "tape:progress-reply") {
    // Store progress for scrubbing
    if (content.tapeId === tapeIdForIndex(currentIndex)) {
      scrubStartProgress = content.progress || 0;
    }
  }
  
  if (type === "tape:preloaded") {
    console.log(`📼 Preloaded ${content.tapeId}: ${content.frameCount} frames`);
  }
}

export { boot, paint, act, receive };
