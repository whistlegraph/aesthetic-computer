// scrub.mjs - Shared horizontal scrubbing behavior for video/audio playback
// Used by video.mjs and tv.mjs for scrubbing through media

export function createScrubber() {
  // Scrubbing state
  let isScrubbing = false;
  let scrubStartY = 0;
  let scrubStartProgress = 0;
  let scrubAccumulatedDelta = 0;
  let lastDragX = 0;
  
  // STAMPLE-style lazy needle physics
  let targetProgress = 0; // Where user is dragging (yellow line)
  let needleProgress = 0; // Where playback actually is (cyan line)
  let needleVelocity = 0;
  let inertiaActive = false;
  
  let wasPlayingBeforeScrub = false;
  
  return {
    // State getters
    get isScrubbing() { return isScrubbing; },
    get inertiaActive() { return inertiaActive; },
    get targetProgress() { return targetProgress; },
    get needleProgress() { return needleProgress; },
    get needleVelocity() { return needleVelocity; },
    
    // Start scrubbing on drag
    start(e, currentProgress, isPlaying) {
      if (isScrubbing) return false; // Already scrubbing
      
      isScrubbing = true;
      scrubStartY = e.y;
      lastDragX = e.x;
      scrubAccumulatedDelta = 0;
      scrubStartProgress = currentProgress || 0;
      wasPlayingBeforeScrub = isPlaying;
      inertiaActive = false;
      
      // Initialize physics
      targetProgress = scrubStartProgress;
      needleProgress = scrubStartProgress;
      needleVelocity = 0;
      
      console.log(`🎯 SCRUB START: progress=${scrubStartProgress.toFixed(3)}, wasPlaying=${wasPlayingBeforeScrub}`);
      return true;
    },
    
    // Update scrub during drag
    drag(e, screenWidth) {
      if (!isScrubbing) return null;
      
      const deltaX = e.x - lastDragX;
      lastDragX = e.x;
      
      scrubAccumulatedDelta += deltaX;
      
      // Calculate target progress (where user is dragging)
      const progressDelta = scrubAccumulatedDelta / screenWidth;
      targetProgress = scrubStartProgress + progressDelta;
      targetProgress = Math.max(0, Math.min(1, targetProgress));
      
      return {
        targetProgress,
        needleProgress,
        delta: deltaX
      };
    },
    
    // End scrubbing on lift
    end() {
      if (!isScrubbing) return { wasScrubbing: false };
      
      const lag = Math.abs(targetProgress - needleProgress);
      const hasVelocity = Math.abs(needleVelocity) > 0.001;
      
      // Activate inertia if there's movement
      if (hasVelocity || lag > 0.01) {
        inertiaActive = true;
        console.log(`💨 INERTIA ACTIVATED: velocity=${needleVelocity.toFixed(4)}`);
      } else {
        needleVelocity = 0;
        targetProgress = needleProgress; // Snap together
      }
      
      isScrubbing = false;
      scrubStartY = 0;
      scrubAccumulatedDelta = 0;
      
      return {
        wasScrubbing: true,
        wasPlayingBefore: wasPlayingBeforeScrub,
        finalProgress: needleProgress
      };
    },
    
    // Physics simulation (call every frame)
    simulate() {
      if (!isScrubbing && !inertiaActive) return null;
      
      // Lazy needle physics - follows target with lag
      const stiffness = 0.15; // How strongly needle follows target
      const damping = 0.85; // Velocity decay
      const minVelocity = 0.0001; // Stop when velocity is tiny
      
      // Calculate spring force toward target
      const displacement = targetProgress - needleProgress;
      const springForce = displacement * stiffness;
      
      // Update velocity with spring force and damping
      needleVelocity += springForce;
      needleVelocity *= damping;
      
      // Update needle position
      needleProgress += needleVelocity;
      needleProgress = Math.max(0, Math.min(1, needleProgress));
      
      // Stop inertia when settled
      if (inertiaActive && !isScrubbing) {
        const settled = Math.abs(needleVelocity) < minVelocity && Math.abs(displacement) < 0.001;
        if (settled) {
          inertiaActive = false;
          needleVelocity = 0;
          needleProgress = targetProgress; // Snap to target
          console.log(`💨 INERTIA SETTLED at ${needleProgress.toFixed(3)}`);
        }
      }
      
      return {
        targetProgress,
        needleProgress,
        velocity: needleVelocity,
        displacement
      };
    },
    
    // Reset all state
    reset() {
      isScrubbing = false;
      inertiaActive = false;
      scrubStartY = 0;
      scrubStartProgress = 0;
      scrubAccumulatedDelta = 0;
      lastDragX = 0;
      targetProgress = 0;
      needleProgress = 0;
      needleVelocity = 0;
      wasPlayingBeforeScrub = false;
    },
    
    // Calculate audio pitch shift based on velocity (for tape audio manipulation)
    getPitchShift(velocityScale = 0.3) {
      // Scale velocity to pitch shift amount
      // velocityScale controls how much the pitch changes per unit velocity
      const pitchShift = needleVelocity * velocityScale * 300;
      return Math.abs(pitchShift) > 0.01 ? pitchShift : 0;
    },
    
    // Expose state as properties for easy access
    get isScrubbing() { return isScrubbing; },
    get targetProgress() { return targetProgress; },
    get needleProgress() { return needleProgress; },
    get needleVelocity() { return needleVelocity; },
    get inertiaActive() { return inertiaActive; },
    get wasPlayingBeforeScrub() { return wasPlayingBeforeScrub; }
  };
}
