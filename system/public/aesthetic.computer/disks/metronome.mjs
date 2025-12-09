/* #region 🟢 TODO 
  - [🟠] Show the bpm rate visually and use arrow keys for dynamic adjustment.
  - [] Add a really nice drum sound.
  - [] Decouple the sound length from the animation... maybe by playing a silent
       sound that can be used for timing? Should there be a "silence" wave type?
  - [] Add other wavetypes to square and rename it to tone.
  - [] Add a sampler so that arbitrary sounds can be loaded and played back with sound.play("name", pitch).for(3, sound.beats)
  - [] Remove potential delay from sound starting up?
  - [] Make sound start-up optional?
  - [] Clean up audio code & api.
  - [] Make an "index" disk that gets booted before any remote disks. (Can just be a timed intro for now.)
  - [] Global ESC menu.
  - [] Video underlay
  - [] Color readable spots in Camera that forces you to move.
#endregion */

let flash = false;
const flashColor = [255, 255, 255];

const melody = [2000, 3000]; //, "c4", "c4", "d4", "e4", "c4", "d4", "e4", "f4", "g4"];
let melodyIndex = 0;
let square;
let firstBeat = true;

// Tap BPM tracking
let tapTimes = [];
let calculatedBpm = null;
let bpmUpdatePending = false;
let runningBpmAverage = null;
let totalTapCount = 0;
let firstTapTime = null;
let recentIntervals = [];
let bpmHistory = [];
const SMOOTHING_FACTOR = 0.08; // Much lower = much more smoothing
let tapFlash = false;
let currentBpmDisplay = 180; // Default BPM to show before first beat

// DAW sync state (tracked at module level for paint access)
let dawSynced = false;
let dawBpm = null;
let dawPlaying = false;
let dawMode = false; // Set from query param OR auto-detected from DAW messages
let dawAutoDetected = false; // True if we auto-detected DAW mode from messages
let persistentDawTime = 0; // Track DAW time for beat alternation

// DAW time interpolation state (smooth animation between infrequent updates)
let lastDawTimeUpdate = 0; // When we last received a time update (performance.now())
let lastDawTimeValue = 0; // The time value we received
let interpolatedDawTime = 0; // Locally interpolated time (beats)
let audioSampleRate = null; // AudioContext sample rate in Hz

function boot({ sound, hud, query }) {
  // Check if we're in DAW mode (loaded from Ableton M4L)
  console.log("🎹 Metronome boot() - query:", JSON.stringify(query));
  dawMode = query?.daw === "1" || query?.daw === 1 || query?.daw === true;
  console.log("🎹 Metronome: dawMode =", dawMode, "query.daw =", query?.daw, typeof query?.daw);
  
  // Also check if we already have DAW data (survives hot reload)
  if (!dawMode && sound.daw?.bpm) {
    dawMode = true;
    dawAutoDetected = true;
    console.log("🎹 Metronome: DAW mode AUTO-DETECTED from existing sound.daw:", sound.daw);
  }
  
  if (dawMode) {
    console.log("🎹 Metronome: DAW mode enabled", dawAutoDetected ? "(auto-detected)" : "(from query)");
  }
  // Note: Don't call sound.bpm() or sound.skip() here - audio worklet isn't ready yet.
  // BPM is set in the beat() function on first call.
}

let odd = false;
let beatCount = 0; // Debug counter

// 💗 Beat
function beat({ api, sound, params, store, hud }) {
  beatCount++;
  
  // Set BPM on first beat (audio worklet is now ready)
  if (beatCount === 1) {
    console.log("🥁 First beat! Setting BPM to 180...");
    sound.bpm(180);
  }
  
  // Set the system metronome using `store`.
  let newBpm;
  
  // 🎹 Track DAW sync state for paint function
  if (sound.daw?.bpm) {
    dawSynced = true;
    dawBpm = sound.daw.bpm;
    dawPlaying = sound.daw?.playing ?? false;
    newBpm = sound.daw.bpm;
  } else {
    dawSynced = false;
    dawBpm = null;
    if (params) {
      if (params[0] === "fast") newBpm = 300;
      else if (params[0] === "medium") newBpm = 120;
      else if (params[0] === "slow") newBpm = 80;
      else newBpm = parseInt(params[0]);
    }
  }

  // Use calculated BPM from tapping if available (and no DAW sync)
  if (!dawSynced && bpmUpdatePending && calculatedBpm) {
    newBpm = calculatedBpm;
    bpmUpdatePending = false;
  }

  const currentBpm = sound.bpm(newBpm || store["metronome:bpm"] || 180);
  store["metronome:bpm"] = currentBpm;
  
  // Update display BPM for non-DAW mode
  if (!dawSynced) {
    currentBpmDisplay = currentBpm;
  }

  // console.log("🎼 BPM:", sound.bpm(), "Time:", sound.time.toFixed(2));
  odd = !odd;

  // 🎹 In DAW mode with sync, skip internal tick sounds (DAW triggers them via sim())
  // But still create the square synth for animation tracking in non-DAW mode
  if (!dawSynced) {
    const tick = sound.synth({
      type: "sawtooth",
      tone: melody[melodyIndex],
      duration: 0.0025,
      volume: 0.35 * 1.5,
      pan: odd ? -0.75 : 0.75,
    });

    const lotick = sound.synth({
      type: "square",
      tone: melody[melodyIndex] / 4,
      duration: 0.005,
      volume: 0.15 * 2,
      decay: 0.999,
      pan: odd ? -0.8 : 0.8,
    });

    square = sound.synth({
      type: "square",
      tone: melody[melodyIndex],
      beats: 1,
      volume: 0.0001,
      pan: 0,
    });
  }

  /*
  // Schedule half-step sound to play at 0.5 beats
  setTimeout(() => {
    if (square && square.progress && square.progress(sound.time) < 1) {
      const halfTick = sound.synth({
        type: "sawtooth",
        tone: melody[melodyIndex] / 2,
        duration: 0.002,
        volume: 0.2,
        pan: odd ? -0.5 : 0.5,
      });

      const halfLotick = sound.synth({
        type: "square",
        tone: melody[melodyIndex] / 8,
        duration: 0.004,
        volume: 0.08,
        decay: 0.998,
        pan: odd ? -0.6 : 0.6,
      });
    }
  }, (60000 / currentBpm) / 2); // Half the beat interval
  */
  flash = true;
  flashColor.fill(255);
  firstBeat = false;

  melodyIndex = (melodyIndex + 1) % melody.length;
}

let squareP = 0;
let debugLogCount = 0;
let dawPhaseProgress = 0; // 0-1 progress within current beat from DAW
let lastDawBeatNumber = -1; // Track beat crossings for DAW-triggered ticks
let dawBeatOdd = false; // Alternate for pan

function sim({ sound }) {
  // 🎹 Capture AudioContext sample rate
  if (sound.sampleRate && !audioSampleRate) {
    audioSampleRate = sound.sampleRate;
    console.log("🎹 AudioContext sample rate:", audioSampleRate, "Hz");
  }
  
  // 🎹 Update DAW state from sound.daw (updated continuously by bios.mjs)
  // Check for DAW data even if dawMode wasn't set - allows auto-detection
  if (sound.daw) {
    // Auto-enable DAW mode if we receive DAW data
    if (!dawMode && sound.daw.bpm !== undefined && sound.daw.bpm !== null) {
      dawMode = true;
      dawAutoDetected = true;
      console.log("🎹 sim() AUTO-DETECTED DAW mode from sound.daw:", JSON.stringify(sound.daw));
    }
    
    // Debug: Log sound.daw every 60 frames (~1 second)
    if (dawMode && debugLogCount % 60 === 0) {
      console.log("🎹 sim() sound.daw:", JSON.stringify(sound.daw), "interpolated:", interpolatedDawTime.toFixed(3));
    }
    if (sound.daw.bpm !== undefined && sound.daw.bpm !== null) {
      dawSynced = true;
      dawBpm = Math.round(sound.daw.bpm);
    }
    if (sound.daw.playing !== undefined && sound.daw.playing !== null) {
      if (dawPlaying !== sound.daw.playing) {
        console.log("🎹 TRANSPORT STATE CHANGED:", dawPlaying, "->", sound.daw.playing);
      }
      dawPlaying = sound.daw.playing;
    }
    
    // 🎹 TIME INTERPOLATION: Ableton's current_song_time only updates ~2Hz,
    // so we interpolate locally based on BPM for smooth animation
    if (sound.daw.time !== undefined && sound.daw.time !== null) {
      const now = performance.now();
      const incomingTime = sound.daw.time;
      
      // Detect if this is a new time update from Ableton
      if (incomingTime !== lastDawTimeValue) {
        lastDawTimeUpdate = now;
        lastDawTimeValue = incomingTime;
        // Snap interpolated time to actual time when we get an update
        interpolatedDawTime = incomingTime;
      } else if (dawPlaying && dawBpm) {
        // No new data - interpolate time based on BPM
        const elapsedMs = now - lastDawTimeUpdate;
        const beatsPerMs = dawBpm / 60000;
        interpolatedDawTime = lastDawTimeValue + (elapsedMs * beatsPerMs);
      }
      
      // Use interpolated time for everything
      persistentDawTime = interpolatedDawTime;
      dawPhaseProgress = interpolatedDawTime % 1; // 0-1 within current beat
      
      // 🎹 DAW Beat Detection - trigger tick sounds when crossing beat boundaries
      const currentBeatNumber = Math.floor(interpolatedDawTime);
      if (dawPlaying && currentBeatNumber !== lastDawBeatNumber && lastDawBeatNumber >= 0) {
        // Beat boundary crossed! Trigger tick sound
        dawBeatOdd = !dawBeatOdd;
        flash = true;
        flashColor.fill(255);
        
        console.log("🎹 BEAT", currentBeatNumber, "odd:", dawBeatOdd, "time:", interpolatedDawTime.toFixed(3));
        
        // Play tick sounds (same as in beat() but triggered from DAW)
        sound.synth({
          type: "sawtooth",
          tone: melody[dawBeatOdd ? 0 : 1],
          duration: 0.0025,
          volume: 0.35 * 1.5,
          pan: dawBeatOdd ? -0.75 : 0.75,
        });
        
        sound.synth({
          type: "square",
          tone: melody[dawBeatOdd ? 0 : 1] / 4,
          duration: 0.005,
          volume: 0.15 * 2,
          decay: 0.999,
          pan: dawBeatOdd ? -0.8 : 0.8,
        });
        
        // Update square for pendulum animation sync
        square = sound.synth({
          type: "square",
          tone: melody[dawBeatOdd ? 0 : 1],
          beats: 1,
          volume: 0.0001,
          pan: 0,
        });
      }
      lastDawBeatNumber = currentBeatNumber;
    }
  }
  
  // Reset interpolation when stopped
  if (!dawPlaying) {
    lastDawTimeUpdate = 0;
  }

  if (square) {
    const p = square.progress(sound.time);
    squareP = p;
    flashColor[0] = 0;
    flashColor[1] = 0;
    flashColor[2] = Math.floor(((1 - p) / 4) * 255);
    if (p === 1) flash = false;
  }
  
  // Handle tap flash decay
  if (tapFlash) {
    tapFlash = false; // Reset tap flash after one frame
  }
}

function paint({ wipe, ink, line, screen, num: { lerp }, text }) {
  // Combine beat flash and tap flash
  const shouldFlash = flash || tapFlash;
  const currentFlashColor = tapFlash ? [255, 200, 100] : flashColor;
  
  wipe(shouldFlash ? currentFlashColor : 0);

  const baseAngle = -90;
  const left = baseAngle - 20;
  const right = baseAngle + 20;

  let angle;
  // In DAW mode, only animate when actively playing
  const dawIsPlaying = dawMode && dawPlaying === true && dawSynced;
  
  if (dawIsPlaying) {
    // 🎹 DAW mode playing: Use interpolated DAW phase for pendulum
    const beatNumber = Math.floor(interpolatedDawTime);
    angle = beatNumber % 2 === 0 ? lerp(left, right, dawPhaseProgress) : lerp(right, left, dawPhaseProgress);
  } else if (dawMode) {
    // 🎹 DAW mode stopped or not synced: Reset pendulum to left (start position)
    angle = left;
  } else {
    // Standard mode: Use square synth progress for pendulum animation
    angle = melodyIndex === 0 ? lerp(left, right, squareP) : lerp(right, left, squareP);
    // Hold at start position before first beat
    if (firstBeat) angle = left;
  }

  // Always draw the pendulum line (even before audio starts)
  ink(255).lineAngle(
    screen.width / 2,
    screen.height - screen.height / 4,
    screen.height / 2,
    angle,
  );

  // BPM display (bottom center) - show in all modes
  if (dawMode) {
    // BPM display (bottom center) - show DAW BPM or "---" if not synced
    const bpmText = dawSynced ? `${dawBpm}` : "---";
    const bpmColor = dawSynced ? [255, 255, 255] : [100, 100, 100];
    ink(bpmColor).write(bpmText, { x: screen.width / 2, y: screen.height - 30, center: "x" });
    ink(100).write("BPM", { x: screen.width / 2, y: screen.height - 18, center: "x" });
    
    // Play/Stop indicator (top-right)
    const triX = screen.width - 16;
    const triY = 16;
    const triSize = 8;
    
    if (dawPlaying === true) {
      // Green play triangle (pointing right)
      ink([100, 255, 100]).poly([
        [triX - triSize, triY - triSize],
        [triX - triSize, triY + triSize],
        [triX + triSize * 0.5, triY]
      ], true);
    } else {
      // Orange/yellow stop square (when stopped or null/unknown)
      ink([255, 200, 100]).box(triX - triSize, triY - triSize + 2, triSize + 4, triSize + 4, "fill");
    }
    
    // Sample rate display (bottom-left)
    if (audioSampleRate) {
      const khz = (audioSampleRate / 1000).toFixed(1);
      ink(100).write(`${khz}kHz`, { x: 8, y: screen.height - 18 });
    }
  }

  // Non-DAW mode: show BPM and tap prompt
  if (!dawMode) {
    // Show current BPM
    const displayBpm = currentBpmDisplay || 180;
    ink(255).write(`${displayBpm}`, { x: screen.width / 2, y: screen.height - 30, center: "x" });
    ink(100).write("BPM", { x: screen.width / 2, y: screen.height - 18, center: "x" });
    
    // If audio hasn't started yet, show a prompt
    if (!square) {
      ink(100).write("tap to start", { x: screen.width / 2, y: screen.height / 2 - 10, center: "xy" });
    }
  }

  // Half-step indicator - show blue dot when we're past midpoint
  if (square && squareP > 0.5) {
    const halfProgress = (squareP - 0.5) * 2; // 0-1 for second half
    const dotY = screen.height - screen.height / 3;
    const dotSize = 3 + halfProgress * 3;
    
    ink(150, 150, 255).circle(screen.width / 2, dotY, dotSize);
  }
}

function calculateBpmFromTaps() {
  if (tapTimes.length < 2) return null;
  
  const currentTime = tapTimes[tapTimes.length - 1];
  
  // Get last interval for immediate response
  const lastInterval = tapTimes[tapTimes.length - 1] - tapTimes[tapTimes.length - 2];
  const instantBpm = Math.round(60000 / lastInterval);
  
  // Build up exponential moving average
  recentIntervals.push(lastInterval);
  if (recentIntervals.length > 20) {
    recentIntervals.shift();
  }
  
  // Calculate exponential moving average
  let emaInterval = recentIntervals[0];
  for (let i = 1; i < recentIntervals.length; i++) {
    emaInterval = emaInterval * (1 - SMOOTHING_FACTOR) + recentIntervals[i] * SMOOTHING_FACTOR;
  }
  const emaBpm = Math.round(60000 / emaInterval);
  
  // Overall average for long-term stability
  let overallBpm = null;
  if (firstTapTime && totalTapCount > 2) {
    const totalDuration = currentTime - firstTapTime;
    const avgInterval = totalDuration / (totalTapCount - 1);
    overallBpm = Math.round(60000 / avgInterval);
  }
  
  // Much more aggressive strategy - prioritize immediate response
  let finalBpm;
  
  if (totalTapCount <= 2) {
    // First 2 taps: Use instant BPM immediately
    finalBpm = instantBpm;
  } else if (totalTapCount <= 4) {
    // Next 2 taps: Still heavily favor instant, but start blending
    finalBpm = Math.round(instantBpm * 0.8 + emaBpm * 0.2);
  } else if (totalTapCount <= 8) {
    // Building confidence: 60/40 split
    finalBpm = Math.round(instantBpm * 0.6 + emaBpm * 0.4);
  } else {
    // Established pattern: Still responsive but more stable
    finalBpm = Math.round(instantBpm * 0.4 + emaBpm * 0.6);
    
    // Very gentle overall validation - only for major drift
    if (overallBpm && Math.abs(finalBpm - overallBpm) > overallBpm * 0.2) {
      finalBpm = Math.round(finalBpm * 0.85 + overallBpm * 0.15);
    }
  }
  
  // Much more lenient BPM history filtering - allow bigger changes
  if (finalBpm) {
    bpmHistory.push(finalBpm);
    if (bpmHistory.length > 10) { // Less history for faster response
      bpmHistory.shift();
    }
    
    // Only limit very extreme changes
    if (bpmHistory.length > 3) {
      const recentAvg = bpmHistory.slice(-3).reduce((sum, bpm) => sum + bpm, 0) / 3;
      const change = Math.abs(finalBpm - recentAvg);
      
      // Allow up to 10% change (much more than before)
      if (change > recentAvg * 0.1) {
        const maxChange = Math.ceil(recentAvg * 0.1);
        if (finalBpm > recentAvg) {
          finalBpm = Math.min(finalBpm, recentAvg + maxChange);
        } else {
          finalBpm = Math.max(finalBpm, recentAvg - maxChange);
        }
      }
    }
  }
  
  // Clamp to reasonable range (no upper limit for fast tapping)
  return finalBpm ? Math.max(30, finalBpm) : null;
}

function act({ event: e, sound }) {
  if (e.is("touch")) {
    const currentTime = Date.now();
    
    // Play tap sound and flash immediately on tap
    const tapTick = sound.synth({
      type: "sawtooth",
      tone: melody[melodyIndex],
      duration: 0.003,
      volume: 0.25,
      pan: 0,
    });
    
    const tapLow = sound.synth({
      type: "square", 
      tone: melody[melodyIndex] / 4,
      duration: 0.006,
      volume: 0.1,
      decay: 0.995,
      pan: 0,
    });
    
    // Trigger tap flash
    tapFlash = true;
    
    // Initialize first tap time
    if (firstTapTime === null) {
      firstTapTime = currentTime;
    }
    
    // Add current tap time
    tapTimes.push(currentTime);
    totalTapCount++;
    
    // Keep a sliding window of recent taps (last 20 taps for smoother averaging)
    if (tapTimes.length > 20) {
      tapTimes.shift();
    }
    
    // Clear session if there's been a long gap (> 3 seconds) - shorter for faster reset
    const gapThreshold = 3000;
    if (tapTimes.length > 1) {
      const lastGap = currentTime - tapTimes[tapTimes.length - 2];
      if (lastGap > gapThreshold) {
        // Reset for new tapping session
        tapTimes = [currentTime];
        firstTapTime = currentTime;
        totalTapCount = 1;
        recentIntervals = [];
        bpmHistory = [];
        runningBpmAverage = null;
      }
    }
    
    // Calculate BPM if we have enough taps
    if (tapTimes.length >= 2) {
      calculatedBpm = calculateBpmFromTaps();
      bpmUpdatePending = true;
    }
  }
}

export { boot, beat, sim, paint, act };

// 📚 Library
// ...