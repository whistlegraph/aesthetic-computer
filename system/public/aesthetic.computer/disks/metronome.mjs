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

function boot({ sound }) {
  sound.skip(); // 💀 Send a signal to skip to the next beat.
}

let odd = false;

// 💗 Beat
function beat({ api, sound, params, store, hud }) {
  // Set the system metronome using `store`.
  let newBpm;
  if (params) {
    if (params[0] === "fast") newBpm = 300;
    else if (params[0] === "medium") newBpm = 120;
    else if (params[0] === "slow") newBpm = 80;
    else newBpm = parseInt(params[0]);
  }

  // Use calculated BPM from tapping if available
  if (bpmUpdatePending && calculatedBpm) {
    newBpm = calculatedBpm;
    bpmUpdatePending = false;
  }

  const currentBpm = sound.bpm(newBpm || store["metronome:bpm"] || 180);
  store["metronome:bpm"] = currentBpm;

  // Update HUD with current BPM and tap info
  if (hud) {
    const tapCount = totalTapCount > 0 ? ` (${totalTapCount} taps)` : '';
    hud.label(`BPM: ${Math.round(currentBpm)}${tapCount}`);
  }

  // console.log("🎼 BPM:", sound.bpm(), "Time:", sound.time.toFixed(2));
  odd = !odd;

  const tick = sound.synth({
    type: "sawtooth",
    tone: melody[melodyIndex],
    duration: 0.0025,
    volume: 0.35 * 1.5,
    pan: odd ? -0.75 : 0.75, // Should I pan left or right on every other beat?
  });

  const lotick = sound.synth({
    type: "square",
    tone: melody[melodyIndex] / 4,
    duration: 0.005,
    volume: 0.15 * 2,
    decay: 0.999,
    pan: odd ? -0.8 : 0.8, // Should I pan left or right on every other beat?
  });

  square = sound.synth({
    type: "square",
    tone: melody[melodyIndex],
    beats: 1,
    volume: 0.0001,
    pan: 0, // Should I pan left or right on every other beat?
  });

  flash = true;
  flashColor.fill(255);
  firstBeat = false;

  melodyIndex = (melodyIndex + 1) % melody.length;
}

let squareP = 0;

function sim({ sound: { time, bpm } }) {
  if (square) {
    const p = square.progress(time);
    squareP = p;
    flashColor[0] = 0;
    flashColor[1] = 0;
    flashColor[2] = Math.floor(((1 - p) / 4) * 255);
    if (p === 1) flash = false; // TODO: This might be skipping 1 frame.
  }
  
  // Handle tap flash decay
  if (tapFlash) {
    tapFlash = false; // Reset tap flash after one frame
  }
}

function paint({ wipe, ink, line, screen, num: { lerp } }) {
  // Combine beat flash and tap flash
  const shouldFlash = flash || tapFlash;
  const currentFlashColor = tapFlash ? [255, 200, 100] : flashColor; // Tap flash is orange-ish
  
  wipe(shouldFlash ? currentFlashColor : 0);

  if (!square) return;

  const baseAngle = -90;
  const left = baseAngle - 20;
  const right = baseAngle + 20;

  let angle =
    melodyIndex === 0 ? lerp(left, right, squareP) : lerp(right, left, squareP);

  if (firstBeat) angle = left;

  ink(255).lineAngle(
    screen.width / 2,
    screen.height - screen.height / 4,
    screen.height / 2,
    angle,
  );
}

function calculateBpmFromTaps() {
  if (tapTimes.length < 2) return null;
  
  const currentTime = tapTimes[tapTimes.length - 1];
  
  // Get last interval for immediate response
  const lastInterval = tapTimes[tapTimes.length - 1] - tapTimes[tapTimes.length - 2];
  const instantBpm = Math.round(60000 / lastInterval);
  
  // Build up exponential moving average more gradually
  recentIntervals.push(lastInterval);
  if (recentIntervals.length > 20) { // Keep more history for smoother averaging
    recentIntervals.shift();
  }
  
  // Calculate exponential moving average with much more smoothing
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
  
  // Very gradual blending strategy for maximum smoothness
  let finalBpm;
  
  if (totalTapCount <= 2) {
    finalBpm = instantBpm;
  } else if (totalTapCount <= 5) {
    // Gradually introduce smoothing
    const instantWeight = Math.max(0.3, 1.0 - (totalTapCount - 2) * 0.2);
    finalBpm = Math.round(instantBpm * instantWeight + emaBpm * (1 - instantWeight));
  } else {
    // Heavily favor the smooth EMA
    finalBpm = emaBpm;
    
    // Very gentle overall validation - only for major drift
    if (overallBpm && Math.abs(finalBpm - overallBpm) > overallBpm * 0.15) {
      finalBpm = Math.round(finalBpm * 0.9 + overallBpm * 0.1);
    }
  }
  
  // Super smooth BPM history filtering
  if (finalBpm) {
    bpmHistory.push(finalBpm);
    if (bpmHistory.length > 15) { // Keep more history
      bpmHistory.shift();
    }
    
    // Very conservative change limiting for ultra-smooth results
    if (bpmHistory.length > 5) {
      const recentAvg = bpmHistory.slice(-5).reduce((sum, bpm) => sum + bpm, 0) / 5;
      const change = Math.abs(finalBpm - recentAvg);
      
      // Limit changes to be very gradual
      if (change > recentAvg * 0.03) { // Only 3% change allowed
        const maxChange = Math.ceil(recentAvg * 0.03);
        if (finalBpm > recentAvg) {
          finalBpm = Math.min(finalBpm, recentAvg + maxChange);
        } else {
          finalBpm = Math.max(finalBpm, recentAvg - maxChange);
        }
      }
    }
  }
  
  // Clamp to reasonable range
  return finalBpm ? Math.max(60, Math.min(300, finalBpm)) : null;
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
    
    // Clear session if there's been a long gap (> 5 seconds)
    const gapThreshold = 5000;
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