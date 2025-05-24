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
const SMOOTHING_FACTOR = 0.15; // Lower = more smoothing

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
}

function paint({ wipe, ink, line, screen, num: { lerp } }) {
  // wipe(0);
  wipe(flash ? flashColor : 0);

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
  
  // Method 1: Overall average from first tap to now
  let overallBpm = null;
  if (firstTapTime && totalTapCount > 1) {
    const totalDuration = currentTime - firstTapTime;
    const avgInterval = totalDuration / (totalTapCount - 1);
    overallBpm = Math.round(60000 / avgInterval);
  }
  
  // Method 2: Recent intervals (last 4-8 taps)
  let recentBpm = null;
  if (tapTimes.length >= 2) {
    const intervals = [];
    const numRecentTaps = Math.min(8, tapTimes.length);
    for (let i = tapTimes.length - numRecentTaps; i < tapTimes.length - 1; i++) {
      intervals.push(tapTimes[i + 1] - tapTimes[i]);
    }
    
    // Filter out outliers (intervals that are too different from median)
    intervals.sort((a, b) => a - b);
    const median = intervals[Math.floor(intervals.length / 2)];
    const filteredIntervals = intervals.filter(interval => 
      Math.abs(interval - median) < median * 0.3 // Within 30% of median
    );
    
    if (filteredIntervals.length > 0) {
      const avgInterval = filteredIntervals.reduce((sum, interval) => sum + interval, 0) / filteredIntervals.length;
      recentBpm = Math.round(60000 / avgInterval);
    }
  }
  
  // Method 3: Exponential moving average of intervals
  const lastInterval = tapTimes[tapTimes.length - 1] - tapTimes[tapTimes.length - 2];
  recentIntervals.push(lastInterval);
  
  // Keep only recent intervals for moving average
  if (recentIntervals.length > 16) {
    recentIntervals.shift();
  }
  
  // Calculate exponential moving average
  let emaBpm = null;
  if (recentIntervals.length > 0) {
    let emaInterval = recentIntervals[0];
    for (let i = 1; i < recentIntervals.length; i++) {
      emaInterval = emaInterval * (1 - SMOOTHING_FACTOR) + recentIntervals[i] * SMOOTHING_FACTOR;
    }
    emaBpm = Math.round(60000 / emaInterval);
  }
  
  // Choose the best BPM estimate
  let finalBpm;
  
  if (totalTapCount <= 3) {
    // For first few taps, use recent calculation
    finalBpm = recentBpm || emaBpm;
  } else if (totalTapCount <= 8) {
    // Blend recent and overall for medium tap counts
    const recentWeight = 0.7;
    const overallWeight = 0.3;
    finalBpm = Math.round(
      (recentBpm || emaBpm) * recentWeight + 
      (overallBpm || recentBpm || emaBpm) * overallWeight
    );
  } else {
    // For many taps, prefer the smoothed exponential moving average
    // but validate against overall trend
    finalBpm = emaBpm;
    
    // Validate against overall BPM - if too different, blend them
    if (overallBpm && Math.abs(finalBpm - overallBpm) > overallBpm * 0.1) {
      finalBpm = Math.round(finalBpm * 0.8 + overallBpm * 0.2);
    }
  }
  
  // Keep BPM history for stability checking
  if (finalBpm) {
    bpmHistory.push(finalBpm);
    if (bpmHistory.length > 10) {
      bpmHistory.shift();
    }
    
    // If we have history, smooth out sudden changes
    if (bpmHistory.length > 3) {
      const recentAvg = bpmHistory.slice(-3).reduce((sum, bpm) => sum + bpm, 0) / 3;
      const change = Math.abs(finalBpm - recentAvg);
      
      // If change is dramatic, moderate it
      if (change > recentAvg * 0.05) { // More than 5% change
        finalBpm = Math.round(finalBpm * 0.7 + recentAvg * 0.3);
      }
    }
  }
  
  // Clamp to reasonable range
  return finalBpm ? Math.max(60, Math.min(300, finalBpm)) : null;
}

function act({ event: e, sound }) {
  if (e.is("touch")) {
    const currentTime = Date.now();
    
    // Initialize first tap time
    if (firstTapTime === null) {
      firstTapTime = currentTime;
    }
    
    // Add current tap time
    tapTimes.push(currentTime);
    totalTapCount++;
    
    // Keep a sliding window of recent taps (last 16 taps)
    if (tapTimes.length > 16) {
      tapTimes.shift();
    }
    
    // Clear session if there's been a long gap (> 4 seconds)
    const gapThreshold = 4000;
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