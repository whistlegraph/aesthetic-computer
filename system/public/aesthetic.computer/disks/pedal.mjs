// Pedal, 2026.2.05
// Audio effect pedal for Ableton Live - receives audio, processes, outputs.

/* 📝 Notes
  - [] This is a filter-style effect where audio passes through
  - [] FFT visualization of input audio from Ableton
  - [] Envelope-triggered synth sounds mixed with dry signal
  - [] Dry/wet control for mixing original and processed audio
  
  Technical Architecture:
  - Max/M4L sends FFT data and envelope via executejavascript
  - Web Audio generates sounds based on analysis
  - jweb~ outputs the web-generated audio
  - Dry signal passes through Max directly (for quality)
*/

// === State ===
let dawMode = false;
let dawSynced = false;
let dawBpm = 120;
let dawPlaying = false;

// FFT analysis data from Max
let fftBins = new Float32Array(64); // 64 bins from pfft~
let fftSmoothed = new Float32Array(64);
const FFT_SMOOTHING = 0.85;

// Envelope data from Max
let envelope = {
  peakL: 0,
  peakR: 0,
  rmsL: 0,
  rmsR: 0,
};
let envelopeSmoothed = {
  peakL: 0,
  peakR: 0,
  rmsL: 0,
  rmsR: 0,
};
const ENV_SMOOTHING = 0.9;
const ENV_ATTACK = 0.3;

// Effect state
let effectMode = "visualizer"; // visualizer, envelope-synth, freeze, gate
const effectModes = ["visualizer", "envelope-synth", "freeze", "gate"];
let effectModeIndex = 0;

// Synth state for envelope-synth mode
let lastTriggerTime = 0;
let triggerThreshold = 0.3;
let triggerCooldown = 100; // ms

// Visual state
let peakHistory = [];
const PEAK_HISTORY_LENGTH = 100;
let flash = false;
let flashIntensity = 0;

// === Boot ===
function boot({ sound, query, hud }) {
  dawMode = query?.daw === "1" || query?.daw === 1;
  
  console.log("🎸 Pedal boot - DAW mode:", dawMode);
  
  if (dawMode) {
    hud.label("pedal");
  }
  
  // Initialize peak history
  for (let i = 0; i < PEAK_HISTORY_LENGTH; i++) {
    peakHistory.push(0);
  }
  
  // Set up window functions for M4L communication
  setupMaxBridge();
}

// === Max for Live Bridge ===
function setupMaxBridge() {
  // In worker context, window doesn't exist - use globalThis
  const global = typeof window !== "undefined" ? window : globalThis;
  
  // FFT data receiver (called from Max via executejavascript)
  global.acPedalFFT = function(...bins) {
    if (bins.length > 0) {
      for (let i = 0; i < Math.min(bins.length, fftBins.length); i++) {
        fftBins[i] = bins[i];
      }
    }
  };
  
  // Envelope data receiver
  global.acPedalEnvelope = function(peakL, peakR, rmsL, rmsR) {
    envelope.peakL = peakL || 0;
    envelope.peakR = peakR || 0;
    envelope.rmsL = rmsL || 0;
    envelope.rmsR = rmsR || 0;
  };
  
  // Peak-only receiver (simpler, lower overhead)
  global.acPedalPeak = function(peak) {
    envelope.peakL = peak;
    envelope.peakR = peak;
  };
  
  // Effect mode control
  global.acPedalMode = function(mode) {
    if (effectModes.includes(mode)) {
      effectMode = mode;
      effectModeIndex = effectModes.indexOf(mode);
    }
  };
  
  // Trigger threshold control
  global.acPedalThreshold = function(threshold) {
    triggerThreshold = Math.max(0, Math.min(1, threshold));
  };
  
  console.log("🎸 Max bridge functions registered on", typeof window !== "undefined" ? "window" : "globalThis");
}

// === Sim ===
function sim({ sound }) {
  // Smooth FFT data
  for (let i = 0; i < fftBins.length; i++) {
    fftSmoothed[i] = fftSmoothed[i] * FFT_SMOOTHING + fftBins[i] * (1 - FFT_SMOOTHING);
  }
  
  // Smooth envelope with attack/release
  const envKeys = ["peakL", "peakR", "rmsL", "rmsR"];
  for (const key of envKeys) {
    const target = envelope[key];
    const current = envelopeSmoothed[key];
    if (target > current) {
      // Attack (fast)
      envelopeSmoothed[key] = current * ENV_ATTACK + target * (1 - ENV_ATTACK);
    } else {
      // Release (slow)
      envelopeSmoothed[key] = current * ENV_SMOOTHING + target * (1 - ENV_SMOOTHING);
    }
  }
  
  // Update peak history for waveform display
  peakHistory.push(envelopeSmoothed.peakL);
  if (peakHistory.length > PEAK_HISTORY_LENGTH) {
    peakHistory.shift();
  }
  
  // Update DAW state
  if (sound.daw?.bpm) {
    dawSynced = true;
    dawBpm = sound.daw.bpm;
    dawPlaying = sound.daw?.playing ?? false;
  }
  
  // Flash decay
  if (flash) {
    flashIntensity *= 0.85;
    if (flashIntensity < 0.01) {
      flash = false;
      flashIntensity = 0;
    }
  }
  
  // === Effect Processing ===
  if (effectMode === "envelope-synth") {
    processEnvelopeSynth(sound);
  }
}

// === Envelope-triggered synth ===
function processEnvelopeSynth(sound) {
  const now = performance.now();
  const peak = Math.max(envelopeSmoothed.peakL, envelopeSmoothed.peakR);
  
  // Trigger on threshold crossing (with cooldown)
  if (peak > triggerThreshold && now - lastTriggerTime > triggerCooldown) {
    lastTriggerTime = now;
    
    // Trigger a synth note based on FFT content
    const dominantBin = findDominantBin();
    const freq = binToFreq(dominantBin);
    
    sound.synth({
      type: "sine",
      tone: freq,
      duration: 0.1 + peak * 0.2,
      volume: peak * 0.3,
      attack: 0.01,
      decay: 0.8,
      pan: (envelope.peakL - envelope.peakR) * 0.5, // Pan based on stereo balance
    });
    
    // Visual feedback
    flash = true;
    flashIntensity = peak;
  }
}

// Find the dominant FFT bin
function findDominantBin() {
  let maxVal = 0;
  let maxBin = 0;
  for (let i = 2; i < fftSmoothed.length - 2; i++) { // Skip DC and very high
    if (fftSmoothed[i] > maxVal) {
      maxVal = fftSmoothed[i];
      maxBin = i;
    }
  }
  return maxBin;
}

// Convert FFT bin to approximate frequency
function binToFreq(bin) {
  const sampleRate = 48000;
  const fftSize = 2048; // Typical pfft~ size
  const nyquist = sampleRate / 2;
  const binWidth = nyquist / (fftSize / 2);
  return Math.max(100, Math.min(2000, bin * binWidth)); // Clamp to musical range
}

// === Paint ===
function paint({ wipe, ink, screen, line }) {
  const { width, height } = screen;
  const cx = width / 2;
  const cy = height / 2;
  
  // Background - darker when no audio
  const bgLevel = Math.floor(10 + envelopeSmoothed.peakL * 20);
  wipe(bgLevel, bgLevel, bgLevel + 5);
  
  // Flash overlay
  if (flash) {
    const flashAlpha = Math.floor(flashIntensity * 100);
    ink(255, 255, 255, flashAlpha).box(0, 0, width, height);
  }
  
  // === FFT Spectrum Visualization ===
  const fftHeight = height * 0.4;
  const fftY = height - fftHeight - 20;
  const barWidth = width / fftSmoothed.length;
  
  for (let i = 0; i < fftSmoothed.length; i++) {
    const val = fftSmoothed[i];
    const barHeight = val * fftHeight;
    const x = i * barWidth;
    
    // Color based on frequency (low=red, mid=green, high=blue)
    const hue = (i / fftSmoothed.length) * 0.7; // 0 to 0.7 (red to blue)
    const r = Math.floor(255 * (1 - hue));
    const g = Math.floor(255 * Math.sin(hue * Math.PI));
    const b = Math.floor(255 * hue);
    
    ink(r, g, b, 200).box(x, fftY + fftHeight - barHeight, barWidth - 1, barHeight);
  }
  
  // === Waveform History ===
  const waveY = height * 0.3;
  const waveHeight = height * 0.2;
  
  ink(100, 200, 100, 150);
  for (let i = 1; i < peakHistory.length; i++) {
    const x1 = ((i - 1) / peakHistory.length) * width;
    const x2 = (i / peakHistory.length) * width;
    const y1 = waveY + (1 - peakHistory[i - 1]) * waveHeight;
    const y2 = waveY + (1 - peakHistory[i]) * waveHeight;
    line(x1, y1, x2, y2);
  }
  
  // === Level Meters ===
  const meterWidth = 20;
  const meterHeight = height * 0.6;
  const meterY = height * 0.2;
  
  // Left meter
  ink(40, 40, 50).box(10, meterY, meterWidth, meterHeight);
  const leftLevel = envelopeSmoothed.peakL * meterHeight;
  ink(50, 200, 100).box(10, meterY + meterHeight - leftLevel, meterWidth, leftLevel);
  
  // Right meter
  ink(40, 40, 50).box(width - 30, meterY, meterWidth, meterHeight);
  const rightLevel = envelopeSmoothed.peakR * meterHeight;
  ink(50, 200, 100).box(width - 30, meterY + meterHeight - rightLevel, meterWidth, rightLevel);
  
  // Threshold line (for envelope-synth mode)
  if (effectMode === "envelope-synth") {
    const threshY = meterY + meterHeight * (1 - triggerThreshold);
    ink(255, 100, 100, 150).line(10, threshY, width - 10, threshY);
  }
  
  // === Status Text ===
  ink(200, 200, 200).write(`MODE: ${effectMode.toUpperCase()}`, { x: 10, y: 15 });
  ink(200, 200, 200).write(`BPM: ${dawBpm}`, { x: 10, y: 30 });
  
  if (dawSynced) {
    ink(100, 255, 100).write(dawPlaying ? "▶ PLAYING" : "⏸ STOPPED", { x: width - 80, y: 15 });
  } else {
    ink(255, 255, 100).write("NO DAW", { x: width - 60, y: 15 });
  }
  
  // Instructions
  ink(120, 120, 130).write("TAP: cycle modes", { x: 10, y: height - 10 });
}

// === Act ===
function act({ event }) {
  if (event.is("touch") || event.is("keyboard:down:space")) {
    // Cycle through effect modes
    effectModeIndex = (effectModeIndex + 1) % effectModes.length;
    effectMode = effectModes[effectModeIndex];
    console.log("🎸 Effect mode:", effectMode);
  }
  
  if (event.is("keyboard:down:up")) {
    triggerThreshold = Math.min(1, triggerThreshold + 0.05);
    console.log("🎸 Threshold:", triggerThreshold.toFixed(2));
  }
  
  if (event.is("keyboard:down:down")) {
    triggerThreshold = Math.max(0, triggerThreshold - 0.05);
    console.log("🎸 Threshold:", triggerThreshold.toFixed(2));
  }
}

// === Meta ===
function meta() {
  return {
    title: "Pedal",
    desc: "Audio effect pedal for Ableton Live",
  };
}

export { boot, sim, paint, act, meta };
