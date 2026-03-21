// Seashells, 2025.6.13.01.41.06.896
// Bytebeat algorithmic synthesis.

/* 📝 Engineering Notes
  Bytebeat synthesis using streaming generators.
  This demonstrates algorithmic music generation where complex patterns
  emerge from simple mathematical expressions operating on integers.
*/

const touchVoices = new Map();
const maxTouchPointers = 8;
const hudSafeTop = 22; // Keep local guide below prompt HUD corner label.
const uiFont = "MatrixChunky8";
const touchOverlayPalette = [
  [255, 120, 110],
  [110, 225, 255],
  [145, 255, 160],
  [255, 220, 120],
  [220, 140, 255],
  [255, 170, 130],
  [170, 255, 255],
  [255, 255, 180]
];

const interactionState = {
  scanOffset: 0,
  scanVelocity: 0.003,
  scanSpread: 1.0,
  orbit: 0,
  memory: 0,
  chaosBias: 0,
  density: 1.0,
  lastTouchAt: 0
};

function createDefaultFeedback() {
  return {
    timeModulation: 0,
    shiftMod1: 0,
    shiftMod2: 0,
    harmonicScale: 1.0,
    rhythmScale: 1.0,
    bitMod1: 0,
    bitMod2: 0,
    complexMod: 0,
    sierpinskiMod: 0,
    mixSpeed: 1.0,
    patternBias: 0,
    blendIntensity: 1.0,
    intensity: 1.0,
    chaosLevel: 0,
    colorMod: { r: 1.0, g: 1.0, b: 1.0 }
  };
}

let sharedPixelFeedback = createDefaultFeedback();

// Bytebeat generator
const generator = {
  // Bytebeat algorithmic synthesis with pixel feedback capability
  bytebeat: ({ frequency, sampleRate, time, samplesNeeded, feedback = null }) => {
    const liveFeedback = feedback || null;
    const samples = [];
    const freqScale = frequency / 440; // Scale relative to A440
    
    // Apply feedback to time offset if available
    let timeOffset = Math.floor(time * sampleRate * freqScale * 0.3);
    if (liveFeedback && liveFeedback.timeModulation) {
      timeOffset += liveFeedback.timeModulation;
    }
    
    for (let i = 0; i < samplesNeeded; i++) {
      const t = timeOffset + Math.floor(i * freqScale * 0.8); // Sample-based time progression
      
      // Classic bytebeat patterns - now modifiable by feedback
      let shiftMod1 = liveFeedback?.shiftMod1 || 0;
      let shiftMod2 = liveFeedback?.shiftMod2 || 0;
      let harmonicScale = liveFeedback?.harmonicScale || 1.0;
      let rhythmScale = liveFeedback?.rhythmScale || 1.0;
      
      // Pattern 1: XOR cascade (crisp, digital texture)
      const pattern1 = (t ^ (t >> (8 + shiftMod1)) ^ (t >> (9 + shiftMod2))) & 255;
      
      // Pattern 2: Melodic stepped pattern with harmonic scaling
      const harmonic = Math.max(1, Math.floor(freqScale * 2 * harmonicScale));
      const pattern2 = ((t * harmonic) & (t >> (5 + (liveFeedback?.bitMod1 || 0))) | (t >> (4 + (liveFeedback?.bitMod2 || 0)))) & 255;
      
      // Pattern 3: Complex rhythmic pattern with frequency modulation
      const rhythmMod = Math.floor((freqScale - 1) * 8 * rhythmScale) + 7;
      const pattern3 = (t | (t >> rhythmMod | t >> 7)) * (t & (t >> 11 | t >> (9 + (liveFeedback?.complexMod || 0)))) & 255;
      
      // Pattern 4: Sierpinski triangle-like pattern
      const pattern4 = (t & (t >> (5 + (liveFeedback?.sierpinskiMod || 0)) | t >> 8)) & 255;
      
      // Pattern 5: Frequency-responsive melodic pattern
      const melodyScale = Math.floor(freqScale * 6) + 1;
      const pattern5 = ((t * melodyScale) ^ (t >> 6)) & (t >> 8) & 255;
      
      // Dynamic pattern mixing based on time, frequency, and feedback
      let mixPhase = (time * 0.08 + freqScale * 0.5) % 5;
      if (liveFeedback && liveFeedback.mixSpeed) {
        mixPhase = (time * 0.08 * liveFeedback.mixSpeed + freqScale * 0.5) % 5;
      }
      if (liveFeedback && liveFeedback.patternBias) {
        mixPhase = (mixPhase + liveFeedback.patternBias) % 5;
      }
      
      let finalPattern;
      let blendIntensity = liveFeedback?.blendIntensity || 1.0;
      
      if (mixPhase < 1) {
        const blend = mixPhase * blendIntensity;
        finalPattern = pattern1 * (1 - blend) + pattern2 * blend;
      } else if (mixPhase < 2) {
        const blend = (mixPhase - 1) * blendIntensity;
        finalPattern = pattern2 * (1 - blend) + pattern3 * blend;
      } else if (mixPhase < 3) {
        const blend = (mixPhase - 2) * blendIntensity;
        finalPattern = pattern3 * (1 - blend) + pattern4 * blend;
      } else if (mixPhase < 4) {
        const blend = (mixPhase - 3) * blendIntensity;
        finalPattern = pattern4 * (1 - blend) + pattern5 * blend;
      } else {
        const blend = (mixPhase - 4) * blendIntensity;
        finalPattern = pattern5 * (1 - blend) + pattern1 * blend;
      }
      
      // Apply feedback intensity modulation
      if (liveFeedback && liveFeedback.intensity) {
        finalPattern *= liveFeedback.intensity;
      }
      
      // Apply chaos injection from feedback
      if (liveFeedback && liveFeedback.chaosLevel > 0.5) {
        finalPattern = finalPattern ^ Math.floor(liveFeedback.chaosLevel * 128);
      }
      
      // Convert from 0-255 byte range to -1 to 1 audio range
      let sample = (finalPattern / 127.5) - 1;
      
      // Keep bytebeat texture but avoid over-attenuating overall loudness.
      const filterAmount = clamp(0.55 + (freqScale - 1) * 0.14, 0.55, 1.0);
      sample *= filterAmount;
      
      // Add subtle bit-crushing effect for authentic bytebeat character
      const crushFactor = 64;
      sample = Math.floor(sample * crushFactor) / crushFactor;
      
      samples.push(sample);
    }
    return samples;
  }
};

// Local waveform buffer for direct bytebeat visualization
let localWaveform = [];
const waveformBufferSize = 512; // Size of local waveform buffer

let currentFrequency = 440; // Pixel-derived frequency used by active voices

function clamp(value, low, high) {
  return Math.max(low, Math.min(high, value));
}

function derivePixelFrequency(feedback) {
  if (!feedback) return 440;

  const harmonicNorm = clamp((feedback.harmonicScale - 0.5) / 1.7, 0, 1);
  const rhythmNorm = clamp((feedback.rhythmScale - 0.3) / 1.7, 0, 1);
  const brightnessNorm = clamp((feedback.intensity - 0.5) / 0.9, 0, 1);
  const chaosNorm = clamp(feedback.chaosLevel, 0, 1);
  const colorAvg = (feedback.colorMod.r + feedback.colorMod.g + feedback.colorMod.b) / 3;
  const colorNorm = clamp(colorAvg - 0.5, 0, 1);
  const biasNorm = ((feedback.patternBias % 4) + 4) % 4 / 4;

  const normalized = clamp(
    harmonicNorm * 0.36 +
      rhythmNorm * 0.2 +
      brightnessNorm * 0.2 +
      colorNorm * 0.12 +
      biasNorm * 0.12 -
      chaosNorm * 0.08,
    0,
    1
  );

  return 120 + normalized * 980;
}

function mapXToFrequency(x, width) {
  const w = Math.max(1, width - 1);
  const nx = clamp((x ?? w / 2) / w, 0, 1);
  const minHz = 80;
  const maxHz = 1600;
  return minHz * Math.pow(maxHz / minHz, nx);
}

function mapYToPitchFactor(y, height) {
  const h = Math.max(1, height - 1);
  const ny = clamp((y ?? h / 2) / h, 0, 1);
  // Two-octave span from bottom to top: 0.5x -> 2x.
  return Math.pow(2, (0.5 - ny) * 2);
}

function deriveVoiceFrequency({ x, y, screenWidth, screenHeight }) {
  const base = mapXToFrequency(x, screenWidth) * mapYToPitchFactor(y, screenHeight);
  return clamp(base, 55, 2600);
}

function drawTouchMapping({ ink, line, write, screen, emphasized = false }) {
  const padTop = Math.min(hudSafeTop, Math.max(0, screen.height - 4));
  const padBottom = screen.height - 1;
  const width = Math.max(1, screen.width - 1);
  const height = Math.max(1, padBottom - padTop);

  const gridColor = emphasized ? [56, 88, 128] : [34, 56, 84];
  const axisColor = emphasized ? [94, 138, 182] : [54, 84, 124];
  const textColor = emphasized ? [232, 242, 255] : [180, 206, 232];

  ink(gridColor[0], gridColor[1], gridColor[2]);
  for (let i = 1; i < 8; i++) {
    const x = Math.floor((i / 8) * width);
    const y = padTop + Math.floor((i / 8) * height);
    line(x, padTop, x, padBottom);
    line(0, y, width, y);
  }

  ink(axisColor[0], axisColor[1], axisColor[2]);
  line(0, padTop, width, padTop);
  line(Math.floor(width * 0.5), padTop, Math.floor(width * 0.5), padBottom);

  const lowHz = Math.round(mapXToFrequency(0, screen.width));
  const midHz = Math.round(mapXToFrequency(width * 0.5, screen.width));
  const highHz = Math.round(mapXToFrequency(width, screen.width));
  const topMult = mapYToPitchFactor(padTop, screen.height);
  const midMult = mapYToPitchFactor(padTop + height * 0.5, screen.height);
  const lowMult = mapYToPitchFactor(padBottom, screen.height);

  ink(textColor[0], textColor[1], textColor[2]);
  write(`${lowHz}hz`, { x: 2, y: padTop + 2 }, undefined, undefined, false, uiFont);
  write(`${midHz}hz`, { x: Math.max(2, Math.floor(width * 0.5) - 12), y: padTop + 2 }, undefined, undefined, false, uiFont);
  write(`${highHz}hz`, { x: Math.max(2, width - 28), y: padTop + 2 }, undefined, undefined, false, uiFont);
  write(`x${topMult.toFixed(2)}`, { x: 2, y: Math.max(padTop + 2, padTop + 12) }, undefined, undefined, false, uiFont);
  write(`x${midMult.toFixed(2)}`, { x: 2, y: padTop + Math.floor(height * 0.5) - 4 }, undefined, undefined, false, uiFont);
  write(`x${lowMult.toFixed(2)}`, { x: 2, y: Math.max(padTop + 2, padBottom - 8) }, undefined, undefined, false, uiFont);
}

function drawTouchOverlays({ ink, line, circle, write, screen }) {
  for (const [key, voice] of touchVoices.entries()) {
    const pointerIndex = Number(key.split("-")[1]) || 1;
    const [r, g, b] = touchOverlayPalette[(pointerIndex - 1) % touchOverlayPalette.length];
    const x = clamp(Math.round(voice.x ?? screen.width * 0.5), 0, screen.width - 1);
    const y = clamp(Math.round(voice.y ?? screen.height * 0.5), 0, screen.height - 1);

    ink(r, g, b, 64);
    circle(x, y, 14);
    ink(r, g, b);
    circle(x, y, 9);
    line(x - 16, y, x + 16, y);
    line(x, y - 16, x, y + 16);

    ink(245, 245, 255);
    const labelX = clamp(x + 10, 1, Math.max(1, screen.width - 70));
    const labelY = clamp(y - 11, hudSafeTop, Math.max(hudSafeTop, screen.height - 8));
    write(`${pointerIndex}:${Math.round(voice.frequency)}hz`, { x: labelX, y: labelY }, undefined, undefined, false, uiFont);
  }
}

function rebalanceVoiceVolumes() {
  const count = touchVoices.size;
  if (count <= 0) return;

  const baseVolume = clamp(0.58 / Math.sqrt(count), 0.16, 0.5);

  for (const voice of touchVoices.values()) {
    voice.sound?.update?.({ volume: baseVolume });
  }
}

function applyTouchInfluence({ x, y, screenWidth, screenHeight }) {
  const nx = clamp((x ?? screenWidth * 0.5) / Math.max(1, screenWidth), 0, 1);
  const ny = clamp((y ?? screenHeight * 0.5) / Math.max(1, screenHeight), 0, 1);

  interactionState.scanOffset = (interactionState.scanOffset + nx * 0.11 + ny * 0.07) % 1;
  interactionState.orbit += (nx - 0.5) * 0.18;
  interactionState.scanSpread = clamp(
    interactionState.scanSpread * 0.9 + (0.65 + ny) * 0.1,
    0.5,
    2.0
  );
  interactionState.memory = clamp(
    interactionState.memory * 0.94 + 0.05 + Math.abs(nx - 0.5) * 0.08,
    0,
    1
  );
  interactionState.chaosBias = clamp(
    interactionState.chaosBias * 0.9 + Math.abs(nx - 0.5) * 0.25,
    0,
    0.75
  );
  interactionState.density = clamp(1 + interactionState.memory * 0.8 + touchVoices.size * 0.05, 0.75, 1.9);
  interactionState.lastTouchAt = performance.now();
}

function createVoice({ sound, frequency, volume = 0.5 }) {
  return sound.synth({
    type: "custom",
    tone: frequency,
    duration: "🔁",
    volume,
    generator: generator.bytebeat
  });
}

function startTouchVoice({ pointerIndex, x, y, screenWidth, screenHeight, sound }) {
  applyTouchInfluence({ x, y, screenWidth, screenHeight });

  const key = `touch-${pointerIndex}`;
  if (touchVoices.has(key)) return;

  const initialFrequency = deriveVoiceFrequency({
    x,
    y,
    screenWidth,
    screenHeight
  });
  const soundVoice = createVoice({ sound, frequency: initialFrequency, volume: 0.5 });
  touchVoices.set(key, {
    sound: soundVoice,
    x,
    y,
    frequency: initialFrequency
  });

  rebalanceVoiceVolumes();
}

function updateTouchVoice({ pointerIndex, x, y, screenWidth, screenHeight, sound }) {
  applyTouchInfluence({ x, y, screenWidth, screenHeight });
  const key = `touch-${pointerIndex}`;
  const existingVoice = touchVoices.get(key);
  if (!existingVoice) {
    startTouchVoice({ pointerIndex, x, y, screenWidth, screenHeight, sound });
    return;
  }
  existingVoice.x = x;
  existingVoice.y = y;
}

function stopTouchVoice(pointerIndex, fade = 0.1) {
  const key = `touch-${pointerIndex}`;
  const voice = touchVoices.get(key);
  if (!voice) return;
  voice.sound?.kill(fade);
  touchVoices.delete(key);
  rebalanceVoiceVolumes();
}

function stopAllVoices(fade = 0.1) {
  for (const voice of touchVoices.values()) {
    voice.sound?.kill(fade);
  }
  touchVoices.clear();
}

function totalVoiceCount() {
  return touchVoices.size;
}

// FEEDBACK SYSTEM: Sample pixels to influence bytebeat generation
function samplePixelFeedback(screen) {
  const samples = [];
  const samplePoints = 12 + Math.floor(8 * interactionState.density);
  const now = performance.now() * 0.001;
  const scanOffset = interactionState.scanOffset;
  const spread = interactionState.scanSpread;
  
  // Sample pixels from different regions of the screen
  for (let i = 0; i < samplePoints; i++) {
    // Sample from different regions: corners, edges, center, diagonal sweeps
    let x, y;
    
    if (i < 4) {
      // Corner sampling
      x = (i % 2) * (screen.width - 1);
      y = Math.floor(i / 2) * (screen.height - 1);
    } else if (i < 8) {
      // Edge sampling  
      const edge = i - 4;
      if (edge < 2) {
        x = (edge * (screen.width - 1));
        y = Math.floor(screen.height / 2);
      } else {
        x = Math.floor(screen.width / 2);
        y = ((edge - 2) * (screen.height - 1));
      }
    } else if (i < 12) {
      // Diagonal sampling
      const diag = i - 8;
      const progress = ((diag / 4) + scanOffset) % 1;
      x = Math.floor(progress * screen.width);
      y = Math.floor(((progress * spread) % 1) * screen.height);
    } else {
      // Orbital scanning pattern modified by interaction memory.
      const phase = now + interactionState.orbit + scanOffset * Math.PI * 2 + (i - 12) * 0.17;
      const wobble = 0.2 + interactionState.memory * 0.7;
      const radiusX = 0.5 + Math.sin(phase * 0.61) * wobble;
      const radiusY = 0.5 + Math.cos(phase * 0.79) * wobble * spread;
      x = Math.floor(radiusX * screen.width);
      y = Math.floor(radiusY * screen.height);
    }
    
    // Clamp to screen bounds
    x = Math.max(0, Math.min(screen.width - 1, x));
    y = Math.max(0, Math.min(screen.height - 1, y));
    
    const pixelIndex = (y * screen.width + x) * 4;
    const r = screen.pixels[pixelIndex] || 0;
    const g = screen.pixels[pixelIndex + 1] || 0;
    const b = screen.pixels[pixelIndex + 2] || 0;
    const a = screen.pixels[pixelIndex + 3] || 0;
    
    samples.push({ r, g, b, a, x, y });
  }
  
  // Process samples into feedback parameters
  const avgR = samples.reduce((sum, s) => sum + s.r, 0) / samples.length;
  const avgG = samples.reduce((sum, s) => sum + s.g, 0) / samples.length;
  const avgB = samples.reduce((sum, s) => sum + s.b, 0) / samples.length;
  const maxBrightness = Math.max(...samples.map(s => s.r + s.g + s.b));
  const minBrightness = Math.min(...samples.map(s => s.r + s.g + s.b));
  const contrast = maxBrightness - minBrightness;
  
  // Calculate variance for chaos level
  const brightnesses = samples.map(s => s.r + s.g + s.b);
  const avgBrightness = brightnesses.reduce((sum, b) => sum + b, 0) / brightnesses.length;
  const variance = brightnesses.reduce((sum, b) => sum + Math.pow(b - avgBrightness, 2), 0) / brightnesses.length;
  
  // Convert pixel data into bytebeat modulation parameters
  const feedback = {
    // Time modulation based on red channel
    timeModulation: Math.floor((avgR / 255) * 10000 - 5000),
    
    // Bit shift modulations based on color channels
    shiftMod1: Math.floor((avgG / 255) * 4) - 2, // -2 to +2
    shiftMod2: Math.floor((avgB / 255) * 4) - 2,
    
    // Pattern parameter modulations
    harmonicScale: 0.5 + (avgR / 255) * 1.5, // 0.5 to 2.0
    rhythmScale: 0.3 + (avgG / 255) * 1.4,   // 0.3 to 1.7
    
    // Bit operation modulations
    bitMod1: Math.floor((contrast / 765) * 3), // 0 to 3 based on contrast
    bitMod2: Math.floor((variance / 10000) * 3),
    complexMod: Math.floor((avgB / 255) * 4),
    sierpinskiMod: Math.floor(((avgR + avgG) / 510) * 3),
    
    // Pattern mixing influences
    mixSpeed: 0.5 + (avgG / 255) * 2.0,      // 0.5 to 2.5
    patternBias: (avgB / 255) * 4,           // 0 to 4
    blendIntensity: 0.3 + (contrast / 765) * 0.7, // 0.3 to 1.0
    
    // Overall modulation intensity
    intensity: 0.7 + (maxBrightness / 765) * 0.6, // 0.7 to 1.3
    
    // Chaos level based on pixel variance
    chaosLevel: Math.min(1.0, variance / 20000), // 0 to 1.0
    
    // Color modulation multipliers
    colorMod: {
      r: 0.5 + (avgR / 255) * 1.0,   // 0.5 to 1.5
      g: 0.5 + (avgG / 255) * 1.0,
      b: 0.5 + (avgB / 255) * 1.0
    }
  };

  // Persistent interaction state nudges future pixel scanning and sound behavior.
  feedback.timeModulation += Math.floor((interactionState.memory - 0.5) * 4500);
  feedback.shiftMod1 = clamp(feedback.shiftMod1 + Math.round((interactionState.scanSpread - 1) * 2), -3, 3);
  feedback.shiftMod2 = clamp(feedback.shiftMod2 + Math.round(interactionState.orbit), -3, 3);
  feedback.patternBias = (feedback.patternBias + scanOffset * 4) % 4;
  feedback.mixSpeed = clamp(feedback.mixSpeed * (0.8 + interactionState.scanSpread * 0.4), 0.4, 3.5);
  feedback.blendIntensity = clamp(feedback.blendIntensity + interactionState.memory * 0.25, 0.2, 1.0);
  feedback.chaosLevel = clamp(feedback.chaosLevel + interactionState.chaosBias * 0.6, 0, 1);
  feedback.intensity = clamp(feedback.intensity + interactionState.memory * 0.2, 0.5, 1.4);

  return feedback;
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, sound }) {
  // NO WIPE - let pixels accumulate over time for trails and patterns
  
  if (totalVoiceCount() === 0) {
    wipe(10, 14, 22);
    drawTouchMapping({ ink, line, write, screen, emphasized: true });
    ink(210, 232, 255);
    write("hold touches to play", { x: 2, y: Math.max(hudSafeTop + 2, screen.height - 16) }, undefined, undefined, false, uiFont);
    write("x=base hz  y=pitch mult", { x: 2, y: Math.max(hudSafeTop + 10, screen.height - 8) }, undefined, undefined, false, uiFont);
    return;
  }
    // FEEDBACK LOOP: Sample existing pixels to influence the bytebeat algorithm
  sharedPixelFeedback = samplePixelFeedback(screen);
  const feedback = sharedPixelFeedback;
  
  // Generate current bytebeat data for pixel manipulation (now with feedback influence)
  const freqScale = currentFrequency / 440;
  const timeOffset = Math.floor((performance.now() / 1000) * 44100 * freqScale * 0.3) + feedback.timeModulation;
  
  // Direct pixel manipulation based on bytebeat algorithm
  for (let x = 0; x < screen.width; x++) {
    const t = timeOffset + Math.floor(x * freqScale * 0.8);
    
    // Multiple bytebeat patterns (now influenced by pixel feedback)
    const pattern1 = (t ^ (t >> (8 + feedback.shiftMod1)) ^ (t >> (9 + feedback.shiftMod2))) & 255;
    const harmonic = Math.max(1, Math.floor(freqScale * 2 * feedback.harmonicScale));
    const pattern2 = ((t * harmonic) & (t >> (5 + feedback.bitMod1)) | (t >> (4 + feedback.bitMod2))) & 255;
    const rhythmMod = Math.floor((freqScale - 1) * 8 * feedback.rhythmScale) + 7;
    const pattern3 = (t | (t >> rhythmMod | t >> 7)) * (t & (t >> 11 | t >> (9 + feedback.complexMod))) & 255;
    const pattern4 = (t & (t >> (5 + feedback.sierpinskiMod) | t >> 8)) & 255;
    
    // Time-based pattern mixing (influenced by pixel feedback)
    const mixPhase = ((performance.now() / 1000) * 0.08 * feedback.mixSpeed + freqScale * 0.5) % 4;
    let finalPattern;
    
    // Use feedback to bias pattern selection
    const biasedMixPhase = (mixPhase + feedback.patternBias) % 4;
    
    if (biasedMixPhase < 1) {
      const blend = biasedMixPhase * feedback.blendIntensity;
      finalPattern = pattern1 * (1 - blend) + pattern2 * blend;
    } else if (biasedMixPhase < 2) {
      const blend = (biasedMixPhase - 1) * feedback.blendIntensity;
      finalPattern = pattern2 * (1 - blend) + pattern3 * blend;
    } else if (biasedMixPhase < 3) {
      const blend = (biasedMixPhase - 2) * feedback.blendIntensity;
      finalPattern = pattern3 * (1 - blend) + pattern4 * blend;
    } else {
      const blend = (biasedMixPhase - 3) * feedback.blendIntensity;
      finalPattern = pattern4 * (1 - blend) + pattern1 * blend;
    }
    
    // Apply feedback modulation to final pattern
    let value = Math.floor(finalPattern * feedback.intensity) & 255;
    
    // Pixel-influenced chaos injection
    if (feedback.chaosLevel > 0.5) {
      value = value ^ Math.floor(feedback.chaosLevel * 128);
    }
    
    // Map bytebeat value to vertical position and color
    const normalizedValue = value / 255;
    const yPos = Math.floor(normalizedValue * screen.height);
    
    // Clamp yPos to screen bounds
    const clampedY = Math.max(0, Math.min(screen.height - 1, yPos));
    
    // Create color based on bit patterns, time, AND pixel feedback
    const r = ((value ^ (value >> 2)) * feedback.colorMod.r) & 255;
    const g = ((value ^ (value >> 4)) * feedback.colorMod.g) & 255; 
    const b = ((value ^ (value >> 6)) * feedback.colorMod.b) & 255;
    
    // Set the pixel directly
    const pixelIndex = (clampedY * screen.width + x) * 4;
    screen.pixels[pixelIndex] = r;         // Red
    screen.pixels[pixelIndex + 1] = g;     // Green
    screen.pixels[pixelIndex + 2] = b;     // Blue
    screen.pixels[pixelIndex + 3] = 255;   // Alpha (fully opaque)
    
    // Add additional pixels for bit pattern visualization
    for (let bit = 0; bit < 8; bit++) {
      if ((value >> bit) & 1) {
        const bitY = bit * Math.floor(screen.height / 8);
        if (bitY < screen.height) {
          const bitPixelIndex = (bitY * screen.width + x) * 4;
          // Additive blending for accumulation effect
          screen.pixels[bitPixelIndex] = Math.min(255, screen.pixels[bitPixelIndex] + 64);
          screen.pixels[bitPixelIndex + 1] = Math.min(255, screen.pixels[bitPixelIndex + 1] + 64);
          screen.pixels[bitPixelIndex + 2] = Math.min(255, screen.pixels[bitPixelIndex + 2] + 64);
          screen.pixels[bitPixelIndex + 3] = 255;
        }
      }
    }
    
    // Add frequency-responsive vertical streaks
    if (x % Math.max(1, Math.floor(8 / freqScale)) === 0) {
      for (let y = 0; y < screen.height; y++) {
        const streakValue = (t + y) & 255;
        if (streakValue > 200) { // Only bright streaks
          const streakPixelIndex = (y * screen.width + x) * 4;
          // Subtle additive streak
          screen.pixels[streakPixelIndex] = Math.min(255, screen.pixels[streakPixelIndex] + 32);
          screen.pixels[streakPixelIndex + 1] = Math.min(255, screen.pixels[streakPixelIndex + 1] + 32);
          screen.pixels[streakPixelIndex + 2] = Math.min(255, screen.pixels[streakPixelIndex + 2] + 32);
          screen.pixels[streakPixelIndex + 3] = 255;
        }
      }
    }
  }
  
  // Add time-based horizontal sweeps using pixel manipulation
  const sweepY = Math.floor((performance.now() * 0.01) % screen.height);
  for (let x = 0; x < screen.width; x++) {
    const t = timeOffset + x;
    const sweepValue = (t ^ (t >> 4)) & 255;
    const sweepPixelIndex = (sweepY * screen.width + x) * 4;
    
    // Bright sweep line
    screen.pixels[sweepPixelIndex] = sweepValue;
    screen.pixels[sweepPixelIndex + 1] = sweepValue;
    screen.pixels[sweepPixelIndex + 2] = sweepValue;
    screen.pixels[sweepPixelIndex + 3] = 255;
  }
  
  drawTouchMapping({ ink, line, write, screen, emphasized: false });
  drawTouchOverlays({ ink, line, circle, write, screen });
}

// 📚 Library

function boot({ hud }) {
  // Set the HUD label to show piece name and current frequency
  // hud.label("🐚 Seashell");
}

function act({ event: e, sound, screen, pens }) {
  let touchHandled = false;
  let drawHandled = false;
  let liftHandled = false;

  // Multi-touch indexed handling: each pointer index owns one voice.
  for (let i = 1; i <= maxTouchPointers; i++) {
    if (e.is(`touch:${i}`)) {
      touchHandled = true;
      const pointer = pens?.(i);
      const pointerX = pointer?.x ?? e.x;
      const pointerY = pointer?.y ?? e.y;
      startTouchVoice({
        pointerIndex: i,
        x: pointerX,
        y: pointerY,
        screenWidth: screen.width,
        screenHeight: screen.height,
        sound
      });
    }

    if (e.is(`draw:${i}`)) {
      drawHandled = true;
      const pointer = pens?.(i);
      const pointerX = pointer?.x ?? e.x;
      const pointerY = pointer?.y ?? e.y;
      updateTouchVoice({
        pointerIndex: i,
        x: pointerX,
        y: pointerY,
        screenWidth: screen.width,
        screenHeight: screen.height,
        sound
      });
    }

    if (e.is(`lift:${i}`)) {
      liftHandled = true;
      stopTouchVoice(i, 0.08);
    }
  }

  // Fallback for environments that only emit generic touch/draw/lift.
  if (!touchHandled && e.is("touch")) {
    startTouchVoice({
      pointerIndex: 1,
      x: e.x,
      y: e.y,
      screenWidth: screen.width,
      screenHeight: screen.height,
      sound
    });
  }

  if (!drawHandled && e.is("draw")) {
    updateTouchVoice({
      pointerIndex: 1,
      x: e.x,
      y: e.y,
      screenWidth: screen.width,
      screenHeight: screen.height,
      sound
    });
  }

  if (!liftHandled && e.is("lift")) {
    stopTouchVoice(1, 0.08);
  }
}

function sim({ sound, hud, screen }) {
  // Poll for waveform data
  sound.speaker?.poll();
  
  // Update HUD with current frequency
  // hud.label(`🐚 Seashell ${Math.round(currentFrequency)}Hz`);
  
  const lerpSpeed = 0.08;

  const millisecondsSinceTouch = performance.now() - interactionState.lastTouchAt;
  const touchRecentlyActive = millisecondsSinceTouch < 250;
  interactionState.memory *= touchRecentlyActive ? 0.998 : 0.992;
  interactionState.chaosBias *= touchRecentlyActive ? 0.997 : 0.985;
  interactionState.orbit *= 0.992;
  interactionState.scanVelocity = 0.0015 + interactionState.memory * 0.01;
  interactionState.scanOffset = (interactionState.scanOffset + interactionState.scanVelocity + interactionState.orbit * 0.0008 + 1) % 1;
  interactionState.scanSpread = clamp(
    interactionState.scanSpread * 0.996 + (touchVoices.size > 0 ? 0.003 : -0.001),
    0.5,
    2.0
  );
  interactionState.density = clamp(1 + interactionState.memory * 0.8 + touchVoices.size * 0.05, 0.75, 1.9);
  const pixelTargetFrequency = derivePixelFrequency(sharedPixelFeedback);
  if (Math.abs(pixelTargetFrequency - currentFrequency) > 0.5) {
    currentFrequency += (pixelTargetFrequency - currentFrequency) * lerpSpeed;
  } else {
    currentFrequency = pixelTargetFrequency;
  }

  for (const voice of touchVoices.values()) {
    const targetFrequency = deriveVoiceFrequency({
      x: voice.x,
      y: voice.y,
      screenWidth: screen.width,
      screenHeight: screen.height
    });

    if (Math.abs(targetFrequency - voice.frequency) > 0.5) {
      voice.frequency += (targetFrequency - voice.frequency) * 0.22;
    } else {
      voice.frequency = targetFrequency;
    }

    voice.sound?.update?.({
      tone: voice.frequency
    });
  }
}

function leave() {
  stopAllVoices(0.05);
}

// function boot() {
// Runs once at the start.
// }

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
