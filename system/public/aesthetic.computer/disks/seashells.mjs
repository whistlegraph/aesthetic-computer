// Seashells, 2025.6.13.01.41.06.896
// Bytebeat algorithmic synthesis.

/* 📝 Engineering Notes
  Bytebeat synthesis using streaming generators.
  This demonstrates algorithmic music generation where complex patterns
  emerge from simple mathematical expressions operating on integers.
*/

let mouseSound = null;
let isDragging = false;

// Bytebeat generator
const generator = {
  // Bytebeat algorithmic synthesis with pixel feedback capability
  bytebeat: ({ frequency, sampleRate, time, samplesNeeded, feedback = null }) => {
    const samples = [];
    const freqScale = frequency / 440; // Scale relative to A440
    
    // Apply feedback to time offset if available
    let timeOffset = Math.floor(time * sampleRate * freqScale * 0.3);
    if (feedback && feedback.timeModulation) {
      timeOffset += feedback.timeModulation;
    }
    
    for (let i = 0; i < samplesNeeded; i++) {
      const t = timeOffset + Math.floor(i * freqScale * 0.8); // Sample-based time progression
      
      // Classic bytebeat patterns - now modifiable by feedback
      let shiftMod1 = feedback?.shiftMod1 || 0;
      let shiftMod2 = feedback?.shiftMod2 || 0;
      let harmonicScale = feedback?.harmonicScale || 1.0;
      let rhythmScale = feedback?.rhythmScale || 1.0;
      
      // Pattern 1: XOR cascade (crisp, digital texture)
      const pattern1 = (t ^ (t >> (8 + shiftMod1)) ^ (t >> (9 + shiftMod2))) & 255;
      
      // Pattern 2: Melodic stepped pattern with harmonic scaling
      const harmonic = Math.max(1, Math.floor(freqScale * 2 * harmonicScale));
      const pattern2 = ((t * harmonic) & (t >> (5 + (feedback?.bitMod1 || 0))) | (t >> (4 + (feedback?.bitMod2 || 0)))) & 255;
      
      // Pattern 3: Complex rhythmic pattern with frequency modulation
      const rhythmMod = Math.floor((freqScale - 1) * 8 * rhythmScale) + 7;
      const pattern3 = (t | (t >> rhythmMod | t >> 7)) * (t & (t >> 11 | t >> (9 + (feedback?.complexMod || 0)))) & 255;
      
      // Pattern 4: Sierpinski triangle-like pattern
      const pattern4 = (t & (t >> (5 + (feedback?.sierpinskiMod || 0)) | t >> 8)) & 255;
      
      // Pattern 5: Frequency-responsive melodic pattern
      const melodyScale = Math.floor(freqScale * 6) + 1;
      const pattern5 = ((t * melodyScale) ^ (t >> 6)) & (t >> 8) & 255;
      
      // Dynamic pattern mixing based on time, frequency, and feedback
      let mixPhase = (time * 0.08 + freqScale * 0.5) % 5;
      if (feedback && feedback.mixSpeed) {
        mixPhase = (time * 0.08 * feedback.mixSpeed + freqScale * 0.5) % 5;
      }
      if (feedback && feedback.patternBias) {
        mixPhase = (mixPhase + feedback.patternBias) % 5;
      }
      
      let finalPattern;
      let blendIntensity = feedback?.blendIntensity || 1.0;
      
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
      if (feedback && feedback.intensity) {
        finalPattern *= feedback.intensity;
      }
      
      // Apply chaos injection from feedback
      if (feedback && feedback.chaosLevel > 0.5) {
        finalPattern = finalPattern ^ Math.floor(feedback.chaosLevel * 128);
      }
      
      // Convert from 0-255 byte range to -1 to 1 audio range
      let sample = (finalPattern / 127.5) - 1;
      
      // Apply dynamic filtering based on frequency to reduce harshness
      const filterAmount = Math.min(0.8, 0.3 + (freqScale - 1) * 0.1);
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

let targetFrequency = 440; // Target frequency for smooth interpolation
let currentFrequency = 440; // Current interpolated frequency

// FEEDBACK SYSTEM: Sample pixels to influence bytebeat generation
function samplePixelFeedback(screen) {
  const samples = [];
  const samplePoints = 16; // Number of strategic sampling points
  
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
      const progress = diag / 4;
      x = Math.floor(progress * screen.width);
      y = Math.floor(progress * screen.height);
    } else {
      // Random strategic points influenced by current frequency
      const freq_offset = (currentFrequency / 440) * (i - 12);
      x = Math.floor((Math.sin(performance.now() * 0.001 + freq_offset) * 0.5 + 0.5) * screen.width);
      y = Math.floor((Math.cos(performance.now() * 0.001 + freq_offset) * 0.5 + 0.5) * screen.height);
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
  return {
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
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, sound }) {
  // NO WIPE - let pixels accumulate over time for trails and patterns
  
  if (!mouseSound) {
    // Only show instructions when no sound, using direct pixel manipulation
    const centerX = Math.floor(screen.width / 2);
    const centerY = Math.floor(screen.height / 2);
    
    // Draw instruction text pixel by pixel (simple approach)
    // Fill a dark overlay first
    for (let y = 0; y < screen.height; y++) {
      for (let x = 0; x < screen.width; x++) {
        const pixelIndex = (y * screen.width + x) * 4;
        screen.pixels[pixelIndex] = 32;     // R
        screen.pixels[pixelIndex + 1] = 32; // G  
        screen.pixels[pixelIndex + 2] = 32; // B
        screen.pixels[pixelIndex + 3] = 180; // A
      }
    }
    
    // Simple centered text using pixels
    const text = "PRESS SPACE TO START";
    const charWidth = 8;
    const startX = centerX - (text.length * charWidth) / 2;
    
    for (let i = 0; i < text.length; i++) {
      const charX = Math.floor(startX + i * charWidth);
      // Draw a simple character representation
      for (let dy = -4; dy <= 4; dy++) {
        for (let dx = -3; dx <= 3; dx++) {
          const x = charX + dx;
          const y = centerY + dy;
          if (x >= 0 && x < screen.width && y >= 0 && y < screen.height) {
            const pixelIndex = (y * screen.width + x) * 4;
            screen.pixels[pixelIndex] = 255;     // R
            screen.pixels[pixelIndex + 1] = 255; // G
            screen.pixels[pixelIndex + 2] = 255; // B
            screen.pixels[pixelIndex + 3] = 255; // A
          }
        }
      }
    }
    return;
  }
    // FEEDBACK LOOP: Sample existing pixels to influence the bytebeat algorithm
  const feedback = samplePixelFeedback(screen);
  
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
  
  // Visual cursor feedback using pixel manipulation
  if (pen) {
    const cursorSize = 8;
    for (let dy = -cursorSize; dy <= cursorSize; dy++) {
      for (let dx = -cursorSize; dx <= cursorSize; dx++) {
        const distance = Math.sqrt(dx * dx + dy * dy);
        if (distance <= cursorSize) {
          const x = pen.x + dx;
          const y = pen.y + dy;
          if (x >= 0 && x < screen.width && y >= 0 && y < screen.height) {
            const pixelIndex = (y * screen.width + x) * 4;
            const intensity = 1 - (distance / cursorSize);
            screen.pixels[pixelIndex] = Math.min(255, screen.pixels[pixelIndex] + 128 * intensity);     // Green cursor
            screen.pixels[pixelIndex + 1] = Math.min(255, screen.pixels[pixelIndex + 1] + 255 * intensity);
            screen.pixels[pixelIndex + 2] = Math.min(255, screen.pixels[pixelIndex + 2] + 128 * intensity);
            screen.pixels[pixelIndex + 3] = 255;
          }
        }
      }
    }
  }
}

// 📚 Library

function boot({ hud }) {
  // Set the HUD label to show piece name and current frequency
  // hud.label("🐚 Seashell");
}

function act({ event: e, sound, screen, hud }) {
  // Space to toggle sound
  if (e.is("keyboard:down:space")) {
    if (mouseSound) {
      mouseSound.kill(0.1);
      mouseSound = null;
    } else {      // Start playing with bytebeat generator
      mouseSound = sound.synth({
        type: "custom",
        tone: currentFrequency,
        duration: "🔁", // Infinite duration
        volume: 0.5,
        generator: generator.bytebeat
      });
    }
  }
    // Mouse/touch interaction
  if (e.is("touch")) {
    isDragging = true;
    targetFrequency = 200 + (e.x / screen.width) * 800;
    if (!mouseSound) {
      currentFrequency = targetFrequency; // Immediate set for new sound
      mouseSound = sound.synth({
        type: "custom",
        tone: currentFrequency,
        duration: "🔁",
        volume: 0.5,
        generator: generator.bytebeat
      });
    }
  }
  
  if (e.is("draw") && isDragging && mouseSound) {
    // Update target frequency based on mouse position
    targetFrequency = 200 + (e.x / screen.width) * 800;
  }
  
  if (e.is("lift")) {
    isDragging = false;
    if (mouseSound) {
      mouseSound.kill(0.1);
      mouseSound = null;
    }
  }
}

function sim({ sound, hud }) {
  // Poll for waveform data
  sound.speaker?.poll();
  
  // Update HUD with current frequency
  // hud.label(`🐚 Seashell ${Math.round(currentFrequency)}Hz`);
  
  // Smooth frequency interpolation
  const lerpSpeed = 0.05; // Adjust this for faster/slower interpolation (0.01 = very smooth, 0.1 = faster)
  
  if (Math.abs(targetFrequency - currentFrequency) > 0.5) {
    // Lerp current frequency towards target frequency
    currentFrequency += (targetFrequency - currentFrequency) * lerpSpeed;
    
    // Update the sound with the smoothly interpolated frequency
    if (mouseSound) {
      mouseSound.update({ tone: currentFrequency });
    }
  } else {
    // Close enough, snap to target
    currentFrequency = targetFrequency;
  }
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
