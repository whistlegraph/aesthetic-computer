// Seashell — bare-bones bytebeat synthesizer
// Lab bench proof of concept: minimal code, maximum clarity

/* Core mechanism:
   - 2 simple bytebeat patterns
   - Hold sequence: spawn voices automatically
   - X-axis = frequency, Y-axis = pitch multiplier
   - No visual feedback loop, no complexity
   - ~350 lines, easy to understand and modify
*/

// Voice management
const touchVoices = new Map();
const maxTouchPointers = 8;

// Hold sequence: auto-spawn voices
const holdSequence = {
  enabled: false,
  activeHolds: [],
  lastSpawnTime: 0,
  spawnInterval: 3000, // 3 seconds between spawns
  nextVoiceId: 100
};

// Parameters (controlled by sliders)
const params = {
  spawnInterval: 3000,      // ms between voice spawns
  voiceDuration: 6000,      // ms each voice lasts
  orbitSpeed: 0.0003,       // rad/frame orbital speed
  blendSpeed: 0.1,          // pattern blend rate
  maxVoices: 5              // max concurrent auto voices
};

// Slider UI
const sliders = [
  {
    label: "Spawn",
    key: "spawnInterval",
    min: 500,
    max: 8000,
    step: 100,
    x: 0,
    y: 0,
    width: 0,
    height: 16,
    dragging: false
  },
  {
    label: "Duration",
    key: "voiceDuration",
    min: 2000,
    max: 20000,
    step: 500,
    x: 0,
    y: 0,
    width: 0,
    height: 16,
    dragging: false
  },
  {
    label: "OrbitSpd",
    key: "orbitSpeed",
    min: 0.0001,
    max: 0.001,
    step: 0.00005,
    x: 0,
    y: 0,
    width: 0,
    height: 16,
    dragging: false
  },
  {
    label: "BlendSpd",
    key: "blendSpeed",
    min: 0.01,
    max: 0.5,
    step: 0.02,
    x: 0,
    y: 0,
    width: 0,
    height: 16,
    dragging: false
  },
  {
    label: "MaxVoices",
    key: "maxVoices",
    min: 1,
    max: 10,
    step: 1,
    x: 0,
    y: 0,
    width: 0,
    height: 16,
    dragging: false
  }
];

// Simple bytebeat generator
const generator = {
  bytebeat: ({ frequency, sampleRate, time, samplesNeeded, feedback = null }) => {
    const samples = [];
    const freqScale = frequency / 440;
    const timeOffset = Math.floor(time * sampleRate * freqScale * 0.3);

    for (let i = 0; i < samplesNeeded; i++) {
      const t = timeOffset + i;

      // Pattern 1: XOR cascade (crisp, digital)
      const p1 = (t ^ (t >> 8) ^ (t >> 9)) & 255;

      // Pattern 2: Melodic (pitched, harmonic)
      const harmonic = Math.max(1, Math.floor(freqScale * 2));
      const p2 = ((t * harmonic) & (t >> 5) | (t >> 4)) & 255;

      // Mix patterns based on time
      const mixPhase = (time * 0.1 + freqScale * 0.5) % 2;
      let finalPattern;
      if (mixPhase < 1) {
        const blend = mixPhase;
        finalPattern = p1 * (1 - blend) + p2 * blend;
      } else {
        const blend = mixPhase - 1;
        finalPattern = p2 * (1 - blend) + p1 * blend;
      }

      // Convert to audio range
      let sample = (finalPattern / 127.5) - 1;
      sample *= 0.6; // Volume scaling
      samples.push(sample);
    }
    return samples;
  }
};

function clamp(value, low, high) {
  return Math.max(low, Math.min(high, value));
}

// Spatial mapping
function mapXToFrequency(x, width) {
  const nx = clamp(x / width, 0, 1);
  const minHz = 55;
  const maxHz = 880;
  return minHz * Math.pow(maxHz / minHz, nx);
}

function mapYToPitch(y, height) {
  const ny = clamp(y / height, 0, 1);
  return Math.pow(2, (0.5 - ny) * 2); // 0.5x to 2x
}

function deriveFrequency(x, y, screenWidth, screenHeight) {
  const base = mapXToFrequency(x, screenWidth) * mapYToPitch(y, screenHeight);
  return clamp(base, 40, 2000);
}

// Voice management
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
  const key = `touch-${pointerIndex}`;
  if (touchVoices.has(key)) return;

  const frequency = deriveFrequency(x, y, screenWidth, screenHeight);
  const voice = createVoice({ sound, frequency, volume: 0.5 });

  touchVoices.set(key, { sound: voice, x, y, frequency });
  rebalanceVolumes();
}

function updateTouchVoice({ pointerIndex, x, y, screenWidth, screenHeight }) {
  const key = `touch-${pointerIndex}`;
  const voice = touchVoices.get(key);
  if (!voice) return;

  voice.x = x;
  voice.y = y;

  const targetFrequency = deriveFrequency(x, y, screenWidth, screenHeight);
  if (Math.abs(targetFrequency - voice.frequency) > 0.5) {
    voice.frequency += (targetFrequency - voice.frequency) * 0.15;
    voice.sound?.update?.({ tone: voice.frequency });
  }
}

function stopTouchVoice(pointerIndex, fade = 0.1) {
  const key = `touch-${pointerIndex}`;
  const voice = touchVoices.get(key);
  if (!voice) return;
  voice.sound?.kill(fade);
  touchVoices.delete(key);
  rebalanceVolumes();
}

function stopAllVoices(fade = 0.1) {
  for (const voice of touchVoices.values()) {
    voice.sound?.kill(fade);
  }
  touchVoices.clear();
}

function rebalanceVolumes() {
  const count = touchVoices.size;
  if (count <= 0) return;
  const baseVolume = clamp(0.5 / Math.sqrt(count), 0.15, 0.4);
  for (const voice of touchVoices.values()) {
    voice.sound?.update?.({ volume: baseVolume });
  }
}

// Hold sequence: auto-voice generation
function spawnHoldVoice(screenWidth, screenHeight, sound) {
  const voiceId = holdSequence.nextVoiceId++;

  // Orbital position
  const angle = (performance.now() * 0.0001) + Math.random() * Math.PI * 2;
  const radius = 0.35;
  const x = (Math.cos(angle) * radius + 0.5) * screenWidth;
  const y = (Math.sin(angle) * radius + 0.5) * screenHeight;

  // Duration with randomness (from params)
  const duration = params.voiceDuration + (Math.random() - 0.5) * (params.voiceDuration * 0.5);

  const hold = {
    voiceId,
    x,
    y,
    startTime: performance.now(),
    duration,
    orbitPhase: angle,
    orbitSpeed: params.orbitSpeed + Math.random() * (params.orbitSpeed * 0.5)
  };

  startTouchVoice({
    pointerIndex: voiceId,
    x: Math.round(x),
    y: Math.round(y),
    screenWidth,
    screenHeight,
    sound
  });

  holdSequence.activeHolds.push(hold);
  holdSequence.lastSpawnTime = performance.now();
}

function updateHoldVoices(screenWidth, screenHeight, sound) {
  if (!holdSequence.enabled) return;

  const now = performance.now();

  // Spawn new voice if needed (use params)
  if (now - holdSequence.lastSpawnTime > params.spawnInterval && holdSequence.activeHolds.length < params.maxVoices) {
    spawnHoldVoice(screenWidth, screenHeight, sound);
  }

  // Update existing holds
  for (let i = holdSequence.activeHolds.length - 1; i >= 0; i--) {
    const hold = holdSequence.activeHolds[i];
    const elapsed = now - hold.startTime;

    if (elapsed > hold.duration) {
      stopTouchVoice(hold.voiceId, 0.1);
      holdSequence.activeHolds.splice(i, 1);
      continue;
    }

    // Orbital movement
    hold.orbitPhase += hold.orbitSpeed;
    const x = (Math.cos(hold.orbitPhase) * 0.3 + 0.5) * screenWidth;
    const y = (Math.sin(hold.orbitPhase) * 0.3 + 0.5) * screenHeight;

    updateTouchVoice({
      pointerIndex: hold.voiceId,
      x: Math.round(x),
      y: Math.round(y),
      screenWidth,
      screenHeight
    });
  }
}

function toggleHoldSequence(screenWidth, screenHeight, sound) {
  if (holdSequence.enabled) {
    for (const hold of holdSequence.activeHolds) {
      stopTouchVoice(hold.voiceId, 0.08);
    }
    holdSequence.activeHolds = [];
    holdSequence.enabled = false;
  } else {
    holdSequence.enabled = true;
    holdSequence.nextVoiceId = 100;
    spawnHoldVoice(screenWidth, screenHeight, sound);
  }
}

// Slider helpers
function drawSliders({ ink, write, screen }) {
  const sliderAreaHeight = sliders.length * 20 + 10;
  const sliderY = screen.height - sliderAreaHeight;

  // Draw slider area background
  ink(8, 10, 16);
  for (let y = sliderY; y < screen.height; y++) {
    for (let x = 0; x < screen.width; x++) {
      // Just set background by clearing that area when we draw
    }
  }

  // Position sliders
  const labelWidth = 55;
  const sliderWidth = screen.width - labelWidth - 15;

  sliders.forEach((slider, i) => {
    slider.y = sliderY + i * 20 + 5;
    slider.x = labelWidth;
    slider.width = sliderWidth;
    slider.height = 14;

    // Draw label
    ink(140, 160, 190);
    write(slider.label, { x: 5, y: slider.y }, undefined, undefined, false, "MatrixChunky8");

    // Draw slider background
    ink(30, 40, 60);
    for (let sx = slider.x; sx < slider.x + slider.width; sx++) {
      write("_", { x: sx, y: slider.y }, undefined, undefined, false, "MatrixChunky8");
    }

    // Draw slider handle
    const normalizedValue = (params[slider.key] - slider.min) / (slider.max - slider.min);
    const handleX = slider.x + Math.floor(normalizedValue * slider.width);

    ink(100, 180, 220);
    write("■", { x: handleX, y: slider.y }, undefined, undefined, false, "MatrixChunky8");

    // Draw value
    ink(180, 200, 230);
    let displayValue = params[slider.key];
    if (slider.step < 1) {
      displayValue = displayValue.toFixed(5);
    } else {
      displayValue = Math.round(displayValue);
    }
    write(`${displayValue}`, { x: slider.x + slider.width + 5, y: slider.y }, undefined, undefined, false, "MatrixChunky8");
  });
}

function checkSliderClick(x, y) {
  for (let i = 0; i < sliders.length; i++) {
    const slider = sliders[i];
    if (y >= slider.y && y < slider.y + slider.height && x >= slider.x && x < slider.x + slider.width) {
      return i;
    }
  }
  return -1;
}

function updateSlider(sliderIndex, x) {
  if (sliderIndex < 0 || sliderIndex >= sliders.length) return;
  const slider = sliders[sliderIndex];
  const relativeX = clamp(x - slider.x, 0, slider.width);
  const normalizedValue = relativeX / slider.width;
  const newValue = slider.min + normalizedValue * (slider.max - slider.min);
  params[slider.key] = newValue;
}

// Rendering
function paint({ wipe, ink, write, screen, box }) {
  const voiceCount = touchVoices.size;

  // Clear screen
  wipe(10, 12, 18);

  // Draw simple grid (frequency reference)
  ink(40, 50, 70);
  for (let x = 0; x < screen.width; x += Math.floor(screen.width / 8)) {
    for (let y = 0; y < screen.height - 110; y += Math.floor(screen.height / 8)) {
      write("·", { x, y }, undefined, undefined, false, "MatrixChunky8");
    }
  }

  // Draw frequency labels
  ink(180, 200, 230);
  write(`${Math.round(mapXToFrequency(0, screen.width))}Hz`, { x: 2, y: 2 }, undefined, undefined, false, "MatrixChunky8");
  write(`${Math.round(mapXToFrequency(screen.width, screen.width))}Hz`, { x: screen.width - 35, y: 2 }, undefined, undefined, false, "MatrixChunky8");

  // Draw status
  ink(220, 240, 255);
  const status = holdSequence.enabled ? "HOLD: ON" : "HOLD: OFF";
  write(status, { x: 2, y: screen.height - 130 }, undefined, undefined, false, "MatrixChunky8");
  write(`Voices: ${voiceCount}`, { x: screen.width - 50, y: screen.height - 130 }, undefined, undefined, false, "MatrixChunky8");

  // Draw voice positions
  ink(100, 180, 220, 100);
  for (const [key, voice] of touchVoices.entries()) {
    const x = Math.round(voice.x);
    const y = Math.round(voice.y);

    // Draw circle
    ink(150, 200, 255, 150);
    write("●", { x, y }, undefined, undefined, false, "MatrixChunky8");

    // Draw frequency label
    ink(200, 220, 255);
    write(`${Math.round(voice.frequency)}`, { x: x + 3, y: y - 2 }, undefined, undefined, false, "MatrixChunky8");
  }

  // Help text
  if (voiceCount === 0) {
    ink(180, 200, 230);
    write("Touch to play  |  Press H for hold", { x: 2, y: Math.floor(screen.height / 2) - 50 }, undefined, undefined, false, "MatrixChunky8");
  }

  // Draw sliders
  drawSliders({ ink, write, screen });
}

// Input handling
function act({ event: e, sound, screen, pens }) {
  // Keyboard: H toggles hold sequence
  if (e.is("keyboard:down:h")) {
    toggleHoldSequence(screen.width, screen.height, sound);
  }

  // Slider interaction (check for slider touches first)
  for (let i = 1; i <= maxTouchPointers; i++) {
    if (e.is(`touch:${i}`)) {
      const pointer = pens?.(i);
      const x = pointer?.x ?? e.x;
      const y = pointer?.y ?? e.y;

      const sliderIndex = checkSliderClick(x, y);
      if (sliderIndex >= 0) {
        sliders[sliderIndex].dragging = i;
        updateSlider(sliderIndex, x);
        return; // Don't create voice if touching slider
      }
    }

    if (e.is(`draw:${i}`)) {
      const pointer = pens?.(i);
      const x = pointer?.x ?? e.x;
      const y = pointer?.y ?? e.y;

      // Check if this was a slider drag
      if (sliders.some(s => s.dragging === i)) {
        const sliderIndex = sliders.findIndex(s => s.dragging === i);
        updateSlider(sliderIndex, x);
        return;
      }

      // Otherwise update touch voice
      updateTouchVoice({ pointerIndex: i, x, y, screenWidth: screen.width, screenHeight: screen.height });
    }

    if (e.is(`lift:${i}`)) {
      // Check if this was a slider
      const sliderIndex = sliders.findIndex(s => s.dragging === i);
      if (sliderIndex >= 0) {
        sliders[sliderIndex].dragging = false;
        return;
      }

      // Otherwise stop touch voice
      stopTouchVoice(i, 0.08);
    }
  }

  // Touch/mouse input (only if not on slider)
  for (let i = 1; i <= maxTouchPointers; i++) {
    if (e.is(`touch:${i}`)) {
      const pointer = pens?.(i);
      const x = pointer?.x ?? e.x;
      const y = pointer?.y ?? e.y;
      if (checkSliderClick(x, y) < 0) {
        startTouchVoice({ pointerIndex: i, x, y, screenWidth: screen.width, screenHeight: screen.height, sound });
      }
    }
  }

  // Fallback for single-touch environments
  if (e.is("touch")) {
    if (checkSliderClick(e.x, e.y) < 0) {
      startTouchVoice({ pointerIndex: 1, x: e.x, y: e.y, screenWidth: screen.width, screenHeight: screen.height, sound });
    }
  }
  if (e.is("draw")) {
    updateTouchVoice({ pointerIndex: 1, x: e.x, y: e.y, screenWidth: screen.width, screenHeight: screen.height });
  }
  if (e.is("lift")) {
    stopTouchVoice(1, 0.08);
  }
}

// Per-frame updates
function sim({ sound, screen }) {
  sound.speaker?.poll();
  updateHoldVoices(screen.width, screen.height, sound);
}

// Initialization
function boot({ hud }) {
  // Runs once at startup
}

// Cleanup
function leave() {
  stopAllVoices(0.05);
}
