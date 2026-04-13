# Seashells: Concrete Variation Examples

Four complete variation sketches with copy-paste code. Each is a self-contained remix you can test.

---

## Example 1: "Harmonic Bell" — Constrained Pitch Mapping

**Concept:** Instead of a continuous frequency space, voices snap to a musical scale. Creates bell-like tones.

**Key change:** Replace the `mapXToFrequency()` function

```javascript
// Replace mapXToFrequency() with this:
function mapXToFrequencyQuantized(x, width) {
  const w = Math.max(1, width - 1);
  const nx = clamp((x ?? w / 2) / w, 0, 1);
  
  // C minor pentatonic scale
  const notes = [
    55,   // A1
    66,   // B1
    82,   // E2
    110,  // A2
    123,  // B2
    165,  // E3
    220,  // A3
    247,  // B3
    330,  // E4
    440   // A4
  ];
  
  const index = Math.floor(nx * (notes.length - 1));
  return notes[index];
}

// Then in deriveVoiceFrequency():
function deriveVoiceFrequency({ x, y, screenWidth, screenHeight }) {
  const base = mapXToFrequencyQuantized(x, screenWidth) * mapYToPitchFactor(y, screenHeight);
  return clamp(base, 55, 1760);
}
```

**What this does:**
- X-axis snaps to 10 specific pitches (A minor pentatonic)
- Y-axis still modulates pitch up/down
- Result: Naturally harmonious, bells/resonators

**To test:** Press H, touch the left side, right side, watch the pitch snap between specific notes.

---

## Example 2: "Chaos Intensifier" — Feedback-Driven Synthesis

**Concept:** High visual variance → more chaotic audio. Creates feedback loops where visual complexity breeds audio wildness.

**Changes:**
1. Increase chaos sensitivity in `samplePixelFeedback()`
2. Add new "chaos patterns" to synthesis

```javascript
// In samplePixelFeedback(), find this line:
// chaosLevel: Math.min(1.0, variance / 20000),

// Replace with:
chaosLevel: clamp(Math.sqrt(variance / 10000), 0, 1), // More sensitive

// Then in the generator.bytebeat() function, find the chaos injection:
// if (liveFeedback && liveFeedback.chaosLevel > 0.5) {
//   finalPattern = finalPattern ^ Math.floor(liveFeedback.chaosLevel * 128);
// }

// Replace with:
if (liveFeedback && liveFeedback.chaosLevel > 0.3) {
  const chaosAmount = Math.floor(liveFeedback.chaosLevel * 200);
  finalPattern = (finalPattern ^ chaosAmount) + (chaosAmount >> 2) & 255;
}
```

**What this does:**
- Chaos level becomes much more sensitive (square root scaling)
- Chaos injection affects more bits
- Bright, contrasty visuals → immediately more chaotic audio

**To test:** Press H, make the screen bright/contrasty with touches, watch audio become glitchier.

---

## Example 3: "Grid Voices" — Spatial Voice Quantization

**Concept:** Hold sequence spawns voices on a grid, creating structured movement patterns.

**Replace the `spawnHoldVoice()` function:**

```javascript
// Grid configuration
const gridConfig = {
  cols: 4,
  rows: 3,
  cellIndex: 0
};

function spawnHoldVoice(screenWidth, screenHeight, sound) {
  const voiceId = holdSequence.nextVoiceId++;
  const feedback = sharedPixelFeedback;

  // Get next grid position (row-major order)
  const cellIndex = gridConfig.cellIndex % (gridConfig.cols * gridConfig.rows);
  const col = cellIndex % gridConfig.cols;
  const row = Math.floor(cellIndex / gridConfig.cols);
  gridConfig.cellIndex += 1;

  // Convert grid to screen coordinates (with padding)
  const padding = 40;
  const cellWidth = (screenWidth - padding * 2) / gridConfig.cols;
  const cellHeight = (screenHeight - padding * 2) / gridConfig.rows;
  
  const x = padding + (col + 0.5) * cellWidth;
  const y = padding + (row + 0.5) * cellHeight;

  // Duration varies by grid position
  const baseDuration = 4000 + (col + row) * 1000;
  const duration = baseDuration + (Math.random() - 0.5) * 1000;

  const hold = {
    voiceId,
    x,
    y,
    startTime: performance.now(),
    duration,
    orbitPhase: 0,
    orbitSpeed: 0.0001 + col * 0.00005 // Different speeds per column
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

// In updateHoldVoices(), replace the orbital movement with:
function updateHoldVoices(screenWidth, screenHeight, sound) {
  if (!holdSequence.enabled) return;

  const now = performance.now();
  const feedback = sharedPixelFeedback;

  // Spawn new hold if interval exceeded
  if (now - holdSequence.lastSpawnTime > holdSequence.spawnInterval && holdSequence.activeHolds.length < 12) {
    spawnHoldVoice(screenWidth, screenHeight, sound);
  }

  // Update positions - GRID movement only (subtle vibrato)
  for (let i = holdSequence.activeHolds.length - 1; i >= 0; i--) {
    const hold = holdSequence.activeHolds[i];
    const elapsed = now - hold.startTime;

    if (elapsed > hold.duration) {
      stopTouchVoice(hold.voiceId, 0.15);
      holdSequence.activeHolds.splice(i, 1);
      continue;
    }

    // Grid position stays fixed, but add vibrato
    const vibratoAmount = 10 + Math.sin(now * 0.003 + hold.voiceId) * 8;
    const vibratoX = Math.sin(now * 0.004 + hold.voiceId * 0.5) * vibratoAmount;
    const vibratoY = Math.cos(now * 0.005 + hold.voiceId * 0.7) * vibratoAmount;

    const x = hold.x + vibratoX;
    const y = hold.y + vibratoY;

    updateTouchVoice({
      pointerIndex: hold.voiceId,
      x: Math.round(x),
      y: Math.round(y),
      screenWidth,
      screenHeight,
      sound
    });
  }
}
```

**What this does:**
- Voices spawn in a 4×3 grid and fill it sequentially
- Each voice has a fixed position with subtle vibrato
- Creates structured, predictable movement
- Different columns have different modulation speeds

**To test:** Press H, watch voices fill grid positions systematically.

---

## Example 4: "Waveform Display" — Visual Audio Feedback

**Concept:** Instead of bytebeat creating vertical lines, show actual waveform shapes. More "traditional" audio visualization.

**Replace most of the `paint()` function (lines 551-612):**

```javascript
// In paint(), replace the main pixel-manipulation loop with:

if (totalVoiceCount() === 0) {
  wipe(10, 14, 22);
  drawTouchMapping({ ink, line, write, screen, emphasized: true });
  ink(210, 232, 255);
  write("hold touches to play  /  press 'h' for hold sequence", { x: 2, y: Math.max(hudSafeTop + 2, screen.height - 16) }, undefined, undefined, false, uiFont);
  write("x=base hz  y=pitch mult", { x: 2, y: Math.max(hudSafeTop + 10, screen.height - 8) }, undefined, undefined, false, uiFont);
  return;
}

// FEEDBACK LOOP
sharedPixelFeedback = samplePixelFeedback(screen);
const feedback = sharedPixelFeedback;

// Generate waveform samples
const samplesPerFrame = screen.width;
const samples = generator.bytebeat({
  frequency: currentFrequency,
  sampleRate: 44100,
  time: performance.now() * 0.001,
  samplesNeeded: samplesPerFrame,
  feedback
});

// Draw waveform as oscilloscope-style
const centerY = screen.height * 0.5;
const amplitude = screen.height * 0.35;

for (let x = 0; x < samplesPerFrame - 1; x++) {
  const sample1 = samples[x];
  const sample2 = samples[x + 1];
  
  const y1 = centerY - sample1 * amplitude;
  const y2 = centerY - sample2 * amplitude;
  
  // Draw line between consecutive samples
  drawLineBresenham(
    Math.round(x),
    Math.round(y1),
    Math.round(x + 1),
    Math.round(y2),
    screen,
    [200, 150, 255, 255]
  );
}

// Draw baseline
for (let x = 0; x < screen.width; x++) {
  const pixelIndex = (centerY * screen.width + x) * 4;
  screen.pixels[pixelIndex] = 80;
  screen.pixels[pixelIndex + 1] = 80;
  screen.pixels[pixelIndex + 2] = 100;
  screen.pixels[pixelIndex + 3] = 255;
}

// Helper: simple Bresenham line drawing
function drawLineBresenham(x0, y0, x1, y1, screen, color) {
  const dx = Math.abs(x1 - x0);
  const dy = Math.abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;
  
  let x = x0, y = y0;
  while (true) {
    if (x >= 0 && x < screen.width && y >= 0 && y < screen.height) {
      const pixelIndex = (y * screen.width + x) * 4;
      screen.pixels[pixelIndex] = color[0];
      screen.pixels[pixelIndex + 1] = color[1];
      screen.pixels[pixelIndex + 2] = color[2];
      screen.pixels[pixelIndex + 3] = color[3];
    }
    
    if (x === x1 && y === y1) break;
    const e2 = 2 * err;
    if (e2 > -dy) err -= dy, x += sx;
    if (e2 < dx) err += dx, y += sy;
  }
}

drawTouchMapping({ ink, line, write, screen, emphasized: false });
drawTouchOverlays({ ink, line, circle, write, screen });
```

**What this does:**
- Shows actual audio waveform like an oscilloscope
- Waveform updates in real-time based on synthesized samples
- Visual directly represents what you're hearing
- Feedback loop still influences timbre

**To test:** Press H, watch the waveform shape change as pattern blending happens.

---

## Example 5: "Memory Painter" — Voices Follow Visual Entropy

**Concept:** Voices spawn where the screen is most chaotic, creating a feedback where audio "grows" from visual disturbance.

**Modify `spawnHoldVoice()`:**

```javascript
function spawnHoldVoice(screenWidth, screenHeight, sound) {
  const voiceId = holdSequence.nextVoiceId++;
  const feedback = sharedPixelFeedback;

  // Sample multiple regions and find the most chaotic
  const samplePoints = 16;
  let maxChaos = 0;
  let spawnX = screenWidth * 0.5;
  let spawnY = screenHeight * 0.5;

  for (let i = 0; i < samplePoints; i++) {
    const x = Math.random() * screenWidth;
    const y = Math.random() * screenHeight;
    
    // Measure local entropy (variance of nearby pixels)
    const regionSamples = [];
    for (let dx = -10; dx <= 10; dx += 5) {
      for (let dy = -10; dy <= 10; dy += 5) {
        const px = clamp(Math.round(x + dx), 0, screenWidth - 1);
        const py = clamp(Math.round(y + dy), 0, screenHeight - 1);
        const pixelIndex = (py * screenWidth + px) * 4;
        const brightness = screen.pixels[pixelIndex] + 
                          screen.pixels[pixelIndex + 1] + 
                          screen.pixels[pixelIndex + 2];
        regionSamples.push(brightness);
      }
    }
    
    // Compute variance
    const avg = regionSamples.reduce((a, b) => a + b, 0) / regionSamples.length;
    const variance = regionSamples.reduce((sum, val) => sum + Math.pow(val - avg, 2), 0) / regionSamples.length;
    
    if (variance > maxChaos) {
      maxChaos = variance;
      spawnX = x;
      spawnY = y;
    }
  }

  const baseDuration = 5000 + (1 - feedback.chaosLevel) * 8000;
  const duration = baseDuration + (Math.random() - 0.5) * 2000;

  const hold = {
    voiceId,
    x: spawnX,
    y: spawnY,
    startTime: performance.now(),
    duration,
    orbitPhase: 0,
    orbitSpeed: 0.0002
  };

  startTouchVoice({
    pointerIndex: voiceId,
    x: Math.round(spawnX),
    y: Math.round(spawnY),
    screenWidth,
    screenHeight,
    sound
  });

  holdSequence.activeHolds.push(hold);
  holdSequence.lastSpawnTime = performance.now();
}
```

**What this does:**
- Analyzes visual entropy (how chaotic pixels are)
- Spawns new voices in the most chaotic regions
- Creates positive feedback: audio → pixels → more audio
- Visual "disturbances" are musically rewarded

**To test:** Press H, touch screen to create visual chaos, watch new voices spawn there.

---

## How to Implement These

1. **Backup original:**
   ```bash
   cp system/public/aesthetic.computer/disks/seashells.mjs seashells_original.mjs
   ```

2. **Pick one variation** (say, Harmonic Bell)

3. **Copy its code** into seashells.mjs, replacing the specified functions

4. **Test in dev environment:**
   ```bash
   npm run ac
   # Navigate to seashells in browser
   # Press H to activate hold sequence
   ```

5. **Iterate:** Once you get one variation working, try others

---

## Combining Variations

You can **stack these concepts**:

- Grid Voices + Harmonic Bell = Structured harmonic grid
- Chaos Intensifier + Waveform Display = Visual feedback of audio chaos
- Memory Painter + Grid Voices = Chaos accumulates in grid cells
- All four = Complex emergent system

The trick is testing each change in isolation first, then carefully combining them.

---

## Debugging Tips

**If synthesis breaks (no sound):**
- Check that `currentFrequency` is in 20–20000 Hz range
- Verify generator.bytebeat returns 512+ samples
- Check sound.synth() is being called with correct parameters

**If hold sequence doesn't work:**
- Verify `holdSequence.enabled` is toggled by 'H' key
- Check `spawnHoldVoice()` is being called from `updateHoldVoices()`
- Make sure `voiceId` values don't collide with touch pointer IDs

**If visuals freeze:**
- Pixel manipulation loops might be expensive
- Reduce `samplesPerFrame` or `gridConfig` cell count
- Profile in DevTools Performance tab

**If feedback loop breaks:**
- Verify `sharedPixelFeedback` is being updated in paint()
- Check pixel sampling doesn't go out of bounds
- Ensure feedback parameters scale to expected ranges

---

## What To Listen For

### Harmonic Bell
- Should sound like struck bells or gongs
- Quantized pitches mean less dissonance
- Movement within grid feels musically constrained

### Chaos Intensifier
- Silent visuals = subtle, calm tone
- Complex/bright visuals = harsh, glitchy audio
- Real feedback loop, not just cosmetic

### Grid Voices
- Predictable, structured movement
- Different columns have different "personalities" (timbre)
- Feels like an instrument you could learn to play

### Waveform Display
- You see exactly what you hear
- Blending between patterns visible as shape changes
- Useful for understanding bytebeat architecture

### Memory Painter
- Audio grows out of visual "accidents"
- Touching creates short-term audio response
- Over time, visual noise accumulates

---

Happy remixing! Each variation teaches you something about how the layers interact.
