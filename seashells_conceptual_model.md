# Seashells: Conceptual Model & Variation Framework

This document breaks down the architecture of **seashells.mjs** into conceptual components, so you can understand, remix, and create variations.

---

## The Core Stack (4 Layers)

```
┌─────────────────────────────────┐
│  SEQUENCING LAYER               │ How voices spawn & interact
│  (Hold system, voice lifecycle) │
├─────────────────────────────────┤
│  SYNTHESIS LAYER                │ How audio is generated
│  (5 bytebeat patterns, blending)│
├─────────────────────────────────┤
│  FEEDBACK LOOP LAYER            │ Audio ↔ Visual feedback
│  (Pixel sampling → parameters)  │
├─────────────────────────────────┤
│  SPATIAL MAPPING LAYER          │ Touch → Frequency/Pitch
│  (X/Y to Hz, modulation axes)   │
└─────────────────────────────────┘
```

Each layer is **independently modifiable**. You can swap out any component without breaking the others.

---

## Layer 1: Spatial Mapping (Touch/Position → Audio Parameters)

### Current Implementation
```javascript
X-axis:  screen position → base frequency (80–1600 Hz logarithmic)
Y-axis:  screen position → pitch multiplier (0.5x–2x linear)
```

**Functions involved:**
- `mapXToFrequency(x, width)` - Convert X pixel to frequency
- `mapYToPitchFactor(y, height)` - Convert Y pixel to pitch multiplier
- `deriveVoiceFrequency()` - Combine both into final frequency

### Variations You Could Try

**1. Polar Coordinate Mapping**
```javascript
// Instead of cartesian X/Y
const angle = Math.atan2(y - centerY, x - centerX);
const distance = Math.sqrt((x-centerX)² + (y-centerY)²);
const frequency = minHz * Math.pow(maxHz/minHz, distance/maxRadius);
const timbre = (angle + Math.PI) / (2 * Math.PI); // Map to 0-1
```

**2. Vertical Strip Mapping** (like a piano keyboard)
```javascript
// Ignore X, only use Y for frequency
const frequency = 55 * Math.pow(2, y / screenHeight * 5); // 5 octaves
```

**3. Grid Quantization** (musical scale constraints)
```javascript
const notes = [55, 62, 69, 82, 110, 123, 147, 165, 196, 220]; // C minor pentatonic
const gridX = Math.round(x / screenWidth * (notes.length - 1));
const octaveY = Math.round(y / screenHeight * 4);
const frequency = notes[gridX] * Math.pow(2, octaveY);
```

**4. Feedback-Influenced Mapping** (space changes based on audio)
```javascript
const baseFreq = mapXToFrequency(x, width);
const pitchMult = mapYToPitchFactor(y, height);
// Modulate by pixel feedback
const feedbackScale = 0.8 + sharedPixelFeedback.intensity * 0.4;
return baseFreq * pitchMult * feedbackScale;
```

---

## Layer 2: Synthesis (Audio Generation)

### Current Architecture: 5 Blending Patterns

The piece uses **5 independent bytebeat generators** that morph through each other:

```javascript
pattern1 = (t ^ (t >> (8 + shiftMod1)) ^ (t >> (9 + shiftMod2))) & 255
pattern2 = ((t * harmonic) & (t >> (5 + bitMod1)) | (t >> (4 + bitMod2))) & 255
pattern3 = (t | (t >> rhythmMod | t >> 7)) * (t & (t >> 11 | t >> complexMod)) & 255
pattern4 = (t & (t >> (5 + sierpinskiMod) | t >> 8)) & 255
pattern5 = ((t * melodyScale) ^ (t >> 6)) & (t >> 8) & 255
```

**Blending mechanism:**
- Time-based phase progresses through 5 states (0→1→2→3→4→0)
- Between states, linear interpolation smooths transitions
- Phase speed and intensity controlled by feedback

### Understanding Each Pattern

| Pattern | Type | Character | Key Insight |
|---------|------|-----------|-------------|
| **Pattern 1** | XOR Cascade | Digital, crisp, glitchy | Bit flips create harsh transitions |
| **Pattern 2** | Melodic | Pitched, harmonic | `t * harmonic` creates repeating cycles |
| **Pattern 3** | Rhythmic | Complex polyrhythm | Multiplication creates interference patterns |
| **Pattern 4** | Fractal | Sierpinski-like, algorithmic | Simple XOR creates complexity |
| **Pattern 5** | Frequency-Responsive | Pitch-sensitive melodic | Scale changes with input frequency |

### Variation: Add Your Own Pattern

**Step 1: Design a pattern**
```javascript
const pattern6 = (t * t) & (t >> (7 + feedback.complexity)) & 255;
```

**Step 2: Integrate into blending loop**
```javascript
let mixPhase = (time * 0.08 + freqScale * 0.5) % 6; // Changed from 5 to 6
if (mixPhase < 1) {
  finalPattern = pattern1 * (1 - blend) + pattern2 * blend;
} else if (mixPhase < 2) {
  finalPattern = pattern2 * (1 - blend) + pattern3 * blend;
} // ... add more conditions ...
else if (mixPhase < 5) {
  finalPattern = pattern5 * (1 - blend) + pattern6 * blend;
}
```

### Pattern Design Ideas

**Additive (Smooth)**
```javascript
const patternSmooth = ((t >> 1) + (t >> 3) + (t >> 5)) & 255;
```

**Multiplicative (Complex)**
```javascript
const patternComplex = (t * (t >> 4) * (t >> 8)) & 255;
```

**Modulo-based (Rhythmic)**
```javascript
const patternModulo = (t % 128 + (t >> 8) % 128) & 255;
```

**Conditional (Structured)**
```javascript
const patternConditional = (t & 128) ? (t << 1) & 255 : (t >> 1) & 255;
```

---

## Layer 3: Feedback Loop (Visual → Audio Influence)

### Current System: Pixel Sampling → Parameter Modulation

**Sampling strategy:** 12-20 points strategically distributed
- 4 corners (detect extreme brightness)
- 4 edge midpoints (detect edge activity)
- 4 diagonal sweeps (detect diagonal patterns)
- 4+ orbital scans (detect center/rotation)

**Conversion:**
```
RED channel        → Harmonic scaling, time modulation
GREEN channel      → Rhythm scaling, mix speed
BLUE channel       → Pattern bias, shift modulation
Brightness        → Intensity, chaos injection
Contrast          → Bit operations
Variance          → Chaos level
```

### Feedback Parameters Affected

```javascript
timeModulation:   How the time variable shifts (larger jumps = more chaotic)
shiftMod1/2:      XOR shift amounts (bigger shifts = less repetitive)
harmonicScale:    How many cycles the melody completes
rhythmScale:      Speed of rhythmic modulation
bitMod1/2:        Bit operation amounts (chaos injection)
mixSpeed:         How fast patterns cycle through
blendIntensity:   How smooth transitions are
chaosLevel:       XOR noise injection probability
colorMod (r,g,b): Color channel multipliers (affects visuals)
```

### Variation: Change What Pixels Affect

**Current: RGB brightness → Audio parameters**

**Alternative 1: Directional Gradient**
```javascript
// Sample top half vs bottom half
const topSamples = sampleRegion(0, 0, width, height/2);
const bottomSamples = sampleRegion(0, height/2, width, height);
const topBrightness = avgBrightness(topSamples);
const bottomBrightness = avgBrightness(bottomSamples);

feedback.mixSpeed = 0.5 + (topBrightness / 255) * 2;
feedback.chaosLevel = (bottomBrightness / 255);
```

**Alternative 2: Edge Detection**
```javascript
// High contrast areas → more complexity
const contrast = maxBrightness - minBrightness;
feedback.complexity = contrast / 255;
```

**Alternative 3: Color-Specific Regions**
```javascript
// Sample only red-dominant pixels
const redRegions = samples.filter(s => s.r > s.g && s.r > s.b);
feedback.intensity = redRegions.length / samples.length;
```

### Variation: Change Visual Effects from Audio

The piece also **paints bytebeat patterns** back to the screen:

**Current:**
```javascript
// For each pixel column:
const bytebeat = pattern(...);
const y = (bytebeat / 255) * screenHeight;
screen.pixels[y * width + x] = color;
```

**Alternative: Oscilloscope Mode**
```javascript
// Draw audio waveform like an oscilloscope
const samples = generator.bytebeat({ frequency, sampleRate, time, samplesNeeded: 512 });
for (let i = 0; i < samples.length; i++) {
  const y = (samples[i] * 0.5 + 0.5) * screenHeight;
  const x = (i / samples.length) * screenWidth;
  screen.pixels[Math.round(y * width + x)] = 255;
}
```

**Alternative: Spectrogram Mode**
```javascript
// Show frequency content over time
const frequencies = fft(bytebeat_output);
for (let freq = 0; freq < frequencies.length; freq++) {
  const brightness = frequencies[freq];
  const y = (freq / frequencies.length) * screenHeight;
  screen.pixels[Math.round(y * width + sweepX)] = brightness;
}
```

---

## Layer 4: Sequencing (Voice Lifecycle & Hold Mechanism)

### Current Architecture: Hold Sequence

**States:**
- **Off** - No automatic voices, only touch interaction
- **On** - Periodically spawns voices at orbital positions, 5-13 second durations

**Parameters:**
```javascript
spawnInterval:    2000ms (spawn every 2 seconds)
maxConcurrentHolds: 6 (never more than 6 at once)
baseDuration:     5000-13000ms (influenced by chaos feedback)
orbitSpeed:       0.0003-0.0006 rad/frame (varies per voice)
wobble:           0.15-0.35 (influenced by memory)
```

**Spawning logic:**
```
Position = orbital path (cosine × radius, sine × radius)
   Radius influenced by feedback.density
   Phase influenced by time + randomness
Duration = base + (1 - chaos) bonus - (1 - quiet bonus)
   Less chaos → longer holds
   High memory → longer holds
Movement = orbital drift + wobble
   Each voice has independent orbital speed
   Memory makes movements more pronounced
```

### Variation: Different Sequencing Strategies

**1. Fibonacci Interval Spawning**
```javascript
const goldenRatio = 1.618;
const intervals = [];
for (let i = 0; i < 10; i++) {
  intervals.push(Math.floor(1000 * Math.pow(goldenRatio, i)));
}
// Spawn voices at fibonacci-spaced intervals
```

**2. Grid-Based Spawning**
```javascript
// Spawn voices at fixed grid positions, one per cell
for (let gx = 0; gx < gridWidth; gx++) {
  for (let gy = 0; gy < gridHeight; gy++) {
    const x = (gx + 0.5) / gridWidth * screenWidth;
    const y = (gy + 0.5) / gridHeight * screenHeight;
    spawnVoiceAt(x, y, sound);
  }
}
```

**3. Random Walk Sequencing**
```javascript
// Each voice position is random walk from previous
const walk = { x: screenWidth * 0.5, y: screenHeight * 0.5 };
for (let i = 0; i < voiceCount; i++) {
  walk.x += (Math.random() - 0.5) * 200;
  walk.y += (Math.random() - 0.5) * 200;
  walk.x = clamp(walk.x, 0, screenWidth);
  walk.y = clamp(walk.y, 0, screenHeight);
  spawnVoiceAt(walk.x, walk.y, sound);
}
```

**4. Brightness-Following Sequencing**
```javascript
// Spawn voices at brightest regions of screen
const samples = samplePixels(screen, 20);
const sorted = samples.sort((a, b) => b.brightness - a.brightness);
sorted.slice(0, 5).forEach(sample => {
  spawnVoiceAt(sample.x, sample.y, sound);
});
```

**5. Phase-Locking to Audio**
```javascript
// Spawn new voices synchronized to audio beat
const audioEnergy = measureAudioEnergy(sound);
if (audioEnergy > threshold && (now - lastSpawn) > spawnDelay) {
  spawnHoldVoice(screenWidth, screenHeight, sound);
  lastSpawn = now;
}
```

---

## Remix Guide: Creating Variations

### Quick Swaps (30 minutes)

**1. Change the color palette**
- Modify `touchOverlayPalette` (line 14-23)
- Modify color generation in `paint()` (line 558-560)

**2. Change spatial mapping**
- Replace `mapXToFrequency()` and `mapYToPitchFactor()`
- E.g., use only vertical axis, or add diagonal

**3. Adjust hold sequence timing**
- Change `spawnInterval` (currently 2000ms)
- Change hold duration calculation (currently 5-13 seconds)
- Change max concurrent holds (currently 6)

**4. Modify feedback sensitivity**
- Increase/decrease pixel sampling points
- Change RGB→parameter mappings
- Adjust decay rates in `sim()`

---

### Medium Swaps (1-2 hours)

**1. Add a 6th bytebeat pattern**
- Design new pattern formula
- Insert into blending loop (change mod 5 to mod 6)
- Adjust blend transitions

**2. Implement alternative sequencing**
- Comment out `updateHoldVoices()`
- Write new spawning logic
- Re-export or call from `sim()`

**3. Change visual rendering**
- Modify pixel drawing (lines 509-598)
- Swap from vertical columns to orbits/grids/waveforms
- Add new visual effects (trails, particles, etc.)

**4. Implement new feedback strategy**
- Rewrite `samplePixelFeedback()` 
- Change what gets sampled (edges, variance, specific colors)
- Change RGB→parameter mappings

---

### Deep Remixes (3-6 hours)

**1. Multi-Layer Synthesis**
- Have different hold voices use different pattern sets
- E.g., lower voices use pattern 1-2, higher voices use 4-5

**2. Envelope Shaping**
- Add ADSR envelopes to voices
- Make volume/timbre evolve over hold duration

**3. Harmonic Relationships**
- Make voices respond to each other
- E.g., new voice spawned at harmonic of existing voices

**4. Spatial Audio Evolution**
- Make voices' frequency change as they move through space
- Create "force fields" where certain regions repel/attract

**5. Generative Visual System**
- Decouple visuals from audio synthesis
- Create independent generative visual patterns
- Use audio to modulate visual parameters

---

## Code Landmarks for Modification

### To understand a layer, read these functions:

**Spatial Mapping:**
- `mapXToFrequency()` (line 190)
- `mapYToPitchFactor()` (line 198)
- `deriveVoiceFrequency()` (line 205)

**Synthesis:**
- `generator.bytebeat()` (line 61)
- Pattern definitions (lines 82-97)
- Pattern blending (lines 100-126)

**Feedback:**
- `samplePixelFeedback()` (line 371)
- Sampling strategy (lines 379-424)
- Parameter derivation (lines 440-486)

**Sequencing:**
- `spawnHoldVoice()` (line 370)
- `updateHoldVoices()` (line 406)
- `toggleHoldSequence()` (line 457)
- Hold state initialization (line 40)

**Visuals:**
- `paint()` (line 525)
- Pixel rendering (lines 551-612)
- Color computation (lines 558-560)

---

## Conceptual Symmetries

Notice these patterns:

1. **Feedback flows upward**: Pixels → Audio → Pixels
2. **Time operates at multiple scales**:
   - Sample-level: Bytebeat generation (44.1kHz)
   - Voice-level: Hold durations (seconds)
   - System-level: State decay (10+ seconds)
3. **Randomness is constrained**: Random values modulated by feedback
4. **Movement is orbital**: Scanning, voice drift, visual sweeps all use trig functions
5. **Colors derive from bits**: RGB computed from bytebeat pattern XORs

These symmetries are **features** you can exploit in variations:
- Use same orbital math for voices and pixel sampling
- Use same bytebeat generators for audio and visuals
- Use same feedback parameters to shape multiple layers

---

## Testing Your Variations

When you remix, test these:

1. **With no touches** (hold sequence only)
   - Does it sustain audio continuously?
   - Are voices distinguishable or do they blend?
   - Does visual feedback remain varied?

2. **With touches** (interactive)
   - Do touch voices feel responsive?
   - Does hold sequence coexist peacefully?
   - Are there frequency collisions (too many same-pitch voices)?

3. **After 5 minutes idle**
   - Does it settle to silence or continue?
   - Do visuals accumulate wisely or become noise?

4. **After 90 minutes**
   - Would you listen to this as a tape?
   - Is there enough emergence/surprise?
   - Does it feel like a composition or just random?

---

## Example Variations to Try

### Variation A: "Comb Filter Seashells"
- Keep synthesis/feedback as-is
- Change `spawnInterval` to 500ms (faster)
- Spawn voices at fixed frequency ratios (1x, 1.5x, 2x, 3x fundamental)
- Result: Harmonic relationships, bell-like tones

### Variation B: "Noise Garden"
- Keep synthesis/feedback as-is
- Add 2-3 new chaotic bytebeat patterns
- Increase `chaosLevel` sensitivity 5x
- Result: More glitchy, algorithmic harshness

### Variation C: "Visual Instruments"
- Keep synthesis as-is
- Change visual rendering to oscilloscope
- Scale oscilloscope based on voice frequency
- High voices = small tight spirals, low voices = large loose ones
- Result: Visual becomes the primary interface, audio is secondary

### Variation D: "Memory Piece"
- Keep synthesis as-is
- Make spawn rate depend on accumulated visual memory
- Bright areas → more voices spawn nearby
- Result: Visuals "grow" audio in response

---

## Final Note

The beauty of this piece is that **every layer is independent**. You can:
- Change synthesis without touching sequencing
- Change sequencing without touching visuals
- Change feedback without touching synthesis
- Change mapping without touching anything else

This independence is intentional. It means you can remix safely, testing one change at a time, without breaking the whole system.

Happy remixing!
