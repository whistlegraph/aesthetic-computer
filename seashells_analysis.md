# Seashells.mjs Analysis

## Overview
**Seashells** is a bytebeat algorithmic synthesizer with visual feedback. It's designed as an interactive piece where touch positions generate audio, but converting it to autoplay requires significant architectural changes.

---

## Core Audio Mechanism

### Bytebeat Synthesis
The piece uses **5 algorithmic patterns** that blend over time:

1. **Pattern 1 (XOR Cascade)** - Crisp, digital texture
   ```
   (t ^ (t >> (8 + shiftMod1)) ^ (t >> (9 + shiftMod2))) & 255
   ```
   Classic bytebeat - creates sharp, glitchy sounds

2. **Pattern 2 (Melodic Stepped)** - Harmonic content
   ```
   ((t * harmonic) & (t >> (5 + bitMod1)) | (t >> (4 + bitMod2))) & 255
   ```
   Responsive to frequency scaling

3. **Pattern 3 (Rhythmic)** - Complex polyrhythmic patterns
   ```
   (t | (t >> rhythmMod | t >> 7)) * (t & (t >> 11 | t >> complexMod)) & 255
   ```
   Highly sensitive to frequency and feedback

4. **Pattern 4 (Sierpinski-like)** - Fractal patterns
   ```
   (t & (t >> (5 + sierpinskiMod) | t >> 8)) & 255
   ```
   Creates algorithmic complexity

5. **Pattern 5 (Frequency-responsive Melodic)**
   ```
   ((t * melodyScale) ^ (t >> 6)) & (t >> 8) & 255
   ```
   Strong pitch sensitivity

### Pattern Blending
- Patterns cycle through 4-phase blend states (0→1→2→3→0)
- Blend speed is modifiable by feedback (`mixSpeed`)
- Blending is smooth and continuous (`blendIntensity` 0.3-1.0)

### Feedback System (Audio ↔ Visual Loop)
The piece samples pixels from the screen and converts them into audio modulation parameters:

**Sampling Strategy:**
- 4 corner samples
- 4 edge samples (mid-points of each edge)
- 4 diagonal sweeps
- 4+ orbital scanning samples (elliptical patterns that move with interaction memory)

**Conversion to Audio Parameters:**
```
Red channel    → timeModulation, harmonicScale, colorMod.r
Green channel  → rhythmScale, mixSpeed, colorMod.g
Blue channel   → shiftMod2, patternBias, colorMod.b
Contrast       → bitMod values (higher contrast = more bit operations)
Variance       → chaosLevel (pixel unpredictability → audio chaos)
```

**Chaos Injection:**
```javascript
if (feedback.chaosLevel > 0.5) {
  finalPattern = finalPattern ^ Math.floor(feedback.chaosLevel * 128);
}
```

---

## Visual Generation

### Pixel Rendering
- **No wipe()** - pixels accumulate, creating permanent trails
- Bytebeat values map directly to Y positions
- Colors computed from bit patterns AND feedback color mods
- Additive blending for accumulation effects

### Visual Elements
1. **Main column visualization** - One vertical line per X pixel, height = bytebeat value
2. **Bit pattern layers** - Each bit of the bytebeat value adds horizontal bands
3. **Vertical streaks** - Frequency-responsive vertical lines (every Nth column)
4. **Horizontal sweep** - Time-based horizontal scan line that moves down the screen

### Interaction Visualization
- Touch overlays show active voices with colored circles
- Frequency label for each touch (Hz)
- Grid showing frequency/pitch mapping
- Help text when idle

---

## Interaction State (The "Memory" System)

The piece maintains persistent modulation state that decays over time:

```javascript
interactionState = {
  scanOffset:    0,      // Orbital scan phase
  scanVelocity:  0.003,  // How fast it scans
  scanSpread:    1.0,    // Vertical spread of scan
  orbit:         0,      // Cumulative rotation bias
  memory:        0,      // Persistent touch "memory" (0-1)
  chaosBias:     0,      // How chaotic it gets
  density:       1.0,    // Sampling density
  lastTouchAt:   0       // Timestamp of last interaction
}
```

**How Touch Influences State:**
```javascript
scanOffset += (nx * 0.11 + ny * 0.07)  // Touch moves scan
orbit += (nx - 0.5) * 0.18             // Horizontal bias → rotation
scanSpread *= 0.9; scanSpread += (0.65 + ny) * 0.1  // Vertical → spread
memory *= 0.94; memory += 0.05 + |nx - 0.5| * 0.08 // Accumulates
chaosBias *= 0.9; chaosBias += |nx - 0.5| * 0.25   // Edges → chaos
```

**Decay Over Time:**
- Memory decays at 0.998/frame if touched recently, 0.992 if idle
- Chaos decays at 0.997 (active) / 0.985 (idle)
- Orbit decays at 0.992
- After ~10 seconds idle: memory → 0, piece quiets down

---

## Current Limitations for 90-Minute Tape

### ❌ Problems
1. **No autoplay** - Requires manual touches to generate audio
2. **Silent when idle** - Help screen displays when no voices active
3. **Limited generative richness** - Only 8 simultaneous voices, all driven by touch
4. **Accumulation without clearing** - Visual system will eventually fill with noise
5. **No time-based voice generation** - No procedural voice triggering
6. **Memory decay** - State fades to silence after ~10 seconds of inactivity

### ✅ Strengths (Why It Could Work)
1. **High algorithmic complexity** - 5 blending patterns × feedback × chaos injection = very large parameter space
2. **Feedback loop creates emergence** - Visual patterns influence audio, creating unpredictable evolution
3. **Deterministic** - Same pixel patterns always produce same audio (reproducible tape)
4. **Minimal repetition** - Bytebeat patterns are subtle and shift continuously via blending
5. **Scaling** - Can handle more simultaneous voices than needed (currently capped at 8)

---

## Required Changes for Autoplay

### Option 1: Procedural Voice Generation (Recommended)

**Add time-based voice triggering in `sim()`:**

```javascript
function sim({ sound, hud, screen }) {
  // ... existing code ...
  
  // Procedural voice generation
  const voiceTargetCount = Math.round(1 + sharedPixelFeedback.density * 4);
  const currentVoiceCount = totalVoiceCount();
  
  if (currentVoiceCount < voiceTargetCount && performance.now() - interactionState.lastAutoVoiceAt > 300) {
    // Add voice at pseudo-random "musical" position
    const nextX = (interactionState.autoVoicePhase * screen.width) % screen.width;
    const nextY = (Math.sin(performance.now() * 0.0003) * 0.5 + 0.5) * screen.height;
    
    startTouchVoice({
      pointerIndex: 8 + currentVoiceCount,  // Use high indices for auto voices
      x: nextX,
      y: nextY,
      screenWidth: screen.width,
      screenHeight: screen.height,
      sound
    });
    
    interactionState.lastAutoVoiceAt = performance.now();
    interactionState.autoVoicePhase = (interactionState.autoVoicePhase + 0.31) % 1;  // Golden ratio
  }
  
  // Age out auto-voices slowly (don't kill, just quiet)
  // This creates natural voice turnover instead of jumping in/out
}
```

**Adjustments needed:**
- Increase `maxTouchPointers` from 8 to ~20-30 for more voices
- Add `lastAutoVoiceAt` and `autoVoicePhase` to `interactionState`
- Modify voice volume calculation to account for mix of auto/touch voices

### Option 2: "Hold" Mode (Simpler, More Controlled)

**Add a single "master" voice that holds until changed:**

```javascript
let holdState = {
  x: null,
  y: null,
  holdUntil: 0,
  nextChangeAt: 0
};

function act({ event: e, sound, screen, pens }) {
  // Existing touch handling...
  
  if (e.is("keyboard:down:h")) {
    // Toggle hold mode
    if (holdState.x === null) {
      // Start holding at a specific position
      holdState.x = screen.width * 0.5;
      holdState.y = screen.height * 0.5;
      holdState.holdUntil = performance.now() + 5000;  // Hold for 5 sec
      startTouchVoice({
        pointerIndex: 99,  // Special hold voice
        x: holdState.x,
        y: holdState.y,
        screenWidth: screen.width,
        screenHeight: screen.height,
        sound
      });
    } else {
      stopTouchVoice(99);
      holdState.x = null;
    }
  }
}

function sim({ sound, hud, screen }) {
  // Auto-release hold if time expired
  if (holdState.x !== null && performance.now() > holdState.holdUntil) {
    stopTouchVoice(99);
    holdState.x = null;
  }
  
  // Or: continuously update hold position based on pixel feedback
  if (holdState.x !== null) {
    const feedback = sharedPixelFeedback;
    holdState.x = (holdState.x + feedback.patternBias * 0.5) % screen.width;
    holdState.y = (holdState.y + feedback.timeModulation * 0.0001) % screen.height;
    updateTouchVoice({
      pointerIndex: 99,
      x: holdState.x,
      y: holdState.y,
      screenWidth: screen.width,
      screenHeight: screen.height,
      sound
    });
  }
}
```

### Option 3: Hybrid (Best for Tape)

Combine procedural generation + controlled hold positions:
- Auto-voices spawn at intervals determined by pixel feedback
- Each voice holds for variable duration (3-15 seconds)
- Hold positions follow orbital patterns (music-like phrasing)
- User can still manually intervene

---

## Viability for 90 Minutes

### Without Changes
**⚠️ Not viable** - Needs manual interaction, would result in 90 minutes of silence + random touches

### With Procedural Voices
**✅ Viable** - Could sustain audio, but:
- Voices may cluster in same regions without spatial variation
- Without user interaction, state may converge to stable patterns
- Visual accumulation could become monolithic

### With Hold Mode + Orbital Sequencing
**✅ Very viable** - Could create:
- Phrased movements (voices move through parameter space)
- Natural emergence from pixel feedback
- Balance between predictability and surprise
- Tape-like "performance" quality

### Recommended Hybrid Approach

1. **Keep current touch system** for interactivity
2. **Add procedural voice spawning** that's influenced by feedback
3. **Add orbital "hold" sequences** that create musical phrasing
4. **Slowly wipe screen** (every 30-60 seconds) to prevent visual noise accumulation
5. **Map feedback more musically** - e.g., high variance → more voices, high brightness → faster tempo

Example voice spawning pattern:
```javascript
// Spawn voices at orbital positions, Fibonacci intervals
const goldenRatio = 1.618;
const nextSpawn = Math.floor(baseInterval * Math.pow(goldenRatio, currentSpawnIndex));
const orbitPhase = (performance.now() * 0.0001 + currentSpawnIndex * 0.31) % (Math.PI * 2);
const x = (Math.cos(orbitPhase) * 0.4 + 0.5) * screen.width;
const y = (Math.sin(orbitPhase) * 0.4 + 0.5) * screen.height;
```

---

## Memory & Emergence

The **key strength** is that visual state influences audio via feedback sampling:

1. Pixels accumulate → visual patterns become complex
2. Complex visuals → chaotic feedback parameters
3. Chaotic feedback → audio becomes more generative
4. Audio via painting → new visual patterns
5. Loop → increasing complexity over 90 minutes

This is **genuine emergence**, not repetition. A 90-minute tape would document the system's exploration of its parameter space, gradually finding new combinations.

---

## Suggested Implementation Priority

If building autoplay version:

1. **First** - Add slow screen wipe (every 45 sec) to prevent accumulation
2. **Second** - Add procedural voice spawning based on pixel variance
3. **Third** - Implement hold sequences (3-15 second voice holds at orbital positions)
4. **Fourth** - Map feedback more musically (high-brightness → voice clusters, etc.)
5. **Optional** - Add keyboard shortcuts for manual phase control (reset wipe, trigger voices, etc.)

---

## Code Entry Points to Modify

- `sim()` (line 696) - Add voice generation logic
- `interactionState` (line 25) - Add autoplay-specific state
- `act()` (line 625) - Add keyboard controls for autoplay
- `paint()` (line 490) - Add conditional wipe logic

Would preserve all existing touch/visual mechanics while enabling tape-like continuous playback.
