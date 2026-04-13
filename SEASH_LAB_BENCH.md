# Seash — Lab Bench Proof of Concept

A minimal, readable bytebeat synthesizer. ~350 lines of clear code. Perfect for understanding the mechanism and experimenting.

---

## What It Does

**Seash** is the distilled core of Seashells:
- Generate sound using bytebeat (simple mathematical synthesis)
- Auto-spawn voices on a loop (hold sequence)
- Play interactively by touching the screen
- Press H to toggle automatic playback

No complex feedback loops. No dense visuals. Just the mechanism.

---

## How It Works (Conceptual)

### Layer 1: Sound Generation (Bytebeat)

**The core idea:** Audio from bit operations on integers.

```javascript
// Two patterns that blend over time
pattern1 = (t ^ (t >> 8) ^ (t >> 9)) & 255         // XOR: crisp, digital
pattern2 = ((t * harmonic) & (t >> 5) | (t >> 4)) & 255  // Melodic: pitched

// Mix them based on time
finalPattern = pattern1 * (1 - blend) + pattern2 * blend
```

**Why it works:** Integer operations are deterministic. Same input → same output. But small changes in parameters create wildly different sounds.

**Key insight:** The two patterns **never collide**. When one fades out, the other fades in. This creates a continuous, evolving texture.

---

### Layer 2: Spatial Control (Touch → Frequency)

**How position maps to sound:**

```javascript
// X-axis (left to right)
X = 0%    →  55 Hz   (very low)
X = 50%   → 220 Hz   (middle)
X = 100%  → 880 Hz   (high)
// Logarithmic scale (musically natural)

// Y-axis (top to bottom)
Y = 0%    →  2.0x pitch multiplier  (octave up)
Y = 50%   →  1.0x pitch multiplier  (normal)
Y = 100%  →  0.5x pitch multiplier  (octave down)
```

**Result:** Any (X, Y) position has a unique frequency. Move around = explore the frequency space.

**Why logarithmic:** Our ears perceive pitch logarithmically. A 2x frequency jump feels the same from 100Hz→200Hz as from 1000Hz→2000Hz.

---

### Layer 3: Multi-Voice Management

**The system tracks multiple simultaneous voices:**

```javascript
const touchVoices = new Map()  // { pointerIndex → { sound, frequency, x, y } }
```

**When you touch:**
1. New voice spawns at that position
2. Frequency derived from X/Y
3. Voice added to map with unique pointer ID

**When you move your finger:**
1. Voice position updates
2. Frequency updates smoothly (with lerp to avoid jumps)

**When you lift:**
1. Voice fades out (0.08 second fade)
2. Removed from map

**Volume balancing:** If you have N fingers down, each voice volume = 0.5 / sqrt(N)
- 1 voice: 0.5 volume
- 4 voices: 0.25 each (stays reasonable)
- 16 voices: 0.125 each

---

### Layer 4: Auto-Voice Generation (Hold Sequence)

**The magic mechanism:**

```javascript
if (holdSequence.enabled) {
  // Every 3 seconds, spawn a new voice
  spawnHoldVoice()
  
  // Each voice:
  // - Starts at orbital position
  // - Drifts along orbit
  // - Lasts 6±1.5 seconds
  // - Fades out when duration expires
  
  // Result: continuous audio, voices never sync
}
```

**Why it works for 90 minutes:**
- New voice spawns before old one dies
- Each has different orbit speed (unique drifting path)
- No two voices follow same trajectory
- Even though only 2 patterns, constant variation

**Why voices don't sync:**
- Orbit speeds differ by small amounts (0.0003–0.0005 rad/frame)
- After 30 seconds, they're all at different phases
- Mathematical: irrational multiples → infinite non-repetition

---

## The Code (Line by Line)

### Sections

**Lines 1–20:** Constants and initialization
- `touchVoices`: Map of active voices
- `holdSequence`: State for auto-generation

**Lines 22–64:** Bytebeat generator
- Takes frequency, time, and sample count
- Returns 512 audio samples
- Blends 2 patterns smoothly

**Lines 66–80:** Utility functions
- `clamp()`: Constrain values to range
- `mapXToFrequency()`: X pixel → Hz (logarithmic)
- `mapYToPitch()`: Y pixel → pitch factor
- `deriveFrequency()`: Combine X+Y

**Lines 82–130:** Voice lifecycle
- `createVoice()`: Initialize synthesizer
- `startTouchVoice()`: New voice from touch
- `updateTouchVoice()`: Move existing voice
- `stopTouchVoice()`: Fade and remove
- `rebalanceVolumes()`: Keep mix balanced

**Lines 132–175:** Hold sequence
- `spawnHoldVoice()`: Create auto voice at orbital position
- `updateHoldVoices()`: Move orbits, spawn new, fade old
- `toggleHoldSequence()`: Start/stop auto mode

**Lines 177–210:** Rendering
- `paint()`: Draw grid, labels, voice positions
- Minimal UI: just frequency readouts and status

**Lines 212–247:** Input handling
- `act()`: Touch and keyboard events
- Support 8 simultaneous touches
- H key toggles hold sequence

**Lines 249–253:** Per-frame updates
- `sim()`: Audio polling, hold sequence updates

---

## How to Use It

### Interactive Mode
```
Touch screen → voice spawns at that position
Move finger → frequency changes in real-time
Lift finger → voice fades out

Multi-touch: 8 fingers at once, each with own voice
```

### Autoplay (Tape Mode)
```
Press H → hold sequence starts
Voices spawn automatically every 3 seconds
Each holds for 6±1.5 seconds
Press H again → stops

Let run for 90 minutes → listen to emergence
```

### Observe the Mechanism
```
Watch voice numbers increase and decrease
Notice frequency labels updating as voices move
See hold sequence spawn new voice before old fades
Listen to how patterns blend smoothly
```

---

## Why It's a Good Lab Bench

### 1. **Readable**
- ~350 lines (vs 800+ for seashells.mjs)
- No visual feedback loop
- No complex state decay
- Direct cause-effect

### 2. **Modifiable**
Each section is independent:

**Change sound:** Edit `generator.bytebeat()` (line 22)
- Add pattern 3: just write another formula
- Integrate it into blending

**Change spatial mapping:** Edit `mapXToFrequency()` (line 69)
- Try linear instead of logarithmic
- Try quantized to specific notes
- Try polar coordinates

**Change auto-generation:** Edit `spawnHoldVoice()` (line 135)
- Different spawn positions (grid, random, bounded)
- Different orbit speeds
- Different durations

**Change UI:** Edit `paint()` (line 179)
- Remove grid, add oscilloscope
- Add different text labels
- Show orbit paths visually

### 3. **Testable**
- Add `console.log()` anywhere to debug
- Change one variable, test immediately
- No side effects (each voice is independent)

### 4. **Minimal Dependencies**
- Only uses AC's `sound.synth()` API
- No external libraries
- No complex state machines

---

## Experiments to Try

### Experiment 1: Change the Patterns
**Goal:** Make it sound more chaotic

```javascript
// In generator.bytebeat(), change p1:
const p1 = (t * t) & (t >> 4) & 255;  // Multiplicative instead of XOR

// Test: Press H, listen to how it differs
```

### Experiment 2: Change Frequency Range
**Goal:** Make it higher or lower pitched

```javascript
// In mapXToFrequency():
const minHz = 110;   // Was 55 (raise minimum)
const maxHz = 440;   // Was 880 (lower maximum)

// Test: Press H, notice narrower frequency range
```

### Experiment 3: Change Auto-Spawn Rate
**Goal:** More or fewer voices

```javascript
// In holdSequence:
spawnInterval: 1500,  // Was 3000 (spawn every 1.5 sec instead of 3)

// Test: More voices, denser texture
```

### Experiment 4: Change Voice Lifespan
**Goal:** Longer or shorter holds

```javascript
// In spawnHoldVoice():
const duration = 15000 + (Math.random() - 0.5) * 5000;  // Was 6000 ± 1500

// Test: Slower, more meditative tape
```

### Experiment 5: Add a Third Pattern
**Goal:** More timbral variety

```javascript
// In generator.bytebeat(), add:
const p3 = ((t >> 2) + (t >> 5)) & 255;  // Additive pattern

// Modify blending to include it:
let mixPhase = (time * 0.1 + freqScale * 0.5) % 3;  // Was % 2
if (mixPhase < 1) { /* p1 to p2 */ }
else if (mixPhase < 2) { /* p2 to p3 */ }
else { /* p3 to p1 */ }

// Test: Three-way blend creates richer texture
```

---

## What's Removed (vs Seashells)

### Removed:
- **Feedback loop** (pixels don't influence audio)
- **5 patterns** (reduced to 2 for clarity)
- **Complex UI** (minimal text only)
- **Grid visualization** (just dots)
- **State decay system** (removed unnecessary complexity)
- **Chaos injection** (simplified synthesis)
- **Harmonic Bell quantization** (bare version uses full spectrum)

### Kept:
- **Core bytebeat synthesis**
- **Spatial frequency mapping**
- **Multi-voice management**
- **Hold sequence (auto-generation)**
- **Smooth blending between patterns**

---

## Performance Notes

**CPU usage:** Very light
- 2 patterns instead of 5
- No pixel sampling
- No feedback loop
- Simple rendering

**Audio quality:** 8-bit bytebeat character (lo-fi by design)

**Latency:** Minimal — direct synthesis, no heavy processing

---

## Next Steps

### To Understand Deeper:
1. Run it and experiment with touches
2. Press H and listen for 5 minutes
3. Add `console.log()` to understand timing
4. Modify one parameter at a time

### To Build Variations:
1. Copy seash.mjs to seashell_variation.mjs
2. Make ONE change
3. Test immediately
4. Document what changed and why
5. Stack changes incrementally

### To Connect to Seashells:
Once you understand seashell:
1. Read seashells_conceptual_model.md
2. Understand how feedback loop works
3. Add feedback sampling to seashell
4. You've now built seashells from scratch!

---

## Philosophy

**Seashell** embodies these principles:

1. **Clarity over cleverness** — Every line should make sense
2. **Mechanism over magic** — You can trace cause-effect
3. **Minimal abstraction** — One function per concept
4. **Modular independence** — Change one layer, others stay stable
5. **Testability** — Add print statements, change values, listen

Use it as a **thinking tool**, not just a sound generator.

---

## Quick Reference

| What | Where | How |
|------|-------|-----|
| Change sound | `generator.bytebeat()` line 22 | Modify pattern formulas |
| Change frequency range | `mapXToFrequency()` line 69 | Edit minHz/maxHz |
| Change pitch modulation | `mapYToPitch()` line 75 | Edit exponent (2) |
| Change auto-spawn timing | `holdSequence.spawnInterval` line 24 | Edit milliseconds |
| Change voice duration | `spawnHoldVoice()` line 149 | Edit duration calculation |
| Change voice count limit | `updateHoldVoices()` line 161 | Edit `.length < 5` |
| Change rendering | `paint()` line 179 | Modify drawing code |
| Change audio response | `sim()` line 245 | Edit voice update logic |

---

## Files to Compare

- **seash.mjs** (this one) — bare bones, ~350 lines
- **seashells.mjs** — full version, ~900 lines, with feedback
- **seashells_conceptual_model.md** — 4-layer architecture explanation

You can learn by:
1. Understanding seashell completely
2. Reading conceptual_model.md
3. Comparing against seashells.mjs to see what each layer adds

Good lab bench = good foundation for understanding the full piece.
