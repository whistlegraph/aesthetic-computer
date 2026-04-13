# Seash Sliders — Lab Bench Control Panel

Five interactive sliders at the bottom of seash for real-time parameter adjustment.

---

## The Sliders

All sliders appear at the bottom of the screen and are touchable in real-time.

### 1. **Spawn** (Spawn Interval)
- **Range:** 500 ms — 8000 ms
- **Default:** 3000 ms (3 seconds)
- **What it does:** How often new voices are spawned in hold sequence
- **Try this:** 
  - Move left (500ms) = dense, overlapping voices
  - Move right (8000ms) = sparse, distinct phrases
  - Sweet spot: 2000-4000ms for continuous texture

### 2. **Duration** (Voice Duration)
- **Range:** 2000 ms — 20000 ms
- **Default:** 6000 ms (6 seconds)
- **What it does:** How long each voice sustains before fading
- **Try this:**
  - Left (2s) = staccato, quick pulses
  - Right (20s) = long held tones, pad-like
  - Quick changes: 2-4 seconds
  - Sustained changes: 10-15 seconds

### 3. **OrbitSpd** (Orbit Speed)
- **Range:** 0.0001 — 0.001 rad/frame
- **Default:** 0.0003 rad/frame
- **What it does:** How fast each voice drifts through frequency space
- **Try this:**
  - Left = slow, lazy movement
  - Right = fast, erratic movement
  - Controls the "wandering" behavior of auto voices

### 4. **BlendSpd** (Pattern Blend Speed)
- **Range:** 0.01 — 0.5
- **Default:** 0.1
- **What it does:** How fast patterns cross-fade into each other
- **Try this:**
  - Left (0.01) = smooth, gradual transitions
  - Right (0.5) = quick, snappy transitions
  - Affects overall timbre evolution

### 5. **MaxVoices** (Max Concurrent Auto Voices)
- **Range:** 1 — 10
- **Default:** 5 voices
- **What it does:** Maximum number of simultaneous auto-generated voices
- **Try this:**
  - 1 = single voice, monophonic
  - 3-5 = balanced polyphony
  - 8-10 = dense cluster

---

## How to Use

### Adjust a Slider
1. **Touch and drag** the slider handle (■) left/right
2. Value updates in real-time
3. Audio responds immediately
4. Release to stop dragging

### Read the Values
Each slider shows its current value to the right:
```
Spawn: ________■____  3000
        (value displayed on right)
```

### Combine Sliders for Effects

**Slow Pad:**
- Duration: 15000 (long holds)
- Spawn: 8000 (slow spawn)
- OrbitSpd: 0.0001 (minimal drift)
- BlendSpd: 0.01 (smooth transitions)

**Textural Chaos:**
- Duration: 3000 (quick pulses)
- Spawn: 500 (rapid spawning)
- OrbitSpd: 0.001 (fast movement)
- BlendSpd: 0.3 (snappy blending)

**Musical Melody:**
- Duration: 8000 (medium holds)
- Spawn: 4000 (structured spacing)
- OrbitSpd: 0.0003 (moderate drift)
- BlendSpd: 0.15 (balanced transitions)

**Dense Chords:**
- MaxVoices: 10 (many simultaneous)
- Duration: 10000 (held chords)
- Spawn: 2000 (continuous)
- OrbitSpd: 0.0001 (minimal movement)

---

## What's Happening Under the Hood

### Spawn Interval
Controls `params.spawnInterval`, used in `updateHoldVoices()`:
```javascript
if (now - lastSpawnTime > params.spawnInterval && count < maxVoices) {
  spawnHoldVoice(); // Create new voice
}
```

### Voice Duration
Controls `params.voiceDuration`, used when spawning:
```javascript
const duration = params.voiceDuration + (Math.random() - 0.5) * (duration * 0.5);
// Voice lasts this long before fading
```

### Orbit Speed
Controls `params.orbitSpeed`, used for voice position:
```javascript
hold.orbitPhase += params.orbitSpeed;
const x = Math.cos(hold.orbitPhase) * radius + center;
```

### Blend Speed
Controls `params.blendSpeed`, in the bytebeat generator:
```javascript
const mixPhase = (time * params.blendSpeed + freqScale * 0.5) % 2;
// Controls how fast patterns morph
```

### Max Voices
Controls `params.maxVoices`, checked during spawning:
```javascript
if (activeHolds.length < params.maxVoices) {
  spawnHoldVoice(); // Only spawn if under limit
}
```

---

## Slider Layout

```
Screen height:

[Main playing area - 80% of screen]
   - Frequency grid
   - Voice positions
   - Touch interaction

[Slider area - bottom 20% of screen]
   Spawn:     ___■_____  3000
   Duration:  ______■__  6000
   OrbitSpd:  _____■___  0.0003
   BlendSpd:  ____■____  0.1
   MaxVoices: _■_______  5
```

Sliders are fully interactive while you play — adjust in real-time.

---

## Tips for Experimentation

### 1. **Change One at a Time**
- Adjust Spawn, listen for 10 seconds
- Adjust Duration next, observe how it interacts
- This trains your intuition for each parameter

### 2. **Listen for Patterns**
- Try MaxVoices=1 (monophonic) with different spawn/duration combos
- Try MaxVoices=8 (dense) with same settings
- Notice how polyphony changes the effect

### 3. **Map the Space**
Create a mental map:
```
Slow, smooth          ←→ Fast, chaotic
(all sliders left)        (all sliders right)

Spacious, minimal     ←→ Dense, overlapping
(low spawn, max=1)        (high spawn, max=10)
```

### 4. **Use as Tape Control**
- Set parameters how you like
- Press H to start recording
- Let it run 30 minutes
- Sliders capture your intended "composition" in 5 parameters

### 5. **Keyboard + Slider Combo**
- H to toggle hold sequence
- Drag sliders while holding plays
- Create interactive performances

---

## Slider Precision

**Coarse adjustments:**
- Touch slider, drag all the way left or right
- Jumps to min/max

**Fine adjustments:**
- Touch slider and drag slowly
- Values update frame-by-frame as you drag

**Quantized sliders:**
- MaxVoices: steps of 1 (always integer)
- Others: continuous (floating point)

---

## Getting the Values Right

### For 90-Minute Tape
- **Spawn:** 3000-5000 (not too dense)
- **Duration:** 6000-10000 (medium holds)
- **OrbitSpd:** 0.0002-0.0004 (gentle movement)
- **BlendSpd:** 0.1-0.2 (smooth but alive)
- **MaxVoices:** 4-6 (polyphonic without muddy)

### For Interactive Play
- **Spawn:** Don't matter (you're creating voices)
- **Duration/OrbitSpd:** Personal taste
- **BlendSpd:** 0.15 for snappy response
- **MaxVoices:** 6-8 (more bandwidth for your touches)

### For Study
- **Spawn:** 3000 (baseline)
- **Duration:** 6000 (baseline)
- **OrbitSpd:** 0.0003 (baseline)
- **BlendSpd:** 0.1 (baseline)
- **MaxVoices:** 5 (baseline)

Then change ONE, listen, understand, reset, repeat.

---

## Not Working?

**Slider doesn't respond:**
- Make sure you're touching on the blue slider line, not the label
- Drag horizontally (not vertically)
- Release and try again

**Values not changing:**
- Confirm you're in seash (not seashells)
- Check that hold sequence is ON (press H)
- Values change immediately; listen for audio change

**Want to reset:**
- Reload the page (F5)
- Or manually drag each slider back to default
- No "reset" button needed

---

## Next: Combine with Touch Play

You can:
1. Set sliders to your preferred hold sequence settings
2. **Also** touch the screen to add manual voices
3. Manual touches + auto holds mix together
4. Create layered performances

The sliders control the "background" (auto voices), your touches are the "foreground" (interactive layer).

---

## Example Sessions

### Session A: Deep Listen (90 minutes)
```
Spawn: 4000      (slower spawn)
Duration: 8000   (longer holds)
OrbitSpd: 0.0002 (minimal drift)
BlendSpd: 0.08   (smooth)
MaxVoices: 4     (sparse chords)

Result: Meditative, slowly evolving texture
```

### Session B: Interactive Play (30 minutes)
```
Spawn: 3000
Duration: 6000
OrbitSpd: 0.0003
BlendSpd: 0.15
MaxVoices: 5

Press H, then also make touches
Auto voices + your gestures = rich conversation
```

### Session C: Chaos Lab (15 minutes)
```
Spawn: 800       (rapid!)
Duration: 3000   (quick bursts)
OrbitSpd: 0.0008 (fast movement)
BlendSpd: 0.3    (snappy blends)
MaxVoices: 8     (dense!)

Result: Glitchy, algorithmic texture
```

---

Made for experimentation. Sliders = direct feedback loop between your intention and the sound.
