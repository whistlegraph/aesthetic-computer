# Seashells.mjs: Complete Documentation

A comprehensive guide to understanding, using, and remixing **seashells.mjs** — a bytebeat algorithmic synthesizer with visual feedback.

---

## What Is Seashells?

**Seashells** is an interactive generative music instrument that:

1. **Synthesizes audio** using bytebeat (mathematical bit operations)
2. **Visualizes in real-time** by painting bytebeat patterns to the screen
3. **Feeds back visuals → audio** by sampling pixels and using them to modulate synthesis
4. **Sustains via hold sequences** — can play autonomously for 90+ minutes

It's designed as a tape-like generative system: set it running and watch/listen to emergence unfold.

---

## Quick Start

### Launching
```bash
npm run ac           # Start dev server
# Navigate to seashells in your browser
```

### Interactive Play
- **Touch anywhere** to start a voice at that position
  - X-axis = base frequency (left=low, right=high)
  - Y-axis = pitch multiplier (top=high, bottom=low)
- **Drag** to change frequency/pitch in real-time
- **Lift** to stop that voice

### Hold Sequence (Autoplay)
- Press **H** to start autonomous voice generation
- Voices spawn at orbital positions every 2 seconds
- Each voice holds for 5-13 seconds
- New voices spawn before old ones fade → continuous audio
- Press **H** again to stop

### For 90-Minute Tape
- Press **H** at start of session
- Let it run unattended
- The system will sustain audio continuously
- Visual feedback creates ongoing emergence

---

## Files in This Documentation

| File | Purpose |
|------|---------|
| `seashells_analysis.md` | Technical breakdown of how each component works |
| `seashells_conceptual_model.md` | High-level architecture, 4-layer model, remix framework |
| `seashells_variation_examples.md` | 5 concrete remix examples with copy-paste code |
| `SEASHELLS_README.md` | This file — quick reference |

**Read in this order:**
1. This README (5 min)
2. Conceptual Model (20 min) — understand the 4 layers
3. Analysis (30 min) — deep dive into each layer
4. Variation Examples (60 min) — try specific remixes

---

## The 4-Layer Architecture

```
┌────────────────────────────────────────────────────┐
│ Layer 4: SEQUENCING                                │
│ How voices spawn, sustain, and move                │
│ (Hold mechanism, orbital paths, voice lifecycle)   │
├────────────────────────────────────────────────────┤
│ Layer 3: FEEDBACK LOOP                             │
│ Audio ↔ Visual feedback                            │
│ (Sample pixels, convert to audio modulation)       │
├────────────────────────────────────────────────────┤
│ Layer 2: SYNTHESIS                                 │
│ Audio generation                                   │
│ (5 bytebeat patterns, blending, modulation)        │
├────────────────────────────────────────────────────┤
│ Layer 1: SPATIAL MAPPING                           │
│ Touch position → Audio parameters                  │
│ (X→frequency, Y→pitch factor)                      │
└────────────────────────────────────────────────────┘
```

**Key insight:** Each layer is independent. Change one without breaking others.

---

## What Makes It Work for 90 Minutes?

1. **Algorithmic Complexity**
   - 5 blending bytebeat patterns (not 1)
   - Each pattern responds to 10+ modulation parameters
   - Feedback loop creates continuous variation

2. **Visual-Audio Feedback**
   - Audio paints pixels
   - Pixels influence audio via feedback
   - Creates genuine emergence, not repetition

3. **Voice Continuity**
   - Hold sequences spawn new voices before old ones fade
   - Ensures unbroken audio stream
   - Voices never synchronize (different speeds)

4. **State Decay**
   - Interaction memory decays over 10 seconds
   - Creates slow drift in parameters
   - System explores new regions of parameter space

---

## Made a Change? Test It Like This

### 1-Minute Test
```
Press H → listen for 60 seconds
Does audio continue without gaps?
Does timbre vary or is it repetitive?
```

### 5-Minute Test
```
Press H → listen for 5 minutes
Do patterns feel structured or random?
Is visual complexity growing or settling?
Any obvious repetition loops?
```

### Tape Test (90 minutes)
```
Press H → start recording
Leave running (no interaction)
Come back after 90 minutes
Listen back: would you put this on cassette?
```

**Tape quality checklist:**
- [ ] Audio never drops out (voices always sustain)
- [ ] Timbre evolves (5+ distinct characters over 90 min)
- [ ] No obvious repetition (don't hear same sequence twice)
- [ ] Visual patterns remain interesting (not just noise)
- [ ] Rhythm/pacing feels intentional, not random

---

## Remix Quick Reference

### Change Spatial Mapping (10 min)
- File: `seashells.mjs`
- Functions: `mapXToFrequency()`, `mapYToPitchFactor()`
- Examples: quantized scale, polar coords, grid snapping

### Change Synthesis Patterns (30 min)
- File: `seashells.mjs`
- Location: `generator.bytebeat()` (line 61)
- Task: Add new pattern, integrate into blending

### Change Feedback (30 min)
- File: `seashells.mjs`
- Function: `samplePixelFeedback()` (line 371)
- Task: Change what pixels are sampled, how they map to audio

### Change Sequencing (30 min)
- File: `seashells.mjs`
- Functions: `spawnHoldVoice()`, `updateHoldVoices()`
- Task: Alter spawn timing, positions, movement patterns

### Change Visuals (1 hour)
- File: `seashells.mjs`
- Function: `paint()` (line 525)
- Task: Different rendering (oscilloscope, spectrogram, particles)

---

## Common Remix Patterns

### "Make It Musical"
- Quantize X-axis to specific scale (pentatonic, chromatic)
- Use grid-based sequencing instead of orbital
- Reduce chaos injection
- Result: Harmonic, bell-like, more consonant

### "Make It Chaotic"
- Increase feedback sensitivity to brightness/variance
- Add more chaos injection
- Increase pattern blending speed
- Result: Glitchy, algorithmic, harsh

### "Make It Visual"
- Replace pixel column visualization with oscilloscope
- Add particles, trails, or fractal rendering
- Sync visual updates to audio beats
- Result: Visuals are primary, audio is secondary

### "Make It Spacious"
- Reduce concurrent voices (max 3-4 instead of 6)
- Increase hold durations (10-30 seconds instead of 5-13)
- Reduce spawn rate (every 5-10 seconds instead of 2)
- Result: Sparse, contemplative, room to breathe

### "Make It Dense"
- Increase concurrent voices (15-20 instead of 6)
- Decrease hold durations (2-5 seconds instead of 5-13)
- Increase spawn rate (every 1 second)
- Result: Dense, layered, orchestral

---

## Performance Notes

### If Synthesis Is CPU-Heavy
- Reduce waveform sample count (currently 512)
- Reduce pixel feedback sampling points (currently 12-20)
- Profile in DevTools to find bottleneck

### If Visuals Fill with Noise
- Add slow screen wipe: `if (now % 45000 < 1000) wipe(0,0,0)`
- Reduce additive blending intensity
- Use opacity/fade instead of accumulation

### If Hold Sequence Is Uneven
- Increase spawn interval (>2000ms)
- Reduce max concurrent voices (to 3-4)
- Make durations more consistent (reduce randomness)

---

## Conceptual Symmetries (Design Patterns)

These patterns appear throughout the code — exploit them:

1. **Orbital Math** — Scanning, voice movement, visual sweeps all use cos/sin
   - Use same orbit equations everywhere for coherence

2. **Feedback Parameters** — Audio parameters match visual feedback sources
   - High brightness → intensity
   - High variance → chaos
   - Exploit this for intuitive relationships

3. **Time Scales**
   - Sample-level: 44.1 kHz (bytebeat)
   - Voice-level: 1-20 seconds (hold durations)
   - System-level: 10+ seconds (state decay)
   - Design remixes that respect these scales

4. **Randomness** — Always constrained by feedback
   - Voice spawn positions: random + orbital structure
   - Hold durations: random ± base duration
   - Pattern mixing: time-based + feedback bias
   - Never pure noise, always quasi-musical

---

## Key Files & Functions

### Core Synthesis
- Line 61: `generator.bytebeat()` — The heart of audio generation
- Lines 82-97: Pattern definitions
- Lines 100-126: Pattern blending logic

### Feedback
- Line 371: `samplePixelFeedback()` — Pixel → audio conversion
- Lines 379-424: Sampling strategy
- Lines 440-486: RGB → audio parameter mapping

### Sequencing
- Line 40: `holdSequence` object initialization
- Line 370: `spawnHoldVoice()` — Create new voice
- Line 406: `updateHoldVoices()` — Update positions & durations
- Line 457: `toggleHoldSequence()` — Start/stop autoplay

### Spatial Mapping
- Line 190: `mapXToFrequency()` — X pixel → Hz
- Line 198: `mapYToPitchFactor()` — Y pixel → pitch multiplier
- Line 205: `deriveVoiceFrequency()` — Combine into final frequency

### Visuals
- Line 525: `paint()` — Main rendering function
- Lines 551-612: Pixel drawing logic
- Line 210: `drawTouchMapping()` — Grid visualization

### Interaction
- Line 696: `sim()` — Per-frame updates
- Line 715: `act()` — Event handling (touch, keyboard)

---

## Next Steps

### To Understand Seashells
1. Read `seashells_conceptual_model.md` (understand 4 layers)
2. Read `seashells_analysis.md` (deep technical)
3. Try pressing H, making touches, observe behavior

### To Remix Seashells
1. Pick one variation from `seashells_variation_examples.md`
2. Copy code into seashells.mjs
3. Test with `npm run ac`
4. Iterate one small change at a time

### To Create Your Own Variation
1. Identify which layer(s) you want to change
2. Read the relevant functions in Analysis doc
3. Sketch the change on paper first
4. Implement in small steps
5. Test after each change

---

## Philosophy

Seashells is built on **layered independence**:
- Spatial mapping doesn't know about synthesis
- Synthesis doesn't know about visuals
- Visuals don't know about sequencing
- Sequencing is just a voice generator

This means:
- You can modify any layer without breaking others
- Testing is incremental (change one thing, test)
- Remixes are combinatorial (stack changes)
- Future extensions are easy (add new layers)

This is intentional design. Use it.

---

## Questions?

- **How does feedback work?** → See `samplePixelFeedback()` in Analysis
- **How can I add a new pattern?** → See "Add Your Own Pattern" in Conceptual Model
- **How do I make it more musical?** → See "Make It Musical" in Remix Patterns
- **What's the audio quality?** → Bytebeat, lo-fi by design (8-bit character)
- **Can I export audio?** → Use your browser's recording, or modify to write to AudioBuffer
- **Can I use this in my own piece?** → Yes, architecture is modular and reusable

---

## Version History

- **2025.6.13** — Initial Seashells release
- **2025.6.14** — Added hold mechanism, documentation suite

**Created for:** 90-minute cassette tape experimentation

**Best consumed as:** 
- Interactive exploration (press H, make touches)
- Tape/long-form listening (press H, walk away)
- Educational dissection (read Analysis, remix patterns)
- Foundation for variations (remix, combine, extend)

---

Made with care for emergence and modular design. Happy creating!
