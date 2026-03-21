# Study: Seashells x Neural Garden

**Date:** 2026-02-22  
**Context:** You asked for an extensive study on merging the state-tracked bitmap buffer bytebeat logic in `seashells.mjs` with neural-garden style learning, while staying in the one-button, bit-shifty, interpretive instrument zone.

---

## Objective

Design a family of **one-button bytebeat instruments** that:

1. Keep the immediacy and grit of `system/public/aesthetic.computer/disks/seashells.mjs`
2. Borrow visible learning behaviors from `system/public/aesthetic.computer/disks/neural-garden.mjs`
3. Use small, transparent "nano-learning" mechanisms (not heavy backprop)
4. Stay graspable: users can see and hear the learning state change

---

## Baseline Read: `seashells.mjs`

### What already works well

- Strong bytebeat core with five bitwise pattern families in the audio generator (`seashells.mjs:35`, `seashells.mjs:39`, `seashells.mjs:43`, `seashells.mjs:47`, `seashells.mjs:51`)
- Feedback from screen pixels into synthesis parameters via `samplePixelFeedback` (`seashells.mjs:117`)
- Rich visual accumulation because paint intentionally does not wipe (`seashells.mjs:219`)
- One-button feel already exists: `space` toggles engine (`seashells.mjs:406`)
- Direct pixel writing gives a crunchy "instrument panel" feel rather than UI-chrome

### Structural gaps worth fixing before/while hybridizing

1. **Audio/visual equation drift**
- Audio path uses 5-pattern mix with one timing model (`seashells.mjs:54-80`)
- Visual path reimplements a related but different 4-pattern mix (`seashells.mjs:273-300`)
- Result: sound and image are cousins, not the same organism

2. **Feedback not clearly flowing into audio in runtime**
- Generator supports `feedback` argument (`seashells.mjs:16`)
- Synth call does not explicitly update custom feedback state (`seashells.mjs:411-417`, `seashells.mjs:465-467`)
- Visual loop definitely uses live feedback; audio loop may be mostly frequency-driven depending on engine internals

3. **Unused state**
- `localWaveform` and `waveformBufferSize` are defined but not used (`seashells.mjs:109-111`)

4. **High-frequency jitter**
- Feedback parameters are computed every paint with no smoothing or hysteresis (`seashells.mjs:166-215`)
- Produces chaotic liveliness, but can make behavior less memorizable as an instrument

---

## Baseline Read: `neural-garden.mjs`

### What is most reusable

- Clear "health phase" framing (`dying`, `sickly`, `healthy`, `mature`, `flourishing`) via `getPhase` (`neural-garden.mjs:269-275`)
- Compact visual telemetry and tiny status graph (`neural-garden.mjs:451-486`)
- Gesture -> internal model -> generated behavior loop
- Explicit controls for mode and reset (small, understandable UI)

### What not to copy directly

- Tiny GPT architecture is heavy for this use case (`neural-garden.mjs:4-190`)
- Training updates only output projection layer (`neural-garden.mjs:204-238`)
- Existing notes already call out average-collapse behavior under partial training (`system/public/aesthetic.computer/disks/neural-garden-notes.md`)

**Conclusion:** borrow the **feedback visibility and lifecycle language**, not the transformer training strategy.

---

## Merge Thesis

Do not force a full neural model into bytebeat.

Instead, build a **stateful bytebeat instrument** where:

- Bytebeat remains deterministic per parameter set
- The bitmap buffer becomes a low-dimensional feature stream
- Tiny online learners steer bit-shift parameters over time
- Learning state is rendered directly as visual telemetry

This keeps the "instrument" feel: players learn how to nudge it, and the system slowly learns how to nudge itself.

---

## Hybrid Architecture: "Bit Garden Loop"

### Core loop

1. Generate audio + visual value from shared bytebeat kernel
2. Read bitmap features from the previous frame buffer
3. Update tiny learner state (no expensive backprop)
4. Modulate bytebeat parameter tuple for next frame/audio block
5. Render learning telemetry as small overlay glyphs/graphs

### Proposed shared state

```javascript
state = {
  running: false,
  tBase: 0,
  freq: 440,
  params: {
    shiftA: 8,
    shiftB: 9,
    rhythm: 7,
    harmonic: 2,
    chaos: 0.0,
    mixBias: 0.0,
  },
  features: new Float32Array(12),
  featuresPrev: new Float32Array(12),
  health: 1.0,
  phase: "healthy",
  memory: {
    hebb: new Float32Array(12 * 8), // feature-to-param influence
    banditQ: new Float32Array(6),   // equation arm values
    reservoir: new Float32Array(64),
    ring: [],
  },
};
```

### Shared bytebeat kernel (single source of truth)

Do not duplicate equations in audio and paint. Use one function:

```javascript
function byteValue(t, p) {
  const a = (t ^ (t >> p.shiftA) ^ (t >> p.shiftB)) & 255;
  const b = ((t * p.harmonic) & (t >> 5 | t >> 4)) & 255;
  const c = (t | (t >> p.rhythm | t >> 7)) * (t & (t >> 11 | t >> 9)) & 255;
  const d = (t & (t >> 5 | t >> 8)) & 255;
  const mix = (p.mixBias + ((t >> 10) & 3)) & 3;
  const v = mix === 0 ? a : mix === 1 ? b : mix === 2 ? c : d;
  return (v ^ Math.floor(p.chaos * 255)) & 255;
}
```

---

## Nano-Learning Toolkit (Recommended)

These techniques are deliberately tiny, cheap, and inspectable.

### 1. EMA + Hysteresis Controller (stability layer)

- Smooths feature stream to avoid jitter
- Quantizes shifts to discrete bins
- Adds deadband so params only change when signal is meaningful

Good first layer for all instruments.

### 2. Hebbian Feature-to-Shift Wiring (micro-association)

Interpret each frame as:
- feature vector `f` (12 dims)
- parameter-activity vector `g` (which shift/mix choices are active)

Update:

```javascript
w[i,j] += lr * f[i] * g[j] - decay * w[i,j];
```

Use `w` as bias when choosing next shift tuple.  
This produces the "it remembers this visual regime" behavior with almost no cost.

### 3. Epsilon-Greedy Bandit Over Equation Arms

- 4-8 predefined bytebeat equation arms
- Maintain per-arm reward estimate
- Reward can be weighted sum of:
  - spectral novelty proxy (difference from short-term history)
  - visual coherence proxy (not full white noise)
  - anti-clip penalty

This gives exploration/exploitation without gradient math.

### 4. Tiny Reservoir Memory (64-cell)

- Update by simple recurrence and tanh
- Use as latent memory to modulate shifts and arm selection
- No training step needed, only readout adaptation if desired

This is strong for "weird nanogenerative drift" with deterministic repeatability.

### 5. Anti-Hebbian Unlearning Trigger

When entropy is too high for too long:

```javascript
w *= 0.98; // or targeted negative update on dominant links
```

Prevents lock-in and keeps the system "alive" rather than frozen into one attractor.

---

## Feature Set from Bitmap Buffer

Use a compact 12-feature vector extracted from the existing pixel buffer:

1. Mean luminance
2. Luminance variance
3. Horizontal gradient energy
4. Vertical gradient energy
5. Edge density
6. Bitplane density (count of high bits)
7. Centroid X
8. Centroid Y
9. Temporal delta mean
10. Temporal delta variance
11. Channel skew (R-G-B imbalance)
12. Saturation ratio

This is enough to drive perceptible learning without expensive ML.

---

## One-Button Instrument Family

All instruments share the same interaction grammar:

- `space` toggle run/stop
- `hold space` (or press for >400ms) = "tend": reduce chaos, increase coherence
- `touch/drag` optional frequency scrub (if enabled), but never required

### Instrument 1: `shell-latch`

**Character:** Slow evolving harmonic XOR glass  
**Learner:** EMA + hysteresis only  
**Why:** Fastest path to reliable one-button instrument behavior

Core tendency:
- Keep shifts near musically stable bins
- Drift one bin every N seconds based on centroid drift

### Instrument 2: `coral-hebb`

**Character:** Visual motifs "teach" future rhythm changes  
**Learner:** Hebbian feature-to-shift matrix  
**Why:** Direct merge of seashell bitmap memory with neural-garden style growth

Visible behavior:
- Repeated visual textures create recurring rhythmic signatures
- Health meter increases when associations are coherent

### Instrument 3: `reef-bandit`

**Character:** Interprets environment and picks equation "arms"  
**Learner:** Epsilon-greedy bandit + reward shaping  
**Why:** Highly graspable mode switching with tiny compute

Visible behavior:
- Displays active arm id and confidence sparkline
- Learns when to stay in one arm versus jump

### Instrument 4: `tide-reservoir`

**Character:** Long-memory, pseudo-neural drift  
**Learner:** 64-cell reservoir + lightweight readout bias  
**Why:** Most "nanogenerative weird" while staying deterministic

Visible behavior:
- Similar gestures can return different but related outcomes
- Slow phase rotations over minutes, not seconds

### Instrument 5: `plankton-prune`

**Character:** Alternates flourishing and pruning cycles  
**Learner:** Hebbian growth + periodic anti-hebbian pruning  
**Why:** Mirrors neural-garden phase language in bytebeat terms

Visible behavior:
- Phase HUD: young -> mature -> blooming -> pruning
- Prune phase audibly simplifies timbre before regrowth

### Instrument 6: `bit-murmur`

**Character:** Whisper-to-crunch dynamics tied to confidence  
**Learner:** 2-cluster online k-means on feature states  
**Why:** Tiny unsupervised model that can be visualized as two attractors

Visible behavior:
- Cluster A/B indicator
- Each cluster maps to a distinct shift/mix personality

---

## Recommended First Build

Build **Instrument 2 (`coral-hebb`) first**.

Reason:
- Most direct answer to your merge question
- Uses existing seashell feedback pipeline
- Avoids transformer/backprop complexity
- Produces visible learning state quickly

---

## Implementation Spec: `seashells-garden-study.mjs` (proposed)

### Module shape

```javascript
let synth = null;
let state = createState();

function boot({ hud }) {
  hud.label("seashells-garden-study");
}

function act({ event: e, sound, screen }) {
  if (e.is("keyboard:down:space")) toggleRun(sound);
  if (e.is("touch")) state.freqTarget = mapTouchToFreq(e.x, screen.width);
}

function sim({ screen, sound }) {
  if (!state.running) return;
  const f = extractFeatures(screen, state);
  updateLearner(state, f);
  updateParamsFromLearner(state);
  if (synth) synth.update({ tone: state.freq });
}

function paint({ screen }) {
  if (!state.running) drawInstructions(screen);
  else drawByteField(screen, state);
  drawLearningHUD(screen, state);
}

export { boot, act, sim, paint };
```

### Learning HUD proposal

- Top-right 56x64 panel (same footprint spirit as `neural-garden.mjs`)
- Elements:
  - phase label (3 chars)
  - health bar
  - active equation arm
  - novelty sparkline (last 64 frames)
  - mini heatmap (8x12 hebbian matrix, downsampled)

This gives "visualized learning algos" without obscuring canvas aesthetics.

---

## Parameter Mapping Strategy

### Discrete bins (important for instrument feel)

Map continuous learner outputs into bounded bins:

- `shiftA`: {6,7,8,9,10,11}
- `shiftB`: {7,8,9,10,11,12}
- `rhythm`: {5..12}
- `harmonic`: {1,2,3,4}
- `mixBias`: {0,1,2,3}
- `chaos`: [0.0..0.9] with low-pass smoothing

Discrete bins create repeatable "chords of logic" users can memorize.

### Reward heuristic example

```javascript
reward =
  + 0.6 * noveltyDelta
  + 0.3 * coherence
  - 0.7 * clipRatio
  - 0.2 * staticPenalty;
```

Tune by ear and eye, not by benchmark metrics alone.

---

## Visual-Learning Merge Patterns

### Pattern A: Screen teaches sound

- Existing seashell behavior, but make it explicit and logged
- Show the feature vector and top influencing links

### Pattern B: Sound teaches screen

- Use current arm/phase to choose rendering motif:
  - blooming phase: additive trails
  - pruning phase: subtraction wipes
  - sick phase: reduced color channels

### Pattern C: Mutual regulation

- If visual entropy is too high, reduce chaos and arm exploration
- If audio repetition is too high, increase exploration and shift mutation

This closes the loop in a controllable way.

---

## Experimental Matrix

### E0: Control
- Original `seashells.mjs` behavior
- Log features and sound stats only

### E1: Stabilized seashells
- Add EMA/hysteresis
- No learning yet

### E2: Hebbian associations
- Add 12x8 matrix
- Track health and phase

### E3: Bandit arms
- Add 6 equation arms
- Reward-driven arm switching

### E4: Reservoir mode
- Add 64-cell reservoir latent
- Compare with E3 for long-form drift quality

Each experiment should be tested for 3-5 minute continuous play on desktop and phone.

---

## Performance Budget

Target budget at 128x128:

- Feature extraction: < 0.7 ms/frame
- Learner update: < 0.2 ms/frame
- Render + byte mapping: < 2.0 ms/frame
- No allocations in hot loops (reuse typed arrays)

If this exceeds budget:

1. Reduce probe count
2. Use sparse feature updates (every 2 frames)
3. Lower reservoir size from 64 -> 32

---

## Risks and Mitigations

### Risk 1: Chaotic noise with no musical identity

Mitigation:
- Hard bounds on shift bins
- Low-pass chaos parameter
- Add minimum dwell time before arm switch

### Risk 2: Learning signal not graspable

Mitigation:
- Always render phase + health + active arm
- Keep HUD tiny but persistent
- Emit one textual reason when phase changes

### Risk 3: Overfitting to one screen state

Mitigation:
- Anti-hebbian decay
- Periodic prune windows
- Reward term for diversity over sliding window

### Risk 4: Audio clipping/harshness spikes

Mitigation:
- Clamp amplitude post-byte conversion
- Soft saturate and tiny DC filter
- Penalize clip ratio in reward

---

## What "Extensive" Means in Practice Here

This study intentionally defines:

- A concrete architecture
- A six-instrument family
- Tiny ML techniques matched to AC constraints
- A phased experiment matrix
- Runtime/performance bounds
- Visualized learning UX details

So we can move immediately from concept to piece implementation.

---

## Suggested Build Order (Concrete)

1. Create new piece scaffold `system/public/aesthetic.computer/disks/seashells-garden-study.mjs`
2. Extract shared `byteValue(t, params)` kernel and use it in both audio + paint
3. Implement EMA+hysteresis mapping and one small phase HUD
4. Add Hebbian matrix (Instrument 2)
5. Add bandit arm mode behind a toggle constant
6. Tune reward/phase thresholds by ear over 3-minute sessions

---

## Acceptance Criteria for First Prototype

1. One-button interaction works (`space` start/stop) with no mandatory UI controls
2. User can perceive at least 3 distinct phases in both sound and visuals
3. Behavior remains coherent for a 3-minute unattended run
4. HUD explains state without reading source code
5. CPU usage stays stable (no obvious frame drops on standard laptop dev environment)

---

## Final Recommendation

Proceed with a **Hebbian-guided seashell variant** first (`coral-hebb`), then layer bandit/reservoir modes.

That path gives the strongest merge of:
- seashells bitmap-memory bytebeat identity
- neural-garden style visible learning lifecycle
- one-button interpretive instrument feel

without stepping into fragile full-neural training complexity.
