# Neural Garden - Approach Log

## Goal
Learn and reproduce hand-drawn gestures using small neural networks that fit in browser memory (~100KB).

## Approaches Tried

### 1. Function Approximation (Fourier Features)
**Idea:** Learn mapping `t → (dx, dy)` where t ∈ [0, 1] is normalized time
**Architecture:**
- Input: Fourier features `[sin(2πt), cos(2πt), sin(4πt), ..., sin(64πt), cos(64πt)]` (12 inputs)
- Hidden: 64 neurons (sigmoid)
- Output: 2 neurons (tanh) → (dx, dy)

**Problems:**
- No directionality: Fourier features are symmetric/periodic, can't distinguish "forward" vs "backward"
- Struggled with wrapping: When t > 1.0, network extrapolates wildly
- Saturation: With high learning rate (0.3), outputs maxed out at boundaries
- Underfitting: With low learning rate (0.05), outputs averaged to constant value
- Dynamic maxMove helped scaling but didn't solve core issues

**Attempts:**
- Added linear `t` feature for directionality → made things worse (linear trend dominates)
- Wrapped t to [0, 1] → helped but quality still poor
- Tried LR values: 0.3 (too fast), 0.05 (too slow), 0.15 (middle ground, still poor)

**Result:** ❌ Predictions averaged to ~(14, 6) regardless of input gesture

---

### 2. LSTM (Sequential Memory)
**Idea:** Autoregressive model - predict next movement from recent history
**Architecture:**
- Input: Recent movements `[(dx₁, dy₁), ..., (dx₈, dy₈)]`
- Hidden: 32 LSTM cells (forget/input/output gates + cell state)
- Output: Next (dx, dy)

**Problems:**
- Only trained output layer (to keep it fast) → LSTM gates never learned
- Without full BPTT (backprop through time), it's just linear regression
- Still averaged to constant output ~(4.1, -0.8)

**Result:** ❌ Same averaging problem, no actual sequence learning

---

### 3. Tiny GPT (Transformer/Attention)
**Idea:** Self-attention over movement sequence to capture dependencies
**Architecture:**
- Input embedding: (dx, dy) → 32-dim + learnable positional encoding
- Self-attention layer: Q, K, V projections with causal masking
- FFN: 32 → 64 → 32 with GELU
- Output: 32-dim → (dx, dy)

**Problems:**
- Again only trained output layer (attention/FFN weights frozen)
- Without full transformer training (expensive), attention doesn't help
- Same averaging behavior

**Result:** ❌ Still averages, attention unused

---

## Core Issues

### Training Problem
All approaches only trained the final output layer to avoid expensive backprop:
- **Fourier approach:** Backprop through 1 hidden layer (feasible, but network too simple)
- **LSTM:** Would need BPTT through time (10x slower)
- **Transformer:** Would need backprop through attention + FFN (20x slower)

Without training the full network, we're just learning a linear fit of the last hidden representation → always averages.

### Fundamental Constraint
Trying to compress complex gestures (30-100 points, ~500px total distance) into tiny networks (~10KB weights):
- 64-neuron MLP: ~50KB
- 32-cell LSTM: ~80KB
- 32-dim Transformer: ~100KB

These networks are **too small** to memorize precise gesture shapes. They can only learn rough approximations (which ends up being the average).

---

## Potential Solutions

### A. Full Backprop (Accurate but Slow)
Implement complete gradient descent through all layers:
- **Pros:** Would actually train properly
- **Cons:** 10-20x slower training, might lag browser
- **Estimate:** 1-2 seconds training time per gesture

### B. Replay with Variation (Simple and Reliable)
Store actual gesture, add controlled noise during playback:
```javascript
// Store movements array
const storedMoves = [...movements];

// During playback
const baseMove = storedMoves[step % storedMoves.length];
const noise = [(random() - 0.5) * 5, (random() - 0.5) * 5];
const move = [baseMove[0] + noise[0], baseMove[1] + noise[1]];
```
- **Pros:** Guaranteed to preserve shape, fast, predictable
- **Cons:** Not "learning", just playback (but maybe that's fine?)

### C. Hybrid: Store + Neural Style Transfer
Store movements but use small network to add "style" variations:
- Network learns global characteristics (speed, smoothness, direction bias)
- Apply learned style as modulation over stored sequence
- **Pros:** Best of both worlds
- **Cons:** More complex architecture

### D. Larger Network with Web Workers
Move training to web worker, use bigger network:
- 128-256 neurons
- Full backprop in background thread
- **Pros:** Might actually work
- **Cons:** Complexity, memory, still might not be enough capacity

---

## Recommendation
Try **Option B (Replay with Variation)** first:
1. Simple and guaranteed to work
2. Can add personality through noise/interpolation
3. If that's boring, upgrade to **Option A (Full Backprop)** or **C (Hybrid)**

The core insight: **compression is hard**. Small neural nets can't memorize complex patterns without proper training. Either train properly (slow) or don't compress (store + playback).
