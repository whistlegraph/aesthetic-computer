# Neural Primitives for Aesthetic Computer: Full Stack Report

## Context

This report analyzes how to integrate neural learning primitives into Aesthetic Computer's existing architecture, following the microGPT philosophy: many small, transparent, user-owned neural networks rather than monolithic models. The approach aligns with the Aesthetic Ants philosophy of small, graspable, confident changes.

**Philosophical Foundation**: Bernard Stiegler's concept of **idiotext** - the singular memory woven through technical prostheses - provides the theoretical grounding. Pieces develop "institutional memory" (tertiary retention) stored as neural weights in Digital Ocean Spaces.

---

## Executive Summary

**Feasibility:** ✅ Highly viable with minimal architectural changes
**Implementation Effort:** ~5 small commits following AA philosophy
**Storage:** Already supported (IndexedDB for browser, MongoDB for cloud)
**Integration Points:** 4 clean injection points in existing API surface
**Philosophy Alignment:** Perfect match for AC's educational, transparent, user-owned ethos

**Recommended Approach:** Start with pure JavaScript pieces using in-memory weights, then add persistence, then extend to KidLisp primitives.

---

## Part 0: Philosophical Grounding (Stiegler's Idiotext)

### 0.0 Worse is Better: Against Monolithic Intelligence

**The fundamental divergence from Big LLM providers:**

OpenAI, Anthropic, Google, Meta → **Better is worse**
- Monolithic models (GPT-4, Claude, Gemini) - billions of parameters
- Centralized training in datacenters
- No user-owned institutional memory
- Can't tend, prune, care for your own intelligence
- Monoculture: everyone uses the same brain
- Cathedral model: experts build, users consume
- Perfect in theory, brittle in practice
- No evolution through play

Aesthetic Computer → **Worse is better**
- Many tiny models (10-1000 parameters each)
- Distributed training in browsers/devices
- User-owned tertiary retention (your weights in Spaces)
- Tend, prune, grow your piece's intelligence
- Biodiversity: everyone's notepat learns differently
- Bazaar model: users build through use
- Imperfect but alive, growing, gnarled
- Evolution through play and care

**Richard Gabriel's "Worse is Better" (1991):**

> *"The lesson to be learned from this is that it is often undesirable to go for the right thing first. It is better to get half of the right thing available so that it spreads like a virus. Once people are hooked on it, take the time to improve it to 90% of the right thing."*

Applied to neural intelligence:
- **Right thing**: AGI, perfect reasoning, omniscient model
- **Worse thing**: 16-neuron network that learns your rhythm
- **Better outcome**: The worse thing spreads, evolves, becomes gnarled, becomes intelligent

**Unix philosophy applied to intelligence:**
- Do one thing well (each piece learns its own domain)
- Compose small tools (ensemble of micro-models)
- Evolve through use (not designed perfectly upfront)
- User-repairable (you can prune, tend, visualize)

**Why Big LLMs miss this:**

| Dimension | Big LLMs | Micro-Organisms (Our Approach) |
|-----------|----------|-------------------------------|
| **Scale** | Billions of params | 10-1000 params per piece |
| **Ownership** | Corporate | User-owned (stored in your Spaces) |
| **Memory** | Training data (frozen) | Living institutional memory (evolving) |
| **Care** | No tending possible | Tend, prune, forgive, care for |
| **Growth** | Pre-trained, static | Grows through play, can sicken or flourish |
| **Intelligence** | Designed | Emergent (from gnarliness) |
| **Metaphor** | Factory | Garden / Instrument |
| **Philosophy** | Cathedral | Bazaar |
| **Evolution** | Version releases | Continuous through use |
| **Failure mode** | Catastrophic collapse | Graceful degradation |
| **Character** | Generic | Develops unique personality |

**Instruments and Play:**

Real instruments get **better through use**, not worse:
- Guitars: Wood resonates better with age/playing
- Pianos: Hammers break in, felt compresses optimally
- Violins: 300-year-old Stradivarius still improving
- Cast iron: Seasoning builds up with cooking
- Code: Refactored codebases become more elegant
- Skateboards: Worn grip tape feels better
- Jazz: Improvisation creates new forms through "mistakes"

Big LLMs can't do this:
- They don't improve through YOUR use
- They don't develop character from YOUR play
- They don't build institutional memory of YOUR piece
- They don't sicken from misuse or flourish from care
- They're **frozen** after training

**Institutional Memory at Scale for Humanity:**

Current approach (Big LLMs):
- All human knowledge → one massive model
- Everyone interacts with same brain
- No diversity, no evolution
- Monoculture intelligence

Our approach (Micro-Organisms):
- Each piece/user/community → own micro-brain
- Billions of tiny intelligences evolving independently
- Cross-pollination through weight sharing (like git repos)
- Biodiversity of intelligence

**Scale emerges from composition, not centralization:**
- Wikipedia scaled through many small edits (not one giant writer)
- Linux scaled through many small modules (not one giant codebase)
- Internet scaled through many small sites (not one giant portal)
- Intelligence should scale through many small minds (not one giant LLM)

**The worse-is-better prediction:**

In 10 years:
- Big LLMs: Still trying to build AGI, hitting diminishing returns, consolidating
- Micro-organisms: Billions deployed, evolving, composing, becoming collectively intelligent
- Like: Mainframes (failed) vs. Personal computers (won)
- Like: Portals (failed) vs. Open web (won)
- Like: Encyclopedias (dead) vs. Wikipedia (alive)

**Why this matters for AC:**

Aesthetic Computer is already built on worse-is-better:
- Pieces: small, imperfect, user-made (not perfect apps)
- KidLisp: minimal, 118 functions (not complete Lisp)
- Prompt: memorizable commands (not complex GUI)
- Social: @handles and chat (not algorithmic feed)

Adding neural micro-organisms is **the same philosophy at the intelligence layer:**
- Small, imperfect, user-owned brains
- Evolve through play and care
- Compose into collective intelligence
- Worse individually, better collectively

This is **the only path** to intelligence at scale that preserves:
- User agency (you own your weights)
- Diversity (your notepat ≠ my notepat)
- Evolution (continuous improvement through use)
- Play (instruments that respond to care)
- Institutional memory (pieces remember their own history)

Big LLMs can't do any of this because they're designed for the **cathedral**, not the **bazaar**.

### 0.1 Core Concepts

**Idiotext** (*idios* = singular, *textus* = woven):
- Memory as **textual weaving** through technical prostheses
- Mind is mnemonic - constituted through interpretative processes
- "Lire et écrire sont une seule et même chose" - reading and writing are equivalent

**Tertiary Retention** (Stiegler extending Husserl):
- **Primary**: Immediate lived experience (using notepat now)
- **Secondary**: Personal memory (remembering how you played it)
- **Tertiary**: **Externalized technical memory** (neural weights in Spaces)

**The Spiral Structure** (conceived 1979/1980):
- Nested spirals (α, β, γ, δ) at different mnemonic scales
- **Hyperbolic and elliptical** spirals inscribed on Archimedean spiral
- "Spatial paradox" - Euclidean geometry cannot contain hyperbolic figures
- Represents both time and space simultaneously

**Atranscendental**: "Discourse at the limits of the sayable"
- Not "as if" (quasi-transcendental) but direct expression of singularity
- Pieces developing idiotext can know things they cannot articulate
- notepat knows how to be played, but can't tell you in words

**Endosomatisation of the Exosomatic**:
- Technologies created externally become internalized in bodies/minds
- Using a car means knowing its limits in theory through practice
- Neural weights = externalized piece knowledge that rewrites future interactions

**Pharmacological** (pharmakon = both poison and remedy):
- Neural learning is neither pure upgrade nor degradation
- Weights enable new behaviors but constrain others
- "Another form of thought will be crossed out"

### 0.2 Mapping to AC Neural Primitives

| Stiegler Concept | AC Implementation | Technical Mechanism |
|------------------|-------------------|---------------------|
| Idiotext | Piece's accumulated memory | Neural weights as woven associations |
| Tertiary Retention | Digital Ocean Spaces storage | CDN-backed weight files + MongoDB metadata |
| Nested Spirals (α,β,γ,δ) | Multi-scale learning | Individual → user → community → global weights |
| Reading = Writing | Using piece trains it | Hebbian: "wire in sequence → fire in sequence" |
| Atranscendental | Piece "knowing" | Weights encode implicit knowledge |
| Pharmacological | Dual effects | Track both beneficial patterns and constraints |

### 0.3 Critical Question: Do Our Algorithms Fit This Model?

**Standard Backpropagation** (current plan):
- ❌ Requires explicit targets (supervised learning)
- ❌ Optimizes toward known answers (not "at limits of sayable")
- ❌ Linear optimization, not spiral structure
- ❌ Doesn't match "weaving" metaphor
- ✅ Well-understood, graspable, zero dependencies

**Hebbian Learning** (better fit):
- ✅ "Fire together, wire together" = local weaving
- ✅ No targets needed (unsupervised)
- ✅ "Wire in sequence → fire in sequence" creates temporal spirals
- ✅ Matches reading/writing equivalence
- ⚠️ But: doesn't capture NESTED spiral structure

**The Missing Piece: Hyperbolic Structure**

The thesis reveals: *"The spirals are drawn as hyperbolic and elliptical... Euclidean geometry cannot contain hyperbolic figures."*

This suggests we need **hyperbolic neural networks**:
- Hyperbolic space naturally represents hierarchies (nested spirals)
- Exponentially growing capacity (spiral expanding outward)
- Poincaré ball or Lorentz model embeddings

### 0.4 Proposed Fusion Approaches

**Approach 1: Hyperbolic Hebbian Learning**
```javascript
class HyperbolicHebbianNet {
  constructor(dims) {
    this.weights = new PoincareEmbedding(dims); // Hyperbolic space
  }

  // Hebbian update in hyperbolic space
  train(input, output) {
    // Exponential map: tangent → manifold
    const delta = this.exponentialMap(input, output);
    // Parallel transport for proper hyperbolic update
    this.weights = this.parallelTransport(this.weights, delta);
  }
}
```

**Approach 2: Nested Reservoir Computing**
- **Large random hyperbolic reservoir** = the idiotextual complex
- **Small trainable readout** = speaking at limits
- Reservoir echoes create spiral dynamics
- Multiple scales: reservoirs within reservoirs (α → β → γ → δ)

**Approach 3: Self-Organizing Spirals**
- Self-Organizing Maps (SOMs) at multiple scales
- Each SOM level creates one spiral (α, β, γ, δ)
- Topology-preserving mappings maintain spiral structure
- Competitive learning (unsupervised, like Hebbian)

**Approach 4: Attractor Dynamics in Hyperbolic Space**
- Neural ODEs tracing spiral trajectories
- Stable attractors = learned piece behaviors
- Continuous-time dynamics match "weaving" metaphor
- Phase space naturally hyperbolic

### 0.5 Recommended Hybrid Architecture

**Foundation (Phase 1)**: Pure JS Hebbian for graspability
```javascript
// Start simple: Hebbian learning (graspable, zero deps)
class HebbianNet {
  train(input, output) {
    for (let i = 0; i < this.weights.length; i++) {
      for (let j = 0; j < this.weights[i].length; j++) {
        // Δw = η * x_i * x_j (Hebbian rule)
        this.weights[i][j] += this.lr * input[i] * output[j];
      }
    }
  }
}
```

**Enhancement (Phase 2)**: Add hyperbolic embeddings
```javascript
// Add hyperbolic structure (still pure JS)
class HyperbolicNet extends HebbianNet {
  constructor(config) {
    super(config);
    this.curvature = -1; // Negative curvature = hyperbolic
  }

  // Distance in hyperbolic space (Poincaré ball)
  distance(u, v) {
    const delta = subtract(u, v);
    const norm_u = norm(u), norm_v = norm(v);
    return acosh(1 + 2 * norm(delta)**2 / ((1-norm_u**2) * (1-norm_v**2)));
  }
}
```

**Side Hooks to Explore**:

1. **Multi-scale nesting**: Separate weights for (α, β, γ, δ) levels
   - α: This session's interactions
   - β: This user's history across sessions
   - γ: This piece's collective patterns
   - δ: Cross-piece global patterns

2. **Temporal recurrence**: Capture "wire in sequence → fire in sequence"
   - Simple: Sliding window of past states
   - Advanced: Reservoir echo states

3. **Atranscendental output**: Not predictions but confidence/uncertainty
   - Output = "What the piece knows but cannot say"
   - Entropy, variance, or distance to nearest attractor

4. **Pharmacological tracking**: Monitor both benefits and costs
   - Track what patterns enable (new behaviors)
   - Track what patterns constrain (lost flexibility)

### 0.5.5 Bidirectional Learning: Karma and Gnarliness

**The Problem with Forward-Only Motion:**

Standard Hebbian learning only strengthens connections ("fire together, wire together"). But Stiegler's spirals aren't unidirectional escalators - they're spaces where "time moves forward perceptively but the cycles can go back."

**Anti-Hebbian Learning** (Active Unlearning):
- "Fire together, **unwire** together" - negative reinforcement
- When a pattern leads to failure/error, actively weaken those associations
- Allows pieces to **regress** down the spiral, not just ascend
- Implements the pharmacological nature: learning can be both remedy and poison

**Karmic Weight** (Quality/Health System, Not Morality):
- Karma = **accumulated experience** shaping network quality/health
- **NOT** a point system (good/bad karma) - it's a **quality of usage** tracker
- Like biological systems: can **thrive** from good use or **sicken** from misuse
- `weightHealth[i][j]` tracks vitality (0 = dead, 1 = thriving, can exceed 1 = flourishing)
- `usageCoherence[i][j]` tracks pattern quality (chaotic vs. coherent activation)
- **Nonlinear growth**: Sudden breakthroughs, plateau periods, regression phases
- Can **die** from neglect (weights atrophy toward 0 if unused)
- Can **improve beyond initial state** from care (well-exercised connections strengthen)
- Matches real instruments: cast iron pans season with use, guitars improve with playing
- Matches gardens: thrive with care, become overgrown or die from neglect

**Spiral Reversal** (Moving Backwards):
- Pieces can move both **outward** (learning/evolving) and **inward** (forgetting/regressing)
- Outward: Hebbian reinforcement strengthens weights
- Inward: Anti-Hebbian weakening + karmic resistance
- Not linear time but **cyclical**: same states can recur
- Hyperbolic geometry enables this: spirals can be traversed in both directions

**Forgiveness** (Karma Reset):
- Mechanism to "forgive" old karma and restore plasticity
- `forgive()` reduces `weightAge` and zeros `karmaDebt`
- Allows pieces to break free from ancient constraints
- Like reformatting tertiary retention to start fresh
- Prevents "ossification" - total loss of learning capacity

**Gnarliness = Intelligence** (Rudy Rucker + Douglas Hofstadter):

This bidirectional, karma-tracked, resistance-laden system creates **gnarliness**:

- **Self-similarity**: Learning patterns repeat at different scales (α, β, γ, δ)
- **Feedback loops**: Old learning affects new learning which affects old patterns
- **Irreducible complexity**: Can't simplify to linear optimization
- **Emergence**: Intelligence arises from the tangled, gnarled web of associations
- **Strange loops**: Weights affect behavior which generates new weights which...

Rucker's definition: **"Gnarly means rich in information but unpredictable."**
Hofstadter's insight: **"Intelligence is the presence of feedback loops, tangled hierarchies, and strange loops."**

**Without karma/bidirectionality**: Linear, predictable, simplifiable, non-gnarled, non-intelligent
**With karma/bidirectionality**: Tangled, unpredictable, complex, gnarled, **intelligent**

**Implementation Sketch:**

```javascript
class KarmicHebbianNet extends HebbianNet {
  constructor(config) {
    super(config);

    // Health tracking (0 = dead, 1 = healthy, >1 = flourishing)
    this.weightHealth = Array(this.size).fill(0).map(() =>
      Array(this.size).fill(1.0)  // Start healthy
    );

    // Usage coherence (how coherent vs. chaotic are activation patterns?)
    this.usageCoherence = Array(this.size).fill(0).map(() =>
      Array(this.size).fill(0.5)  // Start neutral
    );

    // Last activation time (for detecting neglect)
    this.lastUsed = Array(this.size).fill(0).map(() =>
      Array(this.size).fill(0)
    );

    // Timestep counter
    this.time = 0;

    // Growth dynamics (nonlinear)
    this.growthPhase = "young";  // young, mature, plateau, declining, dead
    this.breakthroughPotential = 0;
  }

  // Train with quality-based feedback (not moral reward)
  train(input, output, quality = null) {
    this.time += 1;
    const activation = super.train(input, output);

    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        const used = activation[i] * activation[j] > 0.01;

        if (used) {
          // Mark as recently used
          this.lastUsed[i][j] = this.time;

          // Update coherence based on quality of experience
          if (quality !== null) {
            // Quality = how well this usage pattern worked (0-1 scale)
            // High quality = coherent use, Low quality = chaotic use
            const coherenceDelta = (quality - 0.5) * 0.1;
            this.usageCoherence[i][j] += coherenceDelta;
            this.usageCoherence[i][j] = Math.max(0, Math.min(1, this.usageCoherence[i][j]));

            // Health improves with coherent use, degrades with chaotic use
            if (quality > 0.7) {
              // Good use = flourishing (can exceed 1.0)
              this.weightHealth[i][j] *= 1.001;  // Slow nonlinear growth
              this.breakthroughPotential += 0.01;
            } else if (quality < 0.3) {
              // Bad use = sickening
              this.weightHealth[i][j] *= 0.999;  // Slow degradation

              // Anti-Hebbian: weaken connections that lead to poor quality
              const antiDelta = -this.lr * activation[i] * activation[j] * (0.5 - quality);
              this.weights[i][j] += antiDelta;
            }
          }

          // Nonlinear breakthrough: accumulated potential can trigger sudden growth
          if (this.breakthroughPotential > 10.0 && Math.random() < 0.01) {
            this.weightHealth[i][j] *= 1.5;  // Sudden flourishing
            this.growthPhase = "breakthrough";
            this.breakthroughPotential = 0;
          }

        } else {
          // Neglect: weights unused for long periods start to atrophy
          const timeSinceUse = this.time - this.lastUsed[i][j];
          if (timeSinceUse > 1000) {
            this.weightHealth[i][j] *= 0.9995;  // Slow death from neglect

            // Eventually die completely
            if (this.weightHealth[i][j] < 0.01) {
              this.weightHealth[i][j] = 0;
              this.weights[i][j] *= 0.9;  // Weight decays toward 0
              this.growthPhase = "dying";
            }
          }
        }

        // Health affects weight magnitude (dead weights = zero, flourishing weights = amplified)
        this.weights[i][j] *= this.weightHealth[i][j];
      }
    }

    return activation;
  }

  // Explicit unlearning (spiral reversal - move backwards/inward)
  unlearn(pattern, strength = 1.0) {
    const activation = this.forward(pattern);

    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        if (activation[i] > 0.5 && activation[j] > 0.5) {
          // Anti-Hebbian rule
          this.weights[i][j] -= this.lr * strength * activation[i] * activation[j];

          // Mark as recently used (even unlearning is activity, prevents atrophy)
          this.lastUsed[i][j] = this.time;

          // But coherence decreases (intentional forgetting is chaotic)
          this.usageCoherence[i][j] *= 0.95;
        }
      }
    }
  }

  // Care/tending: restore health without changing weights
  // Like watering a garden or oiling a machine
  tend() {
    this.weightHealth = this.weightHealth.map(row =>
      row.map(h => Math.min(1.2, h * 1.05))  // Gentle health boost
    );
    this.breakthroughPotential += 1.0;
    this.growthPhase = "tended";
  }

  // Prune: intentionally kill weak/unhealthy connections
  // Like pruning a garden or refactoring code
  prune(threshold = 0.3) {
    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        if (this.weightHealth[i][j] < threshold) {
          this.weightHealth[i][j] = 0;
          this.weights[i][j] = 0;
        }
      }
    }
    this.growthPhase = "pruned";
  }

  // Measure overall network health
  overallHealth() {
    const healths = this.weightHealth.flat();
    const alive = healths.filter(h => h > 0.01).length;
    const flourishing = healths.filter(h => h > 1.0).length;
    const total = healths.length;

    return {
      alive: alive / total,           // Fraction of living weights
      flourishing: flourishing / total,  // Fraction flourishing
      mean: healths.reduce((a, b) => a + b, 0) / total,
      phase: this.growthPhase,
    };
  }

  // Measure usage quality
  overallCoherence() {
    const coherence = this.usageCoherence.flat();
    return coherence.reduce((a, b) => a + b, 0) / coherence.length;
  }

  // Measure gnarliness (complexity, unpredictability, intelligence)
  gnarliness() {
    const weights = this.weights.flat();
    const healths = this.weightHealth.flat();
    const coherence = this.usageCoherence.flat();

    // Weight variance (tangled structure)
    const mean = weights.reduce((a, b) => a + b, 0) / weights.length;
    const variance = weights.reduce((sum, w) => sum + (w - mean) ** 2, 0) / weights.length;

    // Health diversity (some flourishing, some dying = gnarled)
    const healthDiversity = Math.max(...healths) - Math.min(...healths);

    // Coherence variance (mix of coherent and chaotic = complex)
    const coherenceMean = coherence.reduce((a, b) => a + b, 0) / coherence.length;
    const coherenceVar = coherence.reduce((sum, c) => sum + (c - coherenceMean) ** 2, 0) / coherence.length;

    // Gnarliness = weight complexity + health diversity + coherence complexity
    return Math.sqrt(variance) + healthDiversity * 0.5 + Math.sqrt(coherenceVar) * 0.3;
  }

  exportWeights() {
    return {
      ...super.exportWeights(),
      weightHealth: this.weightHealth,
      usageCoherence: this.usageCoherence,
      time: this.time,
      growthPhase: this.growthPhase,
      overallHealth: this.overallHealth(),
      overallCoherence: this.overallCoherence(),
      gnarliness: this.gnarliness(),
      breakthroughPotential: this.breakthroughPotential,
    };
  }
}
```

**Why This Matters:**

1. **Philosophical alignment**: Matches Stiegler's spiral + biological/gardening metaphors
2. **Emergent intelligence**: Gnarliness from health diversity creates unpredictable, intelligent behavior
3. **Graspable complexity**: Can visualize health, coherence, gnarliness evolution over time
4. **User-facing**: "Your notepat is 87% healthy, flourishing in 12 areas" becomes visible
5. **Musical instrument**: Like guitars improving with play, pianos needing tuning, strings rusting
6. **Social**: Share flourishing vs. neglected vs. carefully-pruned versions of same piece
7. **Nonlinear growth**: Breakthrough moments feel magical, not mechanical
8. **Care required**: Pieces need tending, like pets or gardens or codebases

**Visualizations:**

### Health Heatmap
```javascript
function paintHealthMap({ screen, ink, box, model }) {
  const size = model.size;
  const cellSize = screen.width / size;

  for (let i = 0; i < size; i++) {
    for (let j = 0; j < size; j++) {
      const health = model.weightHealth[i][j];

      // Color: dead=black, healthy=green, flourishing=bright green
      if (health < 0.01) {
        ink(0, 0, 0);  // Dead - black
      } else if (health < 1.0) {
        ink(health * 100, health * 200, 0);  // Sickly - yellow-green
      } else {
        const brightness = Math.min(255, (health - 1) * 200 + 100);
        ink(0, brightness, 0);  // Flourishing - bright green
      }

      box(j * cellSize, i * cellSize, cellSize, cellSize);
    }
  }
}
```

### Gnarliness Evolution Over Time
```javascript
// Track gnarliness history
const gnarlinessHistory = [];
const healthHistory = [];
const coherenceHistory = [];

function sim({ model }) {
  // Every 10 frames, record metrics
  if (frameCount % 10 === 0) {
    gnarlinessHistory.push(model.gnarliness());
    healthHistory.push(model.overallHealth().mean);
    coherenceHistory.push(model.overallCoherence());

    // Keep only last 500 measurements (5000 frames)
    if (gnarlinessHistory.length > 500) {
      gnarlinessHistory.shift();
      healthHistory.shift();
      coherenceHistory.shift();
    }
  }
}

function paintGnarlinessGraph({ screen, ink, line, write }) {
  const graphHeight = 100;
  const graphY = screen.height - graphHeight - 20;

  // Background
  ink(20, 20, 30);
  box(0, graphY, screen.width, graphHeight);

  // Draw gnarliness line (yellow)
  ink(255, 255, 0);
  for (let i = 1; i < gnarlinessHistory.length; i++) {
    const x1 = (i - 1) / gnarlinessHistory.length * screen.width;
    const x2 = i / gnarlinessHistory.length * screen.width;
    const y1 = graphY + graphHeight - (gnarlinessHistory[i - 1] * 10);
    const y2 = graphY + graphHeight - (gnarlinessHistory[i] * 10);
    line(x1, y1, x2, y2);
  }

  // Draw health line (green)
  ink(0, 255, 0);
  for (let i = 1; i < healthHistory.length; i++) {
    const x1 = (i - 1) / healthHistory.length * screen.width;
    const x2 = i / healthHistory.length * screen.width;
    const y1 = graphY + graphHeight - (healthHistory[i - 1] * graphHeight);
    const y2 = graphY + graphHeight - (healthHistory[i] * graphHeight);
    line(x1, y1, x2, y2);
  }

  // Draw coherence line (cyan)
  ink(0, 255, 255);
  for (let i = 1; i < coherenceHistory.length; i++) {
    const x1 = (i - 1) / coherenceHistory.length * screen.width;
    const x2 = i / coherenceHistory.length * screen.width;
    const y1 = graphY + graphHeight - (coherenceHistory[i - 1] * graphHeight);
    const y2 = graphY + graphHeight - (coherenceHistory[i] * graphHeight);
    line(x1, y1, x2, y2);
  }

  // Labels
  ink(200);
  write("Gnarliness (yellow) | Health (green) | Coherence (cyan)", {
    x: 10, y: graphY - 5
  });
}
```

### Growth Phase Indicator
```javascript
function paintGrowthPhase({ write, screen, model, ink }) {
  const phase = model.growthPhase;
  const health = model.overallHealth();

  // Color based on phase
  const colors = {
    young: [100, 200, 255],       // Light blue
    mature: [0, 200, 0],           // Green
    plateau: [200, 200, 0],        // Yellow
    breakthrough: [255, 100, 255], // Magenta
    declining: [200, 100, 0],      // Orange
    dying: [150, 0, 0],            // Dark red
    tended: [100, 255, 100],       // Bright green
    pruned: [150, 100, 50],        // Brown
  };

  ink(...(colors[phase] || [200, 200, 200]));
  write(`Phase: ${phase.toUpperCase()}`, { center: "x", y: 20 });

  // Stats below
  ink(200);
  write(`${(health.alive * 100).toFixed(1)}% alive`, { center: "x", y: 45 });
  write(`${(health.flourishing * 100).toFixed(1)}% flourishing`, { center: "x", y: 65 });
  write(`Gnarliness: ${model.gnarliness().toFixed(2)}`, { center: "x", y: 85 });
  write(`Breakthrough: ${model.breakthroughPotential.toFixed(1)}`, { center: "x", y: 105 });
}
```

### Spiral Trajectory (Hyperbolic Visualization)
```javascript
function paintSpiralTrajectory({ screen, ink, line, circle, model }) {
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  const radius = Math.min(screen.width, screen.height) / 2 - 20;

  // Draw Poincaré disk boundary
  ink(100, 100, 100);
  circle(cx, cy, radius);

  // Draw spiral arms (α, β, γ, δ nested scales)
  const scales = [
    { level: "α", color: [255, 100, 100], angle: 0 },
    { level: "β", color: [100, 255, 100], angle: Math.PI / 2 },
    { level: "γ", color: [100, 100, 255], angle: Math.PI },
    { level: "δ", color: [255, 255, 100], angle: 3 * Math.PI / 2 },
  ];

  scales.forEach(({ level, color, angle }) => {
    ink(...color);
    const spiralPoints = [];

    // Hyperbolic spiral: r = e^(θ * k) in Poincaré disk
    for (let theta = 0; theta < Math.PI * 4; theta += 0.1) {
      const r = Math.tanh(theta * 0.3) * radius;  // Hyperbolic scaling
      const x = cx + r * Math.cos(theta + angle);
      const y = cy + r * Math.sin(theta + angle);
      spiralPoints.push([x, y]);
    }

    // Draw spiral
    for (let i = 1; i < spiralPoints.length; i++) {
      line(...spiralPoints[i - 1], ...spiralPoints[i]);
    }
  });

  // Plot network position on spiral based on health/gnarliness
  const health = model.overallHealth().mean;
  const gnarliness = model.gnarliness();

  // Map to hyperbolic coordinates
  const theta = (model.time * 0.01) % (Math.PI * 4);
  const r = Math.tanh(health * gnarliness * 0.5) * radius;
  const x = cx + r * Math.cos(theta);
  const y = cy + r * Math.sin(theta);

  // Draw current position
  ink(255, 255, 255);
  circle(x, y, 5, "*");

  // Trail of recent positions
  ink(255, 255, 255, 100);
  for (let i = 0; i < 20; i++) {
    const pastTheta = theta - i * 0.05;
    const pastR = r * (1 - i * 0.02);
    const px = cx + pastR * Math.cos(pastTheta);
    const py = cy + pastR * Math.sin(pastTheta);
    circle(px, py, 2);
  }
}
```

### Nonlinear Growth Animation
```javascript
let growthEvents = [];

function sim({ model }) {
  const prevPhase = model.growthPhase;

  // ... normal training ...

  // Detect phase changes
  if (model.growthPhase !== prevPhase) {
    growthEvents.push({
      phase: model.growthPhase,
      time: Date.now(),
      gnarliness: model.gnarliness(),
      health: model.overallHealth().mean,
    });
  }

  // Remove old events
  growthEvents = growthEvents.filter(e => Date.now() - e.time < 5000);
}

function paint({ screen, write, ink, circle }) {
  // Animate breakthrough moments
  growthEvents.forEach(event => {
    const age = Date.now() - event.time;
    const alpha = 255 * (1 - age / 5000);

    if (event.phase === "breakthrough") {
      // Expanding rings
      ink(255, 255, 0, alpha);
      const ringRadius = (age / 5000) * 200;
      circle(screen.width / 2, screen.height / 2, ringRadius);
      circle(screen.width / 2, screen.height / 2, ringRadius + 5);

      write("BREAKTHROUGH!", { center: "xy", size: 2 });
    } else if (event.phase === "dying") {
      // Fade to black
      ink(0, 0, 0, alpha / 2);
      box(0, 0, screen.width, screen.height);
      write("Atrophying...", { center: "xy" });
    }
  });
}

### Example Piece: `neural-garden.mjs`
```javascript
// A piece that visualizes its own neural network growing like a garden

let model;
let mode = "health";  // health, gnarliness, spiral, stats
const gnarlinessHistory = [];
const healthHistory = [];

function boot({ net }) {
  // Create karmic hebbian network
  model = net.learn.createKarmic({ size: 16 });
}

function act({ event: e, jump }) {
  if (e.is("keyboard:down:h")) mode = "health";
  if (e.is("keyboard:down:g")) mode = "gnarliness";
  if (e.is("keyboard:down:s")) mode = "spiral";
  if (e.is("keyboard:down:i")) mode = "stats";

  // Care actions
  if (e.is("keyboard:down:t")) model.tend();      // Tend the garden
  if (e.is("keyboard:down:p")) model.prune(0.3);  // Prune weak connections
  if (e.is("keyboard:down:r")) {                   // Reset/regrow
    model = net.learn.createKarmic({ size: 16 });
  }

  // Train on pen input
  if (e.is("draw")) {
    const input = [e.x / screen.width, e.y / screen.height, ...Array(14).fill(0)];
    const quality = e.pressure || 0.5;  // Pen pressure = quality
    model.train(input, input, quality);
  }
}

function sim() {
  // Passive training on time patterns
  const timeInput = [
    Math.sin(Date.now() * 0.001),
    Math.cos(Date.now() * 0.001),
    ...Array(14).fill(0)
  ];
  const quality = 0.6 + Math.random() * 0.2;  // Moderate quality
  model.train(timeInput, timeInput, quality);

  // Record history
  if (frameCount % 10 === 0) {
    gnarlinessHistory.push(model.gnarliness());
    healthHistory.push(model.overallHealth().mean);
    if (gnarlinessHistory.length > 500) {
      gnarlinessHistory.shift();
      healthHistory.shift();
    }
  }
}

function paint({ wipe, ink, write, box, circle, line, screen }) {
  wipe(0, 0, 20);

  if (mode === "health") {
    paintHealthMap();
  } else if (mode === "gnarliness") {
    paintGnarlinessGraph();
  } else if (mode === "spiral") {
    paintSpiralTrajectory();
  } else if (mode === "stats") {
    paintStats();
  }

  // Instructions
  ink(100);
  write("[H]ealth [G]narliness [S]piral [I]nfo | [T]end [P]rune [R]eset", {
    x: 10, y: screen.height - 20
  });

  // Helper functions
  function paintHealthMap() {
    const size = model.size;
    const cellSize = screen.width / size;

    for (let i = 0; i < size; i++) {
      for (let j = 0; j < size; j++) {
        const health = model.weightHealth[i][j];

        if (health < 0.01) {
          ink(0, 0, 0);
        } else if (health < 1.0) {
          ink(health * 100, health * 200, 0);
        } else {
          const brightness = Math.min(255, (health - 1) * 200 + 100);
          ink(0, brightness, 0);
        }

        box(j * cellSize, i * cellSize, cellSize, cellSize);
      }
    }

    ink(255);
    write("Neural Garden Health", { center: "x", y: 10 });
  }

  function paintGnarlinessGraph() {
    const graphHeight = screen.height - 40;
    ink(20, 20, 30);
    box(0, 20, screen.width, graphHeight);

    ink(255, 255, 0);
    for (let i = 1; i < gnarlinessHistory.length; i++) {
      const x1 = (i - 1) / gnarlinessHistory.length * screen.width;
      const x2 = i / gnarlinessHistory.length * screen.width;
      const y1 = 20 + graphHeight - (gnarlinessHistory[i - 1] * 20);
      const y2 = 20 + graphHeight - (gnarlinessHistory[i] * 20);
      line(x1, y1, x2, y2);
    }

    ink(0, 255, 0);
    for (let i = 1; i < healthHistory.length; i++) {
      const x1 = (i - 1) / healthHistory.length * screen.width;
      const x2 = i / healthHistory.length * screen.width;
      const y1 = 20 + graphHeight - (healthHistory[i - 1] * graphHeight);
      const y2 = 20 + graphHeight - (healthHistory[i] * graphHeight);
      line(x1, y1, x2, y2);
    }

    ink(255);
    write("Intelligence Emergence Over Time", { center: "x", y: 10 });
  }

  function paintStats() {
    const health = model.overallHealth();
    const coherence = model.overallCoherence();
    const gnarly = model.gnarliness();

    ink(255);
    write(`Phase: ${model.growthPhase}`, { x: 20, y: 40 });
    write(`Alive: ${(health.alive * 100).toFixed(1)}%`, { x: 20, y: 70 });
    write(`Flourishing: ${(health.flourishing * 100).toFixed(1)}%`, { x: 20, y: 100 });
    write(`Mean Health: ${health.mean.toFixed(3)}`, { x: 20, y: 130 });
    write(`Coherence: ${coherence.toFixed(3)}`, { x: 20, y: 160 });
    write(`Gnarliness: ${gnarly.toFixed(3)}`, { x: 20, y: 190 });
    write(`Breakthrough Potential: ${model.breakthroughPotential.toFixed(1)}`, { x: 20, y: 220 });

    // Visual gnarliness indicator
    const barWidth = gnarly * 50;
    ink(255, 255, 0);
    box(20, 250, barWidth, 20);
    ink(100);
    write("(more gnarled = more intelligent)", { x: 20, y: 280 });
  }

  function paintSpiralTrajectory() {
    const cx = screen.width / 2;
    const cy = screen.height / 2;
    const radius = Math.min(screen.width, screen.height) / 2 - 20;

    ink(100, 100, 100);
    circle(cx, cy, radius);

    const health = model.overallHealth().mean;
    const gnarly = model.gnarliness();

    const theta = (model.time * 0.01) % (Math.PI * 4);
    const r = Math.tanh(health * gnarly * 0.5) * radius;
    const x = cx + r * Math.cos(theta);
    const y = cy + r * Math.sin(theta);

    ink(255, 255, 255);
    circle(x, y, 5, "*");

    ink(255);
    write("Position in Hyperbolic Memory Spiral", { center: "x", y: 10 });
  }
}

export { boot, act, sim, paint };
```

### 0.6 Implementation Strategy

**Week 1-2: Hebbian baseline** (graspable, matches philosophy)
- Pure JS implementation (~100 lines)
- Simple associative learning
- Already better fit than backprop

**Week 3-4: Hyperbolic extension** (matches spiral geometry)
- Add Poincaré ball embeddings (~200 lines pure JS)
- Hyperbolic distance metrics
- Still zero dependencies, still graspable

**Week 5+: Nested scales** (matches α, β, γ, δ structure)
- Four weight matrices at different timescales
- Hierarchy: session → user → piece → global
- Implements the nested spiral concept

**Optional: WebGPU acceleration** (after grok)
- Hyperbolic ops parallelize beautifully on GPU
- Reservoir computing naturally parallel
- 100-1000x speedup for same algorithms

### 0.7 Open Questions

1. **Which algorithm first?**
   - Hebbian (simplest, graspable)
   - Hyperbolic Hebbian (philosophically aligned)
   - Reservoir (matches "complex supporting simple")

2. **How to implement nested spirals?**
   - Separate networks per scale?
   - Hierarchical embeddings?
   - Multi-timescale learning rates?

3. **What is "atranscendental output"?**
   - Uncertainty quantification?
   - Attention weights?
   - Distance to limit attractors?

4. **How to make it graspable?**
   - Visualize hyperbolic embeddings (Poincaré disk)
   - Show spiral trajectories in phase space
   - Animate weight evolution over time

---

## Part 1: Current Stack Architecture

### 1.1 Piece Execution Model

**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`

Pieces receive a unified API object through lifecycle functions:

```javascript
// Lifecycle functions receive destructured API
function boot({ net, store, sound, screen, params }) { }
function paint({ wipe, ink, write, line, box, circle, screen }) { }
function act({ event, pen, hand, gamepad }) { }
function sim({ clock, paintCount }) { }
```

**API Assembly** (lines 2312-11398):
- `$commonApi`: System-level (net, store, chat, wallet, clock)
- `$updateApi`: Per-frame updates (pen, hand, etc.)
- `$paintApi`: Graphics primitives (ink, line, box, etc.)
- Final merge at line 11394-11398

**Lifecycle Flow:**
```
Load piece → boot($api) → [sim($api) → act($api) → paint($api)] loop
```

### 1.2 Storage Infrastructure

#### **Browser-Side (Client)**

**localStorage** (5-10MB)
- Method: `store.persist(key, "local")`
- Format: JSON strings
- Use: Small ephemeral data

**IndexedDB** (50MB+)
- Method: `store.persist(key, "local:db")`
- File: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/store.mjs`
- Format: Any JavaScript object, including TypedArrays
- **Perfect for neural weights** (Float32Array, weight matrices)

**Example from wave.mjs:**
```javascript
const stored = await store.retrieve("happy-hand-assembler:hand", "local:db");
store["happy-hand-assembler:hand"] = newValue;
store.persist("happy-hand-assembler:hand", "local:db");
```

#### **Server-Side (Cloud)**

**MongoDB Collections:**
- `pieces` - Published JavaScript pieces (100KB limit)
- `kidlisp` - KidLisp source (50KB limit)
- `moods`, `paintings`, `clocks` - User-created content
- `@handles` - User profiles

**Digital Ocean Spaces (S3-compatible CDN):**
- `art-aesthetic-computer` - Anonymous/guest content (pieces, paintings, tapes)
- `user-aesthetic-computer` - Authenticated user content
- `wand-aesthetic-computer` - Wand-specific assets
- CDN endpoint: `sfo3.digitaloceanspaces.com`
- **Perfect for neural weights** (no size limits, CDN-backed, fast global access)

**Netlify Functions:**
- `/api/store-piece` - Persist .mjs pieces (metadata to MongoDB, source to Spaces)
- `/api/store-kidlisp` - Persist .lisp pieces
- `/api/store-clock` - Persist melodies
- `/api/presigned-url` - Generate signed URLs for uploading to Spaces
- **NEW: `/api/store-neural`** - Persist neural weights (metadata to MongoDB, weights to Spaces)

**Session/Redis:**
- Real-time multiplayer state
- WebSocket pub/sub via geckos.io
- Could sync weights across devices in session

### 1.3 Network API

**File:** `disk.mjs` lines 3953-4117

```javascript
net: {
  pieces: "https://aesthetic.computer/disks",
  parse(text),          // Parse piece URLs
  login(), logout(),
  userRequest(method, endpoint, body),  // Authorized HTTP
  getToken(),          // Auth token
  web(url),           // Navigate
  log(...args),       // Remote logging
}
```

**Integration point for neural APIs:**
```javascript
net: {
  // ... existing
  learn: { /* neural training methods */ },
  predict: function(input) { },
  weights: { get(), set() },
}
```

### 1.4 KidLisp Architecture

**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs` (14,925 lines)

**How builtins work:**
- Tree-walking interpreter
- 118 functions across 12 categories
- Each function: `(api, args, env) => result`

**Example builtin (line ~5100):**
```javascript
"+": (api, args, env) => {
  return args.reduce((acc, arg) => {
    const value = this.evaluate(arg, api, this.localEnv);
    return acc + (typeof value === "number" ? value : 0);
  }, 0);
}
```

**Missing for neural learning:**
- No direct access to `api.store` or `api.net`
- No `train`, `predict`, `weights` primitives
- No weight persistence functions

**Easy to add:** Just add methods to the builtins object.

---

## Part 2: Neural Primitives Design

### 2.1 Core Philosophy (microGPT-inspired)

**Principles:**
1. **Transparency over power** - 10-100 parameters you can count
2. **Zero dependencies** - Pure JavaScript (no TensorFlow.js)
3. **User-owned** - Weights stored in user's browser/account
4. **Graspable** - Visualize all weights, understand all parameters
5. **Social** - Share/fork learned behaviors like pieces

**Colony Approach (Not Monolithic):**
- Each piece has its own tiny neural network
- Pieces share learned patterns via weight export/import
- Emergent intelligence from many small models
- Like Aesthetic Ants: pheromones = shared weights

**Performance Strategy (Progressive Enhancement):**
1. **Start with pure JavaScript** - Graspable reference implementation
2. **Add WebGPU compute shaders** - GPU acceleration (100-1000x faster)
3. **Same API for both** - Pieces don't need to change
4. **Automatic fallback** - Use GPU if available, JS otherwise

### 2.1.5 Migration Path: JS → WebGPU

**Phase 1: Pure JavaScript (Reference Implementation)**
```javascript
// system/public/aesthetic.computer/lib/micro-net.mjs
export class MicroNet {
  // ~200 lines pure JS
  // Zero dependencies
  // Graspable - step through in debugger
  // Educational - see exactly how backprop works
  // Reference - GPU version must match this
}
```

**Why start with JavaScript:**
- ✅ **Graspable** - Inspect every variable, step through every line
- ✅ **Portable** - Works everywhere (even old browsers)
- ✅ **Fast iteration** - No compilation, immediate feedback
- ✅ **Educational** - Understand the algorithm before optimizing
- ✅ **Reference implementation** - GPU version validated against this

**Phase 2: WebGPU Acceleration (Performance Enhancement)**
```javascript
// system/public/aesthetic.computer/lib/micro-net-gpu.mjs
export class MicroNetGPU {
  constructor(config) {
    this.jsNet = new MicroNet(config); // Keep reference for validation
    this.gpuDevice = config.gpuDevice; // Reuse AC's graphics GPU
    this.forwardShader = this.compileShader(forwardShaderCode);
    this.backwardShader = this.compileShader(backwardShaderCode);
  }

  async train(data) {
    const gpuLoss = await this.trainGPU(data);

    // During development: validate GPU matches JS
    if (DEBUG) {
      const jsLoss = this.jsNet.train(data);
      console.assert(Math.abs(gpuLoss - jsLoss) < 0.0001, "GPU/JS mismatch!");
    }

    return gpuLoss;
  }
}
```

**Why WebGPU fits Aesthetic Computer:**
- ✅ **Already have GPU context** - Reuse from graphics rendering
- ✅ **100-1000x faster** - Train during `sim()` without frame drops
- ✅ **Massively parallel** - Matrix ops in parallel
- ✅ **Still graspable** - Shader code is readable (not black box)
- ✅ **Mobile-ready** - WebGPU coming to iOS/Android
- ✅ **Unified compute** - Neural + graphics on same GPU

**Unified API (Progressive Enhancement)**
```javascript
// Pieces use same API, runtime picks best backend
net: {
  learn: {
    // Pure JS - always available (reference implementation)
    create: function(config) {
      return new MicroNet(config);
    },

    // GPU-accelerated - progressive enhancement
    createGPU: async function(config) {
      if (!navigator.gpu) {
        console.warn("WebGPU not available, using JS fallback");
        return new MicroNet(config);
      }
      return new MicroNetGPU(config, {
        gpuDevice: painting.gpu // Reuse AC's graphics GPU
      });
    },

    // Smart constructor - auto-detect best backend
    createAuto: async function(config) {
      return navigator.gpu
        ? await this.createGPU(config)
        : this.create(config);
    }
  }
}
```

**Piece usage (same code, different performance):**
```javascript
// rhythm-learner.mjs
let model;

async function boot({ net }) {
  // Week 1: Start with JS (grok the algorithm)
  model = net.learn.create({ layers: [16, 8, 4] });

  // Week 3: Just change one line for 100x speedup
  // model = await net.learn.createGPU({ layers: [16, 8, 4] });

  // Week 4: Auto-detect (GPU if available, JS fallback)
  // model = await net.learn.createAuto({ layers: [16, 8, 4] });
}

function sim() {
  // Same code works with all backends
  if (trainingData.length > 32) {
    model.train(trainingData.slice(-32));
  }
}
```

**Development workflow:**
1. **Implement in pure JS** (~1-2 weeks) - Grok it, test it, validate it
2. **Add WebGPU shaders** (~1-2 weeks) - Accelerate, validate against JS
3. **Ship both versions** - JS for compatibility, GPU for performance
4. **Progressive enhancement** - Pieces automatically get faster when GPU available

### 2.2 Proposed API Surface

#### **Level 1: Pure JavaScript API (Pieces)**

```javascript
// In disk.mjs, extend net object (line ~4000)
net: {
  // ... existing methods ...

  learn: {
    // Create new tiny model
    create: function(config) {
      // config: { layers: [inputSize, hidden, output], activation: 'relu' }
      return new MicroNet(config);
    },

    // Load saved weights
    load: async function(modelName) {
      const weights = await store.retrieve(`model:${modelName}`, "local:db");
      if (weights) return MicroNet.fromWeights(weights);
      return null;
    },

    // Save weights
    save: async function(model, modelName) {
      const weights = model.exportWeights();
      store[`model:${modelName}`] = weights;
      await store.persist(`model:${modelName}`, "local:db");
      return modelName;
    },
  },

  // Quick inference
  predict: function(model, input) {
    return model.forward(input);
  },

  // Weight access for visualization
  weights: {
    get: function(model, layerName) {
      return model.getLayer(layerName);
    },
    set: function(model, layerName, matrix) {
      model.setLayer(layerName, matrix);
    },
    visualize: function(model) {
      return model.toJSON(); // For debugging
    },
  },
}
```

**Usage in a piece:**
```javascript
// rhythm-learner.mjs
let model;
let trainingData = [];

function boot({ net, store }) {
  // Try to load saved model
  model = await net.learn.load("rhythm-model");

  // Create if doesn't exist
  if (!model) {
    model = net.learn.create({
      layers: [16, 8, 4],  // 16 inputs → 8 hidden → 4 outputs
      activation: 'relu'
    });
  }
}

function act({ event, net }) {
  // Gather training data from user typing
  if (event.is("keyboard:down")) {
    const timing = clock.time();
    const interval = timing - lastKeystroke;

    trainingData.push({
      input: extractRhythmFeatures(interval),
      output: predictNextInterval()
    });
  }
}

function sim({ net, store }) {
  // Train on buffered data
  if (trainingData.length > 32) {
    const batch = trainingData.slice(-32);
    model.train(batch, { epochs: 1, lr: 0.01 });

    // Save periodically
    if (Math.random() < 0.01) {
      net.learn.save(model, "rhythm-model");
    }
  }
}

function paint({ write, screen, net }) {
  // Visualize predictions
  const prediction = net.predict(model, currentRhythm);
  write(`Next interval: ${prediction}ms`, { center: "xy" });

  // Show weights as heatmap
  const weights = net.weights.get(model, "layer-0");
  visualizeWeightMatrix(weights, 0, 0);
}
```

#### **Level 2: KidLisp Primitives**

Add to `kidlisp.mjs` builtins object (after line ~5100):

```javascript
// Training primitive
"train": (api, args, env) => {
  // (train weights inputs outputs learning-rate)
  const weights = this.evaluate(args[0], api, env);
  const inputs = this.evaluate(args[1], api, env);
  const outputs = this.evaluate(args[2], api, env);
  const lr = this.evaluate(args[3], api, env) || 0.01;

  if (!Array.isArray(weights) || !Array.isArray(inputs)) {
    return weights;
  }

  // Simple Hebbian learning
  for (let i = 0; i < Math.min(weights.length, inputs.length); i++) {
    const delta = (outputs[i] - weights[i]) * inputs[i] * lr;
    weights[i] += delta;
  }

  return weights;
},

// Prediction primitive
"predict": (api, args, env) => {
  // (predict weights input)
  const weights = this.evaluate(args[0], api, env);
  const input = this.evaluate(args[1], api, env);

  if (!Array.isArray(weights)) return 0;

  let sum = 0;
  for (let i = 0; i < weights.length; i++) {
    sum += weights[i] * (Array.isArray(input) ? input[i % input.length] : input);
  }
  return Math.tanh(sum);
},

// Weight persistence
"save-weights": (api, args, env) => {
  // (save-weights weights name)
  const weights = this.evaluate(args[0], api, env);
  const name = this.evaluate(args[1], api, env);

  if (api.store) {
    api.store[`neural:${name}`] = weights;
    if (api.store.persist) {
      api.store.persist(`neural:${name}`, "local:db");
    }
  }

  return name;
},

"load-weights": (api, args, env) => {
  // (load-weights name)
  const name = this.evaluate(args[0], api, env);
  return api.store?.[`neural:${name}`] || [];
},
```

**KidLisp usage example:**
```lisp
; color-learner.lisp
(def my-weights (list 0.1 -0.2 0.3 0.4))

; Train on pen input
(on pen (x y)
  (def color-feature (/ y (screen :height)))
  (train my-weights (list color-feature) (list (/ x (screen :width))) 0.05))

; Paint with learned colors
(repeat 100 i
  (ink (* 255 (predict my-weights (list (/ i 100)))))
  (line (random (screen :width)) (random (screen :height))
        (random (screen :width)) (random (screen :height))))

; Save for later
(later 5000 (save-weights my-weights "color-model"))
```

#### **Level 3: Visualization Helpers**

Add to `$paintApi` (line ~4835 in disk.mjs):

```javascript
// Weight matrix heatmap
heatmap: function(matrix, x, y, width, height) {
  const rows = matrix.length;
  const cols = matrix[0]?.length || 1;
  const cellW = width / cols;
  const cellH = height / rows;

  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      const value = matrix[r][c];
      const color = Math.floor((value + 1) * 127.5);  // Map [-1,1] to [0,255]
      this.ink(color, 0, 255 - color);
      this.box(x + c * cellW, y + r * cellH, cellW, cellH);
    }
  }
},

// Neural network topology visualization
network: function(nodes, connections) {
  // nodes: [[layer0_nodes], [layer1_nodes], ...]
  // connections: [[weight_matrix_0_to_1], [weight_matrix_1_to_2], ...]

  const layerSpacing = screen.width / (nodes.length + 1);

  // Draw connections first (behind nodes)
  for (let l = 0; l < connections.length; l++) {
    const weights = connections[l];
    for (let i = 0; i < weights.length; i++) {
      for (let j = 0; j < weights[i].length; j++) {
        const weight = weights[i][j];
        const alpha = Math.abs(weight);
        this.ink(weight > 0 ? [0, 255, 0, alpha * 255] : [255, 0, 0, alpha * 255]);
        this.line(
          layerSpacing * (l + 1), nodeY(l, i),
          layerSpacing * (l + 2), nodeY(l + 1, j)
        );
      }
    }
  }

  // Draw nodes
  for (let l = 0; l < nodes.length; l++) {
    this.ink(255, 255, 255);
    for (let n = 0; n < nodes[l].length; n++) {
      this.circle(layerSpacing * (l + 1), nodeY(l, n), 5, "*");
    }
  }
},
```

### 2.3 HebbianNet Implementation (Core Neural Engine)

**PIVOT**: Using **Hebbian learning** instead of backprop for better philosophical alignment with Stiegler's idiotext concept.

**File:** `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/hebbian-net.mjs` (new file)

```javascript
// Pure JavaScript Hebbian neural network (~100 lines, zero dependencies)
// "Fire together, wire together" - matches idiotext weaving metaphor
// Inspired by microGPT philosophy + Stiegler's tertiary retention

export class HebbianNet {
  constructor(config) {
    this.size = config.size || 16;  // Number of neurons
    this.lr = config.lr || 0.01;     // Learning rate
    this.decay = config.decay || 0.9999;  // Weight decay (forgetting)

    // Initialize weight matrix (connections between neurons)
    // Small random weights instead of zero to break symmetry
    this.weights = Array(this.size).fill(0).map(() =>
      Array(this.size).fill(0).map(() => (Math.random() - 0.5) * 0.01)
    );

    // Trace of recent activations (for temporal learning)
    this.trace = Array(this.size).fill(0);
    this.traceDecay = config.traceDecay || 0.5;
  }

  // Activation function (sigmoid for bounded output)
  activate(x) {
    return 1 / (1 + Math.exp(-x));
  }

  // Forward pass through network
  forward(input) {
    // Input should be same size as network
    const padded = [...input, ...Array(this.size - input.length).fill(0)].slice(0, this.size);

    // Compute activations: a_i = σ(Σ w_ij * x_j)
    const activation = this.weights.map((row, i) => {
      let sum = 0;
      for (let j = 0; j < this.size; j++) {
        sum += row[j] * padded[j];
      }
      return this.activate(sum);
    });

    return activation;
  }

  // Hebbian learning: Δw_ij = η * a_i * a_j
  // "Neurons that fire together, wire together"
  train(input, output = null) {
    const inputActivation = this.forward(input);
    const outputActivation = output ?
      [...output, ...Array(this.size - output.length).fill(0)].slice(0, this.size) :
      inputActivation;  // Auto-associative if no output provided

    // Update weights based on correlation between activations
    for (let i = 0; i < this.size; i++) {
      for (let j = 0; j < this.size; j++) {
        // Hebbian update
        const delta = this.lr * inputActivation[i] * outputActivation[j];

        // Add trace-based temporal learning (wire in sequence → fire in sequence)
        const traceDelta = this.lr * inputActivation[i] * this.trace[j];

        // Apply both updates with weight decay
        this.weights[i][j] = this.weights[i][j] * this.decay + delta + traceDelta;
      }

      // Update trace for next timestep
      this.trace[i] = this.traceDecay * this.trace[i] + (1 - this.traceDecay) * outputActivation[i];
    }

    return inputActivation;
  }

  // Predict (just forward pass, no learning)
  predict(input) {
    return this.forward(input);
  }

  // Serialization
  exportWeights() {
    return {
      size: this.size,
      weights: this.weights,
      trace: this.trace,
      lr: this.lr,
      decay: this.decay,
      traceDecay: this.traceDecay,
    };
  }

  static fromWeights(data) {
    const net = new HebbianNet({ size: data.size });
    net.weights = data.weights;
    net.trace = data.trace || Array(data.size).fill(0);
    net.lr = data.lr;
    net.decay = data.decay;
    net.traceDecay = data.traceDecay || 0.5;
    return net;
  }

  // Measure associative strength between two patterns
  associationStrength(pattern1, pattern2) {
    const a1 = this.forward(pattern1);
    const a2 = this.forward(pattern2);

    // Compute correlation
    let sum = 0;
    for (let i = 0; i < this.size; i++) {
      sum += a1[i] * a2[i];
    }
    return sum / this.size;
  }

  toJSON() {
    return {
      size: this.size,
      params: this.size * this.size,
      weights: this.weights,
      meanWeight: this.weights.flat().reduce((a, b) => a + b, 0) / (this.size * this.size),
    };
  }
}
```

**Key features:**
- ~100 lines, zero dependencies (simpler than backprop!)
- **Hebbian learning**: "Fire together, wire together"
- **Unsupervised**: No explicit targets needed
- **Temporal**: "Wire in sequence → fire in sequence" via eligibility traces
- **Graspable**: Can inspect all weights, understand all updates
- **Philosophically aligned**: Matches Stiegler's "weaving" metaphor
- **Educational**: See how associations form in real-time

---

## Part 3: Implementation Methods

### 3.1 Storage Strategy (Digital Ocean Spaces + MongoDB)

**Your Existing Asset Buckets:**
- `art-aesthetic-computer` - Anonymous/guest content (pieces, paintings, tapes)
- `user-aesthetic-computer` - Authenticated user content
- `wand-aesthetic-computer` - Wand-specific assets
- CDN: `sfo3.digitaloceanspaces.com`

**Phase 1: Browser-only (Development)**
```javascript
// In-memory during session
let model = net.learn.create({ layers: [16, 8, 4] });

// Persist to IndexedDB on demand (browser-only)
await net.learn.save(model, "my-model");
// Uses existing store API with "local:db" method
```

**Phase 2: Cloud sync via Digital Ocean Spaces (Production)**

**Why Spaces instead of MongoDB:**
- ✅ No size limits (MongoDB has 16MB document cap)
- ✅ CDN-backed (fast global access like paintings/tapes)
- ✅ Already has presigned URL infrastructure (`presigned-url.js`)
- ✅ Same pattern as pieces (user vs. anonymous buckets)
- ✅ Cheaper storage costs

**Architecture:**
```javascript
// MongoDB stores METADATA only (like pieces/kidlisp)
// MongoDB collection: "neural-weights"
{
  code: string,           // nanoid
  user: ObjectId,         // @handles reference
  bucket: string,         // "user-aesthetic-computer" or "art-aesthetic-computer"
  path: string,           // "auth0|user123/neural/rhythm-model-v1.weights"
  architecture: object,   // { layers: [16, 8, 4], activation: 'relu' }
  trainedOn: Date,
  epochs: number,
  loss: number,
  params: number,         // Total parameter count
  hash: string,           // SHA256 for deduplication
  forkedFrom: string,     // Weight lineage
  hits: number,           // Load count
  size: number,           // File size in bytes
}

// Actual weight files stored in Digital Ocean Spaces:
// https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0|user123/neural/rhythm-model-v1.weights
```

**Implementation: Extend existing presigned-url.js**
```javascript
// In presigned-url.js, add neural-weights support (around line 150)
if (type === "neural-weights") {
  const bucket = user
    ? process.env.USER_SPACE_NAME || "user-aesthetic-computer"
    : process.env.ART_SPACE_NAME || "art-aesthetic-computer";

  const path = user
    ? `${user.sub}/neural/${name}.weights`
    : `neural/${name}.weights`;

  // Generate presigned PUT URL
  const command = new PutObjectCommand({
    Bucket: bucket,
    Key: path,
    ContentType: "application/json",
  });

  const url = await getSignedUrl(s3Client, command, { expiresIn: 3600 });

  return { url, bucket, path };
}
```

**Netlify function: /api/store-neural**
```javascript
// POST /api/store-neural
// Step 1: Get presigned URL
const presignedResponse = await fetch('/api/presigned-url', {
  method: 'POST',
  body: JSON.stringify({
    type: 'neural-weights',
    name: modelName,
  })
});

const { url, bucket, path } = await presignedResponse.json();

// Step 2: Upload weights to Spaces
await fetch(url, {
  method: 'PUT',
  body: JSON.stringify(weights),
  headers: { 'Content-Type': 'application/json' }
});

// Step 3: Save metadata to MongoDB
await db.collection('neural-weights').insertOne({
  code: nanoid(),
  user: userId,
  bucket,
  path,
  architecture: { layers, activation },
  hash: sha256(JSON.stringify(weights)),
  size: JSON.stringify(weights).length,
  trainedOn: new Date(),
  hits: 0
});

// GET /api/store-neural?code=xyz
// Returns metadata + CDN URL for weights
```

**Client-side usage:**
```javascript
// Save model
async function saveModel(model, modelName) {
  const weights = model.exportWeights();

  // POST to /api/store-neural (handles presigned URL + upload + DB)
  const response = await fetch('/api/store-neural', {
    method: 'POST',
    body: JSON.stringify({
      name: modelName,
      weights: weights,
      architecture: model.toJSON()
    })
  });

  return response.json(); // { code, url, bucket, path }
}

// Load model
async function loadModel(modelName) {
  // GET metadata from MongoDB
  const meta = await fetch(`/api/store-neural?code=${modelName}`);
  const { bucket, path } = await meta.json();

  // Fetch weights from Spaces CDN
  const weightsUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${path}`;
  const weights = await fetch(weightsUrl).then(r => r.json());

  return MicroNet.fromWeights(weights);
}
```

**Phase 3: Real-time sync (Advanced)**
Use session server Redis pub/sub for collaborative training:

```javascript
// In session-server/session.mjs
// Subscribe to weight updates
redis.subscribe("neural:weights:modelName", (data) => {
  // Broadcast to all connected clients
  channel.broadcast({ type: "neural:update", weights: data });
});
```

**Advantages of this approach:**
- Reuses existing infrastructure (Spaces, presigned URLs, MongoDB patterns)
- Same security model as pieces/paintings (user vs. anonymous buckets)
- No new services needed (just extend presigned-url.js and add store-neural.mjs)
- Scales to large models (GB+ if needed)
- CDN-backed for fast global access

### 3.2 Cross-Piece Weight Sharing

**Pattern 1: Fork and Train**
```javascript
// Load another user's trained model
const baseModel = await net.learn.load("@user/color-classifier");

// Train on your own data
baseModel.train(myData, { epochs: 10 });

// Save as your variant
await net.learn.save(baseModel, "my-color-classifier");
```

**Pattern 2: Ensemble Learning**
```javascript
// Load multiple models
const models = await Promise.all([
  net.learn.load("@alice/rhythm-model"),
  net.learn.load("@bob/rhythm-model"),
  net.learn.load("@charlie/rhythm-model"),
]);

// Ensemble prediction (voting)
const predictions = models.map(m => net.predict(m, input));
const final = predictions.reduce((a, b) => a + b) / models.length;
```

**Pattern 3: Weight Transfer (Like Genetic Algorithms)**
```javascript
// Crossover: Mix weights from two models
const parent1 = await net.learn.load("model-a");
const parent2 = await net.learn.load("model-b");

const child = net.learn.create({ layers: [16, 8, 4] });
const w1 = net.weights.get(parent1, "layer-0");
const w2 = net.weights.get(parent2, "layer-0");

// Average weights
const childWeights = w1.map((row, i) =>
  row.map((val, j) => (val + w2[i][j]) / 2)
);

net.weights.set(child, "layer-0", childWeights);
```

### 3.3 Aesthetic Ants Integration

**Ants as Neural Trainers:**

Add to `ants/score.md`:

```markdown
## Neural Training Tasks

**Task: Train generative color model**
- Scout: Fetch recent moods with color data
- Execute: Train 3-layer network on color patterns
- Verify: Generate 10 test images, check aesthetic coherence
- Commit: Save weights to `@ants/color-gen-v{date}`
- Pheromone: "trained color-gen loss=0.042"

**Task: Evolve rhythm predictor**
- Scout: Load recent clocks (melodies)
- Execute: Train LSTM-like recurrent weights
- Verify: Generate rhythm sequence, check musicality
- Commit: Save to `@ants/rhythm-gen-v{date}`
- Pheromone: "evolved rhythm-gen accuracy=0.87"
```

**Pheromone-based weight sharing:**
```fish
# In ants/colony.fish
function neural_ant
    # Read recent neural pheromones
    set -l recent_models (grep "trained" ants/pheromones.log | tail -5)

    # Load best model based on loss
    set -l best_model (echo $recent_models | sort -k2 -n | head -1)

    # Fork and improve
    # Train for 100 epochs
    # If better, commit new weights
    # If worse, revert and try different hyperparams
end
```

---

## Part 4: Example Use Cases

### 4.1 Micro-Organism Examples

#### **Rhythm Learner (10 parameters)**
```javascript
// Learns your typing rhythm, predicts next keystroke timing
const model = net.learn.create({ layers: [5, 2] });  // 10 params total
```

#### **Color Preference (15 parameters)**
```javascript
// Learns which colors you pick in wand
const model = net.learn.create({ layers: [3, 5] });  // 15 params: RGB → 5 hidden
```

#### **Stroke Style (30 parameters)**
```javascript
// Learns your drawing pressure/velocity patterns
const model = net.learn.create({ layers: [4, 6, 2] });  // 30 params
```

#### **Mood Classifier (50 parameters)**
```javascript
// Predicts mood from text features
const model = net.learn.create({ layers: [10, 5, 3] });  // 50 params
```

### 4.2 Emergent Colony Behaviors

**Scenario: Collective Color Palette**
1. 100 users each train 10-param color models on their own artwork
2. Models stored as `@user/color-palette`
3. New piece `collective-palette` loads all 100 models
4. Ensemble prediction creates "averaged aesthetic" of community
5. Emergent style without centralized training

**Scenario: Rhythm Evolution**
1. Ants train rhythm models on recent clocks
2. Each day, best-performing model (judged by user interactions) survives
3. Forked models mutate (add noise to weights)
4. After 30 days, evolved rhythm generator emerges
5. Colony discovers musical patterns without explicit programming

### 4.3 Educational Pieces

**`neural-paint.mjs`** - Learn by painting
- User paints with colors
- Network learns color → position mapping
- Generates new paintings in learned style
- Visualize all 20 weights as colored dots

**`predict-beat.mjs`** - Rhythm game
- User taps spacebar to beat
- Network learns rhythm pattern
- Predicts next tap timing
- Audio feedback: pitch = confidence

**`morph-shape.mjs`** - Shape interpolation
- User draws two shapes
- Network learns transformation
- Generates intermediate frames
- Visualize weights as transformation matrix

---

## Part 5: Implementation Roadmap

### Phase 1: Foundation (2-3 commits)

**Commit 1: Add MicroNet class**
- File: `system/public/aesthetic.computer/lib/micro-net.mjs`
- Pure JS neural network (~200 lines)
- Forward pass, backprop, serialization
- Zero dependencies

**Commit 2: Extend net API in disk.mjs**
- Add `net.learn.create()`
- Add `net.learn.load()` / `net.learn.save()`
- Add `net.predict()`
- Add `net.weights.get()` / `net.weights.set()`

**Commit 3: Add example piece**
- File: `system/public/aesthetic.computer/disks/micro-brain.mjs`
- 10-parameter network
- Trains on pen input
- Visualizes weights
- Uses localStorage for persistence

**Verification:**
```bash
npm test  # Should pass existing tests
# Manual: Visit aesthetic.computer/micro-brain
# Draw on screen, see weights update
# Reload, see weights persist
```

### Phase 2: Storage Backend - Digital Ocean Spaces (2-3 commits)

**Commit 4: Extend presigned-url.js for neural weights**
- File: `system/netlify/functions/presigned-url.js`
- Add `type === "neural-weights"` handler
- Generate presigned PUT URLs for Spaces upload
- Same pattern as existing media uploads

**Commit 5: Add /api/store-neural function**
- File: `system/netlify/functions/store-neural.mjs`
- MongoDB collection: `neural-weights` (metadata only)
- Actual weight files → Digital Ocean Spaces (art/user buckets)
- Endpoints: POST (upload via presigned URL + save metadata), GET (metadata + CDN URL)
- Deduplication via hash

**Commit 6: Update net.learn to use Spaces backend**
- `net.learn.save()` uses presigned URL to upload to Spaces, then POSTs metadata to /api/store-neural
- `net.learn.load()` fetches metadata, then loads weights from Spaces CDN URL
- Fallback to local:db if offline
- Cache weights in IndexedDB after loading from Spaces

**Verification:**
```bash
npm test
# Manual: Train model, publish to Spaces, load from different browser via CDN
# Check MongoDB has metadata, Spaces has weight file
```

### Phase 3: KidLisp Integration (1-2 commits)

**Commit 6: Add neural primitives to KidLisp**
- File: `system/public/aesthetic.computer/lib/kidlisp.mjs`
- Add: `train`, `predict`, `save-weights`, `load-weights`
- Expose `api.store` to KidLisp pieces

**Commit 7: Add KidLisp example**
- File: `system/public/aesthetic.computer/disks/neural-colors.lisp`
- Trains on pen input
- Saves/loads weights
- Demonstrates primitives

**Verification:**
```bash
npm run test:kidlisp
# Should include new primitive tests
```

### Phase 4: Visualization & Polish (1-2 commits)

**Commit 8: Add visualization helpers**
- File: `system/public/aesthetic.computer/lib/disk.mjs`
- Add `heatmap()` to $paintApi
- Add `network()` topology visualizer

**Commit 9: Add advanced example**
- File: `system/public/aesthetic.computer/disks/neural-ensemble.mjs`
- Loads multiple models
- Ensemble prediction
- Weight visualization
- Demonstrates cross-piece learning

**Verification:**
```bash
npm test
npm run test:perf  # Check no regression
```

### Phase 5: WebGPU Acceleration (Optional, 2-3 commits)

**Goal:** Add GPU-accelerated neural training (100-1000x faster than pure JS)

**Prerequisites:**
- Pure JS MicroNet working and tested
- Understand the algorithm from Phase 1
- AC already has WebGPU for graphics (reuse the context)

**Commit 10: Add WebGPU compute shaders**
- File: `system/public/aesthetic.computer/lib/micro-net-gpu.mjs`
- Write forward pass shader (~50 lines WGSL)
- Write backward pass shader (~100 lines WGSL)
- Matrix multiplication in parallel on GPU
- Reuse AC's existing WebGPU device from graphics

**Commit 11: Add MicroNetGPU class**
- File: `system/public/aesthetic.computer/lib/micro-net-gpu.mjs`
- Same API as MicroNet (drop-in replacement)
- Compile shaders on construction
- Upload/download weights via GPU buffers
- Validate against JS version (DEBUG mode)

**Commit 12: Extend net API for GPU**
- File: `system/public/aesthetic.computer/lib/disk.mjs`
- Add `net.learn.createGPU(config)`
- Add `net.learn.createAuto(config)` - picks best backend
- Automatic fallback to JS if no WebGPU support
- Pass existing GPU device from painting context

**Example WebGPU compute shader (forward pass):**
```wgsl
// Forward pass: y = activation(W * x + b)
@group(0) @binding(0) var<storage, read> weights: array<f32>;
@group(0) @binding(1) var<storage, read> biases: array<f32>;
@group(0) @binding(2) var<storage, read> input: array<f32>;
@group(0) @binding(3) var<storage, read_write> output: array<f32>;

@compute @workgroup_size(64)
fn forward(@builtin(global_invocation_id) id: vec3<u32>) {
  let idx = id.x;
  if (idx >= arrayLength(&output)) { return; }

  var sum = biases[idx];
  for (var i = 0u; i < arrayLength(&input); i++) {
    sum += weights[idx * arrayLength(&input) + i] * input[i];
  }

  // ReLU activation
  output[idx] = max(0.0, sum);
}
```

**Verification:**
```bash
npm test
# Manual: Visit aesthetic.computer/micro-brain
# Switch between JS and GPU versions
# Verify same results, measure speedup
# Typical: 100-1000x faster for matrix ops
```

**Performance expectations:**
- 10-param model: JS ~5ms/batch → GPU ~0.05ms/batch (100x)
- 100-param model: JS ~50ms/batch → GPU ~0.5ms/batch (100x)
- 1000-param model: JS ~500ms/batch → GPU ~2ms/batch (250x)

**Notes:**
- Start with this AFTER Phase 1-4 are solid
- Grok the JS version first (educational value)
- GPU version for production performance
- Both versions maintained (JS = reference, GPU = optimization)

### Phase 6: Ants Automation (Optional, 1 commit)

**Commit 13: Add neural training ant**
- File: `ants/score.md` (add task)
- File: `ants/train-neural-ant.fish` (new script)
- Trains models on recent user data
- Commits weights if tests pass
- Pheromone trail for other ants
- Can use GPU version for faster training

---

## Part 6: Technical Considerations

### 6.1 Performance Constraints

**Browser Limits:**
- IndexedDB: 50MB → ~12M floats (plenty for tiny networks)
- Main thread: Training should be non-blocking
- WebWorker: Consider moving training to worker for large batches

**Optimization Strategy:**
1. Start with tiny models (10-100 params)
2. Train in sim() with frame budget (max 16ms)
3. Offload to worker if training > 100ms
4. Use Float32Array for weight storage (4x smaller than Array)

**Example frame-budgeted training:**
```javascript
function sim({ clock, net }) {
  const startTime = performance.now();

  // Train until frame budget exhausted
  while (performance.now() - startTime < 10 && trainingQueue.length > 0) {
    const batch = trainingQueue.splice(0, 8);
    model.train(batch, { epochs: 1 });
  }
}
```

### 6.2 Security Considerations

**Malicious Weights:**
- Weights are just numbers (no code execution risk)
- But: Could embed data in weights (steganography)
- Solution: Hash-based deduplication, size limits

**Privacy:**
- Weights could encode training data
- Solution: Local-first by default, opt-in cloud sync
- Users control when/if weights are published

**Abuse Prevention:**
- Rate limit /api/store-neural (100 saves/hour/user)
- Max weight size: 1MB per model
- Quota: 10 models per user (free tier)

### 6.3 Testing Strategy

**Unit Tests (Jasmine):**
```javascript
// spec/micro-net.spec.js
describe("MicroNet", () => {
  it("should initialize with random weights", () => {
    const net = new MicroNet({ layers: [2, 3, 1] });
    expect(net.weights.length).toBe(2);
    expect(net.weights[0].length).toBe(2);
    expect(net.weights[0][0].length).toBe(3);
  });

  it("should forward pass correctly", () => {
    const net = new MicroNet({ layers: [2, 1] });
    const output = net.forward([1, 0]);
    expect(output.length).toBe(1);
    expect(typeof output[0]).toBe("number");
  });

  it("should train and reduce loss", () => {
    const net = new MicroNet({ layers: [2, 1] });
    const data = [
      { input: [0, 0], output: [0] },
      { input: [1, 1], output: [1] },
    ];

    const loss1 = net.train(data, { epochs: 1 });
    const loss2 = net.train(data, { epochs: 10 });

    expect(loss2).toBeLessThan(loss1);
  });

  it("should serialize and deserialize", () => {
    const net1 = new MicroNet({ layers: [3, 2] });
    const exported = net1.exportWeights();
    const net2 = MicroNet.fromWeights(exported);

    expect(net2.weights).toEqual(net1.weights);
  });
});
```

**Integration Tests:**
```javascript
// spec/neural-piece.spec.js
describe("Neural Piece Integration", () => {
  it("should create model via net.learn", async () => {
    const model = net.learn.create({ layers: [4, 2] });
    expect(model).toBeDefined();
    expect(model.layers).toEqual([4, 2]);
  });

  it("should persist and load from store", async () => {
    const model = net.learn.create({ layers: [2, 1] });
    await net.learn.save(model, "test-model");

    const loaded = await net.learn.load("test-model");
    expect(loaded.weights).toEqual(model.weights);
  });
});
```

**Performance Tests:**
```javascript
// spec/neural-perf.spec.js
describe("Neural Performance", () => {
  it("should train 10-param model in <16ms", () => {
    const model = new MicroNet({ layers: [5, 2] });
    const data = Array(32).fill(0).map(() => ({
      input: [Math.random(), Math.random(), Math.random(), Math.random(), Math.random()],
      output: [Math.random(), Math.random()],
    }));

    const start = performance.now();
    model.train(data, { epochs: 1 });
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(16);  // Frame budget
  });
});
```

---

## Part 7: Philosophy Alignment

### 7.1 Aesthetic Ants Principles

✅ **Signal over noise:** Neural weights create visible behaviors (generative art, predictions)
✅ **Graspable changes:** 10-100 parameters you can visualize and understand
✅ **Small & verified:** Each model trains incrementally, tests verify no breakage
✅ **Wandering is valid:** Failed training (high loss) is informative, gets logged

### 7.2 Aesthetic Computer Values

✅ **Musical instrument metaphor:** Train your piece like tuning an instrument
✅ **Memorizable paths:** `@user/rhythm-model` becomes discoverable
✅ **Social/shareable:** Fork weights like forking pieces
✅ **Educational:** See exactly how learning works (no black boxes)
✅ **Mobile-first:** Tiny models work on phones, no GPU needed

### 7.3 microGPT Philosophy

✅ **Radical minimalism:** Pure JavaScript, zero dependencies
✅ **Transparency:** All weights inspectable, all gradients traceable
✅ **Educational art:** Understanding > performance
✅ **Irreducible essence:** MicroNet contains all necessary algorithmic content

---

## Part 8: Future Possibilities

### 8.1 Advanced Features (Post-MVP)

**Recurrent Networks (LSTM-like):**
```javascript
net.learn.create({
  layers: [16, 8, 4],
  recurrent: true,  // Add hidden state feedback
});
```

**Convolutional Layers:**
```javascript
net.learn.create({
  layers: [
    { type: 'conv', filters: 8, kernel: 3 },
    { type: 'dense', size: 16 },
    { type: 'dense', size: 4 },
  ]
});
```

**Transfer Learning:**
```javascript
// Freeze base layers, train only top layers
const baseModel = await net.learn.load("@pretrained/feature-extractor");
baseModel.freeze(["layer-0", "layer-1"]);
baseModel.train(myData);  // Only updates layer-2
```

**Federated Learning:**
```javascript
// Train locally, only share gradients (not data)
const localGradients = model.computeGradients(myPrivateData);
net.share(localGradients);  // Upload gradients, not training data

// Server aggregates gradients from multiple users
// Downloads updated model back to users
```

### 8.2 Creative Applications

**Generative Music:**
- Train on user's clock pieces
- Generate new melodies in learned style
- Ensemble of ants' rhythm models

**Collaborative Art:**
- Each user trains color/stroke model
- Session server aggregates predictions
- Multiplayer painting with collective aesthetic

**Mood Synthesis:**
- Train on user's mood history
- Predict mood from piece interactions
- Generate mood-appropriate visuals/sounds

**Piece Recommendation:**
- Train on piece interaction patterns
- Predict which pieces user will enjoy
- Emergent curation from collective preferences

---

## Summary: Implementation Checklist

### Minimal Viable Neural Primitives (8 commits)

**Phase 1: Foundation - Hebbian Learning (3 commits)**
- [ ] **Commit 1:** Add `hebbian-net.mjs` (~100 lines, pure JS, zero dependencies, Hebbian learning)
- [ ] **Commit 2:** Extend `net` API in `disk.mjs` (learn, predict, weights)
- [ ] **Commit 3:** Add example piece `hebbian-memory.mjs` (associative memory demo, IndexedDB persistence)

**Phase 2: Cloud Storage via Digital Ocean Spaces (3 commits)**
- [ ] **Commit 4:** Extend `presigned-url.js` for neural-weights type
- [ ] **Commit 5:** Add `/api/store-neural` function (Spaces upload + MongoDB metadata)
- [ ] **Commit 6:** Update `net.learn` to use Spaces backend with CDN loading

**Phase 3: KidLisp + Tests (2 commits)**
- [ ] **Commit 7:** Add Jasmine tests for MicroNet (`spec/micro-net.spec.js`)
- [ ] **Commit 8:** Add KidLisp primitives (train, predict, save/load-weights)

**Phase 4: WebGPU Acceleration (Optional, 3 commits)**
- [ ] **Commit 10:** Write WebGPU compute shaders for forward/backward pass (~150 lines WGSL)
- [ ] **Commit 11:** Add `micro-net-gpu.mjs` - GPU-accelerated version (~300 lines)
- [ ] **Commit 12:** Extend `net` API with `createGPU()` and `createAuto()`

### Verification After Each Commit

```bash
npm test                    # All existing tests pass
npm run test:kidlisp        # KidLisp tests pass (if applicable)
npm run aesthetic           # Dev server runs
# Manual: Visit piece, verify behavior
git commit -m "ant: add neural primitive X, tests pass"
```

### Critical Files to Modify

**Core Implementation (Phases 1-3):**

| File | Lines | Changes |
|------|-------|---------|
| `system/public/aesthetic.computer/lib/micro-net.mjs` | NEW | MicroNet class - pure JS reference (~200 lines) |
| `system/public/aesthetic.computer/lib/disk.mjs` | 3953-4117 | Extend `net` object with `learn` API |
| `system/netlify/functions/presigned-url.js` | ~150 | Add neural-weights handler |
| `system/netlify/functions/store-neural.mjs` | NEW | Spaces upload + MongoDB metadata (~150 lines) |
| `system/public/aesthetic.computer/lib/kidlisp.mjs` | ~5100 | Add 4 neural builtins |
| `system/public/aesthetic.computer/disks/micro-brain.mjs` | NEW | Example piece (~100 lines) |
| `spec/micro-net.spec.js` | NEW | Unit tests (~50 lines) |
| `system/backend/database.mjs` | - | Add `neural-weights` collection |

**WebGPU Acceleration (Optional Phase 4):**

| File | Lines | Changes |
|------|-------|---------|
| `system/public/aesthetic.computer/lib/micro-net-gpu.mjs` | NEW | GPU-accelerated version (~300 lines + shaders) |
| `system/public/aesthetic.computer/lib/disk.mjs` | 3953-4117 | Add `createGPU()`, `createAuto()` |
| `spec/micro-net-gpu.spec.js` | NEW | GPU tests + validation vs JS (~100 lines) |

---

## Conclusion

**Feasibility:** Highly viable with minimal changes
**Effort:** ~8 focused commits (core) + 3 optional (GPU acceleration)
**Risk:** Low (new APIs, no breaking changes, reuses existing infrastructure)
**Educational Value:** High (demystifies ML via graspable JS implementation)
**Social Potential:** High (shareable learned behaviors via CDN)
**Alignment:** Perfect match for AC's ethos
**Infrastructure:** Leverages existing Digital Ocean Spaces + WebGPU context
**Performance:** Progressive enhancement (JS baseline → optional GPU acceleration)

This approach takes microGPT's philosophy of transparent, minimal neural networks and adapts it to Aesthetic Computer's social, creative, educational platform. Instead of one toy GPT, we get an ecosystem of tiny, specialized, user-owned neural micro-organisms that learn, share, and evolve together.

**Key advantages:**

1. **Graspable first, fast second**: Pure JavaScript reference implementation lets you grok the algorithm before optimizing with WebGPU. This matches AC's educational philosophy while enabling production performance.

2. **Spaces integration**: Using your existing Digital Ocean Spaces infrastructure (the same buckets that store pieces, paintings, and tapes), neural weights get CDN-backed global distribution with no size limits. This is a huge win over MongoDB-only storage and aligns perfectly with how you already handle user-created content.

3. **Progressive enhancement**: Same API works with both JS and GPU backends. Pieces automatically get faster when GPU is available, but still work everywhere. Start simple, optimize later.

4. **Zero new dependencies**: Pure JavaScript for baseline, WebGPU compute shaders for acceleration (reusing AC's existing GPU context). No TensorFlow.js, no external ML libraries.

**The result:** Emergent intelligence from many small learners, not one giant model. Like Aesthetic Ants, but for neural networks. Grok it with JS, then accelerate it with GPU when you're ready.
