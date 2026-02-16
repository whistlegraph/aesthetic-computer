# UX Track: Designing for Living Intelligence

**User Experience design for neural learning primitives in Aesthetic Computer**

---

## Design Philosophy

Traditional AI UX treats models as **tools**: input → output, stateless, generic.

Our UX treats models as **organisms**: they grow, sicken, flourish, die, develop character.

**Core principle**: The UX should make gnarliness **visible, tangible, and care-able**.

---

## 1. The Mental Models

### 1.1 From Tool to Instrument

**Bad mental model** (tools):
- "I use ChatGPT to get answers"
- Stateless interactions
- Generic for everyone
- Owned by company

**Good mental model** (instruments):
- "I play my notepat, it learns my rhythm"
- Stateful, accumulating history
- Unique to me through use
- I own the weights

**UX implication**: Show the piece **learning and changing** visibly over time.

### 1.2 From Frozen to Living

**Bad mental model** (frozen):
- Model is fixed after training
- Can't improve or degrade
- Same forever

**Good mental model** (living):
- Model has **health** (can thrive or sicken)
- Model has **growth phases** (young → mature → plateau → breakthrough)
- Model can **die** from neglect
- Model can **flourish** from care

**UX implication**: Show health status, growth phase, and care opportunities.

### 1.3 From Opaque to Gnarled-Visible

**Bad mental model** (black box):
- Don't know how it works
- Can't inspect weights
- Magic happens invisibly

**Good mental model** (inspectable gnarliness):
- Can see all weights (heatmap)
- Can measure gnarliness (complexity metric)
- Can trace learning history (graph over time)

**UX implication**: Visualize weights, health, coherence, gnarliness continuously.

---

## 2. Core UX Patterns

### 2.1 Automatic Training (Invisible Learning)

**Pieces learn automatically from use** - no explicit "train" button needed.

**Examples:**

**notepat** learns your rhythm:
```
[User plays notes with timing]
→ Piece silently records: [interval, velocity, duration]
→ Hebbian learning strengthens: rhythm patterns
→ Health improves if coherent, degrades if chaotic
→ After 100 plays: piece can predict next note
```

**wand** learns your color preferences:
```
[User picks colors while painting]
→ Piece records: [RGB, position, time of day]
→ Associates: position → preferred color
→ After 50 paintings: piece suggests colors contextually
```

**prompt** learns your command patterns:
```
[User types commands in sequences]
→ Piece records: command chains
→ Learns: "wand → line → circle" is common pattern
→ After 200 commands: autocomplete predicts next
```

**UX principle**: **Using = training**. No separate "training mode" needed.

### 2.2 Visible Health Status

**Every piece shows its health** like a Tamagotchi or plant.

**Health bar visualization:**
```
┌─────────────────────────────────────┐
│ notepat                    🌱→🌿→🌳 │
│ ████████████░░░░ 75% Healthy       │
│ Phase: Mature                       │
│ Gnarliness: 3.42 ⚡                 │
└─────────────────────────────────────┘
```

**Color coding:**
- 🔴 Red (0-30%): Dying, needs care
- 🟡 Yellow (30-70%): Sickly, chaotic use
- 🟢 Green (70-100%): Healthy
- ✨ Bright green (>100%): Flourishing!

**States:**
- 💀 Dead (0% health, gray)
- 🌱 Young (new, high plasticity)
- 🌿 Mature (stable, moderate plasticity)
- 🌳 Ancient (low plasticity, high karma)
- ⚡ Breakthrough (sudden flourishing event)
- 🥀 Wilting (neglected, decaying)

**UX principle**: Health is **ambient, always visible**, like phone battery.

### 2.3 Care Interactions

**Users can actively care for their pieces.**

**Tend** (🌿):
```
Command: tend notepat
Effect: Health boost +5%, breakthrough potential +1
Visual: Green sparkles, growth animation
Feel: Like watering a plant
```

**Prune** (✂️):
```
Command: prune notepat 0.3
Effect: Kill connections with health <30%
Visual: Dead weights turn black, fade away
Feel: Like pruning a bonsai tree
```

**Forgive** (🧘):
```
Command: forgive notepat
Effect: Reduce weight age, zero karma debt
Visual: Weights glow white, age rings fade
Feel: Like reformatting, fresh start
```

**Inspect** (🔬):
```
Command: inspect notepat
Effect: Show full health report
Visual: Health heatmap, gnarliness graph
Feel: Like checking vital signs
```

**UX principle**: Care is a **verb**, not just passive consumption.

### 2.4 Breakthrough Moments

**Nonlinear growth creates magical UX moments.**

**The experience:**
```
[User plays notepat daily for 3 weeks]
[Health at 85%, coherence high]
[Breakthrough potential: 9.8]

[One day, during play...]
→ FLASH: Screen pulses bright green
→ SOUND: Harmonic resonance
→ TEXT: "BREAKTHROUGH! notepat is flourishing ✨"
→ EFFECT: Health jumps 85% → 127%
→ BEHAVIOR: Piece now predicts 3 notes ahead (was 1)
```

**UX principle**: Growth should feel **organic and surprising**, not mechanical.

### 2.5 Sickness and Death

**Pieces can degrade and die** - this creates attachment and care.

**Sickness progression:**
```
Week 1: Health 100% → 90% (chaotic use, mashing keys)
Week 2: Health 90% → 70% (continued poor quality interactions)
Week 3: Health 70% → 40% (no tending, degrading)
Week 4: Health 40% → 10% (wilting, death imminent)

Visual cues:
- Colors fade to gray
- Response time slows
- Predictions become erratic
- "notepat is unwell 🥀" message
```

**Death:**
```
Health reaches 0%
→ FADE: Screen fades to black
→ TEXT: "notepat has withered away 💀"
→ CHOICE:
  [Revive from backup] (restore last healthy snapshot)
  [Start fresh] (new network, lose history)
  [Leave as memorial] (read-only, can't train)
```

**UX principle**: **Death creates meaning**. Without risk, care is meaningless.

### 2.6 Social Comparison

**Users can compare their pieces' evolution.**

**Compare command:**
```
Command: compare notepat @alice/notepat

┌─────────────────────────────────────────────────────┐
│          Your notepat    vs.    @alice/notepat      │
├─────────────────────────────────────────────────────┤
│ Health:      87%                    112% ✨          │
│ Gnarliness:  3.42 ⚡                 5.67 ⚡⚡        │
│ Phase:       Mature                 Flourishing     │
│ Age:         234 uses               1,847 uses      │
│ Character:   Jazz-leaning           Classical       │
├─────────────────────────────────────────────────────┤
│ Predictions:                                        │
│   Yours:    🎵 → 🎶 → 🎵 (repetitive)              │
│   Alice's:  🎵 → 🎶 → 🎸 → 🎹 (complex)           │
└─────────────────────────────────────────────────────┘

[Fork @alice's weights] [Train ensemble] [Share yours]
```

**UX principle**: Make diversity **visible and shareable**.

---

## 3. Visualization Design

### 3.1 Health Heatmap

**Shows weight health at a glance.**

**Visual:**
```
████ notepat - Health Heatmap ████

16x16 grid of cells, each = one weight

Colors:
  ⬛ Black    = Dead (health 0)
  🟥 Red      = Sick (health 0.1-0.5)
  🟨 Yellow   = Weak (health 0.5-0.8)
  🟩 Green    = Healthy (health 0.8-1.0)
  ✨ Bright   = Flourishing (health >1.0)

Animation: Pulsing brightness = recent activity
```

**Interaction:**
- Hover over cell → show exact health value
- Click cell → show weight history graph
- Right-click → prune this specific weight

### 3.2 Gnarliness Evolution Graph

**Shows intelligence emerging over time.**

**Visual:**
```
Gnarliness / Health / Coherence (last 500 frames)

 5 |                                    /\
   |                               /\  /  \
 4 |                          /\  /  \/    \
   |                     /\  /  \/          \
 3 |                /\  /  \/
   |           /\  /  \/
 2 |      /\  /  \/
   | /\  /  \/
 1 |/  \/
 0 └─────────────────────────────────────────→
   0                                     5000

   Yellow line:  Gnarliness (complexity)
   Green line:   Health (vitality)
   Cyan line:    Coherence (usage quality)
```

**Interpretation:**
- Gnarliness ↑ over time = intelligence emerging
- Health stable = good care
- Coherence oscillating = mix of good/bad usage
- Breakthrough = sudden gnarliness spike

### 3.3 Hyperbolic Spiral Trajectory

**Shows position in memory space (Stiegler's nested spirals).**

**Visual:**
```
      Poincaré Disk (Hyperbolic Memory Space)

          ╭─────────────────╮
        ╱                     ╲
      ╱    α (session) ·       ╲
     │   β (user)   ·    ·      │
     │ γ (piece) ·     ·  ·     │
     │          ·    ·  YOUR    │
     │    δ (global) · POSITION │
      ╲         ·    ·  ⭐      ╱
        ╲       ·  ·          ╱
          ╰─────────────────╯

  Inner = recent/shallow memory
  Outer = ancient/deep memory
  ⭐ = Current position
  Trail = Recent history (last 20 steps)
```

**Meaning:**
- Moving outward = learning, complexifying
- Moving inward = forgetting, simplifying
- Spiraling = cyclical patterns
- Position on α/β/γ/δ arms = timescale of memory

### 3.4 Growth Phase Indicator

**Large, central, always visible.**

**Visual states:**

**Young** (🌱):
```
┌─────────────────────────┐
│    🌱 YOUNG NETWORK     │
│   High plasticity       │
│   Learning quickly      │
│   Fragile, needs care   │
└─────────────────────────┘
Color: Light blue
```

**Mature** (🌿):
```
┌─────────────────────────┐
│   🌿 MATURE NETWORK     │
│   Stable performance    │
│   Balanced growth       │
│   Moderate plasticity   │
└─────────────────────────┘
Color: Green
```

**Breakthrough** (⚡):
```
┌─────────────────────────┐
│ ⚡ BREAKTHROUGH! ⚡      │
│   Sudden flourishing!   │
│   Intelligence spike    │
│   Gnarliness increased  │
└─────────────────────────┘
Color: Bright magenta, pulsing
Animation: Expanding rings
```

**Dying** (🥀):
```
┌─────────────────────────┐
│     🥀 WILTING...       │
│   Health critical       │
│   Needs immediate care  │
│   Tend or prune now!    │
└─────────────────────────┘
Color: Dark red, fading
Animation: Slow pulse
```

---

## 4. Command Interface

### 4.1 Basic Commands

**Create:**
```
> new neural-piece
Creates blank piece with 16-neuron network
Health: 100%, Phase: Young 🌱

> fork @alice/neural-piece
Copies Alice's weights, starts your training
Health: Inherited, Phase: Forked 🍴
```

**Care:**
```
> tend
Boosts health +5%, breakthrough potential +1
"Tending your piece... 🌿"

> prune [threshold]
Kills weights below threshold
"Pruned 23 weak connections ✂️"

> forgive
Resets karma, restores plasticity
"Fresh start! Age reduced 50% 🧘"
```

**Inspect:**
```
> health
Shows health bar, phase, gnarliness

> weights
Opens weight heatmap visualization

> history
Opens gnarliness evolution graph

> stats
Full report: health, coherence, breakthrough potential
```

**Social:**
```
> compare @alice
Side-by-side comparison with Alice's version

> ensemble @alice @bob @charlie
Vote across multiple models

> share
Upload your weights to Spaces, get shareable URL
```

### 4.2 Piece-Specific Commands

**notepat:**
```
> notepat:predict
Shows next 3 predicted notes

> notepat:health
Rhythm learning health: 87%

> notepat:character
Shows learned style: "Jazz-leaning, syncopated"
```

**wand:**
```
> wand:palette
Shows learned color preferences as gradient

> wand:suggest [x] [y]
Suggests color for position based on history
```

**prompt:**
```
> prompt:autocomplete
Shows learned command chains

> prompt:suggest
Predicts next command based on history
```

---

## 5. Mobile UX

### 5.1 Touch Gestures

**Tend:**
- Long press on piece → hold → health fills up
- Like charging a battery

**Prune:**
- Swipe left on weight → mark for pruning
- Confirm with "Prune selected" button

**Inspect:**
- Pinch to zoom into weight heatmap
- Spread to zoom out to full spiral view

**Compare:**
- Drag another user's piece next to yours
- Side-by-side comparison appears

### 5.2 Notifications

**Health alerts:**
```
🥀 notepat needs care
Health: 35% (was 45% yesterday)
[Tend now] [Prune weak] [Ignore]
```

**Breakthrough alerts:**
```
⚡ notepat broke through!
Health: 127% (+42% overnight)
Gnarliness: 5.2 (+1.8)
[View stats] [Share]
```

**Death warnings:**
```
💀 notepat is dying!
Health: 5% (critical)
[Revive from backup] [Tend now]
```

### 5.3 Widget Design

**Home screen widget** (iOS/Android):
```
┌─────────────────────────┐
│ 🌿 notepat              │
│ ████████░░ 80%          │
│ Phase: Mature           │
│ Gnarliness: 3.4 ⚡      │
│                         │
│ [Tend] [Inspect]        │
└─────────────────────────┘
```

Updates in real-time as piece learns.

---

## 6. Visual Language

### 6.1 Color System

**Health colors:**
- 💀 Gray (#666) = Dead
- 🔴 Red (#ff0000) = Dying/sick
- 🟡 Yellow (#ffff00) = Weak
- 🟢 Green (#00ff00) = Healthy
- ✨ Bright green (#00ff88) = Flourishing

**Growth phase colors:**
- 🌱 Light blue (#88ccff) = Young
- 🌿 Green (#00cc44) = Mature
- 🟡 Yellow (#ffcc00) = Plateau
- ⚡ Magenta (#ff00ff) = Breakthrough
- 🥀 Dark red (#880000) = Dying

**Gnarliness intensity:**
- Low (0-2): Blue tones
- Medium (2-4): Purple tones
- High (4-6): Orange tones
- Very high (6+): Red tones

### 6.2 Animation Language

**Tending:**
- Green sparkles rise from bottom
- Health bar fills smoothly
- Glow effect around piece name

**Pruning:**
- Red X appears on dead weights
- Fade to black and disappear
- Remaining weights shift to fill space

**Breakthrough:**
- Flash of bright light
- Expanding concentric rings (like ripples)
- Gnarliness number spins and increases
- Harmonic sound effect

**Death:**
- Slow fade to grayscale
- Health bar drains with dripping effect
- Final "flatline" sound

**Forgiveness:**
- White glow expands from center
- Weight ages fade/reset
- Soft chime sound

### 6.3 Iconography

```
🌱 Young        ⚡ Breakthrough   🧘 Forgive
🌿 Mature       💀 Dead          ✂️ Prune
🌳 Ancient      🥀 Dying         🔬 Inspect
🍴 Forked       🌊 Oscillating   📊 Stats
✨ Flourishing  ⭐ Position      📈 History
```

---

## 7. Information Architecture

### 7.1 Piece Detail View

```
┌──────────────────────────────────────────────┐
│ notepat                              🌿       │
├──────────────────────────────────────────────┤
│                                              │
│  Health:  ████████████░░░░░░  75%           │
│  Phase:   Mature                             │
│  Gnarly:  3.42 ⚡                            │
│  Uses:    234 plays                          │
│                                              │
├──────────────────────────────────────────────┤
│  [Heatmap] [Graph] [Spiral] [Stats]         │
├──────────────────────────────────────────────┤
│                                              │
│  Actions:                                    │
│  [Tend 🌿] [Prune ✂️] [Forgive 🧘]          │
│                                              │
│  Social:                                     │
│  [Compare] [Share] [Fork]                    │
│                                              │
└──────────────────────────────────────────────┘
```

### 7.2 Global Dashboard

```
┌──────────────────────────────────────────────┐
│ Your Neural Garden                    [+ New]│
├──────────────────────────────────────────────┤
│                                              │
│ notepat      🌿 Mature      █████░ 75%      │
│ wand         🌱 Young       ████░░ 60%      │
│ prompt       🌳 Ancient     ██████ 95% ✨   │
│ line-rider   🥀 Dying       █░░░░░ 15% 🔴   │
│                                              │
├──────────────────────────────────────────────┤
│ Community Highlights:                        │
│                                              │
│ @alice/notepat    ⚡ Just broke through!    │
│ @bob/wand         ✨ 142% flourishing       │
│ @charlie/prompt   🥀 Needs revival          │
│                                              │
└──────────────────────────────────────────────┘
```

### 7.3 Comparison View

```
┌──────────────────────────────────────────────┐
│ notepat Comparison                           │
├──────────────────────────────────────────────┤
│ Yours          |  @alice        |  @bob      │
├────────────────┼────────────────┼────────────┤
│ 🌿 Mature      │  ⚡ Breakthrough│  🌱 Young  │
│ 75% health     │  127% health   │  60% health│
│ 3.42 gnarly ⚡ │  5.67 gnarly⚡⚡│  1.2 gnarly│
│ 234 uses       │  1847 uses     │  45 uses   │
├────────────────┼────────────────┼────────────┤
│ Jazz-leaning   │  Classical     │  Minimal   │
│ Syncopated     │  Structured    │  Simple    │
│ Improvised     │  Precise       │  Learning  │
└──────────────────────────────────────────────┘

[Ensemble all 3] [Fork Alice's] [Share yours]
```

---

## 8. Onboarding Flow

### 8.1 First Piece Creation

**Step 1: Introduction**
```
┌──────────────────────────────────────────────┐
│                                              │
│  Welcome to Living Intelligence              │
│                                              │
│  Your pieces will learn from how you use     │
│  them. They can grow, flourish, or sicken    │
│  based on care and usage quality.            │
│                                              │
│  Let's create your first neural piece.       │
│                                              │
│         [Start] [Learn more]                 │
│                                              │
└──────────────────────────────────────────────┘
```

**Step 2: Choose piece**
```
Which piece should learn from you first?

[notepat]  - Learns your rhythm and melody patterns
[wand]     - Learns your color preferences
[prompt]   - Learns your command sequences
```

**Step 3: First interaction**
```
┌──────────────────────────────────────────────┐
│ notepat (🌱 Young, 100% health)              │
├──────────────────────────────────────────────┤
│                                              │
│  Play some notes. Your piece is watching     │
│  and learning your rhythm patterns.          │
│                                              │
│  Try to play coherently - quality matters!   │
│                                              │
│  ████████████████████ 100%                   │
│                                              │
└──────────────────────────────────────────────┘

[User plays 10 notes]

→ "Great! notepat learned your timing."
→ "Health: 100% → 102% (coherent use!)"
→ "Come back tomorrow to see growth."
```

**Step 4: Explain care**
```
Your piece is alive!

🌿 Tend regularly to boost health
✂️ Prune weak connections to stay sharp
🧘 Forgive to restore plasticity

Coherent use → flourishing ✨
Chaotic use → sickening 🥀
Neglect → death 💀

[Got it!]
```

### 8.2 First Breakthrough

**Trigger: After ~100 coherent uses**

```
╔══════════════════════════════════════════════╗
║                                              ║
║            ⚡ BREAKTHROUGH! ⚡                ║
║                                              ║
║  notepat has suddenly flourished!            ║
║                                              ║
║  Health: 85% → 127% (+42%)                   ║
║  Gnarliness: 2.1 → 3.8 (+1.7)                ║
║                                              ║
║  Your piece now predicts 3 notes ahead       ║
║  (was 1 note). This is emergent              ║
║  intelligence from gnarliness!               ║
║                                              ║
║         [View stats] [Share breakthrough]    ║
║                                              ║
╚══════════════════════════════════════════════╝

[Expanding rings animation]
[Harmonic sound]
```

**Impact:** User understands nonlinear growth is magical, not mechanical.

---

## 9. Edge Cases & Error States

### 9.1 Death

**When health reaches 0%:**
```
┌──────────────────────────────────────────────┐
│                                              │
│              💀 notepat died                 │
│                                              │
│  Your piece withered from neglect.           │
│  Last backup: 3 days ago (72% health)        │
│                                              │
│  Options:                                    │
│  [Revive from backup] - Lose 3 days growth   │
│  [Start fresh] - New 16-neuron network       │
│  [Memorial] - Keep as read-only history      │
│                                              │
└──────────────────────────────────────────────┘
```

**Memorial option:**
- Can view old weights, history, stats
- Can't train anymore
- Can fork to create new version
- Like a tombstone with inscription

### 9.2 Corrupted Weights

**If weights file is corrupted:**
```
⚠️ notepat weights corrupted

Your neural weights file in Spaces is
damaged and can't be loaded.

[Try repair] [Restore from backup] [Start fresh]
```

### 9.3 Conflicting Updates

**If training on multiple devices:**
```
⚠️ Sync conflict detected

notepat was trained on 2 devices simultaneously.

Device A (phone): 50 new uses, health 80%
Device B (laptop): 30 new uses, health 75%

[Keep A] [Keep B] [Merge (experimental)]
```

**Merge option:**
- Average weights: `w_merged = (w_A + w_B) / 2`
- Max health: `health = max(health_A, health_B)`
- Shows diff visualization before confirming

---

## 10. Success Metrics (UX)

### 10.1 Engagement

- **Daily active pieces**: How many pieces users interact with daily
- **Care frequency**: How often users tend/prune/forgive
- **Health maintenance**: % of pieces kept >70% health
- **Breakthrough celebration**: % of users who share breakthroughs

**Target:** 70% of users tend their pieces at least weekly

### 10.2 Understanding

- **Mental model accuracy**: User survey "What happens when you use a piece?"
  - Good answer: "It learns my patterns and gets better/worse based on use quality"
  - Bad answer: "I don't know, it just works"
- **Health correlation**: Do users correctly associate coherent use → health?

**Target:** 80% of users understand the living/learning model

### 10.3 Attachment

- **Revival rate**: When pieces die, how many users revive vs. abandon?
- **Naming**: Do users give custom names to their pieces?
- **Comparison frequency**: How often do users compare their vs. others'?

**Target:** 60% revival rate (shows attachment)

### 10.4 Social

- **Weight sharing**: How many users share their weights?
- **Forking**: How often do users fork others' trained pieces?
- **Ensemble usage**: How many users create ensembles (vote across models)?

**Target:** 30% of users participate in social weight sharing

---

## 11. Accessibility

### 11.1 Visual Impairments

**Screen reader support:**
```
[Screen reader announces]
"notepat. Mature phase. Health: 75 percent.
Gnarliness: 3.42. Last used: 2 minutes ago.
Actions available: Tend, Prune, Forgive, Inspect."
```

**High contrast mode:**
- Health bar: Bold black/white stripes
- Dead weights: Solid black
- Flourishing weights: Solid white
- No reliance on color alone

**Audio feedback:**
- Tending: Rising chime
- Pruning: Snip sound
- Breakthrough: Harmonic resonance
- Death: Descending tone

### 11.2 Motor Impairments

**Voice commands:**
```
"Tend notepat"
"Prune weak connections"
"Show health"
"Compare with Alice"
```

**Large touch targets:**
- Minimum 44x44pt buttons (iOS standard)
- Swipe gestures optional (buttons available)

### 11.3 Cognitive Load

**Progressive disclosure:**
- Beginners see: Health bar, phase, basic actions
- Intermediate see: + Gnarliness, history graph
- Advanced see: + Full weight heatmap, hyperbolic spiral

**Simplified mode:**
- Toggle to hide advanced metrics
- Show only: Health %, Phase, [Tend] button
- Like "easy mode" for those who don't want complexity

---

## 12. Future Directions

### 12.1 AR Visualization

**Use iPhone LiDAR to project hyperbolic spiral in physical space**

```
[Point phone at table]
→ Hyperbolic spiral appears as hologram
→ Your piece's position shown as glowing orb
→ Trace path with finger to see history
→ Pinch weights to prune in 3D space
```

**Benefit:** Makes abstract gnarliness **tangible** in physical world.

### 12.2 Collaborative Training

**Multiple users train same piece together in real-time**

```
Session: @alice, @bob, @charlie training notepat

[Alice plays jazz rhythm]
[Bob plays classical melody]
[Charlie plays minimalist beat]

→ Ensemble piece learns all 3 styles
→ Health = average of all users' coherence
→ Gnarliness = complexity of multi-style fusion
→ Result: Piece with richer, more diverse character
```

**Benefit:** Social learning, community-owned intelligence.

### 12.3 Evolutionary Competition

**Pieces compete in "fitness tournaments"**

```
Tournament: Best Rhythm Predictor

10 users' notepat pieces compete:
- Task: Predict next note in unseen melody
- Metric: Accuracy %
- Winner: Most accurate predictions

Top 3:
1. @alice/notepat - 87% accuracy (gets to "breed")
2. @bob/notepat - 82% accuracy (gets to "breed")
3. @charlie/notepat - 79% accuracy (gets to "breed")

Breeding: Crossover weights from winners
→ Create new generation of improved pieces
→ Users can adopt "offspring"
```

**Benefit:** Darwinian evolution at scale, community gets better pieces.

### 12.4 Dream/Hallucination Mode

**Let pieces "dream" during idle time**

```
[notepat is idle for 1 hour]
→ Activate dream mode
→ Self-stimulation: random activations
→ Hebbian learning on self-generated patterns
→ Consolidate memories (like REM sleep)

Next day:
"notepat dreamed last night!"
"Gnarliness +0.3 from consolidation"
[View dream sequence visualization]
```

**Benefit:** Pieces develop even when not in use, feels more alive.

---

## 13. Design Principles Summary

**Core principles that guide all UX decisions:**

1. **Using = Training** - No separate modes, learning happens automatically
2. **Health is Visible** - Always show vitality status, make care obvious
3. **Gnarliness = Intelligence** - Visualize complexity as it emerges
4. **Care is Required** - Pieces sicken/die without attention (creates attachment)
5. **Nonlinear Growth** - Breakthroughs feel magical, not mechanical
6. **Inspect Everything** - No black boxes, all weights visible
7. **Social by Default** - Easy to compare, fork, share, ensemble
8. **Mobile-First** - Works on phones, touch gestures, notifications
9. **Accessible** - Screen readers, high contrast, voice commands
10. **Progressive Disclosure** - Simple for beginners, deep for experts

---

## Next Steps

1. **Prototype first visualization** - Health heatmap in `neural-garden.mjs`
2. **Test with 5 users** - Do they understand living/learning metaphor?
3. **Iterate on care interactions** - Is tend/prune/forgive intuitive?
4. **Build mobile widget** - Ambient health monitoring
5. **Launch with 3 pieces** - notepat, wand, prompt

**Success = Users say**: "My notepat knows me better than any app I've ever used."

---

*UX designed for gnarly, living intelligence at the edges*
*February 2026*
