# Demoplay Architecture Report
## "Test Suite for Aesthetic Computer" — Prix Ars Electronica 2026 Video

### Concept

A choreographed, automated performance of Aesthetic Computer — structured like a classical music test suite. The system plays itself: navigating between pieces, triggering interactions, speaking narration via TTS, and recording the entire session as video. No manual input during recording. The demoplay IS the artwork.

---

## Existing Infrastructure (What We Already Have)

### 1. Merry Pipeline — Timed Piece Sequencing

**Files:** `disks/merryo.mjs`, `disks/merry.mjs`, `disks/prompt.mjs` (lines 1062–1372)

The merry system already chains pieces with precise timing:

```
/merry:5-tone:3-clock:4-wand
```

This plays `tone` for 5s → `clock` for 3s → `wand` for 4s → returns to prompt.

**Key capabilities:**
- `system.merry` state object tracks pipeline progress, piece index, elapsed time
- UTC-synchronized timing (multiple devices stay in sync)
- Crossfade support via `fadeDuration`
- Preloads all pieces before starting (`preloadPieces()`)
- Can record to video via `markAsTaping: true`
- Loop mode (`merryo`) or single-pass (`merry`)
- `stopMerryPipeline()` for clean exit

**Limitation for demoplay:** Merry only *shows* pieces for a duration — it doesn't *interact* with them. We need synthetic input during each piece's time slot.

### 2. Robo — Synthetic Input Automation

**File:** `disks/robo.mjs` (1,559 lines)

Robo generates synthetic pen events (touch, draw, lift) to automate brush execution at 120fps. It loads a target brush internally and plays back generated paths.

**Key capabilities:**
- `Robo` class in `disk.mjs` — available in every piece's API
- `robo.touch(x, y)`, `robo.draw(x, y, px, py)`, `robo.lift(x, y)`
- Events have `device: "robot"` — distinguishable from human input
- Speed control via `robotState.speed` multiplier
- Path queue system with frame-locked timing
- Grid-based path generation

**What this gives demoplay:** We can script drawing actions, tap interactions, and gesture sequences that play automatically within each piece.

### 3. Artery — Chrome DevTools Protocol Automation

**Files:** `artery/artery.mjs` (2,012 lines), `artery/artery-tui.mjs` (7,667 lines)

External test automation via CDP (Chrome DevTools Protocol):

```javascript
const client = new Artery();
await client.connect();
await client.jump('notepat');
await client.sendKeyEvent({ key: 'a' });
```

**Key capabilities:**
- Connect to running AC instance via CDP
- `jump(pieceName)` — navigate to any piece
- `sendKeyEvent()` — synthetic keyboard events
- `evaluate(jsCode)` — execute JS in page context
- `activateAudio()` — enable audio context

### 4. Bach Prelude Test — Musical Timing Template

**File:** `artery/test-notepat-bach-prelude.mjs`

700+ MIDI events played through synthetic key presses with precise timing:

```javascript
const MIDI_EVENTS = [
  { key: "d", ticks: 120 },
  { key: "a", ticks: 120 },
  { key: "+f#", ticks: 120 },
  // ...
];
```

Each event: press key → hold for 70% of duration → release → gap for 30%.

**What this gives demoplay:** The exact pattern for scripting timed keyboard interactions. We can compose a "score" of interactions the same way.

### 5. TTS — `/api/say` + `speak()` Function

**Endpoint:** `system/netlify/functions/say.js`
**Client:** `system/public/aesthetic.computer/lib/speech.mjs`

```javascript
speak("Welcome to Aesthetic Computer", "female:18", "cloud", {
  provider: "openai",
  volume: 1,
  speed: 1,
  preloadOnly: false,
  skipCompleted: false  // sends speech:completed event when done
});
```

**Key capabilities:**
- OpenAI TTS (`tts-1`) with 6 voices: alloy, echo, fable, onyx, nova, shimmer
- Google Cloud TTS as alternative
- Caches to CDN (Digital Ocean Spaces) — instant replay after first generation
- `preloadOnly: true` — preload all speech samples before performance starts
- `speech:completed` event — know when narration finishes before proceeding
- Pitch, speed, pan, volume controls
- `targetDuration` — time-stretch audio to fit exact timing

### 6. Tape Recording — Video Capture

**File:** `disks/common/tape-player.mjs`

Frame-by-frame video + synchronized audio:
- Records ImageBitmap array + Web Audio AudioBuffer
- Supports VHS-style visual effects
- Play/pause/seek with audio sync
- Merry pipeline already has `isTaping` flag for recording entire sequences

---

## What Needs to Be Built: The Demoplay Piece

### Architecture: `disks/demoplay.mjs`

A new piece that combines merry (sequencing) + robo (synthetic input) + speak (narration) + artery patterns (keyboard automation) into a single scored performance.

### The Score Format

A demoplay score is an array of **movements** (like a classical piece), each containing **measures** of timed actions:

```javascript
const SCORE = [
  {
    movement: "I. The Prompt",
    narration: "No menus. No files. Just a prompt.",
    measures: [
      { action: "wait", duration: 1000 },
      { action: "type", text: "notepat", speed: 80 },  // ms per char
      { action: "key", key: "Enter" },
      { action: "wait-for", event: "piece:loaded" },
      { action: "speak", text: "Three hundred and fifty four pieces.", voice: "female:18" },
      { action: "wait-for", event: "speech:completed" },
      { action: "keys", sequence: [
        { key: "d", hold: 200 },
        { key: "a", hold: 200 },
        { key: "f", hold: 200 },
      ]},
      { action: "wait", duration: 2000 },
      { action: "jump", to: "prompt" },
    ]
  },
  {
    movement: "II. Pieces",
    narration: "Each one a world.",
    measures: [
      { action: "type", text: "line", speed: 60 },
      { action: "key", key: "Enter" },
      { action: "wait", duration: 500 },
      { action: "robo", path: "diagonal", duration: 2000 },
      { action: "wait", duration: 500 },
      { action: "jump", to: "prompt" },
      // ... more pieces
    ]
  },
  // ... more movements
];
```

### Action Types Needed

| Action | Description | Existing Support |
|--------|-------------|-----------------|
| `wait` | Pause for N ms | setTimeout / frame counting |
| `wait-for` | Wait for event (speech:completed, piece:loaded) | act() event system |
| `type` | Type text into prompt char by char | Synthetic keyboard via robo/artery |
| `key` | Press a single key | Robo class / sendKeyEvent |
| `keys` | Sequence of timed key presses | Bach prelude pattern |
| `jump` | Navigate to piece | jump() API — exists |
| `speak` | TTS narration | speak() + speech:completed — exists |
| `robo` | Automated drawing/interaction | Robo class — exists |
| `card` | Display text card overlay | New — render in paint() |
| `fade` | Visual transition | Merry fade system — exists |
| `record-start` | Begin tape recording | Merry taping flag — exists |
| `record-stop` | End tape recording | stopMerryPipeline — exists |

### Implementation Approach

**Option A: In-Process Piece (Recommended)**

A `demoplay.mjs` piece that runs inside AC itself:
- Uses `system.merry`-style pipeline for sequencing
- Uses `Robo` class for synthetic input
- Uses `speak()` for narration
- Renders text cards via `paint()`
- Records via tape system
- Score is a .mjs or .json file

**Pros:** Everything runs in-browser, captures at native quality, audio syncs perfectly, can use all AC rendering (glaze effects, etc.)

**Cons:** Need to handle piece transitions carefully — when you `jump()` to another piece, the demoplay piece's code stops running. Need a conductor that persists across jumps.

**Solution:** Use `system.demoplay` (like `system.merry`) — a persistent state object on the system/BIOS level that survives piece transitions. The BIOS checks `system.demoplay` each frame and advances the score.

**Option B: External Artery Script**

An artery script that drives AC from outside via CDP:
- Full control over timing and navigation
- Can capture via screen recording
- Bach prelude test already proves this works

**Pros:** Simpler — no need to modify BIOS/disk. Score is just a Node.js script.

**Cons:** Audio capture is harder (need system audio recording), visual quality depends on screen resolution, no access to AC's internal rendering/glaze.

**Option C: Hybrid**

Artery script orchestrates high-level movements (jump between pieces, trigger recording). Each piece has a `demoplay` mode that auto-plays its own internal choreography when it detects `system.demoplay` is active.

### Recommended: Option A with BIOS-level conductor

The conductor lives in the BIOS (or as a system-level module), survives piece transitions, and executes the score. Each frame:

```
1. Check system.demoplay.currentAction
2. If action is complete → advance to next action
3. If action is "speak" → call speak(), wait for speech:completed
4. If action is "key" → dispatch synthetic keyboard event
5. If action is "robo" → queue robo path
6. If action is "jump" → call jump(), mark action complete when piece loads
7. If action is "card" → set overlay text for paint() to render
```

### Files to Create/Modify

| File | Change |
|------|--------|
| `disks/demoplay.mjs` | New — entry piece, loads score, starts conductor |
| `lib/demoplay-conductor.mjs` | New — score executor, survives piece transitions |
| `bios.mjs` | Modify — check `system.demoplay` each frame, call conductor |
| `lib/disk.mjs` | Modify — expose `system.demoplay` to pieces, add conductor hooks |
| `disks/demoplay-scores/ars-2026.mjs` | New — the actual score for Ars Electronica video |

### Estimated Effort

| Component | Effort | Notes |
|-----------|--------|-------|
| Score format + parser | Small | JSON/JS object, straightforward |
| Conductor (action executor) | Medium | Core logic — ~300-500 lines |
| BIOS integration | Small | Hook into frame loop + piece transition |
| Text card overlay | Small | paint() overlay in bios or demoplay piece |
| Synthetic typing in prompt | Medium | Need to inject keystrokes into prompt's input |
| Robo path scripting | Small | Robo class already exists |
| TTS preloading + sequencing | Small | speak() + preloadOnly already exist |
| Tape/recording integration | Small | Merry taping system already exists |
| The actual score composition | Large | Creative work — writing the "music" |

**Total new code:** ~800-1200 lines across 3-4 files, plus the score itself.

---

## The Score: "Test Suite No. 1 in A.C. Major"

### Structure (Classical Form)

| Movement | Duration | Content | Mood |
|----------|----------|---------|------|
| I. Overture — The Prompt | 0:00–0:25 | Prompt appears, first command typed, first piece loads | Anticipation |
| II. Exposition — Pieces | 0:25–1:00 | Rapid tour through 8-10 pieces, 3-4 sec each | Energy, variety |
| III. Development — KidLisp | 1:00–1:35 | Code appears, art generates, human-AI collaboration | Wonder |
| IV. Interlude — Social | 1:35–1:55 | Chat, moods, profiles, multi-user | Warmth |
| V. Cadenza — Flow State | 1:55–2:20 | Fast fluent navigation, the instrument played at speed | Virtuosity |
| VI. Coda | 2:20–2:30 | Return to prompt, blinking cursor, title card | Resolution |

### TTS Narration Style

Sparse, poetic, timed to the "rests" between movements. The voice speaks like a test suite announcing itself:

> "Test one. The prompt."
> "Test two. Three hundred and fifty four pieces."
> "Test three. A language a child can read."
> "Test four. Social infrastructure."
> "Test five. Flow state."
> "All tests passed."

The clinical language of a test suite, delivered with the gravity of a classical performance. The juxtaposition IS the statement.

---

## Next Steps

1. **Decide on architecture** — Option A (in-process) vs B (artery) vs C (hybrid)
2. **Prototype the conductor** — Minimal version that can: jump, wait, type, speak
3. **Write the score** — The creative composition of the performance
4. **Record** — Run the demoplay, capture to video
5. **Submit** — Upload to Prix Ars Electronica by Mar 9

### Timeline (5 days)

| Day | Task |
|-----|------|
| Mar 4 (today) | Architecture decision, start conductor prototype |
| Mar 5 | Conductor working, basic score playing |
| Mar 6 | Full score composed, TTS narration recorded/cached |
| Mar 7 | Recording passes, polish transitions |
| Mar 8 | Final cut, prepare submission materials (description, images, bio) |
| Mar 9 | Submit before 2 PM CET |
