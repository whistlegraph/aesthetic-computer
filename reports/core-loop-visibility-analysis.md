# Core Loop & Page Visibility Analysis
## Goal: Keep `clock.mjs` running audio when the laptop lid is closed

---

## Architecture Overview

```
┌─ Main Thread (bios.mjs) ──────────────────────────────────────┐
│  requestAnimationFrame → loop() → requestFrame()              │
│       │                                                       │
│       ├── input(now)           [pen, keyboard, gamepad]       │
│       ├── calc updateTimes     [120fps fixed-step sim ticks]  │
│       └── postMessage("frame") ──→ Worker (disk.mjs)          │
│                                        │                      │
│       ◄── postMessage(result) ◄────────┘                      │
│       └── blit pixels to canvas                               │
│                                                               │
│  AudioContext + AudioWorklet (speaker.mjs)                     │
│    └── process() runs on AUDIO THREAD (independent of rAF!)   │
└───────────────────────────────────────────────────────────────┘
```

## The Problem (3 layers)

When you close the lid (page becomes hidden), browsers **throttle `requestAnimationFrame` to ~1fps or pause it entirely**. This cascades:

| Layer | What Breaks | Why |
|-------|-------------|-----|
| **1. rAF stops** | `loop()` in `loop.mjs:22` stops being called | Browser suspends rAF for hidden tabs |
| **2. No "frame" messages** | `bios.mjs` never calls `requestFrame()` → no `postMessage("frame")` to the worker | No rAF = no frame pump |
| **3. No sim() calls** | `clock.mjs` `sim()` never fires → no melody tick processing, no `sound.play()` calls | Worker receives no frames |

**Critically**: The AudioContext and AudioWorkletProcessor (`speaker.mjs`) **keep running** on the audio thread. The problem is purely that **no new sound commands are dispatched** because the main loop is frozen.

## Key Files & Line Numbers

### `loop.mjs` — Frame Scheduler
- **L7-10**: `updateFps=120`, `renderFps=165` — fixed sim rate + max render rate
- **L22-56**: `loop(now)` — rAF callback; accumulates delta, counts sim ticks, calls `updateAndRender()`  
- **L56**: `window.requestAnimationFrame(loop)` — the only scheduling mechanism
- **L59-65**: **Visibility handler** — on resume, resets `updateTime=0`, `renderTime=0`, rebases `lastNow`. **Does nothing on hide.**
- **L76-92**: iframe fallback using `MessageChannel` — only for cross-origin iframes
- **L100-105**: `pause()`/`resume()` — manual pause, not tied to visibility

### `bios.mjs` — Orchestrator (Main Thread)
- **L3064-3066**: AudioContext created, `suspend()` is **commented out**
- **L3706-3709**: AudioContext resumed on first user gesture (pointerdown/keydown)
- **L4598**: `requestBeat()` — sends beat messages to worker (metronome)
- **L4705-4710**: Frame stall watchdog **disarms when hidden** — won't false-alarm
- **L4712**: `requestFrame()` — sends `"frame"` to worker; **only called from rAF callback**
- **L12407-12443**: `Loop.start()` callback — the bridge from rAF to disk worker
- **L19720-19727**: Sends `"visibility-change"` message to disk worker

### `disk.mjs` — Piece Runner (Web Worker)  
- **L9228+**: `makeFrame()` — receives `"frame"` messages, processes sim/paint/act
- **L9847-9878**: Handles `"visibility-change"` — resets KidLisp timing, sets `visible` flag
- **L11621-11638**: Calls piece's `sim($api)` for each accumulated tick
- **L12117+**: Calls piece's `paint($api)` if render needed

### `clock.mjs` — The Piece
- **L889**: `boot()` — melody parsing, UTC sync, sample loading
- **L4971-5000**: `handleStandbyResume()` — detects >1s gaps, clears hanging sounds
- **L5673**: `sim()` — the 120fps tick handler: polls speaker, processes melody timing, triggers notes
- **L6980-82**: `beat()` is **commented out** — no metronome-based timing

### `speaker.mjs` — AudioWorklet
- AudioWorkletProcessor `process()` runs on the **audio render thread**
- **Completely independent of rAF** — keeps running when page is hidden
- But only processes commands that have already been queued

## What Needs to Change

### Strategy: Replace rAF with a timer-based fallback when hidden

The cleanest approach is to **keep rAF for visible operation** but switch to a `setInterval` or `Worker`-based timer when the page goes hidden. This only needs to pump `sim()` frames (no rendering needed).

### Option A: Timer fallback in `loop.mjs` (Simplest)

Add a `setInterval` fallback that fires ~60fps when hidden, only pumping sim updates (no render):

```
visibilitychange → hidden:
  1. Start setInterval(16ms) that calls updateAndRender(false, simTicks, now)
  2. Stop rAF (or let browser throttle it)

visibilitychange → visible:
  1. Clear the interval
  2. Reset timers (already done)
  3. rAF resumes naturally
```

**Pros**: Minimal changes, contained in `loop.mjs`  
**Cons**: `setInterval` in hidden tabs is also throttled to ~1/sec in most browsers

### Option B: Dedicated Worker timer (Most Reliable) ⭐ RECOMMENDED

Spawn a small timing Worker that uses `setInterval` inside the worker (worker timers are **NOT throttled** when the page is hidden). The worker posts timing ticks back to the main thread, which relays them to `disk.mjs`.

```
┌─ Timer Worker (new) ──────────┐
│  setInterval(8ms)             │  ← Worker timers NOT throttled!
│  postMessage({ tick })  ──────┼──→ Main thread
└───────────────────────────────┘     │
                                      ├── postMessage("frame", {needsRender: false})
                                      └── → disk.mjs Worker → sim() → sound commands
```

**Pros**: Reliable timing even in hidden tabs, works across all browsers  
**Cons**: Slightly more code, need a small new worker file

### Option C: Move melody scheduling to AudioWorklet (Most Precise)

Move the note-scheduling logic into the AudioWorklet's `process()` method, which runs on the audio thread at sample-rate granularity (128 samples / ~2.67ms at 48kHz). This is how professional DAWs and Strudel do it.

**Pros**: Sample-accurate timing, zero dependency on main thread  
**Cons**: Major refactor; AudioWorklet has limited API access (no `fetch`, no DOM)

### Option D: Per-piece `backgroundInterval` flag (Quick Hack)

Let specific pieces like `clock.mjs` opt into a background timer by exporting a flag:

```js
export const background = true; // Keep sim() alive when hidden
```

`bios.mjs` checks this flag and keeps a `setInterval` pump alive for that piece.

**Pros**: Surgical, doesn't affect other pieces  
**Cons**: Per-piece timer still gets throttled (same issue as Option A)

## Recommended Implementation: Option B (Timer Worker)

### Files to modify:
1. **`loop.mjs`** — Add visibility-aware fallback that posts to a timer worker when hidden
2. **`bios.mjs`** — Accept timer-worker ticks as frame sources alongside rAF; skip rendering
3. **`disk.mjs`** — Accept a `backgroundTick` flag on frame messages; run `sim()` only (no paint)
4. **`clock.mjs`** — Remove the standby detection hack (it becomes unnecessary); maybe adjust `handleStandbyResume()` threshold
5. **New file: `timer-worker.mjs`** — Tiny worker with `setInterval(8)` that posts ticks

### Additional consideration:
The `visible` flag in `disk.mjs:L9847` is currently used to reset KidLisp timing. For clock.mjs, we'd want to **NOT reset timing on visibility change** — the whole point is continuous playback. This could be gated on the `background` export flag.

## AudioContext Note

The AudioContext is **never suspended** on visibility change (good!). Chrome's autoplay policy may suspend it initially, but once resumed by user gesture, it stays running. The audio thread (`speaker.mjs` AudioWorkletProcessor) continues processing. So once we keep `sim()` ticking, sound commands will flow through naturally.

## Browser Timer Throttling Reference

| Timer Type | Visible Tab | Hidden Tab | Web Worker |
|------------|------------|------------|------------|
| `requestAnimationFrame` | ~60fps | **0-1fps** | N/A |
| `setInterval(1)` | 4ms min | **1000ms min** | **4ms min** ✅ |
| `setTimeout(1)` | 4ms min | **1000ms min** | **4ms min** ✅ |
| AudioWorklet `process()` | ~375fps (128/48000) | **~375fps** ✅ | N/A |
| `MessageChannel` | Fast | **Throttled** | **Fast** ✅ |

**Key insight**: Worker-based timers are the only reliable non-audio mechanism for background execution.
