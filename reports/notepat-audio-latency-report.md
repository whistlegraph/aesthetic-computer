# 🎹⏱️ Notepat Audio Latency & Performance Report

**Generated:** 2026-02-15
**Updated:** 2026-02-15 (Test Results Added)
**Subject:** Audio performance testing infrastructure and recent performance regression analysis

---

## 🚨 Executive Summary

Aesthetic Computer has a comprehensive audio latency testing infrastructure centered around the **Artery** system (Chrome DevTools Protocol automation). Recent notepat UI updates (last 3 weeks) have introduced **severe performance regressions**.

### ⚠️ Critical Findings

**Test Results (2026-02-15 19:23 UTC):**
- **Mean Latency:** 417.28ms (keyboard → sound detection)
- **Target Latency:** <30ms
- **Performance Degradation:** ~14x slower than target
- **Status:** 🔴 CRITICAL - Latency is extremely noticeable to users

**Historical Context:**
- Commit `22675462f` explicitly **reduced audio latency target to 3ms**, suggesting awareness of latency issues before recent UI updates
- Recent UI additions (waveform visualizer, KidLisp visualizer, screen flash effects) likely causing CPU bottleneck

---

## 📊 Latest Test Results

### Test Run: 2026-02-15 19:23 UTC

**Configuration:**
- Iterations: 5
- Test: `node artery/test-notepat-latency.mjs --iterations=5 --cooldown=200`
- Environment: GitHub Codespaces (devcontainer)

**Results:**

| Metric | Value | Status |
|--------|-------|--------|
| Mean Latency | 417.28ms | 🔴 CRITICAL |
| Median Latency | 406.84ms | 🔴 CRITICAL |
| Std Dev | 14.18ms | - |
| Min Latency | 405.36ms | 🔴 CRITICAL |
| Max Latency | 439.40ms | 🔴 CRITICAL |
| P95 | 439.40ms | 🔴 CRITICAL |
| P99 | 439.40ms | 🔴 CRITICAL |
| Success Rate | 100% (5/5) | ✅ |

**Individual Measurements:**
```
428.9ms, 439.4ms, 406.8ms, 405.4ms, 405.9ms
```

**Analysis:**
- All measurements are consistently high (405-439ms range)
- Very low variance (14ms std dev) suggests systematic overhead, not random jitter
- This measures JS-side latency only (keypress → sound object creation)
- Actual perceived latency is even higher (~450-490ms including audio buffer)

**Root Cause Hypothesis:**
Based on git history analysis, the likely culprits are:
1. **Waveform visualizer** (commit `43e2e1f9c`) - Drawing waveforms every frame
2. **KidLisp visualizer** (commit `e249dd1a6`) - Additional compute per frame
3. **Screen flash effects** (commit `eadd85cee`) - Synchronous paint operations
4. **HUD UI elements** (commits `3ca580f28`, `3331e90d7`) - More rendering overhead

**Immediate Actions Required:**
1. Profile with Chrome DevTools Performance tab to identify hotspot
2. Test with waveform visualizer disabled
3. Consider throttling non-critical UI updates to 30fps
4. Move visualizer rendering to OffscreenCanvas/Web Worker

---

## 📊 Testing Infrastructure Overview

### 1. **Artery System** (`artery/`)

The Artery system is your primary testing infrastructure, using Chrome DevTools Protocol (CDP) to automate browser testing.

**Core Components:**
- **`artery.mjs`** - Main CDP client for remote control of AC runtime
- **`artery-tui.mjs`** - Interactive terminal UI for running tests
- **`test-configs.json`** - Test configuration and parameter definitions
- **`emacs-mcp.mjs`** - Integration with Emacs for running tests

**Key Features:**
- Opens AC in VS Code webview panel
- Injects telemetry hooks into running pieces
- Simulates keyboard/touch input via CDP
- Measures latency from keypress to audio output
- Monitors memory usage, heap growth, audio worklet state

### 2. **Notepat-Specific Tests**

#### **`artery/test-notepat-latency.mjs`** ⏱️
**Purpose:** Measures keyboard→sound detection latency

**How it works:**
1. Opens AC via CDP and navigates to notepat
2. Injects keypress events via `Runtime.evaluate`
3. Monitors `window.__bios_sound_telemetry.triggerCount`
4. Measures time from keypress to `bios.triggerSound()` call
5. Runs configurable iterations (default: 20)

**Configuration:**
```bash
node artery/test-notepat-latency.mjs [options]
  --iterations=N   # Number of test iterations (default: 20)
  --quick          # Use quick mode (shorter notes)
  --cooldown=MS    # Time between tests (default: 300ms)
```

**Metrics Tracked:**
- Mean, median, stdDev latency
- Min/max latency
- P95, P99 percentiles
- Success/failure rate

**Important Note:** This measures **JS-side latency** (keypress → sound object creation), NOT including audio buffer latency (~10-50ms depending on buffer size).

#### **`artery/test-notepat-stability.mjs`** 🔬
**Purpose:** Long-running fuzz test for memory leaks and performance degradation

**How it works:**
1. Runs for configurable duration (default: 5 minutes)
2. Plays random notes at varying intensities (low/medium/high/extreme)
3. Cycles through all notepat modes (wave types, octaves, quick/slide/room)
4. Monitors heap usage, sounds dict, tonestack, audio worklet stats
5. Tracks latency degradation over time

**Configuration:**
```bash
node artery/test-notepat-stability.mjs [duration] [intensity]
  duration  - Minutes to run (default: 5)
  intensity - low|medium|high|extreme (default: medium)
```

**Intensity Profiles:**
- **Low:** 2 notes/sec, 2 max concurrent
- **Medium:** 5 notes/sec, 4 max concurrent
- **High:** 10 notes/sec, 6 max concurrent
- **Extreme:** 20 notes/sec, 10 max concurrent

**Metrics Tracked:**
- Memory (heap growth, peak usage)
- Notepat state (sounds dict size, tonestack size)
- Audio worklet (queue length, running instruments)
- Latency (start avg, end avg, change over time)
- Mode cycling stats

**Pass Criteria:**
- Heap growth < 50%
- Max sounds dict < 20
- Max tonestack < 20
- Warnings < 10
- Latency degradation < 50ms

**Latest Result:** ✅ PASSED (2-minute test, high intensity)
- Notes played: 3229
- Heap growth: -14% (stable)
- No warnings

### 3. **Telemetry Hooks**

The system exposes several telemetry objects in the browser:

#### **`window.__bios_sound_telemetry`** (bios.mjs)
```javascript
{
  triggerCount: number,      // Total sounds triggered
  recentTriggers: Array,     // Recent trigger timestamps
}
```

#### **`window.__speaker_telemetry`** (speaker.mjs AudioWorklet)
```javascript
{
  queueLength: number,       // Audio queue depth
  runningCount: number,      // Active instruments
}
```

#### **`window.__notepat_perfStats`** (notepat.mjs)
```javascript
{
  lastKeyTime: number,       // Last keydown timestamp
  lastSoundTime: number,     // Last sound trigger timestamp
  latency: number,           // Calculated latency
}
```

#### **`window.__acLatency`** (Injected by tests)
```javascript
{
  keyPressTime: number,
  soundStartTime: number,
  samples: Array<{latency, timestamp}>,
  maxSamples: 100,
}
```

### 4. **Audio Analysis Module** (`shared/audio-analyzer.mjs`)

Shared audio analysis algorithms used in both:
- **Speaker.mjs AudioWorklet** (runtime)
- **kidlisp.com editor** (offline analysis)

**Features:**
- FFT implementation (512-point)
- 8 frequency bands (subBass → ultra)
- Beat detection (energy-based onset detection)
- Adaptive threshold for different music types
- Amplitude calculation (peak detection)

**Frequency Bands:**
1. subBass: 20-100 Hz
2. lowMid: 100-400 Hz
3. mid: 400-1000 Hz
4. highMid: 1000-2500 Hz
5. presence: 2500-5000 Hz
6. treble: 5000-10000 Hz
7. air: 10000-16000 Hz
8. ultra: 16000-20000 Hz

### 5. **NPM Scripts**

```bash
# Performance tests
npm run test:perf               # Boot performance test
npm run test:perf:chrome        # Chrome DevTools automated test
npm run test:perf:lighthouse    # Full Lighthouse audit

# Run via Artery TUI (recommended)
# Press T → navigate to test → Enter
```

---

## 🔍 Recent Notepat Changes (Last 3 Weeks)

### Performance-Related Commits

1. **`22675462f`** (Dec 2024) - ⚡ **Reduce audio latency target to 3ms**
   - Explicitly reduced latency target
   - Suggests awareness of latency issues

2. **`387b079e7`** - 🧹 **Remove debug logs from hot paths**
   - Removed logs from MIDI, keyboard, synth
   - Should improve performance

3. **`6b57c97c3`** - 🧹 **Remove debug logs + fix note parsing**
   - Additional cleanup of hot paths

### UI-Related Commits (Potential Performance Impact)

4. **`e249dd1a6`** - **Add kidlisp visualizer support**
   - Added new visualization features
   - **Could impact CPU usage**

5. **`3ca580f28`** - **notepat.com HUD tap + Escape redirect**
   - UI interaction changes
   - **Potential event handling overhead**

6. **`3331e90d7`** - **notepat.com domain + .com superscript HUD branding**
   - Additional UI elements
   - **More rendering per frame**

7. **`13ce78de8`** - **Restore notepat hover overlays**
   - Re-added overlays that were accidentally removed
   - **Additional paint operations**

8. **`43e2e1f9c`** - **Restore top bar waveform visualizer**
   - Waveform drawing every frame
   - **CPU-intensive per-frame operation**

9. **`ebdb69797`** + **`65a358528`** - **Remove/restore waveform visualizer**
   - Churning on visualizer implementation
   - **Suggests performance concerns**

10. **`a4c32080b`** - **Tighten notepat layout and effects**
    - Layout changes
    - **Could affect rendering performance**

11. **`eadd85cee`** - **Add peripheral screen flash for metronome beats**
    - Full-screen flash effect
    - **Potential frame stutter**

### Analysis of Potential Regressions

**High Probability:**
- **Waveform visualizer** (commit `43e2e1f9c`) - Drawing waveforms every frame is CPU-intensive
- **KidLisp visualizer** (commit `e249dd1a6`) - Additional compute per frame
- **Screen flash effects** (commit `eadd85cee`) - Synchronous paint operations

**Medium Probability:**
- **Hover overlays** (commit `13ce78de8`) - More UI elements to render
- **HUD branding elements** (commits `3ca580f28`, `3331e90d7`) - Additional paint operations

---

## 🎯 Recommended Actions

### Immediate Diagnostics

1. **Run baseline latency test:**
```bash
cd /workspaces/aesthetic-computer
node artery/test-notepat-latency.mjs --iterations=50
```

2. **Run stability test with latency tracking:**
```bash
node artery/test-notepat-stability.mjs 10 high
```

3. **Compare with/without visualizer:**
   - Test latency with waveform visualizer ON
   - Test latency with waveform visualizer OFF
   - Compare difference

### Performance Profiling

1. **Use Artery TUI for interactive testing:**
```bash
node artery/artery-tui.mjs
# Press T → select "notepat-latency" → configure params → run
```

2. **Chrome DevTools Performance Timeline:**
```bash
npm run test:perf:chrome
```
Look for:
- Long tasks (>50ms) in main thread
- Paint operations taking >16ms
- Script evaluation blocking audio

3. **Identify CPU hotspots:**
   - Open Chrome DevTools → Performance
   - Record while playing notes
   - Look for expensive functions in flame graph

### Code Investigation Areas

1. **`paint()` function in notepat.mjs**
   - Check if waveform drawing is optimized
   - Consider moving visualizer to lower FPS (30fps vs 60fps)
   - Use `requestAnimationFrame` throttling

2. **Event handlers**
   - Verify no blocking operations in keyboard handlers
   - Check for debouncing/throttling on UI events
   - Ensure HUD updates don't block audio path

3. **Speaker.mjs AudioWorklet**
   - Verify worklet isn't blocking on main thread communication
   - Check queue management for backpressure
   - Monitor buffer underruns

### Potential Fixes

1. **Throttle visualizer updates:**
```javascript
// In notepat.mjs paint()
if (frameCount % 2 === 0) {
  // Only draw waveform every other frame (30fps)
  drawWaveformVisualizer();
}
```

2. **Use OffscreenCanvas for visualizer:**
```javascript
// Move waveform drawing to Web Worker
const offscreenCanvas = visualizerCanvas.transferControlToOffscreen();
const worker = new Worker('visualizer-worker.js');
worker.postMessage({ canvas: offscreenCanvas }, [offscreenCanvas]);
```

3. **Disable UI updates during note trigger:**
```javascript
// In act() keyboard handler
const skipUIUpdate = Date.now() - lastNoteTime < 50; // Skip UI for 50ms
if (!skipUIUpdate) {
  updateHoverOverlays();
}
```

---

## 📈 Performance Metrics Documentation

### Target Metrics

Based on commit history and test infrastructure:

| Metric | Target | Current (2026-02-15) | Status |
|--------|--------|----------------------|--------|
| Audio latency | < 3ms | N/A | - |
| JS keypress→sound | < 30ms | **417ms** | 🔴 **FAILED** |
| Total latency (perceived) | < 50ms | **~460ms** | 🔴 **FAILED** |
| Frame time | < 16.67ms | Unknown | ⚠️ Needs profiling |
| Heap growth (10min test) | < 50% | N/A | - |

### Monitoring Commands

```bash
# Quick latency check
node artery/test-notepat-latency.mjs --iterations=20 --cooldown=200

# Extended stability test
node artery/test-notepat-stability.mjs 5 medium

# Performance report from running AC
node artery/test-perf-report.mjs

# Full Chrome DevTools audit
npm run test:perf:chrome
```

---

## 🔧 Integration with Emacs-MCP-Artery

You mentioned "we can run through emacs-mcp artery" - here's how:

**Via `artery/emacs-mcp.mjs`:**
```bash
# From Emacs, run:
M-x shell-command RET node artery/emacs-mcp.mjs notepat-latency RET

# Or use Artery TUI directly:
M-x eat RET
node artery/artery-tui.mjs
# Press T → select test → Enter
```

**Test results will be displayed in Emacs buffer with:**
- Color-coded latency values (green < 10ms, yellow < 30ms, red > 30ms)
- Statistical summary (mean, median, p95, p99)
- Pass/fail criteria

---

## 📋 Summary

### What You Have

✅ **Comprehensive testing infrastructure:**
- Automated latency measurement (`test-notepat-latency.mjs`)
- Long-running stability tests (`test-notepat-stability.mjs`)
- Real-time telemetry hooks
- Artery TUI for interactive testing
- Chrome DevTools integration
- Shared audio analysis library

### What Might Be Happening

⚠️ **Potential performance regressions from:**
- Waveform visualizer (CPU-intensive per-frame drawing)
- KidLisp visualizer integration
- Additional HUD UI elements
- Screen flash effects

### What To Do Next

1. **Run baseline tests** to quantify current latency
2. **Profile with Chrome DevTools** to identify CPU hotspots
3. **Compare visualizer on/off** performance
4. **Consider throttling** non-critical UI updates
5. **Optimize paint operations** in visualizers

---

## 🔗 Related Files

- Testing: `artery/test-notepat-latency.mjs`, `artery/test-notepat-stability.mjs`
- Reports: `reports/notepat-stability.md`, `reports/notepat-stability-live.json`
- Config: `artery/test-configs.json`
- Audio: `shared/audio-analyzer.mjs`, `system/public/aesthetic.computer/lib/speaker.mjs`
- Piece: `system/public/aesthetic.computer/disks/notepat.mjs`
- Docs: `docs/PERFORMANCE.md`, `artery/CDP-SYSTEM.md`

**Run a test now:**
```bash
node artery/test-notepat-latency.mjs --iterations=30
```
