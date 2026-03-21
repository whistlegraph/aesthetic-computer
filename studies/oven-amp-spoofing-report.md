# Oven Amp Spoofing — Feature Report

## Problem

KidLisp audio-visualizer pieces (those that use `amp`, `leftAmp`, `rightAmp`,
`beat`, `kick`, or the `(amplitude)` function) render as **frozen/static** when
captured by oven's headless Puppeteer browser. There's no microphone, no BGM,
and no speaker output — so all audio globals stay at zero and the visuals never
move.

## Audio Data Flow (Production)

```
┌──────────────────────────────────────────────────────────┐
│ BIOS (main thread)                                       │
│                                                          │
│  ┌─────────────────┐    ┌──────────────────────────────┐ │
│  │ BGM <audio> el   │───▶ AnalyserNode (fftSize=256)   │ │
│  │                  │    │ getByteFrequencyData()       │ │
│  └─────────────────┘    │ → peak bin = amplitude 0-255 │ │
│                          └──────────┬───────────────────┘ │
│                                     │                     │
│  ┌──────────────────────────────┐   │                     │
│  │ SpeakerProcessor (worklet)   │   │                     │
│  │ per-sample peak → ampL/ampR  │   │                     │
│  │ (0.0–1.0 float32)           │   │                     │
│  └──────────┬───────────────────┘   │                     │
│             │ "amplitudes" msg      │ audioMusicAmplitude  │
│             ▼                       ▼                     │
│         postMessage to disk worker                        │
└──────────────────────────────────────────────────────────┘
                        │
                        ▼
┌──────────────────────────────────────────────────────────┐
│ DISK (worker thread)                                     │
│                                                          │
│  speaker.amplitudes = { left, right }                    │
│  $commonApi.bgm.data = { amplitude, sample }             │
│                                                          │
│  ┌───────────────────────────────────────────────────┐   │
│  │ KidLisp evaluator                                 │   │
│  │                                                   │   │
│  │  globalDef.amp      = 0–10 (from updateAudioGlobals) │
│  │  globalDef.leftAmp  = 0–10                        │   │
│  │  globalDef.rightAmp = 0–10                        │   │
│  │  globalDef.beat     = bool                        │   │
│  │  globalDef.kick     = bool                        │   │
│  │  (amplitude) fn → speaker.amplitudes.left         │   │
│  └───────────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────────┘
```

### Key Source Locations

| File | Lines | What |
|------|-------|------|
| `bios.mjs` | 2649, 2833–2840 | AudioContext + AnalyserNode creation |
| `bios.mjs` | 4628–4638 | Per-frame BGM amplitude calculation |
| `bios.mjs` | 4695–4696 | Sends `audioMusicAmplitude` + `audioMusicSampleData` to disk |
| `bios.mjs` | 3262–3277 | Requests + relays speaker amplitudes |
| `speaker.mjs` | 702–703, 844–845 | Per-sample peak amplitude (L/R) |
| `speaker.mjs` | 144–153 | Responds to `get-amplitudes` message |
| `disk.mjs` | 10750 | Stores `speaker.amplitudes = { left, right }` |
| `disk.mjs` | 11498–11501 | Stores `$commonApi.bgm.data` |
| `kidlisp.mjs` | 1145–1150 | `amp`, `leftAmp`, `rightAmp` global init (0) |
| `kidlisp.mjs` | 1927–1931 | `updateAudioGlobals()` — sets globals from audio data |
| `kidlisp.mjs` | 8042–8055 | `(amplitude)` function — reads speaker amplitudes |

## Proposed Spoofing Strategy

### Where to inject

The spoof must happen **inside the disk worker**, because that's where KidLisp
runs. It should feed synthetic data through the same paths that real audio uses:

1. **`speaker.amplitudes`** — set `{ left, right }` to synthetic float values
2. **`updateAudioGlobals()`** — call with synthetic `{ amp, leftAmp, rightAmp }`
3. **`$commonApi.bgm.data`** — set `{ amplitude, sample }` for BGM-reading pieces

### Synthetic signal

A slow sine wave is ideal — it produces smooth, natural-looking visualizer
motion without harsh jumps:

```javascript
// In disk.mjs, when no real AudioContext is available:
const t = performance.now() / 1000;
const syntheticAmp = (Math.sin(t * 2 * Math.PI * 0.5) + 1) / 2; // 0–1, 0.5 Hz
speaker.amplitudes = { left: syntheticAmp, right: syntheticAmp };
```

For KidLisp's 0–10 scale globals:
```javascript
globalDef.amp = syntheticAmp * 10;
globalDef.leftAmp = syntheticAmp * 10;
globalDef.rightAmp = syntheticAmp * 10;
```

### Activation conditions

The spoof should activate **only when all of**:
- No real `AudioContext` exists (headless browser / no user gesture)
- The piece uses audio-reactive globals (`amp`, `leftAmp`, `rightAmp`, `beat`,
  `kick`, or calls `(amplitude)`)
- OR: a global flag like `window.acSpoofAudio = true` is set by the oven's
  Puppeteer page setup

### Delicacies & Edge Cases

1. **Worker context**: `disk.mjs` runs in a Web Worker — no `AudioContext` API.
   Detection must come from BIOS (main thread) signaling "no audio available"
   via a message, or from a URL param like `?spoofaudio=true`.

2. **Timing**: The spoof must start *before* the first `paint()` call, or
   visualizers will render one frozen frame before the sine kicks in. Best
   approach: initialize synthetic values in `boot()` or on first `sim()`.

3. **`(amplitude)` reads `speaker.amplitudes` directly** — so spoofing
   `speaker.amplitudes` covers both the global variables AND the function.

4. **BGM-based pieces** read `$commonApi.bgm.data.amplitude` (0–255 scale) and
   `.sample` (Uint8Array of frequency bins). A synthetic frequency spectrum
   would be needed for full fidelity, but most pieces just read the amplitude.

5. **`beat` and `kick` detection** likely uses threshold crossings on amplitude.
   The sine wave should cross meaningful thresholds to trigger these — a 0.5 Hz
   wave with amplitude 0–1 would cross a 0.5 threshold once per second, giving
   a natural "beat" rate.

6. **Pieces that generate their own audio** (e.g., synth pieces) will have real
   speaker output in production but zero in headless. The spoof should not
   override real audio data when it arrives — it's a fallback only.

7. **Multi-channel**: Some pieces may use `leftAmp` vs `rightAmp` differently
   for stereo visualization. Adding a slight phase offset between L/R channels
   would make stereo pieces look more alive:
   ```javascript
   const leftAmp = (Math.sin(t * Math.PI * 1.0) + 1) / 2;
   const rightAmp = (Math.sin(t * Math.PI * 1.0 + 0.5) + 1) / 2;
   ```

## Implementation Plan

### Phase 1: URL-param approach (simplest)
- Oven Puppeteer adds `?spoofaudio=true` to piece URLs
- `bios.mjs` reads the param and sends `{ type: "spoof-audio", enabled: true }`
  to disk worker on boot
- `disk.mjs` starts a synthetic amplitude loop in `sim()` when spoof is active
- No changes needed in `kidlisp.mjs` — it already reads from the same globals

### Phase 2: Auto-detect (nicer)
- `bios.mjs` checks if `AudioContext` can be created (requires user gesture in
  most browsers — headless Chrome may allow it)
- If not, automatically enables spoof mode
- Pieces opt-in by declaring audio-reactive intent (e.g., using `amp` in source)

### Phase 3: Richer synthetic signal
- Generate fake frequency bin data for `bgm.data.sample`
- Add `beat`/`kick` detection from the synthetic wave
- Optionally vary the sine frequency/amplitude over time for more visual variety
