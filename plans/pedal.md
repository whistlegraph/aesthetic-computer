# 🎸 Pedal: Audio Effect Plugin for Ableton Live

## Overview

Create an **audio effect** (filter-style) M4L device where audio passes IN from Ableton's signal chain, gets processed in an AC piece, and passes OUT back to Ableton.

**Key difference from existing devices**: Current AC M4L devices (notepat, metronome, prompt) only output audio FROM the web page TO Ableton. This is an **effect** that processes incoming audio.

## Technical Challenge

### The jweb~ Limitation

Per Cycling74 documentation:
- **jweb~** has **signal outlets only** (audio OUTPUT from web page)
- There are **no signal inlets** (no direct audio INPUT to web page)
- The `signal` message mentioned in docs is for internal use, not audio input

### Solution Architecture

Since jweb~ cannot receive audio signals directly, we need to:

1. **Capture audio in Max** using `plugin~ 2` (stereo effect input)
2. **Analyze/sample the audio** and send data via messages to jweb~
3. **Process/visualize** in the AC piece (Web Audio)
4. **Output** via jweb~'s signal outlets → `plugout~`

## Audio Data Flow Options

### Option A: FFT Spectrum Data (Recommended for v1)
```
plugin~ 2 → pfft~ → snapshot~ → format → executejavascript window.acPedalFFT(data)
     │
     └→ [delay for latency compensation] → plugout~ 2
```
- Send FFT magnitude/phase arrays to web page
- Web page visualizes and/or generates new audio based on spectrum
- Original audio passes through with optional delay
- **Best for**: Visualizers, spectrum-driven synths, reactive effects

### Option B: Sample-by-Sample (High latency, limited use)
```
plugin~ 2 → snapshot~ @samps 128 → pack → executejavascript window.acPedalSamples(L, R)
```
- Send raw sample values to web page
- Very high message overhead, significant latency
- **Not recommended** for real-time effects

### Option C: Peak/RMS Envelope (Simple, low latency)
```
plugin~ 2 → peakamp~ → snapshot~ → executejavascript window.acPedalEnvelope(peak)
```
- Send envelope followers (peak, RMS, etc.)
- Web audio generates sounds based on amplitude
- **Best for**: Envelope followers, ducking, gates

### Option D: Hybrid (Audio thru + Web effects)
```
plugin~ 2 ────┬────────────────────────────────────→ *~ [dry] ─┐
              │                                                 │
              └→ analysis → jweb~ → [web audio] →──────────→ +~ → plugout~
                                           │
                            [web-generated audio only, wet]
```
- Original audio passes through (dry)
- Web audio adds effects/layers (wet)
- Mix control for dry/wet blend

## Recommended Implementation: Option D (Hybrid)

For the `pedal` piece, we'll use **Option D** because:
1. Audio passes through at native quality (no degradation)
2. Web Audio can add processing, visualization, or triggered sounds
3. Dry/wet mix gives flexibility
4. Lower latency than sample-by-sample approaches

## M4L Device Structure

### Max Patcher Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      AC Pedal Effect                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  plugin~ 2                                                       │
│     │    └────────────────────────────────────────┐              │
│     │                                             │              │
│     ▼                                             ▼              │
│  ┌──────┐   ┌──────────────────────┐         ┌─────────┐        │
│  │ FFT  │──▶│ fft analysis js     │──▶      │ *~ dry  │        │
│  │pfft~ │   │ send to jweb        │          │ level   │        │
│  └──────┘   └──────────────────────┘          └────┬────┘        │
│                                                    │              │
│  ┌────────────────────────────────────────────────┐│              │
│  │                jweb~                           ││              │
│  │  ┌────────────────────────────────────────┐   ││              │
│  │  │  aesthetic.computer/pedal?daw=1        │   ││              │
│  │  │  [signal out L] [signal out R] [msgs]  │   ││              │
│  │  └───────┬────────────┬───────────────────┘   ││              │
│  └──────────┼────────────┼───────────────────────┘│              │
│             │            │                        │              │
│             ▼            ▼                        ▼              │
│          ┌─────────┐  ┌─────────┐              ┌──┴──┐           │
│          │ *~ wet  │  │ *~ wet  │              │ +~  │           │
│          │ level   │  │ level   │              └──┬──┘           │
│          └────┬────┘  └────┬────┘                 │              │
│               └─────────┬──┘                      │              │
│                         ▼                         ▼              │
│                      ┌──┴──┐                   ┌──┴──┐           │
│                      │ +~  ├───────────────────│ +~  │           │
│                      └──┬──┘                   └──┬──┘           │
│                         │                         │              │
│                         ▼                         ▼              │
│                   plugout~ 2                                     │
│                                                                  │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │  Controls:                                                 │  │
│  │  [live.dial dry/wet] [live.dial wet_vol] [live.dial drive] │  │
│  └────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Key M4L Objects

| Object | Purpose |
|--------|---------|
| `plugin~ 2` | Receive stereo audio from Ableton |
| `pfft~` | FFT analysis of input |
| `peakamp~` | Amplitude envelope |
| `jweb~` | Web view with audio output |
| `*~` | Level/gain control |
| `+~` | Mix signals |
| `plugout~` | Send stereo audio back to Ableton |
| `live.dial` | Automatable parameters |

### JavaScript Functions (window.*)

```javascript
// Called from Max with FFT data (32-128 bins typically)
window.acPedalFFT = function(magnitudes) {
  // magnitudes: array of FFT bin magnitudes [0-1]
  // Used for visualization and audio-reactive effects
};

// Called from Max with amplitude envelope
window.acPedalEnvelope = function(peakL, peakR, rmsL, rmsR) {
  // Used for envelope-following effects
};

// Called from Max with tempo/transport (inherited from base)
window.acDawTempo = function(bpm) { ... };
window.acDawTransport = function(playing) { ... };
```

## Web Audio Processing in pedal.mjs

### Piece Structure

```javascript
// pedal.mjs - Audio Effect Pedal for Ableton Live

let fftData = [];
let envelope = { peakL: 0, peakR: 0, rmsL: 0, rmsR: 0 };
let wetLevel = 0.5;
let drive = 1.0;

function boot({ sound, query }) {
  // DAW mode detection
  const dawMode = query?.daw === "1";
  
  // Set up Web Audio processing chain
  // (triggered by envelope/FFT data, outputs via jweb~)
}

function sim({ sound }) {
  // Update envelope followers
  // Trigger sounds based on input analysis
}

function paint({ wipe, ink, screen }) {
  // Visualize FFT spectrum
  // Show input level meters
  // Display effect status
}

// Exported for M4L window.acPedal* functions
export function setFFT(data) { fftData = data; }
export function setEnvelope(pL, pR, rL, rR) {
  envelope = { peakL: pL, peakR: pR, rmsL: rL, rmsR: rR };
}
```

### Effect Ideas for pedal.mjs

1. **Visualizer Only** - Display input spectrum, pass audio through
2. **Envelope Follower** - Trigger synth notes based on input amplitude
3. **Spectral Freeze** - Analyze and hold FFT, generate frozen drone
4. **Vocoder-style** - Use input spectrum to modulate synth output
5. **Transient Detector** - Trigger drum hits on input transients

## Implementation Steps

### Phase 1: Basic Effect Shell ✅ COMPLETE
1. ✅ Create `pedal.mjs` piece with FFT visualization
2. ✅ Add `pedal` device to `devices.json` with `"type": "effect"`
3. ✅ Extend `build.py` with new `generate_effect_patcher()` function
4. ✅ Build AMXD with audio input/output chain

### Phase 2: Analysis Pipeline (NEXT)
1. Add FFT analysis in Max (`pfft~` → `js` → `jweb~`)
2. Add envelope followers (`peakamp~` → `snapshot~` → `jweb~`)
3. Wire up `window.acPedal*` functions

### Phase 3: Audio Generation
1. Web Audio synth triggered by envelope
2. Dry/wet mix controls
3. Parameter automation

---

## How to Test

### 1. Start the local dev server
```bash
npm run site  # or npm run aesthetic
```

### 2. Build and install the pedal device
```bash
cd ac-m4l
python3 build.py pedal --install
```

### 3. In Ableton Live
1. Find "AC 🎸 pedal" in the **Audio Effects** section of the browser
2. Drag it onto an **audio track** with audio playing
3. Observe the FFT visualization responding to input audio
4. The dry signal passes through; the wet signal (from jweb~) is mixed in

### 4. Test controls
- **Tap/Space**: Cycle effect modes (visualizer → envelope-synth → freeze → gate)
- **Up/Down arrows**: Adjust trigger threshold (for envelope-synth mode)

---

## Files Created/Modified

| File | Action | Description |
|------|--------|-------------|
| `system/public/aesthetic.computer/disks/pedal.mjs` | ✅ **CREATED** | AC piece for effect |
| `ac-m4l/devices.json` | ✅ **MODIFIED** | Added pedal device config with `"type": "effect"` |
| `ac-m4l/build.py` | ✅ **MODIFIED** | Added `generate_effect_patcher()` function |
| `ac-m4l/AC 🎸 pedal (localhost:8888).amxd` | ✅ **BUILT** | Generated effect device |
| `ac-m4l/ac-fft-analyzer.js` | 🔜 FUTURE | Max JS for FFT → jweb |
| `plans/pedal.md` | ✅ **CREATED** | This planning document |

## References

- [jweb~ documentation](https://docs.cycling74.com/reference/jweb~)
- [pfft~ documentation](https://docs.cycling74.com/reference/pfft~)
- [M4L Audio Effect Guidelines](https://github.com/Ableton/maxdevtools/blob/main/m4l-production-guidelines)
- Existing AC M4L integration: `ac-m4l/build.py`, `ac-m4l/ABLETON-INTEGRATION-PROGRESS.md`

## Open Questions

1. **Latency compensation**: How much latency does jweb~ add? Need to delay dry signal to match.
2. **Sample rate matching**: Browser AudioContext vs. Ableton - handled by existing `acDawSamplerate`
3. **FFT bin count**: 32? 64? 128? Balance between detail and message overhead
4. **Message rate**: How often to send FFT data? Every vector? Every N ms?

## Testing Checklist

- [ ] Audio passes through when loaded (dry signal)
- [ ] FFT visualization updates with input audio
- [ ] Wet signal (from web audio) mixes correctly
- [ ] Dry/wet control works
- [ ] No clicks/pops on parameter changes
- [ ] Works at 44.1kHz and 48kHz sample rates
- [ ] Multiple instances work simultaneously
