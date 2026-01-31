# Offline Audio Rendering & Clock→USB Export - Technical Feasibility Report

**Date:** January 31, 2026  
**Author:** AC Research  
**Status:** Advanced Research / Architecture Planning  
**Related Pieces:** `stick.mjs`, `clock.mjs`  
**Related Libs:** `speaker.mjs`, `synth.mjs`

---

## The Vision

```bash
stick 30 clock ^c,,,,,,
```

This command would:
1. Run `clock.mjs` with melody `^c,,,,,,` **offline** (not real-time)
2. Generate **30 seconds** of audio as fast as possible
3. Export as WAV/MP3
4. Write directly to the selected USB stick

The audio is rendered computationally, not played through speakers - like rendering a video vs watching it live.

---

## Why This Is Sophisticated

AC's audio system currently runs in **real-time** via AudioWorklet:

```
clock.mjs → bios.mjs → speaker.mjs (AudioWorkletProcessor)
                           ↓
                    Web Audio output (speakers)
```

The synth runs sample-by-sample at 44.1kHz, generating audio as it plays. To render offline, we need to:

1. **Decouple from real-time** - Run the synth faster than playback
2. **Replace AudioWorklet** - Use OfflineAudioContext or pure JS rendering
3. **Capture all samples** - Buffer the entire output
4. **Encode to file** - WAV (easy) or MP3 (needs encoder)

---

## Technical Approach: Three Paths

### Path A: OfflineAudioContext (Web Audio API)

The Web Audio API has a built-in offline rendering mode:

```javascript
// Create offline context for 30 seconds at 44.1kHz, stereo
const offlineCtx = new OfflineAudioContext(2, 44100 * 30, 44100);

// Connect audio graph to offline context
// ...build audio nodes...

// Render as fast as possible
const audioBuffer = await offlineCtx.startRendering();

// audioBuffer contains all samples - export to WAV
```

**The Problem:** AC's audio is generated in `speaker.mjs` (AudioWorkletProcessor), which can't directly run in OfflineAudioContext the same way. The worklet code lives in a separate thread/context.

**Workaround:** Port the worklet logic to run as a ScriptProcessorNode or custom offline renderer.

### Path B: Pure JavaScript Offline Renderer

Extract the synth logic from `speaker.mjs` and `synth.mjs` and run it in a tight loop:

```javascript
// offline-renderer.mjs
import Synth from "./sound/synth.mjs";

function renderOffline(melodySpec, durationSec, sampleRate = 44100) {
  const numSamples = sampleRate * durationSec;
  const leftChannel = new Float32Array(numSamples);
  const rightChannel = new Float32Array(numSamples);
  
  // Parse melody, create synths
  const synths = createSynthsFromMelody(melodySpec);
  
  // Render sample by sample
  for (let i = 0; i < numSamples; i++) {
    let left = 0, right = 0;
    
    // Process beat scheduling
    const currentTime = i / sampleRate;
    processBeatAt(currentTime, synths);
    
    // Mix all active synths
    for (const synth of activeSynths) {
      const sample = synth.next(0);
      left += sample;
      right += sample;
    }
    
    leftChannel[i] = left;
    rightChannel[i] = right;
  }
  
  return { leftChannel, rightChannel, sampleRate };
}
```

**Pros:**
- Pure JS, runs anywhere (browser, Node, Electron)
- Can be made extremely fast (no real-time constraints)
- Full control over the rendering process

**Cons:**
- Must extract/duplicate logic from `speaker.mjs` and `synth.mjs`
- Need to replicate beat scheduling, effects, etc.

### Path C: Modified AudioWorklet with VST Bridge Mode

AC already has VST bridge mode in `speaker.mjs`:

```javascript
// From speaker.mjs lines 175-195
#vstBridgeEnabled = false;
#vstSampleBuffer = { left: [], right: [] };

// When VST bridge is enabled, samples are buffered instead of output
if (this.#vstBridgeEnabled) {
  // Buffer samples for external retrieval
  this.#vstSampleBuffer.left.push(...leftSamples);
  this.#vstSampleBuffer.right.push(...rightSamples);
}
```

**Adaptation:** Create an "offline capture" mode that:
1. Enables VST bridge mode
2. Runs the AudioContext in a headless/muted state
3. Fast-forwards time by scheduling many beats rapidly
4. Collects all samples via `vst:get-samples` message

---

## Recommended Architecture

### Phase 1: Offline Synth Module

Create `lib/offline-synth.mjs` that extracts pure synth generation:

```javascript
// lib/offline-synth.mjs
// Reuses synth.mjs logic but runs without AudioWorkletProcessor

export class OfflineSynth {
  constructor(type, options) {
    // Same as Synth constructor
  }
  
  next() {
    // Generate one sample (same as synth.mjs)
  }
}

export function renderMelody(melody, durationSec, options = {}) {
  const { sampleRate = 44100, bpm = 60 } = options;
  // Parse melody, schedule notes, render all samples
  return { left: Float32Array, right: Float32Array };
}
```

### Phase 2: Clock Offline Runner

Create `lib/clock-offline.mjs`:

```javascript
// lib/clock-offline.mjs
import { parseMelody, parseSequentialMelody } from "./melody-parser.mjs";
import { OfflineSynth, renderMelody } from "./offline-synth.mjs";

export async function renderClockOffline(melodyString, durationSec, options = {}) {
  const { sampleRate = 44100, bpm = 60, waveform = "sine" } = options;
  
  // Parse the melody (reuse clock.mjs parsing)
  const melody = parseMelody(melodyString);
  
  // Calculate total samples
  const totalSamples = Math.ceil(sampleRate * durationSec);
  const left = new Float32Array(totalSamples);
  const right = new Float32Array(totalSamples);
  
  // Beat timing
  const samplesPerBeat = sampleRate * (60 / bpm);
  let currentBeat = 0;
  let noteIndex = 0;
  let activeSynths = [];
  
  // Render loop
  for (let i = 0; i < totalSamples; i++) {
    const beatProgress = i / samplesPerBeat;
    
    // Trigger notes on beat boundaries
    if (beatProgress >= currentBeat) {
      // Get note from melody
      const note = melody[noteIndex % melody.length];
      if (note && note.tone) {
        activeSynths.push(new OfflineSynth(waveform, {
          tone: note.tone,
          duration: samplesPerBeat * 0.8, // Note duration
          volume: note.volume || 1.0
        }));
      }
      noteIndex++;
      currentBeat++;
    }
    
    // Mix active synths
    let sampleL = 0, sampleR = 0;
    activeSynths = activeSynths.filter(synth => {
      if (!synth.playing) return false;
      const s = synth.next();
      sampleL += s;
      sampleR += s;
      return true;
    });
    
    left[i] = sampleL;
    right[i] = sampleR;
  }
  
  return { left, right, sampleRate };
}
```

### Phase 3: Stick Integration

Update `stick.mjs` to use the offline renderer:

```javascript
// In stick.mjs
import { renderClockOffline } from "../lib/clock-offline.mjs";

async function addClockTrack(melodyString, durationSec) {
  status = `Rendering: clock ${melodyString}...`;
  
  const audio = await renderClockOffline(melodyString, durationSec, {
    sampleRate: 44100,
    bpm: 60,
    waveform: "sine"
  });
  
  const wavData = encodeWAV(audio.left, audio.right, audio.sampleRate);
  
  tracks.push({
    name: `clock-${melodyString.slice(0, 10)}`,
    type: "clock",
    data: wavData,
    duration: durationSec
  });
  
  status = `Added ${durationSec}s clock track`;
}
```

### Phase 4: CLI-Style Command Parsing

Support `stick 30 clock ^c,,,,,,` syntax:

```javascript
// Parse: stick <duration> <source> <params>
function parseStickCommand(params) {
  // params = ["30", "clock", "^c,,,,,,"]
  const duration = parseFloat(params[0]);
  const source = params[1]; // "clock", "tone", "noise", etc.
  const sourceParams = params.slice(2).join(" ");
  
  return { duration, source, sourceParams };
}

// In boot()
if (params.length >= 2) {
  const { duration, source, sourceParams } = parseStickCommand(params);
  if (source === "clock") {
    await addClockTrack(sourceParams, duration);
  }
}
```

---

## WAV Encoding (Already in stick.mjs)

The WAV encoder is straightforward and already implemented. For stereo:

```javascript
function encodeWAV(leftChannel, rightChannel, sampleRate) {
  const numSamples = leftChannel.length;
  const buffer = new ArrayBuffer(44 + numSamples * 4); // 16-bit stereo
  const view = new DataView(buffer);
  
  // RIFF header
  writeString(view, 0, "RIFF");
  view.setUint32(4, 36 + numSamples * 4, true);
  writeString(view, 8, "WAVE");
  
  // fmt chunk
  writeString(view, 12, "fmt ");
  view.setUint32(16, 16, true);       // Chunk size
  view.setUint16(20, 1, true);        // PCM
  view.setUint16(22, 2, true);        // Stereo
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * 4, true); // Byte rate
  view.setUint16(32, 4, true);        // Block align
  view.setUint16(34, 16, true);       // Bits per sample
  
  // data chunk
  writeString(view, 36, "data");
  view.setUint32(40, numSamples * 4, true);
  
  // Interleaved samples
  for (let i = 0; i < numSamples; i++) {
    view.setInt16(44 + i * 4, leftChannel[i] * 0x7FFF, true);
    view.setInt16(46 + i * 4, rightChannel[i] * 0x7FFF, true);
  }
  
  return buffer;
}
```

---

## MP3 Encoding Options

For smaller files, encode to MP3:

### Option 1: lamejs (Pure JS)

```javascript
import lamejs from 'lamejs';

function encodeMP3(leftChannel, rightChannel, sampleRate) {
  const mp3encoder = new lamejs.Mp3Encoder(2, sampleRate, 128);
  
  // Convert Float32 to Int16
  const leftInt = floatTo16BitPCM(leftChannel);
  const rightInt = floatTo16BitPCM(rightChannel);
  
  const mp3Data = [];
  const blockSize = 1152;
  
  for (let i = 0; i < leftInt.length; i += blockSize) {
    const leftChunk = leftInt.subarray(i, i + blockSize);
    const rightChunk = rightInt.subarray(i, i + blockSize);
    const mp3buf = mp3encoder.encodeBuffer(leftChunk, rightChunk);
    if (mp3buf.length > 0) mp3Data.push(mp3buf);
  }
  
  mp3Data.push(mp3encoder.flush());
  return new Blob(mp3Data, { type: 'audio/mp3' });
}
```

### Option 2: Native in Electron

Shell out to ffmpeg:

```javascript
// In Electron main process
const { execSync } = require('child_process');

function convertToMP3(wavPath, mp3Path) {
  execSync(`ffmpeg -i "${wavPath}" -codec:a libmp3lame -qscale:a 2 "${mp3Path}"`);
}
```

---

## Performance Considerations

### Rendering Speed

Offline rendering can be **much faster** than real-time:

| Duration | Real-time | Offline (estimated) |
|----------|-----------|---------------------|
| 30 sec   | 30 sec    | < 1 sec             |
| 5 min    | 5 min     | 2-5 sec             |
| 1 hour   | 1 hour    | 30-60 sec           |

The tight loop in pure JS can process millions of samples per second.

### Memory

A 30-second stereo track at 44.1kHz:
- Float32: 30 × 44100 × 2 × 4 = **10.6 MB**
- Int16 WAV: 30 × 44100 × 2 × 2 = **5.3 MB**
- MP3 128kbps: ~**480 KB**

For longer tracks, consider streaming to disk or chunked processing.

---

## Integration with Clock Features

The offline renderer should support clock's full feature set:

| Feature | Implementation |
|---------|---------------|
| Waveforms (`{square}`, etc.) | Use matching OfflineSynth type |
| Volume (`{0.5}`) | Pass to synth volume |
| Hz shift (`{100hz}`) | Add to frequency |
| Parallel tracks | Render each track, sum together |
| Sequential sections (`>`) | Render sections in order |
| Struck notes (`^`) | Use shorter duration + decay |
| Duration modifiers (`.`, `,`) | Adjust note length |
| Swing (`[`, `]`) | Offset beat timing |
| Stample (`{#code}`) | Load sample data, render as sample type |

---

## Implementation Roadmap

### Week 1: Core Offline Renderer
- [ ] Extract synth logic to `offline-synth.mjs`
- [ ] Implement basic waveforms (sine, square, triangle, sawtooth)
- [ ] Create `renderMelody()` with beat scheduling
- [ ] Test with simple melodies

### Week 2: Clock Integration
- [ ] Create `clock-offline.mjs` using melody parser
- [ ] Support waveform types and volume
- [ ] Support parallel tracks
- [ ] Support sequential sections

### Week 3: Stick Integration
- [ ] Add command parsing to stick.mjs
- [ ] Integrate offline renderer
- [ ] Add progress indication for long renders
- [ ] Test full pipeline: `stick 30 clock ^c,,,,,,`

### Week 4: Polish & MP3
- [ ] Add lamejs MP3 encoding
- [ ] Optimize memory for long tracks
- [ ] Add preview playback before export
- [ ] Electron enhancements (ffmpeg, batch export)

---

## Code Reuse Strategy

To minimize duplication with `speaker.mjs` and `synth.mjs`:

```
synth.mjs (current)
    ↓ extract core logic
synth-core.mjs (new, pure functions)
    ↓                    ↓
speaker.mjs          offline-synth.mjs
(real-time)          (offline rendering)
```

The `synth-core.mjs` would contain:
- Waveform generators (sine, square, etc.)
- ADSR envelope logic
- Filter calculations
- Frequency/volume interpolation

Both `speaker.mjs` and `offline-synth.mjs` would import from this shared core.

---

## Future Extensions

1. **Batch Export** - `stick 30 clock c d e f g` creates 5 tracks
2. **Template Sticks** - Pre-defined DJ sets with folder structure
3. **Live Preview** - Render small chunk, play back, confirm, then full render
4. **Rekordbox Metadata** - Generate BPM analysis, waveforms, cue points
5. **Sample Pack Mode** - Export individual sounds as one-shots

---

## Conclusion

Offline audio rendering from AC's clock notation is **absolutely feasible**. The key insight is that `synth.mjs` already contains all the synthesis logic - it just needs to be extracted and run in a tight loop without the AudioWorklet real-time constraints.

The `stick 30 clock ^c,,,,,,` dream is achievable with:
1. Pure JS offline renderer (~500 lines of new code)
2. Reuse of existing melody parsing and synth logic
3. WAV encoding (already done) + optional MP3 via lamejs

This would make AC a legitimate audio production tool - not just for live performance, but for creating static DJ content, sample packs, and sound libraries.

---

## References

- [OfflineAudioContext MDN](https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext)
- [lamejs MP3 encoder](https://github.com/zhuker/lamejs)
- [WAV file format spec](http://soundfile.sapp.org/doc/WaveFormat/)
- AC source: [speaker.mjs](system/public/aesthetic.computer/lib/speaker.mjs)
- AC source: [synth.mjs](system/public/aesthetic.computer/lib/sound/synth.mjs)
- AC source: [clock.mjs](system/public/aesthetic.computer/disks/clock.mjs)
- AC source: [melody-parser.mjs](system/public/aesthetic.computer/lib/melody-parser.mjs)
