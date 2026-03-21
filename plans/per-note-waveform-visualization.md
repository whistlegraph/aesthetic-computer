# Plan: Per-Note Waveform Visualization for Notepat

## Problem Statement
Currently, `notepat.mjs` can only visualize a single mixed waveform from `sound.speaker.waveforms` which combines all playing sounds into one audio stream. When multiple notes are playing simultaneously, we cannot visually separate each note's individual waveform - they're all mixed together in the master output.

## Goal
Enable each active note in notepat to have its own separate waveform data for individual visualization, so that when displaying the visualizer, each note's lane can show its unique audio characteristics rather than all showing the same mixed waveform.

## Current Architecture

### 1. **speaker.mjs (Audio Worklet)**
- Location: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/speaker.mjs`
- Manages the `#queue` array containing all active `Synth` instances
- In the `process()` method (lines ~515-590):
  - Loops through all instruments in `this.#queue`
  - Calls `instrument.next(s)` for each to get amplitude
  - **Mixes all instruments together** into `output[0][s]` (left) and `output[1][s]` (right)
  - Samples the **mixed output** into `waveformLeft` and `waveformRight` arrays
  - Stores these in `#currentWaveformLeft` and `#currentWaveformRight`
- Message handler (lines ~105-135):
  - Responds to `"get-waveforms"` messages
  - Sends back the mixed waveforms via `postMessage({ type: "waveforms", content: { left, right } })`

### 2. **bios.mjs (Audio System Bridge)**
- Location: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`
- Sets up the speaker AudioWorklet (lines ~1385-1490)
- Creates `requestSpeakerWaveforms()` function (line ~1471) that posts `"get-waveforms"` message
- Receives waveform data via message handler (line ~1484) and forwards to disk via `send({ type: "waveforms", content })`
- Also handles sound lifecycle: `killSound()`, `updateSound()`, etc.

### 3. **disk.mjs (Piece Runtime)**
- Receives waveform messages and makes available via `sound.speaker.waveforms` object
- Pieces like `notepat.mjs` access via `sound.speaker.waveforms.left` and `.right`

### 4. **notepat.mjs (Current Implementation)**
- Maintains `sounds` object tracking active notes: `{ "3c": { sound: Synth }, "4e": { sound: Synth }, ... }`
- Each note has a unique `sound.id` (UUID)
- In visualizer mode, gets `sound.speaker.waveforms.left` - but this is the **mixed waveform of all notes**

## Proposed Solution

### Phase 1: Capture Per-Instrument Waveforms in speaker.mjs

**Modify**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/speaker.mjs`

1. Add new private field to track per-instrument waveforms:
```javascript
#perInstrumentWaveforms = new Map(); // Map<instrumentId, {left: [], right: []}>
```

2. In the `process()` method sample loop (around line 527):
```javascript
for (const instrument of this.#queue) {
  const amplitude = instrument.next(s);
  
  // Track per-instrument output BEFORE mixing
  if (!this.#perInstrumentWaveforms.has(instrument.id)) {
    this.#perInstrumentWaveforms.set(instrument.id, { left: [], right: [] });
  }
  
  const leftOutput = instrument.pan(0, amplitude);
  const rightOutput = instrument.pan(1, amplitude);
  
  // Sample individual instrument waveform
  if (s % waveformRate === 0) {
    const instrumentWaveforms = this.#perInstrumentWaveforms.get(instrument.id);
    instrumentWaveforms.left.push(leftOutput);
    instrumentWaveforms.right.push(rightOutput);
  }
  
  // Then mix into master output
  output[0][s] += leftOutput;
  output[1][s] += rightOutput;
  
  // ... volume calculation ...
}
```

3. Maintain waveform buffer size per instrument (similar to global waveform):
```javascript
// After the sample loop
for (const [id, waveforms] of this.#perInstrumentWaveforms) {
  // Remove old samples if exceeding waveformSize
  if (waveforms.left.length > waveformSize) {
    const excess = waveforms.left.length - waveformSize;
    waveforms.left.splice(0, excess);
    waveforms.right.splice(0, excess);
  }
}

// Clean up waveforms for dead instruments
this.#perInstrumentWaveforms = new Map(
  [...this.#perInstrumentWaveforms].filter(([id]) => 
    this.#queue.some(inst => inst.id === id)
  )
);
```

4. Add new message handler:
```javascript
if (msg.type === "get-per-instrument-waveforms") {
  // Convert Map to plain object for postMessage
  const waveformsObj = {};
  for (const [id, waveforms] of this.#perInstrumentWaveforms) {
    waveformsObj[id] = {
      left: [...waveforms.left],
      right: [...waveforms.right]
    };
  }
  
  this.port.postMessage({
    type: "per-instrument-waveforms",
    content: waveformsObj
  });
}
```

### Phase 2: Update bios.mjs to Request and Forward Per-Instrument Data

**Modify**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/bios.mjs`

1. Create new request function (around line 1471, next to `requestSpeakerWaveforms`):
```javascript
requestPerInstrumentWaveforms = function () {
  speakerProcessor.port.postMessage({ type: "get-per-instrument-waveforms" });
};
```

2. Add message handler (around line 1484, in the speaker message handler):
```javascript
if (msg.type === "per-instrument-waveforms") {
  send({ type: "per-instrument-waveforms", content: msg.content });
}
```

3. Add to exports/make available (around line 1027):
```javascript
requestPerInstrumentWaveforms,
```

4. Add message handler in main receive function (around line 8343):
```javascript
if (type === "get-per-instrument-waveforms") {
  requestPerInstrumentWaveforms?.();
}
```

### Phase 3: Update disk.mjs to Expose Per-Instrument Waveforms

**Modify**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/disk.mjs`

1. Add storage for per-instrument waveforms:
```javascript
// In the speaker object initialization
sound.speaker.perInstrumentWaveforms = {}; // { instrumentId: { left: [], right: [] } }
```

2. Add receiver for the new message type (wherever speaker waveforms are received):
```javascript
if (type === "per-instrument-waveforms") {
  sound.speaker.perInstrumentWaveforms = content;
}
```

3. Request per-instrument waveforms alongside regular waveforms (in the frame/sim loop):
```javascript
send({ type: "get-per-instrument-waveforms" });
```

### Phase 4: Update notepat.mjs to Use Per-Instrument Waveforms

**Modify**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/notepat.mjs`

1. In `pictureLines()` function (around line 2040), instead of cycling through notes with the same waveform:
```javascript
activeNotes.forEach((trailNote, noteIndex) => {
  if (!trailNote) return;
  
  const noteData = sounds[trailNote];
  if (!noteData?.sound?.id) return;
  
  // Get THIS note's specific waveform
  const instrumentId = noteData.sound.id;
  const instrumentWaveforms = sound.speaker.perInstrumentWaveforms?.[instrumentId];
  
  if (!instrumentWaveforms?.left || instrumentWaveforms.left.length < 16) {
    return; // Skip if no waveform data available
  }
  
  const noteWaveform = instrumentWaveforms.left;
  const color = colorFromNote(trailNote, num);
  const laneHeight = picture.height / activeNotes.length;
  const laneCenterY = (noteIndex + 0.5) * laneHeight;
  
  const step = picture.width / noteWaveform.length;
  
  for (let i = 1; i < noteWaveform.length; i++) {
    const x1 = (i - 1) * step;
    const y1 = laneCenterY + noteWaveform[i - 1] * laneHeight * 0.4;
    const x2 = i * step;
    const y2 = laneCenterY + noteWaveform[i] * laneHeight * 0.4;
    
    ink(...color).line(x1, y1, x2, y2);
  }
});
```

## Implementation Considerations

### Performance
- **Memory**: Each active note will store ~220 samples (at 44.1kHz/200), so 10 notes = ~2200 floats = ~9KB
- **CPU**: Additional array operations per frame, but minimal since we're already iterating instruments
- **Optimization**: Could throttle per-instrument waveform updates (update every N frames instead of every frame)

### Edge Cases
- **Dead instruments**: Clean up waveform data when instruments are killed (already handled in Phase 1)
- **No waveform data yet**: Check for existence before visualizing (handled in Phase 4)
- **ID mismatch**: Synth IDs should be stable throughout playback - verify this assumption

### Alternative: Simplified Approach
If per-instrument proves too complex, could instead:
- Tag each Synth with metadata (e.g., `instrument.noteLabel = "3c"`)
- Only sample waveforms for instruments with matching labels
- Still mix in speaker.mjs but provide filtered waveforms per label

## Testing Plan
1. Add console logs to verify per-instrument waveforms are captured
2. Test with 1, 2, and 5+ simultaneous notes
3. Verify waveforms differ between notes (play different frequencies)
4. Check memory usage doesn't balloon
5. Verify waveforms clean up when notes stop

## Future Enhancements
- Expose per-instrument amplitudes and frequencies
- Allow filtering by instrument type (synth vs sample)
- Add API to request waveforms for specific instrument IDs
- Consider WebAssembly for waveform processing if performance becomes an issue
