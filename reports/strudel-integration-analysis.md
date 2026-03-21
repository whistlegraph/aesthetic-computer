# Strudel Integration Analysis for KidLisp Jukebox

## Overview

Strudel is a web-based live coding environment for algorithmic music patterns, a JavaScript port of TidalCycles. It's licensed under AGPL-3.0, which requires source code disclosure for network services.

**Repository**: https://codeberg.org/uzu/strudel (cloned to `reference/strudel/`)

## Integration Options

### Option 1: `@strudel/embed` - Iframe Embed (Easiest)

Load Strudel REPL in an iframe:
```html
<script src="https://unpkg.com/@strudel/embed@latest"></script>
<strudel-repl>
<!--
s("bd sd hh sd").play()
-->
</strudel-repl>
```

**Pros**: Simplest, fully isolated, version pinning  
**Cons**: No access to audio output for analysis, no KidLisp integration

### Option 2: `@strudel/repl` - Direct Web Component

```html
<script src="https://unpkg.com/@strudel/repl@latest"></script>
<strudel-editor id="repl"><!--
s("bd sd hh sd").play()
--></strudel-editor>
<script>
  const repl = document.getElementById('repl');
  // Access editor: repl.editor.start(), .stop(), .evaluate(code)
</script>
```

**Pros**: Direct access to REPL API, can control playback  
**Cons**: Still no direct audio node access

### Option 3: `@strudel/web` - Headless Mode (Best for Integration)

```html
<script src="https://unpkg.com/@strudel/web@1.3.0"></script>
<script>
  initStrudel({
    prebake: () => samples('github:tidalcycles/dirt-samples'),
  });
  
  // Evaluate and play
  evaluate(`s("bd sd hh sd").play()`);
  
  // Or use pattern API directly
  s("bd sd hh sd").play();
  
  // Stop
  hush();
</script>
```

**Pros**: Full control, can inject custom audio routing  
**Cons**: Requires understanding Strudel pattern API

## Audio Analysis Integration

### Key Files in Strudel

1. **`packages/superdough/audioContext.mjs`**:
   - `getAudioContext()` - Returns shared AudioContext
   - `setAudioContext(ctx)` - Set custom AudioContext

2. **`packages/superdough/superdough.mjs`**:
   - `analysers` - Map of AnalyserNodes by ID
   - `getAnalyserById(id, fftSize, smoothingTimeConstant)` - Get/create analyser
   - `getAnalyzerData(type, id)` - Get 'time' or 'frequency' data

3. **`packages/superdough/superdoughoutput.mjs`**:
   - `SuperdoughOutput` - Main output controller
   - `SuperdoughAudioController` - Manages orbits (channels) and buses
   - `destinationGain` - Final output gain node before destination

### Hooking Into Audio Output

To get audio analysis from Strudel, we need to:

1. **Access the destination gain node**:
```javascript
import { getAudioContext } from '@strudel/superdough';

// After initStrudel(), the audio context is available
const ctx = getAudioContext();

// Create our own analyser
const analyser = ctx.createAnalyser();
analyser.fftSize = 512;

// We need to intercept the output - Strudel connects to ctx.destination
// Option: Create a custom destination wrapper
```

2. **Use Strudel's built-in analysers**:
```javascript
import { getAnalyserById, getAnalyzerData } from '@strudel/superdough';

// Get analyser (creates if needed)
const analyser = getAnalyserById(1, 512, 0.5);

// In animation loop:
const freqData = getAnalyzerData('frequency', 1);
const timeData = getAnalyzerData('time', 1);
```

**Issue**: Strudel's analysers are per-orbit, not global output. Need to investigate how to connect to global output.

### Recommended Integration Approach

1. **Use `@strudel/web` for headless control**
2. **Intercept audio at the destination**:

```javascript
// After initStrudel()
const ctx = getAudioContext();

// Create a gain node to intercept ALL audio
const masterGain = ctx.createGain();
const analyser = ctx.createAnalyser();
analyser.fftSize = 512;

// Monkey-patch or use custom AudioContext setup
// to route all Strudel output through our analyser
masterGain.connect(analyser);
analyser.connect(ctx.destination);
```

3. **Alternative: Modify superdoughoutput.mjs**

In `SuperdoughOutput.initializeAudio()`:
```javascript
this.destinationGain.connect(audioContext.destination);
// Add: this.destinationGain.connect(window.strudelAnalyser);
```

## Jukebox Implementation Plan

### Phase 1: Basic Strudel Playback

1. Add Strudel tab to Jukebox
2. Load `@strudel/web` dynamically
3. Provide code editor for Strudel patterns
4. Play/Stop buttons call `evaluate()` and `hush()`

### Phase 2: Audio Analysis

1. Create custom AudioContext wrapper that includes our analyser
2. Call `setAudioContext(customCtx)` before `initStrudel()`
3. Route Strudel output through our analyser
4. Send analysis data to KidLisp (`amp`, `bass`, `mid`, `treble`, `kick`)

### Phase 3: KidLisp Integration

1. Strudel patterns can be stored alongside KidLisp code
2. `(strudel "s('bd sd')")` function in KidLisp
3. Live audio variables react to Strudel playback

## Sample Code: Custom Audio Context with Analysis

```javascript
// Create our AudioContext with analyser in the chain
const audioContext = new AudioContext();
const masterGain = audioContext.createGain();
const analyser = audioContext.createAnalyser();
analyser.fftSize = 512;

// Create a proxy destination
const proxyDestination = masterGain;
masterGain.connect(analyser);
analyser.connect(audioContext.destination);

// Set up Strudel with our context
import { setAudioContext } from '@strudel/superdough';
setAudioContext(audioContext);

// Now initStrudel will use our context
initStrudel();

// Analysis loop
function analyze() {
  const freqData = new Uint8Array(analyser.frequencyBinCount);
  const timeData = new Uint8Array(analyser.fftSize);
  
  analyser.getByteFrequencyData(freqData);
  analyser.getByteTimeDomainData(timeData);
  
  // Calculate amp, bass, mid, treble, kick...
  // Send to KidLisp
  
  requestAnimationFrame(analyze);
}
```

## License Considerations

Strudel uses AGPL-3.0:
- Source code must be made available for network services
- Derivative works must use same license
- Our use is embedding/integration, not modification
- If we modify Strudel code, must share those changes
- aesthetic.computer is already open source, so this is fine

## Dependencies

@strudel/web (1.3.0) bundles:
- @strudel/core - Pattern engine
- @strudel/mini - Mini-notation parser
- @strudel/tonal - Music theory
- @strudel/webaudio - WebAudio integration
- @strudel/transpiler - Code transformation
- superdough - Synthesis engine

Total unpacked size: ~1.55 MB

## Next Steps

1. Create proof-of-concept in kidlisp.com with @strudel/web
2. Test audio interception approach
3. Implement pattern editor in Jukebox tab
4. Connect analysis to KidLisp audio variables
5. Add preset patterns for quick testing
