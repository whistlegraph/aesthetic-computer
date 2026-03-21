# KidLisp.com Jukebox Feature - Technical Analysis

**Date:** January 28, 2026  
**Author:** Technical Analysis  
**Status:** Planning / Specification

---

## 1. Feature Overview

Add a **Jukebox tab** to the kidlisp.com editor that:
1. Plays audio tracks from `system/public/assets/oskie/wipppps/`
2. Extracts real-time audio data (amplitude, beat detection, frequency bands)
3. Sends audio parameters to the KidLisp iframe for audio-reactive visuals
4. Broadcasts audio data over PJ (Piece Jockey) for remote live visuals

---

## 2. Current Architecture Analysis

### 2.1 Audio Assets Location
```
system/public/assets/oskie/wipppps/
├── ARrrGh.wav
├── BLuRP.wav
├── GrRr.wav
├── HoNK.wav
├── KLOnK.wav
├── PaFf_master.wav
├── WHoOSh.wav
├── wipppps_cover.jpeg
└── zzzZWAP.wav
```

### 2.2 Existing Audio → KidLisp Pipeline (wipppps.mjs)

The `wipppps.mjs` piece demonstrates the complete audio-visual integration:

**Audio Data Flow:**
```
sound.speaker.amplitudes.left  → amp (0-10 scale)
sound.speaker.frequencies.left → frequency bands
sound.speaker.beat.detected    → kick (0 or 1)
```

**KidLisp Global Variables (lib/kidlisp.mjs:3161-3195):**
```javascript
// Audio globals available in KidLisp code:
this.globalDef.amp = 0;      // Current audio amplitude (0-10 scale)
this.globalDef.leftAmp = 0;  // Left channel amplitude
this.globalDef.rightAmp = 0; // Right channel amplitude
this.globalDef.beat = 0;     // Beat detection (0 or 1)
this.globalDef.kick = 0;     // Kick drum detected (0 or 1)
this.globalDef.mic = 0;      // Microphone amplitude
```

**Update Method (lib/kidlisp.mjs:3161):**
```javascript
updateAudioGlobals(audioData) {
  if (audioData && typeof audioData === 'object') {
    if (typeof audioData.amp === 'number') this.globalDef.amp = audioData.amp;
    if (typeof audioData.kick === 'number') this.globalDef.kick = audioData.kick;
    // ... etc
  }
}
```

### 2.3 KidLisp Code Using Audio (wipppps.mjs examples)

```lisp
;; Audio-reactive circle with kick drum
(ink (? lime magenta 0) (+ 100 (* kick 155)))
(circle (/ width 2) (/ height 2) (+ 3 (* kick 4)))

;; Beat-reactive spin
(spin (2s... -1.125 1.125))

;; Amplitude-based effects
(def beat (* kick 12))
(def pulse (* amp 8))
(tri x1 y1 x2 y2 cx cy)
```

### 2.4 KidLisp.com Editor → Iframe Communication

**Current Message Types (kidlisp.com/index.html):**
```javascript
// Send code to preview iframe
previewIframe.contentWindow.postMessage({
  type: 'kidlisp-reload',  // Full code reload
  code: code,
  createCode: false,
  enableTrace: false
}, aestheticUrl);

// Slide mode (low-latency value updates)
previewIframe.contentWindow.postMessage({
  type: 'kidlisp-slide',
  code: code
}, aestheticUrl);
```

### 2.5 PJ Broadcasting System (kidlisp.com/index.html:22670-22750)

**BroadcastChannel (same-origin):**
```javascript
const bc = new BroadcastChannel(`kidlisp-channel-${codeChannelId}`);
bc.postMessage({
  type: 'code-update',      // or 'slide-update'
  code: code,
  channelId: codeChannelId,
  timestamp: Date.now()
});
```

**WebSocket (cross-origin via session-server):**
```javascript
pjWs.send(JSON.stringify({
  type: 'code',  // or 'slide'
  content: {
    piece: code,
    source: code,
    codeChannel: codeChannelId
  }
}));
```

---

## 3. Proposed Architecture

### 3.1 DOM-Based Jukebox Component

```
┌─────────────────────────────────────────────────────────────┐
│  KidLisp.com Editor                                         │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ Tabs: [Examples] [Recent] [History] [🎵 Jukebox]      │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ Jukebox Panel (DOM-based, outside iframe)             │  │
│  │ ┌─────────────────────────────────────────────────┐   │  │
│  │ │ 🎵 Now Playing: zzzZWAP.wav                     │   │  │
│  │ │ [▶️ Play] [⏸️ Pause] [⏹️ Stop]                   │   │  │
│  │ │ ━━━━━━━━●━━━━━━━━ 2:34 / 5:12                   │   │  │
│  │ │ Volume: ━━━━━━━●━━━ 70%                          │   │  │
│  │ └─────────────────────────────────────────────────┘   │  │
│  │ ┌─────────────────────────────────────────────────┐   │  │
│  │ │ Track List:                                      │   │  │
│  │ │ • ARrrGh.wav                                     │   │  │
│  │ │ • BLuRP.wav                                      │   │  │
│  │ │ • GrRr.wav                                       │   │  │
│  │ │ • HoNK.wav                                       │   │  │
│  │ │ • zzzZWAP.wav  ◀ Playing                        │   │  │
│  │ └─────────────────────────────────────────────────┘   │  │
│  │ ┌─────────────────────────────────────────────────┐   │  │
│  │ │ Audio Meters (real-time):                        │   │  │
│  │ │ amp: ████████░░ 0.73                             │   │  │
│  │ │ kick: ●                                          │   │  │
│  │ │ L/R: ████░░░░ / ███████░░░                       │   │  │
│  │ └─────────────────────────────────────────────────┘   │  │
│  └───────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ KidLisp Preview Iframe                                │  │
│  │ (receives audio data via postMessage)                 │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### 3.2 Data Flow Diagram

```
┌──────────────────┐     ┌───────────────────────────────────────────┐
│ Audio Files      │     │ Jukebox (DOM - kidlisp.com/index.html)    │
│ (wipppps/*.wav)  │────▶│                                           │
└──────────────────┘     │ ┌─────────────────────────────────────┐   │
                         │ │ Web Audio API                        │   │
                         │ │ • AudioContext                       │   │
                         │ │ • AnalyserNode (FFT)                 │   │
                         │ │ • ScriptProcessorNode (amplitude)    │   │
                         │ └─────────────────────────────────────┘   │
                         │                │                          │
                         │                ▼                          │
                         │ ┌─────────────────────────────────────┐   │
                         │ │ Audio Data Extraction (60fps)        │   │
                         │ │ { amp, leftAmp, rightAmp, kick, ... }│   │
                         │ └─────────────────────────────────────┘   │
                         └───────────────┬───────────────────────────┘
                                         │
              ┌──────────────────────────┼──────────────────────────┐
              │                          │                          │
              ▼                          ▼                          ▼
┌─────────────────────────┐  ┌─────────────────────┐  ┌─────────────────────────┐
│ Local Preview Iframe    │  │ BroadcastChannel    │  │ WebSocket (pjWs)        │
│ (postMessage)           │  │ (same-origin PJ)    │  │ (cross-origin PJ)       │
│                         │  │                     │  │                         │
│ type: 'kidlisp-audio'   │  │ type: 'audio-data'  │  │ type: 'audio'           │
│ data: { amp, kick, ... }│  │ data: { ... }       │  │ content: { ... }        │
└─────────────────────────┘  └─────────────────────┘  └─────────────────────────┘
              │                          │                          │
              ▼                          ▼                          ▼
┌─────────────────────────┐  ┌─────────────────────┐  ┌─────────────────────────┐
│ KidLisp Runtime         │  │ PJ Window           │  │ Remote PJ Viewers       │
│ (lib/kidlisp.mjs)       │  │ (pj.html)           │  │ (pj.kidlisp.com)        │
│                         │  │                     │  │                         │
│ updateAudioGlobals()    │  │ Forward to iframe   │  │ Forward to iframe       │
│ ↓                       │  │ ↓                   │  │ ↓                       │
│ globalDef.amp = data.amp│  │ postMessage to      │  │ postMessage to          │
│ globalDef.kick = ...    │  │ kidlisp iframe      │  │ kidlisp iframe          │
└─────────────────────────┘  └─────────────────────┘  └─────────────────────────┘
```

### 3.3 Message Protocol Design

**New Message Type for Audio Data:**
```javascript
// Editor → Preview Iframe
{
  type: 'kidlisp-audio',
  data: {
    amp: 0.73,           // 0-10 scale (normalized amplitude)
    leftAmp: 0.65,       // Left channel 0-10
    rightAmp: 0.81,      // Right channel 0-10
    kick: 1,             // 0 or 1 (beat detected)
    beat: 1,             // 0 or 1 (alternative beat signal)
    frequencies: [...],   // Optional: frequency band data
    timestamp: 1706400000 // For sync
  }
}
```

**PJ Broadcast Extension:**
```javascript
// BroadcastChannel
bc.postMessage({
  type: 'audio-data',
  channelId: codeChannelId,
  data: { amp, kick, leftAmp, rightAmp },
  timestamp: Date.now()
});

// WebSocket
pjWs.send(JSON.stringify({
  type: 'audio',
  content: {
    codeChannel: codeChannelId,
    amp, kick, leftAmp, rightAmp,
    timestamp: Date.now()
  }
}));
```

---

## 4. Implementation Plan

### Phase 1: Web Audio Infrastructure (kidlisp.com/index.html)

**4.1.1 Audio Context & Analyser Setup**
```javascript
// Jukebox audio state
let jukeboxAudioContext = null;
let jukeboxAnalyser = null;
let jukeboxSource = null;
let jukeboxAudioElement = null;
let jukeboxAnimationFrame = null;

const JUKEBOX_TRACKS = [
  'ARrrGh.wav', 'BLuRP.wav', 'GrRr.wav', 'HoNK.wav',
  'KLOnK.wav', 'PaFf_master.wav', 'WHoOSh.wav', 'zzzZWAP.wav'
];

function initJukeboxAudio() {
  jukeboxAudioContext = new (window.AudioContext || window.webkitAudioContext)();
  jukeboxAnalyser = jukeboxAudioContext.createAnalyser();
  jukeboxAnalyser.fftSize = 256; // 128 frequency bins
  jukeboxAnalyser.smoothingTimeConstant = 0.3;
  
  jukeboxAudioElement = document.createElement('audio');
  jukeboxAudioElement.crossOrigin = 'anonymous';
  
  jukeboxSource = jukeboxAudioContext.createMediaElementSource(jukeboxAudioElement);
  jukeboxSource.connect(jukeboxAnalyser);
  jukeboxAnalyser.connect(jukeboxAudioContext.destination);
}
```

**4.1.2 Audio Data Extraction Loop**
```javascript
const frequencyData = new Uint8Array(jukeboxAnalyser.frequencyBinCount);
const timeDomainData = new Uint8Array(jukeboxAnalyser.fftSize);

let lastKickTime = 0;
const KICK_COOLDOWN_MS = 100;

function extractAudioData() {
  jukeboxAnalyser.getByteFrequencyData(frequencyData);
  jukeboxAnalyser.getByteTimeDomainData(timeDomainData);
  
  // Calculate RMS amplitude
  let sum = 0;
  for (let i = 0; i < timeDomainData.length; i++) {
    const normalized = (timeDomainData[i] - 128) / 128;
    sum += normalized * normalized;
  }
  const rms = Math.sqrt(sum / timeDomainData.length);
  const amp = Math.min(10, rms * 15); // Scale to 0-10
  
  // Beat detection (low frequency energy spike)
  const lowFreqSum = frequencyData.slice(0, 8).reduce((a, b) => a + b, 0) / 8;
  const now = performance.now();
  const kick = (lowFreqSum > 180 && now - lastKickTime > KICK_COOLDOWN_MS) ? 1 : 0;
  if (kick) lastKickTime = now;
  
  return { amp, kick, leftAmp: amp, rightAmp: amp, timestamp: Date.now() };
}

function jukeboxAudioLoop() {
  if (!jukeboxAudioElement.paused) {
    const audioData = extractAudioData();
    sendAudioToPreview(audioData);
    broadcastAudioToPJ(audioData);
  }
  jukeboxAnimationFrame = requestAnimationFrame(jukeboxAudioLoop);
}
```

### Phase 2: Jukebox UI Tab (kidlisp.com/index.html)

**4.2.1 HTML Structure**
```html
<!-- Add to list-subtabs -->
<button class="list-subtab" data-subtab="jukebox">🎵 Jukebox</button>

<!-- Jukebox content panel -->
<div id="jukebox-content" class="list-subcontent hidden">
  <div class="jukebox-player">
    <div class="jukebox-now-playing">
      <span class="jukebox-track-name">Select a track...</span>
    </div>
    <div class="jukebox-controls">
      <button id="jukebox-play" class="jukebox-btn">▶️</button>
      <button id="jukebox-pause" class="jukebox-btn">⏸️</button>
      <button id="jukebox-stop" class="jukebox-btn">⏹️</button>
    </div>
    <div class="jukebox-progress">
      <input type="range" id="jukebox-seek" min="0" max="100" value="0">
      <span class="jukebox-time">0:00 / 0:00</span>
    </div>
    <div class="jukebox-volume">
      <label>🔊</label>
      <input type="range" id="jukebox-volume" min="0" max="100" value="70">
    </div>
  </div>
  
  <div class="jukebox-tracklist" id="jukebox-tracklist">
    <!-- Populated by JS -->
  </div>
  
  <div class="jukebox-meters">
    <div class="jukebox-meter">
      <label>amp:</label>
      <div class="meter-bar"><div class="meter-fill" id="meter-amp"></div></div>
      <span class="meter-value" id="meter-amp-value">0.00</span>
    </div>
    <div class="jukebox-meter">
      <label>kick:</label>
      <div class="kick-indicator" id="kick-indicator">●</div>
    </div>
  </div>
</div>
```

**4.2.2 CSS Styling**
```css
.list-subtab[data-subtab="jukebox"] {
  background: linear-gradient(135deg, #1a1a2e, #16213e);
  color: #00d4ff;
}

.list-subtab[data-subtab="jukebox"].active {
  background: linear-gradient(135deg, #0f0f23, #1a1a3e);
  box-shadow: 0 0 10px rgba(0, 212, 255, 0.3);
}

.jukebox-player {
  padding: 12px;
  background: rgba(0, 0, 0, 0.3);
  border-radius: 8px;
  margin-bottom: 8px;
}

.jukebox-meters {
  display: flex;
  gap: 12px;
  padding: 8px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 4px;
}

.meter-bar {
  width: 100px;
  height: 8px;
  background: #333;
  border-radius: 4px;
  overflow: hidden;
}

.meter-fill {
  height: 100%;
  background: linear-gradient(90deg, #00ff00, #ffff00, #ff0000);
  transition: width 0.05s;
}

.kick-indicator {
  font-size: 16px;
  color: #333;
  transition: color 0.1s;
}

.kick-indicator.active {
  color: #ff00ff;
  text-shadow: 0 0 10px #ff00ff;
}
```

### Phase 3: Preview Iframe Integration

**4.3.1 Message Handler in kidlisp.mjs**
```javascript
// Add to existing message listener in kidlisp.mjs
window.addEventListener('message', (event) => {
  if (event.data?.type === 'kidlisp-audio') {
    const audioData = event.data.data;
    if (window.kidlispInstance) {
      window.kidlispInstance.updateAudioGlobals(audioData);
    }
  }
});
```

**4.3.2 Send Audio from Editor**
```javascript
function sendAudioToPreview(audioData) {
  if (previewIframe?.contentWindow) {
    previewIframe.contentWindow.postMessage({
      type: 'kidlisp-audio',
      data: audioData
    }, aestheticUrl);
  }
}
```

### Phase 4: PJ Broadcasting

**4.4.1 Broadcast Audio Data**
```javascript
function broadcastAudioToPJ(audioData) {
  if (!codeChannelId || !hasActivePjSession()) return;
  
  // BroadcastChannel (same-origin)
  if (window.BroadcastChannel) {
    const bc = new BroadcastChannel(`kidlisp-channel-${codeChannelId}`);
    bc.postMessage({
      type: 'audio-data',
      data: audioData,
      channelId: codeChannelId,
      timestamp: Date.now()
    });
    bc.close();
  }
  
  // WebSocket (cross-origin)
  if (pjWs && pjWs.readyState === WebSocket.OPEN) {
    pjWs.send(JSON.stringify({
      type: 'audio',
      content: {
        codeChannel: codeChannelId,
        ...audioData
      }
    }));
  }
}
```

**4.4.2 PJ Window Handler (pj.html)**
```javascript
// Add to sessionWs.onmessage
if (msg.type === 'audio' && msg.content) {
  sendAudioToIframe(msg.content);
}

function sendAudioToIframe(audioData) {
  if (!iframeReady) return;
  iframe.contentWindow.postMessage({
    type: 'kidlisp-audio',
    data: audioData
  }, aestheticUrl);
}

// Add to BroadcastChannel handler
if (event.data?.type === 'audio-data') {
  sendAudioToIframe(event.data.data);
}
```

---

## 5. Session Server Modifications (Optional Enhancement)

If audio data relay through session-server is needed:

**session-server/src/session.rs or equivalent:**
```javascript
// Handle audio message type
case 'audio':
  // Broadcast to all subscribers of this code channel
  broadcastToChannel(msg.content.codeChannel, {
    type: 'audio',
    content: msg.content
  });
  break;
```

---

## 6. Testing Strategy

### 6.1 Local Testing
1. Start `npm run aesthetic`
2. Open `localhost:8888/kidlisp.com`
3. Switch to Jukebox tab
4. Play a track, verify audio meters respond
5. Write KidLisp code using `amp` and `kick` variables
6. Verify visuals react to audio

### 6.2 PJ Testing
1. Open Jukebox and start playing
2. Click PJ button to open broadcast window
3. Verify `pj.kidlisp.com/{channel}` receives audio data
4. Confirm visuals in PJ window react to music

### 6.3 Remote Testing
1. Share PJ URL with another device
2. Verify audio data arrives via WebSocket
3. Confirm sync between editor and remote viewer

---

## 7. File Modifications Summary

| File | Changes |
|------|---------|
| `system/public/kidlisp.com/index.html` | Add Jukebox tab, audio engine, UI, broadcasting |
| `system/public/kidlisp.com/pj.html` | Handle `audio-data` messages, forward to iframe |
| `system/public/aesthetic.computer/lib/kidlisp.mjs` | Already supports `updateAudioGlobals()` ✅ |
| `session-server/` | (Optional) Add audio message relay |

---

## 8. Future Enhancements

- **Waveform visualization** in Jukebox panel
- **Frequency spectrum** display with band-level controls
- **Custom audio upload** (user's own tracks)
- **BPM detection** for tempo-synced effects
- **MIDI clock output** for external sync
- **Audio recording** from microphone into Jukebox

---

## 9. Dependencies

- Web Audio API (native browser)
- Existing postMessage infrastructure
- Existing BroadcastChannel / WebSocket PJ system
- No new npm packages required

---

## 10. Estimated Implementation Time

| Phase | Estimate |
|-------|----------|
| Phase 1: Web Audio Infrastructure | 2-3 hours |
| Phase 2: Jukebox UI Tab | 2-3 hours |
| Phase 3: Preview Integration | 1 hour |
| Phase 4: PJ Broadcasting | 1-2 hours |
| Testing & Polish | 2-3 hours |
| **Total** | **8-12 hours** |

---

*This analysis provides a roadmap for implementing the Jukebox feature while leveraging existing infrastructure in the aesthetic-computer codebase.*
