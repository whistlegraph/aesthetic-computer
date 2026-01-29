# SharedArrayBuffer & Embedding Context Report

**Date:** January 29, 2026  
**Status:** Investigation Complete

---

## 🔒 SharedArrayBuffer Status

### Current State: **NOT ENABLED**

SharedArrayBuffer requires **both** of these HTTP headers to be present:

| Header | Required Value | Current Value | Status |
|--------|----------------|---------------|--------|
| `Cross-Origin-Embedder-Policy` | `require-corp` | ❌ **COMMENTED OUT** | Missing |
| `Cross-Origin-Opener-Policy` | `same-origin` | `same-origin-allow-popups` | Partial |

### netlify.toml Configuration (lines 374-378)

```toml
# These headers are required in order for SharedArrayBuffer to be enabled.
# Currently used by ffmpeg.wasm. 22.08.06.11.01
# Cross-Origin-Embedder-Policy = "require-corp"
Cross-Origin-Opener-Policy = "same-origin-allow-popups"
Cross-Origin-Resource-Policy = "cross-origin"
```

### Why It Was Disabled

The comment indicates it was originally for **ffmpeg.wasm** support but was disabled. Likely reasons:

1. **COEP `require-corp` breaks cross-origin resources** - All external resources (images, scripts, fonts) must have `Cross-Origin-Resource-Policy: cross-origin` headers or use `crossorigin` attribute
2. **Breaks iframes from external origins** - NFT platforms (objkt, OpenSea, fxhash) embedding AC pieces would fail
3. **Breaks external CDN assets** - Any assets without proper CORP headers would be blocked

---

## 🖼️ Embedding Contexts Detected

### Current Embedding Detection Logic

From `boot.mjs`:

```javascript
let sandboxed = (window.origin === "null" && !window.acVSCODE) 
  || localStorageBlocked 
  || sessionStorageBlocked 
  || window.acPACK_MODE 
  || window.acSPIDER 
  || window.acNOAUTH;
```

### Supported Embedding Modes

| Mode | Detection | Description |
|------|-----------|-------------|
| **PACK_MODE** | `window.acPACK_MODE` | NFT bundle mode (objkt, fxhash, etc.) |
| **VSCODE** | `window.acVSCODE` | VS Code extension webview |
| **SPIDER** | `window.acSPIDER` | Spider/crawler mode |
| **NOAUTH** | `window.acNOAUTH` | No authentication mode (kidlisp.com editor) |
| **Sandboxed iframe** | `window.origin === "null"` | Generic sandboxed iframe detection |
| **localStorage blocked** | Try/catch test | Detects strict sandbox restrictions |

### iframe Detection in Pieces

Pieces can check `net.iframe` to determine if running embedded:

```javascript
// From prompt.mjs examples:
if (net.iframe) {
  // Running inside an iframe - disable certain features
}
```

### NFT Platform Compatibility

The system explicitly handles:

- **objkt.com** - Tezos NFT marketplace
- **OpenSea** - Ethereum NFT marketplace  
- **fxhash** - Generative art platform
- **teia.art** - Tezos NFT marketplace

These platforms use sandboxed iframes with restricted permissions, so:
- Auth0 authentication is disabled
- localStorage/sessionStorage may be blocked
- Sockets are disabled in OBJKT mode
- Analytics are skipped

---

## 🎧 Audio Context & Latency

### Current Audio Architecture

```
User Input → bios.mjs triggerSound() → postMessage → speaker.mjs AudioWorklet → Audio Output
```

### Latency Components

| Component | Typical Latency | Notes |
|-----------|-----------------|-------|
| JS Event Handler | ~0.5-2ms | Key press to triggerSound() call |
| postMessage | ~0.1-1ms | Main thread to AudioWorklet |
| AudioWorklet buffer | ~2.7-5.3ms | 128-256 samples at 48kHz |
| OS Audio Buffer | ~5-20ms | Platform dependent |
| **Total** | **~8-30ms** | Varies by device |

### Current AudioContext Settings

From `bios.mjs`:
- `latencyHint: "interactive"` (default for low latency)
- Sample rate: 48kHz (global default)
- Buffer size: 128 samples per render quantum

### If SharedArrayBuffer Were Enabled

**Potential benefits:**
1. **Zero-copy audio transfer** - Waveform/sample data could be shared directly between main thread and AudioWorklet
2. **Atomics for sync** - More precise timing synchronization possible
3. **Reduced GC pressure** - No buffer copying means less garbage collection

**Estimated latency improvement:** ~1-3ms for waveform visualization, minimal impact on actual audio playback (already uses AudioWorklet)

---

## ⚠️ Trade-offs for Enabling SharedArrayBuffer

### If We Uncomment COEP Header

**Would Break:**
1. ❌ All cross-origin images without CORP headers
2. ❌ External fonts from CDNs  
3. ❌ YouTube embeds in chat
4. ❌ Potentially NFT platform embedding (needs testing)
5. ❌ Any external scripts without crossorigin attribute

**Would Enable:**
1. ✅ SharedArrayBuffer for audio/video processing
2. ✅ ffmpeg.wasm for video encoding
3. ✅ WASM threads (multi-threaded WASM)
4. ✅ Potentially faster waveform visualization

### Recommendation

**Don't enable globally.** Instead, consider:

1. **Piece-specific isolation** - Load specific pieces that need SAB in a cross-origin isolated context
2. **Separate subdomain** - `isolated.aesthetic.computer` with full COEP/COOP for ffmpeg features
3. **Feature detection** - Check `crossOriginIsolated` and gracefully degrade

---

## 📊 Current Client Resources

### Audio System

| Resource | Type | Purpose |
|----------|------|---------|
| `speaker.mjs` | AudioWorklet | Sound synthesis (synth, samples, bubbles) |
| `bios.mjs` | Main thread | Audio context management, triggerSound() |
| `Synth` class | In worklet | Oscillator-based synthesis |
| `Reverb` class | In worklet | Multi-tap reverb effect |
| `Glitch` effect | In worklet | Bitcrush + sample-hold |

### WebSocket Connections

| Connection | Purpose | Disabled When |
|------------|---------|---------------|
| Session Server | Real-time multiplayer | OBJKT mode, sandboxed |
| Module Loader | Fast module delivery | PACK mode |

### Storage

| Storage | Purpose | Fallback |
|---------|---------|----------|
| localStorage | Auth cache, preferences | In-memory (sandboxed) |
| sessionStorage | Temporary state | In-memory (sandboxed) |
| IndexedDB | Large asset cache | Not used in sandbox |

---

## 🔧 Quick Reference: Checking Isolation Status

In browser console:
```javascript
// Check if SharedArrayBuffer is available
console.log('SharedArrayBuffer:', typeof SharedArrayBuffer !== 'undefined');

// Check if cross-origin isolated
console.log('crossOriginIsolated:', window.crossOriginIsolated);

// Check embedding context
console.log('In iframe:', window !== window.top);
console.log('Origin:', window.origin);
```

---

## Summary

| Feature | Status | Action Needed |
|---------|--------|---------------|
| SharedArrayBuffer | ❌ Disabled | Intentionally off for compatibility |
| NFT Embedding | ✅ Working | PACK_MODE handles sandbox |
| VS Code Extension | ✅ Working | acVSCODE flag |
| kidlisp.com embed | ✅ Working | NOAUTH mode |
| Audio Latency | ~8-30ms | Already optimized with AudioWorklet |
| Cross-origin resources | ✅ Working | Would break with COEP |
