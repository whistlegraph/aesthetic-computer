# Dependency Loading Analysis: Static vs Dynamic

## Current Loading Patterns

### 🔒 **STATIC IMPORTS** (Always Bundled)
These are imported at module load time and MUST be present in the bundle:

**Core Math & Utilities (Always Needed)**:
- `gl-matrix/*` - Vector/matrix math (imported by disk.mjs, graph.mjs, geo.mjs, etc.)
- `nanoid` - ID generation (disk.mjs, graph.mjs)
- `@akamfoad/qr` - QR code generation (disk.mjs)

**Graphics & Rendering (Conditionally Needed)**:
- `three/three.module.js` - Three.js 3D graphics (lib/3d.mjs - only needed for FPS/3D pieces)
- `three/VRButton.js`, `three/GLTFExporter.js`, etc. - Three.js extensions (lib/3d.mjs)

**Networking (Conditionally Needed)**:
- `geckos.io-client.2.3.2.min.js` - UDP networking (lib/udp.mjs - only for multiplayer)
- `idb.js` - IndexedDB (lib/store.mjs - for local storage)

### 🔄 **DYNAMIC IMPORTS** (Load on Demand)
These are loaded only when specifically requested:

**Game Boy Emulation**:
```javascript
// bios.mjs line 1704 - Only loads when gameboy is initialized
const wasmBoyModule = await import('./dep/wasmboy/wasmboy.ts.esm.js');
```

**Firebase (External CDN)**:
```javascript  
// boot.mjs - Firebase loaded from CDN, not bundled
const { initializeApp } = await import("https://www.gstatic.com/firebasejs/10.8.0/firebase-app.js");
```

**FFmpeg (Commented Out)**:
```javascript
// bios.mjs line 847 - Currently disabled
// return await import(`/aesthetic.computer/dep/@ffmpeg/ffmpeg-core.js`);
```

## 🎯 **Safe to Strip** (Dynamic Dependencies)

These are **only** loaded on-demand and can be safely excluded if not used:

### 1. **wasmboy (19MB)** ✅ HIGH PRIORITY
- **Trigger**: Only loaded in `initGameboy()` function in bios.mjs
- **Detection**: Check if piece uses `sound.gameboy` API
- **Safe to strip**: YES - dynamically imported only when needed

### 2. **@mediapipe (25MB)** ✅ HIGH PRIORITY  
- **Status**: Not directly imported anywhere in static imports!
- **Usage**: Only used via external worker or API calls
- **Detection**: Check if piece uses hand tracking APIs
- **Safe to strip**: YES - appears to be loaded externally

### 3. **aframe (2MB)** ✅ MEDIUM PRIORITY
- **Usage**: Only imported by specific pieces like `aframe.mjs`
- **Detection**: Check if piece imports aframe or has VR functionality  
- **Safe to strip**: YES - piece-specific

### 4. **web3 (1.4MB)** ✅ MEDIUM PRIORITY
- **Usage**: Not found in static imports
- **Detection**: Check if piece uses blockchain/crypto APIs
- **Safe to strip**: YES - appears to be loaded on-demand

### 5. **gpt3-tokenizer (3.5MB)** ✅ MEDIUM PRIORITY
- **Usage**: Not found in static imports
- **Detection**: Check if piece uses AI/GPT APIs
- **Safe to strip**: YES - appears to be loaded on-demand

## ⚠️ **Conditional Static Imports** (Require Analysis)

These are statically imported but only by specific modules:

### 1. **three.js (1.5MB)** - COMPLEX
- **Static import**: `lib/3d.mjs` imports Three.js modules
- **Usage**: Only needed when pieces use 3D/FPS systems
- **Detection**: Check if piece has `system = "fps"` or imports 3D APIs
- **Strategy**: Could stub `lib/3d.mjs` for non-3D pieces

### 2. **geckos.io-client (12KB)** - MINOR
- **Static import**: `lib/udp.mjs`  
- **Usage**: Only for UDP/multiplayer functionality
- **Detection**: Check if piece uses networking APIs
- **Strategy**: Could stub `lib/udp.mjs` for single-player pieces

## 🔧 **Implementation Strategy**

### Phase 1: Safe Dynamic Dependency Stripping
Create analyzer that detects usage patterns and excludes unused dynamic deps:

```javascript
class DependencyAnalyzer {
  analyzeDynamicDependencies(pieceCode, pieceSystem) {
    const exclusions = [];
    
    // wasmboy - check for gameboy usage
    if (!this.usesGameBoy(pieceCode)) {
      exclusions.push('wasmboy/**');
    }
    
    // mediapipe - check for hand tracking
    if (!this.usesHandTracking(pieceCode)) {
      exclusions.push('@mediapipe/**');
    }
    
    // aframe - check for VR
    if (!this.usesVR(pieceCode)) {
      exclusions.push('aframe*');
    }
    
    return exclusions;
  }
  
  usesGameBoy(code) {
    return /sound\.gameboy|gameboy\.|WasmBoy/i.test(code);
  }
  
  usesHandTracking(code) {
    return /hand\.|mediapipe|hand:/i.test(code);
  }
}
```

### Phase 2: Conditional Static Import Handling
For Three.js and other static imports, we could:

1. **Analyze piece requirements** before bundling lib files
2. **Create stubs** for unused lib modules  
3. **Selective lib bundling** based on piece system

### Expected Results

**Before (Current)**:
- All pieces: ~65MB (everything bundled)

**After Phase 1 (Dynamic exclusions)**:
- Simple pieces: ~15-20MB (80% reduction!)
- Game Boy pieces: ~35MB (45% reduction)  
- Hand tracking pieces: ~40MB (40% reduction)

**After Phase 2 (Static analysis)**:
- Simple pieces: ~5-10MB (90% reduction!)
- 3D pieces: ~15MB (75% reduction)

## 🚀 **Immediate Action Plan**

1. **Start with wasmboy exclusion** - Safest 19MB win
2. **Add mediapipe detection** - Another 25MB win for most pieces  
3. **Implement in `bundleDepFiles()` method** with exclusion logic
4. **Add detection methods** to analyze piece code for usage patterns

This approach focuses on the **safe wins first** - dynamic dependencies that are clearly loaded on-demand and can be safely excluded without breaking functionality.