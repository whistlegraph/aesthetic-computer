# TEIA Dependencies Analysis & Optimization Report

## Executive Summary

The current TEIA packaging system bundles **ALL** dependencies (~64MB) regardless of piece requirements. Major optimization opportunities exist to reduce package sizes by 80-90% through selective bundling and tree-shaking.

## Current Dependency Footprint

### Large Dependencies (63MB Total)
```
25MB    @mediapipe      - Hand tracking, computer vision
19MB    wasmboy         - Game Boy emulator  
8.4MB   webpxmux        - WebP muxing
3.5MB   gpt3-tokenizer  - AI tokenization
2.0MB   aframe          - WebVR framework
1.5MB   three           - 3D graphics (Three.js)
1.4MB   web3            - Blockchain integration
204KB   gl-matrix       - Math utilities
160KB   auth0           - Authentication
132KB   gifenc          - GIF encoding
124KB   gif             - GIF utilities
96KB    jszip           - ZIP utilities
92KB    ffmpeg          - Video processing
```

### Usage Analysis by System

**3D/FPS System (Three.js)**: 91 pieces use 3d/fps systems
**Hand Tracking (MediaPipe)**: 58 pieces use hand/mediapipe
**World/Multiplayer**: 15 pieces use world system
**Game Boy Emulation**: Recently integrated, limited usage

## Critical Findings

### 1. Universal Bundling Problem
- All dependencies bundled regardless of piece requirements
- Simple pieces like `$bop` carry 63MB+ of unused libraries
- No conditional loading based on piece analysis

### 2. Heavy Dependencies with Limited Usage
- **wasmboy (19MB)**: Game Boy emulator for specialized pieces only
- **@mediapipe (25MB)**: Hand tracking for camera-based pieces only  
- **aframe (2MB)**: WebVR for VR-specific pieces only
- **gpt3-tokenizer (3.5MB)**: AI pieces only
- **web3 (1.4MB)**: Blockchain pieces only

### 3. Core vs Optional Systems
**Always Needed (Core)**:
- gif/gifenc: GIF generation
- jszip: Package creation
- gl-matrix: Math utilities
- auth0: Authentication (could be stubbed in TEIA)
- ffmpeg: Video processing (could be conditional)

**Conditionally Needed**:
- three.js: Only for 3D/FPS pieces
- mediapipe: Only for hand-tracking pieces
- wasmboy: Only for Game Boy pieces
- aframe: Only for WebVR pieces
- web3: Only for blockchain pieces

## Proposed Optimization Strategy

### Phase 1: Dependency Detection & Analysis

1. **Static Analysis**: Scan piece code for system imports:
   ```javascript
   // Detect in piece files:
   export const system = "fps"          // → three.js needed
   export const system = "world"        // → networking needed  
   import { hand } from "..."           // → mediapipe needed
   // etc.
   ```

2. **API Usage Analysis**: Scan for specific API calls:
   ```javascript  
   // In piece code:
   sound.gameboy                        // → wasmboy needed
   api.hand.track()                     // → mediapipe needed
   api.Form(), api.CUBEL                // → three.js needed
   ```

### Phase 2: Conditional Bundling System

Modify `ac-pack.mjs` to implement selective dependency inclusion:

```javascript
async bundleDepFiles() {
  const requiredDeps = await this.analyzePieceDependencies();
  
  // Only bundle required dependencies
  for (const dep of requiredDeps) {
    await this.bundleSpecificDep(dep);
  }
}

async analyzePieceDependencies() {
  const deps = new Set(['core']); // Always include core deps
  
  // Static analysis
  if (this.pieceUsesSystem('fps') || this.pieceUsesSystem('3d')) {
    deps.add('three');
  }
  
  if (this.pieceUsesHandTracking()) {
    deps.add('mediapipe');
  }
  
  if (this.pieceUsesGameBoy()) {
    deps.add('wasmboy');
  }
  
  return deps;
}
```

### Phase 3: Stub Generation

Create intelligent stubs for unused dependencies:

```javascript
// Auto-generated stub for unused three.js
export const THREE = {
  WebGLRenderer: class { constructor() { console.warn('Three.js not loaded'); } },
  Scene: class { constructor() { console.warn('Three.js not loaded'); } },
  // etc.
};
```

### Phase 4: Progressive Loading

For pieces that might conditionally need dependencies:

```javascript
// Lazy loading pattern
async loadThreeJS() {
  if (!window.THREE) {
    const module = await import('./dep/three/three.module.js');
    window.THREE = module;
  }
  return window.THREE;
}
```

## Implementation Priority

### High Priority (Immediate 80% reduction)
1. **wasmboy exclusion**: Skip unless piece explicitly uses Game Boy
2. **mediapipe exclusion**: Skip unless piece uses hand tracking
3. **aframe exclusion**: Skip unless piece uses WebVR
4. **web3 exclusion**: Skip unless piece uses blockchain

### Medium Priority (Additional 15% reduction)
1. **three.js conditional**: Only for 3D/FPS system pieces
2. **gpt3-tokenizer conditional**: Only for AI pieces
3. **webpxmux conditional**: Only for WebP pieces

### Low Priority (Final 5% optimization)
1. **auth0 stubbing**: Create stub for TEIA mode
2. **ffmpeg conditional**: Only for video processing pieces
3. **Fine-grained three.js**: Import only needed Three.js modules

## Expected Results

### Before Optimization
- Simple pieces: ~65MB packages
- Complex 3D pieces: ~65MB packages  
- All pieces carry full dependency tree

### After Optimization  
- Simple pieces: ~5-10MB packages (85% reduction)
- 3D pieces: ~15-20MB packages (70% reduction)
- Hand tracking pieces: ~30MB packages (55% reduction)
- Game Boy pieces: ~25MB packages (60% reduction)

## Technical Implementation Plan

### 1. Add Dependency Analysis Module
Create `teia/dependency-analyzer.mjs`:
```javascript
export class DependencyAnalyzer {
  analyzePiece(sourceCode, system) {
    // Static analysis logic
    // Return required dependency list
  }
}
```

### 2. Modify ac-pack.mjs bundleDepFiles()
Replace current universal bundling with selective bundling

### 3. Create Stub Generator
Generate appropriate stubs for excluded dependencies

### 4. Add Configuration Options
```bash
node ac-pack.mjs piece --exclude-deps wasmboy,mediapipe
node ac-pack.mjs piece --minimal  # Only core deps
node ac-pack.mjs piece --analyze   # Show what would be included
```

## Risk Assessment

### Low Risk
- Core dependency exclusion (wasmboy, mediapipe for non-using pieces)
- Stub generation for completely unused APIs

### Medium Risk  
- Three.js conditional loading (many pieces might have implicit 3D usage)
- Progressive loading implementation

### High Risk
- Core system stubbing (auth0, ffmpeg)
- Complex interdependency resolution

## Recommendations

1. **Start with wasmboy exclusion** - Safest 19MB reduction
2. **Add mediapipe analysis** - Potential 25MB reduction for non-camera pieces
3. **Implement dependency detection CLI** - For analysis and debugging
4. **Create test matrix** - Verify piece functionality with reduced deps
5. **Add bundle size reporting** - Track optimization progress

This optimization could reduce TEIA package sizes from ~65MB to ~5-15MB for most pieces, dramatically improving distribution and loading times.