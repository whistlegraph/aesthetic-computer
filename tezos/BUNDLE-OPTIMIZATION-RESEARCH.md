# Bundle Size Optimization Research

## Current Status

- **Current Size**: 392 KB (compressed with gzip level 9 + base64)
- **Target Size**: 256 KB (Tezos storage limit)
- **Overage**: 142 KB (36% reduction needed)
- **Test Piece**: `$wwi` - simple KidLisp with `(wipe fade:red-rainbow)`

## Compression Analysis

### Compression Ratio
- Uncompressed: 1007 KB
- Compressed: 392 KB
- **Ratio**: 61.2% reduction
- **Remaining**: 38.8% of original

This is already strong compression, so we need to remove actual code rather than just compress better.

## Size Bottleneck Research

Based on previous optimization work (OPTIMIZATION-RESULTS.md), the bundle includes:

### Files Known to be Removable for Simple KidLisp

1. **3D Graphics Stack** (~80-100 KB uncompressed)
   - `lib/3d.mjs` (54.2 KB → 22.2 KB minified)
   - `dep/three/*` (entire Three.js library)
   - **Impact**: High - not needed for 2D KidLisp pieces

2. **World System** (~24 KB uncompressed)
   - `systems/world.mjs` (23.5 KB → 10.9 KB minified)
   - **Impact**: Medium - physics/world not used in simple graphics

3. **Prompt System** (~7.4 KB uncompressed)
   - `systems/prompt-system.mjs` (7.4 KB → 3.2 KB minified)
   - **Impact**: Low - already running kidlisp code, no prompt needed

4. **Advanced Input Systems** (~50-70 KB estimated)
   - `lib/hand.mjs` + dependencies (hand tracking)
   - `lib/webgpu.mjs` (WebGPU renderer)
   - `lib/chat.mjs` (chat system)
   - `lib/socket.mjs` (websocket)
   - `lib/udp.mjs` + `dep/geckos.io/*` (UDP networking)
   - `lib/microphone.mjs` (audio input)
   - `lib/cam-doll.mjs` (camera)
   - **Impact**: High - none used in simple visual pieces

5. **Unused Common Pieces** (~10-20 KB estimated)
   - `disks/common/products.mjs`
   - `disks/common/tape-player.mjs`
   - Various other common utilities
   - **Impact**: Medium - not referenced by simple KidLisp

### Previous Success: 78.6% Size Reduction

From OPTIMIZATION-RESULTS.md ($pie piece):
- **Before**: 4.48 MB (502 files)
- **After**: 0.96 MB (27 files)
- **Reduction**: 78.6%

Key removals:
- 232 unused disk pieces
- 5 font variants (kept only 1)
- Various lib/ modules

### Current Bundle Analysis

Current bundle ($wwi piece):
- **Files**: 87 files (vs 27 in optimized $pie)
- **Delta**: 60 extra files = candidates for removal

## Optimization Strategies

### Strategy 1: Aggressive Dependency Pruning
Create ultra-minimal version of `bundle-minimal-keep.mjs`:

**Remove from ESSENTIAL_FILES**:
- All 3D-related: `lib/3d.mjs`
- Advanced input: `lib/hand.mjs`, `lib/webgpu.mjs`, `lib/chat.mjs`, `lib/socket.mjs`, `lib/udp.mjs`, `lib/microphone.mjs`, `lib/cam-doll.mjs`
- World system: only include if bios.mjs doesn't auto-load it
- Prompt system: only include if needed for boot

**Expected savings**: ~150-200 KB uncompressed → ~60-80 KB compressed

### Strategy 2: Lazy Dependency Resolution
Modify bundler to:
1. Start with absolute minimum (bios, boot, kidlisp, graph, 2d, color, wipe)
2. Analyze which imports are actually used
3. Only include referenced modules

**Expected savings**: Variable, but could hit 27-file minimum like $pie

### Strategy 3: Code Analysis
For each lib file, check if it's actually imported:
- Run static analysis on `import` statements
- Build dependency graph
- Remove any unreferenced modules

**Expected savings**: ~30-50 KB compressed

### Strategy 4: Inline Small Dependencies
For tiny utilities (<2 KB), inline them instead of separate files:
- Reduces VFS overhead (path strings, JSON structure)
- Merges minification opportunities

**Expected savings**: ~5-10 KB compressed

### Strategy 5: Further Font Reduction
Current fonts (5 variants per font):
- `.ttf`, `.woff`, `.woff2`, `.svg`, `.eot`

Modern browsers only need:
- `.woff2` (best compression, universal support)
- `.woff` (fallback)

**Remove**: `.ttf`, `.svg`, `.eot`
**Expected savings**: ~10-20 KB compressed

### Strategy 6: Minify HTML/CSS
Current bundle template might have unminified HTML/CSS:
- Remove whitespace from template
- Inline and minify CSS
- Compress base64 encoding

**Expected savings**: ~2-5 KB compressed

## Recommended Approach

### Phase 1: Quick Wins (Target: 320 KB → ~80 KB saved)
1. ✅ Remove obvious unused systems:
   - 3D stack (lib/3d.mjs, dep/three/*)
   - Advanced input (hand, webgpu, chat, socket, udp, microphone, cam-doll)
   - World system (if not auto-loaded)
   - Prompt system (for embedded pieces)

2. ✅ Reduce fonts to `.woff2` + `.woff` only

### Phase 2: Dependency Analysis (Target: 280 KB → ~40 KB saved)
1. Build import graph of remaining files
2. Identify unreferenced modules
3. Remove dead code

### Phase 3: Structure Optimization (Target: 260 KB → ~20 KB saved)
1. Inline small utilities
2. Minify HTML template
3. Optimize base64 encoding

### Phase 4: Nuclear Option (If still over 256 KB)
1. Remove even more libs (gamepad, midi, usb if unused)
2. Strip sound system if piece is purely visual
3. Consider making ultra-minimal "KEEP Edition" of bios.mjs

## Implementation Plan

1. **Create `bundle-ultra-minimal-keep.mjs`**
   - Fork from `bundle-minimal-keep.mjs`
   - Implement Phase 1 removals
   - Test on $wwi piece

2. **Verify functionality**
   - Generate bundle
   - Test in browser
   - Ensure wipe/color/rainbow fade still works

3. **Measure and iterate**
   - Check compressed size
   - If still over 256 KB, apply Phase 2
   - Repeat until under limit

4. **Document final minimal set**
   - Update ESSENTIAL_FILES list
   - Create comparison table
   - Archive as template for future KEEPS

## Success Criteria

- ✅ Compressed bundle ≤ 256 KB
- ✅ `$wwi` piece runs correctly
- ✅ Rainbow fade effect displays
- ✅ No console errors (except expected external resource blocks)
- ✅ Boot time < 2s
- ✅ KidLisp loads in < 5ms

## Risk Assessment

**Low Risk**:
- Removing 3D stack (clearly unused)
- Removing networking (chat, socket, udp)
- Removing advanced input (hand, webgpu, microphone, cam)
- Reducing font variants

**Medium Risk**:
- Removing world system (might be auto-loaded by bios)
- Removing prompt system (might be needed for boot)
- Removing some lib/ utilities (might have hidden dependencies)

**High Risk**:
- Modifying bios.mjs or boot.mjs
- Removing core graphics libs (2d, graph, geo)
- Changing VFS structure

## Next Steps

1. Create ultra-minimal bundler with Phase 1 removals
2. Test bundle generation and verify size
3. If successful, document as new minimal template
4. If still over, analyze dependency graph and apply Phase 2
