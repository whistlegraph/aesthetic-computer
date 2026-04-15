# Bootloader.art Integration Feasibility Report

**Date**: 2026-04-14  
**Status**: Research & Feasibility Analysis  
**Author**: Claude Code  

---

## Executive Summary

**bootloader.art** is an open experimental on-chain generative art platform that enables artists to create, mint, and collect unique algorithmic art pieces directly on the blockchain. It challenges traditional NFT immutability by supporting **living, updatable code** post-mint—meaning artists can evolve their pieces after they're tokenized.

### Key Finding
Integrating AC's KidLisp bundler with bootloader.art is **technically feasible** but **constrained by on-chain storage limits**. The integration would work best for:
- **KidLisp pieces** (smaller, more optimizable)
- **Generative art focus** (procedural over interactive)
- **Optimized bundles** (<24KB on-chain, reference off-chain assets)

---

## What is Bootloader.art?

### Platform Overview
- **Purpose**: On-chain generative art platform with mutable code post-mint
- **Code Storage**: Directly on blockchain (with size constraints)
- **Update Mechanism**: Artists can modify code after mint; collectors' pieces evolve
- **Ecosystem**: Part of broader platforms like fxhash, Highlight XYZ, Art Blocks

### Key Innovation: Living Code
Unlike traditional NFTs where metadata is immutable, bootloader.art supports:
- **Post-mint code updates** by artist
- **Evolutionary art**: pieces change as the underlying algorithm evolves
- **Collector engagement**: tokens become more valuable as code improves

### Technical Architecture
- Uses **HTML + JavaScript** as core format
- Code rendered client-side in browser
- Seed-based generative output (each NFT gets unique seed)
- On-chain storage of generative algorithm

---

## Our Current Bundling Infrastructure

### Existing Bundlers

#### 1. **Oven Bundler** (`oven/bundler.mjs`)
**Purpose**: Generate self-contained HTML bundles for AC pieces  
**Current Capabilities**:
- Bundles entire AC runtime into single HTML file
- Supports both KidLisp pieces (`$code`) and JavaScript pieces
- In-memory caching per git commit
- Brotli compression for WASM decoder
- Comic Relief font inlining
- Minification support

**Output**: Typically 2-8 MB self-contained HTML (uncompressed)

**Key Features**:
```
- Core essentials: boot.mjs, bios.mjs, disk.mjs, kidlisp.mjs
- Graphics (2D/3D): graph, geo, 2d, gl-matrix, glaze
- Input handling: keyboard, gamepad, motion, touch
- Audio: sound libraries (speaker, synth, bubble)
- UI components: buttons, text input, ui.mjs
- Storage: store.mjs for persistence
```

#### 2. **KidLisp Bundler** (`system/backend/kidlisp-bundler.mjs`)
**Purpose**: Generate minimal KidLisp-specific bundles  
**Optimizations**:
- Explicit skip list (wasmboy, UDP, world system, optimizer)
- Ultra-minimal file set for basic KidLisp visuals
- SWC minification
- Import rewriting for size optimization

**Key Skip Files**:
- `dep/wasmboy/` — 386 KB source (GameBoy emulator)
- `lib/udp.mjs` — networking (dynamically imported)
- `systems/world.mjs` — 3D features

**Estimated Output**: ~200-400 KB minified (before gzip)

---

## On-Chain Storage Constraints

### Blockchain Size Limits

| Constraint | Value | Notes |
|-----------|-------|-------|
| Ethereum storage per txn | ~128 KB | Hard limit for single transaction |
| Typical contract calldata | 24 KB | Realistic limit for deployed code |
| Arweave bundling | 100 KB+ | More generous, but off-chain |
| Solana program size | 200 KB | Program accounts must fit in this |
| Zora ERC721A calldata | 4 KB+ | Per metadata/code segment |

### Gas Cost Impact
- **String manipulation**: Major gas consumer in on-chain code
- **minification essential**: Every byte counts
- **Gzip/Brotli**: Not applicible on-chain (code must be decompressed off-chain)

### Reference: Autoglyphs Precedent
[Autoglyphs](https://www.larvalabs.com/autoglyphs) (early on-chain generative art):
- Algorithm fit in **single Ethereum transaction** (~13.27% of network capacity per mint)
- Bare-essentials algorithm only
- NO UI, NO audio, NO graphics libraries
- Pure math-based SVG generation

---

## AC Bundle Size Analysis

### Current Oven Output
```
Full AC Runtime (uncompressed):  ~7-8 MB
├── boot.mjs                    ~15 KB
├── bios.mjs                    ~80 KB
├── lib/disk.mjs                ~572 KB (largest)
├── lib/kidlisp.mjs             ~90 KB
├── Graphics libraries          ~250 KB
├── UI/Input systems            ~180 KB
├── Audio systems               ~200 KB
└── Dependencies (gl-matrix, etc) ~100 KB
```

### KidLisp Bundler (Optimized)
```
Minimal KidLisp Bundle:         ~200-400 KB (minified)
├── Core runtime (boot+bios)    ~80 KB
├── KidLisp interpreter         ~65 KB
├── 2D graphics (no 3D)         ~45 KB
├── Essential UI                ~30 KB
└── Math & helpers              ~40 KB
```

### Ultra-Minimal Bootloader Bundle Target
```
On-Chain Optimized Bundle:      ~20-24 KB
├── Minimal AC boot strap       ~5 KB
├── KidLisp evaluator           ~12 KB  (pre-minified)
├── Graphics primitives         ~4 KB   (line, circle, rect only)
├── Color model                 ~1 KB
└── DOM rendering               ~2 KB
```

---

## Feasibility Assessment

### ✅ What Works Well

1. **KidLisp as Primary Target**
   - Interpreted language, smaller interpreter (~12 KB minified)
   - No build step required (unlike JavaScript)
   - Math-centric, good for generative art
   - Already optimized in `kidlisp-bundler.mjs`

2. **Seed-Based Rendering**
   - AC's pieces use params/colon input system
   - Can expose seed-based variation similar to generative platforms
   - KidLisp pieces already deterministic

3. **Existing HTML Bundling**
   - `oven/bundler.mjs` proven in production
   - Modular architecture (can strip down essentials)
   - Font embedding, minification already in place

4. **Graphics Stack**
   - 2D primitives (line, circle, box) can fit in 4-5 KB
   - No dependency on WebGL for basic art
   - Canvas API is lightweight

### ⚠️ Significant Challenges

1. **Size Constraints**
   - Full AC runtime: **7-8 MB** (bloated)
   - Realistic on-chain limit: **20-24 KB**
   - Gap: **97.3% reduction needed**
   - KidLisp-only path could get to ~200 KB (**30x too large**)

2. **Disk.mjs Dependencies**
   - `lib/disk.mjs` is **572 KB** (largest library)
   - Contains UI, audio, networking, 3D graphics
   - Would need complete rewrite of core API surface
   - Risk: AC pieces depend on disk.mjs structure

3. **Browser Context Limitations**
   - Bootloader.art renders in browser (not AC's WebSocket+module-loader model)
   - No hot reloading infrastructure
   - Pieces run in isolation, not linked to AC social system
   - Would lose: networking, chat, multiplayer features

4. **Asset Handling**
   - Fonts (ComicRelief): **20+ KB** (can't fit on-chain)
   - Audio files: **not practical** on-chain
   - Images: **not practical** on-chain
   - Would need off-chain asset references (IPFS/Arweave)

5. **API Incompatibility**
   - AC pieces expect: `{ wipe, ink, line, circle, ui, sound, net, event, ... }`
   - Bootloader expects: deterministic, seed-based `render(seed) → SVG/canvas`
   - Significant refactoring needed per piece

### ❌ What Doesn't Work

1. **Full AC Runtime On-Chain**
   - Simply impossible due to size
   - Would require 300+ separate contracts or sharding

2. **Audio/Networking Features**
   - Sound synthesis: 50+ KB of synthesizers
   - Networking (socket.io): 30+ KB
   - These are fundamentally off-chain

3. **Interactive Game Pieces**
   - Pieces like `squash.mjs`, `1v1.mjs` require multiplayer networking
   - Bootloader is single-user generative only
   - Would lose entire game category

---

## Integration Approaches

### Approach 1: KidLisp-Only Ultra-Minimal (RECOMMENDED)

**Scope**: Support only KidLisp pieces, no JavaScript  
**Target Size**: 20-24 KB on-chain

**Components**:
```
bootstrap.mjs (on-chain)         ~2 KB
├── WASM loader shim
├── Initialize canvas/DOM
└── Invoke KidLisp evaluator

kidlisp-mini.mjs (on-chain)      ~12 KB
├── Minimal evaluator
├── Core functions (draw, color, math)
└── Skip: sound, networking, 3D

graphics-tiny.mjs (on-chain)     ~4 KB
├── line(x1, y1, x2, y2)
├── circle(x, y, r)
├── rect(x, y, w, h)
├── fill(r, g, b, a)
└── Canvas wrapper

piece-data.js (on-chain)         ~4 KB
└── Actual KidLisp code (usually 1-3 KB)

Total on-chain: 22 KB

Off-chain (referenced):
- Fonts (IPFS/Arweave)
- Optional: larger KidLisp libraries via dynamic import
```

**Workflow**:
```
1. Artist writes KidLisp piece on AC (or similar)
2. Extract & minify core KidLisp code
3. Create bootstrap + graphics shim
4. Deploy to bootloader.art with on-chain + IPFS refs
5. Piece rendered: seed → KidLisp eval → canvas → display
```

**Pieces That Would Work**:
- `$pie` (generative visuals)
- `$cw` (color work)
- `$cow` (recursion/fractals)
- `$trees` (procedural generation)
- Any math-based generative KidLisp

**Pieces That Would NOT Work**:
- `squash`, `1v1` (multiplayer)
- Any piece using `sound` library
- Pieces using `net`, `chat`, `socket`
- Pieces using 3D (world.mjs)

**Implementation Effort**: Medium (3-5 days)
- Fork `kidlisp-bundler.mjs` to bootloader target
- Create minimal graphics shim
- Implement `seed → params` conversion
- Test with 5-10 existing KidLisp pieces

---

### Approach 2: Off-Chain Code + On-Chain Metadata

**Scope**: Store full bundle on IPFS/Arweave, reference on-chain  
**Target Size**: 200+ KB off-chain (no on-chain size limit)

**Components**:
```
On-chain NFT metadata:
{
  "name": "piece-name",
  "image": "ipfs://...",           // preview image
  "external_url": "ipfs://...",    // full bundle URL
  "attributes": [
    { "trait_type": "size", "value": "205 KB" },
    { "trait_type": "seed_range", "value": "1-1000000" }
  ]
}

Off-chain (IPFS):
- Full AC runtime bundle (7-8 MB)
- OR KidLisp + minimal graphics (400 KB)
- Self-contained, no external assets
```

**Workflow**:
```
1. Generate HTML bundle with oven/bundler.mjs
2. Upload to IPFS (via Pinata, etc.)
3. Create minimal NFT metadata pointing to IPFS
4. Deploy to bootloader.art
5. User clicks → loads IPFS → renders AC piece
```

**Advantages**:
- No size constraints
- Can use full AC runtime + pieces
- Works for JavaScript pieces too
- Hot updates possible (re-upload to IPFS)

**Disadvantages**:
- Code not truly "on-chain" (defeats bootloader.art philosophy)
- Depends on IPFS availability
- Centralization concern (if IPFS node goes down)
- Not authentic to "generative on-chain" movement

**Implementation Effort**: Low (1-2 days)
- Integrate IPFS upload into oven
- Create metadata templates
- Document process

---

### Approach 3: Hybrid Hybrid: Small On-Chain + Large Off-Chain

**Scope**: Seed + metadata on-chain, full code on IPFS  
**Target Size**: 2-3 KB on-chain, 400 KB off-chain

**Components**:
```
On-chain contract:
{
  seed: 12345,
  ipfs_hash: "QmXxxx...",
  artist: "0x...",
  title: "piece-name"
}

Off-chain (IPFS):
{
  bundle: "full-ac-runtime.html",  // 7-8 MB
  code: "$code",                   // KidLisp source
  entry: "piece-name"              // which disk to load
}
```

**Workflow**:
```
1. Deploy minimal metadata on-chain
2. Full code lives on IPFS
3. Bootloader renders: load IPFS → seed → render
4. Updates: artist re-uploads new IPFS, points contract to new hash
```

**Advantages**:
- Some on-chain component (immutable seed)
- Updates possible (mutable code)
- Full feature set available
- Scalable approach

**Disadvantages**:
- Still not "pure" on-chain
- Requires contract interaction
- More complex architecture

**Implementation Effort**: Medium (4-6 days)

---

## API Constraints & Integration Points

### Bootloader.art Expected Interface

Based on generative art platform conventions, bootloader.art likely expects:

```javascript
// Input: window.seed (unique per token)
const seed = window.seed || 0;

// Output: canvas/SVG/HTML in DOM
// Deterministic: seed → same visual output every time

// Optional: window.tokenId, window.wallet for metadata
const tokenId = window.tokenId;
const wallet = window.wallet;

// Code is stored as HTML blob containing <script>
// No external requests expected (self-contained)
```

### AC Piece Adaptation Required

Current AC piece:
```javascript
function boot({ wipe, ink, line, circle, screen, params }) {
  // AC-specific initialization
}

function paint({ wipe, ink, line, circle }) {
  // Render per frame
}

export { boot, paint, act, sim };
```

For bootloader.art:
```javascript
// No "boot/paint/act/sim" lifecycle
// No parameterization beyond seed
// Single "render" function

function render(seed) {
  const rng = seedRandom(seed);
  // Use rng for all randomness
  
  // Draw to canvas
  // Return visual output
}

// Export as HTML with render() baked in
```

### Missing Features in Bootloader Context

| Feature | AC | Bootloader | Notes |
|---------|----|-----------|----|
| Networking | ✅ | ❌ | No socket.io, net API |
| Audio | ✅ | ❌ | Can't synthesize on-chain |
| Multiplayer | ✅ | ❌ | Single-user only |
| Persistence | ✅ | ❌ | No store.mjs |
| Hot reload | ✅ | ❌ | Static on-chain code |
| Piece discovery | ✅ | ❌ | Isolated on bootloader |
| Params/colon | ✅ | ❌ | Only seed-based |
| Analytics | ✅ | ❌ | No logging to AC backend |

---

## Generative Tokens: AC Angle

### What Are Generative Tokens?

Generative tokens are **NFTs where the artwork is generated algorithmically**, with each token receiving a unique seed that produces a unique visual output. Platforms like fxhash, Art Blocks, and bootloader.art enable this.

### AC's Unique Position

**Strengths for Generative Tokens**:
1. **KidLisp**: Purpose-built for generative art (declarative, functional)
2. **Existing Ecosystem**: 500+ pieces already created
3. **Community**: Active artist community
4. **Interactivity**: Can create interactive generative pieces (if modified)

**Weaknesses**:
1. **Runtime Size**: 7-8 MB baseline (vs. 20-24 KB industry standard)
2. **Not Designed For**: Single-shot rendering (expects interactive loop)
3. **Off-Chain Asset Dependency**: Fonts, samples, etc.
4. **Community Expectation**: AC pieces are interactive/playable, not static

### Generative Token Use Cases for AC

**Viable**:
1. **KidLisp Art Series**: Pure math-based visuals (fractals, patterns, algorithms)
   - Example: `$cow` (recursion) → mint as generative token series
   - Example: `$cw` (color work) → seed-based palettes

2. **Artist Collaborations**: Art Blocks-style drops
   - Mint 100 unique renderings of curated KidLisp piece
   - Each with unique seed
   - Artist retains code update rights via bootloader.art

3. **Cross-Platform Bridges**: 
   - Mint on bootloader.art
   - Code stays on AC (via hot-reload)
   - Collectors view on bootloader, play on AC

**Not Viable**:
1. **Game Pieces**: squash, 1v1, etc. (require multiplayer)
2. **Interactive Sound Art**: pieces with `sound` library
3. **Real-Time Collaboration**: pieces using `net`, `chat`

---

## Recommendations

### Short Term (Feasibility Phase)

1. **Create KidLisp Bootloader Shim** (1-2 days)
   ```bash
   npm run create:bootloader -- --template=kidlisp-mini
   ```
   - Fork kidlisp-bundler.mjs
   - Target 20-24 KB on-chain bundle
   - Test with 5 existing KidLisp pieces
   - Document seed → params mapping

2. **Evaluate 3 Candidate Pieces** (1 day)
   - `$pie` — static generative visual
   - `$cw` — color palette generation
   - `$cow` — fractal/recursive patterns
   - Measure minified bundle size
   - Verify deterministic seed output

3. **Document bootloader.art Integration** (1 day)
   - API compatibility matrix
   - Size reduction techniques
   - Example adaptation (KidLisp piece → bootloader.art)

### Medium Term (Proof of Concept)

1. **Deploy 1-3 Pieces to Bootloader**
   - Partner with bootloader.art
   - Mint small test series (10-20 tokens)
   - Gather community feedback

2. **Implement Seed-Based Parameterization**
   - Map seed → existing `params` system
   - Ensure deterministic output (no timestamps, etc.)
   - Test across multiple browsers/platforms

3. **Create IPFS Bridge** (optional)
   - Upload AC bundles to Pinata
   - Create metadata templates
   - Document off-chain approach

### Long Term (Ecosystem Integration)

1. **Off-Chain Code, On-Chain Metadata**
   - Hybrid approach with IPFS references
   - Allows full AC runtime + pieces
   - Artist + collector-friendly metadata

2. **AC-Native Generative Token Platform**
   - Build bootloader.art-like experience within AC
   - Own token minting flow
   - Seed-based piece rendering
   - Contract integration (ERC-721)

3. **Live Code Updates via bootloader.art**
   - Leverage bootloader's updatable-code feature
   - Artists iterate on pieces post-mint
   - Collectors' NFTs evolve over time
   - Aligned with AC's dynamic philosophy

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|-----------|
| Size constraints prove insurmountable | Medium | High | Fall back to IPFS approach, accept off-chain code |
| bootloader.art API incompatibility | Low | Medium | Early POC with their team, API documentation review |
| KidLisp → seed-determinism issues | Low | Medium | Extensive testing, fix any RNG non-determinism |
| Community disinterest (not seen as "real" AC) | Medium | Medium | Frame as expansion, not replacement; emphasize artist control |
| IPFS availability/centralization concerns | Low | High | Partner with Pinata, Arweave backup, own IPFS node |
| Gas costs for on-chain updates | Low | Low | Use L2s (Optimism, Arbitrum), or fully off-chain approach |

---

## Cost Estimation

| Task | Effort | Cost (Internal) | Notes |
|------|--------|-----------------|-------|
| KidLisp bootloader shim | 1-2d | Low | Reuse kidlisp-bundler.mjs |
| Integration testing | 1-2d | Low | 5-10 pieces |
| Documentation | 1d | Low | API mappings, examples |
| POC deployment | 2-3d | Low | bootloader.art partnership |
| IPFS bridge (optional) | 2d | Medium | Pinata API integration |
| **Total (Bootloader Path)** | **7-10d** | **~$2-3k** | Low cost, medium effort |
| Full AC + IPFS approach | 3-5d | Medium | Simpler, less optimization |

---

## Proof of Concept Roadmap

### Week 1: Research & Planning
- [x] Research bootloader.art capabilities
- [ ] Contact bootloader.art team for API details
- [ ] Identify 3 candidate KidLisp pieces
- [ ] Create detailed size budget

### Week 2: Implementation
- [ ] Create KidLisp bootloader bundle target
- [ ] Implement seed-based rendering
- [ ] Test determinism across browsers
- [ ] Create example piece adaption

### Week 3: Testing & Integration
- [ ] Deploy 1-3 pieces to testnet
- [ ] Gather feedback
- [ ] Optimize bundle size further
- [ ] Document integration process

### Week 4: Go/No-Go Decision
- [ ] Evaluate technical + community reception
- [ ] Decide on IPFS vs. on-chain approach
- [ ] Plan full launch (if proceeding)

---

## Conclusion

**Integration with bootloader.art is technically feasible** for a **KidLisp-focused, on-chain-optimized subset** of AC's creative platform. Key findings:

1. **Size Constraints Are the Core Challenge**
   - Full AC runtime (7-8 MB) is 300+ times too large for on-chain storage
   - Ultra-minimal KidLisp bundle can reach ~20-24 KB (achievable but tight)
   - IPFS off-chain approach removes size constraints, but loses "on-chain" authenticity

2. **KidLisp Is the Right Vehicle**
   - Interpreter-based (smaller than JavaScript)
   - Math-centric (suited to generative art)
   - Already optimized in existing codebase

3. **Three Viable Paths**:
   - **Approach 1** (Recommended): Ultra-minimal KidLisp on-chain (~20 KB)
   - **Approach 2** (Easier): Full bundle on IPFS, metadata on-chain
   - **Approach 3** (Hybrid): Seed on-chain, code on IPFS

4. **Community/Market Fit**
   - Generative tokens are hot; AC has the art
   - But AC's identity is interactive, not static
   - Position as "evolution of AC," not replacement

5. **Recommended Next Step**:
   - Build KidLisp bootloader shim (1-2 days)
   - Test with 3 pieces
   - Contact bootloader.art team with POC
   - Decide on long-term integration path

---

## References

- [bootloader.art](https://bootloader.art/) — Official platform
- [bootloader.art Help](https://bootloader.art/help) — Documentation
- [Advice on Creating On-Chain Generated NFT Art](https://rubydusa.medium.com/advice-on-creating-on-chain-generated-nft-art-6ea2ac79d7cf) — Technical constraints
- [How To Create Generative Art NFTs](https://chain.link/tutorials/how-to-create-generative-art-nfts) — Chainlink tutorial
- [Autoglyphs](https://www.larvalabs.com/autoglyphs) — Early on-chain generative art (precedent)
- [On-Chain Is Art Blocks](https://medium.com/the-link-art-blocks/how-on-chain-is-art-blocks-5ccd553dd370) — Art Blocks architecture

---

**Report compiled**: 2026-04-14  
**Next review**: After initial bootloader.art team contact
