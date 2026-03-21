# Study: Native KidLisp for Notepat + AC Native Pipeline

**Date:** 2026-03-09  
**Scope:** Bring `kidlisp.mjs` / native KidLisp support to FedAC native OS (`fedac/native`), including notepat integration and rendering-path parity.

---

## Short Answer

Yes, this is feasible, but direct "drop-in" of web `kidlisp.mjs` into ac-native will not work cleanly yet.

The blocker is not parsing Lisp syntax. The blocker is render-path parity:
- web KidLisp expects a multi-buffer compositor (`layer0`, `bakes`, `embeddedLayers`, alpha compositing),
- native currently exposes mostly immediate primitives and stubs for buffer routing (`painting/page/paste`).

Recommended path:
1. Add native `.lisp` loading + jump resolution first.
2. Implement real native painting/page/paste/composite bindings (C-backed framebuffers).
3. Run a native-adapted KidLisp runtime with capability flags.
4. Integrate into native notepat as background/overlay rendering with audio amp feed.

---

## Ideal Vision (AC Future KidLisp)

The long-term target should be **KidLisp Everywhere**:
- Same KidLisp source runs on web AC, native OS AC, and headless render paths.
- Host differences are explicit capability tiers, not silent behavior drift.
- Rendering model is shared: base layer + optional embeds + deterministic compositing order.
- Notepat can host KidLisp visuals locally and in future stream/rebroadcast paths.

Practical product vision:
- `system.jump("piece")` can resolve `.mjs` or `.lisp` pieces in native OS.
- Native notepat can run KidLisp visuals behind UI (audio-reactive `amp`).
- A minimal supported KidLisp subset works fast at 60fps; advanced features are added by tier.

---

## Current Pipeline Reality

### Web AC KidLisp Path (today)

Key files:
- `system/public/aesthetic.computer/lib/disk.mjs`
- `system/public/aesthetic.computer/lib/kidlisp.mjs`

Observed behavior:
- Loader tries `.mjs` first, then `.lua`, then `.lisp`.
- KidLisp source detection includes `.lisp`, `$code`, and inline Lisp-like source.
- Load path calls `lisp.module(sourceToRun, isLispFile)`.
- KidLisp runtime is singleton-backed and returns a lifecycle object (`boot/paint/sim/act`).
- Rendering is layer-composited, not just immediate draw calls.

### FedAC Native Path (today)

Key files:
- `fedac/native/src/ac-native.c`
- `fedac/native/src/js-bindings.c`
- `fedac/native/src/graph.c`
- `fedac/native/scripts/build-and-flash.sh`

Observed behavior:
- Boot loads `/piece.mjs`.
- Runtime loop: `act -> beat -> sim -> paint -> display_present`.
- `system.jump()` always resolves `/pieces/<name>.mjs`.
- `js_load_piece()` evaluates QuickJS **module JS only** from file path.
- Build script bundles `/piece.mjs` and `/pieces/*.mjs` only.

---

## Rendering Path Analysis (Critical)

### Web KidLisp Rendering Path

Conceptual frame:
1. Evaluate main AST into `layer0` buffer.
2. Evaluate/refresh embedded layers (`embed`) into independent buffers.
3. Composite in order using alpha:
   - `layer0`
   - bake layers
   - embedded layers
4. Apply post-composite effects (`scroll/zoom/contrast/...`) when needed.
5. Present final display buffer.

This path depends heavily on:
- `painting()` creating offscreen buffers,
- `page()` switching draw target,
- `paste()` and `pasteWithAlpha()` compositing,
- stable `screen.pixels` semantics for pixel-level effects.

### Native Rendering Path Today

Conceptual frame:
1. Piece draw calls hit active `graph->fb` target directly.
2. `display_present()` blits framebuffer to DRM.

Native C supports offscreen buffers (`graph_painting`, `graph_page`, `graph_paste`), but JS bindings currently expose stubs for the key APIs that KidLisp needs.

### Consequence

Even if `.lisp` loads natively, KidLisp behavior diverges immediately without renderer/API parity.

---

## Gap Matrix

| Area | Web KidLisp expectation | Native current state | Gap severity |
|---|---|---|---|
| Piece loading | `.mjs/.lua/.lisp` fallback, KidLisp detection | `.mjs` only for boot/jump | High |
| Runtime entry | `lisp.module()` lifecycle object | JS module lifecycle only | Medium |
| Offscreen buffers | `painting()` real pixel buffers | `painting()` stub object | High |
| Render target switching | `page(buffer)` | `page` stub | High |
| Buffer compositing | `paste`, `pasteWithAlpha`, `compositeLayers` | `paste` stub; no JS composite API | High |
| Pixel access | stable `screen.pixels` behavior | `screen` has width/height only | High |
| Effects path | scroll/blur/zoom/contrast mask-aware layering | limited primitive/effect path | Medium/High |
| Notepat integration | optional `kidlisp()` background + `amp` feed | no native `kidlisp()` API in piece API | High |

---

## Architecture Options

### Option A: Directly run web `kidlisp.mjs` in native as-is

Pros:
- Maximum source reuse.

Cons:
- Requires broad API parity first.
- Heavy browser-adjacent imports/features in current file.
- High integration risk and debugging cost on QuickJS bare-metal.

Verdict: not first move.

### Option B: Native subset KidLisp runtime (new file)

Pros:
- Fastest to ship visuals in native notepat.
- Can optimize strictly for native constraints.

Cons:
- Risks long-term divergence from web KidLisp semantics.

Verdict: useful as temporary bootstrap, risky as final architecture.

### Option C: Shared semantics + host adapter (recommended)

Approach:
- Keep KidLisp semantics unified.
- Introduce host capability adapter for rendering/audio/system hooks.
- Implement missing native buffer/compositor APIs so core semantics remain aligned.

Pros:
- Supports long-term "KidLisp Everywhere" vision.
- Allows phased delivery with measurable parity.

Cons:
- More upfront structure than a quick subset hack.

Verdict: best balance of speed and future correctness.

---

## Recommended Implementation Plan

### Phase 0: Decree + Capability Map (1-2 days)

Deliverables:
- Define `KidLisp Decree '26` capability baseline (required/optional APIs).
- Add runtime capability probe in native to log supported features at boot.
- Lock Tier-1 feature set for first native release.

Tier-1 target for native launch:
- `wipe`, `ink`, `line`, `box`, `circle`, `plot`, `write`, `scroll`, `blur`
- real `painting`, `page`, `paste`
- one compositing path with alpha (`pasteWithAlpha` or `compositeLayers`)

### Phase 1: Native Loader + Packaging for Lisp Pieces (1-2 days)

Changes:
- Update `system.jump` path resolution in `ac-native.c` to try:
  1. `/pieces/<name>.mjs`
  2. `/pieces/<name>.lisp`
- Add `.lisp` awareness to piece load pipeline (loader wrapper or dedicated path).
- Update `build-and-flash.sh` to bundle `/pieces/*.lisp` too.

Outcome:
- Native can navigate to Lisp pieces as first-class assets.

### Phase 2: Real Painting/Compositor Bindings in JS API (3-6 days)

Changes in `js-bindings.c` + `graph.c`:
- Replace stub `painting` with real `ACFramebuffer`-backed object.
- Replace stub `page` with `graph_page` switching.
- Replace stub `paste` with `graph_paste`.
- Add alpha-aware paste/composite function exposed to JS.
- Expose `screen.pixels` access path (or capability fallback with C compositor API).

Outcome:
- Native JS runtime can execute layer-based rendering flows.

### Phase 3: KidLisp Host Adapter for Native (3-6 days)

Changes:
- Add native adapter module (`fedac/native/lib/...`) that binds KidLisp core to native APIs.
- Keep browser-only features guarded or disabled by capability flags.
- Ensure `updateKidLispAudio({ amp, ... })` updates globals in native instance.

Outcome:
- Native KidLisp lifecycle (`boot/paint/sim/act`) runs with deterministic host behavior.

### Phase 4: Notepat Integration (2-4 days)

Changes in `fedac/native/pieces/notepat.mjs`:
- Add optional KidLisp background mode (piece param or runtime toggle).
- In `paint()`, render KidLisp background before notepat UI layers.
- Feed amplitude each frame from `sound.speaker.amplitudes` into KidLisp audio globals.
- Add fallback rendering when KidLisp source is missing/invalid.

Outcome:
- Native notepat can host live KidLisp visuals safely.

### Phase 5: Parity and Performance Hardening (ongoing)

- Frame-time budget guardrails (16.6ms @ 60fps).
- Multi-layer stress tests.
- Optional GPU-assisted compositing later (if needed).

---

## Testing Strategy

### Functional

- Loader tests:
  - boot `.mjs`
  - boot `.lisp`
  - `system.jump()` across both types
- API tests:
  - `painting/page/paste` correctness
  - alpha compositing correctness
- KidLisp behavior tests:
  - first-line color semantics
  - timing expressions
  - embed/layer ordering on supported tier

### Rendering Parity

- Golden-frame snapshot tests for selected programs:
  - simple primitives
  - scroll/blur paths
  - embedded-layer composition
- Compare native output hashes against browser reference with tolerance.

### Performance

- Frame-time logs on notepat + KidLisp background active.
- Regression threshold: no sustained drops below target fps under baseline scenes.

---

## Risks and Mitigations

- Risk: Full web `kidlisp.mjs` is too large/browser-coupled for direct native import.
  - Mitigation: host adapter + capability gating, stage features by tier.

- Risk: QuickJS memory pressure with many buffers.
  - Mitigation: C-side buffer registry + pooling + explicit layer limits.

- Risk: Silent semantic drift between web and native.
  - Mitigation: shared conformance scenes + frame snapshots in CI.

- Risk: Notepat UX regression from expensive background visuals.
  - Mitigation: adaptive quality + simple fallback mode + user toggle.

---

## Concrete Next Slice (Recommended First PR)

1. Add native `.lisp` loading path and jump fallback support.  
2. Bundle `.lisp` assets in initramfs build script.  
3. Replace JS `painting/page/paste` stubs with real C-backed implementations.  
4. Add one demo native piece that runs a minimal KidLisp source through the new path.

This sequence de-risks rendering-path infrastructure before notepat integration.

---

## Notes on Repo State

- The repository score text references `kidlisp/compiler.mjs`, but current runtime integration is centered in `system/public/aesthetic.computer/lib/kidlisp.mjs` + `disk.mjs`.
- Native runtime currently uses QuickJS module loading with many API stubs, so render-path parity work is the critical enabling step.
