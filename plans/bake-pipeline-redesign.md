# KidLisp Bake Pipeline Redesign Plan

## Objective
Establish a dependable bake pipeline for KidLisp pieces that guarantees:
- Identical visual results between the live frame and post-bake redraws
- Stable erase semantics across pre-bake, baked, and post-bake drawing phases
- Predictable routing behavior when switching buffers (screen ↔ baked ↔ post-bake overlay)
- Clear instrumentation so regressions are caught quickly

## Current Flow (as implemented today)
1. **Frame bootstrap** (`KidLisp.paint`) restores ink state, clears caches, and calls `beginBakedFrameRouting($)` to redirect initial drawing into the baked buffer when bake data is present.
2. **Program evaluation** runs in three implicit modes:
   - **Pre-bake**: Direct to screen until first `(bake)`
   - **Bake transition**: `(bake)` snapshots the screen pixels into `bakedLayers[0]`, creates `postBakeLayer`, and flips `suppressDrawingBeforeBake`
   - **Post-bake**: Subsequent strokes should target the baked buffer first (for persistence) and optionally the post-bake overlay
3. **Compositing pass** after evaluation:
   - Fill screen with background hint, then `renderBakedLayers($)` composites baked pixels underneath current content
   - `compositePostBakeLayer($, this.postBakeLayer)` draws persistent overlay on top
   - Embedded layers and HUD render last
4. **Erase mirroring** is enabled only while routing to the post-bake overlay. Wrapped draw commands replay into the baked buffer whenever ink is set to `"erase"`.

### Contract (today)
- **Inputs**: AST for active piece, `$paintApi` (with `page`, draw primitives), cached baked/post-bake buffers
- **Outputs**: Final `screen.pixels`, updated baked layer buffer, optional post-bake overlay
- **Success criteria**: Post-bake visuals match the frame that triggered `(bake)` plus all post-bake edits; erase removes pixels consistently
- **Failure modes**: Incorrect buffer target, stale erase mirroring, size mismatches, unexpected background wipes

### Known edge cases
1. Canvas resize between frames (buffers must grow without losing history)
2. Nested embeds that issue `(bake)` independently
3. First-line color wipes interacting with suppressed pre-bake drawing
4. Post-bake erase commands issued before mirroring wrapper is in place
5. Mixed alpha content (semi-transparent strokes over erased regions)

## Gaps & Findings
- **Routing opacity**: State flags (`suppressDrawingBeforeBake`, `frameRoutingContext`) are implicit; when they desync, erase misses the baked buffer.
- **Erase timing**: Wrappers activate only during post-bake overlay routing; erase issued immediately after `(bake)` but before `switchToPostBakeRouting` can slip through.
- **Background reinitialization**: Screen fill before compositing can hide baked erasures if the alpha hint is wrong.
- **Buffer lifecycle**: `postBakeLayer` persistence and resize logic need clearer guarantees to avoid accidental clears.
- **Observability**: No single telemetry view shows which buffer a draw command hit, making parity bugs hard to diagnose.

## Current Pipeline Behavior
```
FRAME START
  if has baked content:
    route default draw target = baked buffer
    enable erase mirroring to baked when overlay active
  else:
    route default draw target = screen

PROGRAM EVALUATION
  pre-bake commands → active route
  (bake) → snapshot screen → baked[0]; instantiate/clear postBakeLayer; switch routing
  post-bake commands → post-bake overlay unless explicitly overridden
  erase while overlay routing → always mirror into baked buffer

FRAME FINALIZATION
  screen := background hint (alpha 0)
  composite baked layers (respect erase)
  composite post-bake overlay (respect erase)
  replay embeds / HUD
FRAME END
```
Key invariants:
1. **Single source of truth**: baked buffer always reflects ground truth background; overlay is additive ornament.
2. **Erase parity**: any erase in post-bake phases clears both overlay and baked buffers.
3. **Routing transparency**: the engine reports which target each command hit for debugging.
4. **Idempotent baking**: running `(bake)` multiple times only changes what was visible on screen at that moment.

## Intent vs Current System

### Desired semantics (from latest discussion)
- Treat the live screen as the "layer zero" buffer until the first `(bake)` executes.
- On each `(bake)`, allocate a brand-new pixel buffer (page). The buffer starts fully transparent; prior content isn't auto-copied unless the artist explicitly requests a merge.
- Route every draw command **after** a bake into that fresh, non-destructive buffer until the next bake occurs.
- Keep each baked layer looping independently, stacked in order (Photoshop-style) instead of flattening them back into the background.
- Ensure buffer switches are explicit page changes (`graph.page(...)`) so erases and other edits are scoped to the active layer only.
- Guarantee every KidLisp instance—including nested embeds—owns its own bake/overlay layer stack so instances remain fully isolated.

### Current implementation reality
- Only a single `bakedLayers[0]` buffer exists; subsequent bakes overwrite the same buffer rather than creating additional layers.
- Post-bake drawing defaults to either the baked buffer (for persistence) or the single `postBakeLayer` overlay; there is no per-bake layer stack.
- Erase mirroring clears both the post-bake overlay and `bakedLayers[0]`, so erasing post-bake affects the "ground truth" immediately.
- Compositing order is fixed: baked buffer → post-bake overlay → live HUD, so baked content is effectively merged into the background each frame.

### Gap summary
- **Layer multiplicity**: Need a buffer allocator that can mint a new layer for every bake call and retain prior layers for compositing.
- **Routing state**: Must evolve from the current boolean/enum approach to a stack-aware state machine that targets the newest layer by default.
- **Compositing**: Requires iterating an ordered layer stack (bottom-up) instead of the current two-layer arrangement.
- **Erase semantics**: Mirroring rules should become layer-aware so erasing on the active page only touches that page (unless explicitly flattened).

### Plan impact
- Phase 1 should expand to designing a stack-based routing state machine (screen → baked[0] → baked[n] / overlay[n]).
- Phase 2 must include a layer manager responsible for creating, resizing, and preserving a list of buffers instead of a single baked/post-bake pair.
- Later phases should introduce tests that validate multi-layer compositing, including erase isolation and per-layer persistence across frames.

## Future Pipeline Behavior (multi-layer, per-instance intent)

```
FRAME START (per KidLisp instance)
  ensure layer stack exists: [base screen] + baked layers + active overlay
  set active route = top-of-stack overlay (create transparent overlay if stack empty)

PROGRAM EVALUATION
  pre-bake commands → active route (current overlay)
  (bake) → freeze current overlay into stack, push it as immutable baked layer
           allocate brand-new transparent buffer as next overlay, page to it
  post-bake commands → draw into the new overlay until another bake occurs
  explicit layer switches → artist/pages choose which layer to edit; erase only affects active page

FRAME FINALIZATION
  composite layers bottom → top for this instance (base screen, baked layers, overlays)
  composite nested instances after their own stacks resolve
  add HUD / diagnostics last
FRAME END
```

Future invariants:
1. **Per-instance stacks**: every KidLisp instance (parent or nested) owns its own layer stack with no shared buffers.
2. **Transparent overlays**: each `(bake)` allocates a fresh transparent buffer; no automatic copying of prior pixels.
3. **Explicit routing**: state machine tracks which layer is active; `graph.page(...)` operations make layer changes visible and auditable.
4. **Erase locality**: erase/blend operations only mutate the current layer—no mirroring, no hidden side effects.
5. **Composable nesting**: parent pieces composite child stacks as single layers, preserving the child’s internal layering.

## Phase Plan

### Implementation Contract (2025-01-05)

#### Layer primitives
- **BakeLayerRecord** — `{ id, kind: "baked" | "overlay", buffer, width, height, createdAt, metadata }`. Overlay entries carry `metadata.active = true`; baked entries become immutable once committed.
- **LayerStackSnapshot** — `{ order: BakeLayerRecord[], activeOverlayId: string | null }` captured for telemetry and spec assertions.
- **LayerDimensions** — `{ width, height }` helper returned by both manager and routing state so callers never poke directly at raw buffers.

#### `KidLispLayerManager`
- Owns the authoritative per-instance layer stack. Exposes `beginFrame(screenBuffer)`, `ensureOverlay(dimensions)`, `commitOverlay({ cloneStrategy })`, `resizeAll(dimensions)`, `composite(api)`, and `reset()`.
- Stores baked layers in insertion order so `composite()` simply iterates bottom → top. `commitOverlay()` steals (or clones) the current overlay, pushes it into the baked stack, then allocates a fresh transparent overlay buffer and hands it to routing.
- Provides bridging accessors while legacy fields are migrated: `manager.bakedLayers()`, `manager.overlayLayer()`, `manager.hasBakedContent()`.

#### Routing state handshake
- `KidLispRoutingState.beginFrame()` seeds the stack with `{ type: "screen" }`, then the manager hydrates baked layers followed by the active overlay.
- Adds helpers `activateOverlay(layer)`, `activateLayerById(id)`, and `withLayer(kind, fn)` replacing bespoke reroute utilities. Retroactive erase queues attach to specific stack entries so replays target the correct baked layer even when multiples exist.
- Legacy booleans (`hasBakedContent`, `suppressDrawingBeforeBake`) become getters derived from the manager/routing snapshot so dependent call sites continue to function until rewritten.

#### Bake command flow
1. Ensure an overlay buffer exists (`manager.ensureOverlay` lazily allocates one sized to the current screen).
2. `(bake)` asks the manager to `commitOverlay({ cloneStrategy: "steal" })`, which detaches the active overlay buffer, appends it to the baked array, and returns a brand-new transparent overlay.
3. Routing state immediately `activateOverlay(newOverlay)` so subsequent draw commands page to the fresh buffer without flag checks.
4. Telemetry emits `🛰️ BAKE ROUTE` events carrying the new layer IDs so harness specs can assert ordering and active layer identity.
5. Final compositing walks `manager.bakedLayers()` bottom-up, then draws the current overlay if present.

#### Resilience hooks
- `manager.resizeAll(dimensions)` grows baked + overlay buffers on reframe while preserving pixel data and alpha; integrates with buffer pooling.
- `commitOverlay({ cloneStrategy: "copy" })` is available for failure recovery flows that must keep the previous overlay alive.
- `KidLisp.clearBakedLayers()` delegates to the manager so buffer recycling remains centralised.

#### Test surface
- Harness helper `snapshotBakeLayers()` returns the `LayerStackSnapshot` for assertions like `["baked:0", "baked:1", "overlay:current"]`.
- Add a multi-bake spec that executes `(bake)` twice, verifying two baked layers plus a fresh overlay while erase mirroring only affects the active layer.
- Update telemetry expectations so specs read from `routingState.toTelemetry()` and manager snapshots instead of legacy booleans.

### Phase 0 – Instrument & Baseline (diagnostics)
- [x] Add lightweight telemetry hooks (debug flag) that log buffer route and ink mode per draw command.
- [x] Capture `frameRoutingContext` transitions in verbose mode for comparison between frames.
  - `KidLisp.paint` now emits `🛰️ BAKE CTX` snapshots at `frame-start`, `frame-routing-restored`, and `frame-end`, plus context markers inside `beginBakedFrameRouting`, `switchToPostBakeRouting`, and `endBakedFrameRouting`.
- [ ] Extend plan doc with expected console patterns for common recipes (pure bake, bake+erase, bake+overlay).
- [x] Stand up terminal-driven spec tests that load KidLisp/graph modules headlessly and validate routing + erase behavior before manual client checks.
  - _Coverage_: `spec/support/kidlisp-bake-harness.js` + `spec/kidlisp-bake-harness.spec.js` now drive a full `(bake)` flow, asserting routing transitions and buffer targets across consecutive frames, including post-bake erase mirroring into the baked buffer.

#### Telemetry Console Patterns (reference)

The following snippets illustrate the expected `BAKE_TRACE` / `BAKE ROUTE` output when `BAKE_TRACE` is enabled. Field ordering may vary slightly, but the keys **must** appear. Use these to spot regressions quickly.

**1. Pure bake (no erase, single layer)**

```
🛰️ BAKE ROUTE {"event":"beginBakedFrameRouting","from":"screen","to":"baked","layerIndex":0,"layerLabel":"baked[0]","details":{"originalScreen":{"width":256,"height":256},"bakedBuffer":{"width":256,"height":256}}}
🛰️ BAKE TRACE {"cmd":"bake","target":"baked","source":"bake","route":"baked","layerIndex":0,"layerLabel":"baked[0]","args":[],"ink":null}
🛰️ BAKE ROUTE {"event":"switchToPostBakeRouting","from":"baked","to":"postBake","layerLabel":"postBakeOverlay","details":{"overlaySize":{"width":256,"height":256}}}
🛰️ BAKE ROUTE {"event":"endBakedFrameRouting","from":"postBake","to":"screen","layerLabel":"postBakeOverlay","details":{"restoredScreen":{"width":256,"height":256}}}
```

**2. Bake followed by erase (mirroring into baked buffer)**

```
🛰️ BAKE TRACE {"cmd":"line","target":"postBake","source":"postBakeOverlay","layerLabel":"postBakeOverlay","duplicated":true,"details":{"mirrorTarget":"baked","mirrorMode":"postBakeErase"}}
🛰️ BAKE TRACE {"cmd":"line","target":"baked","source":"mirrorErase","layerIndex":0,"layerLabel":"baked[0]","duplicated":true,"details":{"mirrorSource":"postBake","mirrorCommand":"line"},"metadata":{"mirrorStrategy":"erase"}}
```

**3. Bake with overlay-only edits (no baked buffer mirroring)**

```
🛰️ BAKE TRACE {"cmd":"stamp","target":"postBake","source":"postEmbedQueue","layerLabel":"postBakeOverlay","details":{"queueIndex":0,"queueLength":1,"deferredFromEmbed":true,"queuedFrame":120,"asset":"@digitpain/flower"}}
🛰️ BAKE TRACE {"cmd":"stamp","target":"baked","source":"executePreBakeDraw","layerIndex":0,"layerLabel":"baked[0]","phase":"baked-buffer","details":{"expensive":true,"asset":"@digitpain/flower"}}
```

Use these baselines when reviewing logs:

- `🛰️ BAKE ROUTE` events confirm routing state transitions and should precede any rerouted drawing.
- Each expensive asset command (paste/stamp) includes `details.expensive` and an `asset` identifier.
- Mirrored erase commands always emit paired logs; absence of the baked rerun indicates a regression.

### Phase 1 – Routing & State Hygiene
- **Routing State Machine Proposal**
  - **State container**: introduce `this.routingState` with the shape `{ stack: Array<RoutingLayer>, activeIndex: number, frame: { enteredAt: number, transitions: number } }`. Each `RoutingLayer` stores `{ id, type: "screen" | "baked" | "overlay", bufferRef, isPersistent, eraseMirroring, metadata }` so that routing targets are explicit objects rather than inferred flags.
  - **Entry points**:
    - `beginBakedFrameRouting($)` → push baked layer context onto the stack (if present), update `activeIndex`, and emit a `routingState.transition("baked")` event before returning the restore closure.
    - `switchToPostBakeRouting($)` → append/activate the overlay layer, mark it for erase mirroring, and queue any retroactive erase replay when `routingState.lastCommandWasBake` is true.
    - `endBakedFrameRouting($)` → unwind to the last `type === "screen"` layer, calling a shared `routingState.restore()` helper that swaps `graph.page` back to the recorded buffer.
    - `runWithBakedBuffer` / `executePreBakeDraw` → request temporary reroutes via `routingState.withTarget("baked", fn)` so helpers share the same mechanism.
  - **Transition helpers**: add `routingState.transition(nextLayer, options)` which logs `🛰️ BAKE ROUTE`, updates `frameRoutingContext`, and records `lastActivationAt` to aid diagnostics. Provide `routingState.snapshot(label)` to replace manual `traceFrameRoutingContext` calls.
  - **Retroactive erase support**: keep a short-lived `routingState.pendingErase` bag keyed by command name while `(bake)` completes; if `switchToPostBakeRouting` executes before the first overlay draw, replay erase commands against the baked layer using the new routing APIs.
  - **Nested instance isolation**: instantiate `routingState` inside the KidLisp constructor alongside `frameRoutingContext`, ensuring embedded KidLisp invocations receive independent stacks. Parent/child coordination flows through existing `renderEmbeddedLayers` compositing rather than shared mutable state.
  - **Migration plan**:
    1. Implement `RoutingState` class with shims that wrap existing boolean fields (`hasBakedContent`, `suppressDrawingBeforeBake`) so legacy code keeps working.
    2. Refactor `beginBakedFrameRouting` / `switchToPostBakeRouting` / `endBakedFrameRouting` to use the new APIs.
    3. Update telemetry helpers to read from `routingState` instead of `frameRoutingContext` once parity is verified.
    4. Remove deprecated flags after headless specs pass.
- [ ] Replace routing booleans with a stack-aware state machine that tracks `{ target: "screen" | "baked" | "overlay" }` per layer.
- [ ] Ensure `switchToPostBakeRouting` also retroactively mirrors erase commands issued immediately after `(bake)` (before overlay draw).
- [x] Add regression coverage so `switchToPostBakeRouting` restores screen routing when `page` throws.
- [ ] Guard `endBakedFrameRouting` so it always restores original `page` even if evaluation throws.
- [ ] Write unit-style harness (Headless KidLisp runner) that asserts route transitions for scripted sequences.
- [ ] Validate the state machine works when multiple KidLisp instances run concurrently (parent + nested) without buffer leakage.

- [ ] Add a layer manager that allocates a new transparent pixel buffer on each `(bake)` and preserves prior buffers for compositing.
- [ ] Centralize buffer resize logic; ensure baked/post-bake buffers grow using copy-on-resize without clearing erased history.
- [ ] Clarify first-line color interaction: document and enforce that baked buffers are never wiped implicitly post-bake.
- [ ] Add checksum/hash comparison to detect unintended buffer resets across frames.
- [ ] Ensure layer manager scopes buffers per KidLisp instance, including cached/reused nested instances.

### Phase 3 – Erase Semantics Refinement
- [ ] Decouple erase detection from string check by consulting `$paintApi` blend mode or a dedicated flag.
- [ ] Expand mirroring wrapper list to cover all commands that can alter pixels (`spray`, `text`, future tools).
- [ ] Confirm compositing treats all-zero + non-zero-alpha pixels consistently; add tests for semi-transparent erasures.
- [ ] Evaluate removing special-case mirroring entirely by relying on `graph`-level erase blending for whichever layer is currently paged in.

### Phase 4 – Tooling & QA
- [ ] Build scripted regression suite (KidLisp snippets) that render headlessly and compare pixel diffs before/after bake.
- [ ] Integrate the suite with CI to catch bake regressions automatically.
- [ ] Document troubleshooting playbooks in `/docs/kidlisp-bake.md` once stabilized.

## Immediate Action Items
1. Prepare telemetry scaffolding (Phase 0) to reproduce current erase mismatch with clear routing logs.
2. Prototype explicit routing state machine and verify with headless harness.
3. Schedule regression tests for `erase+bake`, `bake+overlay erase`, and mixed-resolution scenarios.

## Validation Checklist
- **Unit harness** confirms routing transitions for: no bake, single bake, multi-bake, nested embed.
- **Pixel diff tests** spot-check erase persistence two frames after `(bake)`.
- **Resilience**: resizing the canvas preserves baked history, including erased pixels.
- **Logging**: enabling debug flag prints deterministic route + buffer info for each command.

## Risks & Open Questions
- How should nested KidLisp instances share or isolate bake buffers?
- Do we need user-facing options to reset baked content explicitly without `(wipe)`?
- Can overlay mirroring become opt-in to reduce overhead for pieces that never erase post-bake?
- What is the desired behavior when multiple baked layers (beyond index 0) exist?

---
_Compiled 2025-10-04 to guide ongoing KidLisp bake stabilization work._
