# Merry Crossfade via KidLisp Compositor — Technical Study

**Date:** 2026-02-18
**Goal:** Determine if `$paintApi.kidlisp()` can use the KidLisp `embed` layer system to crossfade between `$code` pieces during merry pipelines, without generating new programs.

---

## Current State

### How merry transitions work now

`startMerryPiece()` in `prompt.mjs:1298` calls `jump(piece)` which **fully unloads** the current piece and loads the next. No visual transition exists — only a 150ms flash on the 1px progress bar.

### How KidLisp native layering works

Inside a full KidLisp piece, `(embed $roz 0 0 w h 128)` creates:
- An independent `KidLisp()` instance with its own frame counter, random seed, timing
- A dedicated pixel buffer (pooled allocation)
- Per-frame evaluation via `renderSingleLayer()` → `evaluate(parsedCode, api, env)`
- Compositing via `pasteWithAlpha()` or GPU-batched `compositeLayers()`

The compositor runs at the end of each `evaluate()` call:
1. `layer0` → screen (all direct drawing)
2. `bakes[]` → screen (snapshot layers)
3. `embeddedLayers[]` → screen (each with position + alpha)

This is exactly what we need for crossfade: two `$code` pieces rendered as embedded layers with animated alphas.

### How `$paintApi.kidlisp()` works (the JS↔KidLisp bridge)

`disk.mjs:5882` — a JavaScript `.mjs` piece can render KidLisp into a buffer:

```javascript
const painting = paintApi.kidlisp(0, 0, w, h, "$roz");
```

Flow:
1. Resolves `$code` via multi-level cache (RAM → IndexedDB → network)
2. Creates a `painting(w, h, callback)` offscreen buffer
3. Inside callback: sets up API, calls `executeLispCode(source, api)`
4. `executeLispCode` parses + evaluates via `globalKidLispInstance.evaluate(ast, api, env)`
5. Pastes result to screen at `(x, y)`

---

## The Gap: `embed` is disabled in `paintApi.kidlisp()`

**This is the critical finding.**

In `disk.mjs:6100-6112`, before calling `executeLispCode`, the bridge does:

```javascript
globalKidLispInstance.embeddedLayers = null;  // ← KILLS EMBED
```

This means if a `$code` piece contains `(embed $other)`, the embed call has nowhere to register the layer. The full compositor (layer0 → bakes → embeddedLayers) runs inside `evaluate()`, but with `embeddedLayers = null`, step 3 is skipped.

**Why it's nulled:** To prevent cross-contamination between successive `paintApi.kidlisp()` calls. Each call is meant to be a self-contained render into its own buffer.

**But this is also the exact mechanism we need for crossfade.**

---

## Three Approaches (simplest → most capable)

### Approach A: Side-by-side `paintApi.kidlisp()` calls with manual alpha blend

**Don't fix the embed gap. Work around it.**

A thin "merry-fade" host piece (`.mjs`) renders two `$code` buffers separately and alpha-blends them:

```javascript
// merry-fade.mjs (conceptual)
let fadeProgress = 0; // 0 = showing A, 1 = showing B

function paint({ kidlisp, paste, screen, system }) {
  const w = screen.width, h = screen.height;
  const { fadeBuffer, pipeline, currentIndex } = system.merry;

  // Render outgoing piece into buffer (noPaste: true = don't auto-paste)
  const outgoing = kidlisp(0, 0, w, h, `$${pipeline[currentIndex].piece}`, { noPaste: true });

  // Render incoming piece into buffer
  const incoming = kidlisp(0, 0, w, h, `$${pipeline[currentIndex + 1].piece}`, { noPaste: true });

  // Manual alpha composite: incoming first (background), outgoing on top fading out
  if (incoming) paste(incoming, 0, 0);
  if (outgoing) {
    // Need a pasteWithAlpha equivalent exposed to JS pieces
    pasteWithAlpha(outgoing, 0, 0, Math.round(255 * (1 - fadeProgress)));
  }
}
```

**Pros:**
- No changes to KidLisp internals
- Each `$code` renders in its own isolated context (no cross-contamination)
- `paintApi.kidlisp()` already handles `$code` resolution, caching, frame counting

**Cons:**
- `pasteWithAlpha` is not exposed to JS pieces — only exists inside `kidlisp.mjs`
- Two full KidLisp evaluations per frame (but `embed` does this already for native layers)
- Pieces that use `embed` internally would have those embeds silently dropped

**What's needed:**
1. Expose `pasteWithAlpha` (or a simpler alpha-paste) to JS pieces via `$paintApi`
2. A way to render two `$code` pieces with independent frame counters (currently `globalKidLispInstance` is a singleton with one `frameCount`)

---

### Approach B: Un-null `embeddedLayers` for a special "compositor mode"

**Let `paintApi.kidlisp()` optionally enable the full embed pipeline.**

Add a `{ compositor: true }` option:

```javascript
kidlisp(0, 0, w, h, source, { compositor: true });
```

When `compositor: true`:
- Don't null out `embeddedLayers` — initialize a fresh `[]` instead
- Let the KidLisp source use `(embed ...)` normally
- The `evaluate()` compositor phase runs, compositing embeds into the painting buffer
- The painting is then pasted to screen as usual

This would allow a KidLisp source string (not a generated program — a static expression) to drive the crossfade:

```javascript
// In merry pipeline, instead of jump():
const fadeExpr = `(embed $${outPiece} 0 0 ${w} ${h} ${255 - alpha}) (embed $${inPiece} 0 0 ${w} ${h} ${alpha})`;
kidlisp(0, 0, w, h, fadeExpr, { compositor: true });
```

**Pros:**
- Uses the native KidLisp embed path — full layer lifecycle (caching, buffer pooling, independent instances)
- Crossfade is expressed as KidLisp evaluation, not JS pixel manipulation
- Gets GPU-accelerated compositing for free via `compositeLayers`

**Cons:**
- The `fadeExpr` string changes every frame (alpha value changes) — cache key invalidation
- Singleton `globalKidLispInstance` state management gets trickier
- Need to ensure `embeddedLayerCache` persists across frames (currently would be wiped)

**What's needed:**
1. `{ compositor: true }` option in `paintApi.kidlisp()`
2. Initialize `embeddedLayers = []` instead of `null` when compositor mode is on
3. Persist `embeddedLayerCache` across calls so embed doesn't re-fetch/re-create layers every frame
4. Handle the changing alpha in the embed expression without busting the painting cache

---

### Approach C: Direct layer API on the singleton (cleanest)

**Expose the embed layer machinery as a first-class JS API.**

Instead of constructing KidLisp source strings, let JS code directly manage embedded layers on `globalKidLispInstance`:

```javascript
// New API surface on $paintApi or system.kidlisp:
const layerA = kidlisp.embedLayer("$roz", { x: 0, y: 0, w, h, alpha: 200 });
const layerB = kidlisp.embedLayer("$xom", { x: 0, y: 0, w, h, alpha: 55 });
kidlisp.compositeToScreen(); // runs the layer0 → bakes → embeds compositor
```

Under the hood, `embedLayer` would call `createEmbeddedLayerFromSource()`, `renderSingleLayer()`, and update the layer's alpha. `compositeToScreen()` would run the compositor phase.

**Pros:**
- Cleanest API — no string construction, no cache key issues
- Full access to layer lifecycle (create, update alpha, destroy)
- Merry pipeline drives layers directly: tween alphas, add/remove layers
- Could work for `.mjs` pieces too in the future (capture their screen buffer as a "layer")

**Cons:**
- Most work to implement — exposes internal KidLisp structures to JS
- Need to manage layer lifetimes carefully (who cleans up?)

---

## Singleton Problem: One `globalKidLispInstance`, Two Pieces

All three approaches share a challenge: `globalKidLispInstance` is a singleton. When rendering two `$code` pieces:

- **Frame counter**: Each piece needs independent `frameCount` for timing expressions (`3s...`). Native `embed` solves this — each embedded layer gets its own `localFrameCount` (kidlisp.mjs:12567). `paintApi.kidlisp()` currently uses the singleton's `frameCount`, so two successive calls share the same counter.

- **Random seed**: Each piece needs independent randomness. Native embed creates a new `KidLisp()` instance per layer with unique seeds. `paintApi.kidlisp()` reuses the singleton.

- **State isolation**: KidLisp pieces can have stateful variables. Native embed isolates them in separate `localEnv` per instance. `paintApi.kidlisp()` shares the singleton's `localEnv`.

**Approaches A and B inherit these problems. Approach C solves them** because it uses `createEmbeddedLayerFromSource()` which creates independent `KidLisp()` instances per layer.

---

## Recommendation

**Start with Approach B** — it's the sweet spot:

1. It reuses the existing embed infrastructure (buffer pooling, `pasteWithAlpha`, GPU compositing)
2. The "compositor mode" flag is a small, contained change
3. The embed expression approach is very AC — crossfade is expressed as KidLisp evaluation
4. The singleton problem is solved naturally because `embed` creates independent instances per layer

**Implementation sketch:**

```
disk.mjs paintApi.kidlisp():
  - Add `compositor` option
  - When true: set embeddedLayers = [] (not null)
  - Persist embeddedLayerCache across calls via globalKidLispInstance
  - Don't bust painting cache on alpha changes (separate the alpha from the cache key)

prompt.mjs startMerryPiece():
  - When `:fade` param is present and pieces are $code:
    - Don't jump() immediately
    - Enter "crossfade mode" for N ms
    - Each frame: call paintApi.kidlisp() with compositor=true
    - Pass embed expression with interpolated alphas
    - When fade completes: jump() to the new piece (or stay in compositor mode)

Alternative (cleaner): stay in compositor mode the entire merry pipeline
  - Never jump() between $code pieces
  - One persistent paintApi.kidlisp() compositor call manages all layers
  - Merry pipeline just updates which $code is active and at what alpha
  - Progress bar still renders as overlay
```

**The "never jump" variant is the real unlock** — the entire merryo pipeline could be a single compositor context with embed layers fading in and out, no piece loading/unloading at all. Each `$code` piece runs as an embedded layer with its own KidLisp instance, frame counter, and buffer. The merry scheduler just tweens alpha values.

---

## What Exists vs What's Needed

| Capability | Exists? | Where |
|-----------|---------|-------|
| `$code` resolution (multi-level cache) | Yes | kidlisp.mjs:637 |
| Independent KidLisp instances per layer | Yes | kidlisp.mjs:12552 |
| Buffer pooling for layers | Yes | kidlisp.mjs:12684 |
| Per-pixel alpha compositing | Yes | kidlisp.mjs:13261 |
| GPU batch compositing | Yes | graph.mjs:5372 |
| `paintApi.kidlisp()` with `$code` | Yes | disk.mjs:5882 |
| Embed layers in `paintApi.kidlisp()` | **No** — nulled out | disk.mjs:6100 |
| `pasteWithAlpha` exposed to JS pieces | **No** | Only in kidlisp.mjs |
| Independent frame counters per `paintApi.kidlisp()` call | **No** — singleton | disk.mjs:6337 |
| Merry pipeline driving layer alphas | **No** | prompt.mjs does `jump()` |
| `:fade` colon param parsing | **No** | Would go in mo.mjs / prompt.mjs |

---

## Next Steps

1. **Prototype Approach B**: Un-null `embeddedLayers` in `paintApi.kidlisp()` with `{ compositor: true }`, verify that `(embed $code 0 0 w h alpha)` works inside the bridge
2. **Test with two `$code` pieces**: Confirm independent frame counters, state isolation, alpha blending
3. **Wire into merry**: Parse `:fade` param, implement crossfade timing in `startMerryPiece()`
4. **Optimize**: Ensure painting cache doesn't thrash on alpha changes (separate alpha from cache key)
