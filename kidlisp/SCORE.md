# KidLisp — Score

The KidLisp "score": one place to track the language spec, the reference implementation, every conforming runtime in the monorepo, and the conformance corpus they're all graded against.

> KidLisp is *a language*, not *a file*. `system/public/aesthetic.computer/lib/kidlisp.mjs` is the **reference implementation**; the **specification** is `KidLisp Decree '26`. Any runtime — JS, Common Lisp, Swift, WASM, Game Boy ROM — claims a conformance level against the Decree and runs the same corpus.

## 1. Specification

| Document | Path | Role |
|---|---|---|
| Decree '26 | `kidlisp/docs/core/kidlisp-decree-26.md` | Normative ABI + conformance levels |
| Decree (latest pointer) | `kidlisp/docs/core/kidlisp-decree.md` | Always points at the current stable decree |
| Language reference | `kidlisp/docs/core/language-reference.md` | Syntax + constructs |
| Complete API map | `kidlisp/COMPLETE_API_MAP.md` | All 118 built-ins, grouped |

**Conformance levels** (Decree '26 §3):
- `Core` — parser, evaluator, lifecycle ABI, host shape
- `Render` — offscreen buffers, `page`/`paste`, alpha compositing
- `Audio` — `amp`/`mic` globals + ranges

**Named profiles:**
- `RBP-26` — `$roz` Baseline Profile (Decree '26 §13.1). Minimum surface to run `$roz` correctly: `ink`/`line`/`circle`/`scroll`/`spin`/`zoom`/`contrast`/`?`/`1s...`/`2s...`/`0.5s`/`fade:…`, magic vars `w`/`h`/`w/2`/`h/2`.

Canonical claim format: `KidLisp Decree '26: Core + Render` (extend with `+ Audio` and/or `+ RBP-26` as supported).

## 2. Reference Implementation

| Path | Surface |
|---|---|
| `system/public/aesthetic.computer/lib/kidlisp.mjs` | The canonical evaluator — JS, runs in browser + Node + JSC |
| `system/netlify/functions/store-kidlisp.mjs` | Source storage / `$code` resolution / hit counting |
| `kidlisp/tools/` | `api-summary.mjs`, `source-tree.mjs`, etc. |

The reference impl is **load-bearing for deployment** (web runtime, service worker cache, `disk.mjs` imports, session server). Its path is frozen; do not move it.

## 3. Runtime Registry

Every implementation in the monorepo, with claimed conformance level and current status. **Update this row when you change a runtime's surface.**

| Runtime | Path | Decree claim | Status | Notes |
|---|---|---|---|---|
| **JS** (reference) | `system/public/aesthetic.computer/lib/kidlisp.mjs` | `'26: Core + Render + Audio` | shipping baseline | Canonical; local opt-in execution controls and numeric-plan tooling are experimental (see §9) |
| **Common Lisp** (AC Native) | `fedac/native/cl/kidlisp-*.lisp` | `'26: Core + Render` (target) | in progress | Tree-walker, DRM/KMS framebuffer; replacing QuickJS path |
| **Swift** (Menuband) | `slab/menuband/` | `'26: Core + Render` (planned) | not started | This document's motivating port; Metal blit + CPU framebuffer |
| **WASM** | `kidlisp-wasm/`; `lib/kidlisp-plan-wasm.mjs` | unclaimed | experimental | Existing f32 renderer compiler; separate f64 `numeric-v1` backend preserves audited JS arithmetic (§9) |
| **WebGPU benchmark** | `kidlisp/benchmarks/raytrace-gpu.mjs` | unclaimed | experimental | f32 WGSL ray kernel lowered from the numeric plan; bounded handwritten scene host, CPU fallback (§9) |
| **WebGPU feedback graph** | `kidlisp/graph/` | unclaimed | experimental | Pinned `$roz` reference controls → ordered integer GPU effects; persistent buffers and 2D preview (§9) |
| **Playdate** | `kidlisp-playdate/` | unclaimed | experimental | C runtime for Panic Playdate |
| **Game Boy** | `kidlisp-gameboy/` | unclaimed | experimental | GBDK C + asm |
| **N64** | `kidlisp-n64/` | unclaimed | experimental | Bare-metal asm exploration |
| **CLI** | `kidlisp-cli/` | host-runner | shipping | Public `kidlisp` CLI |
| **Sidecar** | `kidlisp-sidecar/` | host-service | shipping | Clojure service |
| **VS Code syntax** | `vscode-extension/kidlisp-syntax.ts` | tooling | shipping | Editor highlighting only |
| **kidlisp.com** | `kidlisp.com/` | site | shipping | Landing page |
| **Knowledge base** | `kidlisp-knowledge/` | docs | shipping | LLM-oriented documentation aggregator |
| **Analysis tools** | `kidlisp-tools/`, `kidlisp/tools/` | tooling | shipping | Probe + source-tree (two locations — see Open Questions) |

## 4. Conformance Corpus

The corpus is **the top KidLisp pieces by live hit count**, pulled from production. Any new runtime is expected to render these pixel-comparably against the reference implementation.

Refresh command: `node kidlisp/conformance/oracle.mjs refresh --top 40` (pins the top 40 into `conformance/corpus.json`). The JS reference is graded against itself by the pixel oracle: `node kidlisp/conformance/oracle.mjs check` renders the corpus from `origin/main` and this checkout and fails on blank, gone or newly-erroring pieces — see [`conformance/README.md`](conformance/README.md).

### Top 10 (refreshed 2026-05-25)

| # | Code | Hits | Chars | Source / feature footprint |
|---|------|-----:|------:|---|
| 1 | `$bop` | 14,708 | 25 | `purple, ink, line, blur 5` — bare-color wipe, bare commands, blur |
| 2 | `$pie` | 10,363 | 153 | `(fps 24)`, timing wipe, magic vars, `scroll frame frame` |
| 3 | `$roz` |  9,106 | 239 | `fade:` gradient, `1s...`/`2s...`/`0.5s`, `?`, spin/zoom/contrast/scroll/circle → **RBP-26 reference** |
| 4 | `$4xa` |  8,330 |   4 | `blue` — bare color = implicit wipe |
| 5 | `$ceo` |  8,167 |  82 | `coat fade:…:frame` animated gradient + zoom |
| 6 | `$cow` |  7,819 |  52 | `($39i 0 0 w h 128)` `($r2f …)` — `$code` embeds (recursive eval) |
| 7 | `$4bb` |  5,629 | 215 | `bake`/`burn`, `ink … erase`, scroll vectors, blur |
| 8 | `$wib` |  5,424 |  11 | `(wipe blue)` |
| 9 | `$39i` |  5,332 | 275 | flood, circle, timed zoom/blur/contrast, multi-statement |
| 10 | `$nsh` |  5,326 |   7 | `kidlisp` (bare identifier) |

### Suggested phasing for a new port

1. **$bop · $wib · $4xa** — bare color → wipe, bare commands, `(wipe color)`, `blur N`
2. **$pie · $39i** — `(fps N)`, timing tokens, magic vars (`w`/`h`/`width`/`height`/`frame`), `scroll`, `flood`, `circle`, `zoom`, `contrast`
3. **$roz · $ceo** — `fade:` gradients with `:frame` animation, `spin`, `coat`, `?`/`...` cycle → **claims `RBP-26`**
4. **$cow** — `$code` embed (recursive sub-region eval + network/cache fetch)
5. **$4bb** — `bake`/`burn` (offscreen page semantics, Decree '26 §6)

A port hitting phase 3 can publish `KidLisp Decree '26: Core + Render + RBP-26`.

## 5. Adding a New Runtime

1. Pick a path. Sibling-of-monorepo (`kidlisp-foo/`) for hardware/platform ports; embedded inside a host app (`slab/menuband/`, `fedac/native/cl/`) for ports tied to a specific runtime.
2. Add a row to §3 above with `claim = unclaimed`, `status = not started`.
3. Build through the §4 phases. After each phase, render the corpus and diff against reference frames.
4. When all `RBP-26` tokens render correctly, update the row to `'26: Core + Render + RBP-26`.
5. Conformance test artifacts (frame PNGs, diff reports) should land under `kidlisp/conformance/<runtime>/`.

## 6. Open Questions

- **`kidlisp/tools/` vs `kidlisp-tools/`** — two tool homes; the sibling should probably absorb the sub-dir or vice versa. Not urgent, but document the decision when made.
- **Physical reorg** — should the reference impl be re-homed under `kidlisp/` and re-exported from `system/public/aesthetic.computer/lib/`? Conceptually cleaner, but the blast radius (service worker cache keys, bundler resolution, WebSocket module loader prefetch, every `disk.mjs` import path) hasn't earned its cost. **Decision: keep `kidlisp.mjs` where it is; `kidlisp/` is the *house* (spec, corpus, docs, registry), not the *runtime location*.**
- **`KDL-26` test suite** — the Decree §10 reserves this name for the formal conformance suite. Land the corpus in §4 here, then graduate it to `KDL-26` once stable.
- **Sibling consolidation** — `kidlisp-knowledge/` and `kidlisp/docs/` overlap; resolve before they drift further.

## 7. Pointers

- Decree '26: [`docs/core/kidlisp-decree-26.md`](docs/core/kidlisp-decree-26.md)
- Language reference: [`docs/core/language-reference.md`](docs/core/language-reference.md)
- API map: [`COMPLETE_API_MAP.md`](COMPLETE_API_MAP.md)
- Directory map: [`STRUCTURE.md`](STRUCTURE.md)
- Top-level project score: [`../SCORE.md`](../SCORE.md)

## 8. High-level assembly direction (proposal)

KidLisp source is a compact, human-editable instruction language for a visual
computer. A compiler can lower its expressions into an execution plan with
explicit values, resources, effects, and update dependencies. Programs retain
their source and meaning across hosts; each host implements the same operations
using its own renderer and media system.

This is an optimization direction, not a new Decree conformance claim or a
replacement compiler already implemented across the language. The existing WASM
compiler and JS precompile paths are precedents to consult before adding a
shared intermediate representation.

### Execution contract

| Stage | Responsibility |
|---|---|
| Parse | Source locations and canonical syntax; preserve inspectable source |
| Resolve | Bind operations and variables to known operations and value slots |
| Lower | Record pure calculations, ordered effects, resources, and dependencies |
| Prepare | Load data and build reusable layouts, buffers, and input regions |
| Update | Recompute values whose inputs changed; execute required effects in source order |
| Host | Draw, play media, and deliver input without changing language semantics |

An operation records its inputs, result shape, effect class, dependencies, and
host capability. Constant arithmetic may fold; stable text may lay out once;
viewport expressions update on resize; media bindings update from the media
clock. Clock, frame, random, ink, page, timers, input, and network operations
have distinct dependencies. Unknown operations keep the reference evaluator
path until their behavior has an explicit contract.

Optimization must preserve evaluation order, random consumption, timer behavior,
mutable graphics state, errors, and lifecycle cleanup. A drawing or random effect
cannot disappear because its arguments stayed constant. Fetching data cannot
execute source. Unsupported host capabilities must be discoverable and fail
explicitly. Future compiled plans should be versioned and inspectable alongside
the original source, with stable operation identifiers and source mappings.

### First example: a prepared reader

```lisp
(wipe black)
(def episode (fetch "episode.json"))
(flow (listen episode))
```

The local JS prototype lowers the episode into cached text lines, link regions,
and positioned word fragments. It validates canonical prose and ordered cues
once per loaded resource. A viewport change rebuilds geometry; an audio-clock
update selects a cue by binary search and paints visible lines. Resource records
are immutable after loading: replace the record to change its contents. Generic
`flow` expressions still compare their values so dynamic prose keeps working.

The reader uses a specialized execution plan. The local `numeric-v1` compiler
now also lowers an audited arithmetic subset into constant, input-slot, and call
instructions; it is available explicitly and does not replace reference
evaluation. `fetch`, `get`, `listen`, and rich-text forms are local JS extensions;
other runtimes have not claimed them.

### Performance evidence and gates

Run `node kidlisp/tools/bench-rich-text.mjs [path-to-rich-text-module]` for a
repeatable synthetic reader benchmark. On Neo, an initial before/after sample
put 295-word steady-frame bookkeeping at approximately 7 μs / 1 μs; a 10,000-word
sample was approximately 77 μs / 1 μs. Preparation became more expensive because
it builds the word geometry up front. These figures exclude glyph rasterization,
the KidLisp interpreter, audio decoding, and GPU presentation. They establish a
specific optimization, not total frame latency or a device-wide speed claim.

A five-second headless Chrome sample of the packed reader at 990 × 270 while
playing narration measured 0.599 seconds of main-thread task time (0.469 seconds
of script time). That covers the packed host as well as the reader; it excludes
startup, other browser processes, and GPU/audio work. Playback, pause, seek,
and the highlighted render were checked separately. This is one desktop sample,
not a mobile or frame-rate guarantee.

A compiler optimization needs semantic differential tests and rendered evidence
under fixed input, clock, and random seeds. Measure preparation time, steady
frame median and tail latency, allocation/GC, memory, audio clock lag, and dropped
frames separately. Test representative pieces and long resources on intended
hosts. Keep the current corpus oracle for deployment regression checks; its
noise-tolerant images cannot establish exact compiler equivalence by themselves.

## 9. Engineering constraints

These are the rules for growing KidLisp. Existing paths that do not yet meet a
rule need an explicit limitation and a migration test; an optimization does not
establish a new language behavior merely by being faster.

1. **Preserve meaning.** Changes to evaluation order, numeric edge cases, random
   consumption, timers, graphics state, or lifecycle are language changes.
   Optimizations need differential tests against the reference evaluator.
2. **Make execution reproducible.** Conformance profiles declare their clock,
   seed, input stream, viewport, resources, and host capabilities. State belongs
   to a runtime instance; tests must not patch global time or randomness.
3. **Declare instruction contracts.** Audited operations specify stable IDs,
   arguments, results, effects, dependencies, and supported profiles. Tooling
   derives its metadata from that registry. Unknown effects stay unoptimized.
4. **Bound work and ownership.** Nested evaluation shares a work budget; depth,
   source size, memory, caches, requests, and media have explicit limits and
   owners. Leaving a piece disposes its resources. Exhaustion must be actionable
   and unwind cleanly.
5. **Make measurement observational.** Enabling a profiler must preserve program
   behavior. Report preparation, execution, rendering, allocation, memory, and
   slow-frame results separately, with the host and workload specified.
6. **Preserve inspectability and portability.** Keep canonical source and content,
   source mappings, versioned plans, declared capabilities, and pinned assets.
   An unsupported host capability must fail explicitly.

### Implemented foundation (local, opt-in)

The JS reference accepts `new KidLisp({ execution })`, where `execution` is a
`KidLispExecution` from `lib/kidlisp-execution.mjs`. The host calls
`execution.beginFrame(frame)` once before that frame's inputs and evaluation.
The context supplies a uint32 seed, epoch, frame duration, and shared work/depth
ledger. Embedded instances derive labeled seeds and inherit that ledger. Source
parsing is capped at one million characters and the selected nesting depth.
Budget failures include a code, frame, work count, and operation; failed frames
remain failed until the host begins the next frame. Function scope is unwound.

`kidlisp/conformance/replay.mjs` exposes the bounded `commands-v1` fixture runner.
`lib/kidlisp-ops.mjs` owns eight audited numeric contracts and their aliases;
`node kidlisp/tools/operations.mjs` emits their versioned JSON manifest.
`lib/kidlisp-plan.mjs` exposes `compileNumeric(expression, { bindings, fold })`
and `runNumeric(plan, inputs, execution?)`. Plans carry source AST paths, explicit
input slots, and numeric encoding that survives JSON even for -0, NaN, and
infinities. Compilation has a node/depth budget. Plans contain data and execute
without dynamic JavaScript generation. Effects and undeclared bindings are
rejected. This numeric ABI is experimental, not a ratified Decree guarantee.

Controlled evaluation bypasses the legacy profiler-dependent arithmetic cache
and diagnostic early-stop heuristic so monitoring is observational in this
profile. Default browser execution and its existing profiler paths have not
been migrated. The work ledger measures evaluator/plan work; host rasterization,
buffer allocation, network, and media still need their own resource budgets.
Command replay does not establish pixel conformance or complete media replay.
The registry covers the audited numeric subset; migrating the remaining
instructions and generated editor/reference documentation is subsequent work.

`kidlisp/conformance/pixels.mjs check` now provides exact `pixels-v1` CPU renders
through the reference module lifecycle and real software rasterizer. Five pinned
fixtures check every RGBA byte, including alpha, against reviewed PNGs and a
second isolated run. It rejects unsupported capabilities, stale goldens, and
swallowed rendering errors. Its first failure exposed boot-time color detection
executing arbitrary first operations; detection now consults registered colors.
The profile's fixed simulation cadence, bounds, and exclusions are specified in
`conformance/README.md`; it does not claim full browser or GPU conformance.

`lib/kidlisp-plan-wasm.mjs` compiles the eight `numeric-v1` operations to native
f64 Wasm, retaining reference division, remainder, rounding, NaN, and signed-zero
behavior. It imports only pure remainder when needed, validates inputs, and
charges the supplied execution ledger before running. It is an explicit backend,
not the default evaluator. The older f32 compiler remains a separate experiment.

`kidlisp/benchmarks/ray-sphere.lisp` is a first rendering-kernel workload: three
spheres, a checker floor, hard shadows, and two reflection bounces. The benchmark
checks exact frames across reference evaluation, numeric plans, Wasm, and direct
JS. It reports preparation, median and p95 whole CPU frame time, resolution,
ray/intersection counts, host, and hashes. JS still owns scene traversal, shading,
and the pixel loop. Include that boundary cost; do not call kernel timing full
runtime performance or promise photorealism from this scene. Compile larger
batches only after measuring this baseline and preserving its pixels.

The next benchmark stage now includes a whole-frame f64 Wasm renderer and an
f32 WebGPU renderer. Both embed the same audited numeric plan's ray kernel;
their scene/shading hosts are still handwritten. Wasm runs one frame with no
host imports, fixed memory, and a bounded reflection stack. GPU compute writes
directly to a persistent texture and presents without pixel readback. The
preview has capability/device-loss fallback to frame Wasm. CPU tests require
exact pixels; the documented GPU profile uses bounded color-error tolerances.
Measure completed GPU work and presentation separately from submission; browser
timestamp quantization can produce zero for short dispatches. See the conformance
README for build hashes, benchmark commands, scope and numerical limits.

`kidlisp/graph/` adds the pinned `roz-feedback-v1` experiment. The reference
evaluator parses `$roz` once and resolves seeded/timed controls each frame.
Ordered line, spin, zoom, contrast, scroll and circle nodes operate on two reused
GPU buffers. Fixed transform gather maps come from the real CPU rasterizer;
integer arithmetic and blend lookup tables retain its pixel quirks. Preparation
has a 512 × 512 limit; frames have at most six nodes and a shared evaluator work
budget. Unknown source and unsupported command parameters fail explicitly.
No model inference or shader recompilation occurs during animation.

The preview displays the source’s own 2D image. A fixed-step accumulator keeps
60 simulation updates per second independently of presentation cadence. Each
GPU submission can contain up to four ordered updates, with at most two
submissions queued. Normal animation does not wait for a GPU fence; diagnostic
runs await completion separately. GPU failure restarts the seeded CPU canvas.
The browser gate checks exact single/batched/queued pixels, wall-clock cadence,
pause/reset/resize and device loss. This controlled host is not a full browser
lifecycle or RBP-26 conformance claim. Commands and limits are documented in
`conformance/README.md`. A backend preview must display the authored source;
additional geometry belongs in a separate explicitly authored example.

For live audio, the intended host is AudioWorklet with bounded Wasm DSP blocks.
GPU processing is a separate option for buffered/offline workloads; audio's
render thread must not wait on the graphics queue. Moving evaluator traversal or
compilation to Wasm requires its own representation and benchmark; whole-frame
numeric rendering speed does not establish a speedup for object-heavy AST work.

### Compilation direction beyond the benchmark

Compile source and plans when source changes; reuse prepared code, resources,
and layouts while frame inputs change. Runtime execution requires no model
inference. Counted loops should stay inside the compiled backend, with numeric
slots and reused buffers. Hoist only proven pure expressions with stable inputs;
retain ordered drawing, random consumption, clock reads and errors.

Future tail-call support must include mutual tail calls with bounded host stack
use; lower them to frame reuse, jumps or a trampoline. Other recursion needs
explicit call frames and a shared work budget. Nested lists/records need a
general value representation and ownership/collection rules; specialize known
numeric shapes into packed layouts without silently changing aliasing or
mutation. GPU kernels require statically bounded layouts. These are design
constraints, not features implemented by the current numeric plan compiler.

Required next gates: fonts/media/GPU in controlled pixel profiles; host
memory/resource ownership; the remaining instruction contracts; then effectful
compiler lowering. Keep the browser corpus gate alongside deterministic checks
until its resources and frame delivery are controlled end to end.
