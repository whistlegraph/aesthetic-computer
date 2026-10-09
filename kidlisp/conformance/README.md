# KidLisp conformance

Run the exact CPU pixel gate:

```sh
node kidlisp/conformance/pixels.mjs check --out /tmp/kidlisp-pixels
npm run test:kidlisp:exact
```

`pixels-v1` runs the reference module's boot, pointer input, simulation, paint,
and leave lifecycle against AC's real `graph.mjs` software rasterizer. Each run
owns a Node worker, isolating the renderer's module state. Each zero-based frame
delivers its recorded inputs, advances simulation once, and paints once. Seed,
clock, viewport, and host FPS (60) are fixed. Unspecified host ink uses the
instance's seeded RNG; global time and randomness are never patched.

The five fixtures cover primitives, clipping, seeded random geometry, alpha,
persistent layer compositing, timers, and pointer state. Every frame is rendered
twice and compared byte-for-byte, including alpha, with checked-in PNGs and
SHA-256 hashes. A single changed pixel fails. The report identifies its first
coordinate and old/new RGBA; failures also save expected, actual, and difference
images. Missing or corrupted goldens, renderer warnings/errors, and unsupported
capabilities fail rather than accepting blank output. Checks never update goldens.

To deliberately record reviewed reference images:

```sh
node kidlisp/conformance/pixels.mjs record --out /tmp/kidlisp-pixels-review
```

Inspect the output and review both `pixel-golden/manifest.json` and the PNGs.
Recording runs the repeat check first. A changed fixture requires an explicit
recording; it cannot silently borrow an old expected image.

This profile is CPU 2D only. Fonts, rich text, media, network resources, embeds,
GPU effects, gradients, and live color patterns are excluded. Unsupported
values are rejected before module execution. Drawing coordinates must be finite
and within ±8192; `line` needs explicit coordinates (its host defaults are
unseeded). A replay allows at most 512 pixels per axis, 240 frames, 32 MiB of
captured RGBA, 10,000 inputs, and 15 seconds by default. Evaluator work/depth
budgets also apply. These bounds are a test-host policy, not general production
resource ownership. Golden metadata records Node and platform; cross-engine
pixel portability has not been established. The browser corpus remains a
separate integration gate.

For a deterministic reference-evaluator **command trace**, run:

```sh
node kidlisp/conformance/replay.mjs kidlisp/conformance/seeded-input.json
node --test tests/kidlisp-execution.test.mjs tests/kidlisp-plan.test.mjs
```

`commands-v1` pins seed, frame time, viewport, and pointer inputs. It records
the drawing calls made by the reference evaluator and emits their SHA-256.
Timers and pointer handlers are included; the compositor, pixels, media,
network resources, microphone, and clock-dependent color patterns are outside
this profile. Unsupported operations fail before module execution. Frames
share one work ledger with their input handlers and nested evaluation.

The numeric-plan tests compare folded and unfurled plans with the independent
reference evaluator, including changing input slots, zero division, negative
zero, NaN, and infinities. The f64 Wasm backend runs these same contracts; `%`
uses an explicit pure JS remainder import because Wasm has no f64 remainder.

The ray-tracing benchmark compares exact frames from the reference evaluator,
numeric plans, Wasm, and direct JS, and measures whole CPU frame time:

```sh
node kidlisp/tools/bench-raytrace.mjs /tmp/kidlisp-raytrace
node kidlisp/tools/build-raytrace-preview.mjs /tmp/kidlisp-raytrace/preview.html
npm run bench:kidlisp:raytrace:browser
```

Its `.lisp` kernel computes ray/sphere discriminants. The original scalar
backends keep scene traversal, shading, reflections and pixel loops in JS.
`wasm-frame` compiles a C implementation of that host plus the generated KidLisp
kernel into one bounded frame call. It uses f64, a fixed 1.125 MiB memory, three
reflection slots, and zero imports. Whole-frame tests require exact RGBA and ray
counts against JS. This includes allocation/layout changes and native loop
compilation; its speedup does not isolate the scalar Wasm boundary cost alone.

WebGPU lowers that same plan into WGSL and runs the whole scene on the GPU.
An 8×8 compute workgroup writes RGBA pixels to a persistent texture; a render
pass presents it without CPU image readback. Uniforms and pipelines persist;
resizing replaces only the texture and bind groups. Frame controls are capped
at 512 per axis and three reflections. The preview starts with WebGPU and falls
back to frame Wasm when no adapter exists, compilation fails, or the device is
lost. Leaving disposes owned GPU resources. No AI inference occurs at runtime.

The shader is an explicit f32 profile, with finite constants and only +, -, *
from the audited plan (maximum 512 instructions); other operations fail closed.
It does not claim `numeric-v1` f64 equivalence. GPU comparisons require exact
alpha, mean RGB error ≤0.5 of 255, and at most 0.2% of pixels with any channel
error >2. Reports retain changed pixels, maximum error and outlier counts.
Blank/missing output must fail this tolerance gate. CPU exact gates stay exact.

The browser harness compares six fixed frames, including partial workgroups,
then measures 12 warmups and 30 samples at each size. CPU times exclude canvas
upload; GPU wall times include submission, compute and canvas blit completion.
Optional timestamp queries measure compute separately and may quantize to zero
on short workloads. Readback for image comparisons occurs outside timed runs.
The report records browser, host and adapter. It also checks switching, resize,
pause/resume, absent-WebGPU fallback and injected device loss. The frame-rate
sample includes presentation scheduling; command submission time alone is not
a GPU performance measurement.

The preview is self-contained. Its builder also writes `ray-plan.json`, the
generated kernel, and the full shader beside the HTML for inspection. It checks
source/host/generated-kernel/Wasm hashes before embedding the binary. To rebuild
the checked-in Wasm artifact (LLVM clang and lld 22 tested):

```sh
node kidlisp/tools/build-raytrace-wasm.mjs
```

`KIDLISP_CLANG` and `KIDLISP_WASM_LD` override compiler paths; defaults use
Homebrew's versioned LLVM/lld 22. The build disables fast math and contraction.
Scene/shading hosts remain handwritten C, WGSL and JS; this benchmark does not
implement arbitrary KidLisp loops, tail calls, nested values, or a full runtime
port. It is a reflective-sphere ray tracer, not a photorealistic path tracer.

## Pinned `$roz` feedback graph

```sh
npm run preview:kidlisp:roz       # /tmp/kidlisp-roz/preview.html
npm run bench:kidlisp:roz         # builds, compares pixels, checks UI and times
node --test tests/kidlisp-roz-graph.test.mjs
```

`roz-feedback-v1` accepts only the `$roz` source pinned in `corpus.json`.
`graph/roz-plan.mjs` uses the reference evaluator with a seeded execution context
and a fixed 1/60-second simulation step. It parses once,
evaluates controls each frame, and emits up to six ordered image operations.
The host initializes the first-line gradient once and retains the body’s image
between frames. This exercises a controlled persistent layer, not the full
module lifecycle, all RBP-26 programs, or production browser integration.

`roz-assets.mjs` prepares integer sampling maps with the actual CPU spin/zoom
implementation, plus circle coverage and blend/contrast tables. WGSL executes
the same gathers and byte arithmetic without CPU image readback. This preserves
the reference’s fractional accumulation, rounding, wrapping, primitive coverage,
and differing line/circle alpha rules. Resources are reused; work is bounded to
32–512 pixels per dimension, six nodes and 10,000 evaluator steps per frame,
and one million frames per plan. Source changes require a new supported profile;
this is specialized effect lowering, not a general effect compiler.

The browser gate compares every RGBA byte for 241 frames at 33×35 (seed 1),
360 at 128×128 (seed 42), 360 at 256×256 (seed 1), and 241 at 512×512
(seed 4294967295). It crosses timer boundaries, exercises all six operations,
tests 120 further frames per size in batches of four, and checks consecutive
submissions with overlapping GPU execution. It tests reset determinism and
rejection without feedback mutation, pause, resize, absent WebGPU and device
loss. GPU fallback restarts the seeded CPU canvas; it does not retain the
failed device’s current image. Normal rendering never reads the image back.

The live host accumulates elapsed time and executes 60 simulation updates per
second even when presentation runs at 30 fps. Every intermediate feedback step
executes in source order. A submission contains at most four updates, and at
most two submissions can be queued. Catch-up after a long suspension is bounded;
the profile slows simulation under sustained overload instead of skipping image
effects. Pausing/hidden tabs discard elapsed paused time. The animation path
submits without awaiting a completion fence; diagnostics measure completion
separately. Tests require 55–65 updates/s over a two-second browser sample and
exactly 240 updates over four simulated seconds at 30, 60 and 120 fps.

Timing uses 12 warmups and 50 samples for batches of one and two updates. CPU
effects exclude compositing and upload; GPU wall time includes submission
through completed graph work and presentation at logical resolution. Readback
comparisons are separate. Control evaluation and preparation have separate
timings. Browser timer resolution can round short samples to zero. Neither GPU
completion nor an animation callback proves physical display latency. The local
report records host, raw samples, cadence, parity and fallback evidence in
`/tmp/kidlisp-roz/browser-report.json`.

The preview keeps the source image's aspect ratio and uses nearest-neighbor
scaling. It adds no geometry, camera controls, or lighting absent from `$roz`.
It does not claim a new audio backend, Wasm evaluator, or production backend
selection. There is no model inference during execution.

## Browser corpus

Renders the corpus (the top pieces by live hits, `corpus.json`) from a checkout
and compares it against another, so a KidLisp change is judged by what it draws.
This is the gate every agent PR runs; its sheets go in the PR body.

```bash
node kidlisp/conformance/oracle.mjs check                 # this checkout vs origin/main (~8 min, exit 1 = broke)
node kidlisp/conformance/oracle.mjs check --against HEAD  # vs your last commit
node kidlisp/conformance/oracle.mjs check --worker-against HEAD # same host, compare committed worker without a checkout
node kidlisp/conformance/oracle.mjs refresh --top 40      # re-pull the corpus from prod
node kidlisp/conformance/oracle.mjs render --out DIR [--repo PATH | --base URL] [--only bop,pie] [--at 1500,4000] [--video]
node kidlisp/conformance/oracle.mjs compare BEFORE AFTER --out DIR [--noise AGAIN]
```

`check` boots `lith/server.mjs` for both trees (no env or db needed; `$code`
lookups are answered from prod by the oracle), renders the base twice and the
change once, and writes `before/`, `noise/`, `after/` and `diff/` (with
`compare.md` and a before/after sheet per flagged piece).

`--worker-against` reads and verifies the committed worker directly from Git,
then selects it in the baseline passes through manifest interception. The
committed worker artifact must still exist in the checkout with the correct
hash, and each page must report that the selected worker is active. It keeps the
current host on both sides and writes that narrower scope into `scope.json`.
This avoids creating a second checkout; it does not compare BIOS or server code.

## verdicts

| verdict | meaning | fails? |
|---|---|---|
| `same` | within the piece's own noise | no |
| `drift` | moved a little past its noise | no, sheet for the eye |
| `changed` | moved a lot; may be a chaotic piece being itself | no, sheet for the eye |
| `blank` | went flat and far from before | **yes** |
| `gone` | a steady piece became a different picture | **yes** |
| `error` | a console error the base didn't throw | **yes** |

## Why the browser corpus is not exact

KidLisp seeds `?` from `Date.now()` and runs in a worker the harness can't
clock, so the same code draws different frames run to run. The oracle judges by
64-bin colour histograms against each piece's measured self-noise. A
deterministic browser mode would extend the controlled CPU profile above into
the full host. It still needs controlled resources, GPU output, and frame delivery.

## gotchas

- `kidlisp.mjs` is bundled into the disk worker. An edit renders nothing new
  until `cd system && npm run build:disk-worker`, and the bundle + manifest
  must be committed with it.
- Clocks start at `window.acBOOTED`, not navigation: boot swings by seconds
  under load. Each piece gets its own browser context (shared service worker
  breaks parallel boots) and background throttling is off.
- Local network noise (no db, no session server) is filtered out of errors.
- Servers started by the oracle always use development mode and select HTTPS
  when the checkout has local certificates. Loopback test certificates are
  allowed; production maintenance jobs stay disabled.
- Proven on 2026-09-23: identical trees pass (38 same, 2 chaotic flagged);
  a tree with `wipe` stubbed fails (4 blank, 1 gone).
