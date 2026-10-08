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
```

Its `.lisp` kernel computes ray/sphere discriminants. Scene traversal, shading,
hard shadows, reflections, and the pixel loop are still JS. The self-contained
preview exposes backend, resolution, and pause controls. This is an experimental
ray tracer, not a photorealistic path tracer or a fully ported Wasm runtime.

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
