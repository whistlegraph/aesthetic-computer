# Two Whistlegraph pieces, JavaScript against KidLisp

Measured 2026-10-10 in the production runtime, headless Chrome, with
`kidlisp/tools/bench-pieces.mjs` (fps from bios's own frame counter over
8 s at each size, after 3 s to settle). Both languages run through the same
renderer and the same loop; only the piece language differs. The KidLisp
versions are hand translations in `kidlisp/examples/whistlegraph/`, same
drawing calls, closures turned into global transform state, lists turned
into pools.

| piece | language | chars | lines | fps 390×520 | fps 1280×720 |
| --- | --- | --- | --- | --- | --- |
| Fía (wgDuram v18) | JavaScript | 27,581 | 513 | 60 | 30 |
| Fía | KidLisp | 26,396 | 409 | 5 | 5 |
| Shooter (wgNirin v7) | JavaScript | 18,586 | 385 | 60 | 56 |
| Shooter | KidLisp | 22,369 | 382 | 2 | 2 |
| Fía | KidLisp, closure compiler v1 | 26,412 | 410 | 18 | 13 |
| Shooter | KidLisp, closure compiler v1 | 22,380 | 383 | 12 | 11 |
| Fía | closure compiler v2 | 26,412 | 410 | 24 | 16 |
| Shooter | closure compiler v2 | 22,380 | 383 | 20 | 20 |
| Fía | compiler v3 + the heart projection as a Wasm kernel | 26,921 | 420 | 28 | 16 |
| Fía | … and no HUD colouring for the hidden label | 26,921 | 420 | 31 | 20 |
| Shooter | compiler v3, no HUD colouring | 22,380 | 383 | 22 | 22 |
| Fía | … and the frame drawn by WebGPU (`; @gpu`) | 26,928 | 421 | 36 | 21 |
| Shooter | … and the frame drawn by WebGPU (`; @gpu`) | 22,387 | 384 | 25 | 24 |

The compiler rows are the same sources with a `; @compile` first line
(`lib/kidlisp-compile.mjs`, later the same day): the program compiled once
into closures with names resolved to slots, the drawing calls still going
through the interpreter's own table. In Node the compiled evaluation costs
33 ms a frame for Fía and 70 ms for the shooter, against 150 and 700
interpreted; JavaScript evaluates each in about 2 ms.

Per frame the KidLisp Fía makes about 2,900 ink, 1,600 line, 730 oval and
30 shape calls; the shooter about 2,400 ink and 5,000 tri calls. The
JavaScript pieces make the same calls.

The kernel row (`lib/kidlisp-kernel.mjs`, PIECE-IL.md §8) moves the heart's
per-point projection into a declared kernel the compiler runs as a Wasm
loop over the pool; the same plan emits WGSL that compiles clean and agrees
with the CPU on the Apple adapter. At 1280×720 Fía is bound by
rasterization, not evaluation: the JavaScript version runs 30 there too.

A worker CPU profile of the compiled Fía piece at phone size put about a
quarter of the frame in the software rasterizer (plot, fillShape) and a
fifth in colouring the source's tokens for a corner label that nolabel had
hidden; the compiled closures themselves barely registered. The colouring
is now skipped for a hidden label (339d671608). The star field in
`kidlisp/examples/kernels/` shows the `gpu` form losing to `run` on this
runtime (35 against 60 fps at phone size): a compute pass has to read its
results back for a CPU rasterizer, and that costs more than the arithmetic.

The `; @gpu` rows (8754e56644, PIECE-IL.md §9): the compiled program
records its drawing into one buffer a frame (`lib/gpu-frame.mjs`) and bios
draws it with WebGPU as one vertex buffer and one draw
(`lib/gpu-frame-renderer.mjs`), the CPU buffer composited on top for text.
The frames match the CPU path's picture. What remains is evaluation and the
runtime's per-frame work around it; the rasterizer is off the clock.

## What it says

- **Size is a wash.** Prefix arithmetic costs what closures saved. A line
  of `(+ gx (* sd 2.2 uu) sx)` is no shorter than `gx + sd * 2.2 * u + sx`.
  The language wins on size only where it has a form the piece lacked: the
  pools replaced forty lines of push/filter/sort with four.
- **The reference evaluator is 12 to 30 times slower than compiled
  JavaScript** on these pieces. It re-evaluates the whole program every
  paint, copies an environment on every function call, and dispatches
  every arithmetic node through a string lookup. That is the gap the
  compiled path has to close; it is not a property of the language.
- **The pieces run.** No browser global, no import, nothing the controlled
  profile could not bound. The shooter's whole 3D pipeline fits in pools of
  16 and 32 points.

## What the translation taught the evaluator

Four faults fixed on the way (commits 91b9151bd1, e943697da8): a `repeat`
body form that did not name the iterator was hoisted and run once even
when it drew or spawned; `if` returned a flag instead of its branch's
value; `shape` could not take a pool; and the function-call path logged to
the console on every call. Rules the parser imposes: a line may not begin
with `else`; names may not contain a hyphen; single-letter names collide
with notes and colors; `>=` does not exist; `random` returns integers;
`quad` is a built-in.

## Next

The closure compiler closed the gap from 12 to 30 times down to 3 to 5.
What remains is per-call allocation in the closures, the interpreter's
drawing wrappers (fill mode, colour parsing, random fills on every call),
and property writes for globals. After that, the plan: compile a piece's
arithmetic and pools to the numeric plan and the Wasm frame
(`lib/kidlisp-plan.mjs`, `kidlisp/benchmarks/raytrace-frame.c`), keep the
drawing calls as a command list the host rasterizes, and measure these two
pieces again. 60 fps at 1280×720 for the shooter is the bar.

## The native shells (evening)

Same bundle, same pieces, run by the native bios lifecycle (PIECE-IL.md §10).

| per frame | Xbox Series X (QuickJS-ng, no JIT) | Mac shell (JavaScriptCore, JIT) |
|---|---|---|
| shooter | 1,200 ms | 15 ms (28 to 42 fps) |
| Fía | 1,010 ms | 28 ms (25 fps) |
| starfield | 645 ms | 10 ms (54 to 60 fps) |
| frame buffer → host calls | 0 to 23 ms | 2 to 14 ms |
| 10 million sin-multiply-adds | 1,909 ms | 308 ms in local QuickJS, 482 in Node |

The Mac shell re-signed without the JIT entitlement ran the same script at
130 to 230 ms a frame: the entitlement is the difference, not the engine.
