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
| Fía | KidLisp, closure compiler | 26,412 | 410 | 18 | 13 |
| Shooter | KidLisp, closure compiler | 22,380 | 383 | 12 | 11 |

The last two rows are the same sources with a `; @compile` first line
(`lib/kidlisp-compile.mjs`, later the same day): the program compiled once
into closures with names resolved to slots, the drawing calls still going
through the interpreter's own table. In Node the compiled evaluation costs
33 ms a frame for Fía and 70 ms for the shooter, against 150 and 700
interpreted; JavaScript evaluates each in about 2 ms.

Per frame the KidLisp Fía makes about 2,900 ink, 1,600 line, 730 oval and
30 shape calls; the shooter about 2,400 ink and 5,000 tri calls. The
JavaScript pieces make the same calls.

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
