# KidLisp as the piece language

Written 2026-10-10 from the Whistlegraph corpus census
(`apple/whistlegraph/reports/piece-api-census.md`, made by
`apple/whistlegraph/tools/piece-api-census.mjs`) and the KidLisp score
(`kidlisp/SCORE.md` §8–9). The question: can KidLisp be the one description
language for AC pieces, the thing a model writes and every host runs, the
console included? The census says the gap is smaller than it looks, and it
says exactly where it is.

## 1. What pieces are, measured

52 threads, 227 versions, 38 heads, four handles. Every head parses; none
imports, none uses a browser global, none is async.

| | |
| --- | --- |
| head source | median 3,374 characters, p75 5,293, max 27,581 |
| plan level | 37 of 38 heads use only 2D primitives, screen size, events, clock, math, synth |
| hand-rolled 3D | 6 heads project their own points and draw them with `line`, `tri`, `box` (the shooter, the garden) |
| hooks | paint 79%, sim 45%, boot 39%, act 34%, leave 11%; 7 heads are paint only |
| state | median 5 module-level declarations, max 37 |
| records | 15 of 38 keep arrays of objects they push, filter and map (bullets, flowers, rings, words) |
| paint loops | depth 0: 37%, 1: 34%, 2: 29%; largest literal bound 460 |

API names destructured from the hook argument, heads then all versions:
`ink` 74/84%, `screen` 74/81%, `wipe` 68/78%, `line` 63/44%, `event` 34/21%,
`circle` 34/28%, `clock` 29/27%, `box` 24/24%, `sound` 21/23%, `oval` 18/24%,
`paintCount` 16/9%, `tri` 13/10%, `write` 13/12%, `shape` 11/6%, `text` 5/2%.
`store` appears in 31% of versions and one head. `pen`, `pens`, `hud`, `speak`,
`delta` once each.

Members: `screen.width/height` 68%, `clock.time` 26%, `clock.resync` 24%,
`sound.synth` 16%, `text.width` 5%.

Calls: `ink` 100%, `wipe` 95%, `Math.min` 92%, `line` 82%, `Math.sin` 76%,
`Math.max` 71%, `Math.cos` 47%, `circle` 45%, `Math.round` 42%, `e.is` 42%,
`Math.abs` 34%, `Math.floor` 34%, `sound.synth` 26%, `box` 24%, `oval` 24%,
`Math.random` 16%, `Math.hypot` 13%, `tri`, `write`, `shape` 11%,
`Math.exp`, `Math.pow`, `Math.ceil`, `Math.atan2` under 10%.

Events: `touch` 34%, `draw` 16%, `lift` 13%, `reframed` 11%,
`keyboard:down:space` 8%, `speech:completed`/`speech:error` once.

No `form`, no `CUBE`, no `Camera` anywhere in the corpus. The model, asked
for 3D, writes a projection in twenty lines and draws triangles. That is the
single most useful fact in the census: the whole corpus is a 2D rasterizer
plus arithmetic plus bounded state.

## 2. What KidLisp already covers

Against the evaluator (`lib/kidlisp.mjs`) and the API map:

| corpus need | KidLisp today |
| --- | --- |
| wipe, ink, line, box, circle, tri, shape, plot, write, oval | all present (`oval` included) |
| screen.width / height | `width`, `height` |
| paintCount, clock.time | `frame`, `clock` |
| sin, cos, min, max, mod, floor, round, ceil, sqrt, abs, pow | present |
| hypot, atan2, exp | missing |
| Math.random | `random` (seeded under the controlled profile) |
| touch, draw, lift | `tap`, `draw`; no lift |
| keyboard:down:space | `key` exists once in the evaluator; not a documented input form |
| gamepad | none |
| def / later / if / repeat / once | present; no `else`, no `while` |
| sound.synth voices | `melody`, `overtone`, `amplitude`, `speaker`; no sustained voice with tone, volume, attack, decay, update, kill |
| arrays of records | none; `len`, `sort`, `choose`, `range`, `repeat` over counts only |
| store | none, and a console should not have it |
| reframed | not needed; declarative sizes re-evaluate |

So of what the corpus calls, drawing is covered, timing is covered, most math
is covered, and the two real holes are **records** and **voices**. Input is a
third, smaller one.

## 3. The four additions, in order of corpus weight

### 3.1 Pools, not arrays

15 of 38 heads keep a growing list of things. In JavaScript that is
`bullets.push({...})` and `bullets = bullets.filter(alive)`. A plan cannot
have unbounded lists, and the GPU graph wants fixed layouts. The KidLisp
form is a pool with a declared capacity and named fields:

```lisp
(pool bullets 64 x y vx vy life)
(spawn bullets (x px) (y py) (vx 3) (vy 0) (life 90))
(each bullets
  (set x (+ x vx))
  (set life (- life 1))
  (if (< life 1) (kill))
  (line x y (+ x vx) y))
```

Capacity is the bound; `spawn` on a full pool reuses the oldest or no-ops
(declared per pool). `each` walks live slots; `kill` frees the current one.
Fields are numbers. This is the whole of what the corpus does with arrays:
spawn, step, cull, draw. It compiles to a struct-of-arrays in the Wasm frame
and to a storage buffer in WGSL without any translation step, and the work
ledger charges capacity, not length.

### 3.2 Voices

10 heads call `sound.synth`. The shape in the corpus is a voice started with
a type, a tone, a volume, attack and decay, then updated or killed from `sim`
or `act`. KidLisp has one-shot sound forms. Add:

```lisp
(voice hum "sine" 220 0.4 0.05 0.5)   ; name type tone volume attack decay
(tune hum 330)                        ; update tone
(hush hum)                            ; kill
```

Voices are named slots with a declared ceiling per piece, so the audio host
knows its polyphony before the first frame. `melody` and `overtone` stay.

### 3.3 Input the console has

`tap`, `draw` and a `lift` form cover 60% of event use. Add `key` as a
documented form and `pad` for the controller, both read as booleans and axes
on the frame, never as callbacks:

```lisp
(if (key "space") (spawn bullets ...))
(def steer (pad "leftx"))
```

The TV screen (`/wgtv`) and the native shell both expose a controller. On the
phone `pad` reads zero and `tap`/`draw` carry the touch.

### 3.4 Three math forms

`hypot`, `atan2`, `exp`. Pure, auditable, two lines each in the numeric
registry. `else` on `if` is the one control-flow gap the corpus hits often
enough to matter; `while` is not needed, every loop in the corpus has a
literal or state-derived bound that `repeat` expresses.

## 4. What this buys

- **One language from model to console.** The worker's edit contract becomes
  "write KidLisp"; the same source runs in the phone's runtime, the TV page,
  the feed, and the native shell through the controlled profile. Nothing is
  transpiled from JavaScript, because nothing is JavaScript.
- **Safety by construction.** No globals, no imports, no fetch, bounded pools,
  bounded voices, a work ledger, seeded random. The visual review stops being
  the only thing standing between the model and a hung tab.
- **Fast rounds.** The median head is 3,400 characters of JavaScript. The
  same piece in KidLisp is a few hundred. Edit-in-place on a small source is
  one round at a few hundred output tokens, where today's big pieces hit the
  16k cap. The review can be exact pixels on the CPU profile instead of a
  model looking at a picture.
- **Composition that is actually bounded.** `$code` inclusion already has
  depth and cycle limits and a shared ledger; pieces that use pieces inherit
  it.

## 5. What it costs

- The model has to write KidLisp well. It writes JavaScript well because it
  has read a great deal of it. The edit contract will need the language card
  (`kidlisp/README.md`) in the prompt and a corpus of good pieces; the first
  rounds will be worse than today's JavaScript rounds.
- Hand-rolled 3D stays hand-rolled. Six heads do projection in arithmetic;
  that works in KidLisp as it works in JavaScript, with `repeat` and a pool
  of faces. The console's mesh and capsule primitives are a later level, not
  a requirement.
- `store` goes. One head uses it. Persistence belongs to the thread, not the
  piece.

## 6. First steps

1. Add `hypot`, `atan2`, `exp`, `else`, `lift`, `key`, `pad` to the evaluator
   and the numeric registry, with exact tests. A day.
2. Add pools and voices to the reference evaluator under the controlled
   profile, with the ledger charging capacity. Pixel goldens for a pool
   piece. A few days.
3. Translate five heads by hand, picked across the census: a paint-only
   piece, a sim piece, a `touch` piece, a pool piece, the shooter. Measure
   source size and round count against their JavaScript. This is the test of
   whether the model can be handed the language.
4. Give the worker a KidLisp edit contract behind a per-thread flag and run
   new pieces through it, with the existing JavaScript lane untouched.
5. Then the console: the native shell's KidLisp subset grows along the
   census order (wipe, ink, line, box, circle, tri, pools, voices), with the
   TV page as the conformance peer.

Steps 1 and 2 are language work in `lib/kidlisp.mjs` and the conformance
corpus. Step 3 is the one that decides the rest.

## 7. Measured

The two biggest pieces in the corpus were translated by hand and benchmarked
against their JavaScript in the production runtime on 2026-10-10:
`kidlisp/reports/whistlegraph-benchmark-2026-10-10.md`. Size is a wash;
the reference evaluator is 12 to 30 times slower; both pieces run bounded.
A closure compiler (`lib/kidlisp-compile.mjs`, `; @compile`) brought that to
3 to 5 times the same evening, same draw stream.

## 8. The kernel subset: WebGPU-safe by construction

A piece's hot arithmetic can be declared as a kernel, the way Common Lisp
lets a function drop to the operator level or inline assembly. A kernel is
a pure function over numbers with declared inputs, uniforms and outputs,
and a body of `def` locals and `set` outputs over kernel-v1: the audited
arithmetic, the functions a shader has (`sin cos tan sqrt abs min max exp
pow sign atan2 hypot clamp`), comparisons as 0/1, and `if … else …` as a
select. No calls, no drawing, no pools, no clocks, no random. Anything
else fails at compile time, so what compiles is safe to lower.

```lisp
(kernel project (in hx hy) (uniform ssc zz cT sT cP sP foc hcx hcy) (out px py)
  (def xr (+ (* hx ssc cT) (* zz sT)))
  (def z0 (- (* zz cT) (* hx ssc sT)))
  (def ff (/ foc (- foc (+ (* hy ssc sP) (* z0 cP)))))
  (set px (+ hcx (* xr ff)))
  (set py (+ hcy (* (- (* hy ssc cP) (* z0 sP)) ff))))
(run project hp sp)
```

`run` applies the kernel to every live slot of a pool: inputs are the
slot's fields, uniforms are the program's globals, outputs are fields of the
same pool or of a second one with the same slots. That is a compute
dispatch: the pool is the storage buffer, the globals the uniform buffer,
one invocation per row.

One plan, three backends (`lib/kidlisp-kernel.mjs`):

| backend | what it is | measured, 100,000 rows of the projection above |
| --- | --- | --- |
| JavaScript | the reference stack runner | 497 ms |
| Wasm | one function, a loop over rows in linear memory, transcendental functions imported from the host | 5.9 ms |
| WGSL | a compute shader emitted from the same plan, f32 | compiles clean and agrees with the CPU to f32 on the Apple adapter (`kidlisp/tools/check-kernel-wgsl.mjs`) |

The Wasm and JavaScript numbers agree bit for bit on every operation
(`tests/kidlisp-kernel.test.mjs`). WGSL is f32, so its profile is tolerant,
as the raytrace backends are. A host without WebAssembly runs the
JavaScript; a host without a GPU runs the Wasm; the piece does not change.

`(gpu kernel from [to])` is the same map dispatched to the device as a
compute pass: the results land in the pool on the next frame that calls
it, one frame late by design, which the source says by using `gpu` and not
`run`. Without WebGPU, or while the device warms up, `gpu` is `run`. A
readback costs about a millisecond regardless of size, so `gpu` pays only
for pools in the thousands; `run` on Wasm is right for a hundred points.

What this is not: the whole program. Control flow over many small
polygons, the painter's order, the near-plane clipper with its variable
output stay in the closure compiler. Kernels take the parts that are
already maps over data, which the census says is most of the arithmetic in
the two big pieces: projection, star fields, the skirt's bands, the
soldier's boxes.
