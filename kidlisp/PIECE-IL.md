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

## 9. The frame as one buffer: drawing on the GPU

A compiled piece that says `; @gpu` does not call the software rasterizer.
Its drawing heads record into a frame buffer (`lib/gpu-frame.mjs`): clear,
line, box, oval, tri and shape, each with its colour, one Float32Array a
frame, handed to bios in a single transferred message. bios tessellates it
to triangles with per-vertex colour and draws it with WebGPU in one pass on
its own canvas (`lib/gpu-frame-renderer.mjs`), then composites the CPU
buffer over it as a texture when the frame asks, so `write` and anything
the path does not take still show. `ink` keeps running on the CPU for its
colour parsing and the resolved colour is read back; `wipe` clears both.
The worker probes bios once; without a renderer the piece draws on the CPU
as before, same stream.

Measured 2026-10-11, phone size / 1280×720: Fía 36 / 21 fps from 31 / 20
on the CPU path; the shooter 25 / 24 from 22 / 22. The pictures match. The
format is the contract: a native host can consume the same buffer.

What is left after this is evaluation and the runtime's own per-frame
work. Kernels (§8) take the data-parallel arithmetic; the whole program to
Wasm takes the rest, and a host that keeps kernel outputs on the GPU and
draws from them never pays a readback at all.


## 10. The console: the same bundle in the native bios

Measured 2026-10-10. `node kidlisp/tools/native-tv-script.mjs` bundles the
evaluator, the closure compiler and the frame builder (esbuild, one classic
script, global `KidLispNative`), embeds the pieces, and adds the
boot/sim/paint/act lifecycle the native bios shells call, with a shim that
walks each frame buffer (§9) into the host's `wipe`, `box`, `line` and
`triangle`. The same file runs in the Mac shell (JavaScriptCore, JIT) and
on the Xbox devkit through the live-piece lane the bios already has
(`node xbox/tools/live.mjs hot-deploy kidlisp/build/kidlisp-tv.js`, no
msix rebuild: the host reloads `LocalState/live-piece.js` and logs
`AC_NATIVE_LIVE_READY`). Nothing in the evaluator needed changing for
either engine.

| per frame | Xbox Series X, QuickJS-ng, no JIT | Mac shell, JavaScriptCore, JIT |
|---|---|---|
| shooter (7,100 triangles) | 1,200 ms | 15 ms |
| Fía (2,250 lines, ovals) | 1,010 ms | 28 ms |
| starfield (4,000 boxes, one kernel) | 645 ms | 10 ms |
| walking the frame buffer into host calls | 0 to 23 ms | 2 to 14 ms |
| 10 million `sin`-multiply-adds | 1,909 ms | 308 ms (local QuickJS) |

The drawing contract holds: ten thousand boxes cost the Xbox 3 ms. The
cost is the evaluation, and the reason is the engine, not the pieces:
the console's QuickJS is six times slower than QuickJS on an M-series
Mac and has no JIT, so the compiled closures that give 25 to 60 fps
under a JIT give one frame a second there. Oskiewar reaches 60 fps on
the same engine only because its JavaScript is written tight by hand.

So on the console the piece has to be compiled below JavaScript. The
closure compiler already lowers a piece to typed arithmetic, slots and
pools (§7), and the kernel subset already emits Wasm and WGSL from one
plan (§8); the next emitter is C, from the same lowered forms, compiled
into the native bios by AppVeyor (`appveyor.yml` builds the msix on any
commit touching `xbox/native-bios/`). That is the "msix like oskiewar"
route: a KidLisp console package whose pieces are C, with the frame
buffer as the only drawing interface and the Device Portal install
(`xbox/tools/live.mjs install`) as the installer. The JavaScript bundle
stays the development path: hot-deploy a piece in seconds, read its
`KIDLISP` telemetry lines, then compile it when it is done.

Open on the Xbox: the frame's CLEAR maps to the host `wipe`, which oskiewar
uses for its dark sky, yet the screen comes up white with the post shader's
vignette; and the shooter's 7,100 filled triangles are counted but not
seen, where the Mac shell draws them (the host's `triangle` puts every
vertex at z = 0 under a depth buffer). Both are host-side questions for
the C++ lane, not evaluator faults.

### 10.1 Later the same night: parity, and the program as source

Three host-side faults explained every missing pixel on the Xbox, none of
them in the evaluator. A live post shader from an earlier session was
still applied (`node xbox/tools/live.mjs shader-reset`); the host draws
its box layer over the GPU triangles, so a filled box larger than a dot is
now two triangles in the shim; and the host refuses any coordinate beyond
±32768 with an exception that drops the rest of the frame, so the shim
clamps (a projected vertex can be far off screen). With those, all three
pieces draw on the console as they draw in the browser and in the Mac
shell.

For speed inside the engine, the plan is now emitted as text
(`lib/kidlisp-emit.mjs`): the closure compiler's scopes, slots and guards,
written out as one JavaScript function with locals, so QuickJS runs
bytecode instead of a call per node. It is a trusted-host step: the
native script tool emits each piece on the Mac and embeds the result; the
worker never evaluates generated source (`KIDLISP_HOST_SOURCE` gates the
kernel's JS backend the same way). `node kidlisp/tools/check-emit.mjs
piece.lisp` runs the emitted program against the closure compiler with
the evaluator's random seeded and compares the frame buffers number for
number: all three pieces are the same over 20 frames. Kernels get the
same treatment (`instantiateKernelJS`), which took the starfield from 645
to 163 ms a frame on the Xbox before the program emitter.

Dynamic resolution is in the shim (`density`, auto by default: the piece's
screen is a fraction of the host's and draws are scaled up, moving toward a
16 ms frame, floor one quarter). It helps a piece whose work scales with
the screen (Fía's star count follows width; its frame fell from 1,010 to
250 ms at a quarter) and does nothing for one whose work is per record
(the shooter clips and projects 3,500 quads a frame at any size).

`node xbox/tools/kidlisp-bench.mjs [seconds] [density|auto]` builds,
publishes, waits, screenshots each piece through the Device Portal and
tabulates the telemetry.

Measured on the Xbox at the end of the night, half density, the program
emitted as source, next to where the evening started:

| per frame on the Xbox | first run, full density | emitted, half density |
|---|---|---|
| shooter | 1,200 ms | 579 ms |
| Fía | 1,010 ms | 178 ms |
| starfield | 645 ms | 124 ms |

Two to six times faster, and still two to eight frames a second. The
shooter's profile is its own polygon clipper: four camera calls, four
spawns and a clip per quad, 3,500 quads a frame, which no interpreter
without a JIT does in 16 ms. The paths to 60 on the console are the ones
§10 names: the piece compiled to C into the native bios package, or, for
a 3D piece, the host's own meshes (`meshUpload`, `meshDraw`,
`triangle3d`) so the projection leaves JavaScript altogether.

## 11. The 3D layer: the language never touches a vertex

The shooter is slow on the console because the piece does the work of a
renderer in the language: camera, clip, spawn, fan, 3,500 times a frame.
The layer that fixes it is the one every engine has: geometry described
once, a camera and placements per frame, projection in the host. On a
machine with a JIT this is a courtesy; on QuickJS without one it is the
difference between one frame a second and sixty.

**Forms.** `(mesh name (cube w h d r g b) (cube x y z w h d r g b)
(face x1 y1 z1 … x4 y4 z4 r g b) (tri … r g b))` builds geometry once,
numbers only, as the Xbox's own layout (vertices; ten floats a face: four
indices, a colour, a normal). `(camera x y z [yaw pitch fov near])` sets
the frame's eye in the piece's pixels. `(place name x y z [yaw pitch roll
scale])` draws a mesh at a pose. In the frame buffer (§9) these are
`CAMERA` and `PLACE` ops; new meshes ride once with the frame that first
uses them. `lib/kidlisp-mesh.mjs` holds the builder, the camera (the
Xbox's 27 floats: position, three view rows, centre, focal length,
perspective, near, viewport, depth base and slope, light) and the
projector.

**Hosts.** The Xbox draws placements with its retained meshes
(`meshUpload` once per pose, `meshDraw` per frame, depth-buffered). The
browser's WebGPU renderer and the Mac shell run the projector in
JavaScript, a port of the Xbox's SceneMesh, so the pictures agree: view
transform, near-plane clip in view space, projection, a guard band in
screen space so nothing projected from just in front of the eye reaches a
host as a coordinate it refuses, lighting by the face normal, back faces
dropped, and every face of the frame sorted once, far to near, across all
placements, so a host without a depth buffer paints in order between
objects as well as within them. The interpreter draws the same faces with
`tri` on the CPU, so a piece runs everywhere, slower without the GPU path.

**Measured.** `kidlisp/examples/meshes/corridor.lisp`: a brick corridor
with pillars, crates and turning guards, the shooter's scene as meshes.
1,352 placements a frame, about 11,000 lit faces.

| per frame | Xbox Series X, QuickJS, no JIT, 1080p | Mac shell, JavaScriptCore |
|---|---|---|
| corridor | 18 ms, 56 fps | 1.1 ms, 60 fps |
| the shooter, same scene drawn by the piece (§10) | 579 ms | 15 ms |

Thirty times faster on the console, from describing instead of drawing.
The rest of the frame on the Xbox is the bridge: 1,352 `meshDraw` calls
at about 7 µs each. Two instancing calls would make that nothing.

**Limits learned.** The Xbox's triangle path takes 8,192 triangles a
frame and its mesh path does not cull back faces, so a wall of cubes is
dropped past the cap; describe the face you see (the corridor's bricks are
single faces, 2 triangles each). The host's mesh store holds 4,096 meshes
and outlives a hot reload, so a benchmark restarts the app. A mesh whose
pose changes every frame goes through the projector rather than an upload
a frame. `def` inside a loop defines once; use `now`.

### 11.1 Light, depth, and the same picture on three hosts

One light model, `lightFor` in `lib/kidlisp-mesh.mjs`: ambient 0.34, diffuse
from one sun, a little bounce from below so undersides are not black. The
projector applies it in the browser and the Mac shell. The Xbox lights its
own meshes with a fixed formula, so the native shim hands it pre-lit
colours with its sun set to zero, divided by what it then multiplies by;
the pose cache keys on the sun too. `(light x y z)` sets the sun's
direction (the way it shines) and rides in the frame as `LIGHT`; the
default shines down, a little left and back, for a y-up world.

The browser renderer has a depth buffer now: the vertex carries z, placed
faces land in (0, 1] from the Xbox's depth range, 2D ops stay at 0 and are
always in front, and the CPU overlay ignores depth. The painter's sort
remains for hosts without depth. The Mac shell draws through `triangle3d`
with the same depth values into its own depth test.

Near-field clipping is the piece's: the corridor's near plane is 6 units
and the eye is kept out of pillars and crates by the piece. No collision is
in the layer.

Measured: the lit corridor at 59 fps in the Mac shell (2.9 to 4.5 ms of
JavaScript a frame); the browser number is in the report.
