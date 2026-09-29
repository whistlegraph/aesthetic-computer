# Oskiewar objects in KidLisp

Props, vehicles, weapons and hats are small KidLisp programs. You write one
in Aesel (a `.lisp` file, the same KidLisp runtime), try it in the object lab,
and then seal it into the game. The same compiled object runs on every
oskiewar host. All it sends is frame ops (`OSKIEWAR-HOSTS.md` §3.0): WORLD
faces, which every interpreter already reads, and baked parts by handle —
lit meshes (MODEL) or, in the flat style @jeffrey chose to try, flat
sketches (SKETCH).

**Forms compile away; hosts see faces, meshes and flat shapes.** Everything
in this dialect, from `revolve`, `radial`, `mirror`, `toward` and `slab` to
level of detail, is expanded once, by `object-lisp.mjs`, when the object
loads. No interpreter ever expands a form. Each host (JS web, C++ Xbox, and
whatever comes next) implements only WORLD, ASSET/MODEL and SHAPES/SKETCH
(plus ELLIPSE, PLATE and OUTLINE for flat shapes that move per tick). What a
host does with them is project, fill, light or ink, and pick sides or a
level. That rule keeps `FrameVm.cpp` small and keeps the hosts from drifting
apart.

Written 2026-09-28. Status:

| piece | where | state |
|---|---|---|
| compiler | `xbox/live/object-lisp.mjs` | done; tests in `xbox/live/tests/object-lisp.test.mjs` |
| MODEL op | `xbox/live/frame-vm.mjs` (op 14) | done on the web; test in `tests/frame-vm.test.mjs`; the game's op table and `FrameVm.cpp` (R6) still need it |
| flat ops | `frame-vm.mjs` ops 15–19: ELLIPSE, PLATE, OUTLINE, SHAPES, SKETCH | done on the web; tests in `tests/frame-vm.test.mjs`; the game's op table and `FrameVm.cpp` still need them |
| first object, lit | `xbox/live/objects/monowheel.lisp` | done: tire wedges, rim, web, 5 spokes rolling with distance, lean, landing squash, turbo, lamps |
| first object, flat | `xbox/live/objects/monowheel-flat.lisp` | done: inked tire, rim, web, spoke bars that roll, drawn-box deck, lamp dots, ground shadow, lean, landing squash, turbo |
| lab | `xbox/live/object-lab.html` (`npm run xbox:play`, then `/object-lab.html`) | done: lit left, flat right, one set of sliders; each pane shows its tick's cost |
| in the game | `xbox/tools/embed-objects.mjs` + `drawMonowheel` | **done 2026-09-28**: the flat monowheel, 42 numbers a tick (the quad wheel sent 1144); see below |

## The budget

This rule holds for every object, and `object-lisp.test.mjs` fails the
monowheel if it breaks it:

- **Flat (the target):** a tick sends **at most 60 numbers**, and at every
  distance the host draws **fewer triangles than the lit version**.
- **Lit:** a tick sends at most 150 numbers, of which at most 8 are moving
  WORLD faces, and the host draws at most 150 / 100 / 80 triangles at
  levels 0 / 1 / 2.
- Baked meshes and sketches go up once, as ASSET and SHAPES, when the object
  loads.

Measured on the monowheel. The old row comes from the game's own frame
program. Distances are the lab camera's (220 is a close-up, and 600 to 1500
spans gameplay). µs are Node on a Mac at load average ~400, so compare them
only with each other:

| | a tick sends | host triangles at 220 / 600 / 1500 / 4000 | JS per tick |
|---|---|---|---|
| the game's flat monowheel today | 88 WORLD = **1144 numbers** | 88 at every distance | — |
| first object slice (all faces) | 260 WORLD = 3380 numbers | 260 | ~150 µs |
| lit, baked | 2 MODEL + 4 WORLD = **92 numbers** | 148 / 96 / 92 / 80 | ~12–20 µs |
| flat, projected in JS every tick | ~17 flat ops = 168 numbers | 134 / 85 / 55 / 47 | ~35 µs |
| **flat, baked** | **3 SKETCH = 42 numbers** | **~125 / 84 / 55 / 33** | **~3–7 µs** |

- **Lit:** the 4 WORLD faces are the lamps, whose brightness follows speed.
  10 meshes are uploaded once: the wheel at 3 levels × 2 turbo values, and
  the decks at 2 distinct levels × 2.
- **Flat:** 5 sketches are uploaded once: the shadow, the wheel × 2 turbo,
  and the deck × 2 turbo.

## Why the compiler does it all

The KidLisp tree-walker is too slow for per-frame work on the console: a
61-line crawl repainted about once every 6 s on the oven. `compile(source)`
walks the tree once:

1. **Constants fold.** Transform nesting depth is checked, and every
   expression learns which slots it reads.
2. **Static runs bake.** A run of statements whose reads are all constants,
   switches, `detail`, or names bound inside the run is a *part*. Its entry ink
   must also be known. Each part is run once at compile time, in its own frame,
   for each switch value (`turbo` 0/1) and each level (`detail` 0/1/2), and the
   faces are recorded into meshes. Twin meshes share a handle, and a part
   inside a part is baked into its parent.
3. **What moves stays closures.** Each tick, a part sends one MODEL (its mesh
   handles, one per level, under the part's current 13-number frame). Anything
   that reads a per-tick input sends WORLD faces.

A rotating part whose inside is static, like the wheel under
`(rotate z (- roll))`, is exactly this: a static mesh under a moving frame.

KidLisp itself is untouched. The dialect reads with KidLisp's rules (bare
lines, commas, `;` comments, auto-closing parens), and a test holds the
reader to `kidlisp.mjs`'s `parse` on every object.

## The dialect

```lisp
def r 24                                   ; binds once, at compile time
(let roll (/ distance r))                  ; re-evaluated every tick
(rotate z (- roll)                         ; a moving frame…
  (let blocks (- 12 (* 4 detail)))         ; …over a baked part: 12/8/4 wedges by level
  (radial z blocks k
    (ink (mix 58 24 (% k 2)) (mix 58 25 (% k 2)) (mix 66 30 (% k 2)))
    (revolve z (/ 1 blocks)  0 -13  r -13  r 13  0 13))
  (mirror z (move 0 0 13 (disc 17))))
```

**Object space:** x forward, y up, z to the owner's right. Units are the
game's.

**Inputs** are bare names, read fresh every tick: `time` (s), `distance`
(signed, rolled world units), `speed` (units/s), `lean`, `heading`, `pitch`
(radians), `hit` and `land` (seconds since the event; very large if it never
happened). `(owner part x|y|z)` reads one of the owner's pose points, which
the game hands over in object space. Heading and pitch are already in the
placement; they are passed as inputs for objects that react to them.

- **Switches:** `turbo` is 0 or 1. A part that reads it is baked once per
  value, and a tick picks the mesh.
- **Levels:** `detail` is 0 near, 1 middle, 2 far. A part reads it to bake
  fewer faces at each level. The host picks the level. Outside baked parts
  `detail` reads 0.

**Binding:** `def` binds once, and a `def` that reads an input is a compile
error that points you to `let`. `let` binds for the rest of its body and is
re-evaluated every tick. A `let` that reads nothing that moves can sit inside
a baked part. `repeat n i body…` loops. `if test body…` has no else, as in
KidLisp. Every value is a number, and a comparison is 1 or 0, so choices are
arithmetic: `(mix a b (% i 2))`.

**Math:** `+ - * / %` (`%` is always positive), `min max abs sign sin cos tan
atan sqrt pow floor round clamp mix`, `= < > <= >= and or not`, and `pi tau`.

**Structure** (all compile-time):

| form | means |
|---|---|
| `(move x y z body…)` · `(rotate x\|y\|z a body…)` · `(scale k body…)` / `(scale x y z body…)` | a new frame for the body; a negative scale mirrors and faces stay facing out |
| `(radial x\|y\|z n [k] body…)` | the body n times, turned evenly about the axis; `k` counts |
| `(mirror x\|y\|z body…)` | the body, then its reflection across that axis |

**Color:** `(ink r g b)` or `(ink name)` stays in effect until the next ink.
Faces are lit by the game's flat rule in world space, `.72 + .28 · max(0,
facing the sun)`, using the same sun as `worldQuad`/`litQuadColor`. A test
lifts `litQuadColor` out of `oskiewar.js` and compares against it. `(glow
body…)` leaves faces unlit.

**Shapes** (object space, current frame). Sides are picked by radius and
level unless you give them. Level is the only thing that changes a shape.

| form | draws |
|---|---|
| `(tri x y z ×3)` · `(quad x y z ×4)` | one face; two faces lit as one (as `worldQuad`) |
| `(revolve x\|y\|z [turn] r h r h …)` | a closed profile (radius, height) turned about the axis. It is oriented for you, so faces face out. A corner on the axis caps. `turn` (0–1) sweeps part of a circle |
| `(disc r [sides])` · `(hoop inner outer [sides])` | flat, facing +z |
| `(band r width [sides] [turn])` | a tube wall around z |
| `(capsule x1 y1 z1 x2 y2 z2 w [sides])` · `(line … [w])` | a rod as a prism; a thin three-sided rod |

Not in this pass: `extrude` (a 2D outline, which would do the decks) and
`sweep`/`tube` along a path (rails, handlebars). They would bake the same way.

## MODEL, the one new op

```
14 MODEL  radius · handle0 handle1 handle2 · origin(3) x(3) y(3) z(3) · light(3)   20 numbers
```

- **The mesh** is an ASSET (op 12, unchanged): vertices, and per face four
  ids (a triangle repeats its third), unlit rgb, and a unit normal.
- **Placement:** a vertex goes to `origin + x·X + y·Y + z·Z`.
- **Normals** go through the cofactor matrix (columns Y×Z, Z×X, X×Y), times
  the sign of the determinant. That is exact under any rotation, scale, shear
  or mirror.
- **Mirrors:** under a mirror (negative determinant) the winding is swapped,
  so a face comes out in the same order it would as WORLD faces.
- **Light:** a face lights `.72 + .28 · max(0, −n·light)`, as MESH does,
  except that a zero normal is unlit (`glow`).
- **Level:** the host projects `origin` through the current CAMERA and takes
  `radius × lerp(orthoScale, focal / depth, perspective)`. At 56 px or more it
  draws handle0, from 20 px it draws handle1, and below that handle2. At
  gameplay widths (700–3400 across the stage) the monowheel spans all three.

That is all a host does: transform, light and pick. It never tessellates.

**Shading is exact.** The object's own WORLD faces take their normal from the
world-space winding. A baked face takes the cofactor-transformed normal of its
object-space winding, and for any affine map `(Ma)×(Mb) = cof(M)(a×b)`, so the
two agree. The only differences are Float32 rounding of the matrix and
vertices: under 0.02 units, and a lit channel occasionally 1 off at a
rounding edge. "Baked meshes draw what the faces would have" tests that across
both switch values, lean and squash.

## How the game runs one

```js
const monowheelObject = compile(monowheelSource, "monowheel");   // once, at load
// upload monowheelObject.meshes[i] as ASSET handle base + i, once
monowheelObject(inputs, place, { face: emitWorldFace, model: emitModel });
```

- **`place`** is twelve numbers: the world origin, then where object x, y and
  z point in world space.
- **`out.face`** has `emitWorldFace`'s signature.
- **`out.model(radius, h0, h1, h2, frame, at)`** gets the object's own mesh
  indices (the game adds its handle base) and the 12 numbers at
  `frame[at…]`.
- **Faces only:** passing a single function instead draws everything as WORLD
  faces at level 0. That is the path the tests use to check the baking.

## The flat style

@jeffrey: "wishes we sort of drew everything in 2d even though the game is in
3d." The world and gameplay stay 3D. Drawing is world-anchored 2D: an object
names anchor points in its own space, the host projects them through the
frame's CAMERA, and it fills flat 2D shapes at the projected size.

- **Style:** each shape is a flat fill with an ink edge and no lighting
  gradient. Each shape carries one flat depth, so near shapes cover far ones,
  and objects keep a ground shadow.
- **Forms:** `(ball x y z r)`, `(limb a b r)` (a stadium), `(ring axis r)`
  (a circle drawn as its projected ellipse), `(drum axis r width)` (a
  cylinder), `(stroke w …)`, `(plate …)` and `(slab x1 y1 z1 x2 y2 z2)`.
- **Scopes:** `(outline w [r g b] …)` gives the shapes inside an ink edge `w`
  world units wide. `(nudge d …)` pushes shapes `d` world units back to settle
  ties. `(toward axis …)` shows the side of that axis that faces the camera,
  which is how a wheel shows the face you can see.
- **Baking:** a run of flat shapes that reads nothing per tick bakes into a
  sketch (op 18 SHAPES), exactly as lit faces bake into meshes. Each tick
  sends one 14-number SKETCH (a handle and the part's placement). The host:
  - projects each shape's anchors;
  - skips a one-sided shape turned away from it (that is what `toward` and
    `slab` bake into);
  - picks an ellipse's side count from its projected size, keeping chords
    within 2 px of the curve (4–24 sides);
  - draws the ink edge first, a hair behind, and leaves it off under half a
    pixel;
  - skips anything under a pixel.
- **Moving shapes:** a flat shape that does read a per-tick input is
  projected in JS and sent as ELLIPSE / DISC / CAPSULE / PLATE, with OUTLINE
  sent only when the edge changes.
- **Drum:** it draws the far end's outer half, the band between the ends'
  tangent points, and the near end, each at its own depth. The far end's
  inner half is never drawn, because the band covers it.
- **Slab:** it bakes as six one-sided faces, so the host draws the three it
  can see, each inked. It reads as a drawn box.

**What it costs to keep it cheap.** Ink doubles what it outlines, so only
silhouettes carry it (the tire and the deck). Details sit on fills that
already contrast. Spokes are flat bars, two triangles each, where a round
end costs eight. These are authoring rules, not engine limits.

**Depth reads.** In the lab shots, the near cap covers the band and the band
covers the far cap. The deck sits behind the tire, and the tire pokes through
it. The front lamp hides behind the deck end at ¾. Seen from the left, the
wheel shows its other face. The shadow lies under everything. What's lost is
shading inside a surface: a cylinder reads by its silhouette and ink, not a
gradient.

## Figures

§7 decision 0 option (b) re-expresses figures as world-space primitives. The
same compiler fits their rigid parts. A forearm, a shoe or a hat is a static
mesh under a moving bone frame (one MODEL per bone), and a head is a
`revolve`. That is about 13 MODEL ops (~260 numbers) a figure, where today's
figures together send ~1700 screen-space ops a frame. Parts that bend
(curved limbs, skirts, hair) are not rigid. They would stay per-tick faces or
need skinning, and that is a separate decision. With this route, FIGURE is not
needed as an op, and the figure's forms, like the objects', compile away in
one place.

**Option (c), world-anchored flat shapes,** is the flat style applied to
figures, and it suits them better than (b):
- **Parts:** a figure is balls (head, hands, joints) and limbs (stadiums
  between joints), with a torso as a plate or a drum, all inked.
- **Bending:** bending parts stop being a problem, because a limb is two
  anchors and a width, projected every tick.
- **Cost:** the pose is the per-tick data, about 12 joints. A figure needs no
  mesh: it is a sketch whose anchors are joint slots rather than fixed
  points. The host already projects anchors and fills stadiums and discs, so
  the only addition is a record that takes its anchor from the pose (a
  joint index) instead of the sketch.
- **Per-figure numbers:** about 12 joints × 3 plus a style handle, roughly
  40–50 numbers, against today's ~1700 screen-space ops for all figures.
- **Face and hair:** flat plates and strokes anchored to the head.

## In the game

Since 2026-09-28 the game draws the monowheel as `objects/monowheel-flat.lisp`.

- **The embed.** `xbox/tools/embed-objects.mjs` seals the compiler and the
  objects the game draws (`objects` in the tool names them; today only
  `monowheel: "monowheel-flat"`) into oskiewar.js, between `// <objects>` and
  `// </objects>`, just before `drawMonowheel`. It exposes `gameObjects`,
  whose objects are compiled and baked when the script loads.
  - `node xbox/tools/embed-objects.mjs` rewrites the block, and `--check`
    fails on drift.
  - The last test in `object-lisp.test.mjs` asserts the block equals
    `generate()`.
  - Edit an object or the compiler, try it in the lab, rerun the tool.
- **The ops the game sends:** SHAPES (18) once per baked sketch, then SKETCH
  (19) per part per tick (`emitSketch`). Nothing else is new in the game's op
  table; MODEL, ELLIPSE, PLATE and OUTLINE aren't sent.
- **Hosts without `frame`** (the console before R6, the harness) run the
  object's per-tick path instead (`immediateObjectOut`). It projects through
  the camera doll and fills flat triangles through `screenTriangle`: the
  same shapes, with ink edges, scissored to the inset like everything else.
  `frame-conformance.test.mjs` holds the two paths to the same screen.
- **The call site:** `drawMonowheel(p)` draws the object (reached from
  `drawSkateboard` for a rider and from the renderables loop for the parked
  wheel), placed on the rig the old wheel used.
  - The old flat-quad wheel is `drawMonowheelQuads`, behind
    `globalThis.oskiewarOldMonowheel = true`.
  - **Inputs:** `distance` is the skate odometer (`skateSpin` ×
    `skateWheelRadius`, or `x` for the parked wheel); `speed`, `turbo`;
    `land` from `landPoseUntil` (set 110 ms on landing); `hit` from
    `wheelHitAt` (set 500 ms when the wheel rams someone); `time` from the
    runtime clock, for the rattle. `lean` is 0.
- **The shadow** is baked 103 units back, the depth the game gives its own
  spot shadows (caster + .018), in about their grey.
- **The lamps stay static.** Brightening them with speed would make them
  per-tick shapes (two projected ellipses, 22 numbers, 64 a tick), which is
  over the 60 budget.
- **Numbers per tick in the game** (measured by `object-lisp.test.mjs`
  through the game's own frame program): the quad wheel is 88 WORLD = 1144;
  the flat object is 3 SKETCH = 42, after three sketches sent once.

### Follow-ups

- **Lean:** the sim has none. A lean from turning (yaw rate × speed in the
  pool, 0 on flat maps) would feed `lean`.
- **Facing:** the wheel keeps today's behaviour. Its front is +x whichever
  way the rider faces, so the white lamp leads only when riding right.
- **Hit and land:** these come from `wheelHitAt` and `landPoseUntil` only.
  A hit on the rider (`hitStunUntil` has no start time) isn't wired.
- **Double shadow:** a mounted rider draws her spot shadow and the wheel
  draws its own. Either could step aside.
- **Shadow tint:** it is fixed in the object. The game's `shadowInk` follows
  the theme; an input could carry it.
- **Per-tick flat shapes on buffered hosts** go through `screenTriangle`
  (FACE faces), not ELLIPSE/PLATE ops. The monowheel has none. The first
  object that does should send the ops instead.
- **R6:** `FrameVm.cpp` needs SHAPES/SKETCH (projection, flat fills, ink,
  one-sided cull, side count by size) before the console draws objects
  from the program. Until then it takes the immediate path.
