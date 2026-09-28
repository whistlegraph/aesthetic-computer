# Oskiewar objects in KidLisp

Props, vehicles, weapons and hats are small KidLisp programs. You write one
in Aesel (a `.lisp` file, the same KidLisp runtime), try it in the object lab,
and then seal it into the game. The same compiled object runs on every
oskiewar host. All it sends is frame ops (`OSKIEWAR-HOSTS.md` §3.0): WORLD
faces, which every interpreter already reads, and one new op, MODEL.

**Forms compile away; hosts see faces and meshes.** Everything in this
dialect, from `revolve` to `radial` to `mirror` to level of detail, is
expanded once, by `object-lisp.mjs`, when the object loads. No interpreter
ever expands a form. Each host (JS web, C++ Xbox, and whatever comes next)
implements only WORLD, ASSET and MODEL. That rule keeps `FrameVm.cpp` small
and keeps the hosts from drifting apart.

Written 2026-09-28. Status:

| piece | where | state |
|---|---|---|
| compiler | `xbox/live/object-lisp.mjs` | done; tests in `xbox/live/tests/object-lisp.test.mjs` |
| MODEL op | `xbox/live/frame-vm.mjs` (op 14) | done on the web; test in `tests/frame-vm.test.mjs`; the game's op table and `FrameVm.cpp` (R6) still need it |
| first object | `xbox/live/objects/monowheel.lisp` | done: tire wedges, rim, web, 5 spokes rolling with distance, lean, landing squash, turbo, lamps |
| lab | `xbox/live/object-lab.html` (`npm run xbox:play`, then `/object-lab.html`) | done; shows each tick's cost |
| in the game | `xbox/tools/embed-objects.mjs` + `drawMonowheel` | **next pass**, see below |

## The budget

This rule holds for every object, and `object-lisp.test.mjs` fails the
monowheel if it breaks it:

- **A tick sends at most 150 numbers** (MODEL ops + WORLD faces), of which
  **at most 8 are moving WORLD faces**. Everything else is baked.
- **The host draws at most 150 / 100 / 80 triangles** at levels 0 / 1 / 2.
- Baked meshes go up once, as ASSETs, when the object loads.

Measured on the monowheel (the old rows come from the game's own frame
program; µs are Node on a Mac at load average ~400, so compare them only
with each other):

| | a tick sends | host triangles | JS per tick |
|---|---|---|---|
| the game's flat monowheel today | 88 WORLD = **1144 numbers** | 88 | — |
| first object slice (all faces) | 260 WORLD = 3380 numbers | 260 | ~150 µs |
| **now** | 2 MODEL + 4 WORLD = **92 numbers** | **148 / 92 / 80** by level | ~14–38 µs |

The 4 WORLD faces are the lamps, whose brightness follows speed. 10 meshes
are uploaded once: the wheel at 3 levels × 2 turbo values, and the decks at
2 distinct levels × 2.

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

## Next pass: into the game

Don't touch `oskiewar.js` without running `npm run xbox:burn:oskiewar-social`
(the manifest is hash-bound). About 41 tests already fail at HEAD, so compare
against that baseline.

1. **MODEL in the game's op table.** Add op 14 (20 numbers) to the table at
   the head of the frame-program section. Add an `emitModel(radius, h0, h1,
   h2, frame, at)` that writes it with `globalLight`. For hosts without
   `frame` (console, harness) it draws the mesh immediately, using the same
   rule as `frame-vm.mjs`'s `drawModel`, through `worldTriangle`.
2. **`xbox/tools/embed-objects.mjs`**, copied from `embed-spine.mjs`.
   - It writes a sealed `// <objects>` … `// </objects>` block just before
     `function drawMonowheel(` (anchor), with `object-lisp.mjs` unexported
     inside its own scope, plus each `objects/*.lisp` as a string literal.
   - The block exposes one global, `gameObjects = { monowheel: compile(source,
     "monowheel"), … }`, compiled (and so baked) once when the script loads.
   - The game uploads each object's meshes as ASSETs, once per host, as it
     does park meshes.
   - `--check` exits 1 on drift, and a test like `spine.test.mjs`'s last one
     asserts `embedded() === generate()`.
3. **The call site:** `drawMonowheel(p)` (oskiewar.js ~25064, reached from
   `drawSkateboard` ~22766 for a rider and from the renderables loop ~24703
   for the parked wheel). Its body becomes:
   ```js
   const at = spunSkateFrame(monowheelFrame(p), p), o = at(0, 0, 0);
   const f = at(1, 0, 0), d = at(0, 1, 0);          // the rig's local y points down
   const fx = f.x - o.x, fy = f.y - o.y, fz = f.z - o.z;
   const ux = o.x - d.x, uy = o.y - d.y, uz = o.z - d.z;
   // right = forward × up keeps the placement a rotation, never a mirror
   gameObjects.monowheel({ time: simSeconds, distance: (p.skateSpin || 0) * skateWheelRadius,
       speed: p.skateVx || p.vx || 0, lean: monowheelLean(p), turbo: p.wheelTurbo ? 1 : 0,
       hit: sinceHit(p), land: sinceLanding(p) },
     [o.x, o.y, o.z, fx, fy, fz, ux, uy, uz,
      fy * uz - fz * uy, fz * ux - fx * uz, fx * uy - fy * ux], objectOut);
   ```
   - `objectOut` is `{ face, model }`: `emitWorldFace`/`emitModel` when
     `programBuffered`, and the immediate equivalents otherwise.
   - `p.skateSpin` is the odometer the board wheels already roll by (~14122).
     The parked wheel passes `distance: m.x`.
   - `simSeconds`, `monowheelLean`, `sinceHit` and `sinceLanding` are
     illustrative names for render-side reads of state the sim already keeps.
4. **Gate:**
   - a `frame-conformance` case: `drawMonowheel` immediate vs program
   - the renderer suite at baseline
   - the social preview reburned
   - a lab screenshot next to an in-game one
   - `FrameVm.cpp` gains MODEL in R6 before the console draws objects from
     the program
