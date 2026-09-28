# Oskiewar objects in KidLisp

Props, vehicles, weapons and hats are small KidLisp programs. You write one
in Aesel (a `.lisp` file, the same KidLisp runtime), try it in the object lab,
and then seal it into the game. The same compiled object runs on every
oskiewar host, because all it produces is WORLD faces in the frame program
(`OSKIEWAR-HOSTS.md` §3.0), and every interpreter already reads those.

Written 2026-09-28 at the first slice (the monowheel). Status:

| piece | where | state |
|---|---|---|
| compiler | `xbox/live/object-lisp.mjs` | done; tests in `xbox/live/tests/object-lisp.test.mjs` |
| first object | `xbox/live/objects/monowheel.lisp` | done: rim, 5 spokes rolling with distance, tread blocks, lean, landing squash, turbo, lamps |
| lab | `xbox/live/object-lab.html` (`npm run xbox:play`, then `/object-lab.html`) | done |
| in the game | `xbox/tools/embed-objects.mjs` + `drawMonowheel` | **next pass**, see below |

## Why a compile target, not the evaluator

The KidLisp tree-walker is too slow to run per frame on the console: a 61-line
crawl repainted about once every 6 s on the oven. `compile(source)` walks the
tree **once** and returns closures. Constant expressions fold at compile time,
transform nesting depth is checked at compile time, and a tick is just the
closures running. The monowheel is 260 faces in 27 µs a tick in Node (about
350 µs with the Mac under load 340). KidLisp's own semantics stay as they are.
The dialect uses KidLisp's reading rules (bare lines, commas, `;` comments,
auto-closing parens) and its words where they fit (`def`, `if`, `repeat`,
`ink`), and a test holds the reader to `kidlisp.mjs`'s `parse` on every object.

## The dialect

```lisp
def r 24                                   ; binds once, at compile time
(let roll (/ distance r))                  ; re-evaluated every tick
(move 0 (- r) 0                            ; transforms take a body
  (rotate x lean
    (move 0 r 0
      (rotate z (- roll)
        (repeat 5 k
          (rotate z (* k (/ tau 5))
            (ink 205 65 117)
            (capsule 4 0 14 15 0 14 2.4 4)))))))
```

**Object space:** x forward, y up, z to the owner's right. Units are the
game's.

**Inputs** are bare names, read fresh every tick: `time` (s), `distance`
(signed, rolled world units), `speed` (units/s), `lean`, `heading`, `pitch`
(radians), `turbo` (0–1, so it can fade), `hit` and `land` (seconds since the
event; very large if it never happened). `(owner part x|y|z)` reads one of
the owner's pose points, which the game hands over in object space (0 when it
isn't there). Heading and pitch are already baked into the placement. They
are passed as inputs for objects that want to react to them (a streaming flag,
a sloshing cup).

**Binding:** `def` binds once, and a `def` that reads an input is a compile
error that points you to `let`. `let` binds for the rest of its body and is
re-evaluated every tick. `repeat n i body…` loops with an optional iterator.
`if test body…` has no else, as in KidLisp. Every value is a number, and a
comparison is 1 or 0, so choices are written as arithmetic: `(mix a b (% i 2))`.

**Math:** `+ - * / %` (`%` is always positive), `min max abs sign sin cos tan
atan sqrt pow floor round clamp mix`, `= < > <= >= and or not`, and `pi tau`.

**Transforms** are scoped: `(move x y z body…)`, `(rotate x|y|z angle body…)`
(right-handed), and `(scale k body…)` or `(scale x y z body…)`. A negative
scale mirrors the body, and faces keep facing out: the winding flips with the
determinant. Nesting is limited to 16 levels.

**Color:** `(ink r g b)` or `(ink name)` stays in effect until the next ink.
Every face is lit by the game's flat rule in world space, `.72 + .28 ·
max(0, facing the sun)`, from its own winding. That is the same rule and the
same sun as `worldQuad`/`litQuadColor`, and a test lifts `litQuadColor` out of
`oskiewar.js` and compares against it. `(glow body…)` leaves faces unlit, for
lamps and trim.

**Shapes** (object space, current frame):

| form | draws | lowers to |
|---|---|---|
| `(tri x y z ×3)` | one face | 1 WORLD |
| `(quad x y z ×4)` | two faces lit as one (as `worldQuad`) | 2 WORLD |
| `(disc r [sides])` | flat disc at the origin, facing +z | sides × WORLD |
| `(hoop inner outer [sides])` | flat ring facing +z | 2·sides × WORLD |
| `(band r width [sides] [turn])` | tube wall around z; `turn` 0–1 draws part of it | 2·sides × WORLD |
| `(capsule x1 y1 z1 x2 y2 z2 w [sides])` | a rod | 2·sides × WORLD (a prism today) |
| `(line x1 y1 z1 x2 y2 z2 [w])` | a thin three-sided rod | 6 WORLD |

Sides default by radius, the way the game's fans pick them (6/8/12/16).

## How the game runs one

```js
const monowheelObject = compile(monowheelSource, "monowheel");   // once, at load
monowheelObject(inputs, place, emit);                            // every draw
```

`place` is twelve numbers: the world origin, then where object x, y and z
point in world space. `emit(ax, ay, az, bx, by, bz, cx, cy, cz, r, g, b)` has
`emitWorldFace`'s signature, so each face becomes one WORLD op under the
current CAMERA, and the interpreter near-clips, band-clips and depth-tests it
like any other world face. The object neither sees nor needs the camera,
view, host or platform.

**Onto the frame ops:** today everything lowers to WORLD (op 10). Two later
ops would move the tessellation into each interpreter, as rule 1b wants:

- **world DISC/CAPSULE** (x y z endpoints, radius, rgb): the `disc`,
  `capsule` and `line` shapes would emit these instead of fans and prisms.
- **MESH with a model matrix** (§3.0 lists `model[12]`; `frame-vm.mjs`'s MESH
  takes only a light today): a sub-tree that reads no inputs could be baked to
  an ASSET once and drawn as one MESH per tick with the transform stack's
  matrix. For the monowheel that is the tire, rim and decks.

## Next pass: into the game

Don't touch `oskiewar.js` without running `npm run xbox:burn:oskiewar-social`
(the manifest is hash-bound). About 41 tests already fail at HEAD, so compare
against that baseline.

1. **`xbox/tools/embed-objects.mjs`**, copied from `embed-spine.mjs`. It writes
   a sealed `// <objects>` … `// </objects>` block just before
   `function drawMonowheel(` (anchor), with `object-lisp.mjs` unexported
   inside its own scope, plus each `objects/*.lisp` as a string literal. The
   block exposes one global,
   `gameObjects = { monowheel: compile(source, "monowheel"), … }`, compiled
   once when the script loads. `--check` exits 1 on drift, and a test like
   `spine.test.mjs`'s last one asserts `embedded() === generate()`.
2. **The call site:** `drawMonowheel(p)` (oskiewar.js ~25064, reached from
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
      fy * uz - fz * uy, fz * ux - fx * uz, fx * uy - fy * ux], objectFace);
   ```
   `objectFace` is `emitWorldFace` when `programBuffered`, and otherwise
   `worldTriangle` with scratch vertices, because the console and the harness
   still draw immediately and faces arrive already lit. `p.skateSpin` is the
   odometer the board wheels already roll by (~14122). The parked wheel passes
   `distance: m.x`. `monowheelLean`, `sinceHit` and `sinceLanding` are
   render-side reads of state the sim already keeps.
3. **Gate:** a `frame-conformance` case drawing `drawMonowheel` immediate vs
   program (same faces), the renderer suite at baseline, the social preview
   reburned, and a lab screenshot next to an in-game one. The monowheel goes
   from about 90 flat faces to about 260 lit ones (about 3.4k floats, the size
   of one figure), so the console frame time should be checked before
   shipping.
