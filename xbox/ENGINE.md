# Oskiewar engine: the game/renderer split

Written 2026-09-26 from four read-only surveys of `xbox/live/oskiewar.js`
(day branch, 21.5k lines) and `xbox/native-bios/` (App.cpp, QuickJsEngine.cpp,
ScenePrimitives.inc). Numbers are Series X `AC_NATIVE_PROFILE hostJsMs`
unless noted.

## The problem in one line

On the console the game's JavaScript runs interpreted (QuickJS, no JIT) and
that JavaScript is the whole frame: ~24 ms in the freeskate park, 60 fps needs
<16. Render CPU is 0.7 ms; the GPU is idle. Every frame the JS re-tessellates
~2,100 faces (Bézier limbs, cloth skirts, hair fans, quad meshes, capsule
edges) and hands them over one call at a time.

The `emitTriangle` comment (batching a Float32Array cost 5.7 ms where per-face
calls cost 1.8) is right and points the wrong way: in QuickJS a typed-array
element write costs ~0.23 µs and a 12-arg C call ~0.85 µs, so **the count of
numbers crossing is the cost, whichever side writes them.** 2,100 faces are
25,000 numbers a frame. A fighter's pose is ~130. Only designs where JS sends
O(objects) and C++ produces the faces get under the 1.8 ms floor.

## The split

**Game** (one JS source; deterministic; 331 tests): input, `gameSim`/`netTick`,
rollback (`netSnapshot`/`netRestore`/`netStateHash`), replay capture, the
world-space pose (`buildRunnerWorldGeometry` — it is also the hitbox source, so
it never leaves JS), the LOD tier decision (`figureLod`), the camera solve, and
the order things are drawn in (`gamePaint`).

**Renderer** (C++ on the console; a JS reference everywhere else): everything
from "world-space pose + camera" down — projection, clipping, capsule/disc/
ribbon fans, skeleton skinning, outfit, face, terrain strips, quad-mesh
lighting, HUD glyph fanning, post effects.

The interface is retained handles plus small per-frame payloads:

```js
// once per map / per look
const mesh  = meshUpload(verticesF32, facesF32, capsulesF32)   // → int
const style = figureStyle(jsonString)                          // → int
// per frame
meshDraw(mesh, cameraF32 /* the persistent 27-float buffer */)
figure(style, pose /* the JS pose object; C++ walks it by atom */, state, tier, depth)
postEffects(focusY, band, feather, tiltPx, motionX, motionY)   // shipped, 1.0.0.48
```

Rules that keep it one game:
- The renderer returns nothing the game reads.
- `paint` never writes a sim object. Render-only fields (`lodTier`,
  `displayFps`, interpolation) live in WeakMaps or a frame-state snapshot, not
  on `player` (players ride `structuredClone` into net snapshots).
- Nothing reachable from `gameSim` reads `runtime().monotonicUs`, `cameraDoll`,
  `projectPoint` or `viewWidth()` — a wall clock inside the pose would give two
  seats different hitboxes.
- Every native call is additive and levelled: native advertises
  `capabilities().sceneApi = N`; JS resolves the level once at boot (no
  `typeof` on hot paths) and keeps its level-(N−1) path. Level 0 is the JS
  reference, which is what the web, the Mac shell and the tests run.
- Native changes bump `Package.appxmanifest` Version every time — the Device
  Portal keeps the package it has when the version matches.

## Where the frame goes (park, five figures)

| phase | ms | why |
|---|---|---|
| figures (`drawRunner` ×5) | 8 | ~514 native calls per pastel skater; skirt cloth, ponytail, face, curved limbs, bow all tessellated in JS |
| sim | 7.5 | poses built 9–12×/frame (melee, balls, camera, paint, inset); `updateBall` 2.4, `resolveMelee` 1.2 |
| park meshes | 3.4 + 2.1 + 1.5 | meshes are cached but their ~120 edge capsules are re-projected in JS every frame; `sceneMesh` re-validates and heap-allocates per face |
| P1 inset | 4.2 | every park mesh re-sent through `sceneMesh` a second time, unculled (the bounds check reads a field only the web path computes) |
| HUD text | 1.6 | strings rebuilt per frame, DirectWrite layout per string per frame |

## Migration, in order of ms per day

| step | lane | work | saves |
|---|---|---|---|
| 0 | JS | cache `runtime()`/`capabilities()` per sim and paint; hoist `typeof` to constants; one pose memo per tick shared by melee, balls, camera, paint and inset (keyed on the sim tick, never the wall clock); one combat-box cache per tick; render fields off `player`; `ac()` at 1 Hz; HUD strings at 10 Hz | 3–4 ms |
| 1 | native | `meshUpload/meshDraw/meshFree`: validate, AABB, pre-light once at upload; capsules folded in and fanned after projection; stack clipping; the inset draws the same handles with its own camera; hall draws retained even with roof shards | ~2.5 ms |
| 2 | native | `figureStyle` + `figure`: tiers 1–3, tier-0 flat body, curved chains, hair cap, outfit minus skirt, trio face, scars, strobe. JS keeps ponytail/skirt/bow/items/shield/glyphs for now. Split `drawRunner` into `figureRecord` → `figureNative` \| `figureJs` | ~1.2–1.4 of 1.6 ms per figure |
| 3 | native | `sceneDraw(camera, viewMask)` for both views; `meshDrawAt` for rigid props (a skateboard = 7 calls, not ~110); `figureInstance` with ponytail/skirt Verlets in C++; crowd batch | ~3.5 + ~2 ms |
| 4 | native | terrain strips by handle for the outdoor maps; native glyph-run cache; `sceneMesh` per-face allocs gone | ~3 ms outdoor |
| 5 | web | ship `scene3d-webgl.mjs` + the reference module so "web = reference" is checked, not hoped; Mac gains `triangles3d`/`figure` via a C shim | 0 console ms |

Each step is measured the same way: dev-deploy, read `AC_NATIVE_PROFILE`
median over 20 lines, screenshot with `xbox/tools/live.mjs screenshot`.

## Tests

- The harness (`createFight`) stubs `triangle3d` and never defines
  `capsule3d`/`disc3d`/`figure`/`meshDraw`, so all 120 triangle-observing tests
  keep exercising the JS reference unchanged.
- Add scene-level stubs that record `{kind, handle, tier}` and assert semantic
  counts (five figures at tier 1, one mesh) — the `countPoseBuilds` pattern.
- Golden cross-check: `quickjs_engine_smoke.cpp` dumps the native triangle
  stream for one canonical pose/mesh; a Node test replays the reference and
  diffs. This is the test that would have caught `SceneDisc` (5 ring tiers)
  already differing from `discRingFor` (6 tiers).
- Source-shape gate: `REQUIRED_SCENE_API` in `oskiewar.js` ≤ the `sceneApi`
  literal in App.cpp, so `oskiewar-release.mjs` cannot ship a JS that needs a
  level the console does not advertise.

## Rejected

- **GPU instancing as the fix.** Building the instance list in JS costs the
  same as the calls it replaces. It pays only as a cheaper sink under a C++
  builder, or for shared props.
- **A retained angle skeleton with C++ forward kinematics.** The rig is ~450
  lines of positional, terrain-probing game logic and the hitbox source; a
  positional pose is the same wire size as angles.
- **Submitting the inset on alternate frames.** Nothing is retained on the GPU
  between frames; it would flicker.
