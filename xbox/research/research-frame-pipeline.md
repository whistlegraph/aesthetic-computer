# oskiewar console frame pipeline — research

Sources (read-only):
- JS: `/Users/jas/ac-worktrees/oskiewar-day-perf/xbox/live/oskiewar.js` (21,546 lines)
- Native: `/Users/jas/ac-worktrees/oskiewar-perf/xbox/native-bios/App.cpp`, `QuickJsEngine.cpp`, `ScenePrimitives.inc`, `xbox/runtime/include/ac/runtime.hpp`

Budget context: `App::Run()` (App.cpp:531-604) measures `hostJsMs` from the top of the loop to after `paint()` — it includes `ProcessEvents`, `PollController`, `PollMidi`, the flush calls, and `sim()`+`paint()`. Every JS draw call lands in `m_frameTriangles` (App.cpp:371-373, cap `kMaxTriangles = 8192` at 2753), and `DrawGpuTriangles` (819-858) copies them into one dynamic VB and issues one `Draw`. Nothing is retained on the GPU between frames; the vertex shader gets screen-space x/y and a depth already computed in JS. There is no model/view transform on the GPU at all.

---

## 1. One frame: call tree, in order

`paint()` 20833 → `beginRenderInterpolation` 3148 → `gamePaint()` 20247 → `renderPoses.clear()` 20849.
(Wrappers at 21232-21243 cache `capabilities()` per sim/paint call and `runtime()` per paint call. `runtime()` is **not** cached during `sim`; `RuntimeInfo` QuickJsEngine.cpp:699 builds a ~20-property object per call.)

| # | Phase (line) | What it emits per frame (freeskate park, 5 figures) | Nature |
|---|---|---|---|
| 0 | prologue 20248-20345 | `governFigureLod`, `syncGameView`, `panPlayer`×2 (2 `projectPoint`), `acFeed = ac()` (native builds a fighter-array object every frame, QuickJsEngine.cpp:847-870), `fighterProfile(name)` per player, `refreshPhotoTheme` (native `themeReady()`), `displayTheme()`→`losAngelesSun()`, ~20 `mixColor` array allocs | recompute of constants |
| 1 | `updateSceneLighting` 18576 | 2-4 `projectPoint` + native `themeLighting` (only when photo theme) | per-frame |
| 2 | `wipe`, `drawSkyAtmosphere` 18418 | returns (not survival) | — |
| 3 | `containFighters` 14993 | camera fit; `runnerWorldGeometry` for `cameraPlayers()` → **first pose build of the frame** for players | per-object |
| 4 | `cameraDoll.prepare` 2073; `terrainSpan` 8785 | — | — |
| 5 | `drawRoomSurfaces` 18215 → `drawGridOverlay(…, false)` 18322 | loops **every** `gridField` cell each frame (gridCols×gridRows), draws only hot cells; + 4 `worldLine` (D2D lines) | static-ish; always iterates |
| 6 | `drawTerrainBackWall` 18196 | indoor → returns | — |
| 7 | `drawTerrainSurface` 18129 | cached `pipeSurfaceMesh` → `drawQuadMesh` → **1 `sceneMesh`** | static per map, cached |
| 8 | `drawTerrainFrontWall` 18153 | cached `pipeWallMesh` → **1 `sceneMesh`** | static per map, cached |
| 9 | `drawDecals` 833 | ≤45 decals: arc = 4-8 `worldQuad` with `{...p1}` spreads; skid = 2 `worldQuad`; others 3 `projectPoint` + fan of `screenTriangle`; blood drops 2 `projectPoint` + 1 tri each | slowly-changing, recomputed |
| 10 | `drawSkateParkFeatures` 1445 → `drawIndoorHall` 755 | `drawCityStreet` 21366: ~4-8 cached city blocks → **4-8 `sceneMesh`**; `hallMesh` → **1 `sceneMesh`** + replay of `mesh.capsules` through JS `worldCapsule` → **~56 `capsule3d`** (6 cubes × 9 + 2 coping); `drawDust` 731: 28 `projectPoint` + **56 `triangle3d`**; `drawLoopTrack` per loop (cached mesh → `sceneMesh`) | static per map; mesh cached, capsules + dust recomputed |
| 11 | ledges loop 20399-20412, `drawBoosterPad` 18405 | `worldQuad` + `worldLine` per visible ledge; 1 `worldQuad` per booster | static, recomputed |
| 12 | shadows 20430-20440 | `drawSpotShadow` 18600 per active player (not kids): 3 `projectPoint` + **14 `triangle3d`**; balls likewise | per-object |
| 13 | HUD top block 20447-20510 | `hudSafeRect`, optional timer `typeWrite`; **`drawHudStatusTray` 19744** → `drawParkSupply` 20936 (axe pickup → `drawBigAxe`), **`drawUnderPipe` 20981** (cached `waterMesh` → 1 `sceneMesh` + replay of ~60+ `worldCapsule` = one per pond + one per intact glass pane, `bottomGlass.length = 60`), `drawWaterSplashes` 21046 → `drawTurboParticles` (≤48 × 2 `disc3d`); freeskate `seatHudText`×2 → 4 `comicWrite` | mixed |
| 14 | pickups 20512-20517 | `drawGunPickup`/`drawSaberPickup`/… per active pickup | per-object |
| 15 | renderables 20520-20549 | 5 array spreads, `parkKids` culled with `projectPoint`, `.sort` with closure `depth()` allocating a vector per compare; per renderable 2-3 `projectPoint` (each allocates 2 objects: `toView` + `projectView` default `out={}`) | per-frame alloc churn |
| 16 | `drawRunner` 17058 ×5 | see breakdown below (~430 `triangle3d`, ~100 `capsule3d`, ~48 `disc3d` per LOD-0 figure) | per-object, pose-driven |
| 17 | debug lines 20573-20580 | all early-return unless `debugHitboxes` | — |
| 18 | `drawFrameMeter` 17321, `drawImpacts` 20112, `drawTitleHeadDoor` 20177 | guard returns / few tris | — |
| 19 | `drawControlLegend` 16110 | returns in freeskate | — |
| 20 | `drawFreeskateSpeed` 5264 → `drawSeatPlayerHud` 21137 | **`drawSeatFirstPerson` 21401** (see below) + per player 2-4 `seatHudText` = **4-8 `comicWrite`** + `drawSeatHeart` | screen-space |
| 21 | hudPlayers loop 20686-20697 | `drawPlayerHandle`/`drawHudInventory` return in freeskate; `drawCommandStream` → `drawSeatAction` 21148: 1 `projectPoint` + 2 `comicWrite` when a move word is fresh | screen-space |
| 22 | `drawDebugPerformance` 19838, `drawDeathFlash`, `drawVersusHud`, `drawSpectatorQr`, `drawNetHealth`, `drawTouchControls` | all return or near-nothing in freeskate | — |

### `drawRunner(player)` 17058 — per LOD-0 figure (players[0] has `skin:'pastel'` 5706; kids too 21272)

| Step (line) | Work | Native calls |
|---|---|---|
| `runnerGeometry` 14869 → `runnerWorldGeometry` 14132 | cache hit for players (built in `containFighters`), **fresh build for each kid**; then `constrainLimbs` 14880 (copies every segment with `{...segment}`, `segments.find` with regex `kind()` per upper bone), `projectRunnerWorldGeometry` 14914 (2 `projectPoint` per segment + head ≈ **29 `projectPoint` ≈ 58 object allocs**) | — |
| `buildRunnerWorldGeometry` 14413 | `fighterAnimationPhase` 13981 (string state machine, `meleeFrames[state]`, 2 `terrainFloorAt`), ~14 `segment()` pushes each running `partForRole` (8 `startsWith`/`includes` string tests) + `hasPart` (`removedParts.includes`), `twoBone` IK, skateboard transform loop with `segments.find` ×2 per leg | — |
| `figureLod` 16978 | scan | — |
| `drawSkateboard` 18712 | 7 stations × 4 `worldQuad` + 2 caps = 30 `worldQuad` (each: `litQuadColor` alloc, 4 `toView`, 4 `projectView`, 2 `projectedTriangle`); cracks `worldCapsule`; `drawBoardRunningGear` 18682: 2 × `drawTruck` (10 `worldCapsule` each, 3 when small) + 4 wheels × (1 `projectPoint` + 4 `filledDisc` + 2 `screenTriangle`) | **~60 `triangle3d` + ~20 `capsule3d` + 16 `disc3d` + 8 `triangle3d`** |
| `drawPonytail` 934 | player: 7-node Verlet × 5 passes, Catmull-Rom 13 pts, 13 `projectPoint`, 2 × 12 `filledCapsule`, 8 `stroke`; civilian (kids): 3 `projectPoint` + 4 `stroke` | **24 `capsule3d` + 16 `triangle3d`** |
| `drawBow(…,"tails")` 1145 | 2 ribbons × 7-node Verlet, 14 `projectPoint`, 24 `stroke` + 2 tris | **~50 `triangle3d`** |
| `drawFighterSilhouette` 13930 → `drawSkeletonSegments` 13859 → **`drawCurvedLimbs` 13799** | chains via `segments.find` + regex; per limb cubic Bézier 4-8 points (`points.push({x,y})`), `drawChain` = 1 `stroke` per step × 2 passes + 2 `filledDisc`; straight bones 2 `filledCapsule` each; head 2 `filledDisc` | **~112 `triangle3d` + ~16 `disc3d` + ~6 `capsule3d`** |
| `drawScars` 1411 | `Object.entries(partDamage)` + `filter` per part every frame | 0-8 `capsule3d` |
| `drawOutfit` 1228 | shoes 2 × (3 `filledCapsule` + 2 `stroke`); sleeves 2 × 2 `filledCapsule`; shirt 2 quads; collar `filledDisc`; **daisy 16 `filledDisc`**; `drawSkirt` 1292: player = 6×4-node cloth, 3 passes × (stretch + `separate` vs thighs), 24 `projectPoint`, 15 quads + 23 `stroke`; kid = 2 tris + 3 strokes | **~10 `capsule3d` + 17 `disc3d` + ~80 `triangle3d`** |
| `drawHairline` 1100 | 19 points, 19-tri fan, 9 `filledCapsule`, 10 `stroke` | **9 `capsule3d` + ~40 `triangle3d`** |
| `drawBow(…,"loops")` | 8 tris + 2 `stroke` + 2 `filledDisc` | 12 `triangle3d` + 2 `disc3d` |
| `drawFace` 16407 → **`drawTrioFace` 16305** | spherical feature mapping; 5 `bezier()` each `Array.from(...).map(P)`; blush 2 ovals × 10 tris; per eye 2 ovals × 12 tris + 4 `filledDisc` + lid path 4 `filledCapsule` + 3 lash `filledCapsule` + brow 2; nose 2; lips 2 fans × 5 + 5 capsules; civilian kids = 4 `filledDisc` + 1 `stroke` | **~90 `triangle3d` + 8 `disc3d` + ~30 `capsule3d`** |
| `drawEmoFringe` 21119 (emo kids) | 7 tris + 1 capsule | — |
| `drawInventory` 16694, `drawHeldAxe` 21108 | `segments.find` ×3 by regex; axe = 1 `worldCapsule` + 2 × (`worldQuad` + `worldCapsule`) | 0-5 calls |
| `drawBubble` 17164 (blocking) | 33 `filledCapsule` + 1 `filledDisc` | — |

Sum per LOD-0 skater ≈ 430 `triangle3d` + ~100 `capsule3d` + ~48 `disc3d` — consistent with the measured ~260 `capsule3d` / ~200 `disc3d` across 2 full figures + 3 cheaper civilian kids.

### `drawSeatFirstPerson` 21401 (P1 inset, 4.2 ms)
- 2 `screenRect` (4 `triangle3d`), `P1 VIEW` `comicWrite`.
- `rebuildSeatInset` 21419 **every frame** walks `[pipeSurfaceMesh, pipeWallMesh, hallMesh, waterMesh?, ...cityBlocks.values()]`: per mesh 8 `local()` corner allocs for the bounds test, `new Float32Array(27)` camera, then **`sceneMesh`** — so the C side re-transforms and re-clips *every face of every park mesh a second time per frame* (that C time is inside the JS call and therefore inside `hostJsMs`). `SceneMesh` in `ScenePrimitives.inc` also allocates a `std::vector<ScenePoint> poly` per face and runs 5 `SceneClip` passes each allocating a fresh vector.
- Kids at 30 Hz (`kidsDue`): `runnerWorldGeometry` (cached) → per segment `face()` (front-fast path, else `clipPolygon` ×5 with `mixVertex` allocs) → `values.push` → `new Float32Array(values)` → `triangles3d`.

### `gameSim` 13090-13519 (7.5 ms) — relevant to paint because it rebuilds poses paint then rebuilds again
- 13415 `updateParkKids` (8 kids, `activePlayers().some` per kid), 13416-13417 `updatePlayer` ×2, 13421 `updateHeavyWheelHits`, 13427 `updateMonowheel`, 13428 `updateParkSupply`, 13430 `updateWheelTurbo`, 13432 `updateSeatHeartbeat` (O(players × (players + balls + bullets)) `Math.hypot`), **13438 `resolveMelee` 11035 → `samples = players.map(p => sampleCombatBoxes(p, now))`** → `runnerWorldGeometry` **uncached** (`sharingRenderPoses` is false in sim, 14134) → full `buildRunnerWorldGeometry` per player per tick + `combatRect` object per segment; **13446 `updateBall` 10442** per ball → `boxesAt` → `sampleCombatBoxes` again (its own per-call `Map`, not shared with `resolveMelee`); 13451 `updateCameraDoll`.
- So `buildRunnerWorldGeometry` runs ≈ 2 (melee) + 2×balls (ball) + 2 (containFighters, paint) + 3 kids (paint) ≈ 9-12×/frame → the measured 2.9 ms/frame (~0.25-0.3 ms per build).

---

## 2. Classification per phase

| Phase | Output class | Cached today? | Recomputed every frame |
|---|---|---|---|
| pipe surface / front wall (18129/18153) | **static per map** | yes: `captureQuadMesh` → `nativeMeshBuffers` (Float32Array pair, built once) → `sceneMesh` | C re-projects + clips all faces per call; JS side: `mainNativeCamera` (1 alloc/frame), bounds nothing |
| hall (755), city blocks (21366), loops (679), water (20981) | **static per map** (keys rebuild only on pane/cube break, glass break) | mesh cached; **`mesh.capsules` are NOT** — replayed through JS `worldCapsule`→`worldSegment` (2 `toView` allocs + `projectView` + `clipSegmentBand`) → `capsule3d` every frame (~120 capsules) | yes for capsules; `drawDust` 28 particles fully recomputed |
| grid heat overlay (18322) | static-ish (decays) | no | iterates all cells; draws only hot |
| ledges, boosters, decals, pickups | static / slowly changing | no | `worldQuad` per item per frame, decals allocate spread objects |
| spot shadows (18600) | per-object | no | 3 `projectPoint` + 14 tris per caster |
| fighter pose (`buildRunnerWorldGeometry` 14413) | **per-object per sim tick** (pure function of player state + `frameNow` quantized to `replayTickUs`) | paint-only `renderPoses` Map 14130 keyed by player, cleared each paint; sim never shares | rebuilt 9-12×/frame across sim + paint |
| `constrainLimbs` + `projectRunnerWorldGeometry` | per-object per frame (camera-dependent) | no | full copy + ~29 projections + allocs per figure |
| limbs/face/outfit/hair/skirt/bow (13799, 16305, 1228, 1292, 934, 1145, 1100) | per-object per frame, **screen-space**, but shape is a function of (pose, facing, yaw bucket, blink, cameraScale) | no | full re-tessellation in JS every frame; cloth/Verlet/Bézier all in JS |
| skateboard (18712) | **rigid body per object** — deck/trucks are a fixed mesh in board-local space; only the frame (`skateFrame` 15531) changes | no | 30 `worldQuad` + 20 `worldCapsule` + 16 `disc3d` rebuilt per board per frame |
| P1 inset (21401) | meshes static per map; kids per-object | kids buffer at 30 Hz (`seatInsetBuffer`); meshes **re-submitted every frame** | yes for meshes (C side re-clips everything) |
| HUD text (`seatHudText`, `drawSeatAction`, fps/score) | **screen-space** | `drawFreeskateSpeed` had a 100 ms string cache (5288-5299) but is short-circuited to `drawSeatPlayerHud` at 5265 which caches nothing | strings rebuilt + `handleWidth` (`[...text].reduce`) per frame; native side DirectWrite `DrawText`/`DrawGlyphRun` per string (App.cpp 2610-2660) |
| `acFeed = ac()` 20281, `fighterProfile()` per player, `displayTheme()`/`losAngelesSun()`, 20 `mixColor` | constants / once-per-second data | no | every frame |

---

## 3. JS→native boundary and marshalling

Bindings (QuickJsEngine.cpp:960-1006): `triangle3d`(12 args), `triangles3d`(Float32Array, count ≤ 8192), `disc3d`(7), `capsule3d`(9), `sceneMesh`(3 Float32Arrays), `sprites3d`, `texturedTriangles3d`, `themeSprite`/`themeQuad`, `box`/`line` (D2D, composited *under* the triangle pass — `DrawVectorBackground` App.cpp:2403), `write` (CPU block glyphs), `systemWrite`/`ywftWrite`/`comicWrite` (DirectWrite, `kMaxSystemDraws = 128`), `runtime`, `capabilities`, `ac`.

Per-call cost on the C side is small: `Triangle3d` (209-228) does 9 `JS_ToFloat64` + 3 `JS_ToInt32` (values are already doubles/ints — tag checks), one `std::isfinite` pass, and a `push_back` into `m_frameTriangles`. `SceneCapsule` tessellates 2 + 2n tris (n = 3…12) in C; `SceneDisc` n = 8…28. `SceneMesh` does the full transform + 5 Sutherland-Hodgman clips per face in C, with a heap `std::vector` per face and per clip pass.

**The `emitTriangle` comment (oskiewar.js:2145-2153) checks out.** It says ~2100 faces/frame, a Float32Array staging buffer cost 5.7 ms vs 1.8 ms for direct `triangle3d`, and the batched host call itself was 0.05 ms. That is consistent with how quickjs-ng executes: a 12-argument native call is one `OP_call` with the arguments already on the operand stack, then 12 cheap tag conversions in C; whereas `buf[i+k] = v` in JS is, per element, index arithmetic bytecode + `OP_put_array_el` → `JS_SetPropertyValue` typed-array path → float conversion + store, i.e. ~12 interpreted property stores per face plus the loop counter. Twelve interpreted stores beat one interpreted call, and the C-side reading of the buffer is free. Corroborating evidence inside the same file: `rebuildSeatInset` avoids typed-array element writes and instead `values.push(...)` into a plain Array and does one `new Float32Array(values)` (21478) — a bulk copy in C — which is the right way to batch *if* batching were the goal.

**Weighing it:** the comment's conclusion (don't batch in JS) is correct, but its framing invites the wrong fix. Neither direct calls nor batching removes the real expense, which is *producing* the ~2100 faces' coordinates in interpreted JS (projection, Bézier, Verlet, IK, string-keyed lookups, allocation). The 1.8 ms is the floor for emitting 2100 faces from JS regardless of API shape. The only architectural moves that cut below it are ones where JS emits O(objects) calls and C++ produces the faces: retained meshes by handle, primitives that tessellate natively (`capsule3d`/`disc3d` already do this and are the right shape), and a native affine/model transform so screen-space or object-space geometry can be uploaded once and re-placed per frame with a handful of floats.

Hidden boundary costs found:
- `sceneMesh` for the inset: every mesh every frame, with C-side per-face heap allocation (ScenePrimitives.inc `SceneMesh`/`SceneClip`). This C time is billed to `hostJsMs`.
- `ac()` every `gamePaint` (20281) → `AcData` QuickJsEngine.cpp:847 builds a nested object graph per frame.
- `runtime()` is only snapshot-cached in paint (21240-21243); every `runtime()` in sim (`updateParkKids`, `updateSeatHeartbeat`, `fighterAnimationPhase` 13982, etc.) rebuilds a ~20-property object natively.
- `themeReady()` per frame in `refreshPhotoTheme` 13711 (cheap).

---

## 4. Top 10 JS-time sinks and the cheapest structural move for each

1. **`buildRunnerWorldGeometry` 14413 — 2.9 ms/frame, built 9-12× (sim + paint).**
   Sinks inside: `partForRole` 14478 (8 string tests per segment), `hasPart` 10069 (`includes` per segment), `segments.find` ×2 per leg 14807-14808, `constrainLimbs` 14880 (`{...segment}` copy + `find` with regex per bone), `fighterAnimationPhase` 13981.
   Move: make the pose a **per-player per-tick memo** keyed on `animation.frameNow` (already quantized to `replayTickUs`) + the handful of state fields the pose reads; populate `renderPoses` (or a sim-side twin) from `resolveMelee`/`updateBall` so paint's `containFighters`/`drawRunner`/`rebuildSeatInset` reuse the sim's build. Share one `sampleCombatBoxes` result between `resolveMelee` 11038 and `updateBall.boxesAt` 10464. Replace `partForRole` with a static `Map<role, part>` built once per (facing, itemHand). Expected: builds/frame 9-12 → ≤5, i.e. ~1.5-2 ms.

2. **`drawCurvedLimbs` 13799 (+ `drawSkeletonSegments`/`drawFighterSilhouette`) — 2.1 + 2.2 ms.**
   Per limb per frame: Bézier eval 4-8 steps, `stroke` per step × 2 passes (2 `triangle3d` each), `filledDisc` caps, plus `find` + regex to pair bones.
   Move: **native `curve3d(x0,y0,c1x,c1y,c2x,c2y,x1,y1,z,width,r,g,b)`** (a cubic ribbon tessellated in C, same family as `capsule3d`) — one call per limb per pass instead of ~9 calls + point allocations; 4 limbs × 2 passes = 8 calls per figure. Pair bones once per pose (store `lowerIndex` on the upper segment in `buildRunnerWorldGeometry`).

3. **`drawOutfit` 1228 + `drawSkirt` 1292 — 2.6 + 1.4 ms.**
   Skirt cloth: 24-node Verlet, 3 passes × (stretch + thigh `separate`), 24 `projectPoint`, 15 quads + 23 strokes. Daisy: 16 `disc3d`. Shoes: 6 `capsule3d` + 4 strokes.
   Move: step the skirt cloth at 30 Hz (the P1 inset already sets the precedent at 21410) and drop pleat resolution when `head.radius * cameraScale()` is small; emit the skirt as one `triangles3d` buffer filled via `values.push` + `new Float32Array` (the seat-inset pattern, not element writes). Daisy/shoes/collar are rigid in torso-local space → one `sprites3d` point sprite (binding exists, 292) or a tiny retained screen-space mesh with a native affine (see 6).

4. **`drawTrioFace` 16305 + `drawFace` 16407 — 0.75 + 0.85 ms.**
   5 `bezier()` per face each `Array.from().map()`, spherical `P()` per feature point, ~90 tris + 30 capsules + 8 discs.
   Move: the face is a pure function of (facing, spin-yaw bucket, blink, roll, radius). Memoize a unit-radius triangle list per (yaw/32, blink) in a plain Array → `new Float32Array` once, then draw with a **native affine**: add `triangles3dAt(buffer, count, tx, ty, scale, depth)` (C applies `x*scale+tx`) so the per-frame JS is one call. Kids already take the 5-call civilian path.

5. **`drawSkateboard` 18712 + `drawBoardRunningGear` 18682 — 1.9 ms per frame.**
   Deck = 30 `worldQuad` (each 8 projections + `litQuadColor` alloc), trucks = 20 `worldCapsule`, wheels = 16 `disc3d` + 8 tris; all rigid in board-local space, only `skateFrame` 15531 changes.
   Move: capture the deck + trucks once with `captureQuadMesh` in board-local coordinates and **add a model matrix to `sceneMesh`** (extend the 27-float camera buffer by 12 floats: 3×3 rotation + translation, or a `sceneMeshAt(verts, faces, camera, model)` binding). One call per board. Wheels: 4 `disc3d` stay (they roll) or become 4 `sprites3d`.

6. **Static park geometry re-submitted every frame — `drawQuadMesh` 21184 (3.4 ms), `drawIndoorHall` 755 (2.1), `drawUnderPipe` 20981 (1.5), plus the inset's second pass.**
   JS cost is not `sceneMesh` itself; it is (a) `mesh.capsules` replayed through `worldCapsule`→`worldSegment` (2 object allocs + clip per capsule, ~120/frame: hall cubes 21181-21186 rows/diagonals/edges, coping, 60 glass-pane highlights 21015, chain links), (b) `drawDust` 731 (28 `projectPoint` + 56 tris), (c) per-block bounds and camera buffers, and (d) C-side re-clipping of every face of every mesh twice per frame (main + inset).
   Move: **retained mesh handles** — `sceneMeshUpload(id, verts, faces)` once, `sceneMeshDraw(id, camera)` per frame (27 floats). Fold capsules into the mesh at capture time as world-space segments in a third stream that C tessellates *after* projection (`captureQuadMesh` already intercepts `worldCapsule` at 21175; just stop replaying them in JS). Merge pipeSurface + pipeWall + hall + water + visible city blocks into one draw. `drawDust` → `sprites3d` (exact fit: 28 point sprites) or 30 Hz. In C, replace the per-face `std::vector` churn in `SceneMesh` with fixed stack arrays (poly ≤ 4+5 verts).

7. **`drawSeatFirstPerson`/`rebuildSeatInset` 21401/21419 — 4.2 ms.**
   Every frame: per mesh 8 `local()` allocs + `new Float32Array(27)` + `sceneMesh` (C re-transforms and re-clips every park face for a 320-px inset); kids 30 Hz `face()`/`clipPolygon`.
   Move: with 6 done, the inset is N `sceneMeshDraw(id, insetCamera)` calls; C can additionally cache the inset's clipped output at 30 Hz. Cheapest immediate: submit the inset's meshes on alternate frames only (the kids already are) — halves the 4.2 ms without touching native — and skip meshes whose 8 bounds corners all fail the frustum test *before* allocating the camera buffer.

8. **HUD text — `drawHudStatusTray` 19744 (1.6 ms) + `drawSeatPlayerHud` 21137 + `drawSeatAction` 21148.**
   12-16 `comicWrite`/frame; each string rebuilt (`Math.round(...) + ' mph'`, `'P1 ' + score + ' : ' + ...`), `handleWidth` = `[...text].reduce(comicGlyphAdvance)` per call; native side lays out with DirectWrite per string per frame (App.cpp 2610-2660; the file's own note at 5281: "the console rasterizes every new string … seven milliseconds"). `drawFreeskateSpeed` 5264 had a 100 ms string cache but is bypassed at 5265.
   Move: cache the seat HUD strings at 10 Hz per player (copy the 5288 pattern), memoize `handleWidth(text, size)`, draw shadows only on the large reading. Later: a native `hudText(id, ...)` that keeps the DirectWrite layout until the string changes.

9. **`gamePaint` prologue 20248-20345 + renderables 20520-20549 — allocation churn.**
   `acFeed = ac()` every frame (native object graph); `fighterProfile(player.name)` per player; `displayTheme()`→`losAngelesSun()`; ~20 `mixColor` arrays; 5 array spreads + `.sort` closure; 3 `projectPoint` per renderable (each 2 allocs); `worldSegment` allocs 2 per capsule; `projectPoint` never takes an `out`.
   Move: call `ac()` at 1 Hz; compute the palette once per `visualTheme` change; give `projectPoint` a scratch `out` (as `worldTriangle`/`worldQuad` already do at 2305-2306) and use it in `projectRunnerWorldGeometry`, `drawSpotShadow`, decals, dust; keep a persistent `renderables` array and sort by a precomputed `depth` field.

10. **`gameSim` pose and box rebuilds — 7.5 ms, of which `updateBall` 2.4 + `resolveMelee` 1.2.**
    `resolveMelee` 11038 samples *all* players' boxes every tick with an uncached pose; `updateBall` 10464 samples again per ball; `sampleCombatBoxes` builds a `combatRect` object per segment; `updateSeatHeartbeat` 21517 is O(P × (P + balls + bullets)) with `Math.hypot`; `runtime()` in sim is uncached (21240 caches paint only).
    Move: one `combatBoxes` cache per tick keyed by (pad, now) shared by melee, balls and `drawDebugHitboxes`; extend the `runtime()` snapshot wrapper to `sim`; skip `resolveMelee` for a player with no active attack (`attacking.hit.length` is checked only after the sample is built).

### Order of operations that gets to <16 ms with the least native work
1. Pose memo shared sim→paint (item 1, 10): pure JS, ~2-3 ms.
2. Stop replaying `mesh.capsules` in JS and submit the inset's meshes on alternate frames (items 6, 7): pure JS, ~3-4 ms.
3. Skirt/ribbon/ponytail sims at 30 Hz, HUD strings at 10 Hz (items 3, 8): pure JS, ~2 ms.
4. Native: `sceneMeshUpload/Draw` with a model matrix (items 5, 6, 7) and `curve3d` (item 2) — this is what removes the O(faces) JS floor the `emitTriangle` comment measured, and turns each figure into ~30 native calls instead of ~580.
