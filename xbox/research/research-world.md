# oskiewar world rendering — retained native scene research

Sources: `/Users/jas/ac-worktrees/oskiewar-day-perf/xbox/live/oskiewar.js` (JS, 21546 lines), `/Users/jas/ac-worktrees/oskiewar-perf/xbox/native-bios/{ScenePrimitives.inc,QuickJsEngine.cpp,App.cpp}`, `/Users/jas/ac-worktrees/oskiewar-perf/xbox/runtime/include/ac/runtime.hpp`. Line numbers below are from those files. Read only; nothing edited.

## Findings that change the plan

1. **`drawQuadMesh`'s 3.4 ms on console is mostly NOT the meshes — it is capsule replay in JS.** The native branch (oskiewar.js 21186-21189) hands the faces to `sceneMesh` once, but first loops `mesh.capsules` and calls `worldCapsule` for each (21187). `hallMesh` carries 56 capsules (6 funboxes × 9 at 799-806 + 2 coping at 811-814); `waterMesh` carries 62 (60 bottom-glass panes at 21010 + 2 pond waterlines at 21001). Each `worldCapsule` (17875) → `worldSegment` (17851: 2 `toView` + 2 `projectView`, all allocating fresh `{x,y,z}` objects, plus `clipSegmentBand`) → `filledCapsule` → `capsule3d` (9-arg native call). ~120 of those a frame. This is the biggest single win and it needs only a capsule list inside the retained handle.

2. **The P1 inset's bounds cull never fires on Xbox.** `rebuildSeatInset` (21446) tests `if(mesh.bounds)`, but `mesh.bounds` is only computed lazily in the JS fallback branch of `drawQuadMesh` (21191), which the native branch returns before (21189). On console `mesh.bounds` is `undefined`, so every frame the inset sends `pipeSurfaceMesh`, `pipeWallMesh`, `hallMesh`, (`waterMesh`), and **all ten** `cityBlocks` through `sceneMesh` a second time, uncullled, each with a freshly allocated `new Float32Array(27)` camera (21459). The main view culls city blocks by x-span (21368) but the inset does not.

3. **`sceneMesh` (ScenePrimitives.inc 51-73) re-validates the whole index stream on every call (line 64) and heap-allocates per face**: `std::vector<ScenePoint> poly{...}` (65) plus five `SceneClip` calls each returning a new vector (66, 68). ~700 faces/frame × 6 allocations. A retained handle validates once at upload and clips into a fixed `ScenePoint[9]` (a quad clipped by 5 planes has at most 9 vertices).

4. **The measured numbers nest.** `drawHudStatusTray` (19744) calls `drawParkSupply`, `drawUnderPipe` and `drawWaterSplashes` first (19746-19748); its 1.6 ms is ~1.5 ms `drawUnderPipe` (= `waterMesh` = 62 capsules) and ~0.1 ms of actual tray. `drawIndoorHall` (755) includes `drawCityStreet` (5 `drawQuadMesh`) + `hallMesh` + `drawDust`. So "drawQuadMesh 3.4 across 13 meshes" already contains most of the 2.1 + 1.5.

5. **`globalLight` is a constant** (1945: `normalize3({x:-.42,y:1,z:-.28})`), so face lighting (`.72 + max(0, -n·L) * .28`, ScenePrimitives.inc 69, litQuadColor 17905) can be baked at upload. The camera's 27 floats then only need 24; keep the slots for compatibility.

6. **The photo texture path is dead for the world.** `photoSurface` (13781) returns `false` for `"wall"`, `"concrete"` and `"skirt"` (13784) before touching `themeQuad`, so no terrain face is ever textured. Retained meshes need no UVs in phase 1. Textured world faces would come back through `texturedTriangles3d` (QuickJsEngine.cpp 413-465; 18 floats/tri; cap `kMaxTexturedTriangles` 2048, App.cpp 2754).

7. **One 8192-triangle budget for everything** (`kMaxTriangles`, App.cpp 2753; `on_triangle` drops silently past it, 371-373). `sceneMesh` output, fighters, HUD boxes and the inset all share it. The inset caps itself at 6000 tris (21429). `AC_NATIVE_FRAME ... trianglesDropped=` is logged once at first frame only (`m_loggedTextFrame`, 2678-2698), so a mid-round overflow is invisible.

8. **When a roof pane breaks, the whole hall falls back to the per-frame JS path** (757: `if(roofShards.length){drawIndoorHallGeometry(ground);return;}`) for as long as shards exist — 91 quads + 56 capsules re-projected in JS per frame. With a retained hall handle the shards (a separate loop at 826-836) draw on their own.

9. **Freeskate HUD bypasses its own 10 Hz string cache.** `drawFreeskateSpeed` (5264) returns on line 5265 straight into `drawSeatPlayerHud` (21130), which formats `measure`/`verb`/`item` strings every frame (21136-21140); the 100 ms cache it skips is at 5284-5296. Each new string is a `JS_ToCString` + `std::string` + `std::wstring` + a fresh DirectWrite glyph run (`QueueSystemWrite` 467-486; `drawPackagedFont` App.cpp 2618-2655 calls `GetGlyphIndices` + `GetDesignGlyphMetrics` per text per frame).

## 1. Inventory

Legend: **S** = static per map (built once, keyed), **D** = dynamic world geometry, **SS** = screen-space. "JS/frame" = what JS does each frame on console today. Native endpoint = where the pixels go.

### Backdrop and room

| Element | Class | Where | Built | JS/frame | Native call |
|---|---|---|---|---|---|
| clear + sky bands/streaks | SS | `gamePaint` 20346 `wipe`; `drawSkyAtmosphere` 18418 (survival only) | per frame | 6 `box` + 11 `line` (survival only; park: nothing) | `wipe`, `box`, `line` → CPU frame / D2D under the 3D pass |
| room walls / containment lines | D | `drawRoomSurfaces` 18215 (station + survival only; indoor returns nothing) | per frame | survival: 1 wall + 4 side quads + 2 `worldLine`; station: `drawGridOverlay` + 4 `worldLine` | `triangle3d` per tri; `line` |
| grid heat overlay (station) | D | `drawGridOverlay` 18322 | per frame | up to 16 quads per hot tile, 57 seam quads (climb only) | `triangle3d` |

### Terrain (three passes)

| Element | Class | Where | Built | JS/frame | Native |
|---|---|---|---|---|---|
| back wall | S (outdoor) | `drawTerrainBackWall` ~18186 → `terrainPass` 18075 | profile thinned per LOD at map change (`rebuildTerrainDrawProfile` 17983) | project each profile point once (`terrainVertex` 18040), 1 quad/segment in span; indoor: returns | `triangle3d` ×2 per quad |
| surface | S | `drawTerrainSurface` 18129: indoor → captured `pipeSurfaceMesh` (~36 quads: 2 transitions × 16 + 4 flats); outdoor → `terrainPass` | key `gridCols:near:far:color` | indoor: `drawQuadMesh`; outdoor: per-point projection over `terrainSpan` (8785, ±2600 apron) | `sceneMesh` / `triangle3d` |
| front wall / skirt | S | `drawTerrainFrontWall` 18153: indoor → `pipeWallMesh` (1 big + ~36 quads); outdoor → `terrainPass` with 1 row on console (18180) | keyed | as above | `sceneMesh` / `triangle3d` |
| grass | D (deterministic) | `drawTerrainGrass` 18385; skipped in space (`!space`, 20390) | per frame | 100 tufts × 2 `worldLine` → each 2 `toView` + 2 `projectView` | `line` (D2D, under everything) |

Terrain camera-space math in JS: `FightCamDoll.toView` 2071, `projectView` 2084, `prepare` 2049. `bandContains` 2298 uses `guardBand = 1` (1936) i.e. ±1 viewport; `cameraNear = 8` (1931).

### Park furniture (indoor hall)

| Element | Class | Where | Built | JS/frame | Native |
|---|---|---|---|---|---|
| funboxes (6 cubes) | S (rebuild on break) | `drawIndoorHallGeometry` 762-806 | in `hallMesh`, key = ground + pane/cube broken bits (758) | 30 quads via `sceneMesh`; **54 capsules replayed in JS** | `sceneMesh` + 54× `capsule3d` |
| pipe coping (2) | S | 811-814 | in `hallMesh` | 2 `worldCapsule` | `capsule3d` |
| glass roof: 20 panes × (pane + glint + frame) + mid rail | S (rebuild on break) | 816-830 | in `hallMesh` (~61 quads) | `sceneMesh` | `sceneMesh` |
| roof shards | D transient | 831-838 | per frame | `projectPoint` + 1 `screenTriangle` each; **forces whole hall to JS path while any exist (757)** | `triangle3d` |
| dust (28 specks) | D cosmetic | `drawDust` 731 | per frame | 28 `projectPoint` + 56 `screenTriangle` | `triangle3d` |
| city street: 10 blocks × 29 quads | S | `drawCityStreet` 21366 / `buildCityBlock` 21374 | lazily captured per block into `cityBlocks` Map | x-span cull (21368) then ~5 `drawQuadMesh` | `sceneMesh` ×5 |
| loop track (indoor: 1 loop r=260; park: 3) | S per LOD | `drawLoopTrack` 679 → `loop.mesh`, key includes `sceneLod()` (681) | 64 seg × 4 quads = 256 quads at LOD 1 (32/16 seg + 1 side at LOD ≥2) | `drawQuadMesh` | `sceneMesh` |
| under-pipe water/tunnel: 30 column quads + pond sheet + bottom + 60 glass panes × 2 quads | S (rebuild on glass break) | `drawUnderPipe` 20981 / `drawUnderPipeGeometry` 20987; key = `bottomGlass` bits + gridCols | `waterMesh` (~153 quads) | `drawQuadMesh`; **62 capsules replayed in JS** | `sceneMesh` + 62× `capsule3d` |

### Park furniture (outdoor course, not captured)

| Element | Class | Where | JS/frame | Native |
|---|---|---|---|---|
| ponds: sheet + 16 front + 16 waterline quads | S-shaped, drawn dynamic | `drawSkateParkFeatures` 1457-1478 | 33 `worldQuad` per visible pond | `triangle3d` |
| boost pads: 6 chevrons × 2 capsules | S | 1480-1486 | up to 12 `worldCapsule`/pad | `capsule3d` |
| chains: 1 bar + 16/linkStep links | D (Verlet) | 1488-1499 | `worldCapsule` per link | `capsule3d` |
| booster pads (station) | D (pulsing tint) | `drawBoosterPad` 18405 | 1 quad per pad | `triangle3d` |
| ledges/decks | S | `gamePaint` 20407-20422 | 1 `worldQuad` + 1 `worldLine` each | `triangle3d`, `line` |
| survival lava | D | `drawSurvivalLava` 18279 | 2 quads + 9 `worldLine` | |

### Decals and marks

| Element | Class | Where | JS/frame | Native |
|---|---|---|---|---|
| decals (≤45 drawn, arc/skid/blood/ding/wheelmark/chip) | D append-only, ages | `drawDecals` 833 | arcs: 4-8 `worldQuad`; skids: 2 `worldQuad`; fans: 3 `projectPoint` + N `screenTriangle` from a cached shape | `triangle3d` |
| blood drops | D transient | 893-902 | 2 `projectPoint` + 1 tri each | `triangle3d` |
| spot shadows (fighters, balls) | D | `drawSpotShadow` 18600 | 3 `projectPoint` + 14 `screenTriangle` | `triangle3d` |

### Props

| Element | Class | Where | JS/frame | Native |
|---|---|---|---|---|
| skateboard (deck) per rider/loose board | rigid body, D pose | `drawSkateboard` 18712: 7 stations × 4 `worldQuad` + 2 caps + crack capsules | ~30 `worldQuad` (each `litQuadColor` + 4 `toView` + 4 `projectView`) | `triangle3d` ×60 |
| trucks (2) | rigid | `drawTruck` 18654: 12 capsules at full scale, 3 at `scale<.65` | 24 `worldCapsule` | `capsule3d` ×24 |
| wheels (4) | rigid + spin mark | `drawBoardRunningGear` 18682 | 4 × (`projectPoint` + 4 `filledDisc` + 2 `screenTriangle`) | `disc3d` ×16, `triangle3d` ×8 |
| monowheel | rigid | `drawMonowheel` 20889: 12 seg × (1 quad + 4 `worldTriangle`) + 8 quads | ~68 faces | `triangle3d` |
| balls (soccer/beach) | D | `drawBall` ~18760 | screen-space fans | `disc3d`, `triangle3d` |
| pickups: axe, SMG, saber, grenade, body trees | D bob | `drawParkSupply` 20936, `drawGunPickup` 18896, `drawSaberPickup` 18965, `drawGrenadePickup` 19080, `drawBodyTree` 9669 | few quads/capsules or `themeSprite` under photo theme | mixed |
| drone / drop | D rare | `drawParkSupply` 20938-20944 | 1 capsule + 2 quads / 1 quad + 2 capsules | |
| turbo particles, splashes, impacts | D transient | `drawTurboParticles` 21345, `drawWaterSplashes` 21047, `drawImpacts` 20112 | `projectPoint` + `filledDisc`/`filledRing` | `disc3d`, `triangle3d` |

### Crowd

| Element | Class | Where | JS/frame | Native |
|---|---|---|---|---|
| parkKids (8, indoor freeskate only) | D characters | `resetParkKids` 21262 (cloned from `players[0]` via JSON), `updateParkKids` 21286 (sim); drawn in `gamePaint` 20544-20547 as `renderables` → `drawRunner` 17058 when on-screen | full runner pipeline per visible kid; `figureLod` 16978 tiers them by screen height | fighter path (character side) |
| kids in P1 inset | D | `rebuildSeatInset` 21478-21500, 30 Hz | `runnerWorldGeometry` per kid **again** (not shared with the main view's solve), ~20 segments × `face()` (4 object literals + clip checks + 12 `values.push`), head/hair/eyes/bow discs (10 tris each), then `new Float32Array(values)` | `triangles3d(seatInsetBuffer, count)` every frame (21413) |

### P1 first-person inset (4.2 ms)

`drawSeatFirstPerson` 21401: 2 `screenRect`, `rebuildSeatInset(rect, kidsDue)` (21411), `triangles3d` replay, 1 `screenRect`, 1 `typeWrite`. Per frame in `rebuildSeatInset` on console: for each of `[pipeSurfaceMesh, pipeWallMesh, hallMesh, waterMesh?, ...cityBlocks.values()]` (21444): bounds test (dead, finding 2), `new Float32Array(27)` camera (21459), `sceneMesh`. Inset camera layout (21459): `[eye.x,eye.y,eye.z, -fz,0,fx, fx*.12,-1,fz*.12, fx,.12,fz, rect.cx,rect.cy, 0, focal=rect.w*.68, 1, near=12, rect.x,rect.y,rect.x+w,rect.y+h, -1.497, .000002, L.x,L.y,L.z]`.

### HUD (screen space)

| Element | Where | JS/frame | Native |
|---|---|---|---|
| seat HUD (freeskate): measure/verb/item per player + heart + FPS + P2 score | `drawSeatPlayerHud` 21130, `drawSeatHeart` 21528, `drawHudStatusTray` 19770-19775 | ~8 `typeWrite` (each = shadow + ink = 2 `comicWrite`), strings rebuilt every frame | `comicWrite` → D2D `DrawGlyphRun` (render side) |
| seat action word | `drawSeatAction` 21150 | 2 `typeWrite` at `hudDepth` | |
| nameplates (fight modes) | `drawPlayerHandle` 17417 | **per glyph** `typeWrite`, 2 passes (shadow + colour) | one `SystemText` per glyph; cap `kMaxSystemDraws` 128 (App.cpp 2752) |
| command stream, inventory, stats | 17604, 17549, 17522 | 2 `typeWrite` per entry | |
| status tray (MIDI piano) | `drawStatusPiano` 19735 | 6 `hudBox` | `triangle3d` at `hudDepth -1.48` |
| frame meter (debug) | `drawFrameMeter` 17321 | up to 2×120 `hudBox` | `triangle3d` |
| control legend / stick gates | `drawControlLegend` 16110 (returns in freeskate) | `filledDisc` ×3 + `typeWrite` per live stick | `disc3d` |
| net health, QR | `drawNetHealth` 20043, `drawSpectatorQr` 20067 | `screenRect` runs | `triangle3d` at -1.43 |
| touch controls | `drawTouchControls` 16219 (touch only) | capsules + discs | |

HUD depth lanes: world faces `-1.4 + vz*.000175` (fallback 21203, C++ `m[22]+p.z*m[23]`), debug `-1.4`, screen UI `-1.42`, net/QR `-1.43`, stats `-1.445`, `hudDepth = -1.48` (2184), inset `-1.497 + z/6000*.012` window, inset frame `-1.482`/`-1.499`. Shell maps `(z+1.5)/3` into the depth buffer, `LESS_EQUAL` (App.cpp 797, 831). `box`/`line` go to the CPU frame/D2D under every triangle (2440-2582 / `DrawVectorBackground` 2404), which is why `hudBox`/`hudLine` (2185/2192) route through `screenRect` on console.

### Camera

`FightCamDoll` 2023; `cameraDoll.prepare()` at 20364 once per paint (also on `dirty`). `mainNativeCamera` 21508 rebuilds a `new Float32Array(27)` whenever `monotonicUs` changes, i.e. once per frame: `[pos xyz(0-2), right(3-5), up(6-8), forward(9-11), centerX(12), centerY(13), orthoScale(14), focal(15), perspective(16), cameraNear(17), bandMinX(18)=0, bandMinY(19)=0, bandMaxX(20)=viewWidth, bandMaxY(21)=viewHeight, depthBase(22)=-1.4, depthSlope(23)=.000175, light xyz(24-26)]`. C++ projection (ScenePrimitives.inc 67): `k = m[14] + (m[15]/z - m[14]) * m[16]`; `x = m[12] + vx*k`, `y = m[13] - vy*k` — identical to `projectView`'s lerp of ortho and perspective. Note the C++ band is the exact viewport, the JS band is ±1 viewport (`guardBand`); harmless, C++ clips.

Native compositing order (App.cpp `Render` 2427-2700): CPU frame (rects, images, lines, block text) or D2D fast path → upload as scene texture → clear depth (2584) → `DrawGpuTriangles` 2587 (all `Triangle`s, one vertex map, 819-857) → `DrawGpuTexturedTriangles` → `DrawGpuSprites` → `DrawGpuThemeQuads` 964-1013 (6 passes by asset, soft passes depth-test without write) → D2D system texts/glyphs on top (2591-2675) → `DrawPostProcess` 2676. JS is timed as `hostJsMs` (sim + paint, 560-594); D2D text and the vertex map are `renderCpuMs`.

## 2. Retained native scene

### API (phase 1 shape, all gated on `typeof meshUpload === "function"`)

```
meshUpload(vertices: Float32Array, faces: Float32Array, capsules?: Float32Array) → handle:int   // once per map/key
meshDraw(handle, camera: Float32Array(27), tintR?, tintG?, tintB?) → trianglesEmitted            // per view
meshFree(handle)
```

Layouts (keep today's so `nativeMeshBuffers` 21504 needs no change):
- vertices: `[x,y,z]` × N (world units).
- faces: `[i0,i1,i2,i3, r,g,b, nx,ny,nz]` × F (10 floats, quads only — `captureQuadMesh` 21172 only intercepts `worldQuad`; `worldTriangle` calls inside captured geometry are not captured today either).
- capsules (new): `[x1,y1,z1, x2,y2,z2, worldWidth, r,g,b, depthBias]` × C (11 floats). Today `captureQuadMesh` stores `args[6] /= cameraScale()` (21175) and `drawQuadMesh` multiplies back by the current `cameraScale()` (21187), so `worldWidth` is exactly that stored value; C++ computes `widthPx = max(1, worldWidth * m[14])` (orthoScale = `cameraScale()`), projects both ends through the same camera with the `worldSegment` near-clip (17851-17857), sets depth `min(za,zb)*m[23] + m[22] + depthBias` (bias −.004, 17876), and reuses `SceneCapsule`'s emitter (ScenePrimitives.inc 37-50) with screen endpoints.
- camera: the 27 floats above, unchanged, so `mainNativeCamera` and the inset camera work as-is. Make both reuse one preallocated `Float32Array(27)` each (27 element writes/frame is fine; `new Float32Array` per mesh per frame is not).

C++ side (`ScenePrimitives.inc`): a `struct RetainedMesh { std::vector<float> verts, faces, capsules; float aabb[6]; std::vector<uint8_t> litColor; }` table in `CallScope`/engine; upload copies (input ownership stays with QuickJS, as `SceneFloats` already notes), validates indices once (move line 64's loop here), computes the AABB, and pre-lights each face with the constant light (finding 5) into `litColor`. `meshDraw`: transform the 8 AABB corners, reject if all behind near or all outside one band edge (this is exactly the JS test at 21194-21199, moved), then the existing per-face loop with `ScenePoint poly[9]` on the stack instead of vectors (65-68), and `points` as a per-engine scratch `std::vector` that is `resize`d, not reallocated. Tint multiplies `litColor` (booster pad pulse, theme light/dark mixes like `hall.wall` fog — optional).

Phase 2 adds:
```
sceneDraw(camera, viewMask) → emitted    // draws every resident handle whose flags & viewMask, C++ AABB cull
meshFlags(handle, flags)                  // bit0 main view, bit1 inset, bit2 hidden
meshDrawAt(handle, camera, x,y,z, yaw,pitch,roll, scale, tintR,tintG,tintB)   // instanced rigid prop
```

### What JS still computes per frame

- The 27-float camera (once; already cached on `monotonicUs`) and the inset camera (eye from `players[0]` pose, 21420-21422).
- Which handles are resident/visible: key changes only (`pipeSurfaceKey`, `pipeWallKey`, `hallMeshKey`, `waterMeshKey`, `loop.meshKey`, `cityBlocks` membership). On a key change: recapture in JS (unchanged), `meshFree(old)`, `meshUpload(new)`. These are rare (glass/roof/cube breaks, LOD tier change for loops, map change).
- Dynamic world geometry that stays on the JS path: decals (≤45 quads/frame; candidate for an append-only handle later), roof shards, dust, particles/splashes/impacts, spot shadows, chains, grass lines, ledges (4 quads), booster pads, drone/drop, pickups. None of these is large.
- Props drawn as instances (phase 2): per board/wheel a `meshDrawAt` with pose from `skateFrame`/`spunSkateFrame` (18716) — JS computes the 6-DOF pose, not the 30 quads.
- Text and HUD (section 4).

### Culls that move into C++

| JS cull today | Where | C++ replacement |
|---|---|---|
| 8-corner bounds test (fallback only) | `drawQuadMesh` 21191-21199, `rebuildSeatInset` 21446-21450 | AABB vs near plane + band in `meshDraw`/`sceneDraw` |
| `bandContains` × 4 fast path / `clipScreenBand` | `drawQuadMesh` 21207-21209, `worldQuad` 17928-17931 | already `SceneClip` against `m[18..21]` (ScenePrimitives.inc 68); add the all-inside fast path before clipping |
| per-face all-left/right/top/bottom rejection | 21204 | same as above, before allocating anything |
| `drawCityStreet` x-span (`cameraCenter ± cameraWidth*1.4`) | 21368 | AABB cull in C++; JS can still skip `meshUpload` for blocks never seen |
| near-plane fast path (`vz >= cameraNear` × 4) | 17926, 21207 | already in `SceneClip`; make it a compare before the clip walk |
| `terrainPass` span filter (`inSpan`) | 18082-18085 | chunked terrain handles (one per 8 tiles) + AABB |
| skateboard `nearSide` face ordering | 18732 | depth buffer does it; draw all faces |

### Two views

One resident set drawn twice: `sceneDraw(mainCamera, 1)` in `gamePaint` where the park draws today (20384-20387), and `sceneDraw(insetCamera, 2)` inside `drawSeatFirstPerson` replacing the mesh loop at 21444-21461. `waterMesh` gets flags `1 | (player.underPipe ? 2 : 0)` (today's `...(player.underPipe?[waterMesh]:[])` at 21444). The inset's clip rect and depth window already ride in camera slots 18-23, so C++ needs nothing new for it. The inset's kids stay on `triangles3d` (section 3, phase 3).

Budget check: indoor world ≈ pipe 36 + wall 37 + hall 91 + water 153 + ~5 city blocks 145 + loop 256 = ~720 quads ≈ 1440 tris per view; two views ≈ 2900 of 8192 before fighters, kids (≤6000 cap on the inset buffer alone!) and HUD. `sceneDraw`'s return value should be summed and logged; also make `AC_NATIVE_FRAME`'s `trianglesDropped` log periodically, not once.

## 3. Instancing

Repeated geometry: skateboard deck (P1, P2, loose `ball.type==="skateboard"` boards via `drawBall` 18784), trucks (2 per board), wheels (4 per board), monowheel, city blocks (10, only 4 heights/colours), roof panes (20 identical), funboxes (6 identical), boost-pad chevrons (6 × 6), chain links, parkKids (8, but they are articulated — not instances of a mesh).

Does `instances(handle, Float32Array transforms)` pay? The `emitTriangle` note (2145-2150) says twelve typed-array writes per face were dearer than twelve C-call args; a 16-float matrix per instance is the same trap. So:

- **Rigid props: `meshDrawAt(handle, camera, x,y,z, yaw,pitch,roll, scale, r,g,b)` — 11 positional args, one call per instance.** A board is 3 handles (deck, truck, wheel) → 1 + 2 + 4 = 7 calls per board instead of ~30 `worldQuad` + 24 `worldCapsule` + 16 `disc3d` + 8 tris (~280 JS projections and ~110 native calls). C++ builds the rotation (yaw about y, pitch about z, roll about x — match `spunSkateFrame`/`skateFrame` order; verify against 18716 before wiring) and runs the retained-mesh path. Cracks (18748-18753) are per-board and rare: bake into a per-board deck handle when `boardCondition` changes. Wheel spin marks (18700-18705): draw the wheel handle with `roll = spin` so the marks rotate; the near/far shade difference (`side===nearSide`) becomes the tint args.
- **Static repeats (panes, cubes, blocks, chevrons): do not instance.** They are already baked into per-map handles; instancing only saves upload memory, which is nothing here.
- **Kids: no mesh instancing — but share the segment list.** Both the main view (`drawRunner`) and the inset (`rebuildSeatInset` 21482-21492) need every kid's `runnerWorldGeometry` segments as world-space capsules. A native `segments3d(buffer, count, camera)` with 11 floats/segment `[x1,y1,z1,x2,y2,z2,worldWidth,r,g,b,depthBias]` lets JS fill one buffer per kid per frame (or per 30 Hz tick) and draw it with two cameras. Typed-array writes cost, but 11 writes per segment × ~20 segments × 9 bodies ≈ 2000 writes vs. today's inset path (4 object literals + 2 `local()` allocs + clip + 12 pushes per segment, then a whole-array `Float32Array` copy). If writes still hurt, C++ can read a plain JS array of numbers via `JS_GetPropertyUint32` (slower per element than a typed array but avoids the `values.push` + copy); measure both. This overlaps the character-side researcher's lane — coordinate the buffer layout with them so `drawRunner` and the inset use the same fill.
- **Native-side instance lists with deltas**: only worth it for kids (positions change every frame anyway) — no. For loose boards (rare, ≤3) per-call `meshDrawAt` is fine.

## 4. HUD and text

Cost split: HUD boxes/lines are 2 `triangle3d` each at `hudDepth` (cheap; `drawStatusPiano` is 12 tris; the debug frame meter is the only heavy one and is debug-only). Text is where the cost is, and it is in two places:
- JS: string formatting per frame (finding 9), `handleWidth` (15775: `[...handle].reduce` allocates per call), `typeWrite` → `comicWrite` marshals a C string per call.
- Render side (`renderCpuMs`, not the 24 ms): `drawPackagedFont` (App.cpp 2618-2655) recomputes glyph indices and metrics for every string every frame, and `drawPlayerHandle` submits one string per glyph per pass. `kMaxSystemDraws = 128` drops the rest silently (`systemDropped`).

Retained HUD widgets (native widgets updated by value) are **not worth it**: boxes are already on the triangle pass at the right depth (2178-2201 explains why), and a widget tree would just move the string formatting. Do instead:
1. Restore the 10 Hz string cache for the seat HUD: build `measure/verb/item` into `player.hudText` every 100 ms (the pattern at 5284-5296), and format `fps` at 2 Hz. Saves the per-frame `Math.round(...)+' bpm'` churn and the per-frame C-string marshal — ~0.2-0.3 ms JS.
2. Native glyph-run cache keyed `(family, emSize, text)` with a small LRU in `drawPackagedFont`, so a stable string costs one `DrawGlyphRun` and no metrics lookups. Render-side win only.
3. `comicWriteRun(text, x, y, size, Float32Array rgbPerGlyph)` (or `Uint8Array`) for nameplates so `drawPlayerHandle` submits one draw per pass instead of one per glyph — halves system draws and removes the 128-cap risk in fights. Optional; freeskate does not draw nameplates.
4. Keep `box`/`line` for HUD off the console path (they land under the world); nothing to change — `hudBox`/`hudLine` already do this.

## 5. Phased plan

Web shell rule for every phase: all native names are gated by `typeof x === "function"`; the JS branches that exist today (`drawQuadMesh` fallback 21190-21212, `rebuildSeatInset` fallback 21463-21473, `drawSkateboard` as is) remain the web/Mac path. `xbox/live/mac-test.html` (578) defines only `triangle`; `macos-native/main.swift` (802, 877) defines only `triangle3d`; `scene3d.mjs`/`scene3d-webgl.mjs` are a triangle sink with the same `(z+1.5)/3` depth mapping and need no change. No WebGL twin is required for correctness; one could later implement `meshUpload/meshDraw` in JS on top of `OskiewarScene3D.triangle` if the web ever wants the same call shape.

### Phase 1 — one day, shippable, ~2.5 ms

JS/C++ API: `meshUpload(vertices, faces, capsules) → handle`, `meshDraw(handle, camera, r?, g?, b?)`, `meshFree(handle)` in `ScenePrimitives.inc`, registered next to `sceneMesh` (QuickJsEngine.cpp 971). Keep `sceneMesh` for one release as the fallback when upload fails (returns −1).

JS changes:
- `captureQuadMesh` 21172: emit capsules as a flat 11-float array alongside `mesh.capsules`; compute `mesh.bounds` at capture (fixes finding 2 for the fallback too).
- `nativeMeshBuffers` 21504: add `mesh.nativeHandle = meshUpload(vertices, faces, capsules)`; on rebuild of a keyed mesh, `meshFree` the old handle (`pipeSurfaceKey`, `pipeWallKey`, `hallMeshKey`, `waterMeshKey`, `loop.meshKey`, `cityBlocks`).
- `drawQuadMesh` 21186: native branch becomes `meshDraw(mesh.nativeHandle, mainNativeCamera())` — no capsule loop.
- `mainNativeCamera` 21508 and the inset camera 21459: write into two preallocated `Float32Array(27)`s.
- `rebuildSeatInset` 21444-21461: `meshDraw(handle, insetCamera)` per mesh; the C++ AABB cull replaces the dead JS bounds test; skip city blocks outside `[eye.x − 3500, eye.x + 3500]` in JS (cheap pre-filter).
- `drawIndoorHall` 757: draw the retained hall even while `roofShards.length`, and draw only the shard loop (831-838) in JS.

C++: per-handle validation and AABB at upload; pre-lit colours; stack `ScenePoint[9]` clipping; all-inside fast path.

Expected: capsule replay −1.0..−1.5 ms, inset uncullled re-sends and allocations −0.5, C++ alloc churn −0.3, roof-shard spike gone. Measure with the existing per-function laps and `AC_NATIVE_PROFILE hostJsMs`.

### Phase 2 — two to three days, ~3.5 ms

API: `sceneDraw(camera, viewMask)`, `meshFlags(handle, flags)`, `meshDrawAt(handle, camera, x,y,z, yaw,pitch,roll, scale, r,g,b)`.

- Replace the 13 `meshDraw` calls in the main view and the inset loop with `sceneDraw(mainCamera,1)` at 20384 and `sceneDraw(insetCamera,2)` at 21444 (−0.3 ms of call overhead and JS loop; the cull is already native from phase 1).
- Skateboard as three handles (deck per board when cracks change; one shared truck; one shared wheel) drawn with `meshDrawAt` in `drawSkateboard`/`drawBoardRunningGear`/`drawTruck`; monowheel as one handle. −1.5 ms (`drawSkateboard` 1.9 → ~0.4 with 2-3 boards).
- Outdoor course: upload the thinned `terrainDrawProfiles` (17976) as chunked strip handles per LOD (surface/front/back, ~8 tiles per chunk, AABB each) and pick the LOD tier in JS as `activeTerrainProfile` does (17987); indoor is already retained. Small in the park; matters on the 320-tile course and in wide shots.
- Ponds/boost chevrons/ledges on the outdoor course: capture with `captureQuadMesh` like the loops (they are static; today they are re-projected per frame at 1457-1486). −0.3 ms outdoors.
- Decals: `meshAppend(handle, vertices, faces)` for an append-only per-map decal handle; blood/ding fans stay JS (they need the affine screen placement). −0.3 ms in long rounds.
- Log `sceneDraw`'s emitted count and periodic `trianglesDropped`.

### Phase 3 — kids, text, polish, ~2 ms

API: `segments3d(buffer, count, camera)` (11 floats/segment), `comicWriteRun(text, x, y, size, colors)`, native glyph-run cache.

- Inset kids: fill one segment buffer per body (shared with the main-view character path if the character lane adopts the same layout) and draw it with the inset camera; drop the 30 Hz `values`/`Float32Array` rebuild and `triangles3d` replay. Heads stay as `disc3d` with the inset projection or become 3 segments each. −1.5 ms of the inset's 4.2.
- Seat HUD strings at 10 Hz, FPS at 2 Hz; `handleWidth` without the spread. −0.3 ms JS.
- Glyph-run cache and per-run coloured `comicWriteRun`: render-side only, but it also lifts the 128-draw cap risk in fights.
- Optional tint use: booster-pad pulse and the hall's `hallAir` fog become tint args instead of colour rebuilds.

Cumulative world-side estimate: ~8 ms of the 24 (phase 1 ≈ 2.5, phase 2 ≈ 3.5, phase 3 ≈ 2). Combined with the character lane this is the path to <16 ms; the retained scene alone does not get there because the fighters, kids and text are the other half of the frame.
