# oskiewar engine split — research

Read-only survey, 2026-09-26. Paths: `day` = `/Users/jas/ac-worktrees/oskiewar-day-perf`, `perf` = `/Users/jas/ac-worktrees/oskiewar-perf`, `main` = `/Users/jas/aesthetic-computer`. Line numbers are from `day/xbox/live/oskiewar.js` (21,546 lines, `buildVersion = 167`) unless a file is named.

## 0. What exists today (measured, not assumed)

**Three diverging copies.** `day` is branch `oskiewar-day-perf` (game side: park, LOD, inset — +4,606/−384 lines over `perf`'s copy). `perf` is `oskiewar-native-on-main` (C++ side: `disc3d`/`capsule3d`/`sceneMesh` at e822dd9e46, post pass at 78f2f08799, package 1.0.0.48 at c2cf815b6d). `main` has *uncommitted* edits to both `xbox/live/oskiewar.js` and `xbox/native-bios/*`. Any plan below lands native first, because the JS feature-detects.

**Frame on console.** `App.cpp:570-575` calls `sim` then `paint` through `QuickJsPiece::Call` (`QuickJsEngine.cpp:1040-1050`); `hostJsMs` (`App.cpp:578-593`) is the two together, averaged over 120 frames. Draw calls land in `HostGraphics::on_triangle` → `m_frameTriangles` (`App.cpp:371-374`, cap `kMaxTriangles = 8192` at 2753); `DrawGpuTriangles` (`App.cpp:819-856`) maps one vertex buffer and issues one `Draw`. Render CPU is 1.6 ms (084affb4d7). The GPU is idle; the interpreter is the frame.

**Phase history from commit bodies** (Series X, `AC_NATIVE_PROFILE` + `xbox/tools/oskiewar-phase-profile.py`):

| build | hostJsMs | what moved |
|---|---|---|
| v142 | 40 (23 fps) | terrain 8.4 ms |
| v143 084affb4d7 | 27 title | terrain projects each point once; HUD rides triangle pass |
| v144 ef651bdbb9 | 18.8 title, ~16 fight | sim 10.5→4.7 ms (`nearRunner` reach cull); terrain 7.2→4.2 |
| park 62ce5e3613 | 36→29.4 | `rebuildSeatInset` 13-14.5→4.1 ms; `updateBall` 4.6→2.4 |
| park f062649517 | 29.4→21.4 | five full figures forced to LOD tier 1 |

Per figure at full detail: **~90 native calls, ~1.6 ms** (f062649517). Two title bots: 9.5 ms of sim. Those two numbers decide the migration order.

**Host surfaces, three rungs of one ladder.**

| host | drawing entry points | source |
|---|---|---|
| web (oskiewar.com) | `triangle` only, Canvas2D (`getContext("2d")` `mac-test.html:228`; `triangle` at 575 batches faces into one path under nonzero winding) | `new Function` injection `mac-test.html:1709-1716`; `capabilities().graphics = "CANVAS2D"` (926-960) |
| macOS native | `triangle`, `triangle3d` → Metal, one `triangleBatch(ptr, count: 1)` per face | `macos-native/main.swift:78-111`, host list 919-969; **no** `triangles3d`/`capsule3d`/`disc3d`/`sceneMesh` |
| Xbox | everything: `triangle3d`, `triangles3d`, `disc3d`, `capsule3d`, `sceneMesh`, `sprites3d`, `texturedTriangles3d`, `themeQuad`, `postEffects` | `QuickJsEngine.cpp:960-1006`, fans in `ScenePrimitives.inc` |

`xbox/live/scene3d.mjs` + `scene3d-webgl.mjs` (WebGL2, depth-tested, `OSKIEWAR_SCENE_VERSION = 1`, same `(z+1.5)/3` depth map as `App.cpp:830`) exist but are imported only by `bench/` and two tests. The shipping web page does not use them.

**Game-side feature detection** (all `typeof X === "function"`): `emitTriangle` 2151, `nativeTrianglePass` 2183, `filledDisc` 13591 (`disc3d`), `filledCapsule` 13641 (`capsule3d`), `drawQuadMesh` 21186 and `rebuildSeatInset` 21458 (`sceneMesh`), seat inset 21409/21416 (`triangles3d`), `themeReady` 13713, `comicWrite` 15782. `filledDisc`/`filledCapsule` re-evaluate `typeof` on every call — a global lookup on the hottest path.

**The `emitTriangle` comment (2145-2150)** — "~2100 faces; Float32Array first cost 5.7 ms where per-face calls cost 1.8 ms; the batched host call measured 0.05 ms." Read as rates: ~0.85 µs per 12-arg C call all-in, ~0.23 µs per typed-array element write. The lesson is not "calls are cheap"; it is **element count is the cost**, whichever side writes it. 2100 faces × 12 = 25k numbers crossing per frame. A pose is ~130 numbers. That ratio is the whole argument for §1.

**Host-object churn.** `runtime()` has 94 call sites (20 in the 16k-18k fighter region); `RuntimeInfo` (`QuickJsEngine.cpp:699-757`) builds a fresh object with ~40 properties and 3 strings every call. `capabilities()` has 42 sites, at least four inside `gamePaint` per frame (20279-20296); `Capabilities` (`QuickJsEngine.cpp:798-840`) builds ~30 properties with ~8 strings each call. The game reads only `inputFamily, socialPreview, replayOven, reelFullUi, reelHud, country, updateReady, pops, midiPulse, midi, liveAgents` — nothing version-shaped.

**The pose is simulation state.** `runnerWorldGeometry` (14132) is read by collision: `sampleCombatBoxes` 11025, `meleeLimbContact` 15274/15441/15501, `damagePart` 6957, `frozenGeometry` 9228, `fallenBodyGeometry` 11116. The `renderPoses` cache (14130) only dedups builds inside one `paint` under `sharingRenderPoses` (20840-20849). So the split line is **not** "sim vs paint" — it is *pose in world space* (game) vs *pose to pixels* (renderer).

**Test harness** (`day/xbox/live/tests/oskiewar.test.mjs`): 331 tests; 120 read `triangles/batches/boxes/lines`; 55 call `fight.paint()`. It stubs `triangle`, `triangle3d`, `triangles3d` (lines 118-139) but **not** `capsule3d`/`disc3d`/`sceneMesh` — so the suite already runs the JS fan fallbacks as the reference. Line 5763 asserts an exact capsule-fan triangle count. `quickjs_engine_smoke.cpp:59-67` exercises the C++ `disc3d`/`capsule3d`/`sceneMesh` but never compares their output to the JS. They already diverge: `SceneDisc` picks n ∈ {8,12,16,20,28} by radius; JS `discRingFor` (13586) has six tiers up to radius 110.

**Release.** `xbox/tools/oskiewar-release.mjs:29-38` `classifySeverity` → any `xbox/native-bios/` path is `"xbox-native"` and the receipt escalates instead of claiming parity; `stampBuildVersion` 189-215. `appveyor.yml` builds `native-bios` MSIX (`1.0.{build}`) on any `xbox/` change; `test-native-windows.cmd` runs `node --check oskiewar.js`, C++ contract tests, and the QuickJS smoke. `capabilities().version` comes from the literal `m_api->system.version = "1.0.0.46"` at `perf/xbox/native-bios/App.cpp:428` while `Package.appxmanifest:9` says `1.0.0.48` — the version the JS can see is already wrong.

## 1. The split and the interface

**Game (one JS source, deterministic, tested):** input → `gameSim`/`netTick` (8128) with `netSnapshot/netRestore/netStateHash` (7604-7680), rollback (`netRollback` 7926-7936), replay capture (`recordReplayCommands` 13254, `captureRoundReplay` 13453 — both inside `gameSim`), pose build (`buildRunnerWorldGeometry` 14413), LOD tier decision (`figureLod` 16978 — a rule about *what* to draw, keep it in JS), camera solve (`containFighters`, `cameraDoll.prepare()` 20365-20368), scene composition order (`gamePaint` 20247+ deciding which things exist this frame).

**Renderer (native on console, JS reference elsewhere):** everything from "world-space pose + camera" down: projection and clipping (`worldTriangle` 2307, `worldQuad` 17924, `clipPolygon` 2237), capsule/disc fans (`filledCapsule` 13640, `filledDisc` 13590), skeleton skinning (`drawSkeletonSegments` 13859, `drawPaletteCapsule`, outline pass), terrain strip tessellation (the three passes at 20383-20385), quad lighting (`litQuadColor` 17902), mesh draw (`drawQuadMesh` 21184), HUD glyph fanning, post effects.

**Interface: retained handles + per-frame small payloads, `sceneApi` levelled.** Design rule from §0: minimise numbers crossing, not just calls; never make JS write a big typed array per frame.

```js
// boot / map change — retained, returns small int handles
const rig    = figureRigCreate(segmentRoles, segmentParts);   // topology once
const mesh   = sceneMeshCreate(verticesF32, facesF32);        // already cached as mesh.nativeBuffers (21504)
const strip  = terrainStripCreate(profileF32);                // terrainDrawProfile, once per map

// per frame
sceneBegin(cameraBuf, lightX, lightY, lightZ);   // cameraBuf = the existing 27-float mainNativeCameraBuffer (21508), persistent
sceneMeshDraw(mesh);                             // 1 call, 1 int
terrainDraw(strip, nearZ, farZ, colorId);        // 1 call per pass instead of a loop of worldQuad
figureDraw(pose, styleId, tier);                 // 1 call per fighter instead of ~90
// HUD keeps screenTriangle/hudBox — few calls, already cheap
```

**How C++ reads `pose` — the four options, priced against the 2151 comment:**

| carrier | per-figure cost (≈130 numbers) | verdict |
|---|---|---|
| positional C args (today) | ~90 calls × 0.85 µs ≈ 80 µs + the JS fan work that makes them | keep for HUD; wrong for figures |
| Float32Array JS fills | 130 × 0.23 µs ≈ 30 µs of JS writes + one call | acceptable, but moves work back into the interpreter |
| **plain JS object, C++ walks it** | 0 extra JS work; C++ `JS_GetProperty` with pre-interned atoms (`JS_NewAtom` once in the constructor at `QuickJsEngine.cpp:950`) ≈ 0.1-0.2 µs × 130 ≈ 20 µs | **use for poses**: the object `buildRunnerWorldGeometry` returns is passed as-is; no JS-side copy at all |
| JSON string | `JSON.stringify` is C inside QuickJS but allocates; needs a C++ parser | only for one-time declarations (rig/mesh), never per frame |
| native-owned buffer JS mutates by index | same 0.23 µs/write as above | right for the camera (27 floats) and post effects (6), already the shape of `mainNativeCameraBuffer` |

`sceneMesh` is the model to copy: retained typed arrays cached per mesh (`nativeMeshBuffers` 21504), a camera buffer rebuilt only when `monotonicUs` changes (`mainNativeCamera` 21508), one call. It just needs the "create → handle" half so the per-frame call carries an int instead of three arrays.

**The JS reference renderer** is the current fallback code lifted into one module (`xbox/live/scene-reference.mjs`, no new behaviour): `figureDraw` = `drawSkeletonSegments` + `filledDisc` + palette pass on top of `triangle3d`; `terrainDraw` = today's pass loops; `sceneMeshDraw` = the non-native branch of `drawQuadMesh`. Web and Mac call it; the console calls native; both produce the same triangle stream. That makes "the web fallback is the reference" a checked property instead of a hope. The existing `scene3d.mjs` depth constants stay the one shared definition.

## 2. Keeping the 331 tests meaningful

Both, layered:

1. **Reference renderer under the harness (no test edits).** `createFight` keeps stubbing `triangle3d`; the game calls `figureDraw` etc. through the reference module, which emits `triangle3d`. All 120 triangle-observing tests, including the exact fan count at 5763, keep passing because the reference emits the same faces the inline fallback emits today.
2. **Scene-level recorder.** Add `figureDraw`/`sceneMeshDraw`/`terrainDraw` stubs to the harness's `new Function` list that push `{kind, handle, tier}`; new tests assert on scene items (five figures at tier 1, one mesh, three strips) — the `countPoseBuilds` pattern (test 6920-6928), counting semantic events instead of pixels.
3. **Golden cross-check native vs reference.** Extend `quickjs_engine_smoke.cpp` (it already runs `disc3d/capsule3d/sceneMesh` at 62-67) to dump the triangle stream for one canonical pose, mesh and terrain strip to JSON; a Node test replays the same inputs through the reference and diffs. Runs in AppVeyor via `test-native-windows.cmd`. This is the test that would have caught the disc-ring divergence in §0.
4. `renderer.test.mjs` (9 tests: near-plane, guard band, clipping) stays pure JS and becomes the spec for the C++ clipper in `ScenePrimitives.inc` (`SceneClip`).

## 3. Rollback, replays: what must stay simulation-only

Good already: `netStateHash` covers players/balls/bullets/grenades/round scalars (7646-7680) and no camera, pose or LOD; `netSimArrays` (7580) restores the live arrays in place; replay capture lives in `gameSim`.

Hazards the split must not carry across:

- **In-place render interpolation.** `beginRenderInterpolation` (3148-3170) lerps fields *on the live sim objects* during `paint` and restores in `finally` (20853-20855). After `netRollback` → `netRestore` the `renderPreviousState` map is stale for one frame, and any paint-side write between begin and restore leaks into the hash. Replace with an explicit frame-state (interpolated copy of positions + camera) handed to the renderer; live objects are never touched by `paint`.
- **Paint writes on sim objects.** `player.lodTier` (17001) is written during paint and rides into `structuredClone` snapshots (players are in `netSimArrays`). Move it to a `WeakMap` like `renderPoses`. Same for `lastPaintAt`/`displayFps` (20270-20275) and `figureLodScale`.
- **Wall clock inside the pose.** `runnerWorldGeometry` reads `runtime().monotonicUs` for the strafe step (14148) and that pose feeds `meleeLimbContact`. If `netSimulateFrame` drives sim with a frame-derived `now`, two seats can compute different foot positions → different hitboxes. Verify: pass `now` in, never call `runtime()` from anything reachable from `gameSim`.
- **Camera is render state, replays record it.** `recordReplayCheckpoint` stores `cameraDoll` (3945-3952) and `startReplay` stores `runtime().width/height` (3618-3628) — metadata, fine. What must hold: nothing reachable from `gameSim`/`netSimulateFrame` reads `cameraDoll`, `projectPoint`, `viewWidth()`. Grep those three inside sim-reachable functions before the split; `runnerScreenBounds` is the likely offender.
- **Instant replay stores poses** (`makeRoundReplayFrame` 3996 `geometry: runnerWorldGeometry(...)`, blended by `blendReplayGeometry` 4045). This is fine because the pose stays in JS; it is also why the pose object shape is part of the replay format and must not change when `figureDraw` arrives.

Rule: the renderer receives (frame-state, poses, camera, handles) and returns nothing the game reads. `sceneMesh` returning `emitted` (`ScenePrimitives.inc` `SceneMesh`) is already a small violation; the game ignores it, keep it that way.

## 4. Versioned capabilities across two release lanes

- Native advertises `sceneApi: N` in `Capabilities` (`QuickJsEngine.cpp:798`); fix `system.version` to read `Package::Current->Id->Version` instead of the literal at `App.cpp:428`. Mac's `capabilities` block in `main.swift` advertises its own level (1 today).
- JS resolves once at boot: `const sceneApi = Number(capabilities().sceneApi) || (typeof triangles3d === "function" ? 1 : typeof triangle3d === "function" ? 0.5 : 0)`; every level-N call site keeps its level-(N−1) path; level 0 is the JS reference. Newer JS on an older console degrades to the reference. Newer console on older JS works because the API is additive: never change an argument layout, add `figureDraw2`. No `typeof` in per-call paths — hoist to module constants like `emitTriangle`.
- Release gate: `oskiewar.test.mjs` already reads `App.cpp` as source (line 37, `nativeApp`). Add a source-shape test: `REQUIRED_SCENE_API` in `oskiewar.js` ≤ the `sceneApi` literal in `App.cpp`. `oskiewar-release.mjs` then cannot ship a JS that needs level 2 against a native advertising 1.
- Retail console (`GDK-PORT.md` §3, policy 10.2.5/XR-009) cannot hot-load JS: JS and native ship together in the MSIX. The dev lane (`deploy-xbox-dev`, Device Portal) is where JS leads native by days — that is exactly where the gate above earns its keep.
- Land order for the three copies: `perf` (native) → merge into `main` → `day` (JS) on top, so the console always has the calls before the JS asks for them.

## 5. End-state and migration order

**End state.** `oskiewar.js` = rules, sim, net, replay, pose, LOD decision, camera solve, scene composition; it emits handles and small payloads through a levelled `sceneApi`. `scene-reference.mjs` = the JS renderer (web via `scene3d-webgl.mjs`, Mac via Metal, tests via the harness). `ScenePrimitives.inc` + a `figure`/`terrain` unit in C++ = the console renderer, golden-checked against the reference. `App.cpp` keeps owning the frame, D3D11 today, D3D12 on GDK (GDK-PORT §1a) without touching the JS. Each step below is measurable as `AC_NATIVE_PROFILE hostJsMs` on the Series X, and on a desk with `JSC_useJIT=0` under the Mac host (`main.swift:781-790`, which times sim and paint separately — App.cpp does not).

| step | work | saves (in-fight, Series X) | ms/day |
|---|---|---|---|
| 0 | Cache `runtime()`/`capabilities()` once per `sim` and per `paint`; hoist the six `typeof` checks to module constants; fix `system.version` | ~0.3-0.5 ms + less GC; zero risk | free |
| **1** | **`figureDraw(pose, styleId, tier)`** in C++ reading the pose object by atom; reference = today's `drawSkeletonSegments` path lifted into `scene-reference.mjs`; tests unchanged | ~1.2-1.4 of the 1.6 ms per figure → 2.5-7 ms with 2-5 figures on screen | **best** |
| **2** | `terrainStripCreate/terrainDraw`: retained profile, C++ runs the three passes with `SceneClip` | ~3 of terrain's 4.2 ms | second |
| **3** | Seat inset: kids through `figureDraw` with the inset camera; the 30 Hz `seatInsetBuffer` rebuild goes away | ~3 of 4.1 ms (park only) | third |
| 4 | Frame-state snapshot replaces in-place interpolation; render-only fields off `player` | 0 ms; buys §3 | do with 1 |
| 5 | Web shell → `scene3d-webgl.mjs` + reference; Mac gains `triangles3d` | 0 console ms; makes "web = reference" literal, gives web a depth buffer | after 1-3 |
| 6 | Title bots (9.5 ms of sim): plan every N ticks, cheaper `botScene` | title only | not renderer work |

Steps 1-3 together take the measured in-fight ~16-24 ms toward the target with the GPU doing what it is idle for now; 0 and 4 are the discipline that keeps the game one deterministic source while it happens.
