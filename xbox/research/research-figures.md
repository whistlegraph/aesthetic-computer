# oskiewar figure rendering — research report

Sources read (read-only):
- JS: `/Users/jas/ac-worktrees/oskiewar-day-perf/xbox/live/oskiewar.js` (21,546 lines; line numbers below are from this file)
- Native: `/Users/jas/ac-worktrees/oskiewar-perf/xbox/native-bios/{QuickJsEngine.cpp,ScenePrimitives.inc,App.cpp}`, `/Users/jas/ac-worktrees/oskiewar-perf/xbox/runtime/include/ac/runtime.hpp`
- Harness: `xbox/live/tests/oskiewar.test.mjs` (`createFight`, lines 93–190)
- Web shell: `xbox/live/mac-test.html` (Caddy rewrites oskiewar.com to it; Canvas2D `triangle` at line 578). Mac shell: `xbox/macos-native/main.swift` (only `triangle3d`, no `capsule3d`/`disc3d`).

Measurements below come from a scratchpad harness that loads oskiewar.js the way the tests do, stubs `capsule3d`/`disc3d`/`triangle3d` as counters, and times `drawRunner` under V8 (scripts: `scratchpad/count-figure-calls.mjs`, `scratchpad/breakdown-figure.mjs`). V8 is JIT; multiply by ~10 for QuickJS (0.150 ms V8 for a pastel skater vs the 1.6 ms measured on the Series X in commit f062649517).

## 0. Where the frame goes today

Native calls per `drawRunner` (one figure, 1080p, in-fight camera, head radius 57 px):

| figure | tier | capsule3d | disc3d | triangle3d | total calls | V8 ms |
|---|---|---|---|---|---|---|
| P1, no skin (fight) | 0 | 57 | 8 | 117 | 182 | 0.064 |
| P1 pastel skater | 0 | 74 | 45 | 395 | 514 | 0.150 |
| any | 1 | 24 | 4 | 0 | 28 | 0.011–0.016 |
| any | 2 | 12 | 1 | 0 | 13 | 0.005–0.011 |
| any | 3 | 1 | 1 | 0 | 2 | 0.008–0.013 |

Per-subsystem cost for the pastel skater at tier 0 (calls / V8 µs):

| pass | function | calls | V8 µs | notes |
|---|---|---|---|---|
| pose build | `buildRunnerWorldGeometry` 14413 | 0 | 1.9 | |
| decorate | `runnerWorldGeometry` 14132 | 0 | +0.1 | breath, strafe, spin, turn, skate rotation |
| IK constrain | `constrainLimbs` 14879 | 0 | +2.4 | fixed bone lengths |
| project | `projectRunnerWorldGeometry` 14914 | 0 | +1.1 | 23 `projectPoint` calls, each allocating |
| ponytail | `drawPonytail` 934 | 40 | 13.6 | 7-node Verlet, 5 passes, Catmull-Rom |
| silhouette | `drawFighterSilhouette` 13930 → `drawCurvedLimbs` 13799 | 136 | 9.5 | 4 cubic chains, 3–7 strokes each, ×2 passes |
| outfit + skirt | `drawOutfit` 1228, `drawSkirt` 1292 | 115 | 18.2 | 6×4 cloth Verlet, 3 passes, capsule separation |
| hair cap | `drawHairline` 1100 | 48 | 3.5 | 19-point fan + 20 edge strokes |
| bow | `drawBow` 1145 | 64 | 7.7 | two 7-node ribbon Verlets |
| face | `drawFace` 16407 → `drawTrioFace` 16305 | 111 | 12.9 | sphere-mapped features, blink, iris |
| scars, inventory | 1411, 16694 | 0 | 0.2 | |

The no-skin P1 is 152/182 calls of hair: `drawRunner` 17121 and 17130 give **pad 0 the ponytail, hairline and bow even without a skin** (`player.skin || player.pad === 0`). P2 flat is ~30 calls plus palette bands when `handleColors` is set (`drawPaletteCapsule` 13889: 6 bands × 2 tris + 2 discs per segment = 154 `triangle3d`/`disc3d` for 11 segments).

The pose itself is cheap (~5 µs V8 ≈ 0.05 ms QuickJS). The draw is the cost. Two things dominate: (1) tessellation work done in interpreted JS before every native call (Bezier/Catmull-Rom sampling, fans, cloth), and (2) the sheer count of boundary crossings with 7–12 `JS_ToFloat64` args each.

The `emitTriangle` note at 2146–2151 is load-bearing for every design below: in QuickJS, twelve typed-array element writes cost more than twelve positional args to one C call (5.7 ms vs 1.8 ms per 2100 faces → ~0.23 µs per element write, ~0.86 µs per 12-arg call). So any design where JS *builds a big buffer* per figure is a loss; JS must send a small buffer (≈100 floats ≈ 25 µs) and C++ must generate the geometry.

## 1. What defines a drawn figure

### 1a. Pose (changes every frame)

World-space skeleton from `buildRunnerWorldGeometry(player, t, at)` 14413–14867:

```
{ head: { x, y, z, radius: 22 },
  segments: [ { x1,y1,z1, x2,y2,z2, width (10|11|12), role, part, hitboxOnly? }, ... ] }
```

Standard pose is **11 segments + head**: `neck, torso, shoulders, lead-thigh, lead-shin, rear-thigh, rear-shin, left-upper-arm, left-forearm, right-upper-arm, right-forearm`. Roles vary by state (`attack-*`, `item-*`, `grab-*`, `rest-*`, `left-*/right-*`, `lead-*/rear-*`) — 27 role strings appear. `part` ∈ {torso, left-arm, right-arm, left-leg, right-leg} (`limbParts` 10066) and is what `hasPart`/`removedParts` gate on (10069). Alternate builders: `hangGeometry` 14370 (coping stall), `wallPressGeometry` 14090, `spiderDummyWorldGeometry` 14815 (2 body + 40 leg + 8 hitbox-only = 50 segments).

Inputs the pose reads (all per-frame sim state on `player`): `x,y,z,vx,vy,facing,grounded,ducking,crouchBlend,attackKind/attackUntil/attackStartedAt,itemAction/itemActionUntil,gunAmmo/gunAimX/gunAimY/itemAimLocked,grabHeld,heldBall,ropeIndex,rig{lean,crouch,arms},skateboard/skatePitch/skateRotation/skateGrab/skateStall/trucks{tilt},onewheel,swimming/swimPhase,spin{angle},jumpLaunchAt,landPoseUntil,motionClock,breathPhase,heartDanger,strafe{from,to,at,frames},turnAt,pad,removedParts` plus terrain probes (`surfaceYAt`, `terrainFloorAt`), `meleeTarget`, `itemHandTarget`, `skateFrame`, `fighterAnimationPhase` 14000. This is game logic, entangled with hitboxes (`sampleCombatBoxes` reads the same builder) — it stays in JS in every design.

`runnerWorldGeometry` 14132 decorates (breath lift, strafe footwork, spin arm opening + Y rotation, civilian `bodyScale`, turn yaw, skate/surf/swim roll) and caches per paint in `renderPoses` (`sharingRenderPoses`). `constrainLimbs` 14879 re-solves every free 2-bone chain to fixed lengths (`boneLength` thigh 36, shin 35, upper-arm 31, forearm 31) — derived. `projectRunnerWorldGeometry` 14914 projects 23 points and multiplies widths by `cameraScale()`.

Screen geometry consumed by every draw function:

```
{ head: { x, y, depth, radius },
  segments: [ { x1,y1,x2,y2, depth, width, role, part, hitboxOnly } ] }
```

Depth: the paint loop (20560–20567) sets ONE `triangleDepth` per figure = min(feet depth, depth at `worldNear+2`). Layers offset from it: face −.012, pupils −.02 (`drawFace` 16410/16532), spin arms ±.03/.015 (`drawSpinArms` 21247), held axe via `worldCapsule` −.004 (17875), HUD −1.48. Per-segment `depth` is only used to sort skateboard limbs (13868) and to split spin arms front/back.

### 1b. Style / identity (changes rarely)

| field | used by | default |
|---|---|---|
| `color` [r,g,b] | body fill, head (skin) | roster color |
| `handleColors` [[r,g,b]…] | `drawPaletteCapsule` bands (only when no skin, not npc) | [] |
| `skin` `'pastel'`\|null | switches to curved limbs, trio face, outfit, ponytail | null; `dressFreeskater` 5703 sets it; parkKids 21262 |
| `shirtColor, pantsColor, skirtColor, shoeColor, hairColor, accent (ribbon), irisColor` | outfit/hair/face | see 1228–1240, 5701 |
| `emo` | `drawEmoFringe` 21119 (+ dark palette 5711) | false |
| `hairVariant` 0–2 | civilian ponytail (939) | |
| `civilian, bodyScale` | cheap face/skirt/ponytail branches | |
| `dummy` | grey [192,188,179], X eyes; `drawRunner` 17060 clones the player object every frame | |
| `npc, bot` | bot brows, dead-eye X | |
| `name === "@FIFI"` | special hair (16447) | |
| `pad` | pad 0 wears ponytail/hairline/bow without skin (17121, 17130); blink/sway phase seeds | |
| generated appearance (`__oskiewarFighterAppearance`, 13720) | shirt/pants/shoes/skin/hair/beard/glasses | null |

### 1c. Damage and transient state (changes on events; drawn every frame)

`removedParts[]`; `partDamage{part:n}` → `damagedPartColor` mix toward red (13710); `scars[]` `{part, role, t, angle, at}` (created lazily in `drawScars` 1411); `hit > 0` → yellow outline; `hitSegment/hitSegmentUntil` → 45 ms strobe capsule (17132); `blocking` + `blockFlash` → bubble shield, 34 capsules + 1 disc (`drawBubble` 17165); `alive`/`headBustedAt` → `drawBrokenRunner` 16903; `resultReaction` (DANCE/POSE/… shifts geometry 17098–17109 and picks mouth/tears/hearts); `frozenGeometry`/`replayGeometry`/`fallenBodyGeometry` (pre-built world poses); held items (`swordHeld, axeHeld, gunAmmo, gunMode, itemAction`) via `drawInventory` 16694 / `drawHeldAxe` 21108; `skateboard/onewheel` via `drawSkateboard` 18712 (world quads, separate object).

Face inputs (`drawFace` 16407): `facing, alive, hit, blocking, attackKind + meleePulse, resultReaction(+At), spitAt, headRoll, bot/npc, deathCinematic winner/loser, inputPads[pad].leftX/leftY + digital arrows (gaze)`, clock `t` for blink/smile sway; LAUGH and victory draw glyphs through `systemWrite` (D2D text lane, 16563/16589).

### 1d. Purely derived (recomputed from pose + style + camera; never needs to cross the boundary)

- `edge = clamp(cameraScale*1.8, 1.25, 3)` outline width (13864 and 6 other places).
- Disc tessellation: JS `discRingFor` 13586 uses 6/8/12/16/24/32 sides at r<6/13/26/52/110; C++ `SceneDisc` uses 8/12/16/20/28 at the same first four thresholds — **console and web already differ triangle-for-triangle**; the parity test at tests 5798 only compares harness hosts.
- Capsule caps: 3/4/6/8/12 half-ring steps at r<6/13/26/52 (JS `capsuleArcs` 13639, C++ `SceneCapsule` identical).
- Curved limbs: cubic Bezier through root/joint/end with clock wobble seeded by role/part string lengths (13815–13830), 3–7 steps by `length/18` (3 for civilians).
- Hair-cap fan (19 points), shoe/sleeve/shirt/daisy placements, skirt mesh topology, all facial feature positions, lash/brow curves — deterministic functions of head radius, facing, clock.
- **Stateful but derived**: ponytail Verlet (`player.hair.nodes`, 7 nodes, link 11, 5 passes), ribbon Verlets (`hair.ribbons`, 2×7 nodes), skirt cloth (`player.skirtCloth`, 6×4 nodes, 3 passes with thigh-capsule separation). These carry velocity across frames; whoever draws them must own that state.

## 2. Architectures

Common ground: the native primitives already exist — `SceneDisc`/`SceneCapsule` in `ScenePrimitives.inc` (bound at QuickJsEngine.cpp 969–970), feeding `Graphics::triangle` → `m_frameTriangles` (App.cpp 372, cap `kMaxTriangles = 8192` at 2753) → `DrawGpuTriangles` 819 maps into one dynamic vertex buffer (`GpuTriangleVertex` {x,y,z,r,g,b,a}, NDC = x/960−1, 1−y/540, depth (z+1.5)/3, LESS_EQUAL) and one `Draw`. `SceneMesh` (ScenePrimitives.inc 43–72) already takes a 27-float camera (`mainNativeCamera()` 21508: position, right, up, forward, centerX/Y, orthoScale, focal, perspective, near, viewport, depthBase, depthScale, light) and projects+clips in C++. Native `renderCpuMs` is 1.6 ms for the whole frame; GPU is idle. There is no per-object handle/class precedent in QuickJsEngine.cpp (`CallScope { Api* api; }` only) — a style/instance registry would be new but trivial (a `std::vector` on `CallScope` or a static in the .inc).

### 2a. Native figure builder (recommended)

JS API:

```js
// once per look (on dress / roster apply / parkKids reset), returns int handle
const style = figureStyle(JSON.stringify({
  kind: "flat"|"pastel"|"dummy"|"civilian",
  skin:[r,g,b], outline:[8,12,24], shirt:[..], pants:[..]|null, skirt:[..], shoes:[..],
  hair:[..], accent:[..], iris:[..], palette:[[..],..]|[], emo:0|1, hairVariant:0..2,
  ponytail:0|1, hairCap:0|1, bow:0|1   // pad-0 rule resolved in JS
}));
// per frame per figure
figure(style, pose /*Float32Array*/, state /*Float32Array*/, tier, depth);
```

`pose` layout (screen space in phase 1, 4 + 8·N floats, N ≤ 64):

```
[0] head.x  [1] head.y  [2] head.radius  [3] segmentCount N
[4+8i+0] x1  [+1] y1  [+2] x2  [+3] y2  [+4] width
[+5] roleId  (enum: 0 neck,1 torso,2 shoulders,3 thigh,4 shin,5 upperArm,6 forearm,7 spiderBody,8 spiderLeg,9 other)
[+6] partId  (0 torso,1 leftArm,2 rightArm,3 leftLeg,4 rightLeg)
[+7] flags   (bit0 hitboxOnly, bit1 chainStart[upper of a 2-bone chain], bit2 free[not attack/item/grab])
```

`state` layout (24 floats): `facing, clockSeconds, hit(0/1), hitSegment(-1..), hitStrobe(0/1), blocking, alive, dead-eyes(0/1), spinAngle, headRoll, blink(0/1), gazeX, gazeY, mouth(enum: smile, grin, ring, line, frown, spit), grin/open amount, damage[5] (per part, 0..3), scarCount, scarsOffset…` — scars are appended as `(partId, roleId, t, angle, heal)` quintuples after the fixed block.

C++ sketch (new file `xbox/runtime/figure/FigureBuilder.hpp/.cpp`, included by `ScenePrimitives.inc`; bind `figureStyle`/`figure` beside line 970):

```cpp
struct FigureStyle { uint8_t kind; Color skin, outline, shirt, pants, skirt, shoes, hair, accent, iris;
                     std::vector<Color> palette; bool hasPants, emo, ponytail, hairCap, bow; int hairVariant; };
struct FigureSink { Graphics& g; float depth; void capsule(...); void disc(...); void tri(...); void stroke(...); };
void drawFigure(FigureSink&, const FigureStyle&, const float* pose, const float* state, int tier, float edge);
// tier 3: torso capsule + head disc                          (drawFigureLod 17010)
// tier 2: outline-less capsules, shirt width, head, hair band (17020-17036)
// tier 1: + outline pass, eyes                                (17024, 17038)
// tier 0: flat kind  -> straight capsules ×2 passes, palette bands, head+edge, default face
//         pastel kind -> curved chains (Bezier as 13815), hair-cap fan (1100), outfit (1228 minus skirt),
//                        trio face (16305), scars (1411), hit strobe
```

Each `capsule()` in the sink calls the existing `SceneCapsule` math; nothing in App.cpp changes for phase 1.

What C++ must reproduce to look identical: the tessellation tables (already shared), `edge` rule, curved-limb Bezier with its string-length seeds (13815–13830 — copy the constants; the seed is `part.length*1.7 + role.length*.9`, so pass `seed` as a float in the pose instead of recomputing from strings), hair-cap fan (1112–1141), outfit shoes/sleeves/shirt/daisy (1249–1289), trio face sphere map (16316–16405: `yaw = facing*.42 + spin`, `lon/lat` formulas, blink window `((now/1e6 + pad*.7) % 3.4) < .12`), default face branches (16470–16630 minus glyph branches), scar geometry (1428–1438), hit-strobe cadence (`Math.floor(now/45000) % 2`).

Hard parts:
- **Cloth and hair sims** — state across frames (`player.hair`, `hair.ribbons`, `player.skirtCloth`). Two options: (i) JS keeps the Verlet and sends node positions (7+14+24 nodes ≈ 100 floats; the sim itself is ~a third of the ponytail's JS cost, so this leaves ~0.15 ms/figure on the table); (ii) `figureInstance(style)` → id with C++-side node arrays, JS sends only the anchors that already exist in the pose (head, torso, thighs) — full win, and the constants (gravity 2200·dt², drag exp(−2.4·dt), 5 passes, keep-out radius `head.radius+3`, skirt `waistHalf 9, drop 38, flare 22`) port directly. Note the Verlets run in **world** units (ponytail nodes are world x/y, projected per point at 1063), so instance state needs the world anchors, not screen — phase 3 sends world poses anyway.
- **Face glyphs** (♪ ♫ ♥ through `systemWrite` 16563, 16589) and `hudCircle` grenade rings — keep in JS as an overlay after `figure()`; they are rare.
- **Items** (`drawInventory`, `drawHeldAxe`, `drawHandgun` 16636, photo-theme sprites) — keep in JS; they are 0–10 calls.
- **Drift**: two implementations of one look. Mitigate with a fixed figure *record* (style JSON + pose + state) that both backends consume, a screenshot diff on the console (`node xbox/tools/live.mjs screenshot`), and forced-tier globals (`__oskiewarFigureLod` 16992 already exists).

Cost after (a): JS does pose (≈0.05 ms), packs ≈100 floats (≈0.025 ms), one call (~0.005 ms), plus whatever stays in JS.

### 2b. GPU instancing

Instancing with a unit capsule/disc mesh and per-instance `{x1,y1,x2,y2,z,halfWidth,rgba}` (or one quad + SDF capsule in the pixel shader — round caps at any radius, antialiased edges, 2 triangles per capsule) would cut `renderCpuMs` (the per-triangle `append` at App.cpp 827–839) and GPU vertex count. Neither is the bottleneck: renderCpuMs is 1.6 ms for everything and the GPU is idle.

It does **not** help while JS builds the instance list: per the 2146 note, JS filling a 9-float instance record costs ~2 µs in QuickJS — about the same as one `capsule3d` call — so "JS emits instances" is a wash at best. It pays only when:
- C++ owns the capsule list (i.e. after 2a/2c), where an instance buffer is a cheaper sink than tessellating into `m_frameTriangles` — a phase-3 refinement inside `FigureSink`;
- the geometry is shared: parkKids with one style and a few keyframed poses, repeated props (wheels, pickups). But parkKids today are full `players[0]` clones (21262–21284) with their own patrol, breath, heart and Verlets, so they are not instances of one mesh; they are cheap instances of one *builder*.

Verdict: a GPU-side quality/cleanliness win to fold into the C++ sink later; not a path to 0.1 ms of JS.

### 2c. Retained skeleton (bone hierarchy + angles, C++ forward kinematics)

The oskiewar rig is not an angle hierarchy: `buildRunnerWorldGeometry` is ~450 lines of positional, IK-flavoured, terrain-probing game logic (twoBone 14498, footPlant/surfaceYAt 14528, meleeTarget, skateFrame transforms 14746–14800, strafe footwork 14150, spin 14197) and it doubles as the hitbox source for the sim. Moving FK to C++ means either porting that logic (and running it twice — JS for combat, C++ for drawing) or moving combat native, which the web shell cannot follow. The pose costs ~0.05 ms QuickJS; a positional pose (11 segments × 4–7 floats ≈ 50–80 floats) is the same wire size as "a few dozen angles". So (c) buys nothing over (a) with positional bones and costs a second rig. The parts of (c) worth keeping are the *retained* bits — style handle and per-instance sim state — which (a) phase 2 adopts.

## 3. One description, every shell

Shells that draw figures today: Xbox UWP (QuickJS + C++ `capsule3d`/`disc3d`/`triangle3d`), macOS native (`main.swift`: only `triangle3d`, so JS tessellates into a Metal batch), the browser (`mac-test.html` Canvas2D `triangle` with path batching; `scene3d-webgl.mjs` exists but is not wired), and the test harness (`triangle3d` stub counting faces, `capsule3d`/`disc3d` absent so JS tessellation runs).

Proposal: split `drawRunner` into a **figure record** and two **backends**.

1. `figureRecord(player, geometry, now)` (JS, new, beside `figureLod` 16978) produces `{ style /*handle or object*/, pose: Float32Array, state: Float32Array, tier, depth }` from exactly the fields listed in §1. Pure data; no drawing. Style objects are cached on the player and invalidated by `dressFreeskater`, `applyRoster`, `resetParkKids`, appearance changes.
2. `figureNative(record)` — one call to `figure()`; used when `typeof figure === "function"`.
3. `figureJs(record)` — today's `drawFigureLod`/`drawFighterSilhouette`/`drawOutfit`/`drawHairline`/`drawFace` bodies, refactored to read the record (roleId/partId/state floats) instead of `player` fields, still emitting through `filledCapsule`/`filledDisc`/`screenTriangle`. Web, Mac and tests use this.

The builder itself is written once in freestanding C++ with a sink interface (`FigureSink` above) and compiled three ways: into QuickJsEngine (Xbox), into the Mac app through a small C shim (adds `figure` to `main.swift` beside `triangle3d` at 802–877), and optionally to WASM for the browser (sink callback → Canvas2D `triangle`), at which point `figureJs` becomes a test-only fallback. Until then `figureJs` is the web path and is fast enough under V8 (0.15 ms/figure).

Testability: `createFight` (tests 93–190) stubs `triangle3d` and counts faces; it never defines `capsule3d`/`disc3d`/`figure`, so all existing face-count tests (shield 3888, capsule ends 5761, disc faces 5782, per-face parity 5798, LOD behaviour) keep exercising `figureJs` unchanged. Add: (i) a record snapshot test — `figureRecord` for a known state yields known floats (this is what guards native parity, since the record is the contract); (ii) a `triangleHost` variant in `createFight` that stubs `figure` and asserts it is called once per drawn figure with the right tier and never at tier 3 for civilians off-screen; (iii) keep `__oskiewarFigureLod` forcing. Console parity is a screenshot diff via `xbox/tools/live.mjs screenshot` with the tier forced.

## 4. Recommendation and phases

Go with **2a with retained state (a+c hybrid)**, staged so each step is measurable with `FRAME_PHASES` (`xbox/tools/oskiewar-phase-profile.py`; the `renderables` phase is the figures) and `AC_NATIVE_PROFILE hostJsMs`.

**Phase 0 — same day, JS only (no native change), ~15–25% off figures.**
- `drawSkirt` civilian branch (1293–1296) rebuilds and re-projects the whole pose per kid per frame; pass `geometry` in.
- `drawRunner` 17060 spreads a new player object for dummies every frame; `figurePartColor` 16999 runs regexes per segment per frame — resolve `roleId`/`partId` once in the `segment()` closure at 14524 and compare ints.
- `drawBow` is called twice per figure ("tails", "loops"); it re-reads and re-projects the same frame; cache the projected ribbon points on `hair.bow`.
- Add profiler marks inside `drawRunner` for silhouette/face/hair/outfit to get the console's own split before writing C++.
Estimate: pastel 1.6 → ~1.3 ms; flat P1 ~0.6 → ~0.5.

**Phase 1 — one day, shippable: `figureStyle` + `figure` (screen-space pose), stateless passes only.**
- C++ (`FigureBuilder.cpp` + two bindings at QuickJsEngine.cpp 970): tiers 1–3 complete; tier 0 body (straight capsules ×2 passes, palette bands, curved chains), head + edge, hair-cap fan, outfit minus skirt, scars, hit strobe, default face and trio face (non-glyph branches).
- JS: `figureRecord`, `figureNative`, `figureJs` split; ponytail, bow, skirt, items, shield, glyphs stay as they are and draw after `figure()`.
- Measure with five figures forced to tier 0 (the f062649517 experiment: hostJsMs 29.4 → 21.4 for tier 1).
Estimate per figure: flat P2 0.3–0.6 → ~0.1 ms; flat P1 (hair in JS) ~0.6 → ~0.45; pastel 1.6 → ~0.8 ms.

**Phase 2 — two to three days: instances and sims in C++.**
- `figureInstance(style)` → id; `figure(instance, pose, state, tier, depth)`; C++ owns ponytail/ribbon/skirt Verlet arrays keyed by instance, stepped on the paint clock exactly as 949–1055, 1163–1191, 1298–1372 (constants copied; keep-out against head and torso capsule; thigh separation). JS passes world anchors, so the pose grows a world block: `head.z` and per-segment `z1,z2` (or switch wholesale to world pose + camera, below).
- Move `drawEmoFringe`, bow loops, shoes.
Estimate: pastel ~0.8 → ~0.2 ms; parkKids (8, civilian branches) ~0.1 ms each.

**Phase 3 — world-space pose, C++ projection, crowd batch, instanced sink.**
- Pose becomes world space (`x1,y1,z1,x2,y2,z2,width,…`) plus the existing 27-float `mainNativeCamera()` (21508); C++ projects per endpoint (23 fewer allocating `projectPoint` calls in JS) and can give limbs real per-segment depth (fixes the "flat at one depth" compromise at 20560 and the skateboard sort at 13868).
- `figures(buffer, count)` batch for the crowd: one call, records packed back to back; parkKids share a style array. The P1 seat inset (`rebuildSeatInset` 21420, kids rebuilt at 30 Hz into `triangles3d`) can reuse the same builder with the inset's camera.
- Sink option: instanced SDF capsules in App.cpp (`GpuCapsuleInstance {x1,y1,x2,y2,z,w,r,g,b}`, one quad, `DrawInstanced`) so figures stop consuming `kMaxTriangles` and caps are resolution-independent.
Estimate: ~0.08–0.1 ms per figure of JS (pose ~0.05, pack ~0.03, call ~0.005); a crowd of 8 kids ≈ 0.3 ms total; native side stays under the current renderCpuMs.

**LOD in the design.** Tier stays chosen in JS (`figureLod` 16978 with the frame-time governor `governFigureLod` 16970 — it needs frame time, which JS has) and is passed to `figure()`. C++ applies the same tier semantics (17006–17055) plus tier-0 detail knobs derived from head radius (curve steps, face features skipped when `head.radius < 5`, 16409). Tier 3 for the crowd and any instance whose projected head is off-screen (the paint loop's margin cull at 20559 stays).

**Crowd.** parkKids remain player clones for behaviour (patrol, startle, heart), but drawing is `figures()` with per-kid instance ids for hair; civilian branches (3-step curves, no bow tails, fixed skirt quad) become `kind: "civilian"` in the style.

**Risks to name up front.** (1) Two renderers of one look — held together by the record contract and console screenshots. (2) `JsLimits.max_callback_us = 8000` in runtime.hpp is not enforced anywhere in QuickJsEngine.cpp (grep) — no hidden budget. (3) QuickJS heap is 32 MB; style JSON per figure is bytes. (4) Disc ring sides already differ between C++ and JS; document rather than fix. (5) `capsule3d` is absent on the Mac shell — `figureJs` keeps it working until the C shim lands.
