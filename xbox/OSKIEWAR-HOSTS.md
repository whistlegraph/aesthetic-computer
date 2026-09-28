# Oskiewar on every host — renderer and audio plan

One game source, four hosts: the web page (also iOS, a WKWebView around it),
the macOS native shell, the Xbox, and the test harness. Today the game draws
and sounds differently on each because it reaches the host through `typeof`
probes and per-platform fallbacks scattered through `oskiewar.js`. This plan
makes the renderer and the audio path platform-agnostic in architecture: the
game writes one **frame program** — bytecode for a scene-and-sound machine —
and every host runs an interpreter for it. The bytecode format is the spec,
not any implementation: a versioned op table plus a conformance suite of
programs. The web, Mac, Xbox and harness interpreters are peers under it.

Written 2026-09-26 from v168 (the monowheel park) on oskiewar.com. Line
numbers are `xbox/live/oskiewar.js` at 270e77723b. Earlier measurements and
the console-perf plan live in the 09-26 engine-split research; this document
replaces its "JS reference renderer + levelled scene API" design with a
bytecode format and puts correctness on every host ahead of console
milliseconds.

## 0. What is wrong on the web today, and why

Seen live on oskiewar.com v168 (screenshots 2026-09-26 22:40):

| symptom | cause |
|---|---|
| P1's own head fills the first-person inset | `projectPoint` (2425) pins z to `cameraPin` = 80 instead of clipping; figures inside the inset camera's near range (view z 20–80, admitted at 21654) are never cut, and the rider is drawn into its own eye view |
| two figures interpenetrate on the monowheel; the blue park deck paints over the road; the HUD lane is meaningless | the web shell has no `triangle3d`, so `emitTriangle` (2177) drops z and Canvas2D paints in submission order; every `triangleDepth` decision is a no-op on web |
| a world segment raked across the screen mid-death | `projectPoint`-based fans, discs and capsules get only a ±32200 cull, never a near clip; `renderer.test.mjs` has 3 floor/near tests failing at HEAD |
| glasses, grenade rings and face glyphs overdraw the inset edge | `hudCircle`/`hudLine` fall to host `line` on web (2275), which is not scissored to `clipView`; `typeWrite` is never scissored on any host |
| shoes float off the feet, monowheel is a flat blob | shoes are screen-space capsules at the shin tip facing `player.facing` only (1252–1266), ignoring pitch and yaw; the wheel is 12 flat `worldQuad`s with no shading, spokes or rolling |
| monowheel is silent on the web | `oscillator`/`oscillatorStop` and `synth` do not exist in the web shell; every `playSine` is dropped there |

The common root: **two depth models and three clip paths.** Xbox has a GPU
depth buffer and native near/rect clipping in `sceneMesh`; the web has
painter's order and JS clipping only on `worldTriangle`/`worldQuad`; text,
`box` and `line` sit in a different stratum on each host (CPU layer under
triangles on Xbox, inline on web).

## 1. Where each host stands

| surface | web / iOS | macOS native | Xbox | harness |
|---|---|---|---|---|
| faces with depth (`triangle3d`) | no (Canvas2D `triangle`) | yes (Metal) | yes | stubbed |
| `disc3d` / `capsule3d` / `sceneMesh` | no (JS fans) | no | yes | not stubbed (JS fans run) |
| retained mesh (`meshUpload`/`meshDraw`) | no | no | no | no |
| text | Canvas `fillText`, inline | yes | D2D, after GPU | stubbed |
| `box`/`line` stratum | inline | inline | CPU layer under triangles | stubbed |
| photo materials (`themeSprite`/`themeQuad`) | no | no | yes | no |
| post effects (`postEffects`) | no | no | binding absent | no |
| `drum` | yes (SFX bank after unlock) | yes, 24 voices, panned | allowlist kick/snare/clap/hat/block; pan ignored; one voice | recorded |
| `synth` (one-shot sine) | **missing** | yes | yes, shares the drum voice | missing |
| `oscillator` (continuous) | **missing** | only on the fans branch | yes, own voice | missing |
| `gameSignal` | DOM event → SFX, VOICE, MIDI | OSC when `OSKIEWAR_OSC=1` | OSC ×3 | recorded |
| `titleVoice`/`titleBeep` | yes | no | no | no |

Game-side probes today: 14 `typeof` bindings resolved once (2247–2266),
8 `consoleHost()` draw branches, ~35 `capabilities()` mode flags, 11
`clipView` branches, and audio wrappers `playDrum`/`playSine`/`emitSignal`
(4463–4494) plus the ad-hoc motor voice (21218).

## 2. Rules the new architecture holds

1. **The game never calls a host.** It writes ops into a frame buffer and
   hands the buffer over once. No `typeof host`, no `consoleHost()`, no
   `capabilities().platform` in draw or audio code. What the host can do is
   the interpreter's business; the game's only capability question is
   "which bytecode level does this host run".
1b. **Ops are scene-level, never triangle-level.** On the console a typed
   array element write costs ~0.23 µs and a 12-arg native call ~0.85 µs
   (measured 09-14, oskiewar.js ~2145). A frame written as triangles from JS
   is 25k numbers (5.7 ms, measured); written as figures, capsules, discs,
   ribbons and mesh handles it is ~3–4k numbers (<1 ms) and the tessellation
   moves into the interpreter. That is the rule that makes bytecode a win
   instead of the loss the earlier benchmark found.
2. **Depth is real on every host.** Every face, disc, capsule, sprite and
   glyph carries a depth; each host honors it (GPU depth test, or a stable
   sort for a 2D fallback).
3. **One clip rule.** Near plane and view rect are part of the format: every
   op is clipped against its VIEW by whichever interpreter runs it, and the
   conformance suite checks the result exactly.
4. **A view is a value, not a global.** Camera, viewport rect, near plane,
   depth window and "figures to omit" travel as one object. The P1 inset is
   the renderer run again with a different view.
5. **The format is the spec, no implementation is.** `xbox/live/frame-ops.md`
   (op table, argument layouts, versioning) plus `xbox/live/conformance/`
   (programs with expected results) define correctness. The JS interpreter
   is the web's interpreter, not a reference the others copy; the Xbox
   interpreter tessellates a FIGURE its own way, on the GPU if it likes, and
   passes the same conformance programs.
6. **Audio is a cue list, not a set of free functions.** One `audio` object
   with capability discovery; one `silent` gate for rollback resims applied
   inside it; sim clock only.

## 3. Renderer

### 3.0 The frame program

Each frame the game fills one preallocated `Float32Array` (plus a per-frame
string table for text) and calls `frame(buffer, length, strings)`. Nothing
else crosses the boundary. Retained things (meshes, terrain profiles, figure
rigs) are uploaded with ASSET ops at boot or map change and referred to by
handles the game assigns.

```
VIEW     id · camera[27] · rect[4] · near · depthBase · depthSlope · omitFigure
MESH     handle · model[12]                       retained; interpreter clips + lights
MODEL    radius · handle×3 · model[12] · light    op 14, in frame-vm.mjs since 2026-09-28: a baked mesh
                                                  under a placement, at one of three baked levels picked by
                                                  projected radius (56 / 20 px); normals by cofactor, zero = unlit.
                                                  Objects (xbox/OBJECT-DIALECT.md) are MODELs plus a few WORLD faces
FACE     3×(x y z) · rgb · depth                  escape hatch for one-off geometry; interpreters may lower to it
DISC     x y z · r · rgb           CAPSULE  x1 y1 z1 x2 y2 z2 · w · rgb
RIBBON   cubic (4 pts) · w · rgb                  limbs, hair, skirt edges
FIGURE   pose (12 segs + head ≈ 130 floats) · style · tier
TEXT     strId · x y · size · font · rgb · depth  clipped to the current VIEW rect
SPRITE   assetId · x y z · w h                    optional host material (Xbox photo)
CUE      kit · vel · pan · simFrame     TONE   freq dur gain pan simFrame
VOICE    id · freq · gain | stop        SIGNAL eventId player v v2       SAY strId
ASSET    kind · payload                           boot / map change only
```

Properties that fall out of "the frame is data":

- **One boundary, one format.** Web, Mac, Xbox and the harness differ only
  in their interpreter. A host that lacks a material or a voice ignores the
  op; the game never knows.
- **Depth and clipping are interpreter duties.** Every op carries a VIEW
  context; the interpreter projects, near-clips, rect-clips and depth-tests
  (GPU) or depth-sorts (Canvas2D). The web defects in §0 cannot recur by
  construction.
- **Rollback and audio are the same story.** Audio ops carry the sim frame;
  a silent resim simply does not flush its buffer, so the motor voice and the
  note queues stop leaking (§4.2 A4 shrinks to "tag the op").
- **Tests assert on ops.** The harness records the program; "the inset never
  contains its own rider" is a scan for FIGURE ops under the inset VIEW.
  Every interpreter answers to the same conformance programs.
- **Recordable.** A frame program is a replay format: the reel oven, the
  social preview burn and phone spectators can consume programs instead of
  re-running the game.
- **Levelled.** `host.bytecode` is a version; an interpreter that does not
  know an op skips it by length. The release gate refuses a game whose
  required level exceeds the console's.

The other reading of "the game has bytecode" — compiling the game itself to
a VM bytecode per host — is already the case on Xbox (QuickJS) and buys
nothing on the web; this plan is about the game's *output* being bytecode.

### 3.1 Layers

```
oskiewar.js  (rules, sim, net, replay, pose, LOD choice, camera solve)
   │  writes the frame program (an Emitter: push(op, …), string table, retained handles)
frame-vm.mjs  (the JS interpreter: projection, near clip, rect clip, depth mapping,
   │           disc/capsule/ribbon/figure tessellation → FACE stream; recorder for tests)
   ├─ scene3d-webgl.mjs  WebGL2, depth-tested  (exists; benched 09-14; not wired)
   ├─ Canvas2D            fallback: stable sort by depth, ?renderer=canvas
   ├─ macOS Metal         triangle3d today; grows to the FACE stream
   └─ Xbox FrameVm.cpp    native interpreter: SceneMesh/capsule/disc fans exist in
                          ScenePrimitives.inc; FIGURE/RIBBON/TEXT/CUE are the new units
```

`frame-vm.mjs` starts as today's projection, clipping and fan code lifted
out of the game, so the 120 triangle-observing tests keep their meaning
during the move. After that it is free to change: it is the web's
interpreter, and the harness asserts on programs, not on its triangles.

### 3.2 Steps

| step | work | fixes | gate |
|---|---|---|---|
| **R1 web depth — SHIPPED e2fb5c268a 2026-09-26** | Wire `scene3d-webgl.mjs` into `mac-test.html` as the `triangle3d` provider with a depth buffer; text after the triangle pass; `box`/`line` as depth-carrying triangles. Canvas2D stays behind `?renderer=canvas`. Pure shell change, lands before any bytecode. | interpenetration, deck-over-road, HUD lane, inset edge overdraw | bench parity run re-run against the live shell; 1280×720 screenshot diff |
| **R2 the emitter + JS interpreter** — clip rule SHIPPED 74844d1600 (v169, 2026-09-27): `project` reports `behind`, figure bones cut through `worldSegment`, figures behind the pin not drawn, inset admits past the pin only, renderables/shadows/parts skip behind the lens, renderer tests 9/9. Emitter + web interpreter SHIPPED 3673855652 (v170, 2026-09-27): the game paints one program per frame (VIEW/FACE/DISC/CAPSULE/TEXT/BOX/LINE/WIPE) through `frame(buffer, length, strings)`; `xbox/live/frame-vm.mjs` runs it on the web; hosts without `frame` (console, harness) execute the ops in the game. World projection moved SHIPPED 250e957a5e (v171, 2026-09-27): CAMERA op (prepared view + clip band, once per re-prepare) and WORLD op (world-space face); worldTriangle, worldQuad and the JS mesh path emit WORLD; frame-vm projects, near-clips and band-clips; `tests/frame-conformance.test.mjs` compares immediate vs program over 45 cameras. v172 fd3c947c4f (2026-09-27): DEPTH op (flat / offset world depth), shadows as world ellipses flat behind the caster, ground marks as world fans, terrain passes as WORLD, park meshes as ASSET once + MESH by handle; program ~52k → ~24k floats. Still screen-space in the game: figures only (~1170 FACE + ~320 CAPSULE + ~220 DISC a frame). FIGURE op needs a decision — see §7. | Add `Emitter` to the game and `frame-vm.mjs`; move projection, `clipPolygon`/`clipScreenBand`/`clipSegmentBand`, `filledDisc`/`filledCapsule` fans and `drawCurvedLimbs` ribbons into the VM; the game's draw functions push DISC/CAPSULE/RIBBON/FACE/TEXT ops instead of calling hosts. Every op is clipped in the VM; `cameraPin` pinning retired; the inset VIEW omits its own rider. Fix the 3 failing `renderer.test.mjs` floor tests here. | giant head in inset, raked segments, glyph leaks, the 14 `typeof` bindings and 8 `consoleHost()` draw branches | `renderer.test.mjs` 9/9; `oskiewar.test.mjs` triangle tests unchanged (the VM emits the same faces) |
| **R3 views and meshes as ops** | VIEW ops replace the `cameraDoll`/`clipView`/`triangleDepth` globals and `withRenderView`; ASSET/MESH replace `captureQuadMesh` monkey-patching and the three `drawQuadMesh` backends. | hidden render state | recorder tests: one VIEW per inset, no global reads in draw code |
| **R4 FIGURE op** | The pose object becomes a FIGURE op; skeleton, outfit, hair, face tessellate inside each interpreter per tier. The web one in JS or in a vertex shader; the Xbox one in C++ (the ~90 calls and 1.6 ms per figure go to one op). Look is held by the conformance images, not by sharing code. | per-figure boundary cost; figure look drift between hosts | conformance: one figure per tier within image tolerance |
| **R5 flat is the reference** | Flat shading is the reference look (decided 2026-09-26). SPRITE/photo materials stay an optional Xbox interpreter feature; the reference, web, Mac and the harness never see them and no game rule may depend on them. | web ≠ Xbox look | poster, web and Mac identical; Xbox differs only in material |
| **R6 Xbox interpreter** | `FrameVm.cpp`: one `frame()` binding replaces the 20-odd draw bindings; retained handles for meshes and terrain strips; the console-perf wins from the 09-26 research arrive here as a consequence. | Xbox frame time | AppVeyor runs the conformance suite through the QuickJS smoke |

### 3.3 Tests

- **Game tests assert on the program.** The harness records ops; "the
  inset never contains its own rider", "no op outside its VIEW", "five
  figures at tier 1 in the park" are scans over the buffer. During R2 the
  harness also runs `frame-vm.mjs` so the 120 triangle-observing tests keep
  passing; once program tests cover the same ground those are retired rather
  than pinned to one interpreter's triangles.
- **Interpreter tests are conformance.** `xbox/live/conformance/` holds
  small programs (one figure per tier, a mesh crossing the near plane, the
  inset view, a CUE burst) with expected outputs: op-level for what must be
  exact (clipping, omission, ordering, audio timing) and image tolerance for
  what may differ (tessellation, antialiasing). Every interpreter runs the
  suite: Node for the JS one, AppVeyor's QuickJS smoke for the Xbox one.
- **Recording is free.** A saved program is the replay fixture: the reel
  oven and the social preview burn render from programs, so they stop
  depending on the game's draw code at all.

## 4. Audio

### 4.1 Ops (from what the game actually uses)

Audio is the same program, not a second interface. The game pushes:

```
CUE     kit(kick snare clap hat bell block whoosh modem) · vel · pan · simFrame   polyphonic, panned
TONE    freq · dur · gain · pan · simFrame        one-shot sine with decay (today's playSine); simFrame offsets absorb the glass/bubble/crowd/laugh queues
VOICE   id · freq · gain | stop                   continuous, interpreter-smoothed (the monowheel motor)
SIGNAL  eventId · player · v · v2                 semantic bus → replay row, SFX/VOICE/MIDI on web, OSC on Mac/Xbox
SAY     strId                                     announcer lines (replaces titleVoice and the orphaned __oskiewar*Line globals)
PAN     player · pan                              render-side pans handed to the interpreter, never read inside the sim
```

Lifecycle (`unlock`, `mute`, `stopAll`, `panic`) belongs to the shell, not
the game. The interpreter reports `caps → { kit, tone, voice, speech, midiOut,
oscOut, latencyMs }` so the harness can assert what a host would have played.
The web's existing event bus (`oskiewar-sfx/voice/midi.mjs`) becomes the
web interpreter's SIGNAL subscriber, not the main source of one-shots.

### 4.2 Steps

| step | work | fixes |
|---|---|---|
| **A1 audio ops** | `playDrum`/`playSine`/`emitSignal`/`updateMotorAudio` push CUE/TONE/VOICE/SIGNAL through the same Emitter; the try/catch drum fallback (4473–4489) goes away; raw `gameSignal` calls (21203) become SIGNAL ops | the throw-and-guess contract |
| **A2 web tone + voice — SHIPPED 9c48f70431 2026-09-27** | `synth` (sine, squared decay) and `oscillator`/`oscillatorStop` (sine + quiet octave, eased pitch and gain, 20–8000 Hz, gain ≤ .5) in the web shell over the drums' AudioContext; the bank's bubble laugh unrouted so the laugh plays once. The countdown route stays because the sfx suite requires every signal to have one. | monowheel motor and every `playSine` audible on the web; victory laugh identical on all hosts |
| **A3 Xbox polyphony + kit** | voice pool instead of one `m_voice` with Stop+Flush per trigger; allowlist replaced by the capability kit (bell/whoosh/modem exist in the mixer already); honor pan | rapid cues cutting each other off; bell/whoosh workarounds |
| **A4 rollback holes** | A silent resim never flushes its buffer, which covers the motor voice for free; note queues become TONE ops with future `simFrame` offsets instead of sim-owned arrays; `bloodDripSound`/`playBubbleSound` on the net clock, not `monotonicUs`; the countdown double-bell check | resim leaks |
| **A5 speech** | `say` on web (SpeechSynthesis/VOICE), no-op elsewhere; remove `__oskiewarResultLine`/`__oskiewarStartLine` and the stale source assertion at test 3748 | orphans |
| **A6 Mac** | ship the fans-branch `oscillator` to main's shell; add `say` no-op and `caps` | Mac parity |

## 5. Figure polish that rides on R1–R2

Do these once the figure draws with real depth and one clip path, so they are
drawn once and look the same everywhere.

- **Shoes**: build in world space on the foot bone (shin end + a foot
  vector from pitch and yaw), not screen-space capsules facing `facing`;
  a sole quad, an upper capsule, a heel; skirt/pant hems occlude by depth.
- **Bodies**: the curved limbs and trio face already exist; on web they
  were unreadable because of painter's order. After R1, tune limb widths and
  outline pass against the Xbox look, and give the pastel skater a
  consistent outline weight across LOD tiers.
- **Monowheel**: a captured mesh (tire with tread bands, rim, hub, foot
  plates) drawn through `mesh(handle)`; rolls by contact distance like the
  board wheels; contact shadow; rider feet snap to the plates; the motor
  voice from A2 pitched by speed.
  **Object, 2026-09-28:** the monowheel is now a KidLisp object,
  `xbox/live/objects/monowheel.lisp`, written with `revolve`, `radial` and
  `mirror`. It has tire wedges, a rim, spokes that roll with distance, lean
  and a landing squash. `xbox/live/object-lisp.mjs` compiles it once, baking
  everything that doesn't move into meshes at three levels of detail. A tick
  sends 2 MODEL + 4 WORLD = 92 numbers, against 1144 for today's flat wheel,
  and the host draws 148 / 92 / 80 triangles by level. Try it in
  `xbox/live/object-lab.html`. It is not in the game yet. The dialect, the
  budget, MODEL and the embed plan are in `xbox/OBJECT-DIALECT.md`.
  **The rule: forms compile away; hosts see faces and meshes.** High-level
  forms (revolve, radial, mirror, later extrude and sweep) and levels of
  detail are expanded only in `object-lisp.mjs`, when an object loads. No
  interpreter expands anything. `FrameVm.cpp` (R6) implements WORLD, ASSET
  and MODEL (transform, light, pick a level) and nothing more, so hosts can't
  drift. This is also a route for §7 decision 0 option (b): a figure's rigid
  parts as baked meshes under moving bone frames, one MODEL each, with the
  same forms compiling away in the same place.

## 6. Order

R1 → R2 (emitter + JS interpreter, with A1 in the same emitter) → A2 → R3 →
polish (§5, drawn through ops) → R4 → A3/A4 → A5/A6 → R6.

R1 and A2 change only the web shell and are the fastest visible wins. R2
onward touches the game file and needs the branch merge sorted first (the
`oskiewar-day-perf` native-bios changes are still unmerged into main). R6
and A3 need a native Xbox release (AppVeyor MSIX); until then the Xbox keeps
its per-call bindings and the JS interpreter lowers the program to them, so
nothing waits on the console.

## 7. Decisions for jeffrey

0. **How figures become a FIGURE op.** The figure painter (skeleton, outfit,
   face, hair, skirt, bow — a few thousand lines) lives in the one game file,
   which every host loads alone; the web interpreter cannot import it. Either
   (a) the web interpreter keeps a second figure painter, held to the game's
   by conformance images, or (b) figures are re-expressed as world-space
   primitives (world capsules and discs, the face as a textured billboard)
   that every interpreter projects, so no painter is duplicated. (b) is the
   agnostic one and the larger rewrite.

1. Keep Canvas2D as a fallback, or make WebGL2 the only web path?
2. ~~Photo materials on the web or flat as the reference?~~ Flat is the reference (2026-09-26).
3. In the first-person inset: hide the rider entirely, or draw hands and the
   wheel's front edge?
4. ~~The line-2 freeskate hardcode: keep or gate?~~ Freeskate is the default
   map (decided 2026-09-26). The line stays. Today it also swallows
   `?opponent=`; if versus or coach URLs are wanted back later, the default
   moves into the shell as a fallback rather than an override.
