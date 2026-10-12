# Doom on Aesthetic Computer

Enter `doom` at the AC prompt or open `/doom-player/`. Whistlegraph starts directly in an empty room. Keyboard: arrows move/turn, Control fires, Space uses doors/switches, comma/period strafe. Touch: tap to fire, drag horizontally to look, hold a second finger to move forward; drag it down to reverse or sideways to strafe. A quick second-finger tap uses doors and switches; tapping the look finger while the movement finger is held fires.

Whistlegraph's `.wad` ware authors editable classic Doom and Doom II projects: multiple maps, arbitrary vertices/linedefs/sidedefs/sectors, adjoining polygon rooms, floor and ceiling heights, doors, stairs, lifts, triggers, things, custom wall textures, flats, sprite frames, generated DMX sound effects, MIDI music data, DEHACKED behavior patches, and nonstructural binary/text lumps. Project tools preserve untouched maps and assets across edits. A completed turn saves one version; failed requests keep the current version. Undo restores its parent. The old rectangular-room format remains readable.

Geometry is compiled through pinned ZDBSP WebAssembly, producing classic SEGS/SSECTORS/NODES/BLOCKMAP and zero REJECT data. Palette artwork is encoded into real Doom patches and flats, with PNAMES/TEXTURE1 definitions and a complete sprite namespace. The HTTPS preview runs compilation in a dedicated worker and returns bytes to the native editor. This avoids custom-scheme WASM loading and keeps the game/UI responsive. Complex projects do not travel in a URL. Export saves the same bytes as `Whistlegraph.wad`. Publish to MIME uploads the WAD and returns its public code; enter `doom <code>` to play it. MIME shows the global address `doom~<code>`.

The runtime uses Freedoom Phase 1 for episode maps and Phase 2 for MAP01–MAP32. This is classic Doom format, not GZDoom/UDMF/ZScript. Vanilla map/rendering limits apply. Generated MIDI is included in export, but MIDI playback remains disabled because this engine's music initialization stalls. Gameplay sound effects work. Editable project JSON is capped at 4 MB, exported WADs at 32 MB; MIME currently accepts files up to 8 MB.

`project.mjs` compiles projects; `project-tools.mjs` defines authoring tools. `wad.mjs` retains the legacy room compiler and binary inspection. `apple/whistlegraph/bundle.mjs` copies the compiler/tools/node builder into the native app. `vendor/README.md` records provenance, source archives, licenses and Freedoom attribution.

Validation:

```
node --test tests/doom/project.test.mjs tests/doom/wad.test.mjs apple/whistlegraph/Tests/wares.test.mjs
node tests/doom/browser.cjs
node tests/doom/project-browser.cjs
node tests/doom/feed.cjs
node apple/whistlegraph/Tests/ware-bridge.test.cjs
```

`examples/lollipop-world-full.wad` has custom candy walls, flats and sprites, four connected sectors and traversable steps. Rebuild it with `node tests/doom/make-full-world.mjs`. `examples/lollipop-world-full.json` contains its editable project. `examples/lollipop-world.wad` is the earlier single-room candy fixture; its separate generator is `tests/doom/make-lollipop-world.mjs`.

With an unlocked iPhone, `apple/whistlegraph/automate-wad.sh DEVICE_UDID [PROMPT]` sends one real signed-in request through the native app and verifies a new playable version and native WAD export. It uses the phone's existing account and saves a screenshot in its xcresult bundle. Ordinary tests skip live generation unless explicitly enabled.
