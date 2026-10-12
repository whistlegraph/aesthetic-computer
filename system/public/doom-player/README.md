# Doom on Aesthetic Computer

Enter `doom` at the AC prompt, or open `/doom-player/`. Tap Play to run the generated arena. Keyboard: arrows move/turn, Control fires, Space uses the south wall exit, comma/period strafe. Touch controls are included.

Whistlegraph’s `.wad` ware generates validated Doom 1 level data, compiles a real patch WAD, and plays it in the shared WebAssembly engine. Export saves `Whistlegraph.wad`; Publish to MIME uploads the WAD and returns its public code. Enter `doom <code>` to play a published WAD. Piece, Roblox, and Doom histories remain separate. AI editing uses the existing signed-in brain connection.

The first grammar is one rectangular arena with adjustable dimensions, lighting, a fixed texture catalog, enemies, and pickups. The south wall is a use-triggered exit. Arbitrary connected-room geometry, custom textures/sprites, Doom 2 maps, and scripted mods are not supported yet. Engine MIDI initialization stalls in this build, so music is disabled; gameplay effects work.

`wad.mjs` is the shared compiler and schema; `apple/whistlegraph/bundle.mjs` copies it into the native bundle. `examples/first-arena.wad` is a playable compiler output. `vendor/README.md` records engine provenance, bundled corresponding source, and Freedoom attribution.

Validation:

```
node --test tests/doom/wad.test.mjs apple/whistlegraph/Tests/wares.test.mjs
node tests/doom/browser.cjs
node apple/whistlegraph/Tests/ware-bridge.test.cjs
```
