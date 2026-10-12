# Doom on Aesthetic Computer

Enter `doom` at the AC prompt, or open `/doom-player/`. Tap Play to run the generated room. Whistlegraph starts directly in an empty room, with no loader labels, file picker, or Play screen. Keyboard: arrows move/turn, Control fires, Space uses the south wall exit, comma/period strafe. On the canvas, tap to fire, drag horizontally to look, and hold a second finger to move forward; drag that finger down to reverse or sideways to strafe.

Whistlegraph’s `.wad` ware generates validated Doom 1 level data, compiles a real patch WAD, and plays it in the shared WebAssembly engine. Export saves `Whistlegraph.wad`; Publish to MIME uploads the WAD and returns its public code. Enter `doom <code>` to play a published WAD. Its global AC address, shown at MIME’s top left, is `doom~<code>`. Piece, Roblox, and Doom histories remain separate. AI editing uses the existing signed-in brain connection.

The first grammar is one rectangular arena with adjustable dimensions, lighting, a fixed texture catalog, enemies, and pickups. The south wall is a use-triggered exit. Arbitrary connected-room geometry, custom textures/sprites, Doom 2 maps, and scripted mods are not supported yet. Engine MIDI initialization stalls in this build, so music is disabled; gameplay effects work.

`wad.mjs` is the shared compiler and schema; `apple/whistlegraph/bundle.mjs` copies it into the native bundle. `examples/empty-room.wad` contains a room and player start; `examples/first-arena.wad` is the earlier arena demo. `vendor/README.md` records engine provenance, bundled corresponding source, and Freedoom attribution.

Validation:

```
node --test tests/doom/wad.test.mjs apple/whistlegraph/Tests/wares.test.mjs
node tests/doom/browser.cjs
node tests/doom/feed.cjs
node apple/whistlegraph/Tests/ware-bridge.test.cjs
```

`examples/lollipop-world.wad` is a playable candy test room with procedural walls, floors, and 20 lollipop health pickups. Edit `examples/lollipop-world.json`, then run `node tests/doom/make-lollipop-world.mjs` to rebuild it. Sprite compatibility data comes from the bundled BSD-licensed Freedoom assets. Test it with `DOOM_TEST_WAD=lollipop-world.wad node tests/doom/browser.cjs`.
