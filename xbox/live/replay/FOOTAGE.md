# Oskiewar footage

A stored round is footage: the round as it was drawn, frame by frame. A
player draws it, so no physics runs and a physics change cannot alter an old
fight. Any engine that reads the footage's spec can play it, in that
engine's current look.

Inputs (`commands`) and checkpoints are still filed with every round, for the
coach, stats and bots. Playback never depends on them.

## Envelope

`demo.footage`, filed with the round and split off by the store into
`oskiewar-footage` (gzipped). The round document keeps a summary:
`footage: { spec, frames, bytes, firstTick, lastTick }`.

```js
{
  spec: 1,          // shape of a frame (not the game build)
  tickRate: 60,     // frame ticks are 60 Hz, counted from the footage start
  startTick: 0,     // footage start on the demo's own tick clock
  keyEvery: 60,     // a key frame at least this often
  frames: [[tick, key, body], ...]
}
```

- `key` is `1` for a whole frame and `0` for a change from the frame before.
  The first frame is always a key.
- The engine films at 30 fps, from the first fought frame to the verdict.

A change is a diff of plain objects:

- A changed key is carried, nested objects recurse, and removed keys are
  listed under `"~d"`.
- An array of objects is patched by index under `"~a": length`.
- Anything else is carried whole.

## Spec 1 frame

The instant-replay frame (`makeRoundReplayFrame`), normalized:

- Every time is in microseconds from the footage start. Player `*Offset`
  fields, ball `serveAt`/`safeUntil`, decal `at` and audio cue `at` are all
  folded onto that clock.
- Numbers are quantized: hundredths below 16, tenths above.
- `decals`, `bloodDrops`, `tearDrops`, `waterSplashes` and `impacts` are
  objects keyed by `"k" + id`, so a thing that stays costs nothing.
- Decals are recipes (kind, position, size, stretch, angle, colour, seed,
  id). Their outline, ground and raster are rebuilt by the watching engine.
- `audio`: the cues heard since the previous frame (`drum`, `synth`).
- `scene`: what the HUD reads:
  - fighter `name`, `nation`, `color`, `score`, `roundWins`
  - `phase`
  - `round` (`result`, `cause`, `timed`, `remainingMs`, `elapsedMs`)
  - `wind`
  - the camera `doll` pose

## Compatibility rules

1. **Readers ignore fields they do not know and default fields a frame
   lacks.** Adding a field is not a new spec.
2. **A spec bump** is only for a field that changes meaning or goes away. It
   appends one step to `footageUpgrades` in `oskiewar.js`, carrying a frame
   from the spec below to the new one. Steps are never edited or removed. The
   player only reads the newest shape, so an old film plays through the
   chain.
3. **A film from a newer spec** than the engine knows is refused ("Update
   Oskiewar to watch"), never guessed at.
4. **The store checks only the envelope.** It never rejects a frame field it
   has not heard of.
5. **Each spec keeps a real recording as a fixture**, with a test that plays
   it in the current engine (`tests/oskiewar-footage.test.mjs`).

## Size

A dojo round is about 40 KB/s before compression and about 10 KB/s gzipped.
An upload over 8 MiB is filed without footage instead of not at all, and
the engine stops filming after ten minutes.

## Stamps

Footage is drawn, not simulated, so an export can render any stretch of it
at any size and speed: a clip, a GIF, a still.
