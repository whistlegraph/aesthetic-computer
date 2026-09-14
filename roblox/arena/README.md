# Aesthetic Arena

A private 1v1 ring-out prototype owned by Whistlegraph. Twelve players can join a
server; two fight while the rest wait in the lobby.

[Experience](https://www.roblox.com/games/96347741188039) ·
[Creator Dashboard](https://create.roblox.com/dashboard/creations/experiences/10766363015/overview)

Stand on the pink pressure pad to queue; step off to cancel. It turns cyan and starts a three-second opponent countdown.
If no second human queues within three seconds, a practice dummy fills the slot
(as soon as its rig loads). Human pairs take priority before a round starts.
The dummy approaches, throws grenades, and moves toward the center near the edge.
The first two fighters teleport into the ring and immediately have sixty seconds to win. Face your opponent and press F / gamepad R2 /
NADE. Grenades use a short 22-stud lob, explode on contact, and have a 1.8-second cooldown. A 1.5-second fuse catches misses. The ring is 112×112 studs. Falling out,
dying, resetting, or disconnecting forfeits the match. Simultaneous elimination
and timeouts draw. Both players return to the lobby; queue again for another duel.
Wins last for the current server session.

## Iterate

Requires Node 22+ and [Rojo 7.7.0](https://github.com/rojo-rbx/rojo/releases/tag/v7.7.0).
Run from the monorepo root:

```sh
mkdir -p roblox/arena/build
rojo build roblox/arena/default.project.json -o roblox/arena/build/arena.rbxlx
node roblox/arena/api.mjs publish
node roblox/arena/api.mjs test
node roblox/arena/api.mjs poll
node roblox/arena/api.mjs logs
```

`test` runs against the exact version returned by the latest upload. Poll until
`state` is `COMPLETE` and `output.results[0].ok` is `true`; `FAILED` and `CANCELLED`
exit unsuccessfully. Task creation is asynchronous. Do not start another test
until the current task finishes; `latest-task.json` tracks one task at a time.
`inspect` reads the private target; `configure` updates its title, description,
and server size. The target IDs are pinned in `target.json`.

Credentials come from `ROBLOX_API_KEY` or the ignored vault env file. The browser
session was used once to create the new universe through Roblox's undocumented
creation endpoint; subsequent configuration, publishing, and tests use Open Cloud
with the API key. Creation is not presented as a supported API-key operation.

Uploads save their SHA-256, returned version, timestamp, and a copy of the place
file under ignored `build/`. Re-publish a retained version to roll back:

```sh
node toolchain/robloxplorer/robloxplorer.mjs publish roblox/arena/build/arena-v4.rbxlx 10766363015 96347741188039 --live
```

This produces a new Roblox version. Existing running servers retain their loaded
version; join a fresh server for a playtest after uploading.

## Source and validation

- `src/server/Match.luau`: FIFO queue, countdown, round timing, result, cooldown.
- `src/server/World.luau`: lobby, elevated ring, spawn, queue pad, signage.
- `src/server/Controller.luau`: player lifecycle, server-validated combat,
  score, RemoteEvent validation, replicated HUD state.
- `src/server/Combat.luau`: server-side range, facing, and impulse calculation.
- `src/client/`: HUD, controls, hit feedback. Clients send throw sequence and normalized aim;
  the server derives the opponent, cooldown, range, and score.
- `tests/headless.luau`: match edge cases, world construction, actual controller
  startup/heartbeat, and shove calculations on Roblox's engine.
- `evidence.json`: checked release and headless result.

The game still needs a two-client playtest. In Studio, open the built place and
start a local server with two players. Check queue/cancel, five-second freeze,
shove direction/reach/cooldown, ring-out, reset/disconnect, spectators, timeout,
and a second match. Check HUD and touch placement on a narrow viewport. Headless
tests do not establish client rendering, network feel, or real-avatar movement.

This version has no persistence, matchmaking between servers, monetization, or wares. The test universe remains private;
console launch is not enabled, although the controls include gamepad bindings.

API references: [place publishing](https://create.roblox.com/docs/cloud/guides/usage-place-publishing),
[Luau execution](https://create.roblox.com/docs/cloud/reference/features/luau-execution),
[universe configuration](https://create.roblox.com/docs/cloud/reference/features/universes).

## Update notices and shared UI

`roblox/shared/UI.luau` starts the reusable label and color-token library. The HUD renders AC’s original `font_1` lettering using generated pixel primitives, with native text fallback for unsupported characters. YWFT Processing is not imported. Roblox does not support importing arbitrary font files.
A glyph atlas or rendered image labels could carry our lettering after checking
font licensing. Keep native text for names, input, and broad character coverage.

Version 6+ servers subscribe to `ac-arena-release`. After a future release, run:

```sh
node roblox/arena/api.mjs announce
```

This broadcasts the latest locally recorded version number to this universe.
Older subscribed servers show a fixed update banner. It does not hot-reload code,
force players out, or guarantee that ordinary rejoin selects a newer server.
Messaging delivery is best effort and not persisted; servers running version 5 or
older have no listener. Cross-server delivery and the rendered banner still need
a live playtest. No release announcement has been broadcast during this change.

For reaching players outside the game, investigate opt-in
[experience notifications](https://create.roblox.com/docs/production/promotion/experience-notifications).
These are for eligible opted-in users, not arbitrary account DMs. No outreach is
implemented or sent. See [messaging](https://create.roblox.com/docs/cloud/guides/usage-messaging)
and [font asset limitations](https://create.roblox.com/docs/projects/assets).

## Contact grenade physics (version 12)

The server advances a ballistic trajectory and spherecasts each traveled segment,
with 1/60-second substeps and no physical bounce. It excludes the thrower and
noncolliding decoration. First contact removes the projectile before broadcasting
one blast. Humanoid knockback temporarily enters Physics on the simulation owner,
then recovers after 0.45 seconds (0.55 for the dummy). The dummy receives 1.5× force.
Player movement retains Roblox network ownership; this is not rollback simulation.

Live version 12 logged player displacement of 13.8 studs after a dummy blast.
This measures movement during stagger, not isolated impulse distance. Two-client
latency testing remains necessary. Headless tests verify 65 logic/engine checks;
headless execution does not run the physics simulation.

See [local testing MCP](../../toolchain/robloxplorer/TESTING.md) and
[physics research](PHYSICS.md). Notepat is a separate unpublished prototype in
`roblox/notepat`.

## Version 13: motion and scale

Players run at 24 studs/second; the dummy runs at 18. The ring is 112×112,
with spawn, lobby and ring-out bounds moved together. Grenades inherit 35% of
horizontal running velocity, capped at 9 studs/second. Server collision sweeps
and client visuals evaluate the same trajectory. Clients render every frame
using server time and blend prediction corrections over roughly 0.12 seconds.
Invisible server projectiles remain authoritative for contact and blasts.
Projectile IDs remove the matching visual on explosion. Recovery uses Freefall
instead of forcing a GettingUp animation; existing velocity survives the impulse.
These changes implement the report's shared math and stable ownership guidance;
they do not implement engine rollback or establish two-client latency quality.

Version 15 shows grenade cooldown in tenths on the NADE button. Queue membership is server-checked pad occupancy; no E/Q or touch toggle. Once paired, fighters teleport in and battle starts immediately (version 16). 68 headless checks pass, including pad boundaries.

Version 16 removes the second countdown in the ring. Movement and throwing unlock on the same transition that places the fighters. The pad’s solo opponent timer remains three seconds.

## Version 18: live media, Unicode and stick characters

The [media bridge](../bridge/README.md) reads a live curated manifest from AC’s
asset host every minute and displays an approved Roblox image in the lobby.
No API key is shipped with the game. The client has confirmed manifest revision 1
and a loaded Pals image; its gallery and stick player were visually inspected.

Player cosmetics are replaced with original thin-limbed stick figures: cyan head
for players, pink for the dummy. Both use the same hidden R6 collision dimensions;
visible parts are massless and noncolliding. Walking and throwing are procedural.
These are an initial stick style, pending a specific oskiewar reference. Respawning
recreates the custom character rather than restoring account cosmetics.

AC lettering now falls back per character to GNU Unifont-derived Plane 0 glyphs;
see [shared components and license](../shared/README.md). Upper-plane characters
retain native fallback, and complex-script shaping is not implemented.

Local Unicode and seven Node checks passed. The new cloud headless task is still
PROCESSING without logs; do not treat the earlier 68-check pass as validation of
this release. New-rig combat and two-client physics need a live playtest. Input
checks refused control when the host focus moved to another app.
