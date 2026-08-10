# Oskiewar replay compatibility

A demo is an input recording for one immutable simulation, not a recording for
“the latest Oskiewar.” Old demos remain accurate only while all three identities
stay explicit:

- `version` selects the envelope decoder and migration chain.
- `simulation` selects an archived physics adapter.
- `build` records which integer game build made the demo; it is provenance, not
  permission to substitute that build's physics with current physics.

`compat.mjs` is the append-only compatibility boundary. New physics receives a
new simulation identifier and adapter entry. Never change an existing contract's
tick duration, button bits, checkpoint width, or behavior. Never automatically
map an unknown simulation to the newest one: an honest “adapter unavailable” is
better than a plausible but inaccurate replay.

Playback first migrates the old envelope, validates its immutable simulation,
then expands command state changes on a fixed 60 Hz tick clock. Paint cadence,
refresh rate, frame drops, and wall time do not participate. Fixture digests pin
both the migrated payload and expanded input timeline so decoder drift fails in
focused tests.

`fixtures/manifest.json` is itself versioned and records the producing build,
minimum player build, canonical replay digest, and expanded-timeline digest for
each preserved recording. The minimum build says where the decoder first
shipped; it never authorizes a different simulation adapter.

To add a format or simulation:

1. Preserve a real recording as a new fixture; do not rewrite old fixtures.
2. Add a one-way envelope migration without deleting the earlier decoder.
3. Add the archived simulation contract/adapter under its original identifier.
4. Assert the fixture's canonical and timeline digests plus a late-player-build
   playback plan.
5. Run `node --test xbox/live/tests/replay-compat.test.mjs`.

The compatibility layer intentionally does not claim that current physics can
re-simulate `oskiewar-physics-1`. The runtime must ultimately dispatch the plan
to a preserved adapter with that exact ID. Until that adapter is extracted from
the live game, stored v1 demos may use their checkpoint viewer, while command
re-simulation should fail closed rather than drift.
