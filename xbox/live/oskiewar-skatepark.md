Run `node xbox/tools/serve-live.mjs 8124`, then open
[the skatepark](http://127.0.0.1:8124/?map=skatepark&opponent=dummy).
The course spans 28,800 world units, with ramps, three loops, seven speed pads,
and three anchored chains. Riders start with a hoverboard. The camera follows
the local rider, including when the other network seat travels out of view.

- **Move / swing:** A/D or left/right on the controller.
- **Climb:** W/S or up/down while attached.
- **Grab:** Space + Enter, or controller A + B, while crossing a chain.
  Holding grab before the crossing also works.
- Release the buttons to stay attached. Press grab again to let go with the
  chain's momentum; release the buttons before another grab can latch.
- **Inspect hitboxes:** Tab / View.

Use `?map=skatepark` for the existing multiplayer room flow. The host's deal
selects the map for both seats. Chain nodes, previous positions, rider
attachments, loop progress and boosts participate in rollback. Live viewers
receive bounded chain snapshots. Both players need the new protocol (v2).

Character combat retains the mainline `sampleCombatBoxes` model, including
animated pose sampling, named damage zones, and limb-loss rules. Rope grabs
use the same swept-box intersection helper as projectiles.
