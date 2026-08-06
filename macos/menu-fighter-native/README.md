# Trackpad Fighter — native macOS

A browserless Swift/AppKit starfighter game built around the Mac trackpad. SpriteKit supplies the native
Metal-backed render loop; GameController and AppKit provide input; AVFoundation
supplies immediate hit/round audio.

```sh
cd macos/menu-fighter-native
swift run trackpad-fighter
```

Put two tiny stick figures across a screen-wide stage over the macOS desktop in
a transparent, chromeless, click-through overlay. Close-range punches win through
knockback and ring-out; normal clicks continue to reach the app underneath. The
overlay closes itself after the round ends.

```sh
swift run trackpad-fighter --desktop
```

Install Menu Fighter alongside Trackpad Fighter:

```sh
./install.sh
```

Trackpad Fighter owns the fleet-only four-corner gesture. Launch this distinct
hand-to-hand game with `menu-fighter --desktop`; Player 2 begins as the local
dummy while the native matchmaker searches for another player.

Public matchmaking requires an Aesthetic Computer access token. Save it in the
macOS Keychain once with `menu-fighter auth <token>`. `AC_TOKEN` and
`~/.config/aesthetic-computer/token` are also supported for development.

Controls:

- Trackpad (Player 1): move the pointer to run, click/tap to punch, two-finger
  click for a heavy punch, and two-finger swipe upward to jump.
- Player 1: `A/D` move, `W` jump, `F` light attack, `G` heavy attack.
- Player 2: arrows move/jump, `/` light attack, `.` heavy attack.
- Controllers: d-pad/stick, A jump, X light, Y heavy.
- The desktop round captures mouse, trackpad, and keyboard input across every
  display so clicks, drags, scrolling, and hover state never reach apps or Prox
  rocks underneath it. The standard cursor stays hidden until the round ends;
  `Escape` quits immediately. In windowed mode, `Escape` opens the local pause
  card and `Return` starts/resets the fight.

## Round lifetime logs

Every watcher launch and fight round appends structured JSON lines to:

```text
~/.local/share/trackpad-fighter/rounds.jsonl
```

Events share process, launch, scene, and round IDs and include monotonic elapsed
times. The trace starts before the scene is built, records duplicate scene-attach
attempts, intro/reset/combat events, attacks and hits, and finishes with the round
outcome and process teardown. A crash or forced kill deliberately leaves a
`round_started` entry without `round_finished`, so incomplete lifetimes remain
discoverable.

```sh
# Follow new events.
tail -f ~/.local/share/trackpad-fighter/rounds.jsonl | jq .

# Reconstruct one round in order.
jq --arg id '<roundId>' 'select(.roundId == $id)' \
  ~/.local/share/trackpad-fighter/rounds.jsonl
```
