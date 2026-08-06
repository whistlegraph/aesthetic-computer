# Trackpad Fighter — native macOS

A browserless Swift/AppKit starfighter game built around the Mac trackpad. SpriteKit supplies the native
Metal-backed render loop; GameController and AppKit provide input; AVFoundation
supplies immediate hit/round audio.

```sh
cd macos/menu-fighter-native
swift run trackpad-fighter
```

Put two tiny, high-contrast triangle starfighters over the macOS desktop in a
transparent, chromeless overlay. Ships rotate, thrust, fire, and wrap around the
screen while sparse drifting asteroids threaten both players and break under fire.
The overlay closes itself after the round ends.

```sh
swift run trackpad-fighter --desktop
```

Install the fleet-only four-corner trackpad watcher:

```sh
./install.sh
```

Touch all four trackpad corners at once. A strong accent-color flash immediately
covers every display and launches the encounter; lifting the fingers cannot cancel
startup. Sparse star streaks then warp from the desktop center toward all four
corners and brake to a stop as a short ascending beep phrase plays. There is no
separate intro screen. Once the stars stop, the live scene unlocks directly into
play with no login step. The gesture is ignored while Menu Band owns the trackpad
for its percussion/FX surface.

Controls:

- Trackpad (Player 1): touch with one finger to highlight the ship. Every finger
  movement immediately points and thrusts the ship in that direction; lifting
  stops adding force while momentum continues. A short stationary tap fires.
  Two-finger acceleration is intentionally disabled for now. Player 1 begins at
  screen center, and the ship palette follows the current macOS system accent color.
- All game marks use the same cursor-like line stack: black outer edge, light
  keyline, and system-accent center line. There is no separate reticle, tether,
  stored charge, or release animation.
- Active thrust sheds short line particles from the rear of the triangle. They
  inherit ship momentum and enter SpriteKit's shared gravity field with damping
  and a short lifetime.
- Player 1: `A/D` rotate, `W` thrust, `F` light shot, `G` heavy shot.
- Player 2: left/right arrows rotate, up arrow thrust, `/` light shot, `.` heavy shot.
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
