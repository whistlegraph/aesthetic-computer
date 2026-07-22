# Menu Fighter — native macOS

A browserless Swift/AppKit build of Menu Fighter. SpriteKit supplies the native
Metal-backed render loop; GameController and AppKit provide input; AVFoundation
supplies immediate hit/round audio.

```sh
cd macos/menu-fighter-native
swift run menu-fighter
```

Put the fighters on a small central stage over the macOS desktop in a
transparent, chromeless, click-through overlay. Shots win through knockback and
ring-out; normal clicks continue to reach the app underneath. The overlay closes
itself after the round ends.

```sh
swift run menu-fighter --desktop
```

Install the fleet-only four-corner trackpad watcher:

```sh
./install.sh
```

Hold one finger in each trackpad corner for five uninterrupted seconds. A
rising noise countdown cancels as soon as the pose breaks. Completion opens the
desktop stage in search/practice mode with Player 2 acting as the dummy. Practice
continues while it searches for another native player, then both clients enter
the scoped online round.

Public matchmaking requires an Aesthetic Computer access token. Save it in the
macOS Keychain once with `menu-fighter auth <token>`. `AC_TOKEN` and
`~/.config/aesthetic-computer/token` are also supported for development.

Controls:

- Trackpad (Player 1): move the pointer to run, click/tap to shoot, two-finger
  click to fire a heavy shot, and two-finger swipe upward to jump.
- Player 1: `A/D` move, `W` jump, `F` light attack, `G` heavy attack.
- Player 2: arrows move/jump, `/` light attack, `.` heavy attack.
- Controllers: d-pad/stick, A jump, X light, Y heavy.
- `Escape` opens the Menu Fighter card; `Return` starts/resets TRAIN.
