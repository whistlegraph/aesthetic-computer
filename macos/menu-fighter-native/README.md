# Menu Fighter — native macOS

A browserless Swift/AppKit build of Menu Fighter. SpriteKit supplies the native
Metal-backed render loop; GameController and AppKit provide input; AVFoundation
supplies immediate hit/round audio.

```sh
cd macos/menu-fighter-native
swift run menu-fighter
```

Put two tiny stick figures across a screen-wide stage over the macOS desktop in
a transparent, chromeless, click-through overlay. Close-range punches win through
knockback and ring-out; normal clicks continue to reach the app underneath. The
overlay closes itself after the round ends.

```sh
swift run menu-fighter --desktop
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
- `Escape` opens the Menu Fighter card; `Return` starts/resets TRAIN.
