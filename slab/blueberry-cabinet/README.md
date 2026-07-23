# Blueberry Cabinet

A native AppKit cabinet surround for MAME on Blueberry. It launches Street
Fighter II: Champion Edition in a fixed, nearest-neighbor-scaled window and
keeps the QWERTY control map visible on the cabinet deck.

```zsh
slab/blueberry-cabinet/install.sh
open "$HOME/Applications/Blueberry Cabinet.app"
```

The cabinet and MAME are separate native windows. MAME sits over the bezel's
screen aperture; closing MAME closes the cabinet.

Controls live in `~/Arcade/ctrlr/blueberry.cfg`, a persistent controller
profile loaded with `-ctrlr blueberry`. MAME never rewrites this file as part
of its per-game configuration lifecycle.
