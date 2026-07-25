# Blueberry Cabinet

A minimal native AppKit control hint for MAME on Blueberry and Neo. It launches
Street Fighter II: Champion Edition in a fixed, nearest-neighbor-scaled window
with a compact QWERTY legend underneath. MAME uses the OpenGL renderer and its
low-latency frame path to keep the game's native refresh smooth.

```zsh
slab/blueberry-cabinet/install.sh
open "$HOME/Applications/Blueberry Cabinet.app"
```

The control hint and MAME are separate native windows. Closing MAME closes the
hint.

Controls live in `~/Arcade/ctrlr/blueberry.cfg`, a persistent controller
profile loaded with `-ctrlr blueberry`. MAME never rewrites this file as part
of its per-game configuration lifecycle.
