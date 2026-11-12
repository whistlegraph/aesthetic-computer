# Blueberry Wallpaper

A lightweight live desktop for the fleet machine `blueberry`, using the exact
Meshy v6 AC Pals model recovered for the Xbox native app:
`xbox/assets/pals-mesh-nat-amethyst.glb`.

The app bundles the original GLB and a SceneKit-readable USD conversion of the
same mesh. An `SCNRenderer` draws that geometry into a Metal layer the app owns,
one frame per display-link tick: a whole number of refreshes per frame, never
under 60 fps (144 Hz → 72, 120 → 60). Varied-size Pals marks rotate
independently while buoyant acceleration and a gentle sway carry them upward.
Every instance uses shared live mesh data rather than a bitmap sprite. One
imported prototype and its geometry/materials are shared across displays and
appearance rebuilds. Imported texture maps are discarded; the field uses a
single opaque accent material.

Antialiasing is full-scene: 4× MSAA everywhere, and on 1× displays the scene
renders at twice the backing resolution and is averaged down. The multisample
targets are memoryless, so they cost no RAM. `--supersample <1–3>` and
`--msaa <1|2|4>` (or `$BLUEBERRY_WALLPAPER_SUPERSAMPLE`, `$BLUEBERRY_WALLPAPER_MSAA`)
override. stdout carries a frame report — fps, worst frame, late frames, render
size — ten seconds after launch, at a minute, then every ten.

With a fishery feed present, the dark backdrop is morphing water: a fragment
shader on one plane behind the stage walks the hue round the wheel every five
minutes while slow currents bend it across the screen. The haze, the marks'
tint and the menu bar's backing follow about once a second. The lighting
environment stays fixed in that mode — SceneKit refilters it on every change,
which drops a frame.

Keep Core Animation shadows off anything that moves over the field. A layer
shadow without a shadow path is re-blurred on every composite; the fishery's
labels alone held a 144 Hz display to 36 fps. `HaloTextLayer` draws the halo
into the text's own bitmap instead.

The accent-shaded model floats over a blueberry field that
responds to macOS appearance. The window sits beneath Finder's desktop icons,
ignores input, joins every Space, and pauses while the display sleeps.

Appearance changes and display wake recreate the desktop windows after a
2.5-second settling period. WindowServer can retain a light menu-bar gradient
over a dark field even when both macOS and Slab report Dark; resuming animation
or restarting Dock alone does not repair that state. Verify the composed menu
bar as well as the wallpaper window after either transition.

Until the Xbox recovery commit lands in the main checkout, `build.sh` also finds
the GLB in `.worktrees/xbox-native-v11/`.

```bash
./toolchain/macos/blueberry-wallpaper/build.sh
./toolchain/macos/blueberry-wallpaper/remote-install.sh blueberry
```

The launch agent is `computer.aesthetic.blueberry-wallpaper`. Logs land in
`~/.local/share/blueberry-wallpaper/` on Blueberry. Reinstalling moves the prior
app bundle to the Trash before replacing it.

## Fuser minis: fishery, one push, one tank

On panda and chicken the field carries a live fishery (`Sources/Fishery.swift`)
fed by the vault's `fuser-fishery` agent. All of its type is GNU Unifont
(`Resources/unifont.otf`, 17.0.03), at whole multiples of 16 device pixels with
no antialiasing and a hard one-pixel shadow.

```bash
./push-minis.sh               # compile + swap on panda and chicken at once
./push-minis.sh --feeder      # also ship the vault feeder
./push-minis.sh --rollback    # back to the previous Sources and binary
./tank-install.sh panda chicken   # one tank: panda left, chicken right
```

A tank (`Sources/Tank.swift`) makes each screen a pane of one wider world. The
leader simulates every fish and streams them over loopback; a reverse SSH tunnel
from the leader carries that port to each follower, so the app never listens
off-loopback and neither firewall is involved. Followers draw 0.18 s behind,
interpolated, and fall back to their own feed when the stream stops.
