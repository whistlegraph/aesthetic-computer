# Blueberry Wallpaper

A lightweight live desktop for the fleet machine `blueberry`, using the exact
Meshy v6 AC Pals model recovered for the Xbox native app:
`xbox/assets/pals-mesh-nat-amethyst.glb`.

The app bundles the original GLB and a SceneKit-readable USD conversion of the
same mesh. SceneKit renders that geometry at 30 FPS at each display's native
backing resolution, with 2× MSAA and two drawable buffers. Varied-size Pals marks rotate independently while buoyant
acceleration and a gentle sway carry them upward. Every instance uses shared
live mesh data rather than a bitmap sprite. One imported prototype and its
geometry/materials are shared across displays and appearance rebuilds. Imported
texture maps are discarded; the field uses a single opaque accent material.

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
