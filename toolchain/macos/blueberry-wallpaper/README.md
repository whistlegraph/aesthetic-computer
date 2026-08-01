# Blueberry Wallpaper

A lightweight live desktop for the fleet machine `blueberry`, using the exact
Meshy v6 AC Pals model recovered for the Xbox native app:
`xbox/assets/pals-mesh-nat-amethyst.glb`.

The app bundles the original GLB and a SceneKit-readable USD conversion of the
same mesh. SceneKit renders that geometry live at each display's native backing
resolution. Varied-size Pals marks rotate independently while buoyant
acceleration and a gentle sway carry them upward. Every instance uses shared
live mesh data rather than a bitmap sprite; its triangle topology is drawn as a
translucent system-accent wireframe without surface textures or lighting.

The model keeps its original amethyst textures over a blueberry field that
responds to macOS appearance. The window sits beneath Finder's desktop icons,
ignores input, joins every Space, and pauses while the display sleeps.

Until the Xbox recovery commit lands in the main checkout, `build.sh` also finds
the GLB in `.worktrees/xbox-native-v11/`.

```bash
./toolchain/macos/blueberry-wallpaper/build.sh
./toolchain/macos/blueberry-wallpaper/remote-install.sh blueberry
```

The launch agent is `computer.aesthetic.blueberry-wallpaper`. Logs land in
`~/.local/share/blueberry-wallpaper/` on Blueberry. Reinstalling moves the prior
app bundle to the Trash before replacing it.
