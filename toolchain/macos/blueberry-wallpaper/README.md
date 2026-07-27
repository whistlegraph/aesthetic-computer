# Blueberry Wallpaper

A lightweight live desktop for the fleet machine `blueberry`, using the exact
Meshy v6 AC Pals model recovered for the Xbox native app:
`xbox/assets/pals-mesh-nat-amethyst.glb`.

The build converts the GLB to USD and renders its real geometry into ten compact
rotation sheets: five glossy blue material variants for each macOS appearance.
The live app uses only Core Animation, cross-dissolving adjacent angles for a
smooth turn while eight translucent pals rise through independent, gently
swaying paths. Both blend layers share one decoded texture. SceneKit and the
5 MB GLB are build tools only; Blueberry never runs a live mesh renderer.

Dark mode uses luminous cobalt, azure, periwinkle, and ice blue over a deep
blueberry field. Light mode uses darker indigo and ocean-blue models over a
misty powder-blue field. The window sits beneath Finder's desktop icons, ignores
input, joins every Space, and pauses while the display sleeps.

Until the Xbox recovery commit lands in the main checkout, `build.sh` also finds
the GLB in `.worktrees/xbox-native-v11/`.

```bash
./toolchain/macos/blueberry-wallpaper/build.sh
./toolchain/macos/blueberry-wallpaper/remote-install.sh blueberry
```

The launch agent is `computer.aesthetic.blueberry-wallpaper`. Logs land in
`~/.local/share/blueberry-wallpaper/` on Blueberry. Reinstalling moves the prior
app bundle to the Trash before replacing it.
