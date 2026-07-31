# Blueberry Wallpaper

A lightweight live desktop for the Fuser seat machines, using the exact Meshy
v6 AC Pals model recovered for the Xbox native app:
`xbox/assets/pals-mesh-nat-amethyst.glb`.

The build converts the GLB to USD for SceneKit while preserving its real
geometry. Eighteen small translucent live mesh instances rise through a
collision-resistant, full-bleed blue-noise field and rotate continuously at
30 fps. The outer lanes crop at the display edges so the field continues beyond
the frame.

Dark mode uses luminous cobalt, azure, periwinkle, and ice blue Pals; light mode
uses darker versions of the same family. The material palette blends the host's
macOS accent with Slab's current aggregate prompt/status colour, while the flat
field uses the system accent darkened or lightened for the current appearance.
Accent, appearance, and prompt-state changes update live. The window sits
beneath Finder's desktop icons, ignores input, joins every Space, and pauses
while the display sleeps. Restrained lighting, 2× antialiasing, and the 30 fps
cap keep the always-on layer inexpensive on battery. Neo uses the same scene at
15 fps without MSAA to protect the interactive pointer seat; Blueberry uses
30 fps with 2× MSAA.

Until the Xbox recovery commit lands in the main checkout, `build.sh` also finds
the GLB in `.worktrees/xbox-native-v11/`.

```bash
./toolchain/macos/blueberry-wallpaper/build.sh
./toolchain/macos/blueberry-wallpaper/remote-install.sh blueberry
```

The launch agent is `computer.aesthetic.blueberry-wallpaper`. Logs land in
`~/.local/share/blueberry-wallpaper/` on Blueberry. Reinstalling moves the prior
app bundle to the Trash before replacing it.
