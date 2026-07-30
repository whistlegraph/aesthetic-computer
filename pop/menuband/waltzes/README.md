# Menu Band Waltzes

Three one-minute, 3/4 performance previews:

- `01-lantern` — 72 BPM, acoustic grand piano
- `02-window` — 84 BPM, electric piano
- `03-ferry` — 96 BPM, accordion

Render all three:

```bash
node pop/menuband/bin/render-menu-band-waltzes.mjs --jobs 3
```

Outputs land in `pop/menuband/out/menu-band-waltzes/`.

## Metal paper-loop renderer

Build the score textures, then the offline renderer:

```bash
for id in 01-lantern 02-window 03-ferry; do
  node pop/menuband/bin/render-raytracer-textures.mjs --id "$id"
done

cd pop/menuband/raytracer
swift build -c release
```

Render one frame:

```bash
.build/release/MenuBandRaytracer \
  --score assets/01-lantern/score.png \
  --keyboard assets/01-lantern/keyboard.png \
  --out out/lantern.png --time 6.25 \
  --size 1080x1920 --spp 32
```

Frame ranges can be split across machines with `--shard-index` and
`--shard-count`. The accepted previews are in
`pop/menuband/out/menu-band-waltzes/raytraced/`.
