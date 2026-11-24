# rax export

Take rax out to Blender: the room as the real renderer draws it, and walks
replayed through it.

## Scene

```
node rax/export/scene.mjs lobby rax-lobby.glb        # [--url https://aesthetic.computer/rax/] [--wait 12000]
node rax/export/scene.mjs basement rax-basement.glb
```

Opens the room in headless Chrome as a guest (watching, no seat), patches the
page's own `Projector` (project.mjs) for one whole frame and keeps every model
handed to `draw()` in world space, before culling, clipping, the lens and the
planet bend. The sky (drawn with `bends` off) and overlays (which never pass
through `draw()`) are left out. Output: one flat glTF mesh with vertex
colours, rax metres, y up. Each capture also holds whatever that room shows of
its neighbours (the tower above the basement, the cellar below the lobby), so
when merging, keep the lobby's faces above the floor and the basement's below
(`rax_import.py` does this with `:above` / `:below`).

## Walks

`walks/ucla-2026-11-23.json` is the UCLA class (Mon 2026-11-23, 11:07–11:25 PT)
from the walk log (`rax/walks.mjs`) via the `rax_walks` analytics tool. Each
sample is `[t_ms, x, y, z, yaw]`; `y` is eye height (feet = y − 1.7) and
facing is `(sin yaw, cos yaw)` in x, z, the same frame as the scene capture.

## Blender

`rax/blender/rax_import.py` is both an add-on (File ▸ Import ▸ rax scene /
rax walks) and a headless renderer:

```
blender -b -P rax/blender/rax_import.py -- \
  --scene rax-lobby.glb:above --scene rax-basement.glb:below \
  --walks rax/export/walks/ucla-2026-11-23.json --from 19:07:30 --to 19:10:30 \
  --speed 10 --fps 24 --samples 24 --out frames/
```

`--from`/`--to` are UTC on the walk file's date. Cycles picks OptiX/CUDA when
there is a GPU. On jastow (RTX 3070) a 1280×720 frame at 24 samples takes
about 3.6 s.
