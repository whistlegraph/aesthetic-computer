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

## A take, frame by frame (same geometry)

```
xvfb-run -a -s "-screen 0 1920x1080x24" node rax/film/shoot.mjs bikes --chrome --record [--cache dir]
blender -b -P rax/blender/rax_import.py -- --record rax/film/out/<take>.raxrec \
  --fps 24 --samples 16 --size 1280x720 --out frames/ [--frames 1-120] [--flat] [--blend take.blend]
ffmpeg -framerate 24 -i frames/f%04d.png -c:v libx264 -crf 16 -pix_fmt yuv420p take.mp4
```

`--record` puts `recorder.mjs` in the film's camera page from the roll to the
wrap. It keeps every model rax's projector drew, once each (by content), and
for every frame where each one was drawn, the camera (eye, axes, field of
view), the small planet's radius as the view bent the land, and the sky
(phase, sun, colours, the other planets from `orbits.mjs`). A screen running
a KidLisp or `.mjs` piece is a model like any other, so its frames come out as
they were drawn. The sky's dome and the shadows are not kept: Blender makes
its own.

`rax_import.py --record` builds each model as a mesh with its corner colours,
each draw as an object keyed only when it moves, the camera keyed per frame,
the sky as a gradient with a sun lamp and the planets as lit balls far off
along their directions, and the small planet's bend as a Geometry Nodes
modifier (`rax-bend`: `planet.mjs` `bend` about the recorded eye with the
recorded radius; `--flat` leaves it out). Small models that come back each
frame with new colours only (a rolling ball rax re-lights) are one model.
On jastow (RTX 3070, Blender 5.2) a 1280×720 frame at 16 samples takes about
3.7 s. Linux Chrome has a GPU only in a window: hence `xvfb-run`.
