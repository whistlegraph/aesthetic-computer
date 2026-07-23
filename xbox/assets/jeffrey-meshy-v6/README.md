# Jeffrey Meshy v6 head

`character.glb` is the accepted high-detail Meshy v6 multi-image master built
from the four quadrants of the canonical platter plate at
`thespianjas/assets/versions/v002/refs/jeffrey-modeling-plate.png`.

- Master: 47,749 triangles, 2048×2048 embedded PBR maps.
- Preview: `preview.png`.
- Exact request, seed, and returned artifact metadata: `result.json`.
- Submitted views: `refs/` (front, profile, three-quarter, full body).
- Xbox head LOD: `../jeffrey-head-xbox.json` (4,915 colored triangles).
- Xbox animated body: `../jeffrey-run-bake.json` (24-joint idle plus a distinct
  20-frame procedural run cycle).

Regenerate the Meshy master with `xbox/tools/meshy-jeffrey-character.mjs` and
derive the console head LOD with `xbox/tools/glb-head-bake.mjs`.
