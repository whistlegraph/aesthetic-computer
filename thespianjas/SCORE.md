# thespianjas

`thespianjas` is the global Jeffrey digital-twin subsystem for the AC monorepo.
It is not owned by one podcast or campaign. Podcast, Reel, lecture, piece, and
live-performance pipelines consume versioned assets from here.

## Architecture

1. **Identity** — canonical studio references resolve from
   `papers/jeffrey-platter` through `identity.json`.
2. **Reconstruction** — provider adapters turn those references into a textured,
   relightable GLB. Meshy 6 multi-image is the first adapter; Tripo H3.1 and SAM
   3D Body are named benchmark candidates.
3. **Studio** — `metal/` is the primary native macOS renderer. Its SceneKit
   scene graph is explicitly backed by Metal; key, fill, rim, ground, camera,
   skeletal playback, and later facial controls remain ours to direct. The
   browser studio is only a quick compatibility viewer.
4. **Performance** — podcast audio drives energy, breathing, head emphasis, and
   eventually facial blendshapes. A video lipsync provider may polish exported
   shots, but it is downstream of the canonical relightable twin.
5. **Compositing** — `marketing/essay-reels` adds word-timed captions and social
   safe-area layout to a rendered thespianjas performance.

The canonical slug is globally unique: `thespianjas`. Generated binaries are
large and versioned locally under `assets/versions/vNNN/`; manifests and recipes
are tracked in git.

## First build

```fish
node thespianjas/bin/generate.mjs --version v001 --provider meshy
cd thespianjas/metal && swift run ThespianJas ../assets/versions/v001/idle.glb
```

Meshy accepts one to four views and returns a textured PBR GLB. The initial
studio platter has a strong face view and two seated body views; a dedicated
front/side/back neutral-pose capture will materially improve the next rig.

The generated `rigged.glb` and `idle.glb` contain the humanoid skeleton. They do
not promise facial ARKit blendshapes; viseme/blendshape authoring is a separate
performance stage.

## Provider notes (July 2026)

- **Meshy 6 multi-image:** 1–4 views, PBR textures, A/T pose options, humanoid
  auto-rig, GLB/FBX/USDZ/OBJ. Best first complete asset lane.
- **Tripo H3.1:** useful single-image benchmark for likeness/geometry.
- **SAM 3D Body:** inexpensive human-specific reconstruction benchmark.
- **Kling / Sync lipsync:** video finishing layers, not substitutes for the
  relightable GLB. They accept rendered video or a portrait plus audio.

Identity outputs are publishable only in Jeffrey-operated AC surfaces. Never
silently substitute another person's photographs or train from private imagery.
