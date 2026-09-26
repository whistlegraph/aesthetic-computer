# Aesel donkey 3D

Preparation only. No generation has been submitted.

Reference identity: `../../shared/assets/donkey-pencil-run-v2.png`.
Current character contract: [`../donkey-character.md`](../donkey-character.md), including the defining red reading glasses.
`reference-prompt.txt` specifies one neutral quadruped pose for reconstruction.

Planned stages:

1. Produce and inspect a single neutral reference with the existing pencil donkey identity. Proposed fal endpoint: `openai/gpt-image-2.5/sunburst/edit`, 1024 square, high quality. fal's measured short-prompt example is about $0.061; this larger source and longer prompt may add input cost.
2. Generate one textured model through `meshy/v7.1/image-to-3d`, 30,000 triangle target, PBR textures, no humanoid rigging. Published price: $1.20.
3. Inspect the GLB from several angles for four distinct legs, ears, tail, and muzzle; preserve raw output and provenance.
4. Attempt a local quadruped armature and weights. Neither the current fal Meshy rigging endpoint nor Meshy's direct rigging API supports quadrupeds. Their web app offers quadruped rigging, but that is a separate route. Do not enable the humanoid rig merely to obtain a nominally rigged file.

Illy backends/pipelines/contracts were consulted; fal credentials are available. Its checked-in provider catalog contains raster adapters only, so a Meshy plan needs an explicit 3D runner rather than silently choosing another image model. Apply the shared `physical-accuracy` prompt contract to the reference generation.

Sources checked September 25, 2026:

- [Meshy 7.1 schema](https://fal.ai/models/meshy/v7.1/image-to-3d/api)
- [Meshy 7.1 price](https://fal.ai/models/meshy/v7.1/image-to-3d)
- [Reference editor price](https://fal.ai/gpt-image-2.5)
- [fal rigging restriction](https://fal.ai/models/fal-ai/meshy/rigging/api)
- [Meshy API restriction](https://docs.meshy.ai/en/api/rigging)
- [Meshy web app quadruped rigging](https://help.meshy.ai/en/articles/16231707-how-to-create-3d-animation-with-auto-rigging)
