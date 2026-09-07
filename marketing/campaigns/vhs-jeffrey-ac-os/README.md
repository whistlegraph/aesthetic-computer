# VHS Jeffrey / AC OS

`source.png` is grounded in the Jeffrey platter, the ThinkPad Yoga 11e product reference, and a real AC Native screen capture. `generate.mjs` creates the original face-animation version.

`generate-v2.mjs` uses OmniHuman 1.5 for full-body performance, refines the mouth with Sync-3, and applies the local VHS finish.

`generate-v3-identity.mjs` transfers that performance through Kling v3 Pro with Jeffrey's real portrait bound as a facial identity element before the Sync-3 mouth pass.

`generate-seedance-2.5-test.mjs` makes a four-second 480p multi-angle identity test using Seedance 2.5 reference-to-video.

`generate-v4-multiview.mjs` binds Jeffrey's front, three-quarter, profile, and full-body plates into one Kling v3 identity element and keeps Kling's inherited mouth motion for evaluation before another paid lipsync pass.

```sh
node marketing/campaigns/vhs-jeffrey-ac-os/generate.mjs
node marketing/campaigns/vhs-jeffrey-ac-os/generate-v2.mjs
node marketing/campaigns/vhs-jeffrey-ac-os/generate-v3-identity.mjs
node marketing/campaigns/vhs-jeffrey-ac-os/generate-seedance-2.5-test.mjs
node marketing/campaigns/vhs-jeffrey-ac-os/generate-v4-multiview.mjs
```

- `jeffrey-ac-os-clean.mp4` — clean Sync-3 master
- `jeffrey-ac-os-vhs.mp4` — 720×1280 VHS-finished reel
- `source-v2-full-body.png` — full-body studio source for the performance model
- `jeffrey-ac-os-omnihuman-v1.5.mp4` — clean whole-body performance master
- `jeffrey-ac-os-clean-v2.mp4` — OmniHuman performance with the Sync-3 mouth pass
- `jeffrey-ac-os-vhs-v2.mp4` — square full-body OmniHuman 1.5 performance with a Sync-3 mouth pass
- `source-v3-identity.png` — identity-corrected studio source
- `jeffrey-ac-os-kling-v3-identity.mp4` — identity-bound full-body motion master
- `jeffrey-ac-os-clean-v3-identity.mp4` — identity-bound master with the Sync-3 mouth pass
- `jeffrey-ac-os-vhs-v3-identity.mp4` — identity-bound final VHS cut
- `jeffrey-ac-os-kling-v3-multiview.mp4` — clean multi-angle identity test
- `jeffrey-ac-os-vhs-v4-multiview.mp4` — VHS finish without an additional mouth pass
- `source-prompt*.txt` — image references and exact prompts
