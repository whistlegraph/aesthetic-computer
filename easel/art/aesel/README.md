# Aesel

Easel's donkey companion, painting at a small easel.

`sheet-source.png` is the generated source, with its prompt and Illy provenance alongside it. The route is explicitly fal `openai/gpt-image-2.5/flare/text-to-image`; generation never retries a paid POST or changes providers. `generate.mjs` prints a plan by default; `--generate` requires an externally provided FAL_KEY or AC_VAULT_ENV and refuses to overwrite an existing take.

`node easel/art/aesel/compile.mjs` extracts the16 cells, removes chroma green, samples each256px cell at4:1, and aligns the ground/right anchors. It emits the64px sprite frames, JSON animation manifest and icon pose into `desktop/assets/`. `bash easel/desktop/build/generate-icon.sh` packages the macOS icon.

The animation design follows `xbox/live/oskiewar-frame-design.md`: named poses, explicit frame holds, fixed anchors, and independent state/animation clocks. Oskiewar's current pipeline is geometry-driven; this companion uses generated raster poses under the same timing contract. No reference game sprites were imported or traced.

| State | Frames | Hold times | Trigger |
|---|---|---|---|
| Idle | 0–1 |1400,180ms | Quiet and ready |
| Awake | 2–3 |500ms | Wake, completion, attention |
| Sleeping | 4–7 |900ms | Ready with no interaction for60s |
| Working | 8–11 |180ms | Generation/tool activity |
| Running | 12–15 |120ms | Starting/connecting/queued |

Desktop source frames are64×64, displayed at128×128 with default16px text. Each sprite pixel is twice a Unifont pixel, following Cmd+/− text zoom. The bottom-right companion faces left toward the conversation. Scaling stays nearest-neighbor. Visibility pauses timers; reduced motion uses still poses. The native terminal uses a matching ASCII donkey, with its compact companion at bottom-right. Runtime images are packaged locally: no boot-time generation or network image requests.
