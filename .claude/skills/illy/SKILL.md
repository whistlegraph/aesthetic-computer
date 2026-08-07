---
name: illy
description: Orchestrate Aesthetic Computer illustrations across OpenAI, fal.ai, and future backends. Use when a user says illy, asks to generate or edit campaign art, covers, panels, storyboards, visual variants, compare image providers, or configure a marketing/pop media pipeline.
---

# Illy

Use the shared `illy` MCP as the source of truth. The implementation, provider registry, pipelines, and full workflow live under `plugins/illy/`.

## Workflow

1. Inspect the target production before generating.
   - Marketing: preserve `brand-brief.md`, `cover-prompt.txt`, `refs/`, and `gens/`.
   - Pop: keep illy/still prompts separate from motion prompts; the illy is the source frame for `pop/lib/motion-pipeline.mjs`.
2. Call `illy_backends`, `illy_pipelines`, and `illy_contracts` before choosing
   a paid route.
3. Call `illy_plan` before a batch or unfamiliar model. Report provider, model, references, output path, and stages.
4. Call `illy_generate` only after the requested route is clear. Existing output is cached unless `force: true`; rerolls archive the old take.
5. Inspect the output and iterate with one targeted prompt change.

Every pipeline enforces the shared `physical-accuracy` prompt contract.
`pop-panel` also enforces `extreme-physical-beats`: a dynamic illy must be one
mechanically valid temporal state with visible supports, contacts, grips,
mounts, weight distribution, force direction, and physically possible camera
geometry. When the built-in image tool is used, append the resolved contracts
to its prompt and record their identifiers with `illy_record`.

Action, transformation, POV, and moving-platform motion shots use
`physical: "extreme"`, ordered normalized `beats`, and explicit `contacts` in
their SHOTS entry. The shared motion pipeline rejects an incomplete extreme
contract before paid generation.

Prefer an explicitly requested provider/model. Otherwise use the configured pipeline order. Never silently switch providers after a paid request fails. OpenAI is the established multi-reference route; fal.ai exposes model-shopping and specialized endpoints. Credentials resolve from the environment or the existing vault file and must never be printed.

For new adapters or custom stages, read `plugins/illy/skills/illy/references/pipeline-contract.md`. Keep shared presets in `plugins/illy/config/`; pass `pipelineFile` for a production-local pipeline.
