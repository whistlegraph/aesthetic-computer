---
name: illy
description: Orchestrate Aesthetic Computer illustrations across OpenAI, fal.ai, Codex built-in image generation, and future backends. Use when a user says illy, asks to generate or edit campaign art, covers, panels, storyboards, visual variants, or wants to compare image providers or configure a marketing/pop media pipeline.
---

# Illy

Treat an “illy” as the project’s provider-neutral illustration artifact. Preserve the creative brief and provenance while allowing the rendering backend to change.

## Workflow

1. Inspect the target lane before generating.
   - Marketing campaign: preserve `brand-brief.md`, `cover-prompt.txt`, `refs/`, and `gens/`.
   - Pop lane: keep still-image prompts separate from motion prompts; the illy is the source frame for `pop/lib/motion-pipeline.mjs`.
2. Call `illy_backends` and `illy_pipelines` before selecting a paid route. Never infer that a missing credential is available.
3. Call `illy_plan` before a batch or unfamiliar model. Show the chosen provider, model, references, output, and stages.
4. Choose execution explicitly:
   - Use `illy_generate` for OpenAI/fal API generation, automation, precise model choice, or reproducibility.
   - Use Codex’s built-in image tool for an interactive subscription-backed render when available. The MCP cannot invoke that tool; after rendering, save the image at the planned output and call `illy_record` to write provenance.
5. Preserve cached outputs unless the user requests a reroll. Use `force: true` only deliberately; Illy archives the replaced image.
6. Inspect the result. Iterate with one targeted prompt change, then retain the selected take.

## Provider selection

- Prefer an explicitly requested provider/model.
- Otherwise use the pipeline’s provider order and the first credentialed adapter that supports the request.
- OpenAI is the default for multi-reference edits and AC identity/style continuity.
- fal.ai is appropriate for model shopping, specialized endpoints, and pipeline breadth. A fal model entry declares its input/output mapping; do not assume all endpoints share a schema.
- Never silently fall back after a paid request fails. Return the failure and let the user choose whether to retry or switch.

Read [pipeline-contract.md](references/pipeline-contract.md) before adding providers, models, or stages.
