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
2. Call `illy_backends`, `illy_pipelines`, and `illy_contracts` before selecting
   a paid route. Never infer that a missing credential is available.
3. Call `illy_plan` before a batch or unfamiliar model. Show the chosen provider, model, references, output, and stages.
4. Choose execution explicitly:
   - Use `illy_generate` for OpenAI/fal API generation, automation, precise model choice, or reproducibility.
   - Use Codex’s built-in image tool for an interactive subscription-backed
     render when available. The MCP cannot invoke that tool: append every
     contract resolved by the planned pipeline to the image prompt before
     rendering, then save the image and call `illy_record` with those contract
     identifiers. Never claim an unapplied contract in provenance.
5. Preserve cached outputs unless the user requests a reroll. Use `force: true` only deliberately; Illy archives the replaced image.
6. Inspect the result. Iterate with one targeted prompt change, then retain the selected take.

## Physical beats

- Every pipeline enforces `physical-accuracy`; `pop-panel` additionally
  enforces `extreme-physical-beats`.
- A dynamic illy depicts one exact temporal state. It must expose weight,
  support, grips, mounts, contact shadows, direction of travel or force, and a
  physically possible camera position. Reject spectacle that hides mechanics.
- Before animating action, transformation, POV, or moving-platform panels,
  encode the motion shot as `physical: "extreme"` with at least two ordered
  normalized beats and explicit contacts. The shared motion pipeline rejects
  incomplete extreme entries before any paid generation.
- Inspect the contracted relationships in the illy itself and again in the
  render. A final cannot repair an invalid source frame by implication.

## Provider selection

- Prefer an explicitly requested provider/model.
- Otherwise use the pipeline’s provider order and the first credentialed adapter that supports the request.
- OpenAI is the default for multi-reference edits and AC identity/style continuity.
- fal.ai is appropriate for model shopping, specialized endpoints, and pipeline breadth. A fal model entry declares its input/output mapping; do not assume all endpoints share a schema.
- Never silently fall back after a paid request fails. Return the failure and let the user choose whether to retry or switch.

Read [pipeline-contract.md](references/pipeline-contract.md) before adding providers, models, or stages.
