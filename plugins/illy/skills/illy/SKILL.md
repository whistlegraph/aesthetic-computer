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
- OpenAI is the default for multi-reference edits and AC identity/style continuity. Shared pipelines default to `gpt-image-2.6-sunburst` (released 2026-10-28; confirmed in the account's model list and on the [official model page](https://developers.openai.com/api/docs/models/gpt-image-2.6-sunburst) on 2026-11-20, generation and edit endpoints both supported). `gpt-image-2.6-flare` is the faster option; the 2.5 pair stays registered for reruns. Its declared sizes and qualities are carried over from 2.5 and only 1024x1024 at `high` has been exercised. To see whether a newer model exists, list `/v1/models` and filter for `image`; do not trust this paragraph's date. Preserve intentional model overrides and historical provenance.
- fal.ai is appropriate for model shopping, specialized endpoints, and pipeline breadth. A fal model entry declares its input/output mapping; do not assume all endpoints share a schema.
- Rechecked 2026-10-30: Sunburst still leads the public edit leaderboards, and both 2.5 models accept `xhigh`/`max` quality and 2K/4K sizes. The fal default is Google's `google/nano-banana-2.1` (edits go to `google/nano-banana-2.1/edit`), and Black Forest Labs' `blackforestlabs/flux-3/text-to-image` and `/edit-image` are registered too. These newer endpoints take `image_urls` (up to 10 refs), `aspect_ratio`, and a `resolution` tier in place of `image_size`. The older fal entries stay selectable.
- Never silently fall back after a paid request fails. Return the failure and let the user choose whether to retry or switch.

Read [pipeline-contract.md](references/pipeline-contract.md) before adding providers, models, or stages.

## Baseline inference check

Every illy is **unverified** until it has been looked at three times and the verdicts are on file. `illy_generate` and `illy_record` write `check.status: "unverified"` into the render's `.illy.json`; nothing else clears it.

1. `illy_check { output }` returns the render, four quadrant crops enlarged two times, the prompt it must satisfy, and the three passes from `config/checks.json`. It never certifies a pass.
2. Inspect once per pass, without merging them:
   - **inventory**: every named person, object, screen and letter, counted against the prompt;
   - **orientation**: each laptop, phone, monitor and held object one at a time, naming the face the camera sees. A clamshell reads keyboard, hinge, screen. Screen content on the far side of a lid, or a keyboard behind its own screen, fails;
   - **contact**: fingers and joints, supports and shadows, scale between copies, light with no source.
3. `illy_verdict { output, passes }` records `pass` or `fail` with notes for all three, bound to the image's SHA-256. A reroll is unverified again.

Any fail means one targeted prompt change, a forced reroll, and a new check. Do not place an unverified or failed illy in a paper, post or deck. When a prompt has devices in it, state each one's facing in a single rule ("keyboard nearest the camera, hinge at the far edge, screen rising behind") rather than describing them one by one.
