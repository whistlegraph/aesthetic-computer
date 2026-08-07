# Illy pipeline contract

Pipelines are named JSON objects in `config/pipelines.json`. They describe conventions rather than provider request bodies.

For production-specific customization, pass `pipelineFile` to `illy_plan` or `illy_generate`. The JSON may contain one `{ "pipeline": { ... } }` or a named `{ "pipelines": { "name": { ... } } }` map. Keep durable shared presets in the plugin config; keep one-off track/campaign presets beside that production.

- `providerOrder`: adapter order used by `provider: "auto"`.
- `modelByProvider`: default model/endpoint for each adapter.
- `outputTemplate`: path relative to `targetDir`; supports `{slug}` and `{variant}`.
- `defaults`: values such as `size`, `quality`, and `variant`.
- `contracts`: shared prompt-contract identifiers from `config/contracts.json`.
  Pipeline contracts are always appended to the creative prompt; callers may
  add contracts but cannot silently remove pipeline requirements.
- `stages`: ordered provenance labels. Adapters own network execution.

The built-in pipelines enforce `physical-accuracy`. `pop-panel` also enforces
`extreme-physical-beats` because a motion source frame must be a mechanically
valid animation state, not merely a persuasive action image. `illy_plan`
returns both `creativePrompt` and the effective contracted `prompt`, plus the
resolved `contracts` list. The effective prompt is what is hashed and sent to
the provider.

Interactive renders made with Codex's built-in image tool must receive the same
contract text before generation. Pass the applied contract identifiers to
`illy_record`; provenance must not claim a contract that was not in the prompt.

## Extreme handoff to motion

An action, transformation, POV, or moving-platform panel hands off to
`pop/lib/motion-pipeline.mjs` with a structured extreme contract:

```js
{
  motion: "the bunny rolls past and flowers the mounted camera",
  physical: "extreme",
  beats: [
    { at: 0, action: "both feet planted; board coasts" },
    { at: 0.25, action: "trigger compresses; stance loads" },
    { at: 0.5, action: "pulse reaches the still-mounted camera" },
    { at: 0.75, action: "camera flowers; board continues forward" },
    { at: 0.95, action: "petals trail behind; new state holds" }
  ],
  contacts: ["feet → deck", "four wheels → ground", "camera bracket → wall"],
  invariants: ["same bunny", "same board", "no reset after flowering"]
}
```

`at` is normalized generation time from `0` to `1`. Beats must be strictly
ordered and start at `0`. An extreme shot without at least two beats and one
explicit contact fails before a paid request. The motion manifest records the
resolved beats, contacts, invariants, and the full contracted prompt.

Provider models live in `config/providers.json`. Each declares supported `modes`. fal models also declare request-field mappings and an output URL path because fal endpoints are heterogeneous.

Credential resolution is environment first, then `aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`, matching existing `/marketing` and `/pop` scripts. Never return secret values from an MCP tool.

Every successful render writes `<output>.illy.json` with the prompt hash,
provider, model, refs, pipeline, contracts, timings, and provider request
identifier when available. Replaced images and provenance move into a sibling
`archive/` directory.

Add post-processing or publishing as a new stage only when it has a deterministic implementation. Do not overload provider adapters with campaign-specific actions.
