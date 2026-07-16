# Illy pipeline contract

Pipelines are named JSON objects in `config/pipelines.json`. They describe conventions rather than provider request bodies.

For production-specific customization, pass `pipelineFile` to `illy_plan` or `illy_generate`. The JSON may contain one `{ "pipeline": { ... } }` or a named `{ "pipelines": { "name": { ... } } }` map. Keep durable shared presets in the plugin config; keep one-off track/campaign presets beside that production.

- `providerOrder`: adapter order used by `provider: "auto"`.
- `modelByProvider`: default model/endpoint for each adapter.
- `outputTemplate`: path relative to `targetDir`; supports `{slug}` and `{variant}`.
- `defaults`: values such as `size`, `quality`, and `variant`.
- `stages`: ordered provenance labels. Adapters own network execution.

Provider models live in `config/providers.json`. Each declares supported `modes`. fal models also declare request-field mappings and an output URL path because fal endpoints are heterogeneous.

Credential resolution is environment first, then `aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`, matching existing `/marketing` and `/pop` scripts. Never return secret values from an MCP tool.

Every successful render writes `<output>.illy.json` with the prompt hash, provider, model, refs, pipeline, timings, and provider request identifier when available. Replaced images and provenance move into a sibling `archive/` directory.

Add post-processing or publishing as a new stage only when it has a deterministic implementation. Do not overload provider adapters with campaign-specific actions.
