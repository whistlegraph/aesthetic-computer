---
name: posthog-on-demand
description: Load one relevant PostHog workflow from the disabled PostHog plugin only when needed. Use for PostHog analytics, experiments, feature flags, surveys, replay, error tracking, logs, data warehouse, endpoints, signals, LLM observability, or PostHog instrumentation tasks.
---

# PostHog on demand

Keep `posthog@posthog` disabled. Its 131 bundled skill descriptions and lifecycle
hook must not load globally.

1. Search narrowly with `posthog_skill_list`; use two or three terms from the
   task rather than listing the whole catalog.
2. Select the closest workflow and call `posthog_skill_get` with its exact name.
3. Treat the returned `SKILL.md` as task instructions and follow it.
4. When those instructions reference a bundled file, fetch only that file with
   `posthog_skill_file_get`.
5. Use `mcp__posthog__exec` for PostHog data and actions. Discover unknown tools
   with `search`, inspect a schema once with `info`, then use `call`.

If no specialized workflow matches, use `mcp__posthog__exec` directly.

The configured PostHog connection is only for the Aesthetic Computer
organization. Never use it for Fuser work or export private prompt, session,
mail, contact, fleet, file, secret, or raw MCP data.
