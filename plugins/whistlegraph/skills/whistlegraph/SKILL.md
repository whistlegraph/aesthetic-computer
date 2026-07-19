---
name: whistlegraph
description: Curate whistlegraph.org and make authenticated, narrowly scoped frontend changes through the Whistlegraph Desk Git workflow.
---

# Whistlegraph Desk

Use the `whistlegraph_*` MCP tools whenever the user asks to inspect, edit, commit,
publish, deploy, or roll forward the Whistlegraph frontend. Do not substitute raw
shell Git commands for the publishing tools.

The user must run Codex from an `aesthetic-computer` checkout whose `origin` can
push to the Tangled knot. Authentication comes from the shared Aesthetic Computer
session in `~/.ac-token`; if it is absent or expired, tell the user to run
`ac-login` and retry.

## Frontend change workflow

1. Call `whistlegraph_whoami` and confirm the server grants maintainer access.
2. Call `whistlegraph_begin_change` with a short summary. This creates an isolated
   worktree from the current `origin/main`; it never uses or alters the user's
   working tree.
3. Inspect with `whistlegraph_list_frontend`, `whistlegraph_read_frontend`, and
   `whistlegraph_diff_change`.
4. Edit only through `whistlegraph_replace_frontend` or
   `whistlegraph_write_frontend`. The server is intentionally limited to
   `system/public/whistlegraph.org/**`.
5. Call `whistlegraph_validate_change`. Resolve every reported error before
   publishing.
6. Show the user the final diff summary and ask for explicit confirmation before
   calling `whistlegraph_publish_change` unless the user already explicitly asked
   to publish.
7. Publishing creates a normal attributed commit, pushes its review branch,
   fast-forwards `main` only if the original base is still current, and asks the
   Auth0-protected Desk endpoint to deploy that exact SHA.

If `main` moved, begin a fresh change and reapply the desired edit. Never force
push, reset shared history, bypass validation, edit outside the allowlist, or ask
for deployment secrets.

Archive metadata edits remain available in the web Desk at
https://whistlegraph.org/desk. This first MCP release focuses on frontend files and
the Git/deployment path.
