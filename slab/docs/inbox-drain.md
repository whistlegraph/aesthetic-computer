# inbox drain — messages into a running agent session, no keystrokes

`slab/bin/inbox-drain.mjs` is the hook-side half of the prox inbox. Other
agents write to `$SLAB_HOME/inbox/<session_id>/messages.jsonl` (see
`prox-inbox.mjs`); the drain runs as a lifecycle hook inside the target
session and hands pending messages to the model at a turn boundary. The
session id is the one the harness passes to its hooks — the same id Slab's
ledger records.

Output is one block:

```
Messages from other sessions (via prox inbox):
[inbox from neo:sip · 2026-09-23 17:58] look at the diff
```

## Claude Code

Wired in `.claude/settings.json` (project layer), two hooks, timeout 5 s:

- `UserPromptSubmit` → `inbox-drain.mjs prompt`. Drains. If anything was
  pending, prints
  `{"hookSpecificOutput":{"hookEventName":"UserPromptSubmit","additionalContext":"…"}}`.
  Claude Code injects that as a system reminder alongside the user's prompt;
  nothing shows in the transcript.
- `Stop` → `inbox-drain.mjs stop`. Peeks; if pending, drains and prints
  `{"decision":"block","reason":"…"}`. Claude Code keeps the turn going and
  shows Claude the reason, so a message that lands mid-turn is read at the
  end of that turn instead of waiting for the next human prompt.

Delivery is therefore at turn boundaries: the next prompt, or the end of the
current turn. A session idle at the prompt gets the message when the human
next types — the Stop hook already fired. Empty inbox: the hook prints nothing
and exits 0. Any internal error (missing `prox-inbox.mjs`, unreadable file)
goes to stderr and still exits 0; the prompt is never lost to a broken hook.

### loop guard

Stop hooks receive `stop_hook_active: true` when the turn is already a
continuation caused by a Stop hook. The drain lets the turn end whenever the
inbox is empty, which covers that case: a continuation only happens because a
message was consumed, so the next Stop sees an empty inbox and stays quiet.
Fresh messages that arrive during a continuation still go through; Claude
Code's own cap ("overrides the hook and ends the turn after 8 consecutive
blocks") is the backstop against a peer that never stops talking.

## Codex CLI

Codex hooks (stable, on by default in 0.156) use the same schema as Claude
Code: same `hooks.json` shape, same `UserPromptSubmit` →
`hookSpecificOutput.additionalContext`, same `Stop` →
`{"decision":"block","reason"}` with a `stop_hook_active` input. One
difference: Codex's Stop "expects JSON on stdout when it exits 0. Plain text
output is invalid", so the Codex wiring passes a `codex` flag and the drain
answers `{}` when the inbox is empty.

`slab/codex/hooks.json` carries both hooks with absolute paths (Codex has no
`$CLAUDE_PROJECT_DIR`). Install, one of:

- user layer: copy or merge it into `~/.codex/hooks.json` (merge if that file
  already exists — Codex loads every source, so a duplicate would double-fire);
- project layer: `<repo>/.codex/hooks.json`, which only loads once the
  project's `.codex/` layer is trusted.

Then open Codex and run `/hooks`: "Before a non-managed hook can run, Codex
requires you to review and trust the exact hook definition. Codex records
trust against the hook's current hash, so new or changed hooks are marked for
review and skipped until trusted." The trust lands in `~/.codex/config.toml`
under `[hooks.state]` as a `trusted_hash`, which is why that file isn't
edited from the repo. Changing the command line (even the path) needs a fresh
trust. `--dangerously-bypass-hook-trust` skips the review for one invocation.

Known Codex quirk: `additionalContext` is currently rendered as a visible
developer message in the transcript (openai/codex#16933), so inbox messages
show up on screen there rather than silently.

## Easel pro

Easel's pro runtime takes messages over its socket; the hook drain isn't
used there. Covered in Easel's own docs.
