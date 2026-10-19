# THE HARNESS YOU ARE EDITING

Aesel is the harness around a model that writes Aesthetic Computer pieces: one
.mjs file of lifecycle functions (boot, paint, act, sim) that draws on a canvas,
plays sound and answers touch and keys. A person makes a request; the model
writes the piece; the piece is played and what breaks comes back to the model
before it finishes.

The words the model receives live in `context/harness/`, one directory per
surface:

- `aesel/` — what every Aesel session reads: `systemprompt.md` (who the model
  is, what a piece is, how it speaks), `guardrails.md` (what it must not do or
  claim), `tool_guidance.md` (when to reach for which tool, and what its output
  proves).
- `whistlegraph/`, when present — the Whistlegraph editor's own generation,
  critique, review and input prompts. Whistlegraph tasks read these first.

Each `## name` heading is one section. Code decides when a section is sent and
in what order; you decide what it says. Different sessions receive different
sections — a terminal session in a repository, a hosted piece session and the
native app each get their own set — and `context/sent.md` lists the ones this
run received. Only those can change what it does. So:

- Rewrite the text of a section in `context/sent.md`. Edit the file in place.
- Keep every `{{placeholder}}` a section already has, exactly, and add none:
  the code fills those and nothing else.
- Do not add, rename or delete sections — no code would send a new one. A rule
  that needs a home goes into the existing section that is sent when it applies.
- Every section is paid for on every request that sends it. If you add, cut.

A rejected harness comes back to you once with the reason. A second rejection
discards your edit and the round keeps the harness it had.
