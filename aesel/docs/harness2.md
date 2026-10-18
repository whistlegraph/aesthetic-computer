# Harness² for Aesel and its embeddings

Plan, October 18, 2026. Google Research's
[Harness²](https://github.com/google-research/harness2)
([paper](https://arxiv.org/abs/2610.14224)) improves an agent without touching
model weights. It edits the harness: system prompt, guardrails, memory, tool
guidance, skills, scripts, subagents and plugins. For each task it runs the
base harness, proposes edits along two axes (how the agent works; what a
practitioner knows), re-runs the same task under each edit, and composes the
edits whose transcripts show better behaviour. No answer key is used, and
keeping the base harness is a valid outcome. After each task it stores the
adopted harness and at most one transferable lesson, citing per-edit verdicts
(`helped`, `hurt`, `no observed effect`, `confounded`), in a library that
later tasks of the same kind read first.

The goal here is better generation loops in every Aesel embedding: the
terminal, phone, web, Windows, iOS/Mac, native, the Whistlegraph editor and
MIME. Refinement runs offline. Its output is a harness diff a person reviews;
nothing it proposes ships automatically.

## 1. Prompt text as editable files — shipped, 0.8.27

The system prompt, guardrails and tool guidance live in
`harness/{systemprompt,guardrails,tool_guidance}.md`, one `## name` section per
paragraph, with `{{placeholders}}` for session values. `npm run harness`
compiles them into `src/harness-text.mjs`; `npm test` fails when it is stale.
Prompts rendered byte-identically across 1,536 session states before and after
the move.

Edits reach the TUI, phone, web, Windows, iOS/Mac and native at once.
Whistlegraph and MIME read a `bundle.mjs` snapshot of `src/`, so they pick up
edits on their next bundle.

## 2. Move the forked prompt text

These still hold their own copies and must move into `harness/` before a loop
can edit them:

- `src/ac-server.mjs` — layered-edit rules, runtime-log trust, the interface
  paragraph.
- `native/prompt.mjs` — the embedded-preview, native-capture and publication
  paragraphs.
- The Whistlegraph engine — generation, critique, wares, visual review,
  drawing input, musical input and painting prompts. Because it passes its own
  `developerInstructions`, `AcServer` drops the shared visual, responsive,
  clock and sound sections for it. Rules that restate the piece guidance
  become those shared sections; the rest become Whistlegraph sections.

## 3. A headless executor

`aesel run --task <file> --harness <dir> --out <rundir>`: one task under one
harness, writing the transcript (`events.jsonl`, rendered as an
action/observation trace) and whether it delivered — `ac check` prints ✓ — plus
preview frames. The engine classes are already scriptable;
`bin/bench-providers.mjs` drives them.

Whistlegraph already has both halves of its own loop: the
`__whistlegraphSequence` benchmark runs tasks headlessly, and its visual review
judges four timed frames. That makes it the first embedding to refine.

## 4. Tasks by kind

About twenty prompts to start, grouped the way the prompt is: visual, sound,
KidLisp, multiplayer, responsive, and Whistlegraph drawings. The group is the
library's domain — lessons transfer within it, not across it.

## 5. The refinement loop

Write a Harness² benchmark adapter (Python) that drives Aesel: the editable
surface is `harness/`, `execute` calls `aesel run`, `delivered` reads the
`ac check` result, and the trajectory is the rendered transcript. The editor
and composer models run through the Claude CLI channel Harness² already
supports. Runs are paid, so they go on the oven or a mini on a schedule, not
on every commit.

## 6. A lesson library as Aesel's memory

Aesel has no memory across sessions. Adopt the library format:
`harness/library/<kind>/INDEX.md` with entries of cue, lesson, evidence and
confidence. Only the index is always on; a lesson or skill loads when its cue
matches. Keep Harness²'s discipline: a lesson resting only on confounded
verdicts is a guess, workspace facts are labelled apart from procedure, and
always-on text has a size cap.
