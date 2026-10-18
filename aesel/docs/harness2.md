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

## 2. Move the forked prompt text — shipped

`src/ac-server.mjs` (layered edits, runtime-log trust, the interface
paragraph, the current-piece header) and `native/prompt.mjs` (embedded
preview, native capture, publication, prior conversation) place harness
sections. The hosted bridge's API workflow is a `{{lookup}}` fill rather than a
string `.replace()` that would silently miss once the sentence was edited. Both
render byte-identically across 290 bridge and native states.

Whistlegraph's prompts are private (its source moved to the vault on October
14), so they live in `apple/whistlegraph/harness/` in the vault, not here:
generation, critique, media and wares; the visual reviewer; performance,
drawing, sound and branch-history input; and the painting, Roblox and Doom
wares. `apple/whistlegraph/tools/sync-harness.mjs` compiles them to
`Resources/Web/whistlegraph-harness.mjs` with this repository's parser
(`bin/sync-harness.mjs --source/--out` does the same by hand), and its tests
fail when stale. 44 rendered strings are byte-identical.

Not yet merged: Whistlegraph still passes its own `developerInstructions`, so
`AcServer` drops the shared visual, responsive, clock and sound sections for
it, and some of its rules restate the piece guidance. Folding those into the
shared sections changes what the model reads, so it waits for the executor
(step 3) to measure it.

## 3. A headless executor — shipped for Whistlegraph

```
ac run --init-harness <dir>        # copy the editable prompt files to start from
ac run --task <task.json> --out <rundir> --live [--harness <dir>] [--model <id>]
```

One task under one harness. The harness directory holds a subdirectory per
surface — `aesel/` and, with the vault linked, `whistlegraph/` — in the same
markdown as the source; a surface or section it leaves out runs as the
repository has it. `src/run-task.mjs` swaps the edited sections in (the
compiled modules' `useHarness`) before anything that places them loads, then
hands the task to its kind's runner.

The run directory keeps `task.json`; `harness/`, what actually ran, with
`result.json` naming the changed, added and inherited sections;
`events.jsonl`, every engine notification, step, paint and verdict;
`frames/`, each paint's four PNGs; `source/turn-N.mjs`; `trace.md`, the
transcript rendered as request, rounds, actions and observations; and
`result.json` — per turn delivered or not, the review's acceptance, repairs,
notes and tokens. Exit status 0 means every turn delivered.

The Whistlegraph runner (`lith/whistlegraph-run.mjs`, private) is the
worker's own `runTurn` — edit contract, layered edits, render pool, one
repair, picture review — on an in-memory ledger, paid by the person's AC
token. A task is a list of requests, each a turn on the version the last one
saved; the first turn that saves nothing ends it. Example:
`apple/whistlegraph/tasks/pink-ball.json`.

The first run (DeepSeek V4.1 Flash, two turns, 72 s) delivered both turns
with review notes, and its trace already shows two things worth a harness
edit or a fix: `wipe("#0b0b12")` painted a different random background in
every frame although the generation prompt allows six-digit hex, and a repair
that ran out of rounds is noted as "did not paint".

Still to build for other embeddings: a plain piece runner (AcServer plus
`ac check`) for the terminal, phone and web surfaces.

## 4. Tasks by kind — Whistlegraph's set shipped

21 tasks in the vault's `apple/whistlegraph/tasks/<domain>/`, worded the way
people talk to the phone: visual, motion, sound, critique, revision,
responsive and drawing (chalk loops, sweeps and chalk alone, as
`whistlegraph-drawing/v1`). The folder is the library's domain — lessons
transfer within it, not across it. Its README says what each domain probes,
as context for a trace's reader, not an answer key. A test checks every task
parses, every drawing normalizes and every starting piece passes the source
checks.

The first drawing run already shows a lesson to find: chalk looped the right
circle with "make this one bigger", the model enlarged both, and the review
passed it — neither the generation prompt nor the reviewer resolved "this" to
the chalked target.

KidLisp and multiplayer tasks wait for the plain piece runner.

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
