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

## 3. A headless executor — shipped for pieces and Whistlegraph

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

The piece runner (`src/piece-run.mjs`, kind `piece`) is the hosted loop the
terminal, phone and web run: AcServer on AC inference, with `ac check` in place
of a person's preview. Every save is played in a browser with a tap and a
space; what that finds goes back through ac_preview before the write's tool
result, so the model's next round already knows. After each turn the final
source is checked fresh, and the turn delivered when it changed the piece and
that check found no errors. Each check's frame is in `frames/`. There is no
picture reviewer, so `reviewed` is null. First run (two turns of
`tasks/revision/pulse.json`, default model, 31 s): both delivered, the circle
grew and pulsed on the shared clock.

## 4. Tasks by kind — shipped for Whistlegraph and pieces

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

Pieces have 21 in `aesel/tasks/<domain>/`: visual, motion, interaction,
sound, responsive, typography, and revision (two turns, the second changing
what the first made). KidLisp and multiplayer wait for the runner to open
`.lisp` files and a second player.

## 5. The refinement loop — `npm run refine`

Harness²'s proposer prompts and tree validation are bound to its OpenCode and
Codex layouts, so the method is carried over rather than adapted:
`bin/refine.mjs` runs base → propose (inv, dom) → probe → compose → blind
select → reflect for each task, with `ac run` as the executor and the `claude`
CLI as editor, composer, selector and reflector, each reading evidence served
as files: the request, every run's trace with its frames, the deliverables,
the domain's library. The method prompts are in `refine/prompts/`, adapted
from Harness² (Apache-2.0). An edited harness must keep every section and its
placeholders; a rejected one gets the reason and one more try, then the stage
keeps the base. The selector sees pieces only. Each task leaves `adopted/` —
the changed sections, itself a `--harness` — and `changes.md`; nothing edits a
surface's source.

    npm run refine -- --tasks tasks/interaction                  # list; spends nothing
    npm run refine -- --live --tasks tasks/interaction [--from <harness>] [--model <claude model>]

First automated round, October 18 (`tasks/interaction/jelly.json`, Sonnet 5
improving, default AC model, $1.40 of Claude plus four AC runs): the base run
threw ReferenceErrors for `circle` in paint and `screen` in sim — helpers
used where they were not destructured — and spent a round fixing them. The dom
editor's `api-workflow` sentence (each lifecycle function destructures its own
copy of the API) gave a clean first save in four rounds instead of six; the
composer kept it and the reflector marked it `helped`. The selector still
chose the base piece, which wobbled on a plain tap and breathed at rest, so
nothing was adopted and the edit lives on as a medium-confidence lesson. The
inv editor had put the same rule in `pro-piece-workflow`, which a hosted run
never sends, so its probe could not test it; runs now record the sections
their prompt carried (`result.json` → `harness.<surface>.sent`) and editors
are shown that list.

First pass by hand, October 18: chalk targets. One edit to the drawing
guidance and the reviewer, two chalk tasks, pixel-scored re-renders of each
run's saved source (36 runs, $0.72). Sonnet 5.5 resolved the target 4/4 with
or without it; Flash went 1/8 → 3/8, and its reviewer caught 4/5 failures
instead of 2/7. Two things the loop needs that a reviewer verdict cannot give:
a scorer per task where one is possible (the reviewer passed wrong results at
baseline), and several repetitions per arm, because Flash is noisy enough
that two runs a side misread the first edit.

## 6. A lesson library as Aesel's memory

`refine` writes `<library>/<domain>/` (default `refine/library/`): an
`INDEX.md` of cues and, per task, an `EXPERIENCE.md` with a verdict for each
edit (`helped`, `hurt`, `no observed effect`, `confounded`) and at most one
lesson with its evidence, plus the chosen harness. Later rounds of the same
domain read it first. It is committed after review like any other change.

Not yet done: carrying lessons into a live session — the index always on, a
lesson loading when its cue matches, always-on text under a size cap — and the
two things the hand-run pass above asked for, a scorer per task where one is
possible and several runs per arm.
