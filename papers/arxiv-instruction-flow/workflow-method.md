# Workflow evidence: September 2026 revision

The user requested analysis of prox history, shared terms, and regularly used tools. This is a descriptive sample, not a productivity experiment. The quantitative window is 2026-09-01 inclusive through 2026-09-15 exclusive, UTC; it excludes the current research session. No original prompt text, session IDs, command arguments, credentials, or correspondence are included in the paper or its bundle.

## Native local sample

`audit-transcripts.py` streams existing native records without modifying their stores. It was run locally on Blueberry with Python from the installed Command Line Tools. `workflow-sample.json` contains selected derived aggregates, extraction counts, patterns, and the parser SHA-256. Private inputs are not distributed. Reproduction requires separately authorized access to the same dated records; this is not an independently downloadable public dataset.

Scope: 77 top-level Claude transcripts and 20 Codex transcripts; exact monorepo root cwd only. Explicit cwd changes take precedence; missing cwd inherits preceding explicit session metadata. Nested cwd, worktrees, other projects, and subagent directories are excluded. This deliberately narrow boundary undercounts overall monorepo work. Files grew during collection, but the closed date window stabilized the relevant records.

Native tool calls are extracted from Claude assistant tool_use blocks and Codex response_item function/custom call records. Deduplication uses provider plus tool-use/call ID. Counts establish requests, not execution or success. Forked records with regenerated IDs may remain.

Codex exec wrapper bodies are scanned for syntactic tools.NAME(...) sites. This is a separate estimate: loops, dynamic dispatch, strings, and comments are not interpreted. It cannot be added to native calls as if it measured actual completed operations. The corpus contains 8,641 native call requests: 6,961 Claude and 1,680 Codex. Selected native families: Bash 4,541; exec wrapper 1,617; Chrome browser 867; Puppet 374; Frame 21; Prox 16; Paper 4. Wrapper candidates include Frame 156, Paper 88, Puppet 19, and Prox 13. Session counts are in the aggregate.

Short-prompt extraction accepts text at most 400 characters, excludes marked instructions/notifications, tool-result messages, and obvious forwarded Markdown, and deduplicates UUIDs or timestamp/content identities. Claude also stores human steering as queued_command attachments. We include the structured prompt only for commandMode=prompt and origin.kind=human; deduplicate source_uuid; ignore queue enqueue/remove copies and rendered attachment text. An initial parser omitted these and was corrected after checking a known workflow-definition event. The initial counts must not be reused.

The final sample contains 1,216 accepted short prompts (944 Claude, 272 Codex), with 439 accepted queued prompts added by that correction. There are 83 sessions with accepted prompts or calls. Lexical matches: compush and selected inflections 28 prompts/19 sessions; prox/proxes 19/13; Chrome 16/13; Easel 10/6; oskieploy 1/1. Quoted or copied short text can survive the heuristic; these are likely-user prompts, not a certified first-hand language corpus. Do not feed them into the public Jeffrey Lexicon as verified authorship.

A zero under a narrow pattern is not evidence of nonuse. The explicit frame-screenshot regex misses formulations such as a quoted “frame” followed by “shot”; ordinary frame also means simulation frame. The paper uses qualitative task context for that distinction, not the zero count.

## Remote index sample

The fleet memory connector recovered the local store but failed on several remote entries. Neo failed on malformed SSH option construction. A direct, read-only SSH invocation of its existing memory search recovered that host. No authentication bypass or configuration mutation was used. Other unreachable histories remain absent.

The remote sample uses the encrypted memory index's search interface, not the native parser. Query: role=user, since 2026-09-01; a regex requires 1–400 characters, excludes named instruction/notification markers, and matches a selected bounded term. Results are additionally filtered before 2026-09-15 and to project labels core or aesthetic-computer. Each pattern is searched independently; output retains counts and distinct-session counts only. `remote-index-sample.json` records these derived values. Compush: 36 events/25 sessions; prox: 29/21; Chrome: 18/12. They must not be pooled with the native sample because provider imports, duplication, authorship filtering, and cwd coverage differ.

## Interpretation

Shared-term occurrences nominate workflow definitions; frequency does not establish authority or desirability. Explicit user definitions outrank inferred synonym clusters. Tool requests demonstrate an active surface, but not successful use, necessity, or efficiency. The data support examining visual inspection, asynchronous steering, and handoffs as part of the workflow; only a controlled intervention could establish whether the proposed changes improve it.
