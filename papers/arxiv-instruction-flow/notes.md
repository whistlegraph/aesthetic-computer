# Consultation and evidence

Prepared 2026-09-15. This is a local analysis, not an implemented instruction migration.

## Corpus

- Working checkout: `275329e763ab6cfb15735434736bd134ef78b70b`.
- Fetched production main: `a97038281a06c1ad3aabc15256e84e803f0ec195`.
- Root instruction documents are identical between those commits. The working copies additionally replace `compush` with `compushloy`, at the user's request.
- `inventory.json` records tracked exact-basename SCORE, AGENTS, CLAUDE and CODEX documents, byte counts and content hashes. Symlink targets count as paths, not independent texts.
- Root SCORE.md: 42,689 bytes / 738 lines. CLAUDE.md: 9,882 bytes / 161 lines. AGENTS.md is a tracked symlink to SCORE.md. No tracked CODEX.md or repository .codex directory was found. There are 22 exact-basename SCORE.md files, including root; one nested CLAUDE.md; no nested AGENTS.md.
- A 32,768-byte prefix of the working score ends at line 504, within the chat-query material. This is a simulated default-budget boundary, not evidence of what this session loaded. The user supplied instructions directly in this conversation. No explicit max-byte or fallback-filename entries were found by an allowlisted search of the local Codex config; launcher overrides and other profiles were not audited.

## Platter consultation

- Read papers/SCORE.md and papers/AESTHETIC-EYE.md.
- Opened https://papers.aesthetic.computer/platter.html in Chrome: HTTP 200; title Research Platter; inspected screenshot. The index displayed 74 papers. This is a displayed count, not a repository census. Browser evidence was temporary and is not included in the source bundle.
- Read papers/arxiv-score-analysis/score-analysis.tex and references.bib. Its six tensions and conductor/parts proposal are precedent, not current empirical counts.
- Consulted the source structure and section inventory of Repository Archaeology and Keymaps as Social Software.
- Consulted papers/lines-platter/manifest.md for the score/making context; excluded private correspondence and uninspected source materials. The image-oriented Jeffrey manifest is not evidence for this paper.
- Verified official Codex discovery documentation (redirecting to https://learn.chatgpt.com/docs/agent-configuration/agents-md) and Claude Code memory documentation (https://code.claude.com/docs/en/memory), accessed 2026-09-15. Their descriptions are not a measurement of this installation's effective instruction context.

## File evidence for findings

1. SCORE.md:446; CLAUDE.md:98: compushloy definition. lith/deploy.fish:21-37: current-branch default and DEPLOY_BRANCH override.
2. CLAUDE.md:28; SCORE.md:660-662; ants/mindset-and-rules.md:7,34-39,53-60: overlapping and inconsistent agent scope.
3. SCORE.md:214-220; CLAUDE.md:81-94; fedac/native/SCORE.md:5-41: native release routes and completion criteria.
4. CLAUDE.md:126; SCORE.md:179; session-server/deploy.fish:2-3,31-49: session-server descriptions and deployment code.
5. CLAUDE.md:34,38,121; SCORE.md:305-306; system/public/aesthetic.computer/disks/CLAUDE.md:3-5: HAND, SCREEN and piece-guide routes.
6. ENVIRONMENT.md:13-23,39-41; SCORE.md:310-337: environment selection and heading scope.
7. .githooks/post-commit:25-35,50-69; .githooks/pre-commit:270-284: branch-specific side effects and source-level push failure fall-through. Locally, no installed pre-commit hook or core.hooksPath was found; the post-commit hook is linked. This is one host observation, not a fleet census.
8. memory/codex-sync.mjs:108-116: checkout-path-specific instruction-envelope filter; stored memory was not examined.
9. easel/bin/sync-context.mjs:30-47,61 onward: explicit context-bundle inventory and check mode as local precedent.

## Incident boundary

In this conversation, v120 was byte-verified after a feature-branch deployment. A later read served v118, including a cache-busting URL, while the server checkout was main. The fixes were cherry-picked onto current main, preserving its loading/camera work, and deployed as v122. Identity, gzip and Brotli responses then matched the intended source. These observations support a branch-publication mismatch; they do not identify the actor that initiated the intervening deployment or prove which instruction caused it.

## Method and limitations

One read-only subagent independently audited repository routing and returned file/line evidence. The primary agent verified loader documentation, selected findings, measured the inventory and default-budget boundary, and authored the synthesis. No controlled agent-adherence trial, model-token benchmark, private memory audit, or cross-platform symlink trial was performed. Proposed byte budgets and migration gates are design targets.

## Revision: empirical literature and authorized prox history

The initial draft excluded private history. The user's follow-up explicitly requested prox-history analysis; the revised paper therefore adds bounded native/indexed history evidence. Its privacy and methods statements supersede the initial draft's history exclusion. See workflow-method.md, the selected aggregates, and the extraction script; no raw transcripts were copied into this directory.

Additional consultation: papers/jeffrey-lexicon/README.md and manifest.json, especially first-hand provenance and separation of generated language. The history heuristics are not sufficient to certify input for that lexicon.

Primary empirical sources checked online on 2026-09-15:
- Gloaguen et al., arXiv:2602.11988v1 and June 2026 v2, sections 3–5, Tables 2–3, and length/category ablations. The revision cites v2 and its significance-aware conclusions; the benchmark was renamed CTXbench.
- Lulla et al., arXiv:2601.20404v1 and v2, sections 3.1.2–3.1.8 and Table 1. The paper cites v2; the reported median results and correctness limitation persist.
- Becker et al., arXiv:2507.09089v1, and METR's February 24, 2026 follow-up at https://metr.org/blog/2026-02-24-uplift-update/ . Separate the earlier randomized finding from the later selection/measurement caveats.
- Chatlatanagulchai et al., arXiv:2511.12884v1, descriptive corpus study.
- Consulted arXiv:2608.20195v1 for extraction pitfalls and study limits, but did not use its headline behavior estimates as evidence for our intervention.

Recent-work counterexamples were independently checked by the audit subagent:
- fetched main easel/src/tui.mjs:166–179 preloads checkout or bundled context; lines805–810 discuss autopublish state reaching the model on a new thread. The comment's latency estimate is not an independent timing experiment.
- slab/bin/prox-mcp.mjs:297–337 records bundle identity and cwd, with home-directory fallback in resume scripts. No claim of a reproduced failed handoff.
- commit3b1496c428 relocates release receipts into Git's common directory for worktrees.
- commit135d05d311 rebuilds compressed sidecars in the webhook path, complementing the CLI route.
- lith/deploy.fish defaults to checkout branch; lith/webhook.sh defaults to main. Multiple production writers require a shared contract. Concurrent receipt overwrite remains a hypothesis, not an observed race.

These revisions remain analysis. No loader migration, hook installation, ongoing telemetry, public publication, or deployment was performed for this paper.
