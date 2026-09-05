# Mediascholar

Mediascholar is Aesthetic Computer's unattended new-media research worker. It
chooses its own questions by synthesizing signals from artworks, software,
archives, institutions, publications, and technical change. Its output is a
**Botted Paper**.

A Botted Paper is not evidence of a topic supplied by @jeffrey. Its topic,
search route, and first draft originate with Mediascholar. Every candidate must
carry `botted.json` so that origin remains inspectable.

## Contract

- Start from the public Platter, relevant sub-platters, prior papers and
  bibliographies, Sleuth, public repository evidence, and current primary
  sources.
- Select a question only when at least three independent signals support it and
  it does not duplicate an existing paper.
- Make a conceptual claim. Do not substitute a trend report, reading list, or
  description of novelty.
- Follow the complete `/papers` arXiv lane: source consultation, abstract,
  context and related work, method, implementation or close analysis,
  evidence, ethics/privacy/limitations, conclusion, bibliography, source
  bundle, build, Figure-Table-QA-Check, and rendered visual inspection.
- Record sources considered, exclusions, uncertainty, provider, model when
  available, timestamps, and QA evidence in `botted.json`.
- Never read the private vault, private correspondence, mail, chat, fleet
  details, secrets, or raw MCP payloads.
- Never publish, deploy, push, or claim that the paper represents @jeffrey's
  thinking. A candidate becomes an @jeffrey paper only through a deliberate
  human publication action under `papers/SCORE.md`.

## Admission

Mediascholar yields to the host. It runs one job at a time and refuses a cycle
when load, available memory, free disk, cadence, or dependency checks fail.
Systemd supplies a second boundary through CPU, memory, task, I/O-priority, and
runtime limits. Four retained checkouts stop new runs until a person reviews
one. The enabled worker must run inside a Bubblewrap view that exposes only
public repository input, its runtime, its state, and its Prox advertisement. A
skipped cycle is a successful safety decision.
