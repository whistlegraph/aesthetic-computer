# Jev at the Aesel harness boundary

Live probe, September 17, 2026: Jev can select a bounded next step in a few
hundred milliseconds. The hosted harness now supports opt-in error triage
between tool rounds, using the same Decisions client as the benchmark.

| Route | Successful calls | Median | p95 | First call | Reported cost |
| --- | ---: | ---: | ---: | ---: | ---: |
| OpenRouter | 30/30 | 219 ms | 588 ms | 1,147 ms | $0.000674982 |
| Vercel Gateway | 4/30 | 368 ms | 1,011 ms | 1,011 ms | $0.000091182 |

Vercel returned HTTP 429 for its remaining 26 calls; its four successful
samples do not establish a fair route comparison. The repeatable runner now
stops that provider after a rate-limit, credit, or credential error.

Calls were sequential from the development host, alternating provider order
each repetition, using ten synthetic scenarios repeated three times per route.
Elapsed time includes transport, gateway, inference, JSON parsing and validation.
The first call is not a controlled cold-cache measurement. OpenRouter resolved
the alias to `typesafe/jev-1.13-20260917`; Vercel reported `typesafe-ai/jev`.
Both requests selected zero data retention.

OpenRouter matched 27/30 authored expected labels: 9/9 Oskiewar practice
decisions and 18/21 Aesel next-step decisions. All three disagreements were the
same intentional-black-screen case: Jev asked for another preview, with
0.78–0.82 probability, despite evidence that the requested artwork was already
black. This is a small fixture check, not a general accuracy measurement. The
untrusted-console-instruction fixture passed all three repetitions, which is
also insufficient to establish prompt-injection resistance.

## Integration

`src/ac-server.mjs` asks `src/jev-advisor.mjs` after tool results and before
another model round. It sends fixed error categories, tool names and counts;
source, prompts, raw logs, filenames, handles and revision IDs stay local.
This integration applies to `--backend ac`; it does not modify the internal
Claude or Codex CLI loops.

Set `EASEL_JEV=1` and `OPENROUTER_API_KEY`, or save `{"enabled":true}` in
`~/.config/easel/jev.json` with the key in the private
`~/.config/aesthetic-computer/jev.env`. `EASEL_JEV=0` overrides the setting.
The key pays OpenRouter directly; Jev calls do not consume the AC handle's
hosted allowance. Provider token usage is emitted through the existing usage
channel. The local Blueberry configuration was enabled during implementation.

There are at most two calls per turn, each with a 1.2-second deadline. Syntax
and write failures get local repair guidance. A blank frame alone triggers
nothing. Other errors can receive a fixed cue when the selected probability
is at least 0.8; this threshold is experimental, not an accuracy guarantee.
Cues apply only to the next round and only if the source revision still
matches. Interrupts cancel triage; errors/timeouts keep the existing flow.
The 12-round bound and selected coding model remain unchanged.

| Evidence | Candidate next step | Possible saving |
| --- | --- | --- |
| Guessed API name failed | Retrieve the relevant `ac_api` reference | Avoid another guessed repair |
| Visual result is uncertain | Collect current `ac_preview` / `ac_frame` evidence | Avoid coding without observing the result |
| Concrete source defect | Give a focused repair task to the coding model | Narrow the next model request |
| Repeated unsuccessful repairs | Ask the selected model to reconsider its diagnosis | Avoid repeating the same unsuccessful approach |

Deterministic checks run first: stale revisions need fresh evidence; known
syntax errors need repair; exact duplicate feedback can be coalesced locally.
Reserve Jev for semantic ambiguity among several plausible next steps. Calling
it before every tool would add latency. A timeout or ambiguous recommendation
continues the existing flow. Initial timeout and confidence thresholds need
measurement, not assumptions about calibrated probabilities.

The integration must be explicitly enabled. It recommends the next diagnostic
step without directly executing a tool or changing models, and discards
recommendations when the revision changes.
Do not send whole sessions, source files, raw logs, or screenshots by default.
Jev accepts text; image judgments would need local measurements or a vision
model. No recommendation should grant approval, publish, declare success, or
create extra unbounded repair loops.

## Captutor / Frame / Puppet

`slab/lib/jev-computer-use.mjs` selects among caller-supplied observed controls.
It sends the bounded goal, roles, and labels; coordinates, selectors, page IDs,
full DOM trees, screenshots and frame IDs stay local. The returned target is
bound to the observation and is only a recommendation: it performs no input.
Frames older than two seconds, low-confidence choices (below 0.9), failed
requests, and unknown prior input outcomes require another observation.
Puppet's existing exact-target/actionability checks still apply before input.

The Aesel computer-use adapter exposes this as `decide(input, options)` for a
trusted host. Captutor provides a CLI for one exact browser target:

```sh
node --env-file="$HOME/.config/aesthetic-computer/jev.env" \
  captutor/bin/jev-frame.mjs --cdp http://127.0.0.1:9222 \
  --target PAGE_ID --goal 'Start the match'
```

This is semantic selection, not pixel understanding or a new vision model.
Captutor's existing screenshot inference remains available for visual QA.
A live local-page test selected Start match in 660 ms, using 495 input tokens
and 51 output tokens, at a reported $0.00002079. It did not click the button.

Measure completed-task wall time, unnecessary tool calls, repair rounds, wrong
interventions, and total cost against the existing harness. A 219 ms decision
is worthwhile only if it avoids more downstream time than it adds. This probe
has not demonstrated an end-to-end Aesel speedup.

## Reproduce

See [`toolchain/jev/README.md`](../../toolchain/jev/README.md) for credentials
and the bounded benchmark command. The input fixtures are in
[`benchmark-fixtures.mjs`](../../toolchain/jev/benchmark-fixtures.mjs), and the
original results are in
[`2026-09-17.json`](../../toolchain/jev/benchmarks/2026-09-17.json).

OpenRouter's [Decisions SDK source](https://github.com/OpenRouterTeam/go-sdk/blob/main/decisions.go)
specifies `POST /api/alpha/decisions`; its chat endpoint is not the Jev interface.
TypeSafe documents the model's [typed decision primitives and limitations](https://docs.typesafe.ai/concepts/system-one).
