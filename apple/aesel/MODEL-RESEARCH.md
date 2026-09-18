# aesel mobile models

Researched 2026-09-17. User selected Luna as the default and Opus as an explicit premium option. The wider comparison below remains a proposal.

## Current service and visibility

`easel/src/ac-server.mjs` now exports `DEFAULT_AC_MODEL = "openai/gpt-5.6-luna"`.
The earlier GLM-4.6 default remains available explicitly. Opus uses
`anthropic/claude-opus-5`; both new IDs were confirmed in OpenRouter’s live model catalog.
New phone threads use Luna; existing threads retain their saved model.
The phone constructs that engine; AC's `/api/easel-inference` authenticates the
AC account, enforces its model allowlist/budget, and relays OpenRouter's Messages
stream. It does not execute the installed Claude or Codex phone app.

Show the requested model before inference, then the upstream-reported model when
`message_start.message.model` arrives. Record that distinction per thread; older
threads cannot retroactively prove which model served them. An upstream model ID
is provider-reported metadata, not independent proof of model weights. A later
server change should include model, provider, request ID, fallback reason and
usage in a compact response receipt, without saving private prompts in analytics.

## Using the user's installed apps

| Route | Documented capability | aesel implication |
| --- | --- | --- |
| Claude iOS App Intent | Ask Claude can process text and return responses through Shortcuts; it uses the app's selected model and the user's Claude usage allowance. | A viable optional user-installed Shortcut: aesel exports a bounded piece request, Ask Claude produces source, aesel imports and previews it. Prototype cancellation, callback and result size before promising parity. |
| Claude Code mobile link | `claude://code/new?q=...` opens a prefilled composer; repo/branch parameters are supported. | Useful explicit handoff to Claude Code. A link alone supplies no streamed edits or automatic result callback into aesel. |
| Codex/ChatGPT mobile | Remote controls work running on a paired desktop host. | Useful companion workflow, but no documented general iOS app-to-app inference endpoint was found. |
| Codex app server | Public protocol supports rich clients, streamed events, history, approvals, and ChatGPT-managed or API-key auth. | A paired aesel desktop/host could run Codex and stream work to the phone. Keep user auth on that host. The installed phone app alone is insufficient. |
| AC-hosted API | AC pays the API provider and applies account quotas. | Best default for a self-contained aesel experience with no extra installed app or computer. |

Sources: [Claude Shortcuts](https://support.claude.com/en/articles/10263469-use-claude-app-intents-shortcuts-and-widgets-on-ios),
[Claude mobile links](https://support.claude.com/en/articles/14898120-open-the-claude-mobile-app-with-a-link),
[Apple Shortcut callbacks](https://support.apple.com/guide/shortcuts/use-x-callback-url-apdcd7f20a6f/ios),
[ChatGPT Remote](https://learn.chatgpt.com/docs/remote-connections),
[Codex app server](https://learn.chatgpt.com/docs/app-server).

Proposed Shortcut return path: register an aesel import App Intent, carry a
one-use request identifier tied to the active thread, accept source as data, and
preview before publication. The app should not extract another app's credentials.
This is a user-run automation, not a replacement for the hosted streaming tool loop.

## Model recommendation

Start quality evaluation with **Claude Sonnet 5 as the normal hosted candidate**,
and **GPT-5.6 Luna as a low-cost candidate**. Include **GPT-5.6 Terra** as a middle
comparison. This is a workload-specific recommendation to test, not a measured
ranking: none has yet run the same aesel evaluation set here.

| Model | Proposed role | Standard input / output per million tokens | Example 10K input + 2K output |
| --- | --- | --- | --- |
| GPT-5.6 Luna | inexpensive first pass, only if rendered-result quality holds | $0.20 / $1.20 | $0.0044 |
| GPT-5.6 Terra | balanced coding candidate | $2 / $12 | $0.044 |
| Claude Sonnet 5 | normal quality candidate, near existing Messages integration | $2 / $10 | $0.040 |

These are direct-provider, uncached, short-context rates; routing fees, reasoning
output, repeated tool calls, retries and longer histories change actual cost.
Sources: [OpenAI pricing](https://developers.openai.com/api/docs/pricing),
[Luna](https://developers.openai.com/api/docs/models/gpt-5.6-luna),
[Terra](https://developers.openai.com/api/docs/models/gpt-5.6-terra),
[Sonnet 5 specifications](https://platform.claude.com/docs/en/models/sonnet-5/overview).

A phone does not require a small hosted model: computation happens remotely.
Optimize time until a working preview and total cost per successful piece.
A cheap failed generation plus several repairs can cost more than one good answer.
Reserve heavyweight reasoning for explicit difficult repairs, rather than making
every small visual adjustment wait for it.

## Evaluation and rollout

Use a fixed set of 20 AC tasks: animated dots, touch drawing, resize/orientation,
color changes, sound with touch unlock, published-piece edits, and repairing bad
API calls. Verify syntax, runtime errors, visible pixels, interactions, persistence,
and source/published parity. Measure first-working-preview latency, success within
two attempts, tokens and dollar cost. Include GLM-4.6 as the current baseline.

Before changing defaults, verify actual provider/account availability and streaming
compatibility, update the server allowlist, reserve quota by model cost instead of
only raw tokens, and display any fallback. OpenAI direct integration requires a
Responses adapter; an OpenRouter model route must be verified rather than guessed.
Do not spend on an automated comparison or change production merely to answer
this research question.

## Selected rollout

Luna is the requested default. Opus 5 is a user-selected premium option, never a
silent fallback. Its provider list price is $5/M input and $25/M output, so the
same 10K-input/2K-output example is $0.10 before extra tool rounds/reasoning.
The iPhone model menu and `/model luna` / `/model opus` preserve conversation
and save the choice per thread. Switching is disabled while work is running.
[Opus specifications](https://platform.claude.com/docs/en/models/opus-5/overview).

The existing energy meter is a rough model-size estimate, not a dollar meter or
a provider measurement. Luna and Opus sizes are explicitly marked unknown/guessed;
do not use that estimate to charge credits. Paid usage needs provider token/cost
receipts and a reservation ledger; see [credits exploration](CREDITS-EXPLORATION.md).

Deployment verification: main commit `95f1c58231` was deployed to lith. Bounded
production `/api/easel-inference` checks for Luna and Opus both returned HTTP 200,
the expected upstream-reported model IDs, and `OK`. These are connectivity/stream
checks, not a comparative evaluation of generated AC pieces.
