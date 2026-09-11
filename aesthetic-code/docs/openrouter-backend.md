# OpenRouter backend

Written 2026-09-11. Prices quoted are from `https://openrouter.ai/api/v1/models`,
pulled that morning. Nothing here is implemented.

The goal is to stop requiring every person who opens Aesthetic Code to already
hold a Claude or Codex subscription. Today both bridges sign in with a vendor
CLI's own credentials, which is elegant — no key, no server, no account — and
also a wall: the interface is unusable to anyone who has not already paid
somebody else. Routing inference through OpenRouter, with Aesthetic Computer
holding the commercial relationship, turns an `@handle` into the thing that buys
the tokens.

That is a bigger change than a third entry in `backends.mjs`. It is the first
time Aesthetic Computer would carry a per-user variable cost, and the first time
`docs/local-contract.md` would have to say that a key exists.

## What already happens, which nobody decided

Aesthetic Computer already hosts inference and already pays for it.
`system/netlify/functions/ask.js` is a 616-line streaming proxy in front of
`api.anthropic.com` and `api.openai.com` using `ANTHROPIC_API_KEY` and
`OPENAI_API_KEY` from the environment. It is reachable in production at
`/api/ask`, it takes no token, and its only gate is an `Origin` header checked
against a three-entry allowlist:

```sh
curl -X POST https://aesthetic.computer/api/ask \
  -H 'origin: https://aesthetic.computer' \
  -d '{"messages":[{"by":"user","text":"say hi"}],"hint":"character:gpt-4o-mini"}'
# 200
```

Without the header it answers 403; with it, anyone on the internet gets a
4,096-token completion on Aesthetic Computer's account. The function counts
tokens — both the Anthropic and the OpenAI paths parse `usage` — and then
`console.log`s them and forgets. There is no per-user record, no cap, and no
rate limit; `system/netlify/functions/prompt-log.mjs` has an in-memory bucket
and `news-api.mjs` has a Mongo day-counter, but neither is anywhere near this
endpoint.

So the question is not whether Aesthetic Computer will pay for strangers'
inference. It does. The question is whether it does so on purpose, with a
ledger. That is the real argument for building this properly, and it is also the
reason the entitlement work is worth more than the backend work: fixing
`/api/ask` and building an OpenRouter tier are the same piece of plumbing.

## Where the key lives

Three arrangements, and they differ mostly in who holds the secret and who sees
the prompt.

**The user brings a key.** They make their own OpenRouter account, and Aesthetic
Code stores the key beside `~/.ac-token`. Aesthetic Computer bears no cost, sees
no prompt, and takes no risk; a runaway agent spends the user's own money. This
is a few hours of work and it is strictly better than the status quo for anyone
willing to sign up. What it does not do is anything @jeffrey asked for: there is
no billing surface, no free tier, and the sign-up wall has moved rather than
gone.

Worth noting, because it changes how bad this option feels: OpenRouter has an
OAuth PKCE flow at `https://openrouter.ai/auth` that exchanges a code at
`POST /api/v1/auth/keys` for a user-scoped key billed to that user's credits,
and it documents a headless mode with no `callback_url` where the user copies
the code across. Aesthetic Code already runs an Authorization-Code + PKCE flow
with a loopback callback in `src/ac-session.mjs`; a second one against a
different issuer is the same ninety lines. So "bring your own key" can be
`/login`-shaped rather than paste-shaped.

**Aesthetic Computer proxies.** A new function beside `run.js` takes the `@handle`
token, checks entitlement, forwards to OpenRouter on Aesthetic Computer's key,
and meters the response. This is the most control: exact per-request accounting,
a hard cutoff mid-stream, model allowlists, the ability to change providers
without touching a client. It is also the arrangement where every prompt a user
types, and every file the agent reads into context, passes through and could be
logged by Aesthetic Computer.

That last part is the problem. `docs/local-contract.md` currently promises "no
runtime account, telemetry, analytics, cloud sync, or hosted control plane" and
that credentials "remain on machines controlled by the user." A proxy is a
hosted control plane through which the entire working session flows. It can be
done honestly — state it, log nothing, say so in the contract — but it inverts
the document, and the live-piece boundary section already shows how much care it
takes to write down one small thing that leaves the machine. This would be the
largest thing.

**Aesthetic Computer mints a key per handle.** OpenRouter's provisioning API is
real and does what it would need to do. `POST /api/v1/keys`, authenticated with a
management key (not an inference key — those are explicitly refused with 403),
takes `name`, `limit` (a spending limit in USD), `limit_reset` of
`daily`/`weekly`/`monthly`/`null`, `expires_at`, `include_byok_in_limit`, and an
`external` object carrying a partner-side `user` identifier. It returns the
plaintext `key` once, and thereafter `GET /api/v1/keys/{hash}` reports `usage`,
`usage_daily`, `usage_weekly`, `usage_monthly`, `limit` and `limit_remaining`.
`PATCH` can `disable` a key. I probed the routes unauthenticated: `/api/v1/keys`,
`/api/v1/key` and `/api/v1/credits` all answer 401 while a nonsense sibling path
answers 404, so they exist.

That is the shape the whole design wants. Aesthetic Computer holds one
commercial account and pays one bill. A signed-in handle asks an authenticated
Aesthetic Computer endpoint for its key; the endpoint mints one capped at, say,
two dollars a month with `limit_reset: "monthly"`, records the hash against the
handle, and hands the key back. The terminal then talks to OpenRouter directly.
Aesthetic Computer is the billing surface without becoming the prompt surface —
the conversation goes from the user's machine to OpenRouter and nowhere else,
which is the only version of this that leaves the local contract mostly intact.
The cap is enforced by the party holding the money rather than by our own
accounting being correct, which is the right place for it.

**Recommendation: the third.** Build the key-minting endpoint and ship the
"bring your own key" path alongside it in the same session — they are the same
code with a different source for the key string, and the BYOK path is the
fallback for every failure mode of the minted one (we run out of credit, the
provisioning endpoint rate-limits us, someone wants a model we do not fund). The
proxy is worth keeping in mind as the eventual home for `/api/ask`, where the
prompts are one-line character completions rather than whole working sessions
and the privacy cost is small; it is the wrong container for a coding session.

The trust boundary in the recommended option is worth stating flatly: Aesthetic
Computer never sees the prompts, but it does mint a bearer token that spends its
money, and a user who extracts that token from their own machine — trivially,
it is a file they own — can spend the cap however they like, including on
something that is not Aesthetic Code. The cap is therefore the entire control.
It should be small, it should reset, and it should be revocable per handle.

## Whether this needs a second kind of engine

It does not, and that is the surprising part.

OpenRouter exposes an Anthropic Messages-compatible endpoint at
`https://openrouter.ai/api` — OpenRouter's own documentation calls it the
"Anthropic Skin" and publishes a Claude Code recipe for it. `POST
/api/v1/messages` answers 401 rather than 404 when probed without credentials,
so the route is there. The documented configuration is three environment
variables:

```sh
ANTHROPIC_BASE_URL="https://openrouter.ai/api"
ANTHROPIC_AUTH_TOKEN="<openrouter key>"
ANTHROPIC_API_KEY=""            # must be explicitly empty
```

with `ANTHROPIC_DEFAULT_OPUS_MODEL`, `…_SONNET_MODEL`, `…_HAIKU_MODEL` and
`CLAUDE_CODE_SUBAGENT_MODEL` selecting what each tier resolves to. Tool use,
streaming, thinking blocks and prompt caching are documented as passing through.

`openEngine()` in `src/tui.mjs` already passes an `environment` object into every
bridge, and `ClaudeServer` already merges it into the spawned process's `env`.
So an OpenRouter backend is an entry in `BACKENDS` whose `Engine` is
`ClaudeServer`, whose `command` is still `claude`, and which contributes three
environment variables and a different default model name. The stream-json
translator, the approval plumbing, the `y`/`a`/`n` contract, the `/model` and
`/backend` restart path, the file-change tracking — none of it changes. The
zero-dependency constraint is untouched, because no new protocol is spoken.

The honest caveat: this removes the requirement to hold a Claude *subscription*.
It does not remove the requirement to have the `claude` binary on `PATH`.
`bin/aesthetic` checks for it at line 189 and refuses to start without it. That
is a much smaller wall — the CLI installs without an account — but it is still a
wall, and it should be said plainly rather than glossed.

The alternative is a native `src/openrouter-server.mjs` that speaks
`POST /api/v1/chat/completions` over plain `fetch`, parses the SSE stream, and
implements its own tools. Plain `fetch` is genuinely sufficient for the wire:
the API is OpenAI-shaped, streaming is SSE, `tools`/`tool_choice`/`tool_calls`
pass through, and `usage` comes back with `cost` and `cost_details` when asked.
A few hundred lines gets text on screen.

The trouble is everything that is not the wire. Writing our own loop means
writing our own `Read`/`Edit`/`Write`/`Bash`/`Grep`, our own workspace
confinement, our own context compaction when a session outgrows the window, and
— the expensive one — our own prompt-cache breakpoint placement. OpenRouter
documents that Anthropic and Gemini models require explicit `cache_control`
breakpoints, at most four of them for Anthropic, with a five-minute default TTL
where writes cost 1.25× input and reads 0.1×. Get that wrong and every turn
re-reads the whole conversation at full input price. The measurements below say
cache reads are 97.5% of all prompt tokens in a real session; at Opus 5's
$5.00-versus-$0.50 spread, misplacing breakpoints is a tenfold cost multiplier.
Claude Code already solved this, and reusing it costs three environment
variables.

So: reuse the bridge, do not write a second kind of engine. Revisit only if a
native local-inference engine is being built anyway, in which case the two share
a loop.

One consequence of the env-var route worth flagging early: `--setting-sources ""`
and `--strict-mcp-config` keep the user's Claude configuration out of the
session, but they do not keep the user's *environment* out. If someone has
`ANTHROPIC_BASE_URL` set in their shell for other reasons, the bridge's env
merge decides who wins. The OpenRouter cookbook's own warning is about exactly
this class of bug — an `ANTHROPIC_AUTH_TOKEN` that expands to an empty string
because the variable it referenced was defined later. Set all three explicitly,
including the empty `ANTHROPIC_API_KEY`, and never inherit any of them.

## Authenticating and metering a handle

`system/netlify/functions/run.js` is the pattern to copy, and it is close to
exactly right already. It rejects a request with no `Authorization` header with
410 rather than 401, because there is no way to retry it as written. It races
`authorize(event.headers)` against a 3-second timeout and answers 503 on
expiry, because Auth0 is slow enough that a hung request is worse than a
reported failure. It resolves the account with `getHandleOrEmail(user.sub)`,
takes the `@`-prefixed form as proof of a handle, and refuses an account without
one. A key-minting endpoint wants all four of those behaviours unchanged.

Two things make this cheaper than it looks. `authorize()` in
`system/backend/authorization.mjs` caches `/userinfo` responses for ten minutes
keyed on `sha256(tenant|authorization)`, so repeated calls from the same session
do not add an Auth0 round trip. And every file in `system/netlify/functions/`
becomes a route automatically — `lith/server.mjs` mounts
`app.all("/api/:fn")` — so `openrouter.mjs` dropped in that directory is
`/api/openrouter` with no configuration, though `system/netlify.toml` still
needs the function's `included_env_vars` and `external_node_modules` entries for
the dev path.

The token side has a template too. `system/backend/piece-hits.mjs` is the only
real per-user counter in the codebase and it is the right shape: a unique
compound index, an upsert with `$inc` on the counter, `$set` on a last-seen
timestamp, `$setOnInsert` on a first-seen one, and a day-string sub-document for
daily buckets. An `llm-usage` collection keyed `{ sub, day }` incrementing
`inputTokens`, `outputTokens` and `microCents` is that file with the nouns
changed.

With minted keys, though, most of the metering is free. OpenRouter already
tracks `usage`, `usage_daily`, `usage_weekly`, `usage_monthly` and
`limit_remaining` per key, so the endpoint can read a handle's spend back on
demand rather than counting tokens itself. Aesthetic Computer's own record needs
only the mapping — handle to key hash to minted-at — plus whatever is wanted for
display. Storing the plaintext key is unnecessary and should be avoided: it is
returned once, and a handle that loses it gets a new one and the old one
disabled.

The `@handles` collection is the identity anchor: documents are `{ _id: <auth0
sub>, handle }`, with Redis caching both directions through
`KeyValue.get("userIDs", sub)` and `KeyValue.get("@handles", handle)`. Note the
sotce/aesthetic sister-sub crosswalk in `findSisterSub` — a sub may arrive
`sotce-` prefixed, and entitlement logic that assumes one tenant will be wrong
for some real accounts.

## Billing, with what is already here

There is more Stripe in this repository than the goal needs, and almost none of
it is pointed at an Aesthetic Computer handle.

The working entitlement system lives on sotce.net.
`system/netlify/functions/sotce-net.mjs` has a `subscribed(user)` function that
reads `KeyValue.get("sotce-subscribed", user.sub)` for a cached
`{ status, current_period_end }`, falls through to Stripe on expiry with
`customers.search({ query: "metadata['sub']:'…'" })`, filters subscriptions by
product, and writes the answer back to Redis. The invalidation half is in
`ticket.js`, which on `customer.subscription.updated` reads
`customer.metadata.sub` and deletes the cache entry. That pair — a Redis-cached
entitlement keyed on the Auth0 sub, invalidated by a webhook — is precisely what
a paid Aesthetic Code tier needs, and it can be ported by changing the product
id.

`system/netlify/functions/news-toll.mjs` is the cleanest single-file reference
for the money half: one endpoint that is both checkout-creator and webhook,
multiplexing on the presence of a `stripe-signature` header, carrying
`metadata.userSub`, and flipping a Mongo document on
`checkout.session.completed`. `give.js` already creates subscription-mode
checkout sessions with inline `price_data`, so a recurring charge needs no
dashboard price object. `give-portal.js` already opens a Stripe Billing Portal
session for cancel-and-manage. `system/backend/stripe-product.mjs` already
normalises Stripe's `price.product` move.

What is missing is the link. On the Aesthetic Computer side, `give.js` passes
only `customer_email`, so Stripe customers are not keyed to subs the way sotce's
are, and `give-portal.js` resolves by email string match. Nothing on an
Aesthetic Computer user or handle carries a tier, a plan, a balance, or a credit
count — those fields do not exist anywhere. So the net-new work is: put
`metadata.sub` on Aesthetic Computer's Stripe customers (or store
`stripeCustomerId` on the handle), add one entitlement field, and port
`subscribed()`.

Two things not to reuse despite the names. `system/netlify/functions/billing.js`
is an admin view of Aesthetic Computer's *outgoing* SaaS costs, not user
billing. `ac-shop/` is Shopify tooling for a store that has been offline since
March 2026.

One piece of prior art does deserve citing: `whistlegraph-llm.mjs` implements
HTTP 402 with x402 USDC settlement on Base, pricing individual resources at
0.10, 1.00 and 5.00 USDC, in roughly 500 lines with no npm dependencies. It is
the only metered-inference-adjacent payment lane in the repository and it proves
the zero-dependency discipline survives contact with payments. It is also
deliberately account-less — the whole point there is a crawler with no signup —
which is the opposite of per-handle billing, so it is a model for the
*discipline*, not the design.

Also worth fixing while in the area: `npm run stripe` at `package.json:122`
resolves to `cd system; npm run stripe-dev`, and no `stripe-dev` script exists.
It is included in the `aesthetic:all` concurrently list, so it fails on every
full-stack start.

## What a session actually costs

Two independent estimates, which agree.

The measured one first. 107 Claude Code transcripts from this repository,
totalling 414 MB under `~/.claude/projects/`, carry real `usage` records. Priced
at OpenRouter's Opus 5 list — $5.00 input, $25.00 output, $0.50 cache read,
$6.25 cache write per million — they come to $2,035 in total, a mean of $19.02
and a median that depends entirely on how long the session ran:

| turns | sessions | median | mean |
|---|---|---|---|
| 21–60 | 3 | $4.18 | $5.35 |
| 61–150 | 16 | $8.68 | $10.00 |
| 151–400 | 19 | $25.24 | $29.53 |
| 401+ | 10 | $82.57 | $125.81 |

The aggregate token mix is the finding that matters: 0.3M input, 58.3M cache
write, 2,715M cache read, 12.5M output. Cache reads are 98% of all prompt tokens
and two thirds of the total bill. A coding session is not a text
generator; it is a machine that re-reads a growing conversation several hundred
times. Any cost model that reasons about "tokens generated" is off by two orders
of magnitude.

These are @jeffrey's own sessions in the full Aesthetic Computer tree, on a
million-token context, and they are an upper bound rather than a typical
Aesthetic Code session — the interface opens on one small piece file in a
workspace the user chose. So, bottom-up: thirty turns, context growing from
about 15k tokens to about 60k, averaging 35k. That is roughly 1.05M cache-read
tokens, 300k cache-write, 20k output. Priced across the shelf at today's
OpenRouter rates, treating auto-caching providers' first reads as ordinary
input:

| model | in / out per Mtok | one session |
|---|---|---|
| `anthropic/claude-opus-5` | 5.00 / 25.00 | $2.90 |
| `anthropic/claude-sonnet-5` | 2.00 / 10.00 | $1.16 |
| `openai/gpt-5.1-codex` | 1.25 / 10.00 | $0.71 |
| `anthropic/claude-haiku-4.5` | 1.00 / 5.00 | $0.58 |
| `z-ai/glm-4.7` | 0.40 / 1.75 | $0.24 |
| `openai/gpt-5.1-codex-mini` | 0.25 / 2.00 | $0.15 |
| `qwen/qwen3-coder-flash` | 0.195 / 0.975 | $0.12 |
| `deepseek/deepseek-v4-flash` | 0.086 / 0.171 | $0.05 |

$2.90 bottom-up against $4.18 measured for a comparable turn count is close
enough to trust the shape. OpenRouter takes no markup on inference — it passes
provider pricing through — but charges 5.5% with a $0.80 minimum to buy credits
by card, 5% by USDC, so multiply anything above by about 1.055 to get what
Aesthetic Computer actually spends.

**So what does "free for @handle holders" cost?** The honest unit is dollars,
not sessions, because sessions vary by a factor of a hundred. A cap of $2 of
inference per handle per month is about eight sessions on a GLM-4.7-class model,
sixteen on `qwen3-coder-flash`, forty on `deepseek-v4-flash` — and two thirds of
one session on Opus 5. The free tier's model choice is the whole decision; the
cap is arithmetic.

At $2 per active handle per month:

| monthly-active handles | cost to AC |
|---|---|
| 50 | $105 |
| 150 | $317 |
| 500 | $1,055 |
| 2,814 (every handle that exists) | $5,938 |

There are 2,814 `@handles` in the database today, which is the ceiling and
nothing like the realistic number. At a plausible 150 monthly-active, a free
tier on cheap coding models costs roughly **$300 a month**. Buying those same
150 people the same eight sessions each on Claude Opus 5 would cost about
$3,700. That factor of twelve is the whole argument for tiering: free means a
competent cheap model, and paying — in credit, in subscription, or with your own
key — means Opus.

The truly-free `:free` models on OpenRouter do not rescue this. Their limits are
20 requests per minute and 1,000 per day, and OpenRouter states that rate limits
are governed globally per account rather than per key, so a thousand requests a
day would be shared across every Aesthetic Computer user — roughly thirty
sessions a day for everyone combined. Useful as a fallback when a handle's cap
is exhausted, useless as a tier.

## Abuse, and where the money leaks

The per-key cap is a real hard boundary, and it is the only one worth relying
on. Aesthetic Computer's own metering can be wrong; OpenRouter refusing the
request at `limit_remaining: 0` cannot. So every minted key gets a `limit` and a
`limit_reset`, always, including for paying users — a subscriber whose cap is
$50 a month is a subscriber who cannot accidentally spend $500.

Rate limits are the gap. OpenRouter's are account-global, not per key, and the
documentation does not describe a configurable per-key requests-per-minute. So
one user in a tight agent loop can consume the account's request budget and
degrade everyone else, even while staying inside their own dollar cap. The
mitigation is at the minting endpoint — a per-handle floor on how often a key
may be issued or re-issued, plus a short `expires_at` so an abandoned key stops
mattering — and it is a partial one. Worth testing before promising a tier.

Runaway spend inside a single session is the most likely everyday failure, and
it is not malice. Aesthetic Code auto-approves tool calls by default:
`handleRequest` in `src/tui.mjs` answers `accept` unless `/ask on` is set,
because the first real session spent two of its two hours and nineteen minutes
parked on prompts with nobody watching. That default is right for a piece
session and it means a model that gets stuck in a read-grep-read loop bills for
every lap. The interface should show remaining cap in the header, the way it
shows the handle, so the number is visible before it is gone.

Prompt injection as a cost attack is real here specifically because Aesthetic
Code's Claude bridge has no operating-system sandbox — `docs/local-contract.md`
says so already. A file in the workspace that instructs the agent to re-read the
repository in a loop spends the handle's cap. The cap bounds the damage to one
handle's month, which is the correct blast radius and the reason a per-handle
key beats a shared proxy key.

Sharing a handle is the flat one: a handle is an identity, not a seat, and two
people using one get one cap between them. That is fine and self-limiting.
Sharing the *minted key* is different — it is a bearer token, extractable from
the user's own machine, spendable on anything OpenRouter sells until the cap
resets. This is the argument for small caps, short expiries, and a `PATCH
disabled: true` path that any support request can reach. It is also an argument
for `external.user` on the key, so OpenRouter's own view carries the handle and
an abuse report can be traced without Aesthetic Computer keeping request logs.

And `/api/ask` should be closed as part of this, not after it. It is the same
credential-and-entitlement problem with a smaller blast radius, it is live and
anonymous today, and it already parses the token counts that a ledger would
want.

## What could not be verified

There is no OpenRouter account or key anywhere on this machine or in the vault,
so everything about OpenRouter below is from its published documentation and
from unauthenticated route probes, not from a working call.

- **Whether the per-key `limit` is a hard server-side stop.** The documentation
  says "credit limit is set per key" and exposes `limit_remaining`, but it does
  not state what happens to an in-flight request that would exceed the limit, or
  whether a streaming response can overrun it. The whole recommendation rests on
  this being a real refusal rather than an accounting field, so it is the first
  thing to test with a $1 key. **This is the single biggest open question.**
- **Whether provisioning is gated or throttled at the scale this needs.** The
  create-key reference states no tier requirement and no cost, and a search
  turned up only that key creation is "rate-limited" without numbers. Minting
  and rotating keys for hundreds of handles may hit a ceiling nobody has
  published.
- **Whether per-key caching helps or hurts.** OpenRouter has posted that cache is
  scoped per API key so keys under one account stay isolated, described as in
  beta. For per-user coding sessions that is probably neutral — the cache is
  per-conversation anyway — but it was a tweet, not documentation, and the
  Anthropic-skin path's cache behaviour specifically is unverified.
- **Whether the Anthropic skin is GA.** The cookbook documents it and publishes a
  Claude Code recipe; `POST /api/v1/messages` answers 401 rather than 404. The
  documentation does not say GA or beta, and I have not made a successful call
  through it. Everything in the bridge section assumes it works as documented.
- **Model quality for agentic coding at the cheap end.** The prices for
  `glm-4.7`, `qwen3-coder-flash` and `deepseek-v4-flash` are real and current;
  whether any of them drives Claude Code's tool loop well enough to write a
  piece without frustrating someone is an empirical question the cost table
  cannot answer. The free tier's viability is a quality question, not a price
  one.
- **The realistic monthly-active handle count.** 2,814 handles exist. How many
  would open Aesthetic Code in a month is a guess, and the $300 figure moves
  linearly with it.

## What this changes in the contract

`docs/local-contract.md` currently says, in the inference boundary, that "each
bridge signs in with its own vendor's existing credentials on this machine;
Aesthetic Computer stores no key of its own." A minted OpenRouter key breaks the
second clause and needs its own paragraph: what is stored, where, under what
cap, that it is revocable, and that the prompts still go straight from this
machine to OpenRouter without passing through Aesthetic Computer. The
"no runtime account" line in the opening list also needs qualifying — a handle
is now optionally load-bearing for inference, not only for publishing.

Writing that paragraph honestly is a good test of the design. If it is hard to
write, the architecture is wrong.
