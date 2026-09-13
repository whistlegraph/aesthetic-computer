---
name: oskiewar-cull
description: Cull underperforming oskiewar reels from Instagram. Use when @jeffrey says "cull the oskiewar reels", "trim the reels", "delete the underperformers", or asks to clear reels under the view line. Drives the deletion in Chrome, because Meta's API has no deletion endpoint.
---

# Oskiewar reel cull

Meta's Graph API can publish a reel and report on it. It cannot delete one.
That is not an oversight in this pipeline — there is no endpoint — so the only
road to deleting a reel is a browser, and `trim.mjs` is deliberately built with
no Instagram login and no deletion path at all. It reports; something else acts.

This skill is that something else. It is you, in Chrome, holding a work list
that a read-only tool produced.

**The account has ~49.5k followers and deletion is irreversible.** Nothing here
deletes without a fresh yes from @jeffrey against the actual list.

## 1. Refresh, then read the list

```sh
cd ~/aesthetic-computer
set -a; . vault/oskiewar/instagram.env; set +a   # OSKIEWAR_IG_TOKEN
node xbox/live/marketing/reel.mjs --insights     # views AND permalinks
node xbox/live/marketing/trim.mjs                # the work list
```

Refresh first, every time. The policy line is 1,000 views and reels keep
earning views for weeks, so a candidate read off stale insights is a reel you
may be deleting after it passed the bar. Insights older than a day or two are
not good enough to delete on.

`--insights` is also what fills in `permalink`. A candidate printed with *no
permalink recorded* has nowhere for you to go: run the insights pass and ask
again.

`node xbox/live/marketing/trim.mjs --json` gives the same list as data.

## 2. Read what the misses mean before deleting them

A reel answering `error subcode 33` — *"does not exist, cannot be loaded due to
missing permissions"* — during the refresh is **already deleted**, as long as
other reels refreshed fine on the same token. Those are ghosts, not candidates.
Do not go looking for them in Chrome; they are gone.

Check that the ghost is recorded (`deletedAt` in `ledger.json`). If it is not,
that is bookkeeping, not a cull.

## 3. Get a yes

Show @jeffrey the candidates — date, views, watch time, skip rate, segment —
and wait. Deleting a reel is outward-facing and cannot be undone. A prior
"go ahead" does not carry to a list he has not seen.

Never delete anything that is not in the trim output. If a reel looks bad but
the policy did not propose it, say so and leave it.

## 4. Delete each one in Chrome

Invoke the `claude-in-chrome` skill, then `tabs_context_mcp` first. If the
extension is not connected, stop and say so — there is no fallback.

For each candidate, open its `permalink`, then: the `⋯` menu on the post →
**Delete** → confirm in the dialog.

- Instagram's confirmation is an in-page modal, not a JS `alert`, so it is safe
  to click. If a real browser dialog ever appears, the extension goes deaf and
  @jeffrey has to dismiss it by hand — say so rather than retrying.
- Verify each deletion landed before moving to the next. A reel that silently
  failed to delete and gets recorded anyway puts a lie in the ledger that
  nothing will ever catch, because the tool trusts the record over the API.
- If two deletions in a row fail, stop and report. Do not grind.

## 5. Record what you actually deleted

```sh
node xbox/live/marketing/trim.mjs --record-deleted <id>,<id> --confirmed-web-delete
```

Only the ids you watched disappear. `--confirmed-web-delete` is an assertion
that a human confirmed the deletion happened — it is not a formality, and it is
why this flag exists instead of the tool just deleting things.

`recordDeleted` refuses any id the policy would not currently propose, so
record promptly: a reel that ages past the 30-day pool, or whose insights go
missing, becomes permanently unrecordable and will read as live forever.

Then commit — `ledger.json` is tracked, and another session in this checkout
will otherwise sweep it into an unrelated commit.

## What the ledger knows

`xbox/live/marketing/ledger.json`, one row per published reel:

| field | |
|---|---|
| `mediaId` | Instagram's API handle. Not an address. |
| `permalink` | The address. Filled by the insights pass. |
| `urls` | The DO Spaces mp4 and cover uploaded **for** the post — not the post. |
| `insights` | Views, reach, watch time, skip rate. `null` until Meta computes them, never zero-as-unknown. |
| `deletedAt` / `deletedReason` | Set only by `--record-deleted`. History is never removed. |

## Policy

`trimPolicy` in `trim.mjs`: under **1,000 views**, at least **24 hours** old,
within the **30-day** deletion pool, live, with insights present. Missing
insights are protected — an unmeasured reel is never a candidate.

The 30-day bound is Instagram's own bulk-delete pool, not a preference.
