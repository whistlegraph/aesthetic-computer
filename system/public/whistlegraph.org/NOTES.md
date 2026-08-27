# whistlegraph.org — open notes

Deferred items from @minanimals (Alex Freundlich) via iMessage, 2026-07-10.
Data edits + copy/loop changes from that thread are already applied to `graphs.json` / `index.html`.

## Whistlegraph Desk

`/desk` is the private Whistlegraph Desk. It uses the existing Aesthetic Computer
Auth0 application and authorizes the immutable Auth0 subjects for @jeffrey and
@alexf (formerly @minanimals, renamed 2026-08-13) on the server; matching a
display handle is never enough. Live edits
are stored as a MongoDB curation overlay and applied on top of the generated
`graphs.json` / `posts.json`, so the scraper remains reproducible and an edit can
be restored to generated data at any time.

The Auth0 application needs these production entries before first deployment:

- Allowed callback URLs: `https://whistlegraph.org`,
  `https://www.whistlegraph.org`
- Allowed logout URLs: the same two URLs
- Allowed web origins: `https://whistlegraph.org`,
  `https://www.whistlegraph.org`

Auth0 returns to the bare origin; Caddy recognizes its `state` parameter, serves
the Desk callback there, and the client replaces the address with `/desk` after
the transaction completes. Ordinary visits to `/` continue to show the public site.

The server allowlist defaults to the two current Auth0 subjects. Override it with
the comma-separated `WHISTLEGRAPH_ADMIN_SUBS` environment variable when identities
change; do not put email addresses or handles in that variable.

### Codex publishing

The Desk's **Codex tools** panel installs the repository's `plugins/whistlegraph`
plugin. Its MCP creates a clean worktree from `origin/main`, restricts writes to
`system/public/whistlegraph.org/**`, validates the patch, creates an attributed
review-branch commit, and fast-forwards `main` only when the reviewed base is still
current. The plugin then POSTs the exact commit SHA to the Auth0-protected
`?action=deploy` endpoint. Production re-checks that SHA against `origin/main`
before `lith/webhook.sh` changes the live checkout. Neither the MCP nor the browser
receives `DEPLOY_SECRET` or the deployment vault.

The publisher requires the Tangled knot as the checkout's `origin`; GitHub remains
the public marketplace/mirror used to install the plugin.

## Bigger feature — needs a data model (2026-07-11)
- **Per-page source-video list.** On each whistlegraph's detail page, list *every*
  TikTok video feeding its aggregate data, each with its own view count — so we can
  audit that the numbers (and which takes belong to which composition) are accurate.
  `graphs.json` currently only stores aggregate `views` + a `perf` count, not the
  individual source URLs/views. Needs a `sources:[{url,views,date}]` array per graph
  (populated from the TikTok archive / Notion) plus a render block in `showDetail`.
  This will also make the folds above self-documenting.

## Data-accuracy pass (Alex, 2026-07-11) — resolve during the source-video intake
- **Renamed codes break CDN assets.** Archive assets are keyed by the *original*
  recovered code (`index/<code>.jpg|.mp4`). Any future code rename must add an
  `asset:"<oldcode>"` field (see `bowm`→lvme) or the thumbnail/video 404s.
  *(2026-08-08: audited every derivable URL against the bucket; 12 renamed works
  were missing their `asset` key and got one — nbff, mush, pump, tipj, bubb,
  bpen, kiss, trip, bugy, asmr, lily, fssl. All asset URLs now resolve.)*
- **Suspicious counts.** `2la` "Certain Personality" shows 10 videos — Alex thinks
  that's dubious. The per-page source list is the way to verify what's actually
  filed under each whistlegraph (he suspects some videos are mis-filed).
  *(2026-08-10: the source list now exists as `/api/wg/sources/<code>` — see
  "Machine access" below. Ran it across all 291 works: **290 reconcile exactly**
  between the reported `perf`/`views` and the summed `contributes` edges. The one
  that doesn't is `lkty` Little Kitty — reported 1 video / 25,900 views against 0
  contributing posts, i.e. the intentionally-empty record noted in the kitty-family
  review below. So the aggregates are arithmetically sound; what Alex suspects is
  **mis-filing, not miscounting**, and that the endpoint can't settle on its own.
  `2la` now carries 12 contributing posts, not 10, and several look worth a second
  look — the 7.4M "see we're not scary!! just #cute and #sunny" and the
  "#duet with @nbekc15" among them. Hand that list to Alex.)*
- **Double posts.** When unprivating everything, decide whether duplicate re-posts
  count toward a whistlegraph's video total or get de-duped. (Alex's open question.)
- **Missed attributions.** Alex says a few attribution requests from the past couple
  days didn't take — needs a final comb-through against his list (get specifics).
- Expect a **large manual component** to the final accuracy check.

## "Where to find our merch?" section (Alex/jeffrey, 2026-07-11)
- Don't funnel merch to one storefront — being spread across URLs and stocked in
  multiple shops (incl. museum / art-museum gift shops) is a feature. Add a
  **"Where to find our merch?"** section that *lists the outlets*: Sex Magazine
  (the zine), the Whistlegraph shop, shop.aesthetic.computer's whistlegraph section
  (TBD), plus any brick-and-mortar / museum gift shops that carry it. A directory,
  not a checkout.
- **Fan-made / user-made merch.** Some whistlegraph merch was made by users & fans —
  worth surfacing (a "made by the community" strand of the directory). Evidence lives in
  the **Instagram platter history** (see `/social`) and possibly **Notion**. Task:
  source that evidence (screenshots / posts / links) — good subagent job, can lean on
  the just-repurchased Notion seat + the IG archive.

## To design / decide
- **Archive combing.** Cross-check the full list against the old Notion + website
  archive; fold duplicates, decide what to keep vs. ditch, and pull in any linkable
  extras (lectures, livestreams, old blogs) not already in "Lectures, shows & press."
  Notion seat was repurchased 2026-07-10 to crawl this.
- **CV line-items.** *The Longest Whistlegraph Ever (So Far)* and *Underpainting*
  live mainly as bottom-of-page CV items, but Alex wants them to still carry a
  `[code]` and count toward the ultimate total even if they're out of the main scroll.
  (`long` and `undr` codes already exist — confirm they're counted the way he means.)

## Needs input from Alex
- **Kunstverein Hamburg link.** He asked to add "Kunstverein Hamburg" on the Feral
  File line (Elsewhere) but the URL didn't come through in iMessage ("have the link
  be:" arrived empty). Placeholder currently points at the existing Kunstverein
  Hamburg recital video (YouTube `YXUUCkqv2LY`) — swap in the intended link.
- ~~**"More Than Enough" duplicate.**~~ Resolved — `wtm` and `more` both alias to
  `enuf`, the single canonical *More Than Enough*.

## Resolved 2026-07-27

- **Calmer cover colors.** The ten featured score-sheet works retain their
  intentional palette. The archive is neutral except for restrained,
  thumbnail-derived accents on the exhibited `long` and `undr` records.
- **Exhibition media.** `long` now carries the complete film, official trailer,
  and two clearly labeled short features; `undr` carries both named YouTube
  performances. All six IDs are present in the checked-in YouTube inventory and
  render through privacy-enhanced embeds with direct source links.
- **Curation lock.** Confirmed work identity/credit and explicit post→work
  relationships are snapshotted in `downloads/curation-lock.json`. Generation
  now fails closed on drift; after manual review, use
  `node toolchain/whistlegraph/gen-model.mjs --accept-curation` to accept the
  exact proposed change.
- **Kitty-family review.** The drawing/doodle/sketch explainer is now a standalone
  talk; the open-source announcement that performs *Lost Kitty* moved to `lost`;
  and the 2024 “I’m a little kitty” post restored the empty `lkty` record. The
  Kitty/Doggy mashups remain explicitly cross-associated.

## Machine access (2026-08-10)

The archive is now published for LLM and agent readers.

**Free, and deliberately complete.** `graphs.json` and `posts.json` are already
served unauthenticated, so gating the same facts would be theater — the
discovery value of being the canonical machine-readable source of the artform is
worth more than the gate.

- `/llms.txt` — the [llms.txt](https://llmstxt.org) convention; a short linked map
- `/index.md` (also `/llms-full.txt`) — the whole index as Markdown: 291 works,
  294 candidates, 793 legacy codes, every alias, every asset URL resolved
- `/robots.txt` — the licensing assertion + pointers to everything above

All three are generated by `toolchain/whistlegraph/gen-llms.mjs` from the same
data the site renders. **Rerun it after every `gen-model.mjs`**, or the machine
index silently goes stale while the human one updates. Prose lives in
`toolchain/whistlegraph/llms-prose.md`; `index.md` is generated — never hand-edit it.

**Paid, via [x402](https://x402.org).** `system/netlify/functions/whistlegraph-llm.mjs`,
routed at `/api/wg/*`. HTTP `402 Payment Required`: ask for a resource, get the
terms back, pay, repeat with an `X-PAYMENT` header. No account and no API key,
because the buyer is usually not a person. Asking unpaid is always free.

| Endpoint | Price | What it is |
|---|---|---|
| `/api/wg/bulk` | 5.00 USDC | The whole normalized dataset in one document |
| `/api/wg/sources/<code>` | 0.10 USDC | The source videos behind one work — the audit trail published nowhere else |
| `/api/wg/license/<code>` | 1.00 USDC | A signed, verifiable redistribution license |
| `/api/wg/verify` | free | Checks a receipt's signature and term |

The license endpoint is honest that the CDN assets are already publicly
reachable: what it sells is the licence and the signed receipt, not access.
Claiming otherwise is a lie the buyer discovers in one request.

Verification is free and stateless. The signature covers the entire receipt —
licensee and issue date included — so a work code can never re-derive it, and
nothing here keeps a database of who bought what. The receipt therefore travels
inside its own `verify` link as a base64url token: `?receipt=<token>&sig=<hex>`,
checked against the exact bytes that were signed. An edited receipt fails, and
a genuine receipt past its term reports `valid: false` with `expired: true`
rather than pretending the grant still stands.

### Before it can take money

The function **fails closed** — unconfigured, every paid route answers 503
rather than serving paid data for free. It needs, in the lith environment:

- `WHISTLEGRAPH_X402_PAY_TO` — receiving address
- `WHISTLEGRAPH_X402_ASSET` — the USDC contract on the chosen network
  (**verify this address against the network's own docs before going live**)
- `WHISTLEGRAPH_X402_NETWORK` — defaults to `base`
- `WHISTLEGRAPH_X402_FACILITATOR` — defaults to `https://x402.org/facilitator`
- `WHISTLEGRAPH_X402_FACILITATOR_TOKEN` — bearer token, if the facilitator wants one
- `WHISTLEGRAPH_LICENSE_SECRET` — HMAC key for signing license receipts

**The default facilitator cannot settle Base mainnet.** Ask it yourself:

```
curl -s https://x402.org/facilitator/supported
```

It advertises testnets only — `base-sepolia`, `solana-devnet`, `hedera:testnet`
and friends. Base mainnet (`eip155:8453`) is not among them, and it speaks x402
`v2` with CAIP-2 network names while this function speaks `v1` with `"base"`.
So the pairing shipped here quotes a mainnet price in real Circle USDC that its
own facilitator has no way to take: the buyer signs, `verify` is refused, and
the request dies at 502 having promised a settlement that was never possible.

Real money needs a facilitator that holds an account — Coinbase CDP (fee-free on
Base) or PayAI — set through `WHISTLEGRAPH_X402_FACILITATOR` and its token. The
function asks `/supported` before it quotes, so until that swap happens the paid
routes answer **503 saying exactly that**, which is the honest failure: a price
nobody can pay is worse than no price at all.

Open: listing in the [x402 Bazaar](https://docs.cdp.coinbase.com/x402/seller/get-discovered),
the catalog buying agents actually browse, requires the CDP facilitator, per-route
metadata (a ≤500-character description of *when* to call the endpoint, input and
output schemas, `METHOD /path` keys), a `POST` to
`https://api.cdp.coinbase.com/platform/v2/x402/validate`, and one real settlement
to activate. Nothing gets discovered until the facilitator swap lands.
