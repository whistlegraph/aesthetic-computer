# X API for @promptDOTac

**Provisioned 2026-09-11.** Credentials are in `vault/promptdotac/x.env` and
`node toolchain/x/x.mjs --as promptdotac me` returns the live account. What is
recorded here is what the console actually does now, which is not what most
guides (or X's own older docs) still describe.

## What exists

- Developer account `2098529848490643457`, display name "aesthetic computer"
- App `33424789`, "2098529848490643457promptDOTac"
- App permissions: **Read and write**; type: **Web App, Automated App or Bot**
- Callback + website: `https://aesthetic.computer/`
- Project access: **Default Project (Pay Per Use)**

## The thing that will surprise you

**There is no free tier.** The old developer.x.com portal with Free / Basic /
Pro tiers — the one that gave 500 posts a month for nothing — has been replaced
by `console.x.com`, which is pay-per-use against a credit balance. A new
account starts at **$0.00 balance and $0.00 free credits**, and the only
projects on offer are Default (Pay Per Use), Ads (MCP access only), and
Enterprise (sales managed).

The lane initially proved its signing and media path before credits were loaded:

```
$ node toolchain/x/x.mjs --as promptdotac post "..."
uploaded workshop-card.jpg (2098530834592473090)
402: credits depleted — the X API is pay-per-use and this account's credit
balance is empty. Buy credits at console.x.com → Billing → Credits...
```

Note what *did* work in that run: `GET /2/users/me` returned the profile, and
the media upload returned a media id. Reads and uploads go through; only
`POST /2/tweets` is metered to zero. That is a useful signal — it means the
OAuth 1.0a signing is correct against the live API, and the only thing missing
is money.

On 2026-09-11, $25 in credits was loaded and @promptDOTac made its first live
API post. Add or inspect funds at console.x.com → Billing. Keep a Billing Cycle
Cap there; it is the only fleet-wide spending limit.

## Adding a second account (@whistlegraph)

Repeat the whole flow signed in **as that account**, then write
`vault/whistlegraph/x.env` with the `WHISTLEGRAPH_` prefix. Each X account
needs its own developer account and app; keys are not shared.

1. Sign in to x.com as the account, in a normal browser window.
2. Go to <https://developer.x.com/en/portal/dashboard> — it redirects to
   `console.x.com/onboarding`. Fill in an account name and a use-case
   description, tick the three agreement boxes, Submit. An app is created for
   you automatically; there is no separate "create app" step.
3. App → **Settings** → set **Read and write** and **Web App, Automated App or
   Bot**, fill Callback URI and Website URL (both required), Save. Saving
   reveals the OAuth 2.0 client id and secret once.
4. App → **Keys & Tokens**:
   - Consumer Key is **masked and cannot be read back** — the console only ever
     shows the last six characters. Hit **Regenerate** to see the full key and
     secret once. On a fresh app nothing is using it yet, so this is safe.
   - Under OAuth 1.0 Keys → Access Token, hit **Generate**. Confirm the header
     reads "For @<handle>  Read and write" before you do; a token minted while
     the app was still read-only carries the old scope and 403s on every post.
5. Write the four values into `vault/<account>/x.env`.

## Verify

```bash
node toolchain/x/x.mjs accounts            # ✅ / ❌ per account, no secrets shown
node toolchain/x/x.mjs --as promptdotac me # handle + follower counts
node toolchain/x/x.mjs --as promptdotac budget
node toolchain/x/x.mjs --as promptdotac search "tezos art -is:retweet"
node toolchain/x/x.mjs --as promptdotac post "test" --dry-run
node toolchain/x/x.mjs --as promptdotac reply <post-url-or-id> "test" --dry-run
```

## Notes

- **Auth is OAuth 1.0a user context**, not OAuth 2.0. Four static strings, no
  expiry, no refresh dance. The OAuth 2.0 client id/secret are kept in the
  vault file too, unused, because the console shows them only once.
- **Character counting is weighted.** Every URL costs a flat 23 regardless of
  length, because X rewrites it to t.co. `x.mjs` accounts for this; a plain
  `text.length` would reject posts that actually fit.
- **Alt text is required** whenever `--media` is given. Local rule, not X's.
- **Recent search is metered.** The CLI defaults to 10 results and reserves the
  request's worst-case cost against a local $0.25/day estimate, assuming
  $0.005 per post read. Override those assumptions with
  `<PREFIX>_X_DAILY_READ_BUDGET_USD` and `<PREFIX>_X_READ_USD_PER_POST` only
  after checking the live console price. Ledgers live under
  `~/.local/state/aesthetic-computer/x-api/`; they are per host, so the console
  Billing Cycle Cap is still authoritative across Neo and Blueberry.
- **Replies are explicit.** `reply <post-url-or-id> "text"` sets
  `in_reply_to_tweet_id`; the MCP requires `confirm:true` for the live action.
  X restricts self-serve API replies to posts whose author already mentioned
  or quoted the replying account. Other replies must be made manually in X;
  do not use browser automation to evade the platform rule.
- The signing is covered by `spec/x-oauth-spec.mjs`, which checks it against
  the worked example in X's own documentation.
