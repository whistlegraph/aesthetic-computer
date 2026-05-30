# keeps opportunistic targeting — runbook (2026-05-18)

> DRAFT for @jeffrey to execute. This audit is read-only: no keys decrypted,
> nothing signed or sent by the agent. Every on-chain step below is yours to
> run/sign. Built from `2026-05-18-nft-attention-map.md`.

## honest expected outcome

~**$50–150 over the coming weeks**, from targeted airdrops + royalties on
resale — not "cash right now." The real value is **re-activating the keeps
engine** (idle ~3 weeks) by putting tokens in front of proven active
collectors. Median Tezos NFT sale is ~$3; this only works because it's
*targeted at specific funded buyers*, not broadcast.

## priority targets (verified, this week, public data)

| # | wallet | identity | on-hand | why |
|---|---|---|---|---|
| 1 | `tz1WrEjvFiVTChGdxAfH8qyB3v9KyN97uUTg` | **Louis** (OBJKT) | **753 tz (~$254)** | March's #1 keeps-airdrop recipient ($b00, 5,500 tz/14d). Still buying this week. Funded. Already engaged once → warm. |
| 2 | `tz1VwAPwtHtQ2jADWSzBJ8kBQQMV2NtcU4W8` | **Jose Antonio Ojeda** · `ojedasbodega.tez` | 531 tz (~$179) | Collector since 2021 (15.8k tx), 7 buys/3 collections this week, funded. |
| 3 | `tz1c2ayR85dUMXZAvNixTJz1WdW9TqvUgfRq` | **6ENERATOR** · `generator.tez` | 1.8 tz | Tastemaker since 2021 (14.9k tx). Spends fast (low idle balance), 9 buys/6 collections this week. Distribution/visibility value. |
| 4 | `tz2DviPC4iRCWBvTktAPLxauX5W6URa6WPY2` | (anon) | 131 tz | Newer (Jan 2026) but diversified — 5 buys/5 collections. Secondary. |
| — | `tz2JpM2Z7p1iWLY5DgBa23xLU1RY1tqhPHQZ` | (anon) | 0.6 tz | Was a 514-tz single-collection whale; balance now near-zero (moved funds). **Skip unless it re-funds.** |

## the asset to deploy

You hold 4 keeps free to use: **$odj #28 (unlisted)**, $pie #81, $tam #86,
$r94 #87. Two routes:

- **A — airdrop existing held tokens.** Zero mint cost. Send $odj #28 (your
  only unlisted held token) to target #1, and re-home one of #81/#86/#87.
  Fastest, no new work.
- **B — mint a fresh keep into the glitch lane (highest-$ shot).** A whale
  swept ~7,150 tz of `HYPER[]GLITCH` this week; the glitch/visual-corruption
  aesthetic is hot and kidlisp does it natively. Mint one corruption-themed
  keep (~2.5 tz cost), list it ~150–250 tz, and offer/airdrop a copy toward
  the glitch buyers + target #1. Best single-sale upside (~$50–90 if it
  lands).

Recommended: **A for targets #1–#3 now** (free, re-activates engine
immediately) + **B as the one revenue swing** this week.

## runbook (you execute — keys never leave your hands)

1. **Withdraw the 7.5 tz already in the contract first.** From the keeps-admin
   wallet, call `withdraw_fees` on `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`.
   That's the only genuine cash-now (~$2.50) and funds the mint below.
2. **(Route A) Airdrop held tokens.** FA2 `transfer` on the keeps contract:
   `$odj #28 → Louis (tz1WrEjv…)`; one of $pie/$tam/$r94 → `ojedasbodega.tez`.
   (Tokens currently listed must be de-listed on OBJKT first or the transfer
   will conflict with the active ask.)
3. **(Route B) Mint the glitch keep.** `keep` entrypoint (~2.5 tz), kidlisp
   corruption piece, IPFS via the self-hosted lith node. List ~150–250 tz on
   OBJKT. Place it on the radar of the glitch buyers (offer or airdrop a
   variant to target #1).
4. **Surface the stale standing bids.** `KT1…` has open offers of **100 tz on
   #3 ($3jj)** and **16.9 tz on #53 ($fsf)** from `tz1QRXx…`, untouched 7
   weeks. You don't hold #3/#53, but a public nudge to those holders (DM /
   chat invite) that there's a live 100-tz bid → if they sell, you collect
   **10% royalty (≈11.7 tz / ~$3.9)**. Free to do.
5. **Re-list $odj #28** if you keep it instead of airdropping (held, currently
   unlisted) and sanity-check the 6 live listings (7–14 tz) vs comparable
   recent sales (~8.8 tz median — your floor listings may be over-priced for
   the current market).

## still blocked: ETH/Base extension

You authorized the read-only OpenSea/Zora data key, but the harness safety
layer independently blocks the agent from scanning vault credential files —
even with your approval — and I won't route around that. To unblock, pick one:

- **Tell me the exact location/var name** (e.g. "OpenSea key is
  `OPENSEA_API_KEY` in `vault/<path>/.env`") so I read only that, no scanning.
- **Add a Bash permission rule** allowing the read, then I retry.
- **Paste it** into a throwaway env var yourself: `set -x OPENSEA_API_KEY …`
  in a `!`-prefixed prompt, and I'll use `$OPENSEA_API_KEY` without ever
  reading the file.
- **Stay Tezos-only** — recommended; ETH-side has the same ~$3–90 ceiling and
  needs new infra, while everything above is ready to run today.
