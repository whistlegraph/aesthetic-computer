# NFT attention map — 2026-05-18

> Read-only network survey. Source: OBJKT GraphQL (keyless) + fxhash, last 48h
> of on-chain sale events (500 events, price > 0). ETH/Base: see gap below.
> Companion to `2026-05-18-crypto-portfolio-market-report.md`.

## the honest ceiling first

Tezos NFT sale price distribution, last 48h (500 sales):

| min | median | p90 | p99 | max |
|---|---|---|---|---|
| 0.05 tz | **8.8 tz (~$3)** | 275 tz (~$93) | 30,000 tz | 10,000,000 tz |

The p99/max are **wash trading** (`hic et nunc` 10M tz/24 "sales",
`_syntax` 69,420 tz, `EmProps` 10k-tz medians with 2 addresses). Filtered out
below. The **real** market is single-digit-to-low-hundreds of tez. A median
sale is **~$3**. A genuinely "hot" sale is ~$90. "Some cash" on this surface
realistically means **tens of dollars from a well-targeted drop**, not an
income event. Mapping attention here is worth doing — but size expectations
to the data.

## where the real attention is (wash-filtered, ranked by distinct buyers)

| collection | distinct buyers | sales | median | read |
|---|---|---|---|---|
| **open objkt** (OE / minting factory) | 12 | 26 | 8.8 tz | broadest genuine attention — cheap open-edition mint surface |
| hic et nunc / teia | 8 | 23 | 120 tz | OG community still transacting at real prices (~$40) |
| Friedeberg | 2 | 9 | 23 tz | small, concentrated |
| CONTINGENT | 2 | 10 | 100 tz | whale-concentrated |
| HYPER[]GLITCH | 1 | 41 | **180 tz** | **one whale swept ~7,150 tz of glitch art** |
| TAPE LOOP CITY | 1 | 45 | 35 tz | one collector sweeping a cheap series |
| fx(hash): Genesis | 1 | 40 | 4 tz | one collector sweeping cheap fxhash |

**Pattern:** attention at this scale is **a person, not a category**. The
"trending" tail is individual collectors sweeping one series each. The only
broad-distribution surface is cheap OBJKT open editions (~$3/sale).

## the attention graph — who is spending right now

| address | buys (48h) | tz | breadth | note |
|---|---|---|---|---|
| `tz1c2ayR85dUMXZAvNixTJz1WdW9TqvUgfRq` | 9 | 123 | **6 collections** | most active diversified collector — prime target |
| `tz1VwAPwtHtQ2jADWSzBJ8kBQQMV2NtcU4W8` | 7 | 96 | 3 | solid active collector |
| `tz2JpM2Z7p1iWLY5DgBa23xLU1RY1tqhPHQZ` | 5 | 514 | 1 | single-collection whale (glitch/EmProps lane) |
| `tz2DviPC4iRCWBvTktAPLxauX5W6URa6WPY2` | 5 | 40 | 5 | diversified, low-budget |
| **`tz1WrEjvFiVTChGdxAfH8qyB3v9KyN97uUTg`** | 3 | 48 | 3 | **= March's #1 keeps-airdrop recipient ($b00, 5,500 tz/14d). Still active 7 weeks later.** |
| `tz1aYMJM8ke8msTo2LyGwdMBxxdbFsKmfBDK` | 25 | 4 | 1 | mint-farmer (free/cheap OE spam) — ignore |

## the opportunistic play (concrete, honest)

Broad minting is **not** the move — median $3, and the "trending" collections
are single whales, not crowds. The only thing that has ever moved keeps is the
**March playbook: mint → airdrop to proven active spenders → royalty on their
resale.** It still applies, and the targets are now identified:

1. **Re-engage `tz1WrEjv…`** — March's top airdrop recipient, *still buying
   today*. Highest-confidence single contact: mint 1 keep, airdrop to this
   exact address. Proven 5,500 tz spender who already engaged once.
2. **Airdrop the diversified actives** — `tz1c2ayR…` (6 collections),
   `tz1VwAPw…`, `tz2DviPC…`. Real taste, buying this week, low spam risk.
3. **One glitch-themed keep into the whale lane** — a whale swept ~7,150 tz of
   HYPER[]GLITCH; another (`tz2JpM2Z…`) spent 514 tz single-collection.
   kidlisp does visual-corruption aesthetics natively. A glitch-themed 1/1
   keep, listed ~150–250 tz and offered directly to those wallets, is the
   single highest-$ shot (~$50–90 if it lands).
4. Funnel only: a cheap open edition rides the "open objkt" 12-buyer surface
   for visibility — but that's ~$3/sale, count it as marketing not revenue.

**Realistic upside of the whole play: ~$50–150 over the coming weeks**, mostly
from (1)+(3), driven by targeted airdrops + royalties, not volume. Plus it
re-activates the keeps engine (idle ~3 weeks).

## ETH / Base — AUDIT GAP (needs a decision)

Every keyless ETH/Base NFT trending source is dead or gated:
- OpenSea v2 → 401 (key required)
- Reservoir → key-gated (collections + users)
- Zora discover/trending → 404 (public API moved/gated)
- CoinGecko keyless `/nfts/list` → returns only static blue-chips
  (Punks/BAYC/Pudgy/Azuki) with no real volume ordering — useless for
  *new/opportunistic* signal, and those aren't mint opportunities anyway.

The real ETH-side opportunistic-mint surface in 2026 is **Zora/Base open
editions + Farcaster mints**, which requires a data API key. The vault holds a
**read-only** OpenSea data key (distinct from any wallet secret). I did **not**
reach for it — using a vault credential is your call. Options:
- **Authorize the read-only OpenSea/Zora data key** → I extend this map to
  Base/Zora/Farcaster mint activity.
- **Skip ETH** → focus the (cheap, fast, fully-mapped) opportunistic play on
  Tezos, where the keeps infra already exists.

## reproduce
`/tmp/tez_events.json` is the raw 48h pull. Survey logic is ad-hoc jq over the
OBJKT GraphQL `event`/`fa` types (schema notes: `event.price_xtz`,
`event.recipient_address` = buyer, `fa.volume_24h` is stale — aggregate from
`event` instead).
