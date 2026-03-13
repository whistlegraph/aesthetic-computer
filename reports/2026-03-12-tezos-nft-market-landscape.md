# Tezos NFT Market Landscape — March 12, 2026 (PDT)

## Scope
- Snapshot focus: Tezos NFT market on Objkt, with XTZ quote from TzKT
- Snapshot captured: March 12, 2026 at ~`19:29` to `19:30` PDT (`2026-03-13T02:29Z` to `02:30Z`)
- Time basis for rolling data: marketplace `24h` fields and sales in the trailing 24 hours

## Chain Context
- XTZ quote at capture: `0.3685 USD`
- Practical implication: even large-looking tez numbers are still a relatively small USD market
- Example conversions at capture:
  - `12 XTZ` ~= `$4.42`
  - `72 XTZ` ~= `$26.53`
  - `100 XTZ` ~= `$36.85`
  - `1000 XTZ` ~= `$368.54`

## Broad Read
- The market is alive, but it is thin and segmented.
- Legacy Tezos NFT contracts still carry most of the liquidity.
- There is severe listing overhang in the broad catalog contracts.
- Curated 1/1 collections still clear meaningful tez prices, but the market is collector-led rather than broad retail momentum.

## 24h Collection Snapshot (Objkt)

Largest liquid pools still come from the legacy contracts and marketplace-wide catalogs:

| Collection | Contract | 24h Volume (XTZ) | Floor (XTZ) | Active Listings | Owners |
| --- | --- | ---: | ---: | ---: | ---: |
| fx(hash): Genesis | `KT1KEa8z6vWXDJrVqtMrAeDVzsvxat3kHaCE` | 3052.21 | 0.10 | 32269 | 20865 |
| hic et nunc | `KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton` | 2494.98 | 0.00 | 708900 | 145432 |
| open objkt | `KT1XaCf6gkjFnKg3QmPfn6gep53moMvjkj1E` | 992.27 | 0.00 | 17729 | 8558 |
| fx(hash) | `KT1U6EHmNxJTkvaWJ4ThczG4FSDaHC21ssvi` | 217.92 | 0.08 | 166423 | 35679 |

More curated, higher-end movement is still present:

| Collection | Contract | 24h Volume (XTZ) | Floor (XTZ) | Active Listings | Owners |
| --- | --- | ---: | ---: | ---: | ---: |
| Installation views | `KT1MCaRatjwuy8poXT4ApihQetaqhj4rGXMe` | 1007.00 | 1000.00 | 0 | 38 |
| PERMANENT | `KT1FYCpBuouzkRScrs265RsNDJS4PehNm7Bz` | 1000.00 | 45.00 | 93 | 42 |
| ANGEL CARVED BALLPOINTZ | `KT1CZGDwXMR63JycKwn7MmnKxBWjbfpTeFeL` | 720.00 | 15.00 | 12 | 4 |
| Kashmir through my lens | `KT1TGySrFHhNHMuUD3DyVfiDczidbX4gLY8a` | 699.00 | 7.77 | 6 | 28 |
| joints | `KT19CPoS1qbGud3BanwXgUC6VguBFhk9X2Vp` | 609.00 | 203.00 | 7 | 10 |
| click click click click click | `KT1R5PSkR5hbXkArGUCXNSD32mjTPzqyyrE9` | 585.00 | 85.00 | 17 | 159 |

## Notable 24h Sales
- `tr4ns4ctions #162` on `fx(hash): Genesis` sold for `1800 XTZ`
- `Lunas` from `Installation views` sold for `1000 XTZ`
- `ANGEL CARVED BALLPOINT 013` sold for `720 XTZ`
- `Fleeting Reminiscence` from `Kashmir through my lens` sold for `699 XTZ`
- `Strato, Segno, Respiro` from `PERMANENT` sold for `600 XTZ`

## Market Structure
- The biggest volume still sits in old, broad, low-floor catalogs.
- The biggest supply overhang also sits there:
  - `hic et nunc`: `708900` active listings
  - `fx(hash)`: `166423` active listings
  - `fx(hash): Genesis`: `32269` active listings
- That makes buyer attention scarce and pushes value toward curation, collector trust, and repeat buyers rather than raw mint count.

## Outlier Caveat
- Objkt's 24h leaderboard currently shows `elementary animal collection for Morning and Night` at `12301 XTZ` of 24h volume with only `39` items and `5` owners.
- Direct `listing_sale` and `offer_sale` queries for that same contract in the same trailing 24h window returned no rows during this snapshot.
- Interpretation: treat that leaderboard row as an anomaly, non-standard activity, or otherwise non-representative of the broader market. It should not be used as the baseline signal for Tezos NFT demand today.

## Where KidLisp Fits
- Collection: `KidLisp`
- Contract: `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`
- Live snapshot near the same capture:
  - Items: `34`
  - Owners: `9`
  - Active listings: `1`
  - Floor: `12 XTZ`
  - Volume (24h rolling): `72 XTZ`
  - Volume (total): `254.5 XTZ`

KidLisp is not in the top 25 by 24h volume from this snapshot, but it is not invisible either. The lower edge of the sampled top-25 set was `100 XTZ` in 24h, and KidLisp was sitting at `72 XTZ` with only `34` total items and just `1` live listing. Relative to its size, that is real activity.

The main constraint is not price ambition. It is market breadth:
- `12 XTZ` is only about `$4.42` at the capture quote
- That price is easy to participate in
- Current demand is real but concentrated
- Discovery and collector relationship management matter more than squeezing the floor upward

## Takeaway
- Tezos NFTs today are culturally alive, but the market is compact in USD terms.
- Legacy contracts still dominate raw liquidity.
- Curated 1/1 art still clears, sometimes at high tez numbers.
- Supply remains enormous almost everywhere, so attention is the scarce asset.
- For KidLisp specifically, accessible pricing plus collector communication is the right lever. The present market does not argue for aggressively pushing floor; it argues for widening participation.

## Sources
- TzKT quote endpoint: `https://api.tzkt.io/v1/quotes/last`
- TzKT head endpoint: `https://api.tzkt.io/v1/head`
- Objkt GraphQL endpoint: `https://data.objkt.com/v3/graphql`
- Objkt collection pages:
  - `https://objkt.com/collections/KT1KEa8z6vWXDJrVqtMrAeDVzsvxat3kHaCE`
  - `https://objkt.com/collections/KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton`
  - `https://objkt.com/collections/KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`
