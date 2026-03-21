# Keeps Market Report — March 11, 2026 (PDT)

## Scope
- Collection: `KidLisp` on Tezos mainnet
- Contract: `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`
- Time window for "today": `2026-03-11T07:00:00Z` to `2026-03-12T07:00:00Z` (Los Angeles day, PDT)
- Snapshot captured: March 11, 2026 at ~`23:35` to `23:38` UTC

## Collection Snapshot (live)
- Items: `25`
- Owners: `8`
- Active listings: `4`
- Active auctions: `0`
- Floor: `12.0 XTZ`
- Volume (24h rolling): `53.0 XTZ`
- Volume (total): `134.5 XTZ`

## Today’s Activity (PDT day window)
- New mints: `6`
- Minted token IDs: `19, 20, 21, 22, 23, 24`
- Listing sales: `0`
- Offer sales: `3`
- Offer-sale volume today: `41.0 XTZ`

Offer sales observed in window:
1. `#24` `$fuc` sold `14.5 XTZ` at `2026-03-11T23:19:13Z` (seller `tz1gkf...`, buyer `tz1inP...`)
2. `#20` `$b7u` sold `13.5 XTZ` at `2026-03-11T23:19:01Z` (seller `tz1gkf...`, buyer `tz1inP...`)
3. `#18` `$kl1` sold `13.0 XTZ` at `2026-03-11T21:06:19Z` (seller `tz1gkf...`, buyer `tz1inP...`)

## Actions Executed Today

Accepted offers (completed):
1. Offer `#12179750` for token `#20` at `13.5 XTZ`
   - Tx: `ooU6N6iTH4GPGvtR2cJrtov2HM4Uc3S8RPhCbHvVDMYMWPDqwv3`
2. Offer `#12179751` for token `#24` at `14.5 XTZ`
   - Tx: `op9ynm3ALH5rQQUKJvSt6Yk48Z57nDMFEwLm3bPCCFJQaV1JtB9`

Listed remaining inventory at floor (`12 XTZ`):
1. Token `#19` `$wif` ask `#12585019`
   - Tx: `ooACR8Rh9RyhJzeP23UXepxjejTAekXcq5o2xGrjsGULyArzKDT`
2. Token `#21` `$inz` ask `#12585020`
   - Tx: `onrYZzoFigjF6z1EhGuprGx621qmpsdy8yfVq6ph83SxxjNzRkz`
3. Token `#22` `$zip` ask `#12585022`
   - Tx: `oozXgKo1cLne4BP3NMWK2qnpv3Srrd5hHtDSU531sW65kkyT2Vh`
4. Token `#23` `$itof` ask `#12585023`
   - Tx: `op3zyt6R7X8vVEcQLtyKmFadpA36kobxC7nCFUeJ7JTG7wnpBFC`

Withdrew keep fees from contract to `aesthetic.tez` (`tz1gkf...`):
- Amount: `22.5 XTZ`
- Tx: `oowQqp1vNNySqfUBjRAhtUQLFut4gXJgnFSw9MDY9RoVUfWXKVZ`
- Contract balance after withdrawal: `0`

## Current Open Market State (post-actions)
- Active asks:
1. `#19` `$wif` at `12.0 XTZ` (ask `#12585019`)
2. `#21` `$inz` at `12.0 XTZ` (ask `#12585020`)
3. `#22` `$zip` at `12.0 XTZ` (ask `#12585022`)
4. `#23` `$itof` at `12.0 XTZ` (ask `#12585023`)

- Active offers:
1. `#3` `$3jj` offer `#12179069` bid `100.0 XTZ` (buyer `tz1QRX...`)

## Notes
- Objkt GraphQL row IDs and on-chain IDs differ for asks/offers:
  - `id` = database row id
  - `bigmap_key` = on-chain ask/offer id used by contract entrypoints
- CLI support was updated accordingly so accepts/retracts use on-chain IDs.
