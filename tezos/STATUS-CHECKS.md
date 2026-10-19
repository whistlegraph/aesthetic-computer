# Tezos Status Checks

Broad Tezos check-ins ("how's our Tezos going?", "our Tezos stuff") cover
**HEN daily posts, Keeps, wallet balances, and braincell payments**. Always
include the HEN dailies, even when the user names no collection.

- **HEN dailies:** consult `marketing/podcast/SCORE.md`, jasellite's
  `marketing/podcast/out/daily/*.token.json` receipts and `~/.podcast-daily.log`,
  then verify current mints, listings and purchases on TzKT. Report the latest
  post with its link, mint time, actual asking price and sale status; summarize
  recent purchases and any missed dates. Read prices from receipts and chain,
  not script defaults. `daily_token_status` in `toolchain/mcp/crypto-mcp.mjs`
  provides discovery when available. Match daily posts to receipts: other HEN
  mints by aesthetic.tez are not necessarily daily posts.
- **HEN paintings:** @jeffrey's finished paintings (nopaint "Done" and every
  other sealed painting) are minted as hic et nunc 1/1s by
  `tezos/hen-paintings.mjs`, tagged `ac-painting-<code>` so the chain itself
  says what is minted. `status` lists the newest with their OBJKT ids;
  receipts are `tezos/out/hen-paintings/<code>.token.json`; the queue is the
  `poll` command on a ten-minute launchd timer
  (`tezos/launchd/computer.aesthetic.hen-paintings.plist`, log in
  `~/.local/share/ac/hen-paintings/poll.log`), which only takes paintings
  finished after `out/hen-paintings/.watermark`. Tell daily posts and
  paintings apart by tag.
- **Sales:** count only applied marketplace settlements matched to the token
  transfer in the same operation. Exclude burns, gifts and backtracked calls;
  outgoing transfers alone are not sales. Distinguish posts, editions sold and
  unique buying wallets. Verify helper totals against the chain.
- **Keeps:** use the flow below. A quiet Keeps market does not establish that
  HEN posts or all Tezos activity are quiet.
- **Wallets and payments:** check current balances and production availability;
  distinguish a working checkout from verified completed payments.

## Keeps Market Stats (Tezos / Objkt)

Use this flow for live Keeps market checks (`jas.tez`, `keeps.tez`, contract-level stats).

**Actual sales (price + piece + buyer):** `node tezos/keeps-sales.mjs` (`--limit=N`,
`--json`, `--network=ghostnet`). Keeps sell through an objkt-style marketplace
(`ask` → `fulfill_ask`) at `KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X`, which **objkt.com's
own GraphQL does not index** — so the `listing_sale`/`offer_sale` queries below return
0. The script reads sales straight from TzKT (a sale = a keeps `transfer` whose sender
is the marketplace; price = the buyer's `fulfill_ask` amount). Prefer it over the objkt
sales query for "what sold / latest sale."

```bash
# 1) Resolve domains + active Keeps contract
curl -sS "https://api.tzkt.io/v1/domains?name=jas.tez" | jq '.[0] | {name,address,owner,reverse}'
curl -sS "https://api.tzkt.io/v1/domains?name=keepz.tez" | jq '.[0] // "not-registered"'
curl -sS "https://api.tzkt.io/v1/domains?name=keeps.tez" | jq '.[0] | {name,address,owner,reverse}'
curl -sS "https://aesthetic.computer/api/keeps-config?network=mainnet" | jq .
```

```bash
# 2) Collection snapshot (Objkt v3 GraphQL, values are mutez)
CONTRACT="KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB"
read -r -d '' Q <<'EOF'
query ($contract: String!) {
  fa(where: { contract: { _eq: $contract } }) {
    contract
    name
    items
    owners
    active_listing
    active_auctions
    floor_price
    volume_24h
    volume_total
  }
}
EOF
curl -sS "https://data.objkt.com/v3/graphql" \
  -H "content-type: application/json" \
  --data "$(jq -n --arg q "$Q" --arg contract "$CONTRACT" '{query:$q,variables:{contract:$contract}}')" \
  | jq '.data.fa[0] | . + {floor_price_xtz:(.floor_price/1000000),volume_24h_xtz:(.volume_24h/1000000),volume_total_xtz:(.volume_total/1000000)}'
```

```bash
# NOTE: for Objkt `offer_active` / `listing_active` rows:
# - `id` is the database row id
# - `bigmap_key` is the on-chain offer/ask id used by contract entrypoints
# Use `bigmap_key` for fulfill/retract calls.
read -r -d '' IDS_Q <<'EOF'
query ($contract: String!) {
  offer_active(where: { fa_contract: { _eq: $contract } }, order_by: { price_xtz: desc }, limit: 20) {
    id
    bigmap_key
    price_xtz
    token { token_id name }
  }
}
EOF
curl -sS "https://data.objkt.com/v3/graphql" \
  -H "content-type: application/json" \
  --data "$(jq -n --arg q "$IDS_Q" --arg contract "$CONTRACT" '{query:$q,variables:{contract:$contract}}')" \
  | jq '.data.offer_active'
```

```bash
# 3) "Today" window in Los Angeles (matches local day conversations)
START="$(TZ=America/Los_Angeles date -d 'today 00:00' -u +%Y-%m-%dT%H:%M:%SZ)"
END="$(TZ=America/Los_Angeles date -d 'tomorrow 00:00' -u +%Y-%m-%dT%H:%M:%SZ)"
echo "$START -> $END"

# Mint count today (from=null means mint)
curl -sS "https://api.tzkt.io/v1/tokens/transfers?token.contract=$CONTRACT&timestamp.ge=$START&timestamp.lt=$END&limit=200" \
  | jq '[.[] | select(.from==null)] | {mint_count:length, token_ids:map(.token.tokenId)}'
```

```bash
# 4) Sales today (listing_sale + offer_sale)
read -r -d '' SALES_Q <<'EOF'
query ($contract: String!, $start: timestamptz!, $end: timestamptz!) {
  listing_sale(
    where: {
      _and: [
        { token: { fa_contract: { _eq: $contract } } }
        { timestamp: { _gte: $start, _lt: $end } }
      ]
    }
    order_by: { timestamp: desc }
    limit: 200
  ) { id timestamp price_xtz seller_address buyer_address token { token_id name } }
  offer_sale(
    where: {
      _and: [
        { token: { fa_contract: { _eq: $contract } } }
        { timestamp: { _gte: $start, _lt: $end } }
      ]
    }
    order_by: { timestamp: desc }
    limit: 200
  ) { id timestamp price_xtz seller_address buyer_address token { token_id name } }
}
EOF
curl -sS "https://data.objkt.com/v3/graphql" \
  -H "content-type: application/json" \
  --data "$(jq -n --arg q "$SALES_Q" --arg contract "$CONTRACT" --arg start "$START" --arg end "$END" '{query:$q,variables:{contract:$contract,start:$start,end:$end}}')" \
  | jq '{listing_sales_count:(.data.listing_sale|length),offer_sales_count:(.data.offer_sale|length),volume_xtz:((([.data.listing_sale[].price_xtz]|add // 0)+([.data.offer_sale[].price_xtz]|add // 0))/1000000),sales:(.data.listing_sale + .data.offer_sale | sort_by(.timestamp))}'
```
