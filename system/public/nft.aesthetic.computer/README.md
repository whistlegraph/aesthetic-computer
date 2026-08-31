# nft.aesthetic.computer

Canonical portal for Jeffrey Scudder's NFT activity across Ethereum, Tezos,
Bitcoin, and Base. Serves two audiences: collectors (history, provenance,
live holder view) and the papers research stack (stable JSON endpoints that
papers.aesthetic.computer platters can cite).

Static site — `index.html` fetches everything at runtime from `./data/`.
No build step, no external JS. Styling follows the papers.aesthetic.computer
conventions (same tokens, Berkeley Mono / YWFT Processing via the
aesthetic.computer webfont CSS, shared `papers-theme` light/dark override).

## Card imagery

- **Radical Digital Painting** — `assets/rdp-painting-*.jpg`, static-curated
  at build time from the collection's DO Spaces CDN
  (`radical-digital-painting.nyc3.cdn.digitaloceanspaces.com/thumbnails/1024/`,
  resolved via the local `system/public/rdp.jas.life` painting pages).
  TODO: live tokenURI rendering — note the on-chain tokenURI host
  `rdp.whistlegraph.com` no longer resolves (see To document).
- **a2p-v2** — `assets/a2p-*.jpg`, static-curated at build time by resolving
  `tokenByIndex`/`tokenURI` on-chain (publicnode RPC) and fetching the IPFS
  images, downscaled to 640px. TODO: live tokenURI rendering.
- **Ten Whistlegraphs** — live client-side from the Feral File exhibition API
  (`feralfile.com/api/exhibitions/ten-whistlegraphs-thv`, series
  `thumbnailURI` + `/public`).
- **KidLisp Keeps** — live client-side from tzkt
  (`api.tzkt.io/v1/tokens?contract=…&limit=4`, ipfs:// resolved via ipfs.io).
- **Planned collections** — inline SVG placeholders drawn in the palette.
- **Network wallets** — deterministic JS identicons; a known reverse-ENS
  wallet (whistlegraph.eth) tries `metadata.ens.domains` with identicon
  fallback.

All live fetches degrade gracefully to skeleton/fallback cells.

## Data contract (platter interface)

The files under `data/` are **stable endpoints**. Papers and platters cite
them by URL; do not rename or restructure without versioning.

- `data/catalog.json` — every collection: `{ generated, snapshotNote,
  collections: [{ id, name, chain, standard, contract, year, supply,
  status, notes, links, dataFile }] }`. `status` is one of
  `released | returning | planned | to-document`.
- `data/rdp-holders.json`, `data/ten-whistlegraphs-holders.json`,
  `data/a2p-v2-holders.json` — holder snapshots:
  `{ totalTokens, holders: [{ addr, editions, self, lastActivity, days }] }`
  where `days` is days since the wallet's last on-chain activity at
  snapshot time and `self` marks artist-controlled wallets.

## Refreshing snapshots

```
cd whistlegraph/codes
ETHERSCAN_API_KEY=... node bin/holders-probe.mjs <erc721-address> > out.json
```

The key lives in the vault (`reference_blockchain_api_keys` — encrypted
`.gpg`). The probe defaults to the Ten Whistlegraphs contract; pass any
ERC-721 address. After refreshing, update `generated` and `snapshotNote`
in `data/catalog.json`.

## Going live

Needs DNS + Caddy for the `nft` subdomain on lith — follow the "lith DNS
recipe" note (`tls { protocols }` gotcha). Serve this directory as the
static root. Do not edit lith's Caddyfile casually; that's a deliberate
deploy step, not part of editing this site.

## To document

- **a2p-v1** — contract address unconfirmed (catalog entry has
  `"status": "to-document"`).
- **Bitcoin ordinals** — inscription ids from the ordinals.com wallets.
- **KidLisp Keeps mint contract** — catalog lists the FA2 marketplace
  contract (`KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`, admin aesthetic.tez);
  the underlying mint contract still needs recording, plus a Tezos holder
  snapshot (tzkt API — the Etherscan probe doesn't cover it).
- **RDP metadata hosting is broken** — the on-chain `tokenURI` for
  0x5381f5…64dd points at `https://rdp.whistlegraph.com/json/<id>.json`,
  and that hostname no longer resolves (checked 2026-08-31). Marketplaces
  showing cached art mask it. The metadata + images still exist (DO Spaces
  bucket `radical-digital-painting`, local `system/public/rdp.jas.life`;
  Wayback has `/json/*` captures) — re-point DNS or re-host to repair.
