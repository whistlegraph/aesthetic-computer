# nft.aesthetic.computer

Canonical portal for Jeffrey Scudder's NFT record across Ethereum, Tezos,
Bitcoin, and Base. Two audiences: collectors (history, provenance, live
holder view) and the papers research stack (stable JSON endpoints that
papers.aesthetic.computer platters can cite).

Information architecture: **CREATED** (works Jeffrey / Whistlegraph made)
and **COLLECTED** (works by others held in the studio wallets), plus a
network view of wallets holding across collections.

Static site — `index.html` fetches everything at runtime from `./data/`.
No build step, no external JS. Styling follows the papers.aesthetic.computer
conventions (same tokens, Berkeley Mono / YWFT Processing via the
aesthetic.computer webfont CSS, shared `papers-theme` light/dark override).

## Data contract (platter interface)

The files under `data/` are **stable endpoints**. Papers and platters cite
them by URL; do not rename or restructure without versioning.

- `data/catalog.json` —
  `{ generated, snapshotNote, artist, wallets, verifiedAbsent, created,
  collected, external }`.
  - `created[]`: `{ id, name, chain, standard, contract,
    secondaryContract?, mintContracts?, year, supply, status, notes,
    links, dataFile }`; `status` ∈
    `released | returning | planned | to-document`.
  - `collected`: `{ caveat, wallets: [{ name, addr, items: [{ name, qty,
    contract?, group? }], note? }] }` — point-in-time net-balance reading
    of Etherscan history (2026-08-31); partial history, verify before
    citing quantities.
  - `external[]`: same shape as created plus `role: "external-context"`
    and `creator` — collections that are *not* Jeffrey's but explain the
    network (currently a2p-v2, creator `0x1d05cf…5ad6`; its dominant
    holder `0xa741d850…cfcf` also holds a full Ten Whistlegraphs set).
  - `verifiedAbsent[]`: chains checked and found empty
    (Solana, both wallets, zero NFT-like tokens, 2026-08-31).
- `data/rdp-holders.json`, `data/ten-whistlegraphs-holders.json` — holder
  snapshots for created collections;
  `data/a2p-v2-holders.json` — external-context holder snapshot.
  Schema: `{ totalTokens, holders: [{ addr, editions, self, lastActivity,
  days }] }` — `days` is days since the wallet's last on-chain activity at
  snapshot time; `self` marks artist-controlled wallets.

## Card imagery

- **Radical Digital Painting** — `assets/rdp-painting-*.jpg`, static-curated
  at build time from the collection's DO Spaces CDN
  (`radical-digital-painting.nyc3.cdn.digitaloceanspaces.com/thumbnails/1024/`,
  resolved via the local `system/public/rdp.jas.life` painting pages).
  TODO: live tokenURI rendering — blocked while `rdp.whistlegraph.com` is
  down (see To document).
- **The Longest Whistlegraph Ever** — `assets/lwge-title.jpg`, the
  chalkboard title card from the live Rhizome microsite
  (`sites.rhizome.org/the-longest-whistlegraph-ever-so-far`). The on-chain
  tokenURI host `long.whistlegraph.com` is dead, so token media can't be
  pulled from chain (see To document).
- **Ten Whistlegraphs** — live client-side from the Feral File exhibition
  API (`feralfile.com/api/exhibitions/ten-whistlegraphs-thv`, series
  `thumbnailURI` + `/public`).
- **hic et nunc works** — live client-side from tzkt
  (`api.tzkt.io/v1/tokens?firstMinter=tz1gkf8…&contract=KT1RJ6Pb…`),
  preferring `displayUri` when `thumbnailUri` is the generic h=n
  placeholder; ipfs:// resolved via ipfs.io.
- **KidLisp Keeps** — live client-side from tzkt across both mint
  contracts (`contract.in=KT1EcsqR…,KT1BoKMQ…`).
- **Planned collections** — inline SVG placeholders drawn in the palette.
- **Network / collected wallets** — deterministic JS identicons; known
  reverse-ENS wallets (whistlegraph.eth, 4esthetic.eth) try
  `metadata.ens.domains` with identicon fallback.

All live fetches degrade gracefully to skeleton/fallback cells.

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

- **Bitcoin ordinals** — inscription ids from the ordinals.com wallets.
- **screenshots-14 contract** — identify and record.
- **Further Tezos contracts** — whether anything beyond the two KidLisp
  mint contracts (KT1EcsqR…, KT1BoKMQ…) and the h=n corpus matters;
  aesthetic.tez is firstMinter of 222 tokens total.
- **RDP metadata hosting is broken** — the on-chain `tokenURI` for
  0x5381f5…64dd points at `https://rdp.whistlegraph.com/json/<id>.json`,
  and that hostname no longer resolves (checked 2026-08-31). The metadata
  + images survive (DO Spaces bucket `radical-digital-painting`, local
  `system/public/rdp.jas.life`, Wayback `/json/*` captures) — re-point
  DNS or re-host to repair.
- **LWGE metadata hosting is broken** — both contracts' `tokenURI`
  (0x449dc3…1894 minted ×5, 0x7aa86a…2ccc deployed-unminted) point at
  `https://long.whistlegraph.com/<id>.json`, which no longer resolves and
  has no Wayback captures. The 19 GB media archive exists in Dropbox
  (`Whistlegraph/The Longest Whistlegraph Ever (so far) Media
  Archive.zip`) and the Rhizome microsite is still live — re-host to
  repair.

Documented since last pass: a2p-v1 (`0x3892f7…9716`, held ×2–3 by
4esthetic.eth — now in COLLECTED); KidLisp Keeps mint contracts; hic et
nunc corpus; LWGE contracts; Solana verified empty.
