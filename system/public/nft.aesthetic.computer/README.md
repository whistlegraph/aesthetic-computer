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

## Card imagery — reliability model

Every CREATED card with real artwork renders **baked local images first**
(same-origin `assets/*.jpg`, cannot be blocked); live indexer fetches only
*upgrade* those in place when they succeed. Live `<img>`s walk an IPFS
gateway chain via `onerror` (`ipfs.io → dweb.link → w3s.link →
nftstorage.link`, verified working 2026-08-31; `cloudflare-ipfs.com` is
dead, `4everland.io` stalls) and end the chain at the baked local copy.
API fetches get a 12s timeout + one retry.

Root causes found on the live site (2026-08-31):

1. **Feral File API 403s any request carrying an `Origin` header** — every
   cross-origin browser fetch fails while curl succeeds. Ten Whistlegraphs
   is therefore static-only (`assets/ff-*.jpg`, baked from the API's
   series `thumbnailURI` + `/public` server-side).
2. **KidLisp `displayUri` is interactive HTML**, not an image — an `<img>`
   pointed at it never renders. The live loader now uses `thumbnailUri`
   only.
3. **KT1BoKMQ… is a shared minting contract** — without
   `firstMinter=tz1gkf8…` the card showed other artists' tokens. The live
   query now filters.
4. **Single-gateway ipfs.io fragility** — now the 4-gateway chain above.

Sources per card:

- **Radical Digital Painting** — `assets/rdp-painting-*.jpg` (DO Spaces
  CDN via the local `rdp.jas.life` pages). Live tokenURI blocked while
  `rdp.whistlegraph.com` is down (see To document).
- **The Longest Whistlegraph Ever** — `assets/lwge-title.jpg` (Rhizome
  microsite title card; `long.whistlegraph.com` is dead).
- **Ten Whistlegraphs** — `assets/ff-*.jpg`, static-only (FF API CORS,
  above).
- **hic et nunc works** — `assets/hen-*.jpg` baked + live tzkt upgrade
  (`firstMinter` + h=n contract; `displayUri` preferred because h=n
  `thumbnailUri` is usually the generic placeholder).
- **KidLisp Keeps** — `assets/kidlisp-*.jpg` ($cow/$mtz/$roz/$berz
  thumbnails) + live tzkt upgrade (`contract.in` both mint contracts,
  `firstMinter` filtered, `thumbnailUri` only).
- **Bitcoin Ordinals** — no media: verified empty (below).
- **Planned collections** — inline SVG placeholders in the palette.
- **Network / collected wallets** — deterministic JS identicons; known
  reverse-ENS wallets (whistlegraph.eth, 4esthetic.eth) try
  `metadata.ens.domains` with identicon fallback.

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

- **Bitcoin ordinals** — the three known ordsies taproot addresses
  (minter / receiver / test) were checked 2026-08-31 via ordinals.com and
  blockstream.info: **zero inscriptions and zero transaction history
  ever** — the addresses were never funded, so nothing was minted or sent
  onward from them. If inscriptions exist they live in a wallet not yet
  identified; find it. (Hiro's Ordinals API is deprecated — HTTP 410;
  ordinals.com address pages + blockstream.info are the working probes.)
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
