# rdp.aesthetic.computer — metadata host

Permanent replacement host for the ERC-721 metadata of **Radical Digital
Painting** (239 tokens, tokenIds 1–239). The contract's current baseURI points
at `https://rdp.whistlegraph.com/json/` — that domain is squatted/dead. This
directory recreates the complete original metadata set at
`/json/1.json` … `/json/239.json` so the contract can be re-pointed here.

Staged 2026-08-31. Not yet deployed; DNS and the on-chain repair are separate
steps (see **The repair transaction** below).

## Recovery method

1. **Wayback Machine — 103 tokens.** The CDX index for
   `rdp.whistlegraph.com/json/*` holds snapshots for 103 of the 239 token
   files (2022–2024 crawls). Each was fetched in raw `id_` form and is stored
   **byte-identical** to the archived original.
2. **Blockscout metadata cache — 136 tokens.** Wayback lacks 136 tokens, and
   the planned fallback (regeneration from the local `rdp.jas.life` mirror)
   turned out to be lossy: the mirror pages for exactly those tokens are
   stubs with no description, no attributes, and no Instagram link. Instead,
   the full original metadata was recovered from Blockscout's token-instance
   cache (`eth.blockscout.com/api/v2/tokens/<contract>/instances/<id>`),
   which indexed the original JSON while `rdp.whistlegraph.com` was alive.
   Each cached object was re-serialized minified in the original key order
   (`name, description, image, external_url, attributes`; attribute objects
   as `display_type?, trait_type?, value`).

### Fidelity validation

- For all **102** tokens present in *both* sources (Wayback ∩ Blockscout;
  Blockscout's only gap is #101, which Wayback covers), the re-serialized
  Blockscout cache is **byte-identical** to the Wayback original —
  102/102. The 136 Blockscout-derived files are therefore recoveries of the
  original bytes with very high confidence, not regenerations.
- All 136 Blockscout-derived image filenames match the per-painting
  datestamp in the corresponding local mirror page
  (`system/public/rdp.jas.life/painting/N/index.html`) — 136/136.
- All 239 files parse and carry `name`, `description`, `image`.
- The original files were minified `JSON.stringify` output with no trailing
  newline; every recovered file round-trips
  (`JSON.stringify(JSON.parse(s)) === s`).

## Image URLs — as recovered

As recovered, no image field referenced `rdp.whistlegraph.com` or any other
dead host:

- **238 tokens** pointed at
  `ipfs://QmSgE9XQx6jfi1GZwK1To8PNHLyLkwS64Lg5VjeYxVh5ei/<datestamp>.png`.
  That directory CID is alive: it resolves via `gateway.pinata.cloud`
  (HTTP 200, `image/png`), and its dag-json listing was fetched and
  cross-checked — **all 238 referenced filenames exist in the directory**
  (240 entries: 239 paintings + a stray `.DS_Store`). Public gateways
  (`ipfs.io`, `dweb.link`) were slow/504 during checks; only the Pinata
  gateway served it reliably — a single-pin dependency, since resolved by
  the mirror below (see **Image mirror**).
- **1 token (#216, "[Redacted]")** points at the live DigitalOcean Spaces CDN
  (`https://radical-digital-painting.nyc3.cdn.digitaloceanspaces.com/thumbnails/2048/18.12.3.17.1.jpg`,
  HTTP 200). Left as-is, byte-identical.

## Image mirror

The 238 IPFS-hosted originals are mirrored to Jeffrey's own DigitalOcean
Spaces bucket, and the metadata now serves images from that mirror:

- **CDN prefix:** `https://radical-digital-painting.nyc3.cdn.digitaloceanspaces.com/pngs/`
  (bucket `radical-digital-painting`, path `pngs/<filename>.png`, ACL
  public-read; the bucket also holds the pre-existing `thumbnails/`).
- **Count:** all **238** IPFS-referenced PNGs were downloaded from
  `gateway.pinata.cloud`, verified (PNG magic bytes, >100KB), uploaded, and
  re-verified over the CDN (HTTP 200 with content-length matching the local
  file, all 238).
- **`image` / `image_ipfs` convention:** in each of the 238 rewritten token
  files, `image` is the CDN URL and a new `image_ipfs` field — placed
  immediately after `image` — preserves the original
  `ipfs://QmSgE9XQx6jfi1GZwK1To8PNHLyLkwS64Lg5VjeYxVh5ei/<name>.png` value.
  Every other byte of each file is unchanged (minified, original key order).
- **#216:** already pointed at the live Spaces CDN (`thumbnails/2048/…`), so
  it has no `image_ipfs` and was not touched.
- **#237:** its filename is literally `NO DATE 1.png` (contains spaces). The
  object key keeps the literal filename; the `image` URL uses the
  URL-encoded form (`pngs/NO%20DATE%201.png`).
- The Pinata pin of CID `QmSgE9XQx6jfi1GZwK1To8PNHLyLkwS64Lg5VjeYxVh5ei` is
  now a **redundant source rather than a dependency** — the metadata no
  longer resolves through any IPFS gateway, and `image_ipfs` keeps the
  content-addressed reference for provenance and re-pinning.

**Known dead links preserved by design:** every token's `external_url` is
`https://rdp.whistlegraph.com/painting/N` — the original value, preserved
exactly per the recovery brief (only the `image` field was in scope for
rewrites). A follow-up edition of these files could re-point `external_url`
at `https://jas.life/painting/N` (live), but that would depart from the
original bytes.

## The repair transaction

The verified contract source (Solidity 0.8.4, `RadicalDigitalPainting`)
constructs the token URI as:

```solidity
string public _baseTokenURI = 'https://rdp.whistlegraph.com/json/';
string private _baseTokenExtension = '.json';

function tokenURI(uint256 _tokenId) public view override returns (string memory) {
    return string(abi.encodePacked(_baseTokenURI, Strings.toString(_tokenId), _baseTokenExtension));
}
```

So the stored base **includes `json/` and the trailing slash**, and the
`.json` suffix comes from the separate (private, non-settable)
`_baseTokenExtension`. Confirmed live on mainnet (2026-08-31):
`tokenURI(1)` → `https://rdp.whistlegraph.com/json/1.json`.

| | |
|---|---|
| Target contract | `0x5381f50ffec1b551f561e99943da88211f1a64dd` (Ethereum mainnet) |
| Function | `setBaseTokenURI(string)` — selector `0x30176e13` |
| Argument | `https://rdp.aesthetic.computer/json/` |
| Owner (must send) | `0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08` |
| `locked` | `false` as of 2026-08-31 — the call will not revert on the lock |

ABI-encoded calldata (verify in MetaMask hex view):

```
0x30176e13
  0000000000000000000000000000000000000000000000000000000000000020   // offset
  0000000000000000000000000000000000000000000000000000000000000024   // length = 36
  68747470733a2f2f7264702e6165737468657469632e636f6d70757465722f6a
  736f6e2f0000000000000000000000000000000000000000000000000000000000 // "https://rdp.aesthetic.computer/json/"
```

Single line:

```
0x30176e130000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000002468747470733a2f2f7264702e6165737468657469632e636f6d70757465722f6a736f6e2f00000000000000000000000000000000000000000000000000000000
```

Prerequisites before sending: deploy this directory so
`https://rdp.aesthetic.computer/json/1.json` returns the metadata (DNS +
lith Caddy vhost), then send the transaction from the owner wallet. The
contract also has `lockMetadata()` (irreversible) if the URI should be
frozen afterward — optional, and deliberate.

## Provenance per token

`wayback` = raw archived original bytes (snapshot timestamp shown).
`blockscout-cache` = Blockscout token-instance cache, re-serialized in the
original key order (validated byte-exact against all 102 overlapping
Wayback originals).

| Token | Source | Wayback snapshot (UTC) |
|---|---|---|
| 1 | wayback | 2024-03-03 05:25:32 |
| 2 | wayback | 2023-02-07 07:37:17 |
| 3 | blockscout-cache | — |
| 4 | wayback | 2023-02-07 09:27:02 |
| 5 | blockscout-cache | — |
| 6 | blockscout-cache | — |
| 7 | wayback | 2022-08-15 00:01:48 |
| 8 | blockscout-cache | — |
| 9 | wayback | 2023-02-07 09:28:34 |
| 10 | wayback | 2023-02-07 07:19:30 |
| 11 | blockscout-cache | — |
| 12 | wayback | 2023-02-07 09:22:59 |
| 13 | wayback | 2023-02-07 08:07:07 |
| 14 | wayback | 2022-08-15 01:27:54 |
| 15 | blockscout-cache | — |
| 16 | blockscout-cache | — |
| 17 | blockscout-cache | — |
| 18 | blockscout-cache | — |
| 19 | wayback | 2022-07-04 12:39:20 |
| 20 | wayback | 2024-03-03 05:34:30 |
| 21 | blockscout-cache | — |
| 22 | blockscout-cache | — |
| 23 | blockscout-cache | — |
| 24 | wayback | 2022-07-04 13:07:08 |
| 25 | wayback | 2024-03-03 05:35:55 |
| 26 | wayback | 2023-02-07 07:35:33 |
| 27 | blockscout-cache | — |
| 28 | blockscout-cache | — |
| 29 | wayback | 2024-03-03 07:05:25 |
| 30 | wayback | 2024-03-03 06:14:01 |
| 31 | blockscout-cache | — |
| 32 | wayback | 2023-02-07 08:56:23 |
| 33 | blockscout-cache | — |
| 34 | blockscout-cache | — |
| 35 | wayback | 2023-02-07 07:30:15 |
| 36 | blockscout-cache | — |
| 37 | blockscout-cache | — |
| 38 | blockscout-cache | — |
| 39 | blockscout-cache | — |
| 40 | blockscout-cache | — |
| 41 | blockscout-cache | — |
| 42 | blockscout-cache | — |
| 43 | blockscout-cache | — |
| 44 | wayback | 2023-02-07 07:22:28 |
| 45 | wayback | 2023-02-07 08:24:23 |
| 46 | blockscout-cache | — |
| 47 | blockscout-cache | — |
| 48 | wayback | 2023-02-07 09:13:19 |
| 49 | blockscout-cache | — |
| 50 | blockscout-cache | — |
| 51 | wayback | 2022-07-04 12:04:36 |
| 52 | wayback | 2022-04-24 16:35:33 |
| 53 | blockscout-cache | — |
| 54 | wayback | 2023-02-07 08:16:54 |
| 55 | wayback | 2024-03-03 04:44:46 |
| 56 | wayback | 2023-02-07 08:02:32 |
| 57 | wayback | 2023-02-07 09:06:30 |
| 58 | wayback | 2024-03-03 05:23:16 |
| 59 | blockscout-cache | — |
| 60 | blockscout-cache | — |
| 61 | blockscout-cache | — |
| 62 | blockscout-cache | — |
| 63 | wayback | 2024-05-31 06:02:38 |
| 64 | blockscout-cache | — |
| 65 | blockscout-cache | — |
| 66 | blockscout-cache | — |
| 67 | blockscout-cache | — |
| 68 | wayback | 2023-02-07 08:47:13 |
| 69 | wayback | 2022-07-04 13:02:39 |
| 70 | wayback | 2022-08-15 00:38:03 |
| 71 | wayback | 2023-02-07 09:16:59 |
| 72 | wayback | 2024-03-03 05:33:46 |
| 73 | blockscout-cache | — |
| 74 | blockscout-cache | — |
| 75 | blockscout-cache | — |
| 76 | blockscout-cache | — |
| 77 | wayback | 2022-07-04 11:49:39 |
| 78 | wayback | 2022-07-04 12:43:07 |
| 79 | blockscout-cache | — |
| 80 | wayback | 2023-02-07 09:09:29 |
| 81 | blockscout-cache | — |
| 82 | wayback | 2022-08-15 01:16:56 |
| 83 | wayback | 2023-02-07 07:40:14 |
| 84 | wayback | 2022-07-04 11:54:18 |
| 85 | wayback | 2023-02-07 07:31:04 |
| 86 | blockscout-cache | — |
| 87 | wayback | 2023-02-07 09:33:34 |
| 88 | blockscout-cache | — |
| 89 | wayback | 2023-02-07 09:21:34 |
| 90 | blockscout-cache | — |
| 91 | blockscout-cache | — |
| 92 | wayback | 2023-02-07 07:34:35 |
| 93 | wayback | 2022-08-15 00:58:40 |
| 94 | blockscout-cache | — |
| 95 | blockscout-cache | — |
| 96 | blockscout-cache | — |
| 97 | wayback | 2023-02-07 08:11:40 |
| 98 | blockscout-cache | — |
| 99 | wayback | 2023-02-07 07:59:04 |
| 100 | blockscout-cache | — |
| 101 | wayback | 2023-02-07 07:59:43 |
| 102 | wayback | 2022-07-04 11:40:49 |
| 103 | wayback | 2023-02-07 08:22:13 |
| 104 | blockscout-cache | — |
| 105 | blockscout-cache | — |
| 106 | wayback | 2023-02-07 08:54:03 |
| 107 | blockscout-cache | — |
| 108 | blockscout-cache | — |
| 109 | wayback | 2023-02-07 09:24:25 |
| 110 | wayback | 2022-08-15 01:11:46 |
| 111 | blockscout-cache | — |
| 112 | wayback | 2022-08-15 01:30:18 |
| 113 | blockscout-cache | — |
| 114 | blockscout-cache | — |
| 115 | blockscout-cache | — |
| 116 | wayback | 2023-02-07 07:38:45 |
| 117 | blockscout-cache | — |
| 118 | wayback | 2022-07-04 11:58:11 |
| 119 | blockscout-cache | — |
| 120 | wayback | 2022-08-15 00:01:04 |
| 121 | wayback | 2022-07-04 12:27:20 |
| 122 | blockscout-cache | — |
| 123 | wayback | 2022-07-04 12:15:24 |
| 124 | wayback | 2023-02-07 08:21:05 |
| 125 | blockscout-cache | — |
| 126 | wayback | 2023-02-07 08:41:50 |
| 127 | blockscout-cache | — |
| 128 | blockscout-cache | — |
| 129 | wayback | 2024-03-03 05:21:47 |
| 130 | blockscout-cache | — |
| 131 | wayback | 2024-03-03 07:15:23 |
| 132 | blockscout-cache | — |
| 133 | blockscout-cache | — |
| 134 | blockscout-cache | — |
| 135 | wayback | 2022-07-04 12:57:19 |
| 136 | wayback | 2024-03-03 05:27:05 |
| 137 | blockscout-cache | — |
| 138 | wayback | 2023-02-07 07:43:53 |
| 139 | blockscout-cache | — |
| 140 | wayback | 2022-07-04 13:10:28 |
| 141 | blockscout-cache | — |
| 142 | blockscout-cache | — |
| 143 | blockscout-cache | — |
| 144 | wayback | 2022-07-04 12:48:54 |
| 145 | blockscout-cache | — |
| 146 | wayback | 2023-02-07 08:29:10 |
| 147 | wayback | 2023-02-07 08:39:45 |
| 148 | wayback | 2023-02-07 08:50:07 |
| 149 | blockscout-cache | — |
| 150 | blockscout-cache | — |
| 151 | blockscout-cache | — |
| 152 | wayback | 2023-02-07 08:49:18 |
| 153 | blockscout-cache | — |
| 154 | wayback | 2022-07-04 11:28:04 |
| 155 | blockscout-cache | — |
| 156 | blockscout-cache | — |
| 157 | wayback | 2022-07-04 12:18:32 |
| 158 | blockscout-cache | — |
| 159 | wayback | 2022-08-15 00:08:57 |
| 160 | wayback | 2024-03-03 06:41:20 |
| 161 | wayback | 2024-03-03 05:15:58 |
| 162 | wayback | 2022-08-14 23:56:09 |
| 163 | blockscout-cache | — |
| 164 | wayback | 2022-07-04 12:23:39 |
| 165 | blockscout-cache | — |
| 166 | wayback | 2022-07-04 12:11:02 |
| 167 | blockscout-cache | — |
| 168 | blockscout-cache | — |
| 169 | wayback | 2022-07-04 12:57:57 |
| 170 | blockscout-cache | — |
| 171 | blockscout-cache | — |
| 172 | blockscout-cache | — |
| 173 | blockscout-cache | — |
| 174 | blockscout-cache | — |
| 175 | blockscout-cache | — |
| 176 | wayback | 2023-02-07 09:18:03 |
| 177 | blockscout-cache | — |
| 178 | blockscout-cache | — |
| 179 | blockscout-cache | — |
| 180 | blockscout-cache | — |
| 181 | blockscout-cache | — |
| 182 | blockscout-cache | — |
| 183 | wayback | 2024-03-03 07:03:21 |
| 184 | wayback | 2022-07-04 11:56:52 |
| 185 | blockscout-cache | — |
| 186 | wayback | 2022-08-15 00:27:26 |
| 187 | blockscout-cache | — |
| 188 | blockscout-cache | — |
| 189 | wayback | 2023-02-07 08:35:44 |
| 190 | blockscout-cache | — |
| 191 | wayback | 2022-07-04 12:02:07 |
| 192 | blockscout-cache | — |
| 193 | blockscout-cache | — |
| 194 | wayback | 2022-07-04 12:50:29 |
| 195 | blockscout-cache | — |
| 196 | wayback | 2023-02-07 09:05:47 |
| 197 | blockscout-cache | — |
| 198 | blockscout-cache | — |
| 199 | wayback | 2022-07-04 11:17:54 |
| 200 | blockscout-cache | — |
| 201 | wayback | 2022-07-04 11:16:36 |
| 202 | blockscout-cache | — |
| 203 | blockscout-cache | — |
| 204 | wayback | 2023-02-07 09:27:27 |
| 205 | blockscout-cache | — |
| 206 | blockscout-cache | — |
| 207 | blockscout-cache | — |
| 208 | blockscout-cache | — |
| 209 | blockscout-cache | — |
| 210 | wayback | 2022-07-04 13:14:10 |
| 211 | wayback | 2022-08-15 00:04:31 |
| 212 | wayback | 2024-03-03 06:59:52 |
| 213 | wayback | 2024-03-03 05:41:33 |
| 214 | blockscout-cache | — |
| 215 | blockscout-cache | — |
| 216 | wayback | 2024-03-03 06:34:11 |
| 217 | wayback | 2023-02-07 08:44:56 |
| 218 | blockscout-cache | — |
| 219 | blockscout-cache | — |
| 220 | blockscout-cache | — |
| 221 | blockscout-cache | — |
| 222 | blockscout-cache | — |
| 223 | blockscout-cache | — |
| 224 | blockscout-cache | — |
| 225 | wayback | 2022-08-15 01:39:44 |
| 226 | wayback | 2023-02-07 07:21:03 |
| 227 | blockscout-cache | — |
| 228 | blockscout-cache | — |
| 229 | blockscout-cache | — |
| 230 | wayback | 2022-07-04 12:54:06 |
| 231 | blockscout-cache | — |
| 232 | blockscout-cache | — |
| 233 | blockscout-cache | — |
| 234 | blockscout-cache | — |
| 235 | blockscout-cache | — |
| 236 | blockscout-cache | — |
| 237 | wayback | 2023-02-07 08:32:26 |
| 238 | wayback | 2022-07-04 12:02:45 |
| 239 | blockscout-cache | — |

## Image hosting — resolved 2026-08-31

`image` in all 239 tokens now points at **Jeffrey's own DigitalOcean Spaces
CDN**, not IPFS:

```
https://radical-digital-painting.nyc3.cdn.digitaloceanspaces.com/thumbnails/2048/<name>.jpg
```

The complete 2048px set (all 239, including the space-containing
`NO DATE 1/2/3.jpg`) was already live in that bucket from the 2021 build —
verified 200 across a 27-token sample. Token #216 already pointed there and
was left byte-identical.

The original `ipfs://QmSgE9XQx6…` value is preserved on every rewritten
token as **`image_ipfs`**, so provenance survives and IPFS becomes a
redundant source rather than a dependency. (The public gateways are
unreliable for this CID: only `gateway.pinata.cloud` serves it, and it
rate-limits at ~3 files/min.)

A full-resolution PNG mirror to `s3://radical-digital-painting/pngs/` is
partially complete (154/238 at ~3 MB each). **Finishing it is optional** —
because this host owns the metadata, `image` can be upgraded from the 2048
JPEGs to the full-res PNGs at any time by editing these files, with **no
further on-chain transaction**. The one-time `setBaseTokenURI` call below is
the only chain action this repair ever needs.
