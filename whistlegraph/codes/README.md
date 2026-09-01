# whistlegraph/codes

The `WhistlegraphCodes` ERC-721 — the token surface of the Funding Codes
architecture (RFC: `papers/arxiv-funding-codes/`, proposal:
`reports/whistlegraph-org-funding-codes.md`).

The code IS the token: a bare lowercase string (`imab`, `grow`, `ppl`) packs
left-aligned into a bytes32 and that word is the tokenId. Same string = URL
slug (`whistlegraph.org/imab`) = ENS subname (`imab.whistlegraph.eth`).
No `$` sigil — that belongs to KidLisp and its Tezos story.

## Layout

- `src/WhistlegraphCodes.sol` — self-contained ERC-721 + EIP-2981 (10%
  default royalty), owner-published codes, payable mint, `mintTo` for
  first-edition claims and the reserve set, `ownerOfCode(string)` for
  funding-code drop tooling. No external dependencies; every line reviewable.
- `test/WhistlegraphCodes.t.sol` — 21 Foundry tests (bijection, publish/
  mint/payment, transfers/approvals, safe-receiver, royalties, withdraw,
  interfaces, owner handoff). `forge test` — all green as of scaffold.
- `script/Deploy.s.sol`, `script/PublishTen.s.sol` — deployment + seeding
  the Ten Whistlegraphs codes (`imab l8ly grow idni ppl wiyh lonr sdog w0w
  puzz`).
- `lib/forge-std` — vendored plain clone (NOT a submodule; keep it out of
  the AC repo index).

## Deploy path

1. **Base Sepolia rehearsal**: `forge script script/Deploy.s.sol
   --rpc-url https://sepolia.base.org --broadcast` with `PRIVATE_KEY` from
   the vault wallet export (never stored here). Mint a couple of codes,
   check them on the testnet explorer.
2. **Base mainnet**: same with `https://mainnet.base.org`; deployer should
   be whistlegraph.eth (0x238c9c645c6EE83d4323A2449C706940321a0cBf), which
   becomes owner + royalty receiver.
3. `PublishTen` at the chosen price; verify on Basescan
   (`forge verify-contract`).

## OrgStar — the patronage sibling

`src/OrgStar.sol` — the ORGSTARS open edition (RFC: `ORGSTARS-RFC.md`). Same
house style: self-contained, no libraries, ERC-721 + EIP-2981. Flat price, no
cap, no utility; a star does nothing but shine.

The art is **fully on-chain** — `tokenURI` returns a base64 data URI wrapping
an SVG built from the *minting* wallet, so there is no host to rot the way
`rdp.whistlegraph.com` did, and no IPFS pin to keep alive. The birth wallet is
recorded at mint and never changes, so a star keeps its face after it trades.

`whistlegraph.org/stars` draws the same sky from mint logs using the same
FNV-1a hash of the same lowercase `0x…` string. `test_StarHashMatchesThePage`
pins the three reference values the page produces — **if that test ever fails,
the sky and the chain have drifted apart.**

- `test/OrgStar.t.sol` — 24 Foundry tests (minting, open edition, birth-wallet
  permanence, page/chain hash agreement, self-contained art, constellation
  year boundaries, transfers/approvals, safe-receiver, royalties, withdraw,
  interfaces, owner handoff). `forge test` — 46 green across both suites.
- `script/DeployStar.s.sol` — `STAR_PRICE` (default 0.01 ether), prints the
  address and deploy block to paste into `stars.html`.
- Runtime size 11,745 B — roughly 3M gas, about **$0.04** on Base at the
  0.005 gwei base fee measured 2026-09-01.

After deploying, set `contract` and `deployBlock` in the `CHAINS` block of
`system/public/whistlegraph.org/stars.html`. Until then the page renders an
honest empty sky and says the contract is not deployed.

## Still to build (next passes)

- `tokenURI` endpoint: `https://whistlegraph.org/api/codes/{code}` serving
  ERC-721 metadata JSON from the site's `graphs.json` (1,084 coded works)
  + canonical score image. Needs a lith route for the whistlegraph.org host.
- Mint page on `whistlegraph.org/{code}`: wallet-connect + `mint(code)`
  with `priceOf` read. Static JS; the site is static files on lith.
- ENS: `url`/avatar records on whistlegraph.eth (mainnet gas needed), then
  per-code subnames — evaluate a CCIP-read offchain resolver served by lith
  so subnames are free.
- Funding-code drops: per-release claim + 0xSplits wiring (separate
  contracts; this one stays minimal).

## Human steps before mainnet

- Fund whistlegraph.eth: ~$50 ETH mainnet gas (ENS) + ~$20 on Base
  (deploys + publishes).
- builderscore.xyz signup with the same wallet (Base Builder Rewards).
- Per-transaction approval on every broadcast — nothing signs unattended.
