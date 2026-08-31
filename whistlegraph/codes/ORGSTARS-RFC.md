# ORGSTARS — RFC addendum

> Star the org. A purchasable star on whistlegraph.org: pay a small amount of
> ETH, your wallet joins the site's night sky. Pure patronage — a star does
> nothing but shine. Drafted 2026-08-31 as an addendum to the Funding Codes
> RFC (`papers/arxiv-funding-codes`); same boundaries, different genus:
> codes are scarce collectibles, stars are open patronage.

## The object

One ERC-721 on Base: `ORGSTAR`.

- **Open edition, flat price ~0.01 ETH** (~$25). No supply cap, no tiers,
  no auction, no roadmap. Mint any number; more stars shine brighter.
- **Fully on-chain art**: `tokenURI` returns an SVG star generated
  deterministically from the minting wallet + token id — hue, point count,
  and twinkle phase derived from the address. No metadata server, no IPFS,
  nothing to host or lose; every marketplace and wallet renders it.
- **Annual constellations**: each calendar year is an epoch in the metadata
  ("Constellation of 2026"). Gives future years a reason to exist and
  early stars a quiet vintage.
- **Withdrawals** to `whistlegraph.eth`. No other mechanics. The contract
  should be boring enough to read in one sitting (~150 lines).

## The sky

`whistlegraph.org/stars` renders the constellation live from chain events
(RPC read of Transfer logs — no backend):

- Each star = one token, positioned deterministically from its wallet;
  multiple stars from one wallet cluster into a brighter formation.
- Hover: ENS name or short address. **Display denylist required** before
  launch (ENS names render on our page).
- The page doubles as the site's wallet rail — the connect flow built here
  is the same one the Codes mint page needs two weeks later.

## Why it fits (from the 2026-08-31 research sweeps)

- The peer field's winning pattern is *relational mechanics* — tokens that
  do something between holder and work. A star inscribes the supporter
  into the site itself.
- The Robak datum: ~$27, narrative-first, community-ops drops clear fast
  in 2026. Stars are the sub-$30 ask for the audience that will never buy
  art — including 2.6M TikTok followers and the RDP long tail.
- **Stars are the allowlist machine for Codes.** Star-holders snapshot at
  drop-minus-48h = the collector-first window. The warm list stops being
  Etherscan archaeology and becomes self-identified.

## Marketing to the collector base (probe tiers, vault
`reports/2026-08-31-collector-network-probe.md`)

1. **Personal notes (6 demonstrated 2026 buyers)** — star as the soft ask,
   Codes preview as the reason: "the sky opens now; the codes open to
   star-holders first."
2. **Announce list (~36 warm wallets ≤90d)** — one email/DM: site, sky,
   date.
3. **Public** — IG + site + podcast episode narrating the mechanic (the
   podcast markets the mint; the mint is never the podcast).
4. Premiere live-mint (Sept 24) mentions both stars and codes on stage.

## Boundaries

Patronage, never investment. A star carries no value share, no access
promise beyond the allowlist window, no future utility language. Cleaner
than Codes by design; if anything in this document starts sounding like a
security, delete it.

## Build + sequence

- Contract + page ≈ a weekend; Base Sepolia rehearsal first; same bridged
  gas prerequisite as Codes (~$50–100 to Base — one bridge covers both).
- **Sept 10–14**: quiet launch (site + one post). **Drop-minus-48h**:
  snapshot. **Sept 21–27**: Codes opens to star-holders first.
- Registers for Base Builder Rewards at deploy; the pair of contracts
  (stars + codes) is a stronger retro-grant story than either alone.

## Open questions for Jeffrey

- Price: 0.005 / 0.01 / 0.02 ETH? (0.01 recommended.)
- One star design family or per-year visual language?
- Does the sky live at whistlegraph.org/stars or on the front page?
