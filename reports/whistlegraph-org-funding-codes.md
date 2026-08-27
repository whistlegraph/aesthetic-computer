# Whistlegraph.org Funding Codes

A code system that makes whistlegraphs ownable on Ethereum, routes a share of
every derivative /pop release to each graph's owner, and pays for its own
construction through Base's builder programs.

## The idea in one loop

Every whistlegraph gets a short code. The code is simultaneously its URL
(`whistlegraph.org/wg7`), its token (one ERC-721 per code), and its name
(`wg7.whistlegraph.eth`). Collectors buy codes. When Whistlegraph Dot Org
releases a /pop track built from a graph, that graph's owner automatically
shares in the release — so owning a code is owning a stake in the songbook
that grows out of it. Releases make graphs appreciate; appreciation sells
codes; code sales and Base grants fund more releases.

## What already exists (nothing here starts from zero)

- **whistlegraph.eth** — resolves to `0x238c9c645c6EE83d4323A2449C706940321a0cBf`
  (plus 4 derived accounts). Currently address-only; no url/avatar records.
- **whistlegraph.org** — live on lith behind Cloudflare (apex, www, tv), mail
  restored (MX/SPF re-added 2026-08-26), Gmail send-as working.
- **Ten Whistlegraphs** — 10 unsold editions being transferred back from
  Feral File (requested 2026-08-26, they pay fees). Provenance: the 2022
  Feral File exhibition "Ten Whistlegraphs" (45 editions across 10 works).
- **A proven code market** — Tezos keeps v11: 92 tokens, 70 organic sales at
  5–14ꜩ, repeat collectors including reas.tez. The mechanic works; only the
  denominating chain is poor.
- **Content supply** — the TikTok whistlegraph corpus, the score pieces
  (gesture = envelope), and the /pop pipeline already publishing as
  Whistlegraph Dot Org.
- **Contract experience** — keeps.mjs (FA2 mint/marketplace) ports
  conceptually to a Solidity ERC-721 + minter.

## Architecture

Two tiers, one site.

**Mainnet (prestige):** the ten Feral File editions live in whistlegraph.eth
and are never fire-sold. A gallery page on whistlegraph.org shows them with
live onchain ownership (Alchemy/Etherscan keys already in vault). Staged
relaunch, not simultaneous listing; 2–3 editions marked permanently
not-for-sale to make the rest scarcer.

**Base (the living system):** `WhistlegraphCodes` ERC-721 where tokenId maps
to code. Mints priced in ETH from the code's own page; EIP-2981 royalties to
whistlegraph.eth; metadata served from `whistlegraph.org/<code>` with lith
rendering the graph. Base because mints cost cents, ownerOf lookups for the
funding mechanics stay same-chain, and a code-keyed art contract is exactly
the "novel onchain format" Base Builder Grants fund.

**The site is the resolver:** ENS `url` record → whistlegraph.org; the site
displays the wallet; each name and domain verifies the other. Codes become
ENS subnames so authenticity is a resolution, not a marketplace claim.

## The funding codes — how owners share in growth

When a /pop track derives from graph `wg7`, its current owner receives, in
order of increasing spice:

1. **Provenance** — the track's metadata and release page cite the source
   token. The graph appreciates culturally; the owner captures it on resale.
2. **First edition** — `ownerOf(wg7)` gets an automatic claim on edition #1
   of the track drop. A patronage perk, not a promise.
3. **Mint split** — the track's primary mint proceeds and resale royalties
   route 80/20 between whistlegraph.eth and the graph's owner (0xSplits,
   resolved at drop time).

**Hard boundary:** only onchain-native value is shared — mints, claims,
resale royalties. Streaming/DistroKid revenue is never tokenized, and the
language is always "the owner receives the first edition and a royalty
split," never "invest to earn." A lawyer reviews any revenue-share copy
before it ships.

## Where the money comes from

| Stream | Mechanism | Horizon |
|---|---|---|
| Primary code mints | ETH sales at whistlegraph.org/<code> | at launch |
| Secondary royalties | EIP-2981 on both tiers | ongoing |
| The mainnet ten | staged relaunch during ETH strength | fall |
| Base Builder Grants | 1–5 ETH retroactive for the shipped contract | post-ship |
| Base Builder Rewards | weekly 2 ETH pool, prototypes count | immediately |
| Feral File channel | whistlegraphs as a DP-1 feed for FF1 / Art Computer | via Sean thread |

## Build plan

**Phase 0 — this week.** Set whistlegraph.eth ENS records (url, avatar);
sign up at builderscore.xyz; receive the Feral File transfer; gallery page
skeleton on whistlegraph.org.

**Phase 1 — the contract.** WhistlegraphCodes on Base + mint page; seed the
first ten codes from the TikTok corpus; post build progress publicly (Base
scouts Twitter/Farcaster — visibility is part of the funding mechanism).

**Phase 2 — the first funding code.** One /pop track wired to one graph:
first-edition claim + 80/20 split, released under Whistlegraph Dot Org.
Apply for the retroactive Base Builder Grant on the shipped system. Pitch
the DP-1 whistlegraph channel to Sean.

**Phase 3 — the relaunch.** The mainnet ten, told as one story: *Ten
Whistlegraphs (2022) → Funding Codes (2026)* — the scores come home and
start funding their own songbook, with the Reas keeps-collecting thread
bridging Tezos past and Ethereum present.

## Risks and lessons already paid for

- **Securities drift** — mitigated by the onchain-only boundary and perk
  framing above.
- **Chain-price dependence** — the Tezos lesson: 70 sales grossed ~$130
  because XTZ collapsed. ETH/Base concentrates on the chain with actual
  collector liquidity; the split design keeps working at any price.
- **Domain fragility** — the whistlegraph.com lesson: whistlegraph.org
  renewal (2027-07-07, Porkbun) goes on the deadlines tracker, and DNS
  records stay documented in the vault.
- **Fire-sale temptation** — the ten editions are the reserve asset, not
  this month's revenue.
