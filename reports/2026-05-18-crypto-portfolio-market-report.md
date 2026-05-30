# crypto portfolio + market report — 2026-05-18

> Read-only audit. Public addresses only — no keys decrypted, no mnemonics
> read, nothing signed or sent. Supersedes `2026-03-30-crypto-portfolio-market-report.md`.
> Reproducible: `node banking/crypto/audit-readonly.mjs` (snapshot in
> `banking/crypto/data/audit-2026-05-18.json`).

## the blunt headline

**Total liquid across all chains ≈ $4.70.** There is no trading / yield /
"make cash right now" play here — there is no capital to deploy. The only
genuinely immediate cash is **withdrawing your own ~7.5 tez sitting in the
keeps contract (~$2.53)**. Everything past that is rebuilding the keeps
creative-economy engine, which pays out over weeks via royalties + new
collectors — a real business lever, not instant cash.

Live prices used: tez **$0.3370**, ETH **$2085.42**, SOL **$83.83** (CoinGecko, 2026-05-18).

## wallet balances

### tezos
| wallet | address | balance | usd |
|---|---|---|---|
| aesthetic.tez | tz1gkf8EexComFBJvjtT1zdsisdah791KwBE | 2.5640 tez | $0.86 |
| kidlisp/treasury | tz1fEjGQrEE2LXNKqcpTYAJV16pbbFpLeNyd | 0.2843 tez | $0.10 |
| keeps admin / royalty rx | tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC | 0.5017 tez | $0.17 |
| staging | tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt | 0.1239 tez | $0.04 |
| **liquid subtotal** | | **3.474 tez** | **$1.17** |
| keeps KT1 contract (withdrawable by admin) | KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB | 7.500 tez | $2.53 |
| **tezos total** | | **10.97 tez** | **$3.70** |

### ethereum (mainnet)
| wallet | address | balance | usd |
|---|---|---|---|
| 4esthetic.eth | 0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08 | 0.000342 ETH | $0.71 |
| whistlegraph.eth | 0x238c9c645c6EE83d4323A2449C706940321a0cBf | 0.0000674 ETH | $0.14 |
| eth-2 (citizen idx1) | 0xF637C4E11072C46A3bF7cF70ef32470562B5a479 | 0.0000035 ETH | $0.01 |
| eth-unknown | 0x98eAc86755792e03D0f027cA8CcFa83818B994c4 | 0.0000023 ETH | ~$0 |
| **ethereum total** | | **~0.000415 ETH** | **~$0.87** |

### base l2
All wallets ~0 (4esthetic dust 3.85e-7 ETH). No change since March.

### solana
| wallet | address | balance | usd |
|---|---|---|---|
| phantom | D5tLrs4Ubh3tcHxhSmorPmGrDsozuSLhQDGVoBbR5P9d | 0.001569 SOL | $0.13 |
| axio trade | 6JRph4ZZf5jt17CZhyBM9SYVexLrKmyPVMGCTMA4A7Ps | 0 SOL | $0 |
| **solana total** | | | **~$0.13** |

`axio` holds 4 SPL token accounts — all pump.fun mints
(`…pump`), all with **no liquidity pool / dead market ≈ $0**. Leftover dust
from old pump.fun trades that rugged. Not a windfall. (Optional: closing the
token accounts reclaims ~0.008 SOL rent — not worth the effort.)

### bitcoin + cardano — AUDIT GAP
4 BTC wallets + 1 Cardano wallet have **no public address stored in
`wallets/wallets.json`** (`keys_in` points at ordinals.com / Sparrow). Cannot
be audited read-only without them. **To include them next run, add a public
`address` (or xpub / receive address) field for each — no private key
needed.** Given they're all "ordinals-test"-type wallets, expected value is
low, but unknown until addresses are supplied.

## keeps NFT engine (KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB)

- **88 tokens minted** (was 81 on 2026-03-30, +7). Each is 1/1.
  *Caveat: TzKT "holder" count ≈ token-balance rows, not 88 distinct people —
  do not quote "88 unique collectors."*
- **Contract holds 7.5 tez** of accumulated mint proceeds/fees — withdrawable
  by the admin wallet. **This is the single most concrete cash item.**
- **We currently hold 4 tokens:** $odj #28, $pie #81, $tam #86, $r94 #87.
- **Our live OBJKT listings (6):** $9x9 #76 @ 7 tez · $tam #86 @ 10 · $weus
  #27 @ 12 · $ixi #48 @ 12 · $pie #81 @ 12 · $r94 #87 @ 14. (#28 $odj is held
  but **unlisted**.)
- **Standing offers on the collection (held by others, not us):**
  - #3 $3jj — **100 tez** bid from `tz1QRXx…`
  - #53 $fsf — **16.9 tez** bid from same `tz1QRXx…`
  - These same two bids existed on 2026-03-30 — **7 weeks stale, holders
    haven't sold.** If either sells, your royalty (10%) = **10 tez + 1.69 tez
    ≈ 11.69 tez (~$3.94 today)**. Not in your control.
- Royalty inflow is a trickle (0.05 / 0.08 / 0.11 tez receipts). Last contract
  activity **2026-04-27** — collection has been quiet ~3 weeks.

## delta vs 2026-03-30

- **tez price collapsed ~$0.80 → $0.337 (−58%)** — the dominant reason the
  position shrank in USD terms.
- Liquid tez 7.17 → 3.47 (spent on 3× `keep` mints @ 2.5 tez on 04-23; 7.5 tez
  now parked in the contract).
- ETH balance actually rose 0.00015 → 0.000415, still dust at today's price.
- Keeps minted 81 → 88; engine otherwise idle since late April.
- The two big standing bids did **not** convert in 7 weeks.

## ranked cash actions (honest)

1. **Withdraw the 7.5 tez from the keeps KT1 contract** — already yours, one
   admin call (`withdraw_fees`) from the keeps-admin wallet. ~$2.53. *You
   execute it — this audit is read-only and never touches keys.*
2. **List $odj #28** on OBJKT (held, currently unlisted) and sanity-check the
   6 live listings against comparable recent sales. A 10-tez sale ≈ $3.37.
   Buyer-dependent.
3. **Nudge the holders of #3 ($3jj) and #53 ($fsf)** that there's a 7-week
   standing bid (100 / 16.9 tez) from `tz1QRXx…`. If they take it you collect
   ~11.69 tez royalty. Indirect, but a stale bid worth surfacing.
4. **Cross-chain distribution (strategic, NOT cash-now):** Zora open editions
   on Base, Farcaster frames, Solana cNFTs. Near-zero cost, volume/visibility
   — payoff is weeks out, not today.
5. **Leave the dust.** ETH+SOL ≈ $1; conversion/gas costs exceed value. Don't
   move it.
6. **Ignore the pump.fun tokens** — dead, ~$0.

## the realistic takeaway

The "make cash right now" answer is: **~$2.50 by withdrawing your own
contract balance, plus whatever the listings/standing-bids convert to over
the coming weeks.** The keeps engine is the only real revenue lever and it's
been idle ~3 weeks — re-activating it (new mints, driving secondary volume so
royalties flow, cross-chain reach) is a multi-week creative-economy play, not
a same-day cash event. Anyone framing a sub-$10 multi-chain position as a
trading opportunity is wrong.

## audit gaps / to improve next run

- Add public BTC + Cardano addresses to `wallets/wallets.json` (no keys).
- ETH NFT enumeration: keyless Reservoir is now API-gated. The vault already
  has a **read-only** Etherscan/OpenSea data key (separate from wallet
  secrets) — wiring that into `audit-readonly.mjs` would restore the NFT
  inventory. Prior manual inventory still stands (4esthetic.eth: a2p-v1 ×3,
  the-longest-whistlegraph-ever 4/5, screenshots-14 ×2; whistlegraph.eth:
  collected works) — all illiquid art, not cash.
