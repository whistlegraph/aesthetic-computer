# crypto portfolio + market report — 2026-03-30

## wallet balances

### tezos
| wallet | address | balance |
|---|---|---|
| aesthetic.tez | tz1gkf8EexComFBJvjtT1zdsisdah791KwBE | 6.59 tez |
| permit signer | tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC | 0.18 tez |
| kidlisp/staging | tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt | 0.12 tez |
| treasury | tz1fEjGQrEE2LXNKqcpTYAJV16pbbFpLeNyd | 0.28 tez |
| keeps contract | KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB | 0.00 tez (just withdrew) |
| **total** | | **~7.17 tez (~$5.74 USD)** |

### ethereum
| wallet | address | balance |
|---|---|---|
| 4esthetic.eth | 0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08 | 0.000076 ETH |
| whistlegraph.eth | 0x238c9c645c6EE83d4323A2449C706940321a0cBf | 0.000067 ETH |
| eth-unknown | 0x98eAc86755792e03D0f027cA8CcFa83818B994c4 | 0.000002 ETH |
| **total** | | **~0.00015 ETH (~$0.28 USD)** |

### base l2
all wallets: 0 ETH

### solana
| wallet | address | balance |
|---|---|---|
| phantom | D5tLrs4Ubh3tcHxhSmorPmGrDsozuSLhQDGVoBbR5P9d | 0.0016 SOL |
| axio | 6JRph4ZZf5jt17CZhyBM9SYVexLrKmyPVMGCTMA4A7Ps | 0 SOL |
| **total** | | **~0.0016 SOL (~$0.21 USD)** |

### bitcoin
4 wallets in vault (ordinals-test, ordsies, ordinals-receiver, btc-me) — balances unchecked

### cardano
1 wallet in vault — balance unchecked

---

## nft holdings

### tezos keeps (KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB)
- **tokens held:** 2 (#28 $odj, #58 $ulu)
- **tokens minted total:** 81
- **tokens airdropped today:** 15 to top OBJKT buyers
- **active offers on collection:** 100 tez on #3 $3jj, 16.9 tez on #53 $fsf (both held by others)
- **unique collectors:** 14 + 15 new airdrop recipients = 29
- **royalty rate:** 10% on secondary sales

### ethereum NFTs (4esthetic.eth)
- ENS: 4esthetic.eth
- a2p-v1: 3 pieces (IMG_4320.jpg, Bug log 2, Ash Ash music video)
- the-longest-whistlegraph-ever: 4/5 editions
- screenshots-14: 2 pieces

### ethereum NFTs (whistlegraph.eth)
- various collected works: wanderlustgirls, yeche, creation babies, foliavirus, mee6

---

## market activity today

### actions taken
1. fixed kidlisp.com routing on lith (dedicated caddy handler)
2. fixed CORS for cross-origin asset loading
3. fixed /api/logo.png route
4. set up self-hosted IPFS node on lith (replaced pinata)
5. unpinned 346 orphaned IPFS pins (freed 176MB)
6. minted $nocp (#80) — first keep using self-hosted IPFS
7. listed $nocp at 8 tez on OBJKT
8. withdrew 2.5 tez fees from keeps contract
9. transferred 5.5 tez from permit signer to aesthetic wallet
10. airdropped 15 keeps to top 15 active OBJKT buyers

### airdrop recipients (top spenders last 14 days)
| token | recipient | their 14-day spend |
|---|---|---|
| $b00 | tz1WrEjv... | 5,500 tez |
| $4wi | tz1i5Dyu... | 3,190 tez |
| $wiww | tz1d45L6... | 2,200 tez |
| $cab | tz1TRPmG... | 2,130 tez |
| $noo | crip2dax | 1,690 tez |
| $wcd | tz1fZjw5... | 1,181 tez |
| $y6p | tz1VutSv... | 1,056 tez |
| $9s5 | tz1YDugU... | 1,050 tez |
| $naee | Wert | 745 tez |
| $ctc | MASS_617 | 615 tez |
| $naoo | tz1aHnqu... | 565 tez |
| $alie | tz1NjsZx... | 452 tez |
| $vsu | tz1XEuFR... | 443 tez (75 buys) |
| $you | tz2HVjee... | 394 tez (107 buys) |
| $nocp | tz1Tj2wf... | 311 tez |

---

## revenue potential

### immediate
- **$ulu (#58):** still listed at 9 tez on OBJKT
- **$odj (#28):** unlisted, could list
- **royalties:** 10% on any secondary sales of the 79 tokens held by others
- **mint + list:** 6.59 tez can mint 2 more keeps at 2.5 tez each

### short-term (tezos)
- the 15 airdrop recipients are the biggest spenders on tezos right now
- if even 2-3 list and resell, you earn royalties
- collection visibility increased from 14 to 29 unique holders
- OBJKT collection page now has broader distribution = more impressions

### cross-chain opportunities
- **farcaster/base:** kidlisp pieces as frames — highest leverage, zero cost, viral distribution
- **zora on base:** open editions at 0.000777 ETH, volume play
- **solana:** compressed NFTs nearly free, massive buyer base on magic eden/tensor
- all chains: AC already serves pieces via URL, porting to other minting platforms is straightforward

---

## infrastructure changes today
- self-hosted IPFS node (kubo 0.33.2) on lith — eliminates pinata dependency
- ipfs.aesthetic.computer DNS → lith (was pinata CNAME)
- keep minting pipeline uses local IPFS instead of pinata API
- IPFS swarm announcing on direct IP (209.38.133.33:4001) for public gateway reachability
- 251 existing pins migrating from pinata to local node (background)

---

## api keys available
- etherscan, opensea, cloudflare, digitalocean, stripe, paypal, shopify
- openai, anthropic (for AI features)
- auth0, firebase, mongodb, redis
- all stored in aesthetic-computer-vault
