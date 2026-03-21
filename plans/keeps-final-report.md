# Keeps Final Status Report

**Date:** March 2, 2026
**System:** KidLisp Keeps — Tezos NFT minting for aesthetic.computer
**Purpose:** Snapshot of everything built, deployed, and remaining before production launch

---

## What Keeps Is

Keeps lets users permanently preserve KidLisp programs as NFTs on Tezos. A user creates generative art in KidLisp on aesthetic.computer, then runs `keep $piece-name` to mint it as a self-contained HTML artifact stored on IPFS with on-chain metadata. 10% royalties on secondary sales. Viewable on objkt.com.

---

## Contract History

| Version | Contract | Admin | Fee | Tokens | Status |
|---------|----------|-------|-----|--------|--------|
| v2 | (compiled only, never deployed to mainnet) | — | — | — | Archived |
| v3 | `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM` | staging | 0 | 5 | Retired |
| v3 | `KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi` | staging | 0 | 5 | Retired |
| v4 | `KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W` | staging | 0 | 2 | Retired |
| v4 | `KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K` | — | 0 | — | Ghostnet only |
| **v5 RC** | **`KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido`** | **staging** | **2.5 XTZ** | **2** | **Active (staging)** |

All mainnet test contracts hold **0 XTZ** — mints were done with 0 fee (admin bypass or fee not yet set).

---

## Wallets

| Name | Address | Alias | Balance | Role |
|------|---------|-------|---------|------|
| staging | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | — | 0.809 XTZ | Contract deployer, current v5 RC admin |
| kidlisp | `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` | keeps.tez | 0.102 XTZ | Future production admin |
| aesthetic | `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` | aesthetic.tez | 0.089 XTZ | @jeffrey's author wallet, 112 active tokens, 3382 historical txs |

**Total across all wallets: ~1.0 XTZ (~$0.38 at $0.378/XTZ)**

Keys stored in `tezos/kidlisp/.env` (kidlisp + aesthetic) and `tezos/staging/.env` (staging). Transfer script: `tezos/transfer.mjs`.

---

## What's Built and Working

### Smart Contract (v5)
- FA2-compliant NFT with TZIP-012, TZIP-016, TZIP-021
- `keep()` — mint with 2.5 XTZ fee (admin exempt)
- `edit_metadata()` — owner or creator can update (preserves attribution)
- `burn()` — token removal
- `content_hashes` bigmap — deduplication (same piece can't mint twice)
- `token_creators` bigmap — tracks original minter forever
- 10% royalties (1000 bps), adjustable via `set_default_royalty`
- Emergency `pause()` / `unpause()` circuit breaker
- `admin_transfer()` — customer service tool
- `set_keep_fee()` / `withdraw()` — fee management + revenue collection
- Source: `tezos/keeps_fa2_v5.py`

### Minting Pipeline (Backend)
- `keep-mint.mjs` — 10-step SSE streaming endpoint
  - JWT auth, handle requirement, ownership verification
  - Self-contained HTML bundle generation
  - WebP thumbnail via oven service (256x256 @ 2x)
  - IPFS upload via Pinata
  - Duplicate detection, cached media reuse
  - Returns prepared mint params for client wallet signing
- `keep-confirm.mjs` — records successful mint in MongoDB
- `keep-update.mjs` — prepares `edit_metadata` params for client signing
- `keep-update-confirm.mjs` — records successful metadata update

### Keep UI (Frontend)
- `disks/keep.mjs` — ~3800 line piece with full mint flow
  - 10-step timeline with real-time progress
  - Beacon wallet integration (popup-based)
  - Rebake (regenerate bundle) + sync (update chain) workflows
  - Already-minted detection with existing token info
  - Ownership display ("You own this" / "Owner: tz1...")
  - Dark/light theme support with themed button palettes
  - Cancel + Escape support during preparation
- `disks/kept.mjs` — post-mint success page with objkt/tzkt links
- `disks/wallet.mjs` — wallet connector + balance display
- `disks/connect-wallet.mjs` — onboarding flow

### Client Libraries
- `lib/keeps-client.mjs` — state machine, mint flow, event system
- `lib/keeps/constants.mjs` — single source of truth for addresses, networks, config
- `lib/keeps/tzkt-client.mjs` — TzKT API queries (token info, ownership, ledger)
- `lib/tezos-wallet.mjs` — Beacon wallet abstraction

### Tests
- `spec/keeps-v5-production-spec.js` — source-level invariants
- `spec/keeps-v4-readonly-spec.js` — read-only TzKT checks
- `spec/keeps-v4-security-spec.js` — authorization checks
- `spec/helpers/keeps-test-helper.mjs` — shared test utilities

### Tooling
- `tezos/keeps.mjs` — CLI admin tool (105KB)
- `tezos/transfer.mjs` — XTZ transfer between wallets
- `tezos/deploy-staging.mjs` — contract deployment script
- `utilities/keep-cleanup.mjs` — contract cleanup utility
- `batch-mint-keeps.mjs` — batch minting script

---

## What's NOT Done

### Blockers for Production Launch

1. **Fund wallets** — ~1 XTZ total is not enough
   - Need ~5 XTZ minimum for contract deployment gas
   - Need ~10 XTZ for test mints (2.5 XTZ each, recoverable from contract)
   - Need ongoing buffer for operations
   - **Recommendation: fund staging wallet with 20-30 XTZ from external source**

2. **Deploy v5 production contract on keeps.tez**
   - Compile v5 with SmartPy
   - Deploy to mainnet with `--wallet=kidlisp`
   - Transfer admin from staging to keeps.tez

3. **Update all contract address references** (8 locations):
   - `lib/keeps/constants.mjs` — NETWORKS.mainnet.contract
   - `keep-mint.mjs:31` — env var fallback
   - `keep-update.mjs:24` — env var fallback
   - `keep-update-confirm.mjs:12` — env var fallback
   - `kidlisp-keep.mjs:16` — env var fallback
   - `kept.mjs:13` — hardcoded
   - `wallet.mjs:14` — hardcoded
   - Set `KEEPS_STAGING = false` in constants.mjs

4. **End-to-end test on production contract**
   - Mint → transfer → owner rebake → creator sync
   - Verify on objkt.com
   - Test fee withdrawal

### Known Client Bugs (from V5-LAUNCH-PLAN.md)

| Bug | Status | Notes |
|-----|--------|-------|
| Rebake triggers `jump()` to IPFS URL | DONE | Fixed — button overlap issue |
| Paint not updating during rebake progress | DONE | Fixed — rendering location |
| Rebake end-to-end manual test | TODO | Needs manual verification |
| "Update Chain" flow after rebake | TODO | Needs manual verification |

### Future Features (Post-Launch)

- `keeps` command to list user's minted tokens
- Fee/gas estimation UI before signing
- Copy share link after mint
- kidlisp.com "Mint as Keep" button
- kidlisp.com "My Keeps" gallery
- kidlisp.com keeps page redesign (SVG castle, source previews)
- $roz attribution fix (sync with aesthetic.tez wallet)

---

## Launch Sequence (When Funded)

```
1. Fund staging wallet with 20-30 XTZ
2. Compile v5: smartpy compile tezos/keeps_fa2_v5.py
3. Deploy to mainnet: node tezos/deploy-staging.mjs --wallet=kidlisp
4. Test mint on new contract (admin, 0 fee)
5. Test mint as regular user (2.5 XTZ fee)
6. Verify token on objkt.com
7. Withdraw fees from contract
8. Update 8 contract address references in codebase
9. Set KEEPS_STAGING = false
10. Deploy to Netlify
11. Smoke test: first real production mint
12. Announce
```

---

## Revenue Model

- **Mint fee:** 2.5 XTZ per keep (~$0.95 at current prices)
- **Royalties:** 10% on all secondary sales (objkt.com, teia, etc.)
- **Fee adjustment:** Admin can call `set_keep_fee()` anytime
- **Revenue collection:** Admin calls `withdraw()` to sweep contract balance
- **Break-even:** IPFS pinning costs are negligible; revenue starts from mint #1

---

## File Map

| Category | Key Files |
|----------|-----------|
| Contracts | `tezos/keeps_fa2_v5.py`, `tezos/keeps_fa2_v4.py` |
| Deployment | `tezos/deploy-staging.mjs`, `tezos/transfer.mjs` |
| Backend | `netlify/functions/keep-mint.mjs`, `keep-confirm.mjs`, `keep-update.mjs` |
| Frontend | `disks/keep.mjs`, `disks/kept.mjs`, `disks/wallet.mjs` |
| Libraries | `lib/keeps-client.mjs`, `lib/keeps/constants.mjs`, `lib/keeps/tzkt-client.mjs` |
| Tests | `spec/keeps-v5-production-spec.js`, `spec/keeps-v4-*.js` |
| Plans | `plans/keeps-next-steps.md`, `plans/keeps-redesign.md`, `tezos/V5-LAUNCH-PLAN.md` |
| CLI | `tezos/keeps.mjs` (105KB admin tool) |
| Config | `tezos/kidlisp/.env`, `tezos/staging/.env` |
