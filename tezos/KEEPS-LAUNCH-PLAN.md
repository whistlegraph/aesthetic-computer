# Keeps Launch Plan 🔮

**Status:** 🟡 v6 Live, Rolling to v7  
**Last Updated:** March 8, 2026

## Current Timeline

| Date | Milestone | Status |
|------|-----------|--------|
| 2026-03-07 | v6 launch prep complete | ✅ Done |
| 2026-03-08 | v6 deployed on mainnet (`KT1J15kADMuRWh9kJZzosBeRBYPjYr7RvhoN`) | ✅ Done |
| 2026-03-08 | First production keep minted (`$roz`, token #0) | ✅ Done |
| 2026-03-08 | v7 source + artifacts prepared | ✅ Done |
| Next | v7 staging validation (transfer + creator refresh + lock) | 🔄 In Progress |
| Next | v7 mainnet deployment from `keeps.tez` | ⏳ Pending |

## Active Contracts

| Network | Contract | Version | Status | Admin |
|---------|----------|---------|--------|-------|
| **Mainnet** | `KT1J15kADMuRWh9kJZzosBeRBYPjYr7RvhoN` | **v6** | **Live** | `keeps.tez` |
| Ghostnet | `KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K` | v3 lineage | Dev/Test | `keeps.tez` |

## Wallet Architecture

| Wallet | Address | Domain | Role |
|--------|---------|--------|------|
| **kidlisp** | `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` | keeps.tez | Production deploy/admin wallet |
| staging | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | — | Staging and migration housekeeping |
| aesthetic | `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` | aesthetic.tez | Creator-flow verification |

## v7 Direction

- Keep FA2 entrypoints and storage compatibility.
- Add creator-safe post-transfer refresh in `edit_metadata`.
- Preserve immutable guarantees for `content_hash` and `royalties`.
- Keep Mongo `secrets` as the single source of truth for active profile/address.
- Detailed runbook: `tezos/V7-LAUNCH-PLAN.md`

---

## Overview

Keeps are interactive KidLisp programs preserved as Tezos FA2 NFTs. Each keep bundles the complete source code, a self-contained HTML artifact, and metadata into an on-chain token that can be viewed on objkt.com or played directly from IPFS.

### Current Architecture

```
User Flow:
  prompt.mjs (keep $code) 
    → keep.mjs (timeline UI with confirmation)
      → /api/keep-mint (SSE streaming)
        → bundle-html (HTML generation)
        → oven/grab-ipfs (256x256 thumbnail)
        → Pinata IPFS upload
      → tezos-wallet.mjs (Beacon SDK)
        → Temple/Kukai/etc wallet
        → Contract.keep() call
      → /api/keep-confirm (record in MongoDB)
      
  Rebake Flow (update existing):
    → keep.mjs (already-minted view)
      → Rebake button → /api/keep-mint?regenerate=true
      → Update Chain button → /api/keep-update
        → edit_metadata entrypoint
```

---

## Phase 1: Ghostnet Hardening ✅ (Complete)

### 1.1 Core Mint Flow Testing

| Test Case | Status | Notes |
|-----------|--------|-------|
| Basic mint from prompt.mjs | ✅ | `keep $code` works |
| Timeline visual feedback | ✅ | 10-step progress UI |
| Confirmation before mint | ✅ | User must confirm before starting |
| Wallet connection (Temple) | ✅ | Beacon SDK integration |
| Wallet connection (Kukai) | ✅ | Works with network switching |
| Bundle HTML generation | ✅ | Self-contained artifact |
| Thumbnail WebP generation | ✅ | Fixed 256x256 via oven |
| IPFS pinning (Pinata) | ✅ | 3 files per keep |
| Duplicate prevention | ✅ | content_hashes bigmap |
| Already-minted UI | ✅ | Shows existing token info + rebake |
| Error handling | ✅ | Improved messages |
| SSE streaming | ✅ | Real-time progress updates |
| Network switching | ✅ | Uses KEEPS_NETWORK env var |
| Token ID retry | ✅ | 5 attempts after mint |

### 1.2 UI/UX Polish

**In `keep.mjs`:**
- [x] Add confirmation step before minting starts
- [x] Show STAGING badge when using staging contract
- [x] Contract button linking to TzKT explorer
- [x] Display sync status (on-chain vs cached/pending)
- [x] Show network label (Mainnet/Ghostnet) in wallet step
- [x] Rebake button for already-minted pieces
- [x] Update Chain button for on-chain metadata updates
- [x] Add cancel button + Escape (cancels prep and returns to prompt)
- [ ] Show estimated gas/fees before signing
- [ ] Add "copy share link" after mint

**In `prompt.mjs` / `wallet.mjs`:**
- [x] Financial news ticker with XTZ price, block, balance
- [x] KidLisp source scrolling in YOUR KIDLISP section
- [x] Fix objkt URL fallback when tokenId is null
- [ ] Add `keeps` command to list user's minted tokens
- [x] `keep $code` already checks minted state (TzKT) and shows existing keep UI

### 1.3 Analysis & Metadata

**Implemented:**
- [x] Full local code analysis with behavior tags
- [x] Complexity tier detection
- [x] Description formatting (newlines → comma-space)
- [x] Store tezos data keyed by contract
- [x] ipfsMedia and pendingRebake tracking

### 1.4 kidlisp.com Integration

**Current state:** kidlisp.com is a standalone HTML editor that:
- Has code editor with syntax highlighting
- Shows live preview
- Can save/load to localStorage
- Links to aesthetic.computer for full execution

**Needed for keeps integration:**
- [ ] Add "Mint as Keep" button for logged-in users
- [ ] Show login prompt for anonymous users
- [ ] Add keeps gallery view (recent mints, trending)
- [ ] Add "My Keeps" section showing user's minted tokens
- [ ] Deep link support: `kidlisp.com/$code` loads that code
- [ ] "Fork" existing keeps into editor

### 1.5 API Improvements

**`/api/keep-mint`:**
- [x] SSE streaming for real-time progress
- [x] Regenerate mode for rebaking existing pieces
- [x] Skip wallet validation in rebake mode
- [ ] Add rate limiting (prevent spam)
- [ ] Add retry logic for IPFS failures

**`/api/keep-update`:**
- [x] Admin-only endpoint for on-chain updates
- [x] Calls edit_metadata entrypoint
- [x] Updates artifactUri, thumbnailUri on-chain
- [x] Uses MichelsonMap for metadata

**`/api/keep-confirm`:**
- [x] Index by user + tokenId for lookup
- [x] Store authorship permanently

**New APIs needed:**
- [ ] `GET /api/keeps` - List recent keeps (paginated)
- [ ] `GET /api/keeps/user/:handle` - User's keeps
- [ ] `GET /api/keeps/code/:code` - Get keep by code

### 1.6 Bundle/NFT Fixes

- [x] Fix NFT bundle imports - use bare specifiers in PACK_MODE
- [x] Skip cache-busting imports in PACK_MODE
- [x] Skip history.replaceState in sandboxed iframes
- [x] Disable perf telemetry in PACK_MODE

---

## Phase 2: Mainnet Preparation ✅ (Complete)

### 2.1 Contract Deployment

```bash
# Mainnet staging contract already deployed!
# Contract: KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi
```

**Pre-deployment checklist:**
- [x] Contract code (KeepsFA2v2) tested
- [x] Storage structure verified
- [x] All entrypoints tested on ghostnet
- [x] edit_metadata entrypoint working
- [x] **Mainnet staging contract deployed** ✅
- [ ] Final production contract (if different from staging)
- [ ] Document admin operations

### 2.2 Configuration

**Environment Variables (Current):**
```
KEEPS_NETWORK=mainnet
KEEPS_CONTRACT=KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM
KEEPS_RPC=https://mainnet.ecadinfra.com
```

**Implemented:**
- [x] Network switching via KEEPS_NETWORK env var
- [x] wallet.mjs uses KEEPS_NETWORK for wallet connect
- [x] keep.mjs uses NETWORK constant throughout
- [x] Network label shown in UI (Mainnet/Ghostnet)
- [x] STAGING badge shown on staging contract
- [x] kidlisp.com configured for mainnet

### 2.3 Network Switching

**In `keep.mjs` and `wallet.mjs`:**
- [x] Network determined by KEEPS_NETWORK env
- [x] Default to mainnet for production
- [x] Show clear network indicator in UI
- [x] Financial ticker shows current network info
- [ ] Warn users about mainnet costs

### 2.4 Fee Structure

| Fee Type | Ghostnet | Mainnet |
|----------|----------|---------|
| Mint fee | 0 XTZ | 1-5 XTZ |
| Gas (estimate) | ~0.1 XTZ | ~0.3 XTZ |
| IPFS pinning | Free | Free (Pinata) |

---

## Phase 3: Launch & Marketing

### 3.1 Soft Launch

1. Enable mainnet for trusted testers
2. Mint first 10-20 canonical keeps
3. Verify objkt.com display
4. Test secondary sales

### 3.2 Public Launch

1. Announce on aesthetic.computer
2. Add keeps to kidlisp.com homepage
3. Create launch collection (curated codes)
4. Document in WRITE-A-PIECE.md

### 3.3 Gallery Features

- [ ] Gallery piece: `prompt gallery` or `prompt keeps`
- [ ] Show keeps on user profile pages
- [ ] Add keeps to feed/ticker system
- [ ] objkt.com collection page

---

## Testing Checklist

### Ghostnet Tests (Do Before Mainnet)

```bash
# From prompt.mjs
keep $cow          # Already minted → shows existing
keep $newpiece     # Fresh mint → full flow

# From kidlisp.com
# 1. Write new code
# 2. Click "Mint"
# 3. Connect wallet
# 4. Sign transaction
# 5. Verify on objkt.com
```

### Manual Test Flow

1. **Start fresh:**
   ```
   wallet disconnect
   ```

2. **Connect wallet:**
   ```
   wallet
   ```

3. **Check piece doesn't exist:**
   ```
   keep $testxyz
   ```

4. **Observe timeline:**
   - Wallet connected ✓
   - Validate piece ✓
   - Analyze source ✓
   - Generate thumbnail ✓
   - Bundle assets ✓
   - Upload to IPFS ✓
   - Create metadata ✓
   - Review & confirm ✓
   - Sign transaction ✓
   - Complete! ✓

5. **Verify on explorers:**
   - TzKT: token exists
   - objkt: artwork displays
   - IPFS: artifact loads

---

## Known Issues

### High Priority 🔴

1. ~~**Wallet reconnection flaky**~~ ✅ Fixed - Beacon session restoration improved
2. ~~**Error messages cryptic**~~ ✅ Fixed - Better user messages
3. ~~**No cancel during prep**~~ ✅ Cancel button + Esc returns to prompt

### Medium Priority 🟡

1. ~~**Thumbnail size**~~ ✅ Fixed - Now 256x256
2. **No batch minting** - One at a time only
3. ~~**Missing analytics**~~ ✅ Added - Sync status, contract linking

### Low Priority 🟢

1. **No royalties** - Could add FA2.1 royalty support
2. **No burns** - Tokens are permanent
3. **No collections** - All in one flat list

---

## Files & Locations

| Component | Location |
|-----------|----------|
| CLI tool | `tezos/keeps.mjs` |
| Contract v3 (Python) | `tezos/keeps_fa2_v3.py` |
| Contract v3 (Michelson) | `tezos/KeepsFA2v3/` |
| Mint UI | `system/.../disks/keep.mjs` |
| Prompt commands | `system/.../disks/prompt.mjs` |
| Wallet lib | `system/.../lib/tezos-wallet.mjs` |
| Mint API | `system/netlify/functions/keep-mint.mjs` |
| Bundle API | `system/netlify/functions/bundle-html.js` |
| kidlisp.com | `kidlisp.com/index.html` |

---

## Next Steps (Production Launch)

### Next Steps (Now)

1. **End-to-end mint via keep UI**
  - Run `keep $code` from prompt and complete wallet signing.
  - Verify `keep-confirm` writes the record and the token shows in objkt.

2. **Artifact bundle validation**
  - Confirm the “bundle” + “ipfs” steps complete.
  - Open the IPFS artifact URL and ensure it runs.

3. **v3 attribution test**
  - Mint with creator wallet → Rebake → Update Chain (prepare mode).
  - Confirm objkt “Created by” remains the original creator.

4. **Fee/contract consistency**
  - Ensure review step shows the contract fee and wallet sends the same amount.

1. [x] Test full mint flow on ghostnet via `keep $abc`
2. [x] Verify already-minted detection works
3. [x] Test rebake functionality for existing tokens
4. [x] Test Update Chain for on-chain metadata updates
5. [x] Deploy staging contract to mainnet

### Mainnet Staging Testing (Current Phase)

**Metadata Editing Tests:**
- [x] Test edit_metadata entrypoint — ✅ Works!
- [x] Understand permission model — ✅ See below
- [ ] Test rebake → update chain from **creator wallet**
- [ ] Verify metadata updates appear on objkt.com with correct artist
- [ ] Ensure old IPFS links still work after update
- [ ] Test updating only specific fields (artifact vs thumbnail)

**v3 Permission Model (edit_metadata):**
```
edit_metadata can be called by:
1. Admin (contract administrator) — loses artist attribution on objkt!
2. Token owner (current holder) — may not be original artist
3. Original creator (token_creators[token_id]) — ✅ PRESERVES attribution!

For proper objkt.com artist display:
→ User must sign the mint from their wallet (becomes token_creator)
→ User must sign edit_metadata from same wallet (preserves firstMinter)
→ Server/admin signing breaks attribution!
```

**Current Token 0 (`$roz`) State:**
- Creator: `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` (aesthetic.tez)
- Owner: `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` (staging)
- Admin: `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` (staging)

**Security & Logic Audit:**
- [x] Verify edit_metadata permissions — ✅ Admin OR owner OR creator
- [ ] Verify only token owner can transfer
- [ ] Test duplicate content hash rejection on mainnet
- [ ] Verify mint fees are collected correctly
- [ ] Check for reentrancy or gas limit issues
- [ ] Test edge cases: empty code, very long code, special chars
- [ ] Verify content_hash computation matches between client/contract

**Integration Tests:**
- [ ] Mint from keep.mjs on mainnet staging
- [ ] Mint from kidlisp.com on mainnet staging  
- [ ] Verify wallet.mjs displays mainnet tokens correctly
- [ ] Test objkt.com token display and metadata
- [ ] Test IPFS artifact loads in browser
- [ ] Verify TzKT explorer shows correct data

### Production Launch
6. [ ] Complete all staging tests above ⬆️
7. [ ] Mint first canonical keeps on mainnet
8. [ ] Verify objkt.com display of mainnet tokens
9. [ ] Test secondary sales on mainnet
10. [ ] Decide: use staging contract for production or deploy fresh?
11. [ ] Public announcement

---

*Last updated: January 1, 2026*
