# Keeps: Current State Analysis (December 2025)

**Status**: Mainnet Staging Active  
**Contract**: `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM`  
**Last Updated**: December 26, 2025

---

## What's Already Implemented ✅

### Authentication & Authorization (keep-mint.mjs)
- [x] JWT authentication via Auth0 (`authorize()`)
- [x] Handle requirement - must have `@handle` to keep
- [x] **Ownership verification** - `piece.user === user.sub` check
- [x] Admin bypass for testing (`hasAdmin()`)
- [x] Anonymous pieces blocked ("Log in and save it first")

### Wallet Enforcement (keep-mint.mjs)
- [x] Tezos wallet must be linked (`userDoc.tezos.address`)
- [x] **Minting wallet MUST match linked wallet** - prevents impersonation
- [x] Server-side minting with admin key for actual Tezos tx

### Minting Flow (keep.mjs UI + keep-mint.mjs)
- [x] 10-step timeline UI with progress feedback
- [x] SSE streaming for real-time updates
- [x] Bundle HTML generation (self-contained artifact)
- [x] Thumbnail WebP via oven (256×256 @ 2x = 512×512)
- [x] IPFS upload via Pinata
- [x] Duplicate prevention (`content_hashes` bigmap)
- [x] Already-minted detection with existing token info
- [x] Confirmation step before minting

### Rebake Flow (keep.mjs + keep-mint.mjs)
- [x] Regenerate bundle for already-minted piece (`regenerate=true`)
- [x] Cached IPFS media detection (skip if source unchanged)
- [x] `ipfsMedia` tracking in MongoDB
- [x] `pendingRebake` flag when bundle differs from on-chain

### Metadata Update (keep-update.mjs)
- [x] `edit_metadata` entrypoint call
- [x] **Preserves original artist** - fetches `firstMinter` from TzKT
- [x] Updates `artifactUri`, `thumbnailUri`, `attributes` on-chain
- [x] Admin-only (for now) - requires server signing key

### Database Tracking (keep-confirm.mjs)
- [x] Records successful mints in `kidlisp` collection
- [x] Stores `kept` object: tokenId, txHash, wallet, URIs, timestamps
- [x] Ownership verification before confirming

---

## What's Actually Next 🎯

Looking at the code, the **self-service minting is already implemented**. The main gaps are:

### 1. Artist-Controlled Updates (Not Admin-Only)
Currently `keep-update.mjs` is admin-only. We need to allow the **original artist** to update their own token metadata without admin intervention.

**Changes needed:**
- [ ] In `keep-update.mjs`: Allow update if `user.sub === piece.user` (not just admin)
- [ ] Verify on-chain that caller is the firstMinter
- [ ] UI: Show "Update Chain" button to original author (not just admin)

### 2. Missing Features from Plan
- [ ] `keeps` command to list user's minted tokens
- [ ] `keep:status $code` to check if already minted
- [ ] Cancel ability during preparation
- [ ] Show estimated gas/fees before signing
- [ ] Copy share link after mint

### 3. kidlisp.com Integration
- [ ] "Mint as Keep" button for logged-in users
- [ ] "My Keeps" section
- [ ] Gallery of recent keeps

---

## Environment Variables (Already Configured)

```env
KEEPS_NETWORK=mainnet
KEEPS_CONTRACT=KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM
OVEN_URL=https://oven.aesthetic.computer
# Secrets in MongoDB 'secrets' collection:
#   pinata: { apiKey, apiSecret, jwt }
#   tezos-kidlisp: { address, publicKey, privateKey }
```

---

## Files Reference

| File | Purpose |
|------|---------|
| `system/netlify/functions/keep-mint.mjs` | Streaming SSE mint endpoint |
| `system/netlify/functions/keep-update.mjs` | On-chain metadata update |
| `system/netlify/functions/keep-confirm.mjs` | Record successful mint |
| `system/public/aesthetic.computer/disks/keep.mjs` | Keep UI/flow (2482 lines) |
| `system/public/aesthetic.computer/lib/keep/tezos-wallet.mjs` | Beacon wallet |
| `tezos/keeps.mjs` | CLI tool for admin ops |
