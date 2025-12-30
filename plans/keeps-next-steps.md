# Keeps: Current State Analysis (December 2025)

**Status**: Mainnet Staging Active  
**Contract**: `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM`  
**Last Updated**: December 30, 2025

---

## Recent Changes (December 30, 2025) 🆕

### Light/Dark Theme Support
- [x] Added `export const scheme = { dark: {...}, light: {...} }` to keep.mjs
- [x] Created `PHASE_COLORS_DARK` and `PHASE_COLORS_LIGHT` for step indicator colors
- [x] `paint($)` now uses `$.dark` to select palette: `pal = $.dark ? scheme.dark : scheme.light`
- [x] Background (`wipe()`) adapts to theme
- [x] Animated progress background adapts to theme

### Button Palette Thematization
All button color schemes now use centralized palette entries with dark/light variants:

| Button | Purpose |
|--------|---------|
| `btnLink` | IPFS links (HTML, THUMBNAIL) |
| `btnObjkt` | View on objkt marketplace |
| `btnRebake` | Regenerate cached bundle |
| `btnSync` | Sync chain metadata |
| `btnWallet` | Wallet shortcut |
| `btnContract` | Contract address link |
| `btnTx` | Transaction hash link |
| `btnPreview` | Preview piece (pulsing) |
| `btnConfirm` | Keep It action (pulsing) |
| `btnLogin` | Login prompt (pulsing) |
| `btnCached` | Cached media indicator |
| `btnNet` | Network toggle |
| `btnView` | View on objkt after mint |
| `btnToll` | Fee display |
| `btnStaging` | Staging contract link |
| `btnRetry` | Retry after error |

Pulsing buttons (`btnPreview`, `btnConfirm`, `btnLogin`) use `normalBase` property for animation calculations while `hover` and `disabled` come directly from palette.

### Fee Reading from Contract
- [x] `keep-mint.mjs` reads `keep_fee` from contract storage (was hardcoded 5tz)
- [x] UI shows actual fee from server with `?? 0` fallback

### UI Polish
- [x] Added "to keep on mainnet staging" text with clickable staging link
- [x] Fixed Sign Transaction detail text color (was too gray)

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

### 🔧 Artist Attribution Bug - FIXED (December 30, 2025)

**Original Report**: After updating metadata on token #4 (`$berz`), the artist attribution appeared to disappear from objkt.com "Created" tab (still shows in "Owned").

**ROOT CAUSE FOUND**: The `edit_metadata` entrypoint was being called by the **admin server wallet** (`tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC`), not the original creator's wallet. objkt.com re-attributes tokens based on who calls `edit_metadata`, causing the token to disappear from the original creator's "Created" tab.

**Evidence**: All `edit_metadata` calls in TzKT were from the admin wallet, while initial `keep` (mint) calls from different wallets worked correctly.

**FIX IMPLEMENTED** (December 30, 2025):
- ✅ Added `mode: "prepare"` to `keep-update.mjs` - returns Michelson params for client-side signing
- ✅ Updated `keep.mjs` "Sync Chain" button to use prepare mode and sign with user's wallet  
- ✅ Created `keep-update-confirm.mjs` to record successful updates in database
- ✅ Server-side signing path deprecated (still works but logs warning)

**How it works now**:
1. Client calls `/api/keep-update` with `mode: "prepare"`
2. Server prepares metadata, uploads to IPFS, generates Michelson params
3. Server returns `prepared` event with contract call parameters
4. Client uses `_api.tezos.call()` to have user's wallet sign the transaction
5. After confirmation, client calls `/api/keep-update-confirm` to record in DB

**Status**: ✅ FIX DEPLOYED - Original creator's wallet now signs `edit_metadata`, preserving objkt.com attribution.

---

### 1. Artist-Controlled Updates ✅ DONE
- [x] In `keep-update.mjs`: Allow update if `user.sub === piece.user` (already implemented)
- [x] Client-side signing so user's wallet is the caller (just implemented!)
- [x] UI: Show "Update Chain" button to original author (already implemented)

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
| `system/netlify/functions/keep-update.mjs` | On-chain metadata update (prepare mode for client signing) |
| `system/netlify/functions/keep-update-confirm.mjs` | Record successful metadata update (NEW) |
| `system/netlify/functions/keep-confirm.mjs` | Record successful mint |
| `system/public/aesthetic.computer/disks/keep.mjs` | Keep UI/flow (~3000 lines) |
| `system/public/aesthetic.computer/lib/keep/tezos-wallet.mjs` | Beacon wallet |
| `tezos/keeps.mjs` | CLI tool for admin ops |
