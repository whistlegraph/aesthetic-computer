# Keeps: Current State Analysis (December 2025)

**Status**: Mainnet Staging Active  
**Contract**: `KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi` (v3)  
**Last Updated**: December 31, 2025

---

## Recent Changes (December 31, 2025) 🆕

### V3 Contract Deployed
- [x] New contract `KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi` with owner-editable metadata
- [x] Contract has `token_creators` bigmap to track original minters
- [x] `edit_metadata` allows: admin, token owner, OR original creator

### Client-Side Wallet Signing for Metadata Updates
- [x] Server returns `mode: "prepare"` params for client to sign
- [x] User's wallet signs `edit_metadata`, preserving objkt.com "Created by" attribution
- [x] Fixed tokenId==0 bug (JS falsy check issue)

### Token Owner Rebake Support
- [x] Token owners (not just piece creators) can rebake bundles
- [x] Wallet connected at rebake time for ownership verification via TzKT
- [x] Ownership displayed in UI: "★ You own this" or "Owner: tz1abc..."

### Metadata Sync Permissions
- [x] **Admin**: Always allowed to sync
- [x] **Original creator**: Always allowed (preserves objkt attribution)  
- [x] **Token owner**: Blocked by default (would change objkt "Created by")
- [x] Added `allowOwnerEdit` flag for future use (contract supports it)
- [x] Error explains why token owners can't sync + shows original minter address

### Environment Fixes
- [x] Added `OVEN_URL=https://localhost:3002` to dev environment
- [x] Fixed `TZKT_API` undefined in keep-update.mjs

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

### ✅ COMPLETED (December 31, 2025)
- [x] V3 contract with owner/creator editable metadata
- [x] Client-side signing for edit_metadata (preserves attribution)
- [x] Token owner can rebake (regenerate bundle) 
- [x] Sync restricted to original creator (protects objkt attribution)
- [x] Ownership display in UI

### 🚀 READY FOR PRODUCTION
Before moving from staging to production contract:
- [ ] Test full flow: mint → transfer → owner rebake → creator sync
- [ ] Fix $roz attribution (sync with aesthetic.tez wallet)
- [ ] Consider: Should we burn+remint test tokens or keep them?

### 📋 Missing Features from Plan
- [ ] `keeps` command to list user's minted tokens
- [x] `keep $code` already checks minted state and shows existing keep UI
- [x] Cancel button + Escape during preparation
- [ ] Show estimated gas/fees before signing
- [ ] Copy share link after mint

### 🎨 kidlisp.com Integration
- [ ] "Mint as Keep" button for logged-in users
- [ ] "My Keeps" section
- [ ] Gallery of recent keeps

### 🔧 Developer Experience
- [ ] Auto-restart site on Netlify function error
- [ ] Better error messages in console
- [ ] Site health monitoring in artery

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
