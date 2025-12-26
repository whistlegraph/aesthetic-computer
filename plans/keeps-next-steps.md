# Keeps: Next Steps (December 2025)

**Current Status**: Mainnet Staging Active  
**Contract**: `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM`  
**Last Updated**: December 26, 2025

---

## Completed ✅

### Core Minting
- [x] Basic mint from `keep $code` command
- [x] Timeline UI with 10-step progress
- [x] Wallet connection (Temple/Kukai via Beacon)
- [x] Bundle HTML generation (self-contained artifact)
- [x] Thumbnail WebP generation via oven (256×256)
- [x] IPFS pinning (Pinata)
- [x] Duplicate prevention (`content_hashes` bigmap)
- [x] Already-minted UI with rebake option

### Metadata Editing
- [x] `edit_metadata` entrypoint in contract
- [x] `/api/keep-update` endpoint for on-chain updates
- [x] Preserves original creator (`firstMinter`) during updates
- [x] Rebake button in UI (regenerate bundle without new token)
- [x] Update Chain button (push rebaked content to Tezos)

### Oven Integration
- [x] Oven service for thumbnail capture
- [x] Source tracking (`source: 'keep'`, `keepId`)
- [x] IPFS upload for thumbnails
- [x] Manual capture form in oven dashboard
- [x] Blank frame detection

---

## In Progress 🔄

### Phase 2: Creator Self-Service (Primary Goal)

The goal is to let users mint their own pieces without admin intervention.

#### 2.1 Ownership Verification

**Already Have:**
- `kidlisp-codes` MongoDB collection stores `{ code, source, user, when }`
- `user` field is Auth0 `sub` ID of whoever saved the piece
- `handleFor(userId)` resolves `@handle` from `sub`

**Need to Implement:**
- [ ] `GET /api/kidlisp-keep?piece=cow` - Check mint status + ownership
  - Returns: `{ piece, canMint, owner, minted, tokenId? }`
  - Checks `kidlisp-codes` for ownership
  - Checks Tezos contract for existing mint

- [ ] `POST /api/kidlisp-keep` - Self-service mint
  - Validates JWT from Auth0
  - Requires `@handle`
  - Verifies `piece.user === user.sub`
  - Signs transaction with admin key (server-side)
  - Returns: `{ tokenId, txHash, objktUrl }`

#### 2.2 UI Changes

**In `keep.mjs`:**
- [ ] Check ownership before showing mint button
- [ ] Show "Not your piece" message if user doesn't own it
- [ ] Show owner's `@handle` for pieces by others

**In kidlisp.com:**
- [ ] Add "Mint as Keep" button when viewing own saved piece
- [ ] Show "Login to mint" for anonymous users
- [ ] Add "My Keeps" section

#### 2.3 Implementation Order

1. **Create ownership check endpoint** (`GET /api/kidlisp-keep`)
2. **Update keep.mjs** to use ownership check
3. **Create self-service mint endpoint** (`POST /api/kidlisp-keep`)
4. **Add rate limiting** (5 mints/day per user)
5. **Test on mainnet staging**
6. **Add to kidlisp.com**

---

## Future Phases 📋

### Phase 3: Artist Edit Flow
- [ ] Let original artist edit their own token metadata
- [ ] Don't require admin for updates to owned tokens
- [ ] Add `/api/keep-edit` for artist-controlled updates

### Phase 4: Secondary Features
- [ ] `keeps` command to list user's minted tokens
- [ ] `keep:status $code` to check if already minted
- [ ] Collection thumbnail auto-update on each mint
- [ ] Pass `keepId` from keeps.mjs to oven for tracking

### Phase 5: kidlisp.com Gallery
- [ ] Recent keeps feed
- [ ] Trending keeps
- [ ] User profile pages with their keeps
- [ ] Fork/remix existing keeps

---

## Environment Variables

```env
# Already configured
KEEPS_NETWORK=mainnet
KEEPS_CONTRACT=KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM
KEEPS_RPC=https://mainnet.ecadinfra.com
PINATA_API_KEY=...
PINATA_API_SECRET=...
OVEN_URL=https://oven.aesthetic.computer

# Need for self-service (already have, just document)
TEZOS_KIDLISP_KEY=edsk...  # Admin signing key
```

---

## Files

| File | Purpose |
|------|---------|
| `system/public/aesthetic.computer/disks/keep.mjs` | Keep UI/flow |
| `system/public/aesthetic.computer/lib/keep/tezos-wallet.mjs` | Beacon wallet |
| `nanos/kidlisp-keep/index.mjs` | API endpoint (to create) |
| `tezos/keeps.mjs` | CLI tool |
| `tezos/KEEPS-SYSTEM.md` | Full documentation |
