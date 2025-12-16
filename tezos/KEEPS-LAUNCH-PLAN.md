# Keeps Launch Plan 🔮

**Status:** Ghostnet Testing (Pre-Mainnet)  
**Contract:** `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc` (Ghostnet)  
**Tokens Minted:** 7 (as of Dec 2024)

---

## Overview

Keeps are interactive KidLisp programs preserved as Tezos FA2 NFTs. Each keep bundles the complete source code, a self-contained HTML artifact, and metadata into an on-chain token that can be viewed on objkt.com or played directly from IPFS.

### Current Architecture

```
User Flow:
  prompt.mjs (keep $code) 
    → keep.mjs (timeline UI)
      → /api/keep-mint (SSE streaming)
        → bundle-html (HTML generation)
        → oven/grab-ipfs (thumbnail)
        → Pinata IPFS upload
      → tezos-wallet.mjs (Beacon SDK)
        → Temple/Kukai/etc wallet
        → Contract.keep() call
      → /api/keep-confirm (record in MongoDB)
```

---

## Phase 1: Ghostnet Hardening ✅ (Current)

### 1.1 Core Mint Flow Testing

| Test Case | Status | Notes |
|-----------|--------|-------|
| Basic mint from prompt.mjs | ✅ | `keep $code` works |
| Timeline visual feedback | ✅ | 10-step progress UI |
| Wallet connection (Temple) | ✅ | Beacon SDK integration |
| Wallet connection (Kukai) | 🔶 | Needs testing |
| Bundle HTML generation | ✅ | Self-contained artifact |
| Thumbnail WebP generation | ✅ | Animated via oven |
| IPFS pinning (Pinata) | ✅ | 3 files per keep |
| Duplicate prevention | ✅ | content_hashes bigmap |
| Already-minted UI | ✅ | Shows existing token info |
| Error handling | 🔶 | Improve user messages |

### 1.2 UI/UX Polish

**In `keep.mjs`:**
- [ ] Add "cancel" ability during preparation
- [ ] Better error messages (translate RPC errors)
- [ ] Show estimated gas/fees before signing
- [ ] Add "copy share link" after mint
- [ ] Improve thumbnail loading animation

**In `prompt.mjs`:**
- [ ] Show ꜩ balance in wallet button tooltip
- [ ] Add `keeps` command to list user's minted tokens
- [ ] Add `keep:status $code` to check if already minted
- [ ] Improve `.tez` domain resolution reliability

### 1.3 kidlisp.com Integration

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

### 1.4 API Improvements

**`/api/keep-mint`:**
- [ ] Add rate limiting (prevent spam)
- [ ] Validate piece exists before expensive operations
- [ ] Add retry logic for IPFS failures
- [ ] Log mint attempts to MongoDB for analytics

**`/api/keep-confirm`:**
- [ ] Index by user + tokenId for lookup
- [ ] Store authorship permanently

**New APIs needed:**
- [ ] `GET /api/keeps` - List recent keeps (paginated)
- [ ] `GET /api/keeps/user/:handle` - User's keeps
- [ ] `GET /api/keeps/code/:code` - Get keep by code

---

## Phase 2: Mainnet Preparation

### 2.1 Contract Deployment

```bash
# Deploy new contract to mainnet
cd tezos
node keeps.mjs deploy --network mainnet --wallet aesthetic

# Expected address: KT1... (will be different)
```

**Pre-deployment checklist:**
- [ ] Audit contract code (KeepsFA2v2)
- [ ] Verify storage structure matches
- [ ] Test all entrypoints on ghostnet
- [ ] Document admin operations

### 2.2 Configuration

**Environment Variables:**
```
TEZOS_NETWORK=mainnet
TEZOS_KEEPS_CONTRACT=KT1... (mainnet address)
TEZOS_MAINNET_RPC=https://mainnet.ecadinfra.com
```

**Wallet Configuration:**
- [ ] Set up mainnet minting wallet
- [ ] Configure appropriate gas limits
- [ ] Set mint fee (suggestion: 0.5-2 XTZ)

### 2.3 Network Switching

**In `keep.mjs` and `prompt.mjs`:**
- [ ] Add network selector (ghostnet/mainnet)
- [ ] Default to mainnet for production
- [ ] Show clear network indicator in UI
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

1. **Wallet reconnection flaky** - Sometimes Beacon doesn't restore session
2. **Error messages cryptic** - RPC errors shown raw to users
3. **No cancel during prep** - Users stuck if they change mind

### Medium Priority 🟡

1. **Thumbnail size** - Currently 256x256, could be larger
2. **No batch minting** - One at a time only
3. **Missing analytics** - No mint tracking

### Low Priority 🟢

1. **No royalties** - Could add FA2.1 royalty support
2. **No burns** - Tokens are permanent
3. **No collections** - All in one flat list

---

## Files & Locations

| Component | Location |
|-----------|----------|
| CLI tool | `tezos/keeps.mjs` |
| Contract (Michelson) | `tezos/KeepsFA2v2/` |
| Mint UI | `system/.../disks/keep.mjs` |
| Prompt commands | `system/.../disks/prompt.mjs` |
| Wallet lib | `system/.../lib/tezos-wallet.mjs` |
| Mint API | `system/backend/keep-mint.mjs` |
| Bundle API | `system/backend/bundle-html.mjs` |
| kidlisp.com | `kidlisp.com/index.html` |

---

## Next Steps (Today)

1. [ ] Test full mint flow on ghostnet via `keep $abc`
2. [ ] Test via kidlisp.com editor
3. [ ] Verify already-minted detection works
4. [ ] Check objkt.com display of existing tokens
5. [ ] Document any UX issues found

---

*Last updated: December 15, 2024*
