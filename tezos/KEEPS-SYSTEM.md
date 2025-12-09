# KidLisp Keeps - NFT Minting System

## Overview

"Keeps" are KidLisp pieces preserved as NFTs on Tezos. Each `$code` can only exist once at a time, ensuring uniqueness and provenance.

---

## Current Contract: `KT1Ah5m2kzU3GfN42hh57mVJ63kNi95XKBdM` (Ghostnet)

### Storage
| Field | Type | Description |
|-------|------|-------------|
| `administrator` | address | Admin wallet (can mint, burn, update, lock) |
| `content_hashes` | big_map[bytes, nat] | Maps piece name → token_id (prevents duplicates) |
| `contract_metadata_locked` | bool | If true, collection metadata is frozen |
| `ledger` | big_map[nat, address] | Token ownership (token_id → owner) |
| `metadata` | big_map[string, bytes] | Contract-level TZIP-16 metadata |
| `metadata_locked` | big_map[nat, bool] | Per-token metadata lock status |
| `next_token_id` | nat | Auto-incrementing token counter |
| `operators` | big_map | FA2 operator approvals |
| `token_metadata` | big_map[nat, record] | Per-token TZIP-21 metadata |

### Entrypoints

| Entrypoint | Access | Description |
|------------|--------|-------------|
| `keep` | Admin | Mint new token with full TZIP-21 metadata |
| `edit_metadata` | Admin | Update token metadata (if not locked) |
| `lock_metadata` | Admin | Permanently freeze token metadata |
| `burn_keep` | Admin | Destroy token and free piece name for re-mint |
| `set_contract_metadata` | Admin | Update collection metadata (if not locked) |
| `lock_contract_metadata` | Admin | Permanently freeze collection metadata |
| `transfer` | Owner/Operator | FA2 standard transfer |
| `balance_of` | Public | FA2 standard balance query |
| `update_operators` | Owner | FA2 operator management |

### Uniqueness Enforcement
- Each piece name (e.g., "cow", "roz") can only be minted once
- `content_hashes` big_map tracks: piece_name → token_id
- Burning removes the entry, allowing re-mint
- Checked client-side via TzKT API before IPFS upload

---

## CLI Commands (`node keeps.mjs`)

| Command | Description |
|---------|-------------|
| `deploy` | Deploy new contract |
| `status` | Show contract info |
| `balance` | Check wallet balance |
| `mint <piece> [--thumbnail]` | Mint piece with optional animated thumbnail |
| `update <token_id> <piece>` | Update token metadata |
| `lock <token_id>` | Permanently lock token metadata |
| `burn <token_id>` | Destroy token (allows re-mint) |
| `redact <token_id> [--reason="..."]` | Censor token content |
| `set-collection-media --image=<uri>` | Set collection thumbnail |
| `lock-collection` | Permanently lock collection metadata |

---

## Metadata Structure

### Token Metadata (TZIP-21)
```json
{
  "name": "$cow",
  "description": "(wipe \"blue\")\n(ink \"yellow\")\n...\n\nby @jeffrey\nac25namuc",
  "artifactUri": "ipfs://Qm...",
  "displayUri": "ipfs://Qm...",
  "thumbnailUri": "ipfs://Qm... (animated WebP)",
  "symbol": "KEEP",
  "tags": ["$cow", "KidLisp", "Aesthetic.Computer", "interactive"],
  "attributes": [
    { "name": "Language", "value": "KidLisp" },
    { "name": "Code", "value": "$cow" },
    { "name": "Author", "value": "@jeffrey" },
    { "name": "User Code", "value": "ac25namuc" },
    { "name": "Lines of Code", "value": "3" },
    { "name": "Dependencies", "value": "2" },
    { "name": "Packed", "value": "2025.12.9" },
    { "name": "Interactive", "value": "Yes" },
    { "name": "Platform", "value": "Aesthetic Computer" }
  ]
}
```

### Collection Metadata
```json
{
  "name": "KidLisp Keeps",
  "version": "2.0.0",
  "interfaces": ["TZIP-012", "TZIP-016", "TZIP-021"],
  "imageUri": "https://oven.aesthetic.computer/keeps/latest",
  "homepage": "https://aesthetic.computer"
}
```

---

## Infrastructure

### Services
| Service | URL | Purpose |
|---------|-----|---------|
| Oven | `https://oven.aesthetic.computer` | Thumbnail generation (Puppeteer + FFmpeg) |
| Grab | `https://grab.aesthetic.computer` | Static screenshot fallback |
| Pinata | IPFS pinning | Artifact and metadata storage |
| TzKT | `api.ghostnet.tzkt.io` | On-chain data queries |

### Thumbnail Generation
- **Format**: Animated WebP
- **Size**: 96x96 @ 2x density (192x192 actual)
- **Duration**: 8 seconds capture
- **FPS**: 10 capture → 20 playback
- **Quality**: 70

---

## Phase 2: Creator Authorization (PLANNED)

### Problem
Currently only admin can mint. We want:
1. Only **handled** users (with `@handle`) can mint
2. Users can only mint **their own** pieces (pieces they authored)
3. Minting should be self-service via web UI

### Existing Infrastructure

#### Authentication
- **Auth0** provides JWT tokens via `/userinfo` endpoint
- `authorize()` in `backend/authorization.mjs` validates tokens
- Returns `{ sub, email, email_verified, ... }`

#### Piece Ownership (Already Tracked!)
The `kidlisp-codes` MongoDB collection already stores:
```javascript
{
  code: "cow",           // Piece name
  source: "(wipe...)",   // Source code
  hash: "...",           // SHA-256 of source
  user: "auth0|123...",  // Creator's Auth0 sub ID ✅
  when: Date,            // Created timestamp
}
```

#### Handle Resolution
- `handleFor(userId)` in `backend/authorization.mjs` gets `@handle` from `sub`
- `fetchAuthorInfo(userId)` in `bundle-html.js` resolves handle + userCode

### Architecture

```
┌─────────────────┐     ┌──────────────────────────────┐     ┌─────────────────┐
│   AC Frontend   │────▶│  /api/kidlisp-keep           │────▶│  Tezos Contract │
│   (user clicks  │     │  (Netlify function)          │     │  (SmartPy FA2)  │
│    "Keep" btn)  │     │                              │     │                 │
└─────────────────┘     │  1. Validate JWT (Auth0)     │     └─────────────────┘
        │               │  2. Check user has @handle   │
        │ JWT Bearer    │  3. Verify piece ownership   │
        │ token         │  4. Check not already minted │
        ▼               │  5. Generate bundle & thumb  │
                        │  6. Upload to IPFS           │
                        │  7. Sign & submit Tezos tx   │
                        └──────────────────────────────┘
                                      │
                                      ▼
                        ┌──────────────────────────────┐
                        │  MongoDB `kidlisp-codes`     │
                        │  - code → user mapping       │
                        │  - piece ownership proof     │
                        └──────────────────────────────┘
```

### Authorization Flow

1. **User Authentication** (Auth0)
   ```javascript
   const user = await authorize({ authorization: req.headers.authorization });
   if (!user) return 401 Unauthorized;
   ```

2. **Handle Requirement**
   ```javascript
   const handle = await handleFor(user.sub);
   if (!handle) return 403 "You need an @handle to mint";
   ```

3. **Piece Ownership Verification**
   ```javascript
   const piece = await db.collection('kidlisp-codes').findOne({ code: pieceName });
   if (!piece) return 404 "Piece not found";
   if (piece.user !== user.sub) return 403 "You don't own this piece";
   ```

4. **Duplicate Check**
   ```javascript
   const duplicate = await checkDuplicatePiece(pieceName);
   if (duplicate.exists) return 409 "Already minted as token #X";
   ```

5. **Minting**
   - Generate bundle via existing `bundle-html.js` logic
   - Generate thumbnail via Oven
   - Upload to IPFS
   - Sign transaction with server-side admin key
   - Submit to Tezos

### API Endpoints

#### `POST /api/kidlisp-keep`
Mint a new keep (requires auth)

**Headers:**
- `Authorization: Bearer <JWT>`

**Body:**
```json
{
  "piece": "cow",
  "generateThumbnail": true
}
```

**Response:**
```json
{
  "success": true,
  "tokenId": 5,
  "txHash": "op...",
  "artifactUri": "ipfs://...",
  "objktUrl": "https://objkt.com/asset/KT1.../5"
}
```

**Errors:**
- `401` - Not authenticated
- `403` - No @handle, or not piece owner
- `404` - Piece not found
- `409` - Already minted

#### `GET /api/kidlisp-keep?piece=cow`
Check piece mint status (public)

**Response:**
```json
{
  "piece": "cow",
  "canMint": true,
  "owner": "@jeffrey",
  "minted": false
}
// or if minted:
{
  "piece": "cow",
  "canMint": false,
  "minted": true,
  "tokenId": 5,
  "objktUrl": "https://..."
}
```

### Security

1. **Admin Key Protection**
   - Tezos private key in Netlify env: `TEZOS_KIDLISP_KEY`
   - Never exposed to client
   - Server signs all transactions

2. **Ownership Enforcement**
   - Only `piece.user === user.sub` can mint
   - First saver owns the piece (existing behavior)
   - Admin can mint any piece (bypass)

3. **Rate Limiting** (Future)
   - Per-user limits (e.g., 5 mints/day)
   - Prevent spam

### Implementation Steps

1. [x] Document existing infrastructure
2. [ ] Implement `/api/kidlisp-keep` GET (check status)
3. [ ] Implement `/api/kidlisp-keep` POST (mint)
4. [ ] Add "Keep" button to UI when viewing own piece
5. [ ] Test locally with dev server
6. [ ] Deploy to production
7. [ ] Add rate limiting

### Environment Variables Needed

```env
# Netlify env vars (already have most of these)
TEZOS_KIDLISP_KEY=edsk...       # Admin signing key
TEZOS_CONTRACT_ADDRESS=KT1...   # Keeps contract
TEZOS_NETWORK=ghostnet          # or mainnet
PINATA_API_KEY=...              # For IPFS uploads
PINATA_API_SECRET=...
OVEN_URL=https://oven.aesthetic.computer
```

---

## Files

| File | Purpose |
|------|---------|
| `tezos/keeps_fa2_v2.py` | SmartPy contract source |
| `tezos/keeps.mjs` | CLI tool for minting/management |
| `tezos/contract-address.txt` | Current deployed contract |
| `oven/server.mjs` | Thumbnail generation server |
| `oven/grabber.mjs` | Puppeteer frame capture |

---

## Deployment History

| Date | Contract | Network | Notes |
|------|----------|---------|-------|
| 2025-12-09 | KT1Ah5m2kzU3GfN42hh57mVJ63kNi95XKBdM | Ghostnet | Current - with burn, redact |
| 2025-12-09 | KT1FvJyG4e6tRHdJLTjMhvi7mMrrAGkBCdBv | Ghostnet | Added piece-name uniqueness |
| 2025-12-09 | KT1CfExN8EcSMS5Pm2vzxpQKyzkijNHvGCdm | Ghostnet | Added content_hashes |
| 2025-12-09 | KT1N9jz6NJaBYW4LVhccZs6ttQMvFEAmkkSM | Ghostnet | First with metadata lock |
