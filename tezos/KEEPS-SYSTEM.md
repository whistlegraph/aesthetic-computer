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

### Proposed Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   AC Frontend   │────▶│  Netlify Edge    │────▶│  Tezos Contract │
│   (React/JS)    │     │  /api/keeps/*    │     │  (SmartPy FA2)  │
└─────────────────┘     └──────────────────┘     └─────────────────┘
        │                       │
        │ Auth0 JWT             │ Verify:
        ▼                       ▼
┌─────────────────┐     ┌──────────────────┐
│     Auth0       │     │   Redis/KV       │
│  (handled user) │     │  (piece author)  │
└─────────────────┘     └──────────────────┘
```

### Authorization Flow

1. **User Authentication** (Auth0)
   - User logs in via Auth0
   - JWT contains `sub` (user ID) and custom claims
   - Only users with a `@handle` can access minting

2. **Piece Ownership Verification**
   - When user saves a piece, store: `piece_name → user_id` in Redis/KV
   - API endpoint checks: Does this user own this piece?
   - Reject if piece belongs to someone else

3. **Minting Authorization**
   - Netlify edge function validates JWT
   - Checks piece ownership
   - Signs transaction with admin key (server-side)
   - Submits to Tezos

### API Endpoints (Netlify)

| Endpoint | Method | Auth | Description |
|----------|--------|------|-------------|
| `/api/keeps/check` | GET | JWT | Check if piece can be minted |
| `/api/keeps/mint` | POST | JWT | Mint piece (server signs tx) |
| `/api/keeps/status` | GET | Public | Get token status |
| `/api/keeps/my-keeps` | GET | JWT | List user's minted keeps |

### Security Considerations

1. **Admin Key Protection**
   - Tezos private key stored in Netlify env vars
   - Never exposed to client
   - Server signs all transactions

2. **Rate Limiting**
   - Per-user mint limits (e.g., 5 per day)
   - Prevent spam/abuse

3. **Piece Ownership**
   - Verify via Redis: `pieces:{piece_name}:author`
   - Set when piece is first saved
   - Immutable once set (first-come-first-served)

4. **Handle Requirement**
   - Check Auth0 user metadata for `handle`
   - Only handled users can mint
   - Prevents anonymous spam

### Data Model

```
Redis Keys:
  pieces:{name}:author     → user_id (who created it)
  pieces:{name}:handle     → @handle (display name)
  users:{user_id}:pieces   → Set of piece names
  keeps:{piece_name}       → token_id (if minted)
```

### Implementation Steps

1. [ ] Create `/api/keeps/check` endpoint
2. [ ] Create `/api/keeps/mint` endpoint  
3. [ ] Add piece ownership tracking to save flow
4. [ ] Build minting UI in AC frontend
5. [ ] Add rate limiting
6. [ ] Test with local dev server
7. [ ] Deploy to production

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
