# Keeps Code Consolidation Plan

**Created:** 2025-12-26  
**Updated:** 2025-12-26  
**Status:** In Progress

---

## Change Log (Newest First)

### December 26, 2025 - Session 2: Wallet & Contract Inventory

#### Completed
- ✅ **Fixed Ghostnet contract reference** - `contract-address-ghostnet.txt` now points to `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc` (aesthetic admin)
- ✅ **Updated vault README** - Added mainnet contract, fixed staging wallet address
- ✅ **Funded staging wallet** - Sent 50 XTZ from aesthetic to staging on Ghostnet
- ✅ **Verified wallet inventory**:

| Wallet | Address | Domain | Ghostnet XTZ |
|--------|---------|--------|--------------|
| **aesthetic** | `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` | aesthetic.tez | ~925 |
| **kidlisp** | `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` | keeps.tez | ~885 |
| **staging** | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | — | 50 |

#### Contract Inventory

| Network | Contract | Admin Wallet | Status |
|---------|----------|--------------|--------|
| **Ghostnet** | `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc` | aesthetic | ✅ Active (empty) |
| **Mainnet** | `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM` | kidlisp | ✅ Active |

#### Next Steps
1. Run security tests on Ghostnet (now have funded wallets)
2. Test full mint → edit → lock → burn cycle
3. Verify transfer works between users

---

### December 26, 2025 - Session 1: Metadata & Security Audit

#### Completed
- ✅ Fixed `keep-update.mjs` to use `analyzeKidLisp` for rich traits (was using simplified attributes)
- ✅ Fixed `keeps.mjs` lock status display (now checks for `true`, not just existence)
- ✅ Verified objkt.com creator attribution preserved after metadata updates
- ✅ Completed security audit of `keeps_fa2_v2.py` contract

#### Findings
- **objkt "No data" for artist** may be UI caching issue - GraphQL shows correct creator data
- Token 4 ($berz) has correct `creators: ["tz1gkf8EexComFBJvjtT1zdsisdah791KwBE"]`
- Both on-chain and off-chain (IPFS) JSON have correct creator attribution

---

## Upcoming Tasks (Priority Order)

### 🔴 High Priority

1. ✅ **Code Consolidation Phase 1** - Extract constants (COMPLETED Dec 26)
   - [x] Create `lib/keeps/constants.mjs`
   - [x] Update `keeps-client.mjs` to use it
   - [x] Update `keep.mjs` disk to use it
   - [x] Update `kidlisp.com` to use it (dynamic import)

2. ✅ **Code Consolidation Phase 2** - Extract TzKT client (COMPLETED Dec 26)
   - [x] Create `lib/keeps/tzkt-client.mjs`
   - [x] Migrate `keep.mjs` to use it
   - [x] Migrate `keeps-client.mjs` to use it
   - [x] Functions: `checkIfMinted`, `fetchTokenInfo`, `fetchLedgerOwner`, `findTokenByName`, `isMetadataLocked`, `fetchAllTokens`

3. **Code Consolidation Phase 3** - Unify keeps-client.mjs
   - [ ] Make `keeps-client.mjs` the canonical browser client
   - [ ] Have `keep.mjs` use `KeepsClient` internally
   - [ ] Have `kidlisp.com` import and use `KeepsClient`

4. **Add `send` command to keeps.mjs** - For easier wallet transfers

### 🟡 Medium Priority

5. **Ghostnet Security Testing** - Now that wallets are funded
   - [ ] Mint as user (pay fee, verify firstMinter)
   - [ ] Mint as admin to user address (verify attribution)
   - [ ] Try duplicate content_hash (should fail)
   - [ ] Edit metadata on unlocked token
   - [ ] Try edit on locked token (should fail)
   - [ ] Burn token, re-mint same piece
   - [ ] Transfer token between users
   - [ ] Set/update operators
   - [ ] Withdraw fees
   - [ ] Lock collection metadata

6. **Add owner-can-burn option** - Security recommendation
   - Currently only admin can burn (users can't burn their own tokens)

### 🟢 Low Priority

7. **Metadata size limits** - Add max bytes checks in SmartPy
8. **Consider multisig** - For production admin key

---

## Contract Security Audit

### Contract: `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM` (Mainnet Staging)
### Source: `tezos/keeps_fa2_v2.py` (SmartPy)

---

### Entrypoint Analysis

#### 1. `keep` - Mint New Token
**Access Control:** Two modes
- **Admin mode:** Can mint to any `owner` address
- **User mode:** Must pay `keep_fee`, can only mint to themselves (`MUST_MINT_TO_SELF`)

**Security Features:**
- ✅ `DUPLICATE_CONTENT_HASH` - Prevents re-minting same piece
- ✅ `INSUFFICIENT_FEE` - Enforces payment for non-admin mints
- ✅ User is `firstMinter` when minting to self (proper artist attribution)

**Risks:**
- ⚠️ No metadata validation - malformed bytes could cause indexer issues
- ⚠️ No size limits on metadata fields - gas bombs possible
- ⚠️ Admin can mint to any address (impersonate artist on objkt)

---

#### 2. `edit_metadata` - Update Token Metadata
**Access Control:** Admin only (`FA2_NOT_ADMIN`)

**Security Features:**
- ✅ `METADATA_LOCKED` check - respects permanent lock
- ✅ `FA2_TOKEN_UNDEFINED` - validates token exists

**Risks:**
- ⚠️ Admin can change ANY unlocked token's metadata
- ⚠️ Could change `creators` array (though firstMinter preserved on-chain)
- ⚠️ No partial update - replaces entire `token_info` map

---

#### 3. `lock_metadata` - Permanently Lock Token
**Access Control:** Admin only

**Security Features:**
- ✅ One-way lock (cannot unlock)
- ✅ Validates token exists

**Status:** ✅ Secure

---

#### 4. `burn_keep` - Destroy Token
**Access Control:** Admin only (NOT token owner!)

**Security Features:**
- ✅ Clears `content_hash` - allows piece to be re-minted
- ✅ Clears `ledger`, `token_metadata`, `metadata_locked`

**Risks:**
- ⚠️ Admin can burn ANY token (not just their own)
- ⚠️ User cannot burn their own token (only admin can)
- ⚠️ Consider: Should owner be able to burn?

---

#### 5. `set_keep_fee` - Change Minting Fee
**Access Control:** Admin only
**Status:** ✅ Secure - simple admin-controlled value

---

#### 6. `withdraw_fees` - Withdraw Collected Fees
**Access Control:** Admin only

**Security Features:**
- ✅ Admin-only
- ✅ Sends to specified destination (allows cold wallet withdrawal)

**Status:** ✅ Secure

---

#### 7. `set_contract_metadata` - Update Collection Metadata
**Access Control:** Admin only, if not locked

**Security Features:**
- ✅ `CONTRACT_METADATA_LOCKED` check

**Status:** ✅ Secure

---

#### 8. `lock_contract_metadata` - Lock Collection
**Access Control:** Admin only
**Status:** ✅ Secure - one-way lock

---

#### 9. FA2 Standard Entrypoints (inherited)
- `transfer` - Standard FA2, owner/operator only ✅
- `update_operators` - Standard FA2, owner only ✅
- `balance_of` - View, public ✅

---

### Security Concerns Summary

| Priority | Issue | Recommendation |
|----------|-------|----------------|
| **High** | Admin can burn any token | Consider: Add owner-can-burn option |
| **High** | Admin can mint to any address | Consider: Log original requestor in metadata |
| **Medium** | No metadata size limits | Add max bytes checks in SmartPy |
| **Medium** | edit_metadata replaces all | Consider: merge update vs replace |
| **Low** | Admin key is hot wallet | Consider: Multisig for production |

---

### Storage Structure

```python
storage = {
  administrator: address,           # Admin wallet
  next_token_id: nat,               # Token ID counter
  ledger: big_map[nat, address],    # Token ID -> Owner
  token_metadata: big_map[nat, record],  # Token ID -> Metadata
  metadata: big_map[string, bytes], # Contract metadata (TZIP-16)
  metadata_locked: big_map[nat, bool],   # Token ID -> Locked?
  content_hashes: big_map[bytes, nat],   # Hash -> Token ID (dedup)
  contract_metadata_locked: bool,   # Collection lock flag
  keep_fee: mutez,                  # Required fee for user mints
  operators: big_map[...],          # FA2 operator permissions
}
```

---

### Recommended Tests

- [ ] Mint as user (pay fee, verify firstMinter)
- [ ] Mint as admin to user address (verify attribution)
- [ ] Try duplicate content_hash (should fail)
- [ ] Edit metadata on unlocked token
- [ ] Try edit on locked token (should fail)
- [ ] Burn token, re-mint same piece
- [ ] Transfer token between users
- [ ] Set/update operators
- [ ] Withdraw fees
- [ ] Lock collection metadata

---

## Problem Statement

We currently have **three separate implementations** for managing Keeps (KidLisp NFTs on Tezos):

| Context | File | Lines | Purpose |
|---------|------|-------|---------|
| **CLI** | `tezos/keeps.mjs` | ~2260 | Server-side minting, contract deployment, admin ops |
| **AC Piece** | `system/public/aesthetic.computer/disks/keep.mjs` | ~2480 | In-browser minting via prompt UI |
| **KidLisp.com** | `system/public/kidlisp.com/index.html` | inline JS | Keeps tab in kidlisp.com editor |

There's also a **partially-implemented shared client**:
- `system/public/aesthetic.computer/lib/keeps-client.mjs` (~550 lines)
- `system/public/aesthetic.computer/lib/tezos-wallet.mjs` (Beacon wallet wrapper)

**Neither `keep.mjs` (the disk) nor `kidlisp.com` use `keeps-client.mjs`.** They each have their own:
- Wallet connection logic
- TzKT API calls
- Mint flow state machines
- UI rendering

## Current Duplication

### 1. Wallet Connection
- **keeps.mjs (CLI)**: Uses Taquito `InMemorySigner` with server keys
- **keep.mjs (disk)**: Direct Beacon SDK via `api.wallet().connect()`
- **kidlisp.com**: Own `BeaconWallet` instance with custom init
- **keeps-client.mjs**: Wraps `tezos-wallet.mjs` (unused)

### 2. TzKT API Queries
All three implement their own:
- `checkIfAlreadyMinted()` / `checkIfMinted()` - content hash lookup
- `fetchExistingTokenInfo()` / `fetchTokenInfo()` - token metadata fetch
- Ledger owner lookup
- Attribute parsing

### 3. Contract Constants
Duplicated in each file:
```javascript
// keeps.mjs
CONFIG.paths.contractAddresses.mainnet = 'contract-address-mainnet.txt'

// keep.mjs
const KEEPS_CONTRACT = "KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM";

// kidlisp.com
const KEEPS_CONTRACT = "KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM";
const KEEPS_RPC = "https://mainnet.ecadinfra.com";

// keeps-client.mjs
export const NETWORKS = { mainnet: { contract: "KT1...", ... } }
```

### 4. Mint Step State Machine
- **keep.mjs**: Timeline array with `{ id, label, status, detail, time, startedAt, duration }`
- **keeps-client.mjs**: Similar but uses `MINT_STEPS` and `STEP_STATUS` enums
- **kidlisp.com**: No step tracking (simpler flow)

### 5. Analysis & Metadata Building
- **keeps.mjs (CLI)**: Full metadata building with attributes, TZIP-21 compliance
- **keep.mjs (disk)**: Calls server API (`/api/keep-mint`) for preparation
- **keeps-client.mjs**: Also calls `/api/keep-mint` SSE endpoint

## Proposed Architecture

### Shared Modules (Browser + Server)

```
system/public/aesthetic.computer/lib/keeps/
├── constants.mjs        # Contract addresses, networks, RPCs, step definitions
├── tzkt-client.mjs      # All TzKT API calls (check mint, fetch token, ledger)
├── mint-state.mjs       # Step state machine (browser-only UI state)
├── metadata.mjs         # TZIP-21 metadata building (shared logic)
└── index.mjs            # Re-exports all
```

### Server Modules

```
shared/keeps/
├── ipfs.mjs             # Pinata upload (server-only)
├── taquito.mjs          # Taquito contract calls (server-only, uses InMemorySigner)
├── bundle.mjs           # HTML bundle generation
└── oven.mjs             # Thumbnail generation via oven service
```

### Context-Specific UI

| Context | Shared Code | Own Code |
|---------|-------------|----------|
| **CLI** (`keeps.mjs`) | `tzkt-client`, `metadata`, `constants` + server modules | CLI interface, readline, file I/O |
| **keep.mjs** | `tzkt-client`, `mint-state`, `constants` | AC piece rendering, button UI |
| **kidlisp.com** | `tzkt-client`, `constants` | Inline keeps tab UI |
| **keeps-client.mjs** | Everything | Orchestration layer |

## Migration Steps

### Phase 1: Extract Constants (Low Risk)
1. Create `lib/keeps/constants.mjs` with:
   - `KEEPS_CONTRACT`, `KEEPS_NETWORK`, `KEEPS_RPC`
   - `NETWORKS` config object
   - `MINT_STEPS` definition
2. Update all three consumers to import from shared

### Phase 2: Extract TzKT Client
1. Create `lib/keeps/tzkt-client.mjs`:
   ```javascript
   export async function checkIfMinted(piece, contract, network)
   export async function fetchTokenInfo(tokenId, contract, network)
   export async function fetchLedgerOwner(tokenId, contract, network)
   ```
2. Migrate `keep.mjs` and `kidlisp.com` to use it
3. Keep CLI version separate (different runtime)

### Phase 3: Unify keeps-client.mjs
1. Make `keeps-client.mjs` the canonical browser client
2. Have `keep.mjs` use `KeepsClient` internally
3. Have `kidlisp.com` import and use `KeepsClient`

### Phase 4: Server Consolidation (Optional)
1. Move CLI-only logic to `shared/keeps/`
2. Ensure `/api/keep-mint` endpoint uses same code
3. Consider extracting Taquito helpers if used elsewhere

## Benefits

1. **Single source of truth** for contract addresses and networks
2. **Bug fixes propagate** to all contexts
3. **Easier testing** - can unit test shared modules
4. **Clearer separation** - UI vs logic vs blockchain
5. **Smaller bundles** - tree-shaking removes unused code

## Risks

1. **Breaking changes** during migration
2. **Different runtime requirements** (Node vs browser)
3. **Bundler complexity** for kidlisp.com (currently no bundler)
4. **Async loading** - kidlisp.com dynamically loads scripts

## Next Steps

1. [ ] Review this plan
2. [ ] Decide on Phase 1 scope
3. [ ] Create `lib/keeps/constants.mjs` 
4. [ ] Update one consumer (start with `keeps-client.mjs`)
5. [ ] Test and iterate

## Questions to Resolve

- Should kidlisp.com use ES modules or stay with inline scripts?
- Do we need a build step for kidlisp.com?
- Should the CLI (`keeps.mjs`) remain standalone or share code via npm workspace?
- Is `keeps-client.mjs` the right abstraction layer, or should we simplify?
