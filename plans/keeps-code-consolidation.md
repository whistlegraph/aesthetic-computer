# Keeps Code Consolidation Plan

**Created:** 2024-12-26  
**Status:** Draft

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
