# Keeps TUI Admin Interface

A terminal-based UI for managing Keeps NFT contracts across Tezos networks.

## Overview

The Keeps TUI will provide a single command-line interface to:
- Browse and manage multiple Keeps contracts (mainnet + ghostnet)
- Switch between wallet identities (rolodex)
- Perform admin operations on contracts
- View token holdings and minting activity

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    keeps-tui.mjs                            │
│  (Entry point - imports from keeps.mjs + vault loader)      │
├─────────────────────────────────────────────────────────────┤
│  Views:                                                     │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────────────────┐│
│  │ Contracts   │ │ Wallet      │ │ Token Browser           ││
│  │ Browser     │ │ Rolodex     │ │ (list/search tokens)    ││
│  └─────────────┘ └─────────────┘ └─────────────────────────┘│
├─────────────────────────────────────────────────────────────┤
│  Services:                                                  │
│  • TzKT API (read contract state, tokens, holders)          │
│  • Taquito (write operations with vault keys)               │
│  • Beacon SDK (optional - for external wallet signing)      │
└─────────────────────────────────────────────────────────────┘
```

## Wallet Rolodex

The TUI maintains a rolodex of wallet identities. **Private keys stay in vault only.**

### Wallet Sources

| Wallet        | Address                                 | Source                          | Role       |
|---------------|----------------------------------------|---------------------------------|------------|
| staging       | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | `vault/tezos/staging/.env`      | Testing    |
| kidlisp       | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | `vault/tezos/kidlisp/.env`      | Production |
| aesthetic     | `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` | Beacon (external wallet)        | Treasury   |
| custom        | (user specified)                        | Runtime input / Beacon          | Ad-hoc     |

### Key Loading Strategy

```javascript
// Vault loader - NEVER exposes keys outside this module
async function loadWalletFromVault(walletId) {
  const envPath = `${VAULT_PATH}/tezos/${walletId}/.env`;
  // Returns a signing function, NOT the raw key
  return {
    address: pubKeyHash,
    sign: async (payload) => signWithKey(privateKey, payload),
    source: 'vault'
  };
}

// For external wallets, use Beacon
async function loadExternalWallet() {
  const wallet = new BeaconWallet({ name: 'Keeps TUI' });
  await wallet.requestPermissions({ network: { type: NetworkType.MAINNET } });
  return {
    address: await wallet.getPKH(),
    sign: (payload) => wallet.sign(payload),
    source: 'beacon'
  };
}
```

## Contract Registry

The TUI tracks known contracts across networks:

```javascript
const CONTRACTS = {
  mainnet: {
    staging: {
      address: 'KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM',
      admin: 'tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt',
      label: 'Mainnet Staging',
      deployed: '2025-01-XX'
    },
    production: {
      address: null, // TBD
      admin: 'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE',
      label: 'Mainnet Production',
      deployed: null
    }
  },
  ghostnet: {
    testing: {
      address: 'KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K',
      admin: 'tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt',
      label: 'Ghostnet Testing',
      deployed: '2025-01-XX'
    }
  }
};
```

## TUI Screens

### 1. Main Menu

```
╔══════════════════════════════════════════════════════════╗
║              KEEPS CONTRACT ADMIN TUI                    ║
╠══════════════════════════════════════════════════════════╣
║                                                          ║
║  Active Wallet: staging (tz1dfo...4Gzt)                  ║
║  Network: mainnet                                        ║
║                                                          ║
║  [C] Contracts    - Browse & manage contracts            ║
║  [W] Wallets      - Switch wallet identity               ║
║  [T] Tokens       - Browse minted tokens                 ║
║  [M] Mint         - Mint a new Keep                      ║
║  [S] Settings     - Configure networks/RPCs              ║
║  [Q] Quit                                                ║
║                                                          ║
╚══════════════════════════════════════════════════════════╝
```

### 2. Contract Browser

```
╔══════════════════════════════════════════════════════════╗
║              CONTRACTS (mainnet)                         ║
╠══════════════════════════════════════════════════════════╣
║                                                          ║
║  → [1] KT1Ecs...79K  STAGING     0 tokens   $cow minted  ║
║    [2] KT1Abc...123  PRODUCTION  -- tokens  (not active) ║
║                                                          ║
║  ─────────────────────────────────────────────────────── ║
║  GHOSTNET:                                               ║
║    [3] KT1Stx...q79K  TESTING   5 tokens                 ║
║                                                          ║
║  [Enter] View contract  [A] Admin ops  [D] Deploy new    ║
║  [N] Switch network     [B] Back                         ║
╚══════════════════════════════════════════════════════════╝
```

### 3. Contract Detail View

```
╔══════════════════════════════════════════════════════════╗
║  KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM                     ║
║  Mainnet Staging                                          ║
╠══════════════════════════════════════════════════════════╣
║                                                          ║
║  Admin:     tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt         ║
║  Minter:    tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt         ║
║  Tokens:    1                                            ║
║  Balance:   0.05 ꜩ                                       ║
║                                                          ║
║  TOKEN LIST:                                             ║
║    #0  $cow    1 minted   holder: tz1gkf...BHE           ║
║                                                          ║
║  ─────────────────────────────────────────────────────── ║
║  ADMIN OPERATIONS (requires admin wallet):               ║
║  [1] set_metadata   [2] update_minter   [3] update_admin ║
║  [4] Toggle pause   [5] Withdraw balance                 ║
║                                                          ║
║  [O] Open on objkt  [T] Open on TzKT  [B] Back           ║
╚══════════════════════════════════════════════════════════╝
```

### 4. Wallet Rolodex

```
╔══════════════════════════════════════════════════════════╗
║              WALLET ROLODEX                              ║
╠══════════════════════════════════════════════════════════╣
║                                                          ║
║  → [1] staging    tz1dfo...Gzt  (vault)   0.95 ꜩ         ║
║    [2] kidlisp    tz1dfo...Gzt  (vault)   0.00 ꜩ         ║
║    [3] aesthetic  tz1gkf...BHE  (beacon)  --             ║
║    [4] + Add custom wallet                               ║
║                                                          ║
║  ─────────────────────────────────────────────────────── ║
║  CURRENT: staging                                        ║
║  Can sign: ✓ (vault key loaded)                          ║
║                                                          ║
║  [Enter] Switch to wallet  [B] Back                      ║
╚══════════════════════════════════════════════════════════╝
```

## CLI Commands

The TUI also supports direct CLI commands for scripting:

```bash
# List contracts
node keeps-tui.mjs contracts --network mainnet

# Switch wallet
node keeps-tui.mjs wallet use staging

# Mint a token
node keeps-tui.mjs mint --name '$butterfly' --wallet staging --network mainnet

# View token
node keeps-tui.mjs token 0 --contract KT1Ecs...

# Admin operations
node keeps-tui.mjs admin set-minter --address tz1abc... --contract KT1Ecs...
node keeps-tui.mjs admin set-metadata --uri ipfs://... --contract KT1Ecs...
```

## Security Model

### Critical Rules

1. **Private keys NEVER leave the vault**
   - Keys are loaded into memory only during signing
   - No key export or display functions
   - All signing happens through wrapper functions

2. **Vault structure**
   ```
   aesthetic-computer-vault/
   └── tezos/
       ├── staging/.env    # TEZOS_PRIVATE_KEY=edsk...
       ├── kidlisp/.env    # TEZOS_PRIVATE_KEY=edsk...
       └── wallets.json    # Address registry (no keys)
   ```

3. **External wallets via Beacon**
   - For wallets where vault key isn't available
   - Requires manual approval in wallet app
   - Used for `aesthetic.tez` (Jeffrey's main wallet)

4. **Audit logging**
   - All admin operations logged with timestamp
   - Logs stored in vault (not repo)
   - Includes: operation, wallet, contract, result

## Implementation Phases

### Phase 1: Read-Only Browser
- [ ] Contract list view
- [ ] Token browser
- [ ] TzKT integration
- [ ] Basic TUI framework (blessed or ink)

### Phase 2: Wallet Management
- [ ] Vault key loading
- [ ] Wallet rolodex UI
- [ ] Balance display
- [ ] Beacon SDK integration

### Phase 3: Admin Operations
- [ ] Mint via TUI
- [ ] set_metadata
- [ ] update_minter / update_admin
- [ ] Pause/unpause

### Phase 4: Deployment
- [ ] Deploy new contract from TUI
- [ ] Contract comparison (diff storage/code)
- [ ] Migration tools

## Dependencies

```json
{
  "dependencies": {
    "blessed": "^0.1.81",       // Terminal UI
    "@taquito/taquito": "^19",  // Tezos operations
    "@airgap/beacon-sdk": "^4", // External wallet connection
    "dotenv": "^16"             // Vault env loading
  }
}
```

## File Location

```
tezos/
├── keeps.mjs            # Existing CLI (mint, deploy, etc.)
├── keeps-tui.mjs        # NEW: Interactive TUI entry point
├── keeps-vault.mjs      # NEW: Secure vault key loader
├── keeps-contracts.mjs  # NEW: Contract registry
└── KEEPS-TUI-PLAN.md    # This document
```

## Notes

- Start with Phase 1 (read-only) to validate the TUI framework choice
- Consider using `ink` (React for CLI) if blessed feels dated
- The existing `keeps.mjs` commands will be wrapped, not replaced
- All destructive operations require confirmation prompt
