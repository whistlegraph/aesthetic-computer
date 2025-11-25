# Tezos KEEPS - KidLisp NFT Minting System

Tezos FA2 NFT contract for minting self-contained KidLisp pieces as on-chain NFTs.

## Status

✅ **Bundle System Working** - Fits in 256 KB Tezos storage limit!

### Current Bundle Performance

- **Test piece**: `$wwi` - `(wipe fade:red-rainbow)`
- **Final size**: 216 KB (Brotli compressed)
- **Tezos limit**: 256 KB
- **Headroom**: 40 KB (15.6%)
- **Files**: 17 essential (80% reduction from 87)

### FA2 Contract

- **Address**: `KT1S1sXpFiV4GGxLM3zWX4cDLVEhVp9yuD7b` (Ghostnet)
- **Deployed**: November 13, 2024
- **Standard**: TZIP-12 compliant FA2
- **Compiler**: SmartPy v0.23.1
- **Status**: Functional, tested

## Quick Commands

```bash
# Create ultra-minimal bundle for Tezos (< 256 KB)
node bundle-ultra-minimal-keep.mjs <piece-name>

# Example:
node bundle-ultra-minimal-keep.mjs wwi
# Output: keep-bundles/wwi-ultra-self-contained.html (216 KB)

# Research compression techniques
node compression-research.mjs <bundle-path>
```

## Bundle Optimization Details

### Key Achievements

1. **Aggressive Pruning** (87 → 17 files, 80% reduction):
   - Removed 3D graphics (lib/3d.mjs, Three.js)
   - Removed unused systems (world, prompt, nopaint)
   - Removed advanced input (hand, WebGPU, chat, networking)
   - Removed non-essential libs (sound, UI, gamepad, MIDI, USB)

2. **Brotli Compression** (51 KB better than gzip):
   - Gzip level 9: 200 KB → 267 KB (with base64)
   - Brotli level 11: 165 KB → 216 KB (with base64)
   - **Savings**: 51 KB (19% improvement)

3. **JavaScript Minification** (66-70% reduction):
   - Terser with unsafe optimizations
   - 3 compression passes
   - Dead code elimination

### Bundle Composition

**Core Files** (17 total):
- `boot.mjs`, `bios.mjs` - System bootstrap (229 KB minified)
- `lib/disk.mjs` - Piece loader (160 KB minified)
- `lib/kidlisp.mjs` - KidLisp interpreter (151 KB minified)
- `lib/graph.mjs` - Graphics engine (85 KB minified)
- Essential graphics/math libs (geo, 2d, pen, num)
- Minimal utilities (helpers, logs, store, platform, pack-mode)

## Tezos Wallet Vault

This directory stores Tezos wallet credentials in the private vault repo.

## Directory Structure

```
tezos/
├── aesthetic/          # Personal aesthetic.tez wallet
│   └── .env           # Address and domain only (keys managed separately)
└── kidlisp/           # KidLisp project wallet
    └── .env           # Full credentials (address, keys, mnemonic)
```

## Wallets

### aesthetic.tez (Personal)
- **Address**: tz1gkf8EexComFBJvjtT1zdsisdah791KwBE
- **Domain**: aesthetic.tez
- **Purpose**: Primary AC/aesthetic.computer wallet
- **Key Storage**: Private keys NOT stored in vault for security

### kidlisp (Project)
- **Address**: tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC
- **Domain**: kidlisp.tez (to be registered)
- **Purpose**: KidLisp project wallet for keeps, payments, etc.
- **Key Storage**: Full credentials stored in `kidlisp/.env`

## Security Notes

- This is a **PRIVATE** repository - safe to store credentials here
- Different security levels:
  - `aesthetic/`: Address only, keys managed externally
  - `kidlisp/`: Full credentials for project use
- Never commit wallet files to public repos
- Always verify you're in the vault repo before committing

## Usage

Scripts in `/workspaces/aesthetic-computer/tezos/` reference these wallets:

```bash
# Check all wallet balances
python3 /workspaces/aesthetic-computer/tezos/balance.py

# Check specific wallet
python3 /workspaces/aesthetic-computer/tezos/balance.py aesthetic
python3 /workspaces/aesthetic-computer/tezos/balance.py kidlisp
```

## Network

Currently configured for **Ghostnet** (testnet):
- Faucet: https://faucet.ghostnet.teztnets.com/
- Explorer: https://ghostnet.tzkt.io/

For mainnet deployment, update network configurations accordingly.
