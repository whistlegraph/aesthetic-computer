# Aesthetic Computer Tezos Keeps

A Tezos FA2 NFT contract for minting **Keeps** — immutable aesthetic.computer pieces stored on IPFS and the Tezos blockchain.

## Overview

**Keeps** are NFTs that preserve aesthetic.computer pieces permanently. Each Keep contains:
- Interactive HTML content hosted on IPFS
- Full TZIP-21 metadata (name, description, creators, etc.)
- Immutable on-chain storage with optional metadata locking
- Support for both Ghostnet (testing) and Mainnet

## Current Deployment

**Ghostnet Contract:** `KT1EMgN12CGoXRnj82dLUs57MKJZLnbK4EBS`

View on TzKT: https://ghostnet.tzkt.io/KT1EMgN12CGoXRnj82dLUs57MKJZLnbK4EBS

## Quick Start

### Prerequisites

1. **Node.js dependencies:**
```bash
npm install
```

2. **Environment variables** (create `.env` file):
```bash
# Tezos wallet
PRIVATE_KEY=edsk...

# Pinata IPFS (for uploads)
PINATA_API_KEY=your_key
PINATA_API_SECRET=your_secret
IPFS_GATEWAY=https://ipfs.aesthetic.computer/
```

3. **LIGO compiler:**  
Download from https://ligolang.org/docs/intro/installation  
Place `ligo` binary in this directory (gitignored - too large for repo)

### Mint a Keep (includes IPFS upload)

The minting script automatically uploads to IPFS and mints with full metadata:

```bash
node archive/mint_with_ipfs_ghostnet.js
```

This will:
1. Create an HTML iframe wrapper for the aesthetic.computer piece
2. Upload the directory structure to IPFS via Pinata
3. Generate IPFS CID (Content Identifier)
4. Mint an NFT with full TZIP-21 metadata
5. Return the token ID and IPFS URL

**Example output:**
```
✅ Minted token 2
   artifactUri: ipfs://bafybeiep5lx4mjw37aq7nu3gxrnzxgohp6rdkn4rhxznbtnjzlq7vnqd2i
   View on TzKT: https://ghostnet.tzkt.io/KT1EMgN12CGoXRnj82dLUs57MKJZLnbK4EBS/tokens/2
```

### Deploy a New Contract

```bash
node archive/deploy_keeps_ghostnet.js
```

This will:
- Compile `archive/keeps.mligo` to Michelson (requires ligo binary)
- Deploy to Ghostnet
- Return the new contract address

## Contract Features

### FA2 Standard

The contract implements the [FA2 (TZIP-12)](https://tzip.tezosagora.org/proposal/tzip-12/) standard with:
- ✅ `transfer` - Transfer tokens between addresses
- ✅ `balance_of` - Query token balances
- ✅ `update_operators` - Manage transfer operators

### Keep Operations

#### `keep` - Mint a new Keep
Mints a new NFT with complete TZIP-21 metadata (17 fields)

#### `edit_metadata` - Update metadata
Only the token owner can edit metadata (unless locked)

#### `lock_metadata` - Permanently lock metadata
Makes metadata immutable forever. Once locked, cannot be changed by anyone.

## TZIP-21 Metadata Structure

Each Keep includes complete [TZIP-21](https://tzip.tezosagora.org/proposal/tzip-21/) metadata:

```json
{
  "name": "aesthetic.computer Keep #1",
  "description": "An interactive generative art piece...",
  "artifactUri": "ipfs://bafybei...",
  "displayUri": "ipfs://bafybei...",
  "thumbnailUri": "ipfs://bafybei.../thumb.png",
  "decimals": 0,
  "symbol": "KEEP",
  "isBooleanAmount": true,
  "shouldPreferSymbol": false,
  "formats": [
    {
      "uri": "ipfs://bafybei...",
      "mimeType": "application/x-directory"
    }
  ],
  "tags": ["interactive", "generative", "aesthetic"],
  "attributes": [
    {"name": "Type", "value": "Interactive", "type": "string"},
    {"name": "Platform", "value": "aesthetic.computer", "type": "string"}
  ],
  "creators": ["tz1abc..."],
  "rights": "© 2025 Artist Name"
}
```

## IPFS Integration

### Directory Structure

Keeps are uploaded as IPFS directories (not ZIP files) for proper `application/x-directory` MIME type.

### CORS Handling

The `?token=true` parameter triggers special handling in aesthetic.computer:

1. **Detects null origin** from IPFS (file:// protocol)
2. **Enables PACK mode** to skip API calls that would fail with CORS
3. **Alternative:** API endpoints have proper CORS headers allowing requests from any origin

See commits `e80e78f2` and `2162ec6f` for CORS fixes.

## File Structure

```
tezos/
├── README.md                          # This file
├── archive/                           # Working files + old attempts
│   ├── keeps.mligo                    # ← LIGO source (281 lines)
│   ├── keeps_compiled.tz              # ← Compiled Michelson
│   ├── deploy_keeps_ghostnet.js       # ← Deployment script
│   ├── mint_with_ipfs_ghostnet.js     # ← Minting script
│   └── [old attempts...]              # Previous experiments
├── aesthetic/                         # Aesthetic-specific utilities
├── kidlisp/                           # KidLisp integration
├── package.json                       # Node dependencies
└── .gitignore                         # Excludes ligo, .env, etc.
```

**Note:** Working contract files are in `archive/` after cleanup.

## Compilation

### Manual Compilation

```bash
./ligo compile contract archive/keeps.mligo > archive/keeps_compiled.tz
```

## Testing on Ghostnet

1. **Fund wallet:** https://faucet.ghostnet.teztnets.com/
2. **Deploy:** `node archive/deploy_keeps_ghostnet.js`
3. **Mint:** `node archive/mint_with_ipfs_ghostnet.js`
4. **View:** https://ghostnet.tzkt.io/[CONTRACT_ADDRESS]

## Resources

- **TZIP-12 (FA2):** https://tzip.tezosagora.org/proposal/tzip-12/
- **TZIP-21 (NFT Metadata):** https://tzip.tezosagora.org/proposal/tzip-21/
- **LIGO Documentation:** https://ligolang.org/docs/intro/introduction
- **TzKT API:** https://api.tzkt.io/
- **Pinata IPFS:** https://www.pinata.cloud/

## License

Part of the aesthetic.computer project.
