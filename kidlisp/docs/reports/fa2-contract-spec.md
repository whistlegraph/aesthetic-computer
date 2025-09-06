# KidLisp FA2 Contract Specification

*For Feral File Integration*  
*Date: September 1, 2025*

## Contract Overview

**Name**: KidLisp Creative Tokens  
**Symbol**: KIDLISP  
**Standard**: FA2 (Tezos Fungible Asset Standard)  
**Type**: 1/1 Unique Tokens (Non-Fungible)

## Fee Structure

### Minting Fees (Paid by User)
- **Feral File Device Fee**: 2.5% of mint price
- **Aesthetic Computer Development Fee**: 2.5% of mint price  
- **Objkt Marketplace Fee**: 2.5% of mint price
- **Creator Receives**: 92.5% of mint price

### Secondary Sales (Automatic Royalties)
- **Creator Royalty**: 10%
- **Feral File Fee**: 2.5%
- **Aesthetic Computer Fee**: 2.5%
- **Objkt Fee**: 2.5% (handled by marketplace)

## Smart Contract Functions

### Core FA2 Functions
```python
# Standard FA2 interface
def transfer(params):
    # Handle token transfers with royalty calculations

def balance_of(params):
    # Return token balances

def update_operators(params):
    # Manage token operators
```

### KidLisp-Specific Functions
```python
def mint_kidlisp_token(code_hash, creator_address, metadata_uri, mint_price):
    """
    Mint a new 1/1 KidLisp token
    
    Args:
        code_hash: SHA-256 hash of KidLisp source code
        creator_address: Tezos address of the creator
        metadata_uri: IPFS/HTTP URI for token metadata
        mint_price: Price in mutez (1 tez = 1,000,000 mutez)
    
    Fees automatically distributed:
        - Feral File: mint_price * 0.025
        - Aesthetic Computer: mint_price * 0.025  
        - Objkt: mint_price * 0.025
        - Creator: mint_price * 0.925
    """

def get_token_for_hash(code_hash):
    """
    Check if token exists for given code hash
    Returns: token_id or None
    """

def get_creator(token_id):
    """
    Get original creator address for token
    """

def get_code_hash(token_id):
    """
    Get original KidLisp code hash for token
    """

def update_fees(new_feral_fee, new_ac_fee, new_objkt_fee):
    """
    Admin function to update fee percentages
    Only callable by contract admin
    """
```

## Storage Structure

```python
{
    # Token storage
    "tokens": big_map(token_id -> {
        "owner": address,
        "creator": address,
        "code_hash": string,
        "metadata_uri": string,
        "mint_timestamp": timestamp,
        "mint_price": mutez
    }),
    
    # Code hash to token mapping (prevent duplicates)
    "code_to_token": big_map(code_hash -> token_id),
    
    # Fee recipients
    "feral_file_address": address,
    "aesthetic_computer_address": address,
    "objkt_address": address,
    
    # Fee percentages (in basis points, 250 = 2.5%)
    "feral_fee_bp": nat,
    "ac_fee_bp": nat,
    "objkt_fee_bp": nat,
    "creator_royalty_bp": nat,
    
    # Admin
    "admin": address,
    "next_token_id": nat,
    
    # FA2 required storage
    "ledger": big_map((address, token_id) -> nat),
    "operators": big_map((address, address, token_id) -> unit),
    "token_metadata": big_map(token_id -> token_metadata)
}
```

## Metadata Format

```json
{
    "name": "KidLisp Creation #001",
    "description": "Interactive code art created with KidLisp",
    "image": "https://aesthetic.computer/$abc123.png",
    "animation_url": "https://aesthetic.computer/$abc123",
    "attributes": [
        {
            "trait_type": "Creator Handle",
            "value": "@alice"
        },
        {
            "trait_type": "Code Hash",
            "value": "a1b2c3..."
        },
        {
            "trait_type": "Creation Date",
            "value": "2025-09-01"
        },
        {
            "trait_type": "Device",
            "value": "Feral File FF-X1"
        },
        {
            "trait_type": "Code Length",
            "value": 156
        },
        {
            "trait_type": "Functions Used",
            "value": ["wipe", "ink", "circle", "line"]
        }
    ],
    "external_url": "https://aesthetic.computer/$abc123",
    "kidlisp_source": "(wipe blue)\n(ink red)\n(circle 50 50 25)"
}
```

## Minting Flow

1. **User Creates KidLisp Piece**
   - Code gets cached as `$abc123`
   - SHA-256 hash generated: `a1b2c3d4e5f6...`

2. **User Chooses to Mint**
   - Clicks "Make This a 1/1 Token" in FF-X1 interface
   - Sets mint price (minimum 1 tez)

3. **Contract Validation**
   - Check if token already exists for this code hash
   - If exists: reject with error "Token already minted"
   - If new: proceed with minting

4. **Fee Distribution**
   ```
   Mint Price: 10 tez
   - Feral File: 0.25 tez (2.5%)
   - Aesthetic Computer: 0.25 tez (2.5%)
   - Objkt: 0.25 tez (2.5%)
   - Creator: 9.25 tez (92.5%)
   ```

5. **Token Creation**
   - Mint token with unique ID
   - Store metadata on IPFS
   - Emit mint event for indexers

## Secondary Sales Royalties

When token is sold on Objkt or other marketplaces:

```
Sale Price: 50 tez
- Seller: 42.5 tez (85%)
- Creator Royalty: 5 tez (10%)
- Feral File: 1.25 tez (2.5%)
- Aesthetic Computer: 1.25 tez (2.5%)
```

Royalties enforced through FA2 transfer hooks that calculate and distribute fees automatically.

## Security Features

- **Duplicate Prevention**: Code hash mapping prevents multiple tokens for same code
- **Creator Verification**: Only authenticated users can mint tokens
- **Admin Controls**: Fee updates require admin approval
- **Audit Trail**: All mints/transfers logged for transparency

## Integration Points

### Aesthetic Computer
- [Auto-minting backend](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/netlify/functions/store-kidlisp.mjs#L845-L870)
- [Tezos integration](https://github.com/whistlegraph/aesthetic-computer/blob/main/tezos/src/integration.js)

### Feral File FF-X1
- iframe integration via `window.postMessage`
- "Mint Token" button in KidLisp interface
- Gallery view of minted pieces

### Objkt Marketplace
- Automatic token indexing
- Secondary sales with royalty distribution
- Collection page for KidLisp tokens

## Deployment Plan

1. **Testnet Deployment** (Ghostnet)
   - Deploy contract with test addresses
   - Integration testing with AC backend
   - FF-X1 device testing

2. **Mainnet Deployment** 
   - Deploy to Tezos mainnet
   - Update AC production endpoints
   - Go live with FF-X1 integration

3. **Marketplace Integration**
   - Register collection with Objkt
   - Configure royalty settings
   - Launch gallery curation

---

**Contract Repository**: [tezos/KidLisp/](https://github.com/whistlegraph/aesthetic-computer/tree/main/tezos/KidLisp)  
**Integration Code**: [tezos/src/integration.js](https://github.com/whistlegraph/aesthetic-computer/blob/main/tezos/src/integration.js)
