# Getting Valid Tezos Keys

## 🔑 For Development/Testing (Ghostnet)

### Option 1: Generate New Keys
```fish
# Generate a new key pair
octez-client gen keys my_dev_key

# Show the address
octez-client show address my_dev_key

# Get testnet funds from faucet
# Visit: https://faucet.ghostnet.teztnets.xyz/
# Or: https://teztnets.xyz/ghostnet-faucet
```

### Option 2: Import from Faucet
1. Go to https://faucet.ghostnet.teztnets.xyz/
2. Download a JSON file with testnet account
3. Import it:
```fish
octez-client activate account faucet_account with ~/Downloads/tz1abc123.json
```

### Option 3: Use Temple/Kukai Wallet
1. Install Temple Wallet (browser extension)
2. Create account and switch to Ghostnet
3. Export private key from wallet
4. Import to CLI:
```fish
octez-client import secret key my_wallet unencrypted:edsk...
```

## 🔐 For Production (Mainnet)

### Generate Production Keys
```fish
# Generate production keys (KEEP THESE SAFE!)
octez-client gen keys mainnet_deployer

# Show address to fund
octez-client show address mainnet_deployer

# Fund this address with real XTZ from:
# - Exchange (Coinbase, Kraken, etc.)
# - DEX (Plenty, QuipuSwap, etc.)
# - Another wallet
```

### Security Best Practices
1. **Never commit private keys to git**
2. **Use hardware wallet for large amounts**
3. **Keep backups of private keys**
4. **Use separate keys for different purposes**
5. **Test everything on Ghostnet first**

## 🏦 Recommended Key Strategy

```fish
# Development
octez-client gen keys dev_deployer        # For contract deployment testing
octez-client gen keys dev_user           # For user testing

# Production  
octez-client gen keys prod_deployer      # For contract deployment
octez-client gen keys prod_treasury      # For treasury management
octez-client gen keys prod_royalties     # For royalty collection
```

## 💰 Funding Requirements

### Ghostnet (Free)
- Get free testnet XTZ from faucet
- No real value, unlimited supply

### Mainnet (Real Money)
- Contract deployment: ~0.5-2 XTZ
- Transaction fees: ~0.001-0.01 XTZ per transaction
- Token minting: ~0.01-0.05 XTZ per mint
- Keep 10-20 XTZ for operations

## 🔧 Integration with aesthetic.computer

Add your production keys to the vault:
```fish
# Edit vault env file
nano ../aesthetic-computer-vault/tezos/.env

# Add your keys:
CONTRACT_ADMIN_SECRET_KEY_GHOSTNET=edsk...your_ghostnet_key
CONTRACT_ADMIN_SECRET_KEY_MAINNET=edsk...your_mainnet_key

# Symlink to project
./devault.fish
```
