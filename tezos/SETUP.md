# Quick Setup for KidLisp Tezos

## 🚀 Quick Start

1. **Set up sensitive environment variables** (from vault):
   ```fish
   # From aesthetic-computer-vault directory
   ./devault.fish
   ```

2. **Enhanced CLI setup with beautiful JSON**:
   ```fish
   cd tezos
   source setup-enhanced.fish
   ```

3. **Configure KidLisp parameters**:
   ```fish
   source configure.fish
   ```

4. **Load beautiful JSON formatting**:
   ```fish
   source format.fish
   ```

5. **Explore the network**:
   ```fish
   source explore.fish
   network_info
   check_balance_all
   ```

6. **Deploy to Ghostnet** (testnet):
   ```fish
   ./deploy.fish ghostnet
   ```

## 🎨 Beautiful JSON Display

All APIs and commands support beautiful formatting:
```fish
## 🎨 Beautiful JSON Display with FX

Interactive JSON viewer and multiple formatters:
```fish
# Load formatting functions
source format.fish

# Interactive FX viewer (recommended) - navigate with arrows, search, collapse
octez-client rpc get /chains/main/blocks/head | fx_json

# Standard pretty format
curl -s "https://api.ghostnet.tzkt.io/v1/accounts/[address]" | pretty_json

# Compact single-line format
curl -s "https://api.ghostnet.tzkt.io/v1/contracts" | compact_json

# Flattened greppable format
curl -s "https://api.ghostnet.tzkt.io/v1/tokens" | flat_json

# See all formatter options
npm run json-demo
```

### FX Interactive Features:
- **Navigate**: Arrow keys to expand/collapse
- **Search**: `/` to search, `n` for next match
- **Filter**: `.key` to filter by key, `.[0]` for array items
- **Exit**: `q` to quit
```

## 🔑 Personal Wallet Integration

Add your mnemonic to vault/.env:
```bash
PERSONAL_WALLET_MNEMONIC="your twelve word mnemonic phrase here"
```

Then import with:
```fish
octez-client import secret key personal
# Paste mnemonic when prompted
```

## 📁 Structure

- `configure.fish` - KidLisp configuration (coin name, royalties, etc.)
- `deploy.fish` - Deployment script that uses vault env vars
- `.env` → symlink to `../aesthetic-computer-vault/tezos/.env`
- `KidLisp/` - Pre-compiled SmartPy contract files
- `scripts/` - Node.js deployment and interaction scripts

## 🔒 Security

- Private keys and sensitive data are stored in `aesthetic-computer-vault/tezos/.env`
- Configuration parameters are in `configure.fish` (version controlled)
- Never commit actual private keys to git

## 📋 Next Steps

See `TODO.md` for the complete development roadmap.
