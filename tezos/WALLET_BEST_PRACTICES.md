# Tezos Wallet Integration Best Practices

## The Issue with Mnemonic Import

The `octez-client import keys from mnemonic` command has a significant limitation:
- It uses a **fixed BIP32 derivation path** 
- Different wallets (Temple, Kukai, Ledger) use **different derivation paths**
- This means importing your mnemonic often generates the **wrong address**

## What We Discovered

**Your Wallet**: `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` (aesthetic.tez)
**Mnemonic Import Generated**: `tz1a13KZoABJjaV7Lm4bxqbm4esgsSEcbHN4` ❌

This mismatch confirms the derivation path issue.

## Best Practices by Use Case

### 1. 📊 Data Exploration & Analysis (✅ Perfect!)
```fish
# All these work without importing the wallet:
check_aesthetic_balance                 # Balance on both networks
aesthetic_account_info mainnet         # Beautiful account data
curl -s "https://api.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE/operations" | fx
```

### 2. 💰 Real Mainnet Transactions (Use Your Existing Wallet!)
- **Temple Wallet** 
- **Kukai**
- **Ledger**
- **Any wallet you already use**

**Why this is better:**
- More secure (wallet handles key management)
- No derivation path issues
- Better UX for complex operations
- Integrated with dApps

### 3. 🧪 Development & Testing (Use Testnet Wallet!)
```fish
# Get testnet funds first:
# Visit: https://faucet.ghostnet.teztnets.xyz/
# Enter: tz1fELY2NAnS5QFS1UYshn8sYhUmGaEAgzaU

use_testnet
octez-client get balance for aesthetic-test
octez-client transfer 1 from aesthetic-test to tz1... 
```

### 4. 🔧 Smart Contract Development  
```fish
# Deploy to testnet first:
use_testnet
octez-client originate contract my_contract transferring 0 from aesthetic-test running "$(cat contract.tz)"

# Then use your real wallet for mainnet deployment
```

## Alternative Import Methods (Advanced)

If you need CLI access to your mainnet wallet, you could:

1. **Export private key** from your existing wallet (if supported)
2. **Use a derivation tool** to try different BIP32 paths
3. **Use Temple/Kukai CLI integration** (if available)

But honestly, the API + existing wallet approach is cleaner and safer.

## Summary

✅ **Use APIs for data** - Perfect for exploration
✅ **Use existing wallet for mainnet** - Secure & proven
✅ **Use testnet CLI wallet for development** - Safe experimentation
❌ **Don't rely on mnemonic import** - Derivation path issues

Your current setup is actually **ideal** for most use cases!
