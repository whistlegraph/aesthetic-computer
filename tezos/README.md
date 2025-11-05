# Tezos Directory

Smart contracts, wallet management, and blockchain integration for aesthetic.computer.

## 🚀 Quick Start

```fish
# Setup the environment
./setup.fish

# Create a new wallet for KidLisp
python create_kidlisp_wallet.py

# Check all wallet balances
python balance.py

# Check specific wallet balance
python balance.py aesthetic  # aesthetic.tez personal wallet
python balance.py kidlisp    # KidLisp project wallet
```

## 📁 Structure

- **`requirements.txt`** - Python dependencies for Tezos development
- **`setup.fish`** - Setup script for the Tezos environment
- **`create_kidlisp_wallet.py`** - Wallet creation script for KidLisp project
- **`balance.py`** - Check wallet balances
- **`fa2_nft_template.py`** - FA2 NFT smart contract template
- **`.env`** - Configuration (DO NOT COMMIT - git ignored)

## 🔐 Wallet Management

### Configured Wallets

#### aesthetic.tez (Personal)
- **Address**: `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE`
- **Domain**: aesthetic.tez
- **Purpose**: Primary AC/aesthetic.computer wallet
- **Key Storage**: Private keys managed separately (not in vault)

#### kidlisp (Project)
- **Address**: `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC`
- **Domain**: kidlisp.tez (to be registered)
- **Purpose**: KidLisp project wallet for keeps, payments, etc.
- **Key Storage**: Full credentials in `aesthetic-computer-vault/tezos/kidlisp/.env`

### Creating the KidLisp Wallet

The KidLisp wallet has already been created and stored in the vault. To create additional wallets:

```bash
python create_kidlisp_wallet.py
```

This will generate:
- A new Tezos address
- A private key (encrypted)
- A mnemonic phrase for recovery
- Auto-save to `aesthetic-computer-vault/tezos/kidlisp/.env`

### Storing Keys Securely

**IMPORTANT**: Never commit wallet keys to public repositories!

The vault structure in `aesthetic-computer-vault`:

```
tezos/
├── aesthetic/          # Personal aesthetic.tez wallet
│   └── .env           # Address and domain only
└── kidlisp/           # KidLisp project wallet
    └── .env           # Full credentials (address, keys, mnemonic)
```

The `aesthetic-computer-vault` is a **private repository**, so credentials are safe there.

## 🌐 Tezos Domains

### Registering kidlisp.tez

To register the `kidlisp.tez` domain:

1. **Visit**: https://tezos.domains/
2. **Search**: kidlisp.tez
3. **Register**: Using the KidLisp wallet address
4. **Configure**: Point to KidLisp smart contracts

### Setting up DNS Records

Once registered, configure the domain:

```python
# Update domain records (example)
from pytezos import pytezos

client = pytezos.using(key='<kidlisp_key>', shell='https://mainnet.api.tez.ie')

# Set address record
client.contract('KT1...').updateRecord(
    domain='kidlisp.tez',
    address='<kidlisp_wallet_address>'
).send()
```

## 🔧 Development

### Networks

- **Mainnet**: `https://mainnet.api.tez.ie`
- **Ghostnet (Testnet)**: `https://ghostnet.ecadinfra.com`

### Environment Variables

Create a `.env` file (DO NOT COMMIT):

```bash
TEZOS_RPC_URL=https://mainnet.api.tez.ie
TEZOS_TESTNET_RPC_URL=https://ghostnet.ecadinfra.com

# KidLisp Wallet (stored in vault)
KIDLISP_WALLET_ADDRESS=tz1...
KIDLISP_WALLET_PRIVATE_KEY=edsk...
KIDLISP_WALLET_MNEMONIC=word word word...
```

## 📦 Dependencies

All Python dependencies are managed in `requirements.txt`:

```bash
pip install -r requirements.txt
```

Dependencies included in the dev container Dockerfile:
- `pytezos>=3.9.0,<4` - Python SDK for Tezos
- `tezos-smartpy==0.22.0` - SmartPy compiler (x86_64 only)
- `octez-client` - Tezos CLI tool
- `octez-node` - Tezos node binary

## 🎯 KidLisp Project

The KidLisp wallet will be used for:
- **Minting**: KidLisp code as NFTs
- **Keeps**: Payment system for "keeping" pieces
- **Smart Contracts**: FA2 token contracts
- **Domain**: kidlisp.tez identity

## 📚 Resources

- [PyTezos Documentation](https://pytezos.org/)
- [Tezos Domains](https://tezos.domains/)
- [SmartPy Documentation](https://smartpy.io/)
- [Tezos Developer Portal](https://tezos.com/developers/)

## 🔒 Security Notes

1. **Never commit private keys** to version control
2. **Store keys in aesthetic-computer-vault** (separate private repo)
3. **Use environment variables** for sensitive data
4. **Keep mnemonic phrases** offline in secure storage
5. **Test on Ghostnet first** before mainnet deployment

---

*Part of the aesthetic.computer blockchain integration*
