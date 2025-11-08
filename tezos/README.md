# Tezos Wallet Vault

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
