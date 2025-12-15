# Tezos Wallet Vault

This directory stores Tezos wallet credentials for Aesthetic Computer.

## Directory Structure

```
tezos/
├── README.md              # This file
├── aesthetic/             # Personal aesthetic.tez wallet
│   └── .env              # Address and private key
├── kidlisp/              # Production keeps.tez wallet (future)
│   └── .env              # Full credentials
└── staging/              # Mainnet staging/test wallet
    └── .env              # Full credentials for testing
```

## Wallets

### 1. aesthetic.tez (Personal)
- **Address**: `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE`
- **Domain**: aesthetic.tez
- **Network**: Mainnet
- **Purpose**: Primary personal wallet, AC identity
- **Balance**: ~2 XTZ

### 2. kidlisp / keeps.tez (Production)
- **Address**: `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC`
- **Domain**: keeps.tez (to be registered)
- **Network**: Mainnet
- **Purpose**: Production keeps contract, KidLisp NFTs
- **Balance**: ~3.4 XTZ
- **Status**: Reserved for production deployment

### 3. staging (Mainnet Testing)
- **Address**: `tz1TtAufdTNEP8uqAwswAmZHAZp38QEo8hFo`
- **Domain**: None
- **Network**: Mainnet
- **Purpose**: Testing contracts on mainnet before production
- **Balance**: 0 XTZ (needs funding)
- **Status**: NEW - Created Dec 14, 2025

## Funding the Staging Wallet

Send XTZ to `tz1TtAufdTNEP8uqAwswAmZHAZp38QEo8hFo` from aesthetic.tez or an exchange.

Recommended: ~10 XTZ for testing (contract deploy + several test keeps)

## Contract Deployment Strategy

1. **Ghostnet**: Initial development (free faucet XTZ)
   - Contract: `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc`
   
2. **Mainnet Staging**: Test with real XTZ using `staging` wallet
   - Deploy contract owned by staging wallet
   - Test full flow with Temple mobile
   
3. **Mainnet Production**: Deploy final version using `kidlisp` wallet
   - Contract owned by keeps.tez
   - Register keeps.tez domain

## Security Notes

- This is a **PRIVATE** repository
- Never commit to public repos
- Different wallets for different purposes:
  - `aesthetic/`: Personal identity
  - `kidlisp/`: Production NFTs
  - `staging/`: Throwaway testing

## Quick Commands

```bash
# Check balances
curl -s "https://api.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE/balance"  # aesthetic
curl -s "https://api.tzkt.io/v1/accounts/tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC/balance"  # kidlisp
curl -s "https://api.tzkt.io/v1/accounts/tz1TtAufdTNEP8uqAwswAmZHAZp38QEo8hFo/balance"  # staging
```

## Links

- [TzKT Explorer](https://tzkt.io/)
- [Tezos Domains](https://tezos.domains/)
- [Ghostnet Faucet](https://faucet.ghostnet.teztnets.com/)
