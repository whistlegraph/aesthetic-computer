# 🔮 Tezos Wallet Rolodex

> Aesthetic Computer wallet credentials & contract registry.
> **Last updated**: December 26, 2025

---

## 📇 Wallet Directory

| Wallet | Address | Domain | Role | Network |
|--------|---------|--------|------|---------|
| **aesthetic** | `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` | aesthetic.tez | Personal identity, **Ghostnet contract admin** | Mainnet + Ghostnet |
| **kidlisp** | `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` | keeps.tez | Production NFT minting, **Mainnet contract admin** | Mainnet |
| **staging** | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | — | Mainnet testing | Mainnet |

---

## 📜 Deployed Contracts (Aligned to v3)

### Ghostnet (Testnet)

| Contract | Address | Admin | Status |
|----------|---------|-------|--------|
| **Keeps FA2 v2** | `KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc` | `aesthetic` | ✅ Active (empty - all tokens burned) |

- Explorer: https://ghostnet.tzkt.io/KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc
- Objkt: https://ghostnet.objkt.com/collection/KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc
- Next Token ID: 9
- Keep Fee: 0 XTZ (free)

### Mainnet (Staging v3)

| Contract | Address | Admin | Status |
|----------|---------|-------|--------|
| **Keeps FA2 v3** | `KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi` | `staging` | 🟢 Staging |

- Explorer: https://tzkt.io/KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi
- Objkt: https://objkt.com/collection/KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi

### Legacy (v2 - Archived)

| Contract | Address | Admin | Status |
|----------|---------|-------|--------|
| **Keeps FA2 v2** | `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM` | `kidlisp` | 🗄️ Legacy (do not use for new mints) |

- Explorer: https://tzkt.io/KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM
- Objkt: https://objkt.com/collection/KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM

---

## 👛 Wallet Details

### 1. aesthetic (Personal / Admin)

```
Address:  tz1gkf8EexComFBJvjtT1zdsisdah791KwBE
Domain:   aesthetic.tez
Keys in:  kidlisp/.env (AESTHETIC_ADDRESS, AESTHETIC_KEY)
```

**Roles:**
- ✅ Ghostnet Keeps contract administrator
- ✅ Can mint, burn, lock, redact tokens (Ghostnet)
- ✅ Can set fees and withdraw (Ghostnet)
- 🔜 Personal NFT collection holder (Mainnet)

**CLI Usage:**
```bash
node keeps.mjs status --wallet=aesthetic
node keeps.mjs mint $piece --wallet=aesthetic
```

---

### 2. kidlisp (Production)

```
Address:  tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC
Domain:   keeps.tez
Keys in:  kidlisp/.env (KIDLISP_ADDRESS, KIDLISP_KEY)
```

**Roles:**
- ✅ Mainnet Keeps contract administrator
- ✅ Production NFT minting service
- Default wallet for `keeps.mjs` commands

**CLI Usage:**
```bash
node keeps.mjs status              # Uses kidlisp by default
node keeps.mjs mint $piece
```

---

### 3. staging (Testing)

```
Address:  tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt
Domain:   —
Keys in:  staging/.env (STAGING_ADDRESS, STAGING_KEY)
```

**Roles:**
- 🧪 Mainnet contract testing before production
- 💸 Throwaway wallet (don't hold significant funds)

**CLI Usage:**
```bash
node keeps.mjs deploy mainnet --wallet=staging
```

---

## 📁 File Structure

```
tezos/
├── README.md                    # This rolodex
├── aesthetic/
│   └── .env                     # Legacy (keys now in kidlisp/.env)
├── kidlisp/
│   └── .env                     # Main credentials file
│       ├── KIDLISP_ADDRESS      # Production wallet
│       ├── KIDLISP_KEY          # Production secret key
│       ├── AESTHETIC_ADDRESS    # Admin wallet  
│       ├── AESTHETIC_KEY        # Admin secret key
│       └── ...                  # RPC endpoints, etc.
└── staging/
    └── .env                     # Staging wallet credentials
        ├── STAGING_ADDRESS
        └── STAGING_KEY
```

---

## 🚀 Deployment Roadmap (v3)

```
[✅] 1. Ghostnet Development
     └── Contract: KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc (v2)
     └── Admin: aesthetic wallet
     └── Status: Active, tested, all test tokens burned

[🟢] 2. Mainnet Staging (v3)
     └── Contract: KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi
     └── Admin: staging wallet
     └── Status: Soft launch testing
```

---

## 🔧 Quick Commands

```bash
# Check all wallet balances
curl -s "https://api.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE/balance"    # aesthetic
curl -s "https://api.tzkt.io/v1/accounts/tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC/balance"    # kidlisp  
curl -s "https://api.tzkt.io/v1/accounts/tz1TtAufdTNEP8uqAwswAmZHAZp38QEo8hFo/balance"    # staging

# Ghostnet balances
curl -s "https://api.ghostnet.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE/balance"
curl -s "https://api.ghostnet.tzkt.io/v1/accounts/tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC/balance"

# keeps.mjs commands
cd /workspaces/aesthetic-computer/tezos
node keeps.mjs status --wallet=aesthetic    # Ghostnet contract status
node keeps.mjs balance --wallet=aesthetic   # Wallet balance
node keeps.mjs fee                          # Current keep fee
```

---

## 🔐 Security Notes

- ⚠️ This is a **PRIVATE** repository — never commit to public repos
- 🔑 Different wallets for different purposes (separation of concerns)
- 💾 Keys stored in `.env` files, loaded by `keeps.mjs` via `loadCredentials()`
- 🛡️ `aesthetic` wallet is admin — protect this key carefully

---

## 🔗 Links

| Resource | URL |
|----------|-----|
| TzKT Explorer | https://tzkt.io |
| Ghostnet Explorer | https://ghostnet.tzkt.io |
| Tezos Domains | https://tezos.domains |
| Ghostnet Faucet | https://faucet.ghostnet.teztnets.com |
| Objkt (Ghostnet) | https://ghostnet.objkt.com |
| Objkt (Mainnet) | https://objkt.com |
