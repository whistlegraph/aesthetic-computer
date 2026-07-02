# 🔮 Tezos Wallet & Contract Rolodex

> Aesthetic Computer wallet credentials & contract registry.
> **Last updated**: July 1, 2026
>
> **Source of truth:** `tezos/contracts.json` (schema-validated). This README is the
> human-readable view — when the two disagree, `contracts.json` wins. Secret keys live in
> the vault (`aesthetic-computer-vault/wallets/wallets.json.gpg`), never in this repo.

---

## 📇 Wallet Directory

| Wallet | Address | Domain(s) | Role |
|--------|---------|-----------|------|
| **aesthetic** | `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` | aesthetic.tez, jas.tez | Primary identity + **keeps sales receiver** |
| **kidlisp** | `tz1fEjGQrEE2LXNKqcpTYAJV16pbbFpLeNyd` | keeps.tez, kidlisp.tez | **Production minter** (calls `keep()` on mainnet) |
| **keeps (admin/treasury)** | `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` | — | Contract **admin** + **treasury** (royalty + `withdraw_fees` receiver) |
| **staging** | `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt` | — | Mainnet staging / migration housekeeping |

> ⚠️ The minter wallet (`tz1fEjGQ…`) needs XTZ to mint. Keep it funded before any mint run.

---

## 📜 Active Contracts

### Mainnet (Production) — **v11**

| Field | Value |
|-------|-------|
| **Keeps FA2 v11** | `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB` |
| Source | `keeps_fa2_v11.py` |
| Deployed | 2026-03-09 by `tz1Lc2Dz…` |
| Keep fee | 2.5 XTZ |
| Royalties | 9% artist (900 bps) + 1% platform (100 bps) |
| Treasury | `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` |
| Status | ✅ **Active** |

- Explorer: https://tzkt.io/KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB
- Objkt: https://objkt.com/collection/KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB
- Launched 2026-03-09; ongoing activity logged in `tezos/KEEPS-MARKET-TIMELINE.md`.

### Ghostnet (Testnet) — **v3**

| Field | Value |
|-------|-------|
| **Keeps FA2 v3** | `KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K` |
| Status | 🧪 Testing (user-callable `keep()` entrypoint) |

- Explorer: https://ghostnet.tzkt.io/KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K

### Deprecated

| Network | Version | Address | Notes |
|---------|---------|---------|-------|
| Mainnet | v6 | `KT1J15kADMuRWh9kJZzosBeRBYPjYr7RvhoN` | First mainnet deploy (1 mint); superseded by v11 |
| Mainnet | v4 (staging) | `KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W` | Deprecated staging origination |
| Mainnet | v2 | `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM` | Early admin-only-mint contract; wrong first-minter attribution |
| Ghostnet | v1 | `KT1Ah5m2kzU3GfN42hh57mVJ63kNi95XKBdM` | Metadata encoding issues |
| Ghostnet | v2 | `KT1KRQAkCrgbYPAxzxaFbGm1FaUJdqBACxu9` | 189 test tokens; admin-only mint |

---

## 🔧 Quick Commands

```bash
# Active contract (from the registry)
jq -r '.activeContracts.mainnet.keeps' tezos/contracts.json   # -> KT1Q1irs...YwBB

# Wallet balances (mutez → divide by 1e6)
curl -s "https://api.tzkt.io/v1/accounts/tz1gkf8EexComFBJvjtT1zdsisdah791KwBE/balance"  # aesthetic
curl -s "https://api.tzkt.io/v1/accounts/tz1fEjGQrEE2LXNKqcpTYAJV16pbbFpLeNyd/balance"  # kidlisp minter
curl -s "https://api.tzkt.io/v1/accounts/tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC/balance"  # admin/treasury

# Contract fees waiting to withdraw
curl -s "https://api.tzkt.io/v1/contracts/KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB" | jq '.balance/1e6'

# keeps.mjs
node tezos/keeps.mjs status                 # contract status
node tezos/keeps.mjs balance --wallet=kidlisp
node tezos/keeps.mjs fee

# actual sales — price + piece + buyer (reads the ask/fulfill_ask marketplace,
# which objkt.com's own indexer does NOT surface)
node tezos/keeps-sales.mjs --limit=20       # --json, --network=ghostnet
```

For a full live market snapshot (floor, volume, owners, sales, mints today), see the
**Keeps Market Stats** flow in the repo root `SCORE.md`.

---

## 🔐 Security Notes

- 🔑 Secret keys live only in `aesthetic-computer-vault/wallets/wallets.json.gpg` — never commit keys.
- 🛡️ `keeps` (`tz1Lc2Dz…`) is the contract admin — protect this key carefully.
- 🧱 Separation of concerns: minter mints, treasury holds fees/royalties, aesthetic receives sales.

---

## 🔗 Links

| Resource | URL |
|----------|-----|
| TzKT Explorer | https://tzkt.io |
| Ghostnet Explorer | https://ghostnet.tzkt.io |
| Objkt (Mainnet) | https://objkt.com |
| Tezos Domains | https://tezos.domains |
| Keeps shop | https://buy.kidlisp.com |
