# KidLisp Keeps - Fee System Documentation

## Overview

The Keeps FA2 contract supports a configurable keep fee system that allows the admin to:
- Set a keep fee (in XTZ) that users must pay to keep NFTs
- Withdraw accumulated fees from the contract
- Change the fee at any time

---

## Fee Flow Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        KEEPING FLOW                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────┐     ┌─────────────┐     ┌──────────────────────┐ │
│  │   User   │────►│  API Server │────►│   Smart Contract     │ │
│  │ (Wallet) │     │ (keep-mint) │     │ (KeepsFA2v2)         │ │
│  └──────────┘     └─────────────┘     └──────────────────────┘ │
│       │                                        │               │
│       │                                        ▼               │
│       │                              ┌──────────────────────┐  │
│       │                              │   Contract Balance   │  │
│       │                              │   (Accumulated Fees) │  │
│       │                              └──────────────────────┘  │
│       │                                        │               │
│       ▼                                        ▼               │
│  ┌──────────────────┐              ┌──────────────────────┐   │
│  │ User pays keep   │              │  Admin withdraws to  │   │
│  │ fee via Beacon   │              │  designated wallet   │   │
│  │ wallet popup     │              │                      │   │
│  └──────────────────┘              └──────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Contract Storage

The contract stores the keep fee in the `keep_fee` field:

```python
# In SmartPy contract (keeps_fa2_v2.py)
self.data.keep_fee = sp.tez(0)  # Default: 0 XTZ (free)
```

The fee is stored in **mutez** (1 XTZ = 1,000,000 mutez).

---

## Contract Entrypoints

### `set_keep_fee(new_fee)`
Set the required keep fee.

| Parameter | Type | Description |
|-----------|------|-------------|
| `new_fee` | mutez | New fee amount in mutez |

**Access**: Admin only  
**Example**: Set fee to 5 XTZ → `set_keep_fee(5000000)`

### `withdraw_fees(destination)`
Withdraw all accumulated fees from the contract.

| Parameter | Type | Description |
|-----------|------|-------------|
| `destination` | address | Wallet to receive the fees |

**Access**: Admin only  
**Note**: Withdraws the entire contract balance.

### Fee Validation in `keep()`
The `keep` entrypoint validates that `sp.amount >= keep_fee`:

```python
# Validate keep fee payment
assert sp.amount >= self.data.keep_fee, "INSUFFICIENT_FEE"
```

---

## CLI Commands

### Check Current Fee
```bash
node keeps.mjs fee [network]
```

Example output:
```
╔══════════════════════════════════════════════════════════════╗
║  💰 Current Keep Fee                                         ║
╚══════════════════════════════════════════════════════════════╝

   Contract: KT1Ah5m2kzU3GfN42hh57mVJ63kNi95XKBdM
   Keep Fee: 5 XTZ (5000000 mutez)
```

### Set Keep Fee
```bash
node keeps.mjs set-fee <amount_in_tez> [network]
```

Examples:
```bash
# Set fee to 5 XTZ
node keeps.mjs set-fee 5

# Make keeping free
node keeps.mjs set-fee 0

# Set fee to 0.5 XTZ
node keeps.mjs set-fee 0.5

# Set fee on mainnet
node keeps.mjs set-fee 10 mainnet
```

### Withdraw Fees
```bash
node keeps.mjs withdraw [destination] [network]
```

Examples:
```bash
# Withdraw to admin wallet (default)
node keeps.mjs withdraw

# Withdraw to specific address
node keeps.mjs withdraw tz1YourWalletAddress...

# Withdraw on mainnet
node keeps.mjs withdraw tz1... mainnet
```

---

## Client Integration

### API Response (keep-mint.mjs)
The `/api/keep-mint` endpoint returns the keep fee in the `prepared` event:

```javascript
await send("prepared", {
  success: true,
  piece: pieceName,
  contractAddress: CONTRACT_ADDRESS,
  network: NETWORK,
  keepFee: 5, // XTZ - should be fetched from contract
  // ... other fields
});
```

### Wallet Integration (tezos-wallet.mjs)
The client wallet library sends the fee when calling the contract:

```javascript
const op = await contract.methodsObject.keep({
  ...mintParams,
  owner: stringToBytes(connectedAddress),
}).send({
  amount: 5, // XTZ keep fee
  mutez: false,
});
```

---

## Fee Configuration Best Practices

### Recommended Settings

| Environment | Recommended Fee | Rationale |
|-------------|-----------------|-----------|
| Ghostnet (testnet) | 0 XTZ | Free testing |
| Mainnet (production) | 1-10 XTZ | Cover IPFS costs + margin |

### Cost Breakdown (Per Keep)
| Item | Approximate Cost |
|------|------------------|
| Tezos gas fees | ~0.01-0.05 XTZ |
| IPFS storage (bundle) | ~$0.001 |
| IPFS storage (metadata) | ~$0.0001 |
| IPFS storage (thumbnail) | ~$0.001 |
| **Total cost** | ~0.05 XTZ + $0.003 |

### Revenue Model Example
With a 5 XTZ keep fee:
- 1,000 keeps = 5,000 XTZ (~$5,000 at $1/XTZ)
- Cost: ~50 XTZ + ~$3 IPFS = ~$53
- Net profit: ~$4,947

---

## Security Considerations

### Admin Key Protection
- The admin private key can set fees and withdraw funds
- Store securely in environment variables / vault
- Never expose in client-side code

### Fee Validation
- Contract enforces `sp.amount >= keep_fee`
- Transactions with insufficient payment are rejected
- Users see the fee in their wallet before signing

### Withdrawal Safety
- Only admin can call `withdraw_fees`
- Entire balance is withdrawn (no partial withdrawals)
- Destination must be a valid Tezos address

---

## Upgrading Existing Contracts

⚠️ **Important**: The fee system requires contract redeployment.

Existing contracts deployed before this update do NOT have:
- `keep_fee` storage field
- `set_keep_fee` entrypoint
- `withdraw_fees` entrypoint

To upgrade:
1. Deploy new contract with fee system
2. Migrate tokens (if needed) by minting to new contract
3. Update `contract-address.txt` to point to new contract
4. Update production environment variables

---

## Error Codes

| Error | Description |
|-------|-------------|
| `INSUFFICIENT_FEE` | Transaction amount less than required keep fee |
| `FA2_NOT_ADMIN` | Non-admin tried to set fee or withdraw |

---

## Future Enhancements

Potential improvements for the fee system:

1. **Dynamic pricing** - Fee based on piece complexity
2. **Partial withdrawals** - Withdraw specific amounts
3. **Fee recipients** - Split fees between multiple addresses
4. **Tiered pricing** - Different fees for different user tiers
5. **Auction system** - Time-limited keeping with dynamic fees

---

## Related Documentation

- [KEEPS-SYSTEM.md](./KEEPS-SYSTEM.md) - Full system documentation
- [keeps_fa2_v2.py](./keeps_fa2_v2.py) - SmartPy contract source
- [keeps.mjs](./keeps.mjs) - CLI tool source
