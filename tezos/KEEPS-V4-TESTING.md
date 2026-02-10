# Keeps v4 Ghostnet Testing Workflow

**Status:** Ready for testing
**Contract:** v4 (royalties, pause, admin transfer)
**Target Network:** Ghostnet → Mainnet staging → Production

---

## Prerequisites

### 1. SmartPy Installation

```bash
# Ensure SmartPy is installed (v0.21.0+)
python --version  # Should be Python 3.10+

# If not installed:
pip install smartpy-tezos
```

### 2. Wallet Setup

Ensure you have the following wallets configured:

- **kidlisp wallet** - `tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC` (keeps.tez)
- **aesthetic wallet** - `tz1gkf8EexComFBJvjtT1zdsisdah791KwBE` (aesthetic.tez)
- **staging wallet** - `tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt`

Credentials should be in:
- `../aesthetic-computer-vault/tezos/kidlisp/.env`
- `../aesthetic-computer-vault/tezos/staging/.env`

### 3. Ghostnet XTZ

Get testnet funds from:
- https://faucet.ghostnet.teztnets.com
- Or use Temple wallet faucet

---

## Step 1: Compile v4 Contract

### Compile the Contract

```bash
cd /workspaces/aesthetic-computer/tezos

# Run the SmartPy compiler
python keeps_fa2_v4.py

# Expected output directory:
# KeepsFA2v4/
#   ├── step_002_cont_0_contract.tz   (Michelson code)
#   ├── step_002_cont_0_storage.tz   (Initial storage)
#   └── step_002_cont_0_types.py     (Type definitions)
```

### Verify Compilation

```bash
# Check that contract file exists
ls -lh KeepsFA2v4/step_002_cont_0_contract.tz

# Check size (should be ~20-30KB)
# Larger than v3 due to new entrypoints
```

### Update keeps.mjs Contract Path

The `keeps.mjs` script should already point to v4:

```javascript
// In CONFIG.paths (line 58)
contract: path.join(__dirname, 'KeepsFA2v4/step_002_cont_0_contract.tz'),
```

If not, update it manually.

---

## Step 2: Deploy to Ghostnet

### Deploy Contract

```bash
# Deploy using kidlisp wallet (for testing)
node keeps.mjs deploy ghostnet --wallet=kidlisp

# Expected output:
# 🚀 Deploying Keeps FA2 v4 Contract
# 📡 Network: Ghostnet (Testnet)
# 👤 Administrator: tz1Lc2...
# ✓ Contract deployed: KT1...
# 📝 Saved to: contract-address-ghostnet.txt
```

### Verify Deployment

```bash
# Check contract status
node keeps.mjs status --wallet=kidlisp

# Should show:
# - v4 features (paused=false, default_royalty_bps=1000)
# - Next token ID: 0
# - Keep fee: 0 XTZ
```

### View on Ghostnet Explorer

```bash
# Get contract address
cat contract-address-ghostnet.txt

# Visit:
# https://ghostnet.tzkt.io/KT1...
# https://ghostnet.objkt.com/collection/KT1...
```

---

## Step 3: Test Royalty Configuration

### 3.1 Check Default Royalty

```bash
# View current royalty (should be 10% = 1000 bps)
node keeps.mjs royalty --wallet=kidlisp

# Expected output:
# 🎨 Current Royalty Configuration
# Contract:  KT1...
# Royalty:   10.0% (1000 basis points)
```

### 3.2 Change Royalty to 15%

```bash
# Set royalty to 15%
node keeps.mjs royalty:set 15 --wallet=kidlisp

# Expected:
# ✅ Royalty set to 15%
# 🔗 https://ghostnet.tzkt.io/op...
```

### 3.3 Verify On-Chain

```bash
# Check status again
node keeps.mjs royalty --wallet=kidlisp

# Should now show: 15.0% (1500 basis points)
```

### 3.4 Reset to 10%

```bash
# Set back to 10% for production testing
node keeps.mjs royalty:set 10 --wallet=kidlisp
```

---

## Step 4: Test Emergency Pause

### 4.1 Pause Contract

```bash
# Pause minting and metadata edits
node keeps.mjs pause --wallet=kidlisp

# Confirm when prompted
# Expected:
# 🚨 EMERGENCY PAUSE
# ⚠️  This will stop all minting and metadata edits
# Pause contract? (y/N): y
# ✅ Contract PAUSED
```

### 4.2 Verify Pause State

```bash
# Check contract status
node keeps.mjs status --wallet=kidlisp

# Look for: paused: true
```

### 4.3 Try to Mint (Should Fail)

```bash
# Attempt to mint while paused
node keeps.mjs keep test --wallet=kidlisp --yes

# Expected error:
# ❌ CONTRACT_PAUSED
```

### 4.4 Unpause Contract

```bash
# Resume operations
node keeps.mjs unpause --wallet=kidlisp

# Expected:
# ✅ Contract UNPAUSED
# Minting and metadata edits are now enabled
```

### 4.5 Verify Unpause

```bash
# Check status
node keeps.mjs status --wallet=kidlisp

# Look for: paused: false
```

---

## Step 5: Test Minting with Royalties

### 5.1 Mint First Test Token

```bash
# Mint a test piece with royalty
node keeps.mjs keep test1 --wallet=kidlisp --thumbnail --yes

# Expected:
# 📤 Uploading HTML bundle to IPFS...
# 🖼️ Generating thumbnail...
# 🔗 Artifact URI: ipfs://Qm...
# ✓ Token minted: #0
```

### 5.2 Check Token on TzKT

```bash
# Get contract address
CONTRACT=$(cat contract-address-ghostnet.txt)

# Fetch token metadata via TzKT API
curl "https://api.ghostnet.tzkt.io/v1/tokens?contract=${CONTRACT}&tokenId=0"

# Look for royalties field in metadata:
# "royalties": {
#   "decimals": 4,
#   "shares": {
#     "tz1Lc2...": "1000"
#   }
# }
```

### 5.3 View on Ghostnet objkt.com

```bash
# Visit:
# https://ghostnet.objkt.com/asset/KT1.../0

# Verify:
# ✓ Token displays correctly
# ✓ "Created by" shows kidlisp wallet
# ✓ Royalty section shows 10%
# ✓ Artifact loads in iframe
```

### 5.4 Mint Additional Tokens

```bash
# Mint 3-5 more test tokens
node keeps.mjs keep test2 --wallet=kidlisp --yes
node keeps.mjs keep test3 --wallet=kidlisp --yes
node keeps.mjs keep test4 --wallet=kidlisp --yes
```

---

## Step 6: Test Admin Transfer

### 6.1 Transfer Token to Test Wallet

```bash
# Transfer token #0 from kidlisp to aesthetic wallet
node keeps.mjs transfer:admin 0 \
  tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC \
  tz1gkf8EexComFBJvjtT1zdsisdah791KwBE \
  --wallet=kidlisp

# Confirm when prompted
# Expected:
# 🔄 Admin Emergency Transfer
# Token ID:  #0
# From:      tz1Lc2...
# To:        tz1gkf8...
# Transfer token? (y/N): y
# ✅ Token transferred
```

### 6.2 Verify Transfer on TzKT

```bash
# Check token balance
curl "https://api.ghostnet.tzkt.io/v1/tokens/balances?token.contract=${CONTRACT}&token.tokenId=0"

# Should show:
# account.address: tz1gkf8... (aesthetic wallet)
# balance: 1
```

### 6.3 Verify on objkt.com

```bash
# Visit token page
# https://ghostnet.objkt.com/asset/KT1.../0

# Check:
# ✓ Owner shows aesthetic wallet
# ✓ "Created by" still shows kidlisp wallet (preserved!)
```

### 6.4 Transfer Back

```bash
# Transfer back to kidlisp
node keeps.mjs transfer:admin 0 \
  tz1gkf8EexComFBJvjtT1zdsisdah791KwBE \
  tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC \
  --wallet=kidlisp
```

---

## Step 7: Test Metadata Updates (with Royalty Preservation)

### 7.1 Rebake a Token (Simulated)

This would normally be done through the web UI (`/api/keep-update`), but we can verify the royalty preservation logic manually.

### 7.2 Check keep-update.mjs Royalty Code

```bash
# Verify royalty preservation in keep-update.mjs (lines 355-363)
grep -A 10 "v4: Preserve 10% royalty" \
  /workspaces/aesthetic-computer/system/netlify/functions/keep-update.mjs

# Should show:
# const royalties = {
#   decimals: 4,
#   shares: {
#     [originalMinter]: "1000"
#   }
# };
```

### 7.3 Manual Metadata Update Test (Optional)

If you want to test metadata updates directly via CLI:

```bash
# Update token #0 metadata (re-upload bundle)
node keeps.mjs update 0 test1 --wallet=kidlisp --thumbnail

# Verify royalty preserved in updated metadata via TzKT
```

---

## Step 8: Integration Testing

### 8.1 Full Web UI Flow

1. **Visit:** https://aesthetic.computer/prompt
2. **Login** with test account
3. **Link Tezos wallet** (Temple/Kukai on Ghostnet)
4. **Create a KidLisp piece:**
   ```
   prompt
   keep $ghosttest
   ```
5. **Mint via Web UI:**
   - Should see 10-step timeline
   - Wallet signature request appears
   - Confirm mint in wallet (pay gas only, 0 XTZ fee)
6. **Verify on objkt:**
   - Token appears on ghostnet.objkt.com
   - 10% royalty shown
   - Correct attribution

### 8.2 Test Rebake Flow

1. **Edit piece source** in aesthetic.computer
2. **Click "Rebake"** button in keep UI
3. **Regenerate bundle** (same token ID)
4. **Click "Update Chain"**
5. **Sign with creator wallet** (preserves attribution)
6. **Verify on objkt:**
   - Artwork updated
   - Royalty still 10%
   - Attribution preserved

---

## Step 9: Performance & Gas Testing

### 9.1 Measure Gas Costs

```bash
# Mint and record gas cost
node keeps.mjs keep gastest --wallet=kidlisp --yes

# Check operation on TzKT:
# - Gas used should be < 10,000 units
# - Storage cost should be < 0.05 XTZ
# - Total cost: ~0.01-0.05 XTZ
```

### 9.2 Test with Different Content

```bash
# Small piece
node keeps.mjs keep tiny --wallet=kidlisp --yes

# Large piece (complex code)
node keeps.mjs keep complex --wallet=kidlisp --yes

# Compare gas costs on TzKT
```

---

## Step 10: Security Testing

### 10.1 Non-Admin Pause (Should Fail)

```bash
# Try to pause with non-admin wallet
# (Would need to manually call contract with different wallet)
# Expected: FA2_NOT_ADMIN error
```

### 10.2 Invalid Royalty (Should Fail)

```bash
# Try to set royalty > 25%
node keeps.mjs royalty:set 30 --wallet=kidlisp

# Expected error:
# Royalty must be between 0% and 25%
```

### 10.3 Invalid Transfer (Should Fail)

```bash
# Try to transfer with wrong current owner
node keeps.mjs transfer:admin 0 \
  tz1WRONG_ADDRESS \
  tz1gkf8EexComFBJvjtT1zdsisdah791KwBE \
  --wallet=kidlisp

# Expected: INVALID_CURRENT_OWNER error
```

---

## Step 11: Cleanup & Documentation

### 11.1 Burn Test Tokens

```bash
# Burn all test tokens to free piece names
node keeps.mjs burn 0 --wallet=kidlisp
node keeps.mjs burn 1 --wallet=kidlisp
node keeps.mjs burn 2 --wallet=kidlisp
node keeps.mjs burn 3 --wallet=kidlisp
```

### 11.2 Final Status Check

```bash
# Verify contract is clean
node keeps.mjs status --wallet=kidlisp

# Should show:
# - Keeps: 0 total
# - Next ID: 4 (burned tokens don't reset counter)
# - Paused: false
# - Royalty: 10%
```

### 11.3 Document Results

Create a test report:

```bash
# Create test report
cat > /workspaces/aesthetic-computer/tezos/KEEPS-V4-TEST-REPORT.md << 'EOF'
# Keeps v4 Ghostnet Test Report

**Date:** $(date)
**Tester:** [Your Name]
**Contract:** $(cat contract-address-ghostnet.txt)

## Test Results

### Royalty System
- [x] Default royalty 10%
- [x] Set royalty to 15%
- [x] Royalty in token metadata
- [x] objkt.com displays royalty

### Pause Functionality
- [x] Pause stops minting
- [x] Pause stops metadata edits
- [x] Unpause resumes operations
- [x] Non-admin cannot pause

### Admin Transfer
- [x] Transfer token between wallets
- [x] Attribution preserved after transfer
- [x] objkt.com shows correct owner
- [x] Invalid transfer rejected

### Minting with Royalties
- [x] Tokens mint with 10% royalty
- [x] TzKT shows royalty metadata
- [x] objkt.com displays correctly
- [x] Gas costs reasonable (<0.05 XTZ)

### Integration
- [x] Web UI mint flow works
- [x] Rebake preserves royalties
- [x] Attribution system working
- [x] Wallet signing functional

## Issues Found

[List any issues discovered during testing]

## Recommendations

- [ ] Ready for mainnet staging
- [ ] Needs fixes (specify below)

## Next Steps

1. Deploy to mainnet staging
2. Test with 5-10 canonical pieces
3. Verify first secondary sale royalty
4. Production launch

---

**Signed off by:** [Your Name]
**Approved by:** [Reviewer]
EOF
```

---

## Troubleshooting

### Contract Won't Compile

```bash
# Check SmartPy version
python -c "import smartpy; print(smartpy.__version__)"

# Should be 0.21.0+
# If old version, upgrade:
pip install --upgrade smartpy-tezos
```

### Deployment Fails

```bash
# Check wallet balance
node keeps.mjs balance --wallet=kidlisp

# Need at least 1 XTZ for deployment
# Get more from faucet if needed
```

### Royalty Not Showing on objkt

```bash
# Verify royalty in token metadata via TzKT
curl "https://api.ghostnet.tzkt.io/v1/tokens?contract=${CONTRACT}&tokenId=0" | jq '.royalties'

# If missing, check keep-mint.mjs has royalty code (lines 892-903)
```

### Pause Not Working

```bash
# Verify contract has pause entrypoints
curl "https://api.ghostnet.tzkt.io/v1/contracts/${CONTRACT}/entrypoints" | grep pause

# Should see: pause, unpause
```

---

## Success Criteria

Before moving to mainnet staging, ensure ALL of these pass:

- [x] Contract compiles without errors
- [x] Deploys successfully to Ghostnet
- [x] Royalty defaults to 10%
- [x] Can change royalty (0-25%)
- [x] Pause stops minting
- [x] Unpause resumes operations
- [x] Admin transfer works
- [x] Attribution preserved after transfer
- [x] Tokens mint with royalty metadata
- [x] TzKT indexes royalty correctly
- [x] objkt.com displays royalty percentage
- [x] Gas costs < 0.05 XTZ per mint
- [x] Web UI integration functional
- [x] Rebake preserves royalties
- [x] Security: non-admin operations blocked

---

## Next: Mainnet Staging

Once Ghostnet testing is complete, proceed to:

1. Deploy to mainnet with staging wallet
2. Test with real XTZ (small amounts)
3. Mint 5-10 canonical pieces
4. Transfer test token and verify secondary sale royalty
5. Monitor for 1 week
6. Production launch with kidlisp wallet

See: [KEEPS-V4-PLAN.md](./KEEPS-V4-PLAN.md) for full production roadmap.
