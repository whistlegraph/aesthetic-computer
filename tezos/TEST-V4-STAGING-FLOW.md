# 🧪 Test v4 Staging Contract - User Flow
**Date:** February 13, 2026
**Goal:** Verify v4 staging contract works before deploying v5 to production

---

## Prerequisites

Make sure you're in the tezos directory:
```bash
cd /workspaces/aesthetic-computer/tezos
```

---

## Step 1: Check Contract Status

First, let's verify the staging contract is accessible:

```bash
# Check current contract address (should be v4 staging)
cat contract-address-mainnet.txt

# Check contract status
node keeps.mjs status mainnet
```

**Expected output:**
- Contract address (should start with `KT1...`)
- Admin wallet address
- Current fee (probably 0 XTZ)
- Next token ID

---

## Step 2: Check Your Wallet Balance

Make sure you have some XTZ for testing:

```bash
# Check balance (uses staging wallet by default)
node keeps.mjs balance mainnet
```

**Expected:** At least 5-10 XTZ for testing

**If low balance:** You might be using the wrong wallet. The default is `staging` wallet.

---

## Step 3: Create a Test KidLisp Piece

First, create a simple KidLisp piece to mint:

**Option A: Use aesthetic.computer UI**
1. Go to https://aesthetic.computer
2. Type a simple KidLisp piece:
   ```lisp
   (wipe "navy")
   (ink "yellow")
   (circle (wiggle width) (wiggle height) 50)
   ```
3. Hit save/cache
4. Note the $code (e.g., `$xyz`)

**Option B: Use an existing $code**
- Pick any existing KidLisp $code from the database
- Example: `$cow`, `$wand`, `$test`, etc.

---

## Step 4: Test Mint (Admin Mode - No Fee)

Since you're using the staging wallet (which should be admin), you can mint without paying fees:

```bash
# Mint a test keep
node keeps.mjs mint test1 mainnet

# Or mint with thumbnail generation
node keeps.mjs mint test1 mainnet --thumbnail
```

**What happens:**
1. Script fetches KidLisp source from `https://aesthetic.computer/$test1`
2. Creates IPFS bundle (HTML + metadata)
3. Uploads to Pinata IPFS
4. Calls contract `keep()` entrypoint
5. Mints NFT to staging wallet

**Expected output:**
```
📦 Fetching piece: $test1
✓ Source loaded
📤 Uploading to IPFS...
✓ Uploaded: ipfs://QmXXX...
🔗 Transaction: https://tzkt.io/opXXX...
✅ Keep minted! Token ID: 0
```

---

## Step 5: Verify on TzKT Explorer

After minting, check the transaction on TzKT:

```bash
# The output will include a TzKT link like:
# https://tzkt.io/opXXX.../12345
```

**Click the link and verify:**
- ✅ Transaction status: `applied` (success)
- ✅ Sender: staging wallet address
- ✅ Contract: KT1... (your staging contract)
- ✅ Entrypoint: `keep`
- ✅ Token minted: Check token ID

---

## Step 6: Check Token Metadata

View the token metadata to ensure it's properly formatted:

```bash
# Via TzKT API (replace {contract} and {token_id})
curl https://api.tzkt.io/v1/contracts/{contract_address}/bigmaps/token_metadata/keys/{token_id}
```

**Or visit TzKT UI:**
```
https://tzkt.io/{contract_address}/storage
```

**Check for:**
- ✅ `name`: "KidLisp $test1" (or similar)
- ✅ `artifactUri`: IPFS link
- ✅ `displayUri`: IPFS link
- ✅ `thumbnailUri`: IPFS link (if --thumbnail used)
- ✅ `royalties`: Should show 10% royalty info (v4 feature!)
- ✅ `creators`: Should list creator wallet

---

## Step 7: View on objkt.com

Check if the token appears on objkt.com:

```
https://objkt.com/collection/{contract_address}
```

**Verify:**
- ✅ Collection appears
- ✅ Token displays with thumbnail (may take 1-5 mins to index)
- ✅ Metadata shows correctly
- ✅ Royalty info visible (10%)

**Note:** objkt.com might take a few minutes to index new tokens.

---

## Step 8: Test User Mint (With Fee - Optional)

If you want to test the fee system, you can set a fee and mint from a non-admin wallet:

```bash
# Set fee to 2.5 XTZ (requires admin wallet)
node keeps.mjs set-fee 2.5 mainnet

# Check fee was set
node keeps.mjs fee mainnet
# Should show: "Current fee: 2.5 XTZ"
```

**Then try minting from non-admin wallet:**
- This would require connecting a different wallet
- The mint should require 2.5 XTZ payment
- For now, you can skip this and just test admin minting

---

## Step 9: Check Royalty Settings (v4 Feature)

Check the default royalty configuration:

```bash
# Check royalty percentage
node keeps.mjs royalty mainnet
```

**Expected:** "Default royalty: 10% (1000 basis points)"

---

## Step 10: Test Emergency Pause (v4 Feature - Optional)

Test the pause functionality (careful - this will block ALL minting):

```bash
# Pause the contract
node keeps.mjs pause mainnet

# Try to mint (should fail)
node keeps.mjs mint test2 mainnet
# Should error: "CONTRACT_PAUSED"

# Unpause
node keeps.mjs unpause mainnet

# Verify minting works again
node keeps.mjs mint test2 mainnet
```

**⚠️ Warning:** Only test this if you're comfortable pausing/unpausing. Skip if unsure.

---

## Step 11: Mint Multiple Pieces (Volume Test)

Mint 3-5 test Keeps to verify batch operations work:

```bash
node keeps.mjs mint piece1 mainnet
node keeps.mjs mint piece2 mainnet
node keeps.mjs mint piece3 mainnet
```

**Verify:**
- ✅ All transactions succeed
- ✅ Token IDs increment (0, 1, 2, 3...)
- ✅ All appear on objkt.com

---

## Step 12: Check Contract Balance & Withdraw

If you set a fee earlier, check accumulated fees:

```bash
# Check contract balance (fees collected)
curl https://api.tzkt.io/v1/contracts/{contract_address}/balance

# Withdraw fees to your wallet (admin only)
node keeps.mjs withdraw mainnet
```

**Expected:**
- Balance shows accumulated fees (if any)
- Withdrawal succeeds (sends XTZ to admin wallet)

---

## ✅ Success Criteria

After completing this flow, you should have:

- ✅ **3-5 test Keeps minted** on mainnet staging contract
- ✅ **Tokens visible on TzKT** with proper metadata
- ✅ **Tokens on objkt.com** with thumbnails and royalty info
- ✅ **Fee system tested** (set fee, verify requirement)
- ✅ **Royalties configured** (10% default)
- ✅ **Pause/unpause working** (optional)
- ✅ **No errors or issues**

---

## 🚨 Common Issues & Solutions

### Issue 1: "Contract file not found"
**Solution:** Compile the v4 contract first:
```bash
cd /workspaces/aesthetic-computer/tezos
smartpy compile keeps_fa2_v4.py KeepsFA2v4/
```

### Issue 2: "Insufficient balance"
**Solution:** Fund the staging wallet with XTZ:
- Check current balance: `node keeps.mjs balance mainnet`
- Send XTZ from another wallet

### Issue 3: "Piece not found: $xyz"
**Solution:** Make sure the $code is cached first:
- Visit `https://aesthetic.computer/#$xyz`
- Or create/cache the piece via the UI

### Issue 4: "IPFS upload failed"
**Solution:** Check Pinata credentials:
- Verify `.env.pinata` exists in vault
- Check `PINATA_API_KEY` and `PINATA_API_SECRET` are set

### Issue 5: objkt.com doesn't show token
**Solution:** Wait 5-10 minutes for indexing, then:
- Refresh the objkt.com page
- Check TzKT directly (always faster)
- Verify contract address is correct

---

## 📋 Quick Command Reference

```bash
# Status & Info
node keeps.mjs status mainnet          # Contract info
node keeps.mjs balance mainnet         # Wallet balance
node keeps.mjs fee mainnet             # Current fee
node keeps.mjs royalty mainnet         # Royalty settings

# Minting
node keeps.mjs mint $piece mainnet                 # Basic mint
node keeps.mjs mint $piece mainnet --thumbnail     # With Oven thumbnail

# Admin Operations
node keeps.mjs set-fee 2.5 mainnet     # Set fee
node keeps.mjs withdraw mainnet        # Withdraw fees
node keeps.mjs pause mainnet           # Emergency pause
node keeps.mjs unpause mainnet         # Resume

# Token Management
node keeps.mjs update 0 $piece         # Update token metadata
node keeps.mjs burn 0                  # Burn token
node keeps.mjs lock 0                  # Lock metadata (permanent)
```

---

## 🎯 Next Steps After Testing

Once you've successfully tested v4 staging:

1. ✅ **Document any issues** you found
2. ✅ **Verify objkt.com integration** works perfectly
3. ✅ **Test royalty calculation** (try a secondary sale if possible)
4. ✅ **Ready for v5** - Create production contract with fee = 2.5 XTZ

Then proceed to:
- Create v5 contract (copy v4, set default fee)
- Deploy v5 to Ghostnet for testing
- Deploy v5 to mainnet production (keeps.tez)

---

*Testing guide prepared February 13, 2026 for v4 staging contract validation*
