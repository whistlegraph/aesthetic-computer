# 🎨 Test Keep.mjs UI - Production Flow
**Date:** February 13, 2026
**Contract:** v4 Staging on Mainnet (`KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W`)

---

## ✅ Configuration Status

Your `keep.mjs` piece is currently configured for:

```javascript
// From: system/public/aesthetic.computer/lib/keeps/constants.mjs

KEEPS_STAGING = true                           // Shows "STAGING V4" badges
DEFAULT_NETWORK = "mainnet"                    // Uses mainnet (not testnet)
Contract = "KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W"  // v4 staging contract
```

**This is PERFECT for testing!** You're using:
- ✅ v4 contract (with royalties, pause, admin transfer)
- ✅ Mainnet (real XTZ, real objkt.com integration)
- ✅ Staging mode (shows version info, test-friendly)

---

## 🚀 User Flow: Mint a Keep via aesthetic.computer UI

### Step 1: Create or Pick a KidLisp Piece

**Option A: Create New Piece**
1. Go to https://aesthetic.computer
2. Type a simple KidLisp piece:
   ```lisp
   (wipe "purple")
   (ink "yellow")
   (circle (wiggle width) (wiggle height) 60)
   ```
3. Press Enter to run it
4. When it looks good, type: `save test-keep-1`
5. Note the $code generated (e.g., `$xyz`)

**Option B: Use Existing $code**
- Pick any existing cached KidLisp piece
- Examples: `$cow`, `$wand`, `$butterfly`, etc.

---

### Step 2: Navigate to Keep Piece

Type in aesthetic.computer:
```
keep test-keep-1
```

Or with the $code:
```
keep $xyz
```

**Expected:** Keep UI loads showing:
- Preview of your KidLisp piece
- "STAGING V4" badge (orange/yellow)
- Contract address shown: `v4: KT1ER1Gy..`
- "Keep This" button (or similar)

---

### Step 3: Connect Your Wallet

**What you'll see:**
- "Connect Wallet" button or prompt
- Options: Temple Wallet, Umami, Kukai, etc.

**Action:**
1. Click "Connect Wallet"
2. Choose your Tezos wallet (Temple recommended)
3. Approve the connection in your wallet popup
4. Wait for connection confirmation

**Expected result:**
- ✅ Wallet address shown in UI
- ✅ "Keep This" button becomes active
- ✅ Console shows: "🪙 KEEP: Wallet connected: tz1..."

---

### Step 4: Review Mint Details

Before minting, the UI should show:
- **Piece name:** `$xyz` (or your piece name)
- **Network:** "MAINNET (STAGING V4)"
- **Contract:** `v4: KT1ER1Gy..` (clickable link)
- **Fee:** 0 XTZ or 2.5 XTZ (depends on if you're admin)
- **Preview:** Animated thumbnail (if generated) or static preview

**Check the browser console** for detailed logs:
```
🪙 KEEP: Starting mint process for $xyz
🪙 KEEP: Network: mainnet, Contract: KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
🪙 KEEP: Staging mode: true
```

---

### Step 5: Start the Keep Process

Click the **"Keep This"** or **"Confirm"** button.

**What happens (automatic pipeline):**
1. ✓ **Validate Piece** - Checks $code exists
2. ✓ **Analyze Source** - Parses KidLisp code
3. ✓ **Generate Preview** - Creates thumbnail via Oven
4. ✓ **Bundle Assets** - Packages HTML + metadata
5. ✓ **Upload to IPFS** - Pinata upload
6. ✓ **Create Metadata** - TZIP-21 JSON with royalties
7. 👁️ **Review & Confirm** - Shows final details
8. ✍️ **Sign Transaction** - Wallet popup

**Watch the progress bar** - each step shows:
- ⏳ Pending (gray)
- ⚡ Active (yellow/orange)
- ✅ Done (green)
- ❌ Error (red, if something fails)

---

### Step 6: Sign the Transaction

When you reach the "Sign Transaction" step:

**What you'll see:**
- Wallet popup (Temple/Umami/etc.)
- Transaction details:
  - **To:** `KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W` (contract)
  - **Entrypoint:** `keep`
  - **Amount:** 0 XTZ (if you're admin) or 2.5 XTZ (if user)
  - **Fee:** ~0.01-0.05 XTZ (network gas)

**Action:**
1. Review the transaction details
2. Click "Confirm" or "Sign" in your wallet
3. Wait for transaction to broadcast

**Expected:**
- ✅ Wallet shows "Transaction submitted"
- ✅ UI shows operation hash: `opXXX...`
- ✅ Console logs: "🪙 KEEP: Transaction sent: opXXX..."

---

### Step 7: Wait for Confirmation

**Tezos confirmation takes ~30 seconds** (2 blocks).

**What you'll see:**
- Progress spinner or "Waiting for confirmation..." message
- Operation hash link (clickable to TzKT)

**In console:**
```
🪙 KEEP: Waiting for confirmation...
🪙 KEEP: Confirmed! Searching for token...
🪙 KEEP: Found token #X for KidLisp $xyz
```

---

### Step 8: Mint Complete! 🎉

**Success screen shows:**
- ✅ "Keep Minted!"
- **Token ID:** #0 (or next available)
- **Network:** MAINNET (STAGING V4)
- **Contract:** KT1ER1Gy... (clickable)
- **Transaction:** opXXX... (clickable to TzKT)
- **Links:**
  - "View on objkt.com"
  - "View in Wallet"
  - "View Transaction"

**Buttons to test:**
- **View on objkt.com** → Opens `https://objkt.com/tokens/KT1ER1Gy.../X`
- **Contract** → Opens `https://tzkt.io/KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W`
- **Transaction** → Opens `https://tzkt.io/opXXX...`

---

### Step 9: Verify on TzKT

Click the transaction link or visit:
```
https://tzkt.io/KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
```

**Check:**
- ✅ Recent operations show your `keep` call
- ✅ Status: `applied` (success)
- ✅ Sender: Your wallet address (tz1...)
- ✅ Token minted: Check storage for `next_token_id`

---

### Step 10: Verify on objkt.com

Visit:
```
https://objkt.com/collection/KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
```

**Wait 1-5 minutes** for objkt.com to index the token.

**Then check:**
- ✅ Token appears in collection
- ✅ Thumbnail displays (animated WebP if Oven was used)
- ✅ Name: "KidLisp $xyz" (or similar)
- ✅ Royalty info: 10%
- ✅ Creator: Your wallet address

**Click on the token** to see full metadata:
- Description
- Source code snippet
- Artifact URI (IPFS)
- Display URI (IPFS)
- Thumbnail URI (IPFS)
- Royalties (10% to your wallet)

---

## 🔍 What to Watch in Browser Console

Enable browser console (F12) and watch for these logs:

### Successful Mint Logs
```
🪙 KEEP: Starting mint process for $xyz
🪙 KEEP: Network: mainnet, Contract: KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
🪙 KEEP: Staging mode: true
🪙 KEEP: Wallet connected: tz1abc...
🪙 KEEP: Fetching piece: $xyz
🪙 KEEP: Source loaded (123 bytes)
🪙 KEEP: Thumbnail generated: https://oven.aesthetic.computer/xyz.webp
🪙 KEEP: Bundle created
🪙 KEEP: IPFS upload started...
🪙 KEEP: IPFS upload complete: ipfs://QmXXX...
🪙 KEEP: Metadata created
🪙 KEEP: Review complete, ready to sign
🪙 KEEP: Transaction sent: opXXX...
🪙 KEEP: Waiting for confirmation...
🪙 KEEP: Confirmed! Token ID: 0
🪙 KEEP: Mint complete! 🎉
```

### Error Logs (if something goes wrong)
```
❌ KEEP ERROR: Wallet not connected
❌ KEEP ERROR: Piece not found: $xyz
❌ KEEP ERROR: IPFS upload failed
❌ KEEP ERROR: Transaction rejected
❌ KEEP ERROR: Already minted (token ID: 5)
```

---

## 🚨 Common Issues & Solutions

### Issue 1: "Piece not found: $xyz"
**Solution:** Make sure you cached the piece first:
1. Go to aesthetic.computer
2. Type/run the KidLisp code
3. Type `save piece-name` to cache it
4. Then try `keep piece-name`

### Issue 2: Wallet won't connect
**Solution:**
1. Make sure Temple Wallet (or your wallet) is installed
2. Refresh the page
3. Try disconnecting/reconnecting wallet
4. Check wallet is set to Mainnet (not Ghostnet)

### Issue 3: "Insufficient balance"
**Solution:**
- If fee is 2.5 XTZ, make sure you have at least 3 XTZ in wallet
- If you're testing as admin (tz1staging...), fee should be 0

### Issue 4: Transaction fails
**Solution:**
1. Check transaction on TzKT for error message
2. Common errors:
   - "INSUFFICIENT_FEE" → Need to pay 2.5 XTZ
   - "DUPLICATE_CONTENT_HASH" → Already minted this exact piece
   - "CONTRACT_PAUSED" → Contract is paused (emergency)

### Issue 5: objkt.com doesn't show token
**Solution:**
1. Wait 5-10 minutes (indexing can be slow)
2. Refresh the collection page
3. Check TzKT directly (always faster)
4. If still missing after 1 hour, might be objkt.com issue

### Issue 6: Thumbnail not generated
**Solution:**
- Check if Oven service is running
- If Oven fails, mint will use static preview instead
- Not critical - token will still mint successfully

---

## 📊 Test Checklist

After minting, verify all these:

### Frontend (keep.mjs UI)
- [ ] "STAGING V4" badge shows
- [ ] Contract address displayed: `v4: KT1ER1Gy..`
- [ ] Wallet connection works
- [ ] Progress bar updates through all steps
- [ ] Preview/thumbnail displays
- [ ] Transaction hash shown
- [ ] Success screen appears

### Backend (Netlify Functions)
- [ ] `/api/keep-mint` endpoint responds
- [ ] IPFS upload succeeds (Pinata)
- [ ] MongoDB updated with keep info
- [ ] Oven thumbnail generated (or gracefully fails)

### Blockchain (Tezos)
- [ ] Transaction applied successfully
- [ ] Token minted with correct ID
- [ ] Metadata stored in contract
- [ ] Royalty field present (10%)
- [ ] Creator address correct

### Indexing (TzKT & objkt.com)
- [ ] TzKT shows transaction within 1 minute
- [ ] objkt.com indexes within 5-10 minutes
- [ ] Token metadata displays correctly
- [ ] Thumbnail visible on objkt.com
- [ ] Royalty info visible

---

## 🎯 Success Criteria

**Minimum success (basic mint works):**
- ✅ Keep UI loads
- ✅ Wallet connects
- ✅ Transaction signs and confirms
- ✅ Token ID returned
- ✅ Shows on TzKT

**Good success (full integration works):**
- ✅ All above +
- ✅ Thumbnail generated via Oven
- ✅ IPFS upload succeeds
- ✅ Token appears on objkt.com
- ✅ Royalty metadata present

**Perfect success (everything polished):**
- ✅ All above +
- ✅ No errors in console
- ✅ Smooth UX (no weird delays/bugs)
- ✅ All links work (objkt, TzKT, wallet)
- ✅ Ready for v5 production!

---

## 🔗 Quick Links

**Contract on TzKT:**
```
https://tzkt.io/KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
```

**Collection on objkt.com:**
```
https://objkt.com/collection/KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
```

**Keep UI:**
```
https://aesthetic.computer/keep/$xyz
```
(Replace `$xyz` with your piece code)

---

## 📝 What to Note for v5

While testing, pay attention to:
1. **Current fee:** Is it 0 XTZ or 2.5 XTZ?
2. **User experience:** Any confusing steps?
3. **Error messages:** Are they helpful?
4. **Performance:** Any slow steps?
5. **Visual polish:** Anything look broken?

**Document any issues** so we can fix them before v5 production launch!

---

## 🚀 Next Steps After Testing

Once you've successfully minted 1-3 test Keeps:

1. ✅ **Verify objkt.com integration** works perfectly
2. ✅ **Check royalty metadata** is present
3. ✅ **Test secondary sale** (optional, list on objkt)
4. ✅ **Ready for v5 creation** - Copy v4, set fee = 2.5 XTZ
5. ✅ **Deploy v5 to production** - Follow [KEEPS-V5-PRODUCTION-ROADMAP.md](KEEPS-V5-PRODUCTION-ROADMAP.md)

---

*Testing guide prepared February 13, 2026 for v4 staging contract UI testing via aesthetic.computer production frontend*
