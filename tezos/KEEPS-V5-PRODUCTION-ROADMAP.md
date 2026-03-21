# 🚀 Keeps FA2 v5 Production Launch Roadmap
**Generated:** February 13, 2026
**Objective:** Deploy final production contract on keeps.tez with revenue enabled
**Timeline:** 2-3 weeks from start to production

---

## Executive Summary

**Current State:**
- ❌ Old v2 contract live: `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM` (no royalties, fee = 0 XTZ)
- ✅ v4 contract ready: Full features (royalties, pause, admin transfer)
- ⏳ v5 needed: v4 + production defaults + final polish

**Target State:**
- ✅ v5 contract deployed to mainnet
- ✅ Admin: kidlisp wallet (tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC / keeps.tez)
- ✅ Fee: 2-3 XTZ per Keep (revenue enabled!)
- ✅ Royalties: 10% on secondary sales
- ✅ All systems pointed to v5 contract

**Why This Matters:**
- **Current revenue: $0/month** (fee disabled, no royalties)
- **After v5: $20-100+/month** from mints + secondary sales
- **Plus Solana integration** → Multi-chain revenue strategy

---

## Phase 1: Design v5 Contract (Week 1)

### v5 Changes from v4

| Feature | v4 | v5 | Reason |
|---------|----|----|--------|
| **Default fee** | 0 XTZ | **2.5 XTZ** | Revenue enabled by default |
| **Royalties** | 10% (1000 bps) | **10%** (keep) | Standard creator royalty |
| **Pause** | False | **False** | Active by default |
| **Content hash check** | Yes | **Yes** (keep) | Prevent duplicates |
| **Admin transfer** | Yes | **Yes** (keep) | Customer service |
| **Editable metadata** | Yes | **Yes** (keep) | Creator flexibility |

**Key Change: Fee enabled at 2.5 XTZ (~$2.50)**
- Users expect to pay for permanent NFT storage
- Covers IPFS costs + margin
- Still cheaper than gas on most chains
- Can be adjusted later via `set_keep_fee`

### Contract Modifications

**File:** `tezos/keeps_fa2_v5.py` (create from v4)

```python
# Change line 90 from:
self.data.keep_fee = sp.tez(0)

# To:
self.data.keep_fee = sp.tez(2.5)  # $2.50 at current XTZ price
```

**Optional improvements:**
1. **Gas optimization** (if possible)
   - Review contract size
   - Optimize storage access patterns

2. **Better error messages**
   - "INSUFFICIENT_FEE" → "INSUFFICIENT_FEE: Required 2.5 XTZ"
   - "CONTRACT_PAUSED" → "MINTING_PAUSED: Contract under maintenance"

3. **Event emissions** (if SmartPy supports)
   - Emit events for mints, burns, transfers
   - Better indexer integration

**Estimated effort:** 1-2 days (mostly testing)

---

## Phase 2: Ghostnet Testing (Week 1-2)

### Test Plan Checklist

#### 2.1 Deploy to Ghostnet

```bash
cd /workspaces/aesthetic-computer/tezos

# Compile v5
smartpy compile keeps_fa2_v5.py output/

# Deploy with aesthetic wallet (Ghostnet admin)
node keeps.mjs deploy ghostnet --wallet=aesthetic --contract=v5
```

**Expected result:**
- Contract deployed to Ghostnet
- Admin: aesthetic wallet
- Default fee: 2.5 XTZ
- Royalty: 10%

#### 2.2 Core Functionality Tests

**Test 1: Admin Minting (No Fee)**
```bash
node keeps.mjs mint $test1 --wallet=aesthetic --network=ghostnet
```
✅ Verify: Admin can mint without paying fee

**Test 2: User Minting (With Fee)**
```bash
# Mint from non-admin wallet (need to send 2.5 XTZ)
node keeps.mjs mint $test2 --wallet=staging --network=ghostnet --fee=2.5
```
✅ Verify: Transaction succeeds with 2.5 XTZ payment
✅ Verify: Contract balance increases by 2.5 XTZ

**Test 3: Insufficient Fee Rejection**
```bash
node keeps.mjs mint $test3 --wallet=staging --network=ghostnet --fee=1.0
```
❌ Verify: Transaction fails with "INSUFFICIENT_FEE"

**Test 4: Royalty Metadata**
```bash
# Check token metadata on TzKT
curl https://api.ghostnet.tzkt.io/v1/contracts/{contract}/bigmaps/token_metadata/keys/{token_id}
```
✅ Verify: Royalty field present with 10% (1000 bps)
✅ Verify: Creator address matches minter

**Test 5: Content Hash Deduplication**
```bash
# Mint same piece twice
node keeps.mjs mint $test4 --wallet=aesthetic
node keeps.mjs mint $test4 --wallet=aesthetic  # Should fail
```
❌ Verify: Second mint fails (duplicate content hash)

**Test 6: Emergency Pause**
```bash
# Pause contract
node keeps.mjs pause --wallet=aesthetic

# Try to mint (should fail)
node keeps.mjs mint $test5 --wallet=aesthetic
```
❌ Verify: Minting blocked when paused
✅ Verify: Unpause works, minting resumes

**Test 7: Admin Transfer**
```bash
# Transfer token from one user to another (admin action)
node keeps.mjs admin-transfer --token=0 --from=tz1abc... --to=tz1xyz...
```
✅ Verify: Token ownership updated
✅ Verify: Only admin can call

**Test 8: Metadata Editing**
```bash
# Edit token metadata (by creator)
node keeps.mjs edit-metadata --token=0 --field=description --value="Updated"
```
✅ Verify: Creator can edit their token
❌ Verify: Non-creator cannot edit

**Test 9: Fee Adjustment**
```bash
# Change fee to 5 XTZ
node keeps.mjs set-fee --amount=5.0 --wallet=aesthetic

# Verify fee updated
node keeps.mjs fee
```
✅ Verify: Fee changed to 5 XTZ
✅ Verify: New mints require 5 XTZ

**Test 10: Fee Withdrawal**
```bash
# Withdraw accumulated fees
node keeps.mjs withdraw --to=tz1aesthetic... --wallet=aesthetic
```
✅ Verify: Balance transferred to admin wallet
✅ Verify: Contract balance reduced

#### 2.3 Integration Tests

**Test 11: Oven Thumbnail Generation**
```bash
# Mint piece with Oven thumbnail
curl -X POST https://oven.aesthetic.computer/bake/$test6
node keeps.mjs mint $test6 --wallet=aesthetic
```
✅ Verify: Thumbnail URL in metadata
✅ Verify: Animated WebP accessible via IPFS

**Test 12: objkt.com Display (Ghostnet)**
1. Mint token on Ghostnet
2. Visit https://ghostnet.objkt.com/collection/{contract}
3. ✅ Verify: Token appears with thumbnail
4. ✅ Verify: Metadata displayed correctly
5. ✅ Verify: Royalty info visible

**Test 13: End-to-End User Flow**
1. User creates KidLisp piece at aesthetic.computer
2. Cache as $code via `/api/store-kidlisp`
3. Click "Keep" button
4. Wallet prompts for 2.5 XTZ payment
5. Transaction signed and submitted
6. Keep minted with IPFS metadata
7. ✅ Verify: Token appears in wallet.mjs
8. ✅ Verify: Token on Ghostnet objkt.com

**Estimated testing time:** 3-5 days

---

## Phase 3: Mainnet Staging Deploy (Week 2)

### 3.1 Deploy to Mainnet (Staging Wallet)

**Why staging first?**
- Test with real XTZ (not testnet tokens)
- Verify objkt.com integration on mainnet
- Catch any production-only issues
- Low risk (staging wallet, throwaway funds)

```bash
cd /workspaces/aesthetic-computer/tezos

# Deploy v5 to mainnet with staging wallet
node keeps.mjs deploy mainnet --wallet=staging --contract=v5
```

**Expected result:**
- Contract: KT1{new_address}
- Admin: staging wallet (tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt)
- Fee: 2.5 XTZ
- Network: Mainnet

### 3.2 Mainnet Staging Tests

**Test 1: Real XTZ Minting**
```bash
# Fund staging wallet with 10 XTZ
# Mint 3 test tokens
node keeps.mjs mint $main1 --wallet=staging --network=mainnet
node keeps.mjs mint $main2 --wallet=staging --network=mainnet
node keeps.mjs mint $main3 --wallet=staging --network=mainnet
```
✅ Verify: Mints succeed
✅ Verify: Real XTZ spent (check wallet balance)

**Test 2: objkt.com Mainnet Display**
1. Visit https://objkt.com/collection/{new_contract}
2. ✅ Verify: Collection appears
3. ✅ Verify: Tokens display with thumbnails
4. ✅ Verify: Royalty info visible (10%)

**Test 3: Secondary Sale Simulation**
1. List token for sale on objkt.com
2. Buy with different wallet
3. ✅ Verify: 10% royalty paid to creator
4. ✅ Verify: objkt.com recognizes royalty metadata

**Test 4: Fee Withdrawal (Mainnet)**
```bash
# Withdraw accumulated fees to staging wallet
node keeps.mjs withdraw --to=tz1staging... --wallet=staging --network=mainnet
```
✅ Verify: XTZ transferred successfully

**Test 5: Production API Integration**
```bash
# Point staging environment to new contract
export KEEPS_CONTRACT={new_staging_contract}
export KEEPS_NETWORK=mainnet

# Test minting via Netlify function
curl -X POST https://aesthetic.computer/api/kidlisp-keep \
  -H "Authorization: Bearer {token}" \
  -d '{"code": "testpiece"}'
```
✅ Verify: Backend can mint to new contract
✅ Verify: MongoDB records updated with tokenId

**Test 6: Client-Side Wallet Integration**
1. Open aesthetic.computer/keep
2. Connect Temple wallet
3. Mint a Keep (user-initiated, pays 2.5 XTZ)
4. ✅ Verify: Prepare mode works
5. ✅ Verify: User becomes firstMinter
6. ✅ Verify: Token appears in wallet.mjs

**Estimated staging time:** 3-5 days

### 3.3 Decision Point: Staging → Production

**Go/No-Go Criteria:**

✅ **GO if all pass:**
- All Ghostnet tests passed
- All mainnet staging tests passed
- objkt.com displays tokens correctly
- Royalties working on secondary sales
- No critical bugs discovered
- API integration tested end-to-end
- User flow smooth (2.5 XTZ payment acceptable)

❌ **NO-GO if any fail:**
- Critical bugs in contract
- objkt.com integration broken
- Royalties not enforced
- API can't communicate with contract
- User experience issues (fee too high, confusing UX)

---

## Phase 4: Production Deploy (Week 3)

### 4.1 Final Production Deploy

**This is the BIG ONE.** Deploy with kidlisp wallet (keeps.tez).

```bash
cd /workspaces/aesthetic-computer/tezos

# Double-check you're using kidlisp wallet
echo $KIDLISP_ADDRESS  # Should be: tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC

# Deploy v5 to mainnet with kidlisp (production) wallet
node keeps.mjs deploy mainnet --wallet=kidlisp --contract=v5
```

**Expected result:**
- Contract: KT1{final_production_address}
- Admin: kidlisp wallet (tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC / keeps.tez)
- Fee: 2.5 XTZ
- Network: Mainnet
- Status: **PRODUCTION LIVE**

### 4.2 Update All Systems

**Environment Variables (Netlify)**

Update production env vars:
```bash
KEEPS_CONTRACT={new_v5_contract_address}
KEEPS_NETWORK=mainnet
KEEPS_ADMIN_ADDRESS=tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC
TEZOS_RPC=https://rpc.mainnet.tezos.dev
```

**Frontend Configuration**

Update contract addresses in:
- `system/public/aesthetic.computer/lib/keeps-client.mjs`
- `system/public/aesthetic.computer/disks/keep.mjs`
- `system/public/aesthetic.computer/disks/wallet.mjs`

**Backend Functions**

Update contract address in:
- `system/netlify/functions/kidlisp-keep.mjs`
- `system/netlify/functions/keep-mint.mjs`
- `system/netlify/functions/keep-update.mjs`

**Documentation**

Update contract address in:
- `tezos/README.md` (wallet rolodex)
- `tezos/KEEPS-SYSTEM.md` (system docs)
- `reports/monetization-stack-2026-02.md`

### 4.3 Smoke Test Production

**Critical path test:**

```bash
# 1. Mint first production Keep (admin, no fee)
node keeps.mjs mint $production1 --wallet=kidlisp --network=mainnet

# 2. Verify on TzKT
curl https://api.tzkt.io/v1/contracts/{v5_contract}/storage

# 3. Verify on objkt.com
open https://objkt.com/collection/{v5_contract}

# 4. Test user mint from aesthetic.computer UI
# - Create KidLisp piece
# - Click "Keep"
# - Pay 2.5 XTZ
# - Verify mint succeeds

# 5. Verify fee collection
node keeps.mjs balance --wallet=kidlisp
```

✅ **If all pass: Production is LIVE!**

---

## Phase 5: Migration & Communication (Week 3)

### 5.1 Old Contract Handling

**Old v2 contract:** `KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM`

**Options:**

**Option A: Keep Both (Recommended)**
- Old tokens remain on v2 contract
- New tokens mint to v5 contract
- Users see both in wallet.mjs
- No disruption to existing NFT holders

**Option B: Migrate Tokens**
- Burn old tokens on v2
- Re-mint on v5 (preserving metadata)
- Risk: Token IDs change, objkt.com links break
- **Not recommended** unless critical security issue

**Recommendation:** Option A (keep both)

### 5.2 User Communication

**Announcement channels:**
1. In-app banner at aesthetic.computer
2. Discord announcement
3. Twitter/Bluesky post
4. Update give.aesthetic.computer "About Keeps" section

**Message template:**

```
🎉 Keeps v5 is now live!

What's new:
✨ 10% creator royalties on secondary sales
💰 Keep fee: 2.5 XTZ (~$2.50) for permanent storage
🛡️ Enhanced security with emergency pause
🔧 Better creator tools (editable metadata)

Old Keeps (v2) remain valid and supported.
New Keeps mint to v5 contract: {address}

Questions? See keeps.tez or ask in Discord.
```

### 5.3 Documentation Updates

**Create:**
- `KEEPS-V5-CHANGELOG.md` (what changed from v2)
- `KEEPS-V5-USER-GUIDE.md` (how to use new features)

**Update:**
- `README.md` (point to v5 as default)
- `KEEPS-SYSTEM.md` (architecture docs)
- `KEEPS-FEE-SYSTEM.md` (reflect 2.5 XTZ default)

---

## Phase 6: Revenue Activation (Week 3)

### 6.1 Monitor Revenue

**Track metrics:**

```bash
# Daily revenue check
curl https://api.tzkt.io/v1/contracts/{v5_contract}/balance

# Mint count
curl https://api.tzkt.io/v1/contracts/{v5_contract}/storage | jq '.next_token_id'

# Calculate daily revenue
# (next_token_id - previous_token_id) × 2.5 XTZ
```

**Expected Month 1:**
- 20-50 Keeps minted
- Revenue: 50-125 XTZ ($50-125)

### 6.2 Fee Optimization

**Monitor user feedback:**
- Too expensive? Lower to 2.0 XTZ
- No complaints? Try 3.0 XTZ
- Demand is high? Test 5.0 XTZ

**A/B test (optional):**
- Week 1-2: 2.5 XTZ, track conversion
- Week 3-4: 3.0 XTZ, compare mints
- Choose optimal price

### 6.3 Royalty Revenue Tracking

**Monitor secondary sales:**

```bash
# Query objkt.com API for sales
curl https://data.objkt.com/v3/graphql \
  -H "Content-Type: application/json" \
  -d '{
    "query": "query { fa { floor_price, listings { price } } }",
    "variables": { "contract": "{v5_contract}" }
  }'
```

**Track:**
- Total secondary sales volume
- Your 10% royalty share
- Compare to primary mint revenue

**Expected:**
- Month 1: Low (new collection)
- Month 3-6: Growing as collectors trade
- Month 12+: Royalties may exceed mint fees

---

## Risk Mitigation

### Risk 1: Contract Bug in Production

**Mitigation:**
- Thorough Ghostnet + staging testing
- Code review before deploy
- Emergency pause if issue discovered
- Can deploy v6 if needed (don't rush)

**If bug found:**
1. Call `pause()` immediately
2. Assess severity
3. If critical: Deploy fixed version (v5.1 or v6)
4. If minor: Document workaround, fix in next version

### Risk 2: Users Reject Fee

**Mitigation:**
- Clear value proposition (IPFS, permanence, objkt listing)
- Lower fee if conversion drops
- Offer free trial period (first Keep free via promo code)

**Monitoring:**
- Compare mint rate before/after v5
- Survey users: "Is 2.5 XTZ fair?"
- Track abandonment rate in UI

### Risk 3: Solana Integration Delays v5

**Solution:**
- **Decouple timelines**
- Deploy v5 Tezos contract ASAP (Week 3)
- Solana integration happens in parallel (Week 4-6)
- Don't wait for Solana to fix Tezos revenue

### Risk 4: objkt.com Doesn't Recognize Royalties

**Mitigation:**
- Test on Ghostnet objkt first
- Follow TZIP-21 standard exactly
- Contact objkt support if issues
- Have backup plan (use different marketplace)

### Risk 5: Migration Breaks Existing Keeps

**Mitigation:**
- **Don't migrate** (keep both contracts)
- wallet.mjs supports multiple contracts
- Users don't lose access to old Keeps
- Zero disruption

---

## Timeline Summary

```
Week 1: Design & Ghostnet Testing
├── Day 1-2: Create v5 contract (from v4, set fee = 2.5 XTZ)
├── Day 3: Deploy to Ghostnet
├── Day 4-5: Core functionality tests (10 tests)
└── Day 6-7: Integration tests (Oven, objkt, API)

Week 2: Mainnet Staging
├── Day 8: Deploy to mainnet (staging wallet)
├── Day 9-10: Mainnet staging tests (real XTZ)
├── Day 11-12: objkt.com integration + secondary sales test
├── Day 13: API integration end-to-end
└── Day 14: Go/No-Go decision

Week 3: Production Launch
├── Day 15: Deploy v5 to mainnet (kidlisp wallet) 🚀
├── Day 16: Update all systems (env vars, frontend, backend)
├── Day 17: Smoke test production + first real mint
├── Day 18-19: User communication + documentation
└── Day 20-21: Monitor revenue, gather feedback

Week 4+: Optimization
├── Monitor mint conversion rate
├── A/B test fee amounts (2.5 → 3.0 XTZ?)
├── Track royalty revenue from secondary sales
└── Iterate based on data
```

**Total time:** 2-3 weeks from start to production revenue

---

## Success Criteria

### Minimum Viable Success (Week 3)
- ✅ v5 contract deployed to mainnet
- ✅ Admin: kidlisp wallet (keeps.tez)
- ✅ Fee: 2.5 XTZ working
- ✅ First 5-10 Keeps minted successfully
- ✅ Revenue: >$0 (finally!)

### Good Success (Month 1)
- ✅ 20-50 Keeps minted
- ✅ Revenue: 50-125 XTZ ($50-125)
- ✅ Zero critical bugs
- ✅ Positive user feedback
- ✅ objkt.com secondary sales with royalties working

### Great Success (Month 3)
- ✅ 100+ Keeps minted
- ✅ Revenue: $300-500 from mints + royalties
- ✅ Solana integration live (dual-chain!)
- ✅ Fee optimized based on data
- ✅ Users actively trading Keeps on objkt.com

---

## Next Steps (Immediate Actions)

### Today
1. ✅ Read this roadmap
2. ✅ Decide: Commit to v5 launch?
3. ✅ Set timeline (start Week 1 when?)

### Tomorrow (If go-ahead)
1. Create `keeps_fa2_v5.py` from v4
2. Change default fee to 2.5 XTZ
3. Add any final polish (error messages, comments)
4. Compile with SmartPy

### This Week (Week 1)
5. Deploy to Ghostnet
6. Run all 13 Ghostnet tests
7. Fix any issues discovered

### Week 2
8. Deploy to mainnet staging
9. Run mainnet tests with real XTZ
10. Go/No-Go decision

### Week 3 🚀
11. **DEPLOY v5 TO PRODUCTION**
12. Update all systems
13. Announce to users
14. **START EARNING REVENUE**

---

## Questions to Answer Before Starting

1. **Fee amount:** 2.5 XTZ good? Or prefer 2.0 / 3.0?
2. **Royalty percentage:** Keep 10%? Or try 5% / 15%?
3. **Old contract:** Keep both (recommended) or migrate?
4. **Timeline:** Start Week 1 today/tomorrow? Or wait?
5. **Solana:** Proceed in parallel or wait for v5 first?

**My recommendation:**
- Fee: 2.5 XTZ (test, adjust later)
- Royalty: 10% (industry standard)
- Old contract: Keep both (no disruption)
- Timeline: Start ASAP (revenue needed!)
- Solana: Parallel track (don't wait)

---

## Appendix: CLI Commands Reference

### Deployment
```bash
# Ghostnet
node keeps.mjs deploy ghostnet --wallet=aesthetic --contract=v5

# Mainnet staging
node keeps.mjs deploy mainnet --wallet=staging --contract=v5

# Mainnet production
node keeps.mjs deploy mainnet --wallet=kidlisp --contract=v5
```

### Testing
```bash
# Mint
node keeps.mjs mint $piece --wallet=X --network=Y

# Fee operations
node keeps.mjs fee                           # Check current fee
node keeps.mjs set-fee --amount=2.5          # Set fee
node keeps.mjs withdraw --to=tz1...          # Withdraw fees

# Admin
node keeps.mjs pause                         # Pause minting
node keeps.mjs unpause                       # Resume minting
node keeps.mjs admin-transfer --token=0 --from=tz1... --to=tz1...

# Status
node keeps.mjs status                        # Contract info
node keeps.mjs balance --wallet=kidlisp      # Wallet balance
```

### Verification
```bash
# TzKT API
curl https://api.tzkt.io/v1/contracts/{contract}/storage
curl https://api.tzkt.io/v1/contracts/{contract}/bigmaps/token_metadata/keys/{id}

# Contract balance
curl https://api.tzkt.io/v1/contracts/{contract}/balance
```

---

*Roadmap prepared February 13, 2026 for final production launch of Keeps FA2 v5 on keeps.tez with revenue activation.*
