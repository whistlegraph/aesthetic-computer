# KidLisp Keeps v4 Production Plan

**Prepared:** February 10, 2026
**Current Status:** v3 in mainnet staging (`KT1JEVyKjsMLts63e4CNaMUywWTPgeQ41Smi`)
**Target:** v4 production deployment with 10% royalties

---

## Executive Summary

**✅ GOOD NEWS: Attribution is working correctly in v3!**

The current system uses client-side wallet signing (prepare mode) which makes users the `firstMinter` on TzKT/objkt.com. The `token_creators` bigmap preserves creator identity for metadata edits. This is exactly how it should work.

**⚠️ CRITICAL GAP: No royalty support**

Without royalties, creators earn $0 on secondary sales. objkt.com enforces royalties via standard TZIP-21 metadata - we just need to add the fields.

---

## v4 Core Features

### 1. 10% Royalty Support (ESSENTIAL)

Add royalty metadata to every minted token:

```python
# In keep() entrypoint - add to token_info
"royalties": stringToBytes(JSON.stringify({
    "decimals": 4,
    "shares": {
        [creator_wallet]: "1000"  // 10% = 1000/10000 basis points
    }
}))
```

**Why This Matters:**
- 10 XTZ sale = 1 XTZ to creator, 8.75 XTZ to seller, 0.25 XTZ to objkt
- Without royalties: 10 XTZ sale = 0 XTZ to creator, 9.75 XTZ to seller
- **Creators lose ~90% of NFT revenue potential**

**Implementation Effort:** 1-2 days
- Modify keeps_fa2_v3.py (add royalty field to token_info)
- Update keep-mint.mjs (add royalty to metadata JSON)
- Update keep-update.mjs (preserve royalty on edits)
- Test on Ghostnet → verify on objkt.com

### 2. Emergency Pause (HIGH PRIORITY)

```python
# Add to contract storage
self.data.paused = False

# Admin entrypoints
@sp.entrypoint
def pause(self):
    assert self.is_administrator_(), "FA2_NOT_ADMIN"
    self.data.paused = True

@sp.entrypoint
def unpause(self):
    assert self.is_administrator_(), "FA2_NOT_ADMIN"
    self.data.paused = False

# Modify keep() entrypoint
def keep(self, params):
    assert not self.data.paused, "CONTRACT_PAUSED"
    # ... rest of logic
```

**Why This Matters:**
- Security vulnerability discovered? Pause minting immediately
- IPFS infrastructure down? Pause until fixed
- Spam attack? Pause and investigate
- **Without pause: Exploits continue until contract replaced**

**Implementation Effort:** 4 hours

### 3. Admin Transfer (CUSTOMER SERVICE)

```python
@sp.entrypoint
def admin_transfer(self, params):
    """Emergency transfer for customer service"""
    sp.cast(params, sp.record(
        token_id=sp.nat,
        from_=sp.address,
        to_=sp.address
    ))

    assert self.is_administrator_(), "FA2_NOT_ADMIN"
    current_owner = self.data.ledger[params.token_id]
    assert current_owner == params.from_, "INVALID_CURRENT_OWNER"

    self.data.ledger[params.token_id] = params.to_
```

**Use Cases:**
- User minted to wrong wallet by mistake
- User lost private keys (with verification)
- Customer service dispute resolution

**Implementation Effort:** 2 hours

---

## Attribution Analysis ✅

### Current Flow (v3 - WORKING CORRECTLY)

```
1. User: keep $cow
2. Server: /api/keep-mint?mode=prepare
   - Validates user owns piece
   - Generates bundle + uploads to IPFS
   - Returns Michelson params for contract call
3. User: Wallet signs transaction
   - User becomes firstMinter (TzKT field)
   - Contract stores user in token_creators[tokenId]
   - objkt.com reads firstMinter for "Created by" ✅
4. Server: /api/keep-confirm
   - Records mint in MongoDB
```

**Proof from Code:**

- **keep-mint.mjs:888-890** - Comments explicitly state objkt.com uses firstMinter
- **keep-mint.mjs:515-527** - Validates wallet matches linked address (prevents hijacking)
- **keep-mint.mjs:970** - `ownerAddress = creatorWalletAddress` (user wallet used)
- **keep-update.mjs:206-214** - Fetches firstMinter from TzKT for edit authorization
- **keeps_fa2_v3.py:173** - `token_creators[token_id] = params.owner` (stores creator)

**Conclusion:** No attribution fixes needed! v3 is correctly designed.

---

## Deployment Strategy

### Option 1: Fresh v4 (RECOMMENDED)

**Pros:**
- Clean start with all features
- Royalties from token #0
- Only 1 token on v3 staging ($roz)
- Professional foundation

**Cons:**
- New contract address (update env vars)
- Need to re-mint $roz

**Process:**
```bash
# Week 1: Code v4
cd tezos
cp keeps_fa2_v3.py keeps_fa2_v4.py
# Add: royalty support, pause, admin_transfer

# Week 2: Ghostnet
python keeps_fa2_v4.py
node keeps.mjs deploy ghostnet
node keeps.mjs royalty:set 10 --network ghostnet
node keeps.mjs keep $test --network ghostnet
# Verify on https://ghostnet.objkt.com

# Week 3: Mainnet Staging
node keeps.mjs deploy mainnet --wallet staging
# Mint 5-10 test tokens

# Week 4: Production
node keeps.mjs deploy mainnet --wallet kidlisp
# Update KEEPS_CONTRACT env var
# Announce to users
```

### Option 2: Continue v3 (NOT RECOMMENDED)

**Problem:** No royalties = creators earn $0 on secondary sales

This is a deal-breaker for NFT adoption. Artists won't mint if they can't earn royalties.

---

## CLI Enhancements

Add to keeps.mjs:

```bash
# Royalty Management
node keeps.mjs royalty:set 10           # Set default to 10%
node keeps.mjs royalty:get              # View current default
node keeps.mjs royalty:token <id>       # View token-specific

# Emergency Controls
node keeps.mjs pause                    # Stop minting
node keeps.mjs unpause                  # Resume
node keeps.mjs status --full            # Shows pause + royalty state

# Admin Operations
node keeps.mjs transfer:admin <id> <from> <to>  # Customer service
node keeps.mjs metadata:sync <code>             # Update from MongoDB
node keeps.mjs metadata:lock <id>               # Freeze forever

# Analytics
node keeps.mjs stats                    # Contract statistics
node keeps.mjs stats:creator <address>  # Tokens by creator
node keeps.mjs stats:royalties          # Revenue report
```

---

## Testing Checklist

### Ghostnet v4 Testing

- [ ] Deploy v4 contract
- [ ] Set default royalty to 10%
- [ ] Mint token with `node keeps.mjs keep $test`
- [ ] Verify royalty field in TzKT API
- [ ] View on ghostnet.objkt.com - confirm 10% royalty shown
- [ ] Test pause/unpause
- [ ] Test admin transfer
- [ ] Verify metadata edits preserve royalties

### Mainnet Staging

- [ ] Deploy v4 to mainnet
- [ ] Mint 5 canonical pieces
- [ ] Transfer token to test wallet
- [ ] List for sale on objkt.com
- [ ] Complete test sale
- [ ] **Verify 10% royalty paid to creator** ✅
- [ ] Test rebake flow
- [ ] Verify attribution preserved

### Production Launch

- [ ] All Ghostnet tests passing
- [ ] 10+ tokens on mainnet staging
- [ ] First secondary sale verified
- [ ] Emergency pause tested
- [ ] Admin operations documented
- [ ] Deploy to production wallet
- [ ] Update KEEPS_CONTRACT env var
- [ ] Announce to community

---

## Contract Changes Summary

### Storage Additions

```python
# NEW in v4
self.data.paused = False
self.data.default_royalty_bps = 1000  # 10%
```

### New Entrypoints

1. `pause()` - Admin only, emergency stop
2. `unpause()` - Admin only, resume
3. `set_default_royalty(bps)` - Admin only, configure royalty
4. `admin_transfer(token_id, from_, to_)` - Admin only, customer service
5. `get_royalties(token_id)` - View, returns royalty config

### Modified Entrypoints

**keep() changes:**
- Line 87: Add `assert not self.data.paused, "CONTRACT_PAUSED"`
- After line 153: Add royalty field to token_info

**edit_metadata() changes:**
- Add pause check
- Preserve royalty fields

---

## Success Metrics

### Technical
- [ ] Gas costs < 0.5 XTZ per mint
- [ ] TzKT indexes within 1 block
- [ ] objkt.com displays within 5 minutes
- [ ] First secondary sale pays correct royalty

### User Experience
- [ ] 100% correct "Created by" attribution
- [ ] < 60 seconds mint to confirmation
- [ ] < 5% failed mint rate
- [ ] Clear error messages

### Business
- 100 mints in first month
- 10 unique creators in first week
- First royalty payment within 30 days
- 50% of tokens listed on objkt.com

---

## Implementation Timeline

**Week 1: Code v4 Contract**
- Create keeps_fa2_v4.py from v3
- Add royalty support (10% default)
- Add pause/unpause entrypoints
- Add admin_transfer entrypoint
- Write unit tests

**Week 2: Ghostnet Testing**
- Compile and deploy
- Configure royalty to 10%
- Mint 10 test tokens
- Verify objkt.com display
- Test pause functionality
- Test admin transfer

**Week 3: API Updates**
- Update keep-mint.mjs (add royalty metadata)
- Update keep-update.mjs (preserve royalties)
- Update keeps.mjs CLI (new commands)
- Test end-to-end flow
- Deploy to dev environment

**Week 4: Mainnet Staging**
- Deploy v4 to mainnet
- Mint canonical pieces
- Test secondary sales
- Verify royalty payments
- Monitor for issues
- Prepare production launch

**Week 5: Production Launch**
- Deploy v4 with kidlisp wallet
- Update KEEPS_CONTRACT env var
- Update MongoDB indexes
- Announce new contract
- Monitor first 24 hours
- Document lessons learned

---

## Critical Files for Implementation

1. **tezos/keeps_fa2_v3.py** → **keeps_fa2_v4.py**
   - Add royalty fields to token_info
   - Add pause storage + entrypoints
   - Add admin_transfer entrypoint

2. **system/netlify/functions/keep-mint.mjs**
   - Lines 913-946: Add royalty to metadataJson
   - Add royalty bytes to onChainMetadata

3. **system/netlify/functions/keep-update.mjs**
   - Lines 390-418: Preserve royalty in tokenInfo

4. **tezos/keeps.mjs**
   - Add royalty management commands
   - Add pause/unpause commands
   - Add admin transfer command

5. **tezos/KEEPS-LAUNCH-PLAN.md**
   - Update with v4 timeline
   - Add v4 testing checklist

---

## Risk Mitigation

### Risk: Storage costs increase
**Mitigation:** Monitor TzKT, royalty fields are small (~100 bytes)

### Risk: Pause abuse
**Mitigation:** Multi-sig admin wallet, transparent announcements

### Risk: Royalty calculation errors
**Mitigation:** Test on Ghostnet with various percentages, cap at 25%

### Risk: Admin transfer misuse
**Mitigation:** Require written request, audit trail, monthly transparency report

---

## Recommendation

**Deploy fresh v4 with royalties immediately.**

**Rationale:**
- Only 1 token on v3 ($roz can be re-minted)
- Royalties are essential for creator monetization
- Emergency controls critical for production security
- Clean start establishes professional foundation
- v3 attribution system already perfect - just add royalties

**Timeline:** 4 weeks from code to production

**Next Steps:**
1. Create keeps_fa2_v4.py (1-2 days)
2. Test on Ghostnet (3-5 days)
3. Update APIs (2-3 days)
4. Deploy mainnet staging (1 week testing)
5. Production launch (announce + monitor)

---

## Questions?

Contact: @jeffrey on aesthetic.computer
Contract Issues: https://github.com/whistlegraph/aesthetic-computer/issues
TzKT Explorer: https://tzkt.io
objkt.com: https://objkt.com

---

**Status:** Ready to implement
**Approval:** Pending user confirmation
**Expected Launch:** March 2026
