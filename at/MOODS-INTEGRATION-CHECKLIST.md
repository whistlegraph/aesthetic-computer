# Moods ATProto Integration - Implementation Checklist

**Start Date:** _____  
**Target Completion:** _____  
**Status:** 🟡 Not Started

---

## ✅ Phase 1: Lexicon Creation & Testing (Week 1)

- [ ] Create lexicon deployment script `create-mood-lexicon.mjs`
- [ ] Deploy lexicon to PDS
- [ ] Validate lexicon with `@atproto/lexicon`
- [ ] Create test mood record manually via PDS admin API
- [ ] Query test record with `com.atproto.repo.listRecords`
- [ ] Verify record in ATProto firehose (if applicable)

**Blocker Notes:**

---

## ✅ Phase 2: Backend Integration (Week 1-2)

### 2.1: Create Helper Functions
- [ ] Create `/system/backend/mood-atproto.mjs`
  - [ ] `createAtprotoMood()` function
  - [ ] `deleteAtprotoMood()` function  
  - [ ] `deleteAllAtprotoMoods()` function
- [ ] Add unit tests for helper functions
- [ ] Test with mock user data

### 2.2: Update Mood Function
- [ ] Update `/system/netlify/functions/mood.mjs`
  - [ ] Import `createAtprotoMood`
  - [ ] Add dual-write after MongoDB insert
  - [ ] Add MongoDB update with ATProto URI
  - [ ] Add error handling for ATProto failures
- [ ] Test locally with single user
- [ ] Verify MongoDB record has `atproto` field
- [ ] Verify ATProto record exists via PDS query
- [ ] Deploy to production with feature flag (optional)

**Blocker Notes:**

---

## ✅ Phase 3: Deletion Sync (Week 2)

- [ ] Update `/system/netlify/functions/delete-erase-and-forget-me.mjs`
  - [ ] Import `deleteAllAtprotoMoods`
  - [ ] Add ATProto deletion after MongoDB deletion
  - [ ] Add error handling
- [ ] Test account deletion with test user
- [ ] Verify MongoDB moods deleted
- [ ] Verify ATProto moods deleted
- [ ] Query PDS to confirm no orphaned records
- [ ] Deploy to production

**Blocker Notes:**

---

## ✅ Phase 4: Backfill Historical Moods (Week 2-3)

### 4.1: Create Migration Script
- [ ] Create `/at/scripts/migrate-moods-to-atproto.mjs`
  - [ ] Implement dry-run mode
  - [ ] Implement single-user migration
  - [ ] Implement batch migration
  - [ ] Add rate limiting (100ms delay)
  - [ ] Add progress logging
  - [ ] Add error handling

### 4.2: Run Migration
- [ ] Run dry-run for all users
- [ ] Review dry-run output
- [ ] Test single-user migration (`@jeffrey`)
- [ ] Verify test user's moods in ATProto
- [ ] Run migration for first 10 users
- [ ] Monitor for errors/rate limits
- [ ] Run migration for all users
- [ ] Monitor MongoDB + PDS during migration
- [ ] Verify random sample of migrated moods

**Blocker Notes:**

**Migration Stats:**
- Total users: _____
- Users with ATProto: _____
- Total moods: _____
- Moods migrated: _____
- Moods failed: _____

---

## ✅ Phase 5: Testing & Verification (Week 3)

### 5.1: Create Audit Script
- [ ] Create `/at/scripts/audit-mood-atproto-sync.mjs`
  - [ ] Single-user audit
  - [ ] Batch audit
  - [ ] MongoDB vs ATProto comparison
  - [ ] Summary statistics

### 5.2: Run Tests
- [ ] Unit test: Create mood with ATProto account
- [ ] Unit test: Create mood without ATProto account
- [ ] Unit test: Create duplicate mood (no duplicate ATProto)
- [ ] Unit test: Soft delete mood (nuke)
- [ ] Unit test: Hard delete account
- [ ] Integration test: New signup → create mood
- [ ] Integration test: Existing user → create mood
- [ ] Integration test: Delete account
- [ ] Audit single user
- [ ] Audit first 10 users
- [ ] Audit first 50 users
- [ ] Audit all users with ATProto

### 5.3: Verification
- [ ] Check MongoDB counts match ATProto
- [ ] Verify no orphaned records
- [ ] Check for sync failures in logs
- [ ] Review error rates
- [ ] Verify deletion works correctly

**Blocker Notes:**

**Test Results:**
- Sync success rate: _____%
- Average sync latency: _____ms
- Moods with ATProto URI: _____
- Moods missing ATProto: _____

---

## ✅ Phase 6: Monitoring & Documentation (Week 3-4)

### 6.1: Monitoring
- [ ] Set up alerts for sync failures
- [ ] Set up dashboard for sync metrics
- [ ] Add logging for ATProto operations
- [ ] Create runbook for troubleshooting

### 6.2: Documentation
- [ ] Update `/at/README.md` with moods lexicon
- [ ] Add mood sync to `/at/MANIFEST.md`
- [ ] Update API docs for mood endpoint
- [ ] Add moods to `/at/QUICK-REFERENCE.md`
- [ ] Document rollback procedure
- [ ] Create troubleshooting guide

**Blocker Notes:**

---

## ✅ Phase 7: Production Launch (Week 4)

- [ ] Review all tests passing
- [ ] Review monitoring dashboards
- [ ] Review error logs (should be minimal)
- [ ] Enable for 100% of users
- [ ] Monitor for 48 hours
- [ ] Address any issues
- [ ] Mark project complete ✅

**Launch Stats:**
- Launch date: _____
- Total moods: _____
- Sync rate: _____%
- Error rate: _____%

---

## 🚨 Issues & Blockers

**Issue #1:**
- Description: _____
- Status: _____
- Resolution: _____

**Issue #2:**
- Description: _____
- Status: _____
- Resolution: _____

---

## 📊 Final Metrics

- **Total users with ATProto:** _____
- **Total moods in MongoDB:** _____
- **Total moods in ATProto:** _____
- **Sync success rate:** _____%
- **Average sync latency:** _____ms
- **Failed syncs:** _____
- **Manual fixes required:** _____

---

## ✨ Project Sign-Off

- [ ] All phases complete
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Monitoring in place
- [ ] Team notified
- [ ] Project retrospective completed

**Completed by:** _____  
**Completion date:** _____  

---

**Next Steps:**
See `MOODS-ATPROTO-INTEGRATION-PLAN.md` for detailed implementation guide.
