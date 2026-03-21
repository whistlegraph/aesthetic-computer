# ATProto User Creation & Sync Investigation Summary

**Date:** October 14, 2025  
**Status:** 🔴 **CRITICAL ISSUES FOUND**

---

## 🔍 Investigation Overview

Investigated the user creation flow from Auth0 signup → MongoDB → ATProto PDS to understand why ATProto accounts are not being automatically created.

---

## 📋 Current Flow (Expected)

1. **User Signs Up** → Auth0
2. **Auth0 Log Stream** → Webhook to `/.netlify/functions/auth0-events` (event type: "ss")
3. **auth0-events.mjs**:
   - ✅ Creates `verifications` record (count: 0)
   - ✅ Creates `users` record with generated code
4. **User Verifies Email** → Auth0
5. **Auth0 Log Stream** → Webhook to `/.netlify/functions/auth0-events` (event type: "sv")
6. **auth0-events.mjs**:
   - ✅ Increments `verifications` count to 1
   - ✅ Calls `createAtprotoAccount()` to create PDS account
   - ✅ Stores ATProto credentials in `users` collection

---

## 🐛 Issues Found

### Issue #1: Users Records Not Created ❌

**Affected Users:** All 5 recent signups (100%)

**Evidence:**
```
MongoDB Collections Status:
  Verifications: 5/5 ✅ (100%)
  Users: 0/5 ❌ (0%)
  Handles: 3/5 (60%)
  ATProto: 0/5 ❌ (0%)
```

**Root Cause:**
The user code generation in `auth0-events.mjs` is failing silently:

```javascript
try {
  await ensureUserCodeIndex(database);
  const signupDate = new Date(log.data.date);
  const code = await generateUniqueUserCode(database, signupDate);
  
  const users = database.db.collection("users");
  await users.insertOne({ 
    _id: aestheticSub, 
    code,
    when: signupDate
  });
  
  shell.log("🎫 Generated user code:", code, "for:", aestheticSub);
} catch (error) {
  shell.log("⚠️ Failed to generate user code:", aestheticSub, error);
  // Don't block signup on code generation failure
}
```

The error is logged but swallowed, so signup succeeds but user record is never created.

**Impact:**
- No user codes generated
- No ATProto accounts created (dependent on user records)
- Users can still sign up and use handles, but missing identity data

---

### Issue #2: ATProto Accounts Not Created ❌

**Affected Users:** All 4 email-verified users (100%)

**Root Cause:** Cascading failure from Issue #1
- `createAtprotoAccount()` requires user record to exist
- Without user record, no code is available for fallback handle
- ATProto account creation is never attempted

**Evidence from auth0-events.mjs:**
```javascript
// 🦋 Create ATProto account on first verification
shell.log("🦋 Creating ATProto account for newly verified user...");
const atprotoResult = await createAtprotoAccount(
  database,
  aestheticSub,
);
```

This code runs, but likely fails because:
1. No user record with code exists
2. User may not have handle set yet
3. `createAtprotoAccount()` needs either handle or code

---

## ✅ What's Working

1. **Auth0 Log Stream Webhook** - Confirmed working
   - `AUTH0_LOG_TOKEN` is set in Netlify env
   - Signup events ("ss") are being received
   - Verification events ("sv") are being received

2. **Verifications Collection** - Working perfectly
   - All users have verification records
   - Counts are accurate (0 for unverified, 1 for verified)

3. **Handles Collection** - Partially working
   - 3 out of 5 users have handles set
   - Handle creation via `/handle.mjs` endpoint works

---

## 🔧 Recommended Fixes

### Fix #1: Investigate User Code Generation Failure

**Priority:** 🔴 CRITICAL

**Action Items:**
1. Check Netlify function logs for user code generation errors
2. Review `generateUniqueUserCode()` implementation
3. Check if MongoDB index creation is failing
4. Verify database permissions for inserting into `users` collection

**Command to check logs:**
```bash
netlify functions:log auth0-events
```

### Fix #2: Backfill Missing User Records

**Priority:** 🟡 HIGH

**Action:**
```bash
cd /workspaces/aesthetic-computer/at
node scripts/test-user-creation-flow.mjs 5 --fix
```

This will:
- Create missing `users` records with generated codes
- Create ATProto accounts for email-verified users
- Sync all data to PDS

**Affected Users:**
- `auth0|68ee503033f18c5d54238469` - Needs user record (email not verified)
- `auth0|68ee500f0729405b6c0dfa18` - Needs user record + ATProto account
- `auth0|68ee4fe6041a48c2971c7fb1` - Needs user record + ATProto account  
- `auth0|68ede47abdf0b654d1475476` - Needs user record + ATProto account
- `auth0|68ebc6c78aa676dc64c40609` - Needs user record + ATProto account

### Fix #3: Improve Error Handling

**Priority:** 🟢 MEDIUM

Update `auth0-events.mjs` to:
1. Log full error details (not just message)
2. Send alerts for user creation failures
3. Retry user code generation
4. Create user record with temporary/fallback code if generation fails

---

## 📊 Detailed Audit Results

### Recent Users Status (Last 5 Signups)

#### User 1: auth0|68ee503033f18c5d54238469
- Email: bjarke.hee@gmail.com
- Email Verified: ❌
- Created: 6.1 hours ago
- Verifications: ✅ (count=0)
- Users Record: ❌
- Handle: None
- ATProto: ❌

#### User 2: auth0|68ee500f0729405b6c0dfa18
- Email: csx0909@gmail.com
- Email Verified: ✅
- Created: 6.1 hours ago
- Verifications: ✅ (count=1)
- Users Record: ❌
- Handle: @csx
- ATProto: ❌

#### User 3: auth0|68ee4fe6041a48c2971c7fb1
- Email: violinoletsgo@gmail.com
- Email Verified: ✅
- Created: 6.1 hours ago
- Verifications: ✅ (count=1)
- Users Record: ❌
- Handle: None
- ATProto: ❌

#### User 4: auth0|68ede47abdf0b654d1475476
- Email: ligands-bays1k@icloud.com
- Email Verified: ✅
- Created: 13.7 hours ago
- Verifications: ✅ (count=1)
- Users Record: ❌
- Handle: @zhuxin
- ATProto: ❌

#### User 5: auth0|68ebc6c78aa676dc64c40609
- Email: christianbluhme@yahoo.dk
- Email Verified: ✅
- Created: 52.3 hours ago (2.2 days)
- Verifications: ✅ (count=1)
- Users Record: ❌
- Handle: @43
- ATProto: ❌

---

## 📁 Files Involved

### Netlify Functions
- `/system/netlify/functions/auth0-events.mjs` - Webhook handler (ISSUE HERE)
- `/system/netlify/functions/handle.mjs` - Handle management

### Backend
- `/system/backend/at.mjs` - ATProto account creation
- `/system/backend/authorization.mjs` - User lookup functions
- `/system/backend/database.mjs` - MongoDB connection
- `/system/public/aesthetic.computer/lib/user-code.mjs` - Code generation

### Audit Scripts (Created)
- `/at/scripts/audit-user-creation-sync.mjs` - Full audit tool
- `/at/scripts/test-user-creation-flow.mjs` - Fix/backfill tool
- `/at/scripts/check-auth0-webhook-config.mjs` - Config checker

---

## 🎯 Next Steps

1. **Immediate:** Check Netlify logs for actual error messages
2. **Quick Fix:** Run backfill script with `--fix` flag
3. **Root Cause:** Debug why user code generation is failing
4. **Long-term:** Add monitoring/alerting for user creation failures
5. **Testing:** Create test users to verify fix

---

## 📞 Key Questions to Answer

1. ❓ What is the actual error message from user code generation?
2. ❓ Is it a MongoDB permission issue?
3. ❓ Is it a unique index collision issue?
4. ❓ Has this been working in the past, or is it a new deployment issue?
5. ❓ Are there any rate limits or throttling on MongoDB writes?

---

**Investigation Tools Available:**
- `audit-user-creation-sync.mjs` - Check sync status
- `test-user-creation-flow.mjs` - Simulate/fix user creation
- `check-auth0-webhook-config.mjs` - Verify webhook config
