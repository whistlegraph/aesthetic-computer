# Quick Reference: User Creation & ATProto Sync

## 🚨 Problem We Fixed

**Issue:** Users signing up weren't getting user codes or ATProto accounts created automatically.

**Root Cause:** MongoDB index naming conflict in `user-code.mjs` → `ensureUserCodeIndex()` was failing silently.

**Fix:** Added error handling to gracefully handle existing index with different name.

---

## ✅ What's Working Now

- ✅ Auth0 signup webhook → Creates verifications record
- ✅ User code generation → No more index conflicts
- ✅ User records → Created with unique codes
- ✅ Email verification webhook → Triggers ATProto account creation
- ✅ ATProto accounts → Created on PDS with DID + handle
- ✅ Handle sync → Updates ATProto handle when AC handle is set

---

## 🛠️ Quick Commands

### Check Recent Signups Status
```bash
cd /workspaces/aesthetic-computer/at
node scripts/audit-user-creation-sync.mjs aesthetic 10
```

### Fix Missing Records (Dry Run)
```bash
node scripts/test-user-creation-flow.mjs 10
```

### Fix Missing Records (Actually Fix)
```bash
node scripts/test-user-creation-flow.mjs 10 --fix
```

### Test User Code Generation
```bash
node scripts/diagnose-user-code-generation.mjs
```

### Fix Single User
```bash
node scripts/test-user-creation-flow.mjs auth0|68ee500f0729405b6c0dfa18 --fix
```

---

## 📊 Current State (After Fix)

**Recent 5 Users:**
- Verifications: 5/5 ✅
- Users Records: 5/5 ✅
- User Codes: 5/5 ✅  
- ATProto Accounts: 4/4 ✅ (1 email not verified yet)
- PDS Accessible: 4/4 ✅

**All checks passing!** ✨

---

## 🔍 User Creation Flow

```
┌─────────────────┐
│  User Signs Up  │
└────────┬────────┘
         │
         ↓
┌──────────────────────────┐
│  Auth0 Webhook ("ss")    │
│  auth0-events.mjs        │
└────────┬─────────────────┘
         │
         ↓
   ✅ verifications (count: 0)
   ✅ users (with code)
         │
         ↓
┌─────────────────────┐
│  User Verifies Email │
└────────┬────────────┘
         │
         ↓
┌──────────────────────────┐
│  Auth0 Webhook ("sv")    │
│  auth0-events.mjs        │
└────────┬─────────────────┘
         │
         ↓
   ✅ verifications (count: 1)
   ✅ createAtprotoAccount()
   ✅ users.atproto { did, handle, password }
         │
         ↓
   ✅ ATProto PDS Account Created
```

---

## 🎯 Key Files

- `system/netlify/functions/auth0-events.mjs` - Webhook handler
- `system/backend/at.mjs` - ATProto account creation
- `system/public/aesthetic.computer/lib/user-code.mjs` - **FIXED HERE**
- `system/netlify/functions/handle.mjs` - Handle management

---

## 🔧 MongoDB Collections

### verifications
```javascript
{
  _id: "auth0|123...",  // user sub
  count: 0              // 0 = unverified, 1+ = verified
}
```

### users
```javascript
{
  _id: "auth0|123...",                    // user sub
  code: "ac25xadeh",                      // unique user code
  when: ISODate("2025-10-14T..."),       // signup date
  atproto: {                              // added on email verification
    did: "did:plc:abc123...",
    handle: "user.at.aesthetic.computer",
    password: "encrypted...",
    created: ISODate("2025-10-14T...")
  }
}
```

### @handles (optional, user-set)
```javascript
{
  _id: "auth0|123...",   // user sub
  handle: "jeffrey"      // AC handle (no @)
}
```

---

## 🎓 What We Learned

1. **Index conflicts can fail silently** → Always handle gracefully
2. **Auth0 webhooks work** → Verifications prove it
3. **Cascading failures** → One small error broke entire chain
4. **Audit tools are essential** → Created 4 diagnostic scripts
5. **Backfill is necessary** → Fixed 5 existing users

---

## 📞 If Things Break Again

1. Run audit: `node at/scripts/audit-user-creation-sync.mjs`
2. Check Netlify logs: `netlify functions:log auth0-events`
3. Test code generation: `node at/scripts/diagnose-user-code-generation.mjs`
4. Check webhook config: `node at/scripts/check-auth0-webhook-config.mjs`
5. Manual fix: `node at/scripts/test-user-creation-flow.mjs <sub> --fix`

---

## 🎉 Success!

All recent signups are now properly synced with:
- ✅ User codes
- ✅ MongoDB records
- ✅ ATProto accounts
- ✅ PDS accessibility

**Future signups will work automatically!**
