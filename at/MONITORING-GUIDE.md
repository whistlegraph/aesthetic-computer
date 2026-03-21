# ATProto User Creation Monitoring Guide

## 🎯 Quick Start - Check Recent Signups

```bash
cd /workspaces/aesthetic-computer/at
node scripts/audit-user-creation-sync.mjs aesthetic 10
```

This will show you if new signups are:
- ✅ Getting user codes
- ✅ Creating MongoDB records
- ✅ Creating ATProto accounts
- ✅ Accessible on PDS

---

## 📊 What You'll See

### ✅ Healthy Signup
```
👤 User: auth0|68ee500f0729405b6c0dfa18
   Email: user@example.com
   Email Verified: ✅
   ✅ Verifications record: count=1
   ✅ Users record found:
      Code: ac25nocoj
      ATProto:
         DID: did:plc:u6wlzqrokjtfzcra5lbbgh6i
         Handle: user.at.aesthetic.computer
      🔍 Checking PDS account...
         ✅ PDS account exists and is accessible
   ✅ ALL CHECKS PASSED
```

### ⚠️ Problem Signup
```
👤 User: auth0|123456789
   Email: user@example.com
   Email Verified: ✅
   ✅ Verifications record: count=1
   ❌ No users record found
   ⚠️ ISSUES FOUND:
      - Missing users record
```

---

## 🛠️ Available Tools

### Permanent Monitoring Tools (`/at/scripts/`)

**Primary Tool:**
- `audit-user-creation-sync.mjs` ⭐ - Check user sync status
  ```bash
  node scripts/audit-user-creation-sync.mjs aesthetic 10
  ```

**Supporting Tools:**
- `check-auth0-webhook-config.mjs` - Verify Auth0 config
- `query-auth0-signups.mjs` - Query Auth0 directly
- `check-auth0-flow.mjs` - Check Auth0 flow
- `check-recent-users.mjs` - Check recent users

### One-Off Fix Tools (`/at/oldtools/`)

Only use if problems arise:
- `test-user-creation-flow.mjs` - Backfill missing records
- `diagnose-user-code-generation.mjs` - Test code generation
- `check-code-1-user.mjs` - Check index conflicts

---

## 📅 Recommended Schedule

### Weekly Health Check
```bash
# Check last 20 signups
node scripts/audit-user-creation-sync.mjs aesthetic 20
```

Expected: All checks passing, ATProto accounts created

### After Deployment
```bash
# Verify config
node scripts/check-auth0-webhook-config.mjs

# Check recent signups
node scripts/audit-user-creation-sync.mjs aesthetic 5
```

### If Issues Detected
```bash
# Detailed audit
node scripts/audit-user-creation-sync.mjs aesthetic 50

# Check Netlify logs
netlify functions:log auth0-events

# If needed, run fix (from oldtools)
node oldtools/test-user-creation-flow.mjs 10 --fix
```

---

## 🔍 What to Look For

### Good Signs ✅
- All users have verification records
- Email-verified users have user codes
- Email-verified users have ATProto accounts
- PDS accounts are accessible
- Zero issues reported

### Warning Signs ⚠️
- Missing user records for verified users
- Missing ATProto accounts for verified users
- PDS login failures
- Issues count > 0

### Critical Issues 🚨
- Multiple recent signups failing
- No ATProto accounts being created
- MongoDB connection errors
- Auth0 webhook not firing

---

## 📞 Troubleshooting Steps

1. **Run audit** to identify scope of problem
   ```bash
   node scripts/audit-user-creation-sync.mjs aesthetic 20
   ```

2. **Check Netlify function logs**
   ```bash
   netlify functions:log auth0-events
   ```

3. **Verify webhook config**
   ```bash
   node scripts/check-auth0-webhook-config.mjs
   ```

4. **Test user code generation**
   ```bash
   node oldtools/diagnose-user-code-generation.mjs
   ```

5. **Manual fix if needed**
   ```bash
   node oldtools/test-user-creation-flow.mjs <user-sub> --fix
   ```

---

## 📚 Documentation

- `QUICK-REFERENCE.md` - Quick commands
- `FIX-SUMMARY.md` - Oct 14, 2025 fix details
- `INVESTIGATION-REPORT.md` - Full investigation
- `TOOLS-README.md` - Complete tools documentation
- `oldtools/README.md` - Archived tools info

---

## 🎉 Current Status (as of Oct 14, 2025)

- ✅ Index conflict fixed in `user-code.mjs`
- ✅ All recent users backfilled (5 users)
- ✅ Automated ATProto creation working
- ✅ Monitoring tools in place

**Next signup will automatically create ATProto account!**

---

## 💡 Key Insights

1. **Auth0 webhook IS working** - Verifications prove it
2. **User codes follow format:** `ac{YY}{5-chars}` (e.g., `ac25nocoj`)
3. **ATProto handles:** `{handle}.at.aesthetic.computer` or fallback to code
4. **One check catches all:** audit script shows full pipeline
5. **Fix once, monitor always:** Tools prevent future issues

---

**For questions or issues, refer to full docs in `/at/` directory**
