# ATProto Tools Directory Structure

## `/at/scripts/` - Permanent Operational Tools

### `audit-user-creation-sync.mjs` ⭐ KEEP
**Purpose:** Monitor and verify user creation flow from Auth0 → MongoDB → ATProto

**When to use:**
- Check that new signups are working correctly
- Verify ATProto accounts are being created
- Monitor system health after deployments
- Troubleshoot user creation issues

**Usage:**
```bash
# Check recent aesthetic tenant signups
node scripts/audit-user-creation-sync.mjs aesthetic 10

# Check sotce tenant
node scripts/audit-user-creation-sync.mjs sotce 10

# Check both tenants
node scripts/audit-user-creation-sync.mjs both 10
```

**Keep this tool!** It's essential for ongoing monitoring.

---

### `check-auth0-webhook-config.mjs` ⭐ KEEP
**Purpose:** Verify Auth0 webhook configuration and environment variables

**When to use:**
- After environment changes
- Debugging webhook delivery issues
- Verifying configuration in new deployments

**Usage:**
```bash
node scripts/check-auth0-webhook-config.mjs
```

**Keep this tool!** Useful for configuration verification.

---

### `query-auth0-signups.mjs` ⭐ KEEP (Already existed)
**Purpose:** Query Auth0 for recent user signups

**Keep this tool!** Already part of your toolkit.

---

## `/at/oldtools/` - One-Off Fix Scripts

### `test-user-creation-flow.mjs` 🔧 ARCHIVE
**Purpose:** Backfill missing user records and create ATProto accounts

**What it does:**
- Creates missing users records with codes
- Creates missing ATProto accounts
- One-time fix for the index conflict issue

**Historical use:** Fixed 5 users on Oct 14, 2025 after index conflict was discovered.

**Keep in oldtools:** May be useful if similar issues happen again, but shouldn't be needed for normal operations.

---

### `diagnose-user-code-generation.mjs` 🔧 ARCHIVE
**Purpose:** Test user code generation in isolation

**What it does:**
- Tests `generateUserCode()` function
- Simulates auth0-events.mjs flow
- Identifies index conflicts

**Historical use:** Discovered the `code_1` index conflict issue.

**Keep in oldtools:** Good diagnostic tool but not needed for day-to-day operations.

---

### `check-code-1-user.mjs` 🔧 ARCHIVE
**Purpose:** Verify no user has code "code_1" and list MongoDB indexes

**What it does:**
- Checks for users with invalid codes
- Lists all indexes on users collection
- Confirms index naming conventions

**Historical use:** Confirmed `code_1` was just an index name, not user data.

**Keep in oldtools:** Educational but not needed for operations.

---

## Quick Reference Commands

### Daily/Weekly Monitoring
```bash
# Check last 10 signups are properly synced
cd /workspaces/aesthetic-computer/at
node scripts/audit-user-creation-sync.mjs aesthetic 10
```

### After Deployment
```bash
# Verify webhook config
node scripts/check-auth0-webhook-config.mjs

# Check recent signups
node scripts/audit-user-creation-sync.mjs aesthetic 5
```

### If Issues Arise
```bash
# First, audit to see what's wrong
node scripts/audit-user-creation-sync.mjs aesthetic 20

# If needed, use one-off fix tool
node oldtools/test-user-creation-flow.mjs 10 --fix

# Or diagnose specific issues
node oldtools/diagnose-user-code-generation.mjs
```

---

## Recommended Monitoring Schedule

**Weekly:** Run audit to spot-check recent signups
```bash
node scripts/audit-user-creation-sync.mjs aesthetic 20
```

**After Auth0/MongoDB changes:** Verify configuration
```bash
node scripts/check-auth0-webhook-config.mjs
node scripts/audit-user-creation-sync.mjs aesthetic 5
```

**After PDS updates:** Check ATProto account creation
```bash
node scripts/audit-user-creation-sync.mjs aesthetic 10
```

---

## Documentation

- `INVESTIGATION-REPORT.md` - Full investigation details (Oct 14, 2025)
- `FIX-SUMMARY.md` - Complete fix documentation
- `QUICK-REFERENCE.md` - Quick command reference
- `README.md` - This file

---

**Last Updated:** October 14, 2025
**Issue Fixed:** MongoDB index conflict preventing user creation
**Status:** ✅ All systems operational
